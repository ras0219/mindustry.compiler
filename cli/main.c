#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "be.h"
#include "cg.h"
#include "elaborator.h"
#include "errors.h"
#include "fe.h"
#include "lexstate.h"
#include "parse.h"
#include "preproc.h"
#include "stdlibe.h"
#include "tok.h"
#include "token.h"
#include "unwrap.h"

#ifdef _WIN32
#include <io.h>
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#endif

void fe_destroy(struct FrontEnd* fe)
{
    if (fe->cg)
    {
        cg_destroy(fe->cg);
        free(fe->cg);
    }
    if (fe->be)
    {
        be_destroy(fe->be);
        free(fe->be);
    }
    if (fe->elab)
    {
        elaborator_destroy(fe->elab);
        free(fe->elab);
    }
    if (fe->parser)
    {
        parser_destroy(fe->parser);
        free(fe->parser);
    }
    if (fe->pp) preproc_free(fe->pp);
    if (fe->fout) fclose(fe->fout);
}

int fe_preproc(struct FrontEnd* fe, const char* filename)
{
    FILE* f = fopen(filename, "rb");
    if (!f)
    {
        char buf[128];
        snprintf(buf, 128, "%s: failed to open", filename);
        perror(buf);
        return 1;
    }
    int rc = preproc_file(fe->pp, f, filename);
    fclose(f);
    return rc;
}

int fe_lex_file_opened(struct FrontEnd* fe, const char* filename, FILE* f) { return preproc_file(fe->pp, f, filename); }

int usage(const char* self)
{
    fprintf(stderr,
            "Usage: %s [options] <file.c>\n"
            "Options:\n"
            "  -c                                Compile only.\n"
            "  -D <macro>, -D <macro>=<value>    Define <macro> with optional <value>.\n"
            "  -E                                Preprocess only.\n"
            "  -S                                Emit assembly.\n"
            "  -I <dir>                          Add directory to the include search path.\n"
            "  -o <path>                         Specify output path.\n"
            "  --debug-be                        Emit backend tracing information\n"
            "  --debug-parse                     Emit parse tree.\n"
            "",
            self);
    return 1;
}

typedef struct Arguments
{
    struct Array inputs;
    struct Array input_offsets;
    struct Array link_flags;
    const char* output;
    const char* env_file;
    struct Array inc;
    unsigned fCompile : 1;
    unsigned fPreprocOnly : 1;
    unsigned fAssembleOnly : 1;
    unsigned fParseOnly : 1;
    unsigned fDebugBE : 1;
    struct Array macro_name;
} Arguments;

static void args_destroy(Arguments* args)
{
    array_destroy(&args->inputs);
    array_destroy(&args->link_flags);
    array_destroy(&args->input_offsets);
    array_destroy(&args->inc);
    array_destroy(&args->macro_name);
}

static int my_system(const char* cmd)
{
    int ec = system(cmd);
    if (WIFEXITED(ec))
    {
        return WEXITSTATUS(ec);
    }
    else
    {
        return WTERMSIG(ec);
    }
}

static void append_cli_arg(Array* arr, const char* s, size_t sz)
{
    if (arr->sz) array_push_byte(arr, ' ');
    array_push(arr, s, sz);
}

static int matches_link_switch(const char* arg)
{
    if (arg[0] == '-' && arg[1] == 'W' && arg[2] == 'l') return 1;
    return strcmp(arg, "-dynamiclib") == 0;
}
static int matches_link_setting(const char* arg)
{
    return strcmp(arg, "-install_name") == 0 || strcmp(arg, "-framework") == 0;
}

static int parse_arguments(int argc, const char* const* argv, struct Arguments* out)
{
    for (int i = 1; i < argc; ++i)
    {
        if (*argv[i] == '-'
#ifdef _WIN32
            || *argv[i] == '/'
#endif
        )
        {
            if (strcmp(argv[i] + 1, "c") == 0)
            {
                out->fCompile = 1;
            }
            else if (strcmp(argv[i] + 1, "S") == 0)
            {
                out->fAssembleOnly = 1;
            }
            else if (strcmp(argv[i] + 1, "E") == 0)
            {
                out->fPreprocOnly = 1;
            }
            else if (strcmp(argv[i] + 1, "-debug-parse") == 0)
            {
                out->fParseOnly = 1;
            }
            else if (strcmp(argv[i] + 1, "-debug-be") == 0)
            {
                out->fDebugBE = 1;
            }
            else if (strcmp(argv[i] + 1, "o") == 0)
            {
                ++i;
                if (i == argc)
                {
                    fprintf(stderr, "error: expected filename after %s\n", argv[i - 1]);
                    goto usage;
                }
                if (out->output)
                {
                    fprintf(stderr, "error: output already specified\n");
                    goto usage;
                }
                out->output = argv[i];
            }
            else if (strcmp(argv[i] + 1, "I") == 0)
            {
                ++i;
                if (i == argc)
                {
                    fprintf(stderr, "error: expected path after %s\n", argv[i - 1]);
                    goto usage;
                }
                if (out->inc.sz)
                {
                    array_push_byte(&out->inc, ';');
                }
                array_appends(&out->inc, argv[i]);
            }
            else if (argv[i][1] == 'I')
            {
                if (out->inc.sz)
                {
                    array_push_byte(&out->inc, ';');
                }
                array_appends(&out->inc, argv[i] + 2);
            }
            else if (strcmp(argv[i] + 1, "N") == 0)
            {
                ++i;
                if (i == argc)
                {
                    fprintf(stderr, "error: expected filename after %s\n", argv[i - 1]);
                    goto usage;
                }
                if (out->env_file)
                {
                    fprintf(stderr, "error: env file already specified\n");
                    goto usage;
                }
                out->env_file = argv[i];
            }
            else if (strcmp(argv[i] + 1, "D") == 0)
            {
                ++i;
                if (i == argc)
                {
                    fprintf(stderr, "error: expected macro name after %s\n", argv[i - 1]);
                    goto usage;
                }
                array_appends(&out->macro_name, argv[i]);
                array_push_byte(&out->macro_name, 0);
            }
            else if (argv[i][1] == 'D')
            {
                array_appends(&out->macro_name, argv[i] + 2);
                array_push_byte(&out->macro_name, 0);
            }
            else if (matches_link_switch(argv[i]))
            {
                append_cli_arg(&out->link_flags, argv[i], strlen(argv[i]));
            }
            else if (matches_link_setting(argv[i]))
            {
                append_cli_arg(&out->link_flags, argv[i], strlen(argv[i]));
                ++i;
                if (i == argc)
                {
                    fprintf(stderr, "error: expected macro name after %s\n", argv[i - 1]);
                    goto usage;
                }
                append_cli_arg(&out->link_flags, argv[i], strlen(argv[i]));
            }
            else
            {
                fprintf(stderr, "error: unrecognized flag %s\n", argv[i]);
                goto usage;
            }
        }
        else
        {
            arrsz_push(&out->input_offsets, out->inputs.sz);
            array_appends(&out->inputs, argv[i]);
            array_push_byte(&out->inputs, '\0');
        }
    }

    if (out->env_file)
    {
        FILE* f = fopen(out->env_file, "r");
        if (!f)
        {
            perror(out->env_file);
            return 1;
        }
        char buf[1024];
        size_t read = fread(buf, 1, 1024, f);
        if (read)
        {
            if (out->inc.sz)
            {
                array_push_byte(&out->inc, ';');
            }
            array_push(&out->inc, buf, read);
        }
        if (ferror(f)) return 1;
        fclose(f);
    }

    char* inc = getenv("INCLUDE");
    if (inc)
    {
        if (out->inc.sz)
        {
            array_push_byte(&out->inc, ';');
        }
        array_appends(&out->inc, inc);
    }

#ifdef __APPLE__
    if (out->inc.sz)
    {
        array_push_byte(&out->inc, ';');
    }
    array_appends(&out->inc,
                  "/Library/Developer/CommandLineTools/SDKs/MacOSX11.3.sdk/usr/include"
                  ";/Library/Developer/CommandLineTools/usr/lib/clang/13.0.0/include");
#endif

    array_push_byte(&out->inc, 0);

    if (out->input_offsets.sz == 0)
    {
        fprintf(stderr, "error: no input files\n");
        goto usage;
    }

    return 0;

usage:
    return usage(argv[0]);
}

typedef struct PathComponents
{
    int basename;
    int ext;
    int end;
} PathComponents;

static void parse_path(const char* path, PathComponents* comps)
{
    int x = 0;
    int b = 0;
    int c = 0;
    for (char ch = path[x]; ch; ch = path[++x])
    {
        if (ch == '/')
            b = x + 1, c = b;
        else if (ch == '.')
            c = x;
    }

    if (c == b) c = x;

    comps->basename = b;
    comps->ext = c;
    comps->end = x;
}

static int path_ext_is(const char* input, const PathComponents* comps, const char* ext)
{
    const size_t extlen = strlen(ext);
    return comps->end - comps->ext == extlen && memcmp(input + comps->ext, ext, extlen) == 0;
}

static const char* predefs[] = {
    "__LP64__",
    "__x86_64__",
    "__x86_64",
    "__STDC__",
    "__APPLE__",
    "__MACH__",
    "_POSIX_SOURCE",
    "_POSIX_C_SOURCE=200112L",
    "_DARWIN_C_SOURCE=200112L",
    "_C99_SOURCE",
    "__DARWIN_OS_INLINE=static inline",
    "__llvm__",
    "__func__=\"__func__\"",
    "__FILE__=\"/path/to/__FILE__\"",
    "__LINE__=0",
    "__ras0219_cc__",
    "__GNUC__=3",
    "__GNUC_MINOR__=0",
    "_FORTIFY_SOURCE=0",
    "double=long",
    "float=int",
    "__asm__(X)=",
    "__attribute__(X)=",
    "TARGET_CPU_X86_64",
    "__has_include(X)=0",
};

int main(int argc, const char* const* argv)
{
    int rc = 0;
    struct Arguments args = {0};
    struct Array files_buf = {0};
    struct Array cmd_buf = {0};
    struct Array link_line = {0};
    struct FrontEnd fe = {0};
    UNWRAP(parse_arguments(argc, argv, &args));

    const size_t n_inputs = arrsz_size(&args.input_offsets);
    if (args.output && (args.fCompile || args.fAssembleOnly) && n_inputs > 1)
    {
        fprintf(stderr, "error: cannot specify multiple inputs with -S or -c\n");
        UNWRAP(1);
    }

    array_appends(&link_line, "clang -target x86_64-apple-darwin20.3.0 ");
    array_push(&link_line, args.link_flags.data, args.link_flags.sz);
    for (size_t i = 0; i < n_inputs; ++i)
    {
        const char* input = (const char*)args.inputs.data + arrsz_at(&args.input_offsets, i);
        const char* c_file = NULL;
        const char* asm_file = NULL;
        const char* obj_file = NULL;
        PathComponents comps;
        parse_path(input, &comps);

        if (path_ext_is(input, &comps, ".c"))
        {
            // C file
            c_file = input;

            if (args.fAssembleOnly && args.output)
            {
                asm_file = args.output;
            }
            else
            {
                array_clear(&files_buf);
                array_push(&files_buf, input + comps.basename, comps.ext - comps.basename);
                array_push(&files_buf, ".s", 3);

                if (args.fCompile && args.output)
                {
                    obj_file = args.output;
                }
                else
                {
                    const size_t obj_start = files_buf.sz;
                    array_push(&files_buf, input + comps.basename, comps.ext - comps.basename);
                    array_push(&files_buf, ".o", 3);
                    obj_file = files_buf.data + obj_start;
                }
                asm_file = files_buf.data;
            }
        }
        else if (path_ext_is(input, &comps, ".s"))
        {
            // asm file
            asm_file = input;
            if (args.fCompile && args.output)
            {
                obj_file = args.output;
            }
            else
            {
                array_clear(&files_buf);
                array_push(&files_buf, input + comps.basename, comps.ext - comps.basename);
                array_push(&files_buf, ".o", 3);

                obj_file = files_buf.data;
            }
        }
        else
        {
            // Unknown -- pass to linker
            obj_file = input;
        }

        if (c_file)
        {
            fe.pp = preproc_alloc(args.inc.data);

            const char* macro_names = args.macro_name.data;
            for (size_t i = 0; i < args.macro_name.sz;)
            {
                preproc_define(fe.pp, macro_names + i);
                size_t name_len = strlen(macro_names + i);
                i += name_len + 1;
            }

            for (size_t i = 0; i < sizeof(predefs) / sizeof(predefs[0]); ++i)
            {
                preproc_define(fe.pp, predefs[i]);
            }

            UNWRAP(fe_preproc(&fe, c_file));
            if (args.fPreprocOnly)
            {
                preproc_dump(fe.pp);
                goto fail;
            }

            fe.parser = (struct Parser*)my_malloc(sizeof(struct Parser));
            parser_init(fe.parser);
            UNWRAP(parser_parse(fe.parser, preproc_tokens(fe.pp), preproc_stringpool(fe.pp)));
            parser_debug_check(fe.parser);
            UNWRAP(parser_has_errors());

            if (args.fParseOnly)
            {
                parser_dump(fe.parser, stdout);
                goto fail;
            }

            fe.elab = (struct Elaborator*)my_malloc(sizeof(struct Elaborator));
            elaborator_init(fe.elab, fe.parser);
            UNWRAP(elaborate(fe.elab));
            UNWRAP(parser_has_errors());

            fe.cg = (struct CodeGen*)my_malloc(sizeof(struct CodeGen));
            cg_init(fe.cg);

            fe.be = (struct BackEnd*)my_malloc(sizeof(struct BackEnd));
            be_init(fe.be, fe.parser, fe.elab, fe.cg);
            fe.be->debug_taces = args.fDebugBE;

            UNWRAP(be_compile(fe.be));
            UNWRAP(parser_has_errors());
            fe.fout = fopen(asm_file, "wbT");

            if (!fe.fout)
            {
                perror(asm_file);
                UNWRAP(1);
            }

            UNWRAP(cg_emit(fe.cg, c_file, fe.fout));
            fclose(fe.fout);
            fe.fout = NULL;
            UNWRAP(parser_has_errors());
            fe_destroy(&fe);
            memset(&fe, 0, sizeof(fe));
        }

        if (asm_file && obj_file)
        {
            // #ifdef _WIN32
            //     buf, sizeof(buf), "ml64 /Zd /Zi /Sa %s /link /OPT:REF /OPT:ICF /out:%s",
            //     asm_file, exe_file);
            // #else
            // #endif
            array_clear(&cmd_buf);
            array_appends(&cmd_buf, "clang -target x86_64-apple-darwin20.3.0 -g -c");
            append_cli_arg(&cmd_buf, asm_file, strlen(asm_file));
            array_appends(&cmd_buf, " -o");
            append_cli_arg(&cmd_buf, obj_file, strlen(obj_file));
            array_push_byte(&cmd_buf, 0);
            printf("%s\n", (char*)cmd_buf.data);
            UNWRAP(my_system(cmd_buf.data));
        }

        if (obj_file)
        {
            append_cli_arg(&link_line, obj_file, strlen(obj_file));
        }
    }
    if (args.fAssembleOnly || args.fCompile || args.fParseOnly || args.fPreprocOnly)
    {
        goto fail;
    }

    array_appends(&link_line, " -o");
    if (args.output)
    {
        append_cli_arg(&link_line, args.output, strlen(args.output));
    }
    else
    {
        array_appends(&link_line, " a.out");
    }
    array_push_byte(&link_line, 0);
    printf("%s\n", (char*)link_line.data);
    UNWRAP(my_system(link_line.data));

fail:
    if (parser_has_errors())
    {
        parser_print_errors(stderr);
    }
    fe_destroy(&fe);
    array_destroy(&cmd_buf);
    array_destroy(&files_buf);
    args_destroy(&args);
    return rc;
}
