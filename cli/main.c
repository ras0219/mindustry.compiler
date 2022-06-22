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

struct Arguments
{
    const char* input;
    const char* output;
    const char* env_file;
    struct Array inc;
    unsigned fCompile : 1;
    unsigned fPreprocOnly : 1;
    unsigned fAssembleOnly : 1;
    unsigned fParseOnly : 1;
    unsigned fDebugBE : 1;
    struct Array macro_name;
};

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
            else
            {
                fprintf(stderr, "error: unrecognized flag %s\n", argv[i]);
                goto usage;
            }
        }
        else
        {
            if (out->input)
            {
                fprintf(stderr, "error: input already specified\n");
                goto usage;
            }
            out->input = argv[i];
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
    array_push_byte(&out->inc, 0);

    if (!out->input)
    {
        fprintf(stderr, "error: no input file\n");
        goto usage;
    }

    return 0;

usage:
    return usage(argv[0]);
}

static void extract_basename(const char* path, Array* out)
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
    array_push(out, path + b, c - b);
}

int main(int argc, const char* const* argv)
{
    int rc = 0;
    struct Arguments args = {};
    if (rc = parse_arguments(argc, argv, &args)) return rc;

    struct Array asm_file = {0};
    struct Array obj_file = {0};
    struct Array exe_file = {0};
    struct Array cmd_buf = {0};
    extract_basename(args.input, &asm_file);
    array_push(&obj_file, asm_file.data, asm_file.sz);
    array_appends(&asm_file, ".s");
    array_appends(&obj_file, ".o");
    array_appends(&exe_file, "a.out");
    if (args.output)
    {
        if (args.fAssembleOnly)
        {
            array_clear(&asm_file);
            array_appends(&asm_file, args.output);
        }
        else if (args.fCompile)
        {
            array_clear(&obj_file);
            array_appends(&obj_file, args.output);
        }
        else
        {
            array_clear(&exe_file);
            array_appends(&exe_file, args.output);
        }
    }
    array_push_byte(&asm_file, 0);
    array_push_byte(&obj_file, 0);
    array_push_byte(&exe_file, 0);
    struct FrontEnd fe = {};

    fe.pp = preproc_alloc(args.inc.data);

    const char* macro_names = args.macro_name.data;
    for (size_t i = 0; i < args.macro_name.sz;)
    {
        preproc_define(fe.pp, macro_names + i);
        size_t name_len = strlen(macro_names + i);
        i += name_len + 1;
    }

    const char* predefs[] = {
        "__LP64__",
        "__x86_64__",
        "__x86_64",
        "__STDC__",
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
        "_FORTIFY_SOURCE=0",
        "double=long",
        "float=int",
    };

    for (size_t i = 0; i < sizeof(predefs) / sizeof(predefs[0]); ++i)
    {
        preproc_define(fe.pp, predefs[i]);
    }

    UNWRAP(fe_preproc(&fe, args.input));
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
    fe.fout = fopen(asm_file.data, "wbT");

    if (!fe.fout)
    {
        perror(asm_file.data);
        UNWRAP(1);
    }

    UNWRAP(cg_emit(fe.cg, args.input, fe.fout));
    fclose(fe.fout);
    fe.fout = NULL;
    UNWRAP(parser_has_errors());

    if (!args.fAssembleOnly)
    {
        array_clear(&cmd_buf);
        array_appendf(&cmd_buf,
                      "clang -target x86_64-apple-darwin20.3.0 -g -c %s -o %s",
                      (char*)asm_file.data,
                      (char*)obj_file.data);
        array_push_byte(&cmd_buf, 0);
        printf("%s\n", (char*)cmd_buf.data);
        UNWRAP(system((char*)cmd_buf.data));
        if (!args.fCompile)
        {
#ifdef _WIN32
            const char* exe_file = args.output ? args.output : "test.exe";
            char buf[512];
            rc = sizeof(buf) <=
                 snprintf(buf, sizeof(buf), "ml64 /Zd /Zi /Sa %s /link /OPT:REF /OPT:ICF /out:%s", asm_file, exe_file);
            if (!rc) rc = system(buf);
#else
            array_clear(&cmd_buf);
            array_appendf(&cmd_buf,
                          "clang -target x86_64-apple-darwin20.3.0 %s -o %s",
                          (char*)obj_file.data,
                          (char*)exe_file.data);
            array_push_byte(&cmd_buf, 0);
            printf("%s\n", (char*)cmd_buf.data);
            UNWRAP(system((char*)cmd_buf.data));
#endif
        }
    }

fail:
    if (parser_has_errors())
    {
        parser_print_errors(stderr);
    }
    if (fe.fout) fclose(fe.fout);
    fe_destroy(&fe);
    array_destroy(&cmd_buf);
    array_destroy(&exe_file);
    array_destroy(&obj_file);
    array_destroy(&asm_file);
    return rc;
}
