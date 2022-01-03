#define __DARWIN_OS_INLINE static inline

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "errors.h"
#include "fe.h"
#include "tok.h"

#ifdef _WIN32
#include <io.h>
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#endif

int usage()
{
    fprintf(stderr, "Usage: mindustry.compiler [-D <macro>] [-E env.txt] [-o <output>] [-I <path>] [-c] <file.c>\n");
    return 1;
}

struct Arguments
{
    const char* input;
    const char* output;
    const char* env_file;
    struct Array inc;
    char fCompile;
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
            else if (strcmp(argv[i] + 1, "o") == 0)
            {
                ++i;
                if (i == argc)
                {
                    fprintf(stderr, "error: expected filename after %s\n", argv[i - 1]);
                    return usage();
                }
                if (out->output)
                {
                    fprintf(stderr, "error: output already specified\n");
                    return usage();
                }
                out->output = argv[i];
            }
            else if (strcmp(argv[i] + 1, "I") == 0)
            {
                ++i;
                if (i == argc)
                {
                    fprintf(stderr, "error: expected path after %s\n", argv[i - 1]);
                    return usage();
                }
                if (out->inc.sz)
                {
                    array_push_byte(&out->inc, ';');
                }
                array_appends(&out->inc, argv[i]);
            }
            else if (strcmp(argv[i] + 1, "E") == 0)
            {
                ++i;
                if (i == argc)
                {
                    fprintf(stderr, "error: expected filename after %s\n", argv[i - 1]);
                    return usage();
                }
                if (out->env_file)
                {
                    fprintf(stderr, "error: env file already specified\n");
                    return usage();
                }
                out->env_file = argv[i];
            }
            else if (strcmp(argv[i] + 1, "D") == 0)
            {
                ++i;
                if (i == argc)
                {
                    fprintf(stderr, "error: expected macro name after %s\n", argv[i - 1]);
                    return usage();
                }
                array_appends(&out->macro_name, argv[i]);
                array_push_byte(&out->macro_name, 0);
            }
            else
            {
                fprintf(stderr, "error: unrecognized flag %s\n", argv[i]);
                return usage();
            }
        }
        else
        {
            if (out->input)
            {
                fprintf(stderr, "error: input already specified\n");
                return usage();
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
        return usage();
    }

    return 0;
}

int main(int argc, const char* const* argv)
{
    int rc = 0;
    struct Arguments args = {};
    if (rc = parse_arguments(argc, argv, &args)) return rc;

    const char* asm_file = args.fCompile && args.output ? args.output : "test.asm";
    FILE* f = fopen(asm_file, "wbT");

    if (!f)
    {
        char buf[256];
        snprintf(buf, 256, "%s", asm_file);
        perror(buf);
        return 1;
    }

    struct FrontEnd fe;
    fe_init(&fe, args.inc.data, args.macro_name.data, args.macro_name.sz);
    fe.fout = f;
    rc = fe_lex_file(&fe, args.input);
    fe_destroy(&fe);
    fclose(f);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    else if (!args.fCompile)
    {
        const char* exe_file = args.output ? args.output : "test.exe";
        char buf[512];
        rc = sizeof(buf) <=
             snprintf(buf, sizeof(buf), "ml64 /Zd /Zi /Sa %s /link /OPT:REF /OPT:ICF /out:%s", asm_file, exe_file);
        if (!rc) rc = system(buf);
    }
    return rc;
}
