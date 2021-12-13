#include <fcntl.h>
#include <io.h>
#include <stdio.h>
#include <stdlib.h>

#include "errors.h"
#include "fe.h"
#include "tok.h"

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

int usage()
{
    fprintf(stderr, "Usage: mindustry.compiler [-o <output>] [-c] <file.c>\n");
    return 1;
}

struct Arguments
{
    const char* input;
    const char* output;
    char fCompile;
};

static int parse_arguments(int argc, const char* const* argv, struct Arguments* out)
{
    for (int i = 1; i < argc; ++i)
    {
        if (*argv[i] == '-' || *argv[i] == '/')
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
                    fprintf(stderr, "error: expected filename after -o\n");
                    return usage();
                }
                if (out->output)
                {
                    fprintf(stderr, "error: output already specified\n");
                    return usage();
                }
                out->output = argv[i];
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
    fe_init(&fe);
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
