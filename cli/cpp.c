#include <fcntl.h>
#include <io.h>
#include <stdio.h>
#include <stdlib.h>

#include "errors.h"
#include "fe.h"
#include "lexstate.h"
#include "preproc.h"
#include "tok.h"
#include "token.h"

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

int usage()
{
    fprintf(stderr, "Usage: mindustry.compiler [-o <output>] [-I <path>] [-c] <file.c>\n");
    return 1;
}

struct Arguments
{
    const char* input;
    const char* output;
    struct Array inc;
    char fCompile;
};

static int parse_arguments(int argc, const char* const* argv, struct Arguments* out)
{
    for (int i = 1; i < argc; ++i)
    {
        if (*argv[i] == '-' || *argv[i] == '/')
        {
            if (strcmp(argv[i] + 1, "o") == 0)
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
                    fprintf(stderr, "error: expected filename after %s\n", argv[i - 1]);
                    return usage();
                }
                if (out->inc.sz)
                {
                    array_push_byte(&out->inc, ';');
                }
                array_appends(&out->inc, argv[i]);
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

    if (out->inc.sz)
    {
        array_push_byte(&out->inc, ';');
    }
    char* inc = getenv("INCLUDE");
    if (inc) array_appends(&out->inc, inc);
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

    FILE* f = args.output ? fopen(args.output, "wbT") : stdout;

    if (!f)
    {
        char buf[256];
        snprintf(buf, 256, "%s", args.output);
        perror(buf);
        return 1;
    }

    struct FrontEnd fe;
    fe_init(&fe, args.inc.data);
    fe.fout = f;
    rc = fe_preproc(&fe, args.input);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    else
    {
        const struct Token* data = fe.pp->toks.data;
        for (size_t i = 0; i < array_size(&fe.pp->toks, sizeof(struct Token)); ++i)
        {
            printf("%-21s : %s\n", lexstate_to_string(data[i].basic_type), pp_token_str(fe.pp, data + i));
        }
    }
    fe_destroy(&fe);
    if (args.output) fclose(f);
    return rc;
}
