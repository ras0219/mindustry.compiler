#include <stdio.h>
#include <stdlib.h>

#include "fe.h"
#include "tok.h"

int usage()
{
    fprintf(stderr, "Usage: mindustry.compiler <file.c>\n");
    return 1;
}

int main(int argc, const char* const* argv)
{
    if (argc != 2)
    {
        return usage();
    }
    struct FrontEnd fe;
    fe_init(&fe);
    int rc = fe_lex_file(&fe, argv[1]);
    fe_destroy(&fe);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    return rc;
}
