#include <stdio.h>

#include "fe.h"
#include "tok.h"

int wasmmain(_Bool debug)
{
    parser_clear_errors();
    FrontEnd fe;
    fe_init(&fe);
    fe.parser.cg.fdebug = debug ? stderr : NULL;
    int rc = fe_lex_file(&fe, "main.c");
    fe_destroy(&fe);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    return 0;
}
