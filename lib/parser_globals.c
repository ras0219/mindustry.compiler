#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>

#include "errors.h"
#include "tok.h"
#include "token.h"

static size_t s_error_buf_used = 0;
static char s_error_buffer[4096];

int parser_has_errors() { return s_error_buf_used > 0; }

int parser_tok_error(const struct Token* tok, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);

    if (tok)
        parser_vferror(&tok->rc, fmt, argp);
    else
        parser_vferror(NULL, fmt, argp);

    va_end(argp);
    return 1;
}

int parser_ferror(const struct RowCol* rc, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);

    parser_vferror(rc, fmt, argp);

    va_end(argp);
    return 1;
}
int parser_vferror(const struct RowCol* rc, const char* fmt, va_list argp)
{
    if (s_error_buf_used < sizeof(s_error_buffer) && rc)
    {
        size_t n = snprintf(s_error_buffer + s_error_buf_used,
                            sizeof(s_error_buffer) - s_error_buf_used,
                            "%s:%d:%d: ",
                            rc->file,
                            rc->row,
                            rc->col);
        s_error_buf_used += n;
    }
    if (s_error_buf_used < sizeof(s_error_buffer))
    {
        s_error_buf_used +=
            vsnprintf(s_error_buffer + s_error_buf_used, sizeof(s_error_buffer) - s_error_buf_used, fmt, argp);
    }
    return 1;
}

int parser_ice_tok(const struct Token* tok)
{
    parser_tok_error(tok, "error: an internal compiler error has occurred.\n");
    return 1;
}

int parser_ice(const struct RowCol* rc)
{
    parser_ferror(rc, "error: an internal compiler error has occurred.\n");
    return 1;
}
void parser_clear_errors() { s_error_buf_used = 0; }
void parser_print_errors(FILE* f) { fprintf(f, "%.*s", (int)s_error_buf_used, s_error_buffer); }
