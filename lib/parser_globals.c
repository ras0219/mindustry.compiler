#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>

#include "tok.h"

size_t s_error_buf_used = 0;
char s_error_buffer[1024];

int parser_has_errors() { return s_error_buf_used > 0; }

int parser_ferror(const struct RowCol* rc, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);

    if (s_error_buf_used < sizeof(s_error_buffer))
    {
        size_t n = snprintf(s_error_buffer + s_error_buf_used,
                            sizeof(s_error_buffer) - s_error_buf_used,
                            "%s(%d,%d): ",
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
int parser_ice(const struct RowCol* rc)
{
    parser_ferror(rc, "error: an internal compiler error has occurred.\n");
    return 1;
}
void parser_clear_errors() { s_error_buf_used = 0; }
void parser_print_errors(FILE* f) { fprintf(f, "%.*s", (int)s_error_buf_used, s_error_buffer); }
