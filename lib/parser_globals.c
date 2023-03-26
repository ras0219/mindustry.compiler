#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "errors.h"
#include "tok.h"
#include "token.h"

static struct FixedMsgBuffer
{
    size_t used;
    char buf[4096];
} s_error = {}, s_warn = {};

static int parser_vfmsg_impl(struct FixedMsgBuffer* buf, const struct RowCol* rc, const char* fmt, va_list argp)
{
    if (buf->used < sizeof(buf->buf) && rc)
    {
        buf->used +=
            snprintf(buf->buf + buf->used, sizeof(buf->buf) - buf->used, "%s:%d:%d: ", rc->file, rc->row, rc->col);
    }
    if (buf->used < sizeof(buf->buf))
    {
        buf->used += vsnprintf(buf->buf + buf->used, sizeof(buf->buf) - buf->used, fmt, argp);
    }
#if defined(FAIL_ON_FIRST)
    if (buf == &s_error)
    {
        parser_print_msgs(stderr);
        fflush(stderr);
        abort();
    }
#endif
    return 1;
}

int parser_has_errors() { return s_error.used > 0; }

int parser_has_warnings() { return s_warn.used > 0; }

int parser_tok_error(const struct Token* tok, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);

    parser_vfmsg_impl(&s_error, tok ? &tok->rc : NULL, fmt, argp);

    va_end(argp);
    return 1;
}

int parser_ferror(const struct RowCol* rc, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);

    parser_vfmsg_impl(&s_error, rc, fmt, argp);

    va_end(argp);
    return 1;
}

int parser_vferror(const struct RowCol* rc, const char* fmt, va_list argp)
{
    return parser_vfmsg_impl(&s_error, rc, fmt, argp);
}

int parser_tok_warn(const struct Token* tok, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);

    parser_vwarn(token_rc(tok), fmt, argp);

    va_end(argp);
    return 1;
}
int parser_warn(const struct RowCol* rc, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);

    parser_vwarn(rc, fmt, argp);

    va_end(argp);
    return 1;
}
int parser_vwarn(const struct RowCol* rc, const char* fmt, va_list argp)
{
    return parser_vfmsg_impl(&s_warn, rc, fmt, argp);
}

int parser_fmsg(enum LogChannel chan, const struct RowCol* rc, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);

    parser_vfmsg_impl(chan == error ? &s_error : &s_warn, rc, fmt, argp);

    va_end(argp);
    return 1;
}

int parser_vfmsg(enum LogChannel chan, const struct RowCol* rc, const char* fmt, va_list argp)
{
    return parser_vfmsg_impl(chan == error ? &s_error : &s_warn, rc, fmt, argp);
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
void parser_clear_errors()
{
    s_error.used = 0;
    s_warn.used = 0;
}

void parser_print_errors(FILE* f) { fprintf(f, "%.*s", (int)s_error.used, s_error.buf); }
void parser_print_warns(FILE* f) { fprintf(f, "%.*s", (int)s_warn.used, s_warn.buf); }
void parser_print_msgs(FILE* f)
{
    parser_print_errors(f);
    parser_print_warns(f);
}

size_t parser_print_msgs_mem(void* buf, size_t sz)
{
    size_t n = sz < s_error.used ? sz : s_error.used;
    memcpy(buf, s_error.buf, n);
    sz -= n;

    size_t m = sz < s_warn.used ? sz : s_warn.used;
    memcpy(buf, s_warn.buf, m);
    sz -= m;
    return n + m;
}