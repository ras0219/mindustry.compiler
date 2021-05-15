#include "tok.h"
#include "lexstate.h"
#include <stdio.h>

static int is_ascii_alphu(int ch) { return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'; }
static int is_ascii_digit(int ch) { return '0' <= ch && ch <= '9'; }
static int is_ascii_alnumu(int ch) { return is_ascii_alphu(ch) || is_ascii_digit(ch); }
static int is_ascii_symbol(int ch) { return ch == '(' || ch == ')' || ch == '=' || ch == ',' || ch == ';' || ch == '[' || ch == ']' || ch == '*' || ch == '%' || ch == '&' || ch == '+' || ch == '-' || ch == '<' || ch == '>'; }

static int emit_token(Lexer *l)
{
    struct RowCol tmp = l->rc;
    l->rc = l->tok_rc;
    l->tok[l->sz] = 0;
    int rc = l->f_on_token(l);
    if (!rc)
    {
        l->rc = tmp;
        l->sz = 0;
    }
    return rc;
}

static void advance_rowcol(struct RowCol *l, int ch)
{
    if (ch == '\t')
        l->col = (l->col + 7) / 8 * 8 + 1; // round to next 8-width tab stop
    else if (ch == '\n')
    {
        l->row++;
        l->col = 1;
    }
    else
    {
        ++l->col;
    }
}

void init_lexer(Lexer *l, int (*f_on_token)(struct Lexer *))
{
    l->f_on_token = f_on_token;
    l->state = LEX_START;
    l->sz = 0;
    l->rc.row = 1;
    l->rc.col = 1;
    l->tok_rc = l->rc;
}

int lex(Lexer *l, Buffer *buf)
{
    int rc;
    size_t i = 0;
    switch (l->state)
    {
    case LEX_START:
    LEX_START:
        for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
        {
            const char ch = buf->buf[i];
            if (ch == ' ' || ch == '\n' || ch == '\r')
                continue;
            if (is_ascii_alphu(ch))
            {
                l->tok_rc = l->rc;
                l->state = LEX_IDENT;
                goto LEX_IDENT;
            }
            else if (is_ascii_digit(ch))
            {
                l->tok_rc = l->rc;
                l->state = LEX_NUMBER;
                goto LEX_NUMBER;
            }
            else if (is_ascii_symbol(ch))
            {
                l->tok_rc = l->rc;
                l->tok[0] = ch;
                l->tok[1] = 0;
                l->sz = 1;
                l->state = LEX_SYMBOL;
                if (rc = emit_token(l))
                    return rc;
                l->state = LEX_START;
            }
            else
            {
                snprintf(s_error_buffer, sizeof(s_error_buffer), "%d:%d: error: unrecognized character: '%c'\n", l->rc.row, l->rc.col, ch);
                return 1;
            }
        }
        break;
    case LEX_IDENT:
    LEX_IDENT:
        for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
        {
            const char ch = buf->buf[i];
            if (is_ascii_alnumu(ch))
            {
                if (l->sz == sizeof(l->tok) - 1)
                {
                    snprintf(s_error_buffer, sizeof(s_error_buffer), "%d:%d: error: overflowed identifier buffer\n", l->rc.row, l->rc.col);
                    return 1;
                }
                l->tok[l->sz++] = ch;
            }
            else
            {
                if (rc = emit_token(l))
                    return rc;
                l->state = LEX_START;
                goto LEX_START;
            }
        }
        break;
    case LEX_NUMBER:
    LEX_NUMBER:
        for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
        {
            const char ch = buf->buf[i];
            if (is_ascii_digit(ch))
            {
                if (l->sz == sizeof(l->tok) - 1)
                {
                    snprintf(s_error_buffer, sizeof(s_error_buffer), "%d:%d: error: overflowed identifier buffer\n", l->rc.row, l->rc.col);
                    return 1;
                }
                l->tok[l->sz++] = ch;
            }
            else
            {
                if (rc = emit_token(l))
                    return rc;
                l->state = LEX_START;
                goto LEX_START;
            }
        }
        break;
    }
    return 0;
}

int end_lex(Lexer *l)
{
    // todo flush state
    l->state = LEX_EOF;
    return emit_token(l);
}