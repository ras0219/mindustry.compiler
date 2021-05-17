#include <stdio.h>

#include "lexstate.h"
#include "string.h"
#include "tok.h"

static int is_ascii_alphu(int ch) { return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'; }
static int is_ascii_digit(int ch) { return '0' <= ch && ch <= '9'; }
static int is_ascii_alnumu(int ch) { return is_ascii_alphu(ch) || is_ascii_digit(ch); }
static int is_ascii_symbol(int ch)
{
    return ch == '/' || ch == '(' || ch == ')' || ch == '=' || ch == ',' || ch == ';' || ch == '[' || ch == ']' ||
           ch == '*' || ch == '%' || ch == '&' || ch == '+' || ch == '-' || ch == '<' || ch == '>' || ch == '{' ||
           ch == '}' || ch == ':';
}

static int emit_token(Lexer* l)
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

static void advance_rowcol(struct RowCol* l, int ch)
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

void init_lexer(Lexer* l, int (*f_on_token)(struct Lexer*))
{
    l->f_on_token = f_on_token;
    l->state = LEX_START;
    l->sz = 0;
    l->rc.row = 1;
    l->rc.col = 1;
    l->tok_rc = l->rc;
}

static int push_tok_char(Lexer* l, char ch)
{
    if (l->sz == sizeof(l->tok) - 1)
    {
        snprintf(s_error_buffer,
                 sizeof(s_error_buffer),
                 "%d:%d: error: overflowed identifier buffer\n",
                 l->rc.row,
                 l->rc.col);
        return 1;
    }
    l->tok[l->sz++] = ch;
    return 0;
}

int lex(Lexer* l, Buffer* buf)
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
                if (ch == ' ' || ch == '\n' || ch == '\r') continue;
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
                    l->sz = 1;
                    l->state = LEX_SYMBOL;
                    advance_rowcol(&l->rc, buf->buf[i++]);
                    goto LEX_SYMBOL;
                }
                else if (ch == '"')
                {
                    l->tok_rc = l->rc;
                    l->state = LEX_STRING;
                    advance_rowcol(&l->rc, buf->buf[i++]);
                    goto LEX_STRING;
                }
                else
                {
                    snprintf(s_error_buffer,
                             sizeof(s_error_buffer),
                             "%d:%d: error: unrecognized character: '%c'\n",
                             l->rc.row,
                             l->rc.col,
                             ch);
                    return 1;
                }
            }
            break;
        case LEX_SYMBOL:
        LEX_SYMBOL:
            for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
            {
                const char ch = buf->buf[i];
                if (l->tok[0] == '/' && ch == '*')
                {
                    // multi-line comment
                    advance_rowcol(&l->rc, buf->buf[i++]);
                    l->sz = 0;
                    l->state = LEX_MULTILINE_COMMENT;
                    goto LEX_MULTILINE_COMMENT;
                }
                l->tok[l->sz] = 0;
                if (rc = emit_token(l)) return rc;
                l->state = LEX_START;
                goto LEX_START;
            }
            break;
        case LEX_STRING:
        LEX_STRING:
            for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
            {
                const char ch = buf->buf[i];
                if (ch == '"')
                {
                    // finish string
                    if (rc = emit_token(l)) return rc;
                    advance_rowcol(&l->rc, buf->buf[i++]);
                    l->state = LEX_START;
                    goto LEX_START;
                }
                else if (ch == '\\')
                {
                    // escape
                    advance_rowcol(&l->rc, buf->buf[i++]);
                    l->state = LEX_STRING1;
                    goto LEX_STRING1;
                }
                else if (ch == '\n')
                {
                    snprintf(s_error_buffer,
                             sizeof(s_error_buffer),
                             "%d:%d: error: unexpected end of line in string literal\n",
                             l->rc.row,
                             l->rc.col);
                    return 1;
                }
                else
                {
                    int rc;
                    if (rc = push_tok_char(l, ch)) return rc;
                }
            }
            break;
        case LEX_STRING1:
        LEX_STRING1:
            if (i < buf->sz)
            {
                const char ch = buf->buf[i];
                if (ch == '\n')
                {
                    snprintf(s_error_buffer,
                             sizeof(s_error_buffer),
                             "%d:%d: error: unexpected end of line in string literal\n",
                             l->rc.row,
                             l->rc.col);
                    return 1;
                }
                else
                {
                    // TODO: handle escape characters
                    int rc;
                    if (rc = push_tok_char(l, ch)) return rc;
                    advance_rowcol(&l->rc, buf->buf[i++]);
                }
                l->state = LEX_STRING;
                goto LEX_STRING;
            }
            break;
        case LEX_MULTILINE_COMMENT:
        LEX_MULTILINE_COMMENT:
            for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
            {
                const char ch = buf->buf[i];
                if (ch == '*')
                {
                    l->sz = 1;
                }
                else if (ch == '/' && l->sz == 1)
                {
                    l->tok[0] = 0;
                    l->sz = 0;
                    if (rc = emit_token(l)) return rc;
                    advance_rowcol(&l->rc, buf->buf[i++]);
                    l->state = LEX_START;
                    goto LEX_START;
                }
                else
                {
                    l->sz = 0;
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
                    int rc;
                    if (rc = push_tok_char(l, ch)) return rc;
                }
                else
                {
                    if (l->sz == sizeof("__attribute__") - 1 && memcmp("__attribute__", l->tok, l->sz) == 0)
                    {
                        l->state = LEX_ATTRIBUTE;
                    }
                    else if (l->sz == sizeof("int") - 1 && memcmp("int", l->tok, l->sz) == 0)
                    {
                        l->state = LEX_INT;
                    }
                    else if (l->sz == sizeof("void") - 1 && memcmp("void", l->tok, l->sz) == 0)
                    {
                        l->state = LEX_VOID;
                    }
                    else if (l->sz == sizeof("goto") - 1 && memcmp("goto", l->tok, l->sz) == 0)
                    {
                        l->state = LEX_GOTO;
                    }
                    else if (l->sz == sizeof("return") - 1 && memcmp("return", l->tok, l->sz) == 0)
                    {
                        l->state = LEX_RETURN;
                    }
                    if (rc = emit_token(l)) return rc;
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
                        int rc;
                        if (rc = push_tok_char(l, ch)) return rc;
                    }
                    l->tok[l->sz++] = ch;
                }
                else
                {
                    if (rc = emit_token(l)) return rc;
                    l->state = LEX_START;
                    goto LEX_START;
                }
            }
            break;
    }
    return 0;
}

int end_lex(Lexer* l)
{
    if (l->state == LEX_STRING || l->state == LEX_STRING1)
    {
        snprintf(s_error_buffer,
                 sizeof(s_error_buffer),
                 "%d:%d: error: unexpected end of file in string literal\n",
                 l->rc.row,
                 l->rc.col);
        return 1;
    }
    if (l->state == LEX_MULTILINE_COMMENT)
    {
        snprintf(s_error_buffer,
                 sizeof(s_error_buffer),
                 "%d:%d: error: unexpected end of file inside comment\n",
                 l->rc.row,
                 l->rc.col);
        return 1;
    }
    if (l->sz > 0)
    {
        l->tok[l->sz] = 0;
        int rc;
        if (rc = emit_token(l)) return rc;
    }
    l->tok_rc = l->rc;
    l->state = LEX_EOF;
    return emit_token(l);
}

const char* lexstate_to_string(enum LexerState_t s)
{
    switch (s)
    {
        case LEX_NUMBER: return "LEX_NUMBER";
        case LEX_START: return "LEX_START";
        case LEX_SYMBOL: return "LEX_SYMBOL";
        case LEX_COMMENT: return "LEX_COMMENT";
        case LEX_EOF: return "LEX_EOF";
        case LEX_IDENT: return "LEX_IDENT";
        case LEX_MULTILINE_COMMENT: return "LEX_MULTILINE_COMMENT";
        default: return "LEX_UNKNOWN";
    }
}
