#include <stdio.h>
#include <stdlibe.h>

#include "errors.h"
#include "lexstate.h"
#include "string.h"
#include "tok.h"

static int is_ascii_alphu(int ch) { return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'; }
static int is_ascii_digit(int ch) { return '0' <= ch && ch <= '9'; }
static int is_ascii_alnumu(int ch) { return is_ascii_alphu(ch) || is_ascii_digit(ch); }

static int emit_token(Lexer* l)
{
    struct RowCol tmp = l->rc;
    l->rc = l->tok_rc;
    l->tok[l->sz] = 0;
    int rc = l->f_on_token(l);
    l->ws_before = 0;
    l->not_first = 1;
    l->rc = tmp;
    l->sz = 0;
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

void init_lexer(Lexer* l, const char* file, int (*f_on_token)(struct Lexer*))
{
    memset(l, 0, sizeof(Lexer));
    l->f_on_token = f_on_token;
    l->rc.file = file;
    l->rc.row = 1;
    l->rc.col = 1;
    l->tok_rc = l->rc;
}

static int push_tok_char(Lexer* l, char ch)
{
    if (l->sz == sizeof(l->tok) - 1)
    {
        return parser_ferror(&l->rc, "error: overflowed token buffer\n"), 1;
    }
    l->tok[l->sz++] = ch;
    return 0;
}

static int symbol_is_compound(char c1, char c2)
{
    if (c1 == '+') return c2 == '+' || c2 == '=';
    if (c1 == '-') return c2 == '-' || c2 == '=' || c2 == '>';
    if (c1 == '|' && c2 == '|')
    {
        return 1;
    }
    if (c1 == '&' && c2 == '&')
    {
        return 1;
    }
    if (c1 == '=' || c1 == '!' || c1 == '>' || c1 == '<')
    {
        if (c2 == '=')
        {
            return 1;
        }
    }
    if (c1 == '#' && c2 == '#') return 1;
    return 0;
}

__forceinline static int handle_backslash_nl(struct Lexer* const l, const char* const buf, size_t const sz, size_t* p_i)
{
    size_t i = *p_i;
    while (i != sz)
    {
        const char ch = buf[i];
        if (ch == '\\')
        {
            if (sz == i + 1)
            {
                l->backslash = 1;
                return 1;
            }
            if (buf[i + 1] == '\r')
            {
                advance_rowcol(&l->rc, '\n');
                if (i + 2 == sz)
                {
                    l->skip_next = 1;
                    return 1;
                }
                i += 3;
                continue;
            }
            else if (buf[i + 1] == '\n')
            {
                advance_rowcol(&l->rc, '\n');
                i += 2;
                continue;
            }
        }
        break;
    }
    *p_i = i;
    return 0;
}

#define HANDLE_BACKSLASH_NL()                                                                                          \
    do                                                                                                                 \
    {                                                                                                                  \
        if (handle_backslash_nl(l, buf, sz, &i)) return 0;                                                             \
    } while (0)

int lex(Lexer* const l, const char* const buf, size_t const sz)
{
    if (!sz) return 0;

    int rc = 0;
    size_t i = 0;

    while (l->backslash)
    {
        l->backslash = 0;
        char slashbuf[2] = {'\\', buf[i++]};
        rc = lex(l, slashbuf, 2);
        if (rc) return rc;
        if (i == sz) return 0;
    }

    if (l->skip_next)
    {
        l->skip_next = 0;
        ++i;
        if (i == sz) return 0;
    }

    switch (l->state)
    {
    LEX_START:
        l->state = LEX_START;
        l->ws_sensitive = 0;
        case LEX_START:
        {
            if (!l->ws_sensitive)
            {
                for (; i < sz; advance_rowcol(&l->rc, buf[i++]))
                {
                    HANDLE_BACKSLASH_NL();
                    const char ch = buf[i];
                    if (ch == ' ' || ch == '\t')
                    {
                        l->ws_before = 1;
                        continue;
                    }
                    if (ch == '\n' || ch == '\r')
                    {
                        l->ws_before = 1;
                        l->not_first = 0;
                        continue;
                    }
                    l->ws_sensitive = 1;
                    break;
                }
            }
            if (i == sz) return 0;
            HANDLE_BACKSLASH_NL();
            const char ch = buf[i];
            l->tok_rc = l->rc;
            if (l->expect_header)
            {
                l->expect_header = 0;
                if (ch != '<' && ch != '"')
                {
                    return parser_ferror(&l->rc, "error: expected header to include but found '%c'\n", ch);
                }
                l->tok[0] = ch;
                l->sz = 1;
                advance_rowcol(&l->rc, ch);
                ++i;
                goto LEX_HEADER;
            }
            else if (is_ascii_alphu(ch))
            {
                goto LEX_IDENT;
            }
            else if (is_ascii_digit(ch))
            {
                goto LEX_NUMBER;
            }
            else if (ch == '"')
            {
                advance_rowcol(&l->rc, ch);
                ++i;
                goto LEX_STRING;
            }
            else
            {
                l->tok[0] = ch;
                l->sz = 1;
                advance_rowcol(&l->rc, ch);
                ++i;
                goto LEX_SYMBOL;
            }
            break;
        }
        LEX_HEADER:
            l->state = LEX_HEADER;
        case LEX_HEADER:
        {
            const char closing_ch = l->tok[0] == '<' ? '>' : '"';
            for (; i < sz; advance_rowcol(&l->rc, buf[i++]))
            {
                HANDLE_BACKSLASH_NL();
                const char ch = buf[i];
                if (ch == '\n' || ch == '\r')
                {
                    return parser_ferror(&l->rc, "error: expected '%c' but found newline\n", closing_ch);
                }
                if (rc = push_tok_char(l, ch)) return rc;

                if (ch == closing_ch)
                {
                    if (rc = emit_token(l)) return rc;
                    advance_rowcol(&l->rc, buf[i++]);
                    goto LEX_START;
                }
            }
            break;
        }
        LEX_SYMBOL:
            l->state = LEX_SYMBOL;
        case LEX_SYMBOL:
            for (; i < sz; advance_rowcol(&l->rc, buf[i++]))
            {
                HANDLE_BACKSLASH_NL();
                const char ch = buf[i];
                if (l->sz == 1)
                {
                    if (l->tok[0] == '/' && ch == '*')
                    {
                        // multi-line comment
                        advance_rowcol(&l->rc, buf[i++]);
                        l->sz = 0;
                        goto LEX_MULTILINE_COMMENT;
                    }
                    if (l->tok[0] == '/' && ch == '/')
                    {
                        // single-line comment
                        advance_rowcol(&l->rc, buf[i++]);
                        l->sz = 0;
                        goto LEX_COMMENT;
                    }
                    if (symbol_is_compound(l->tok[0], ch))
                    {
                        l->tok[1] = ch;
                        l->sz = 2;
                        continue;
                    }
                    if (l->tok[0] == '.' && ch == '.')
                    {
                        // ellipsis
                        l->tok[1] = ch;
                        l->sz = 2;
                        continue;
                    }
                }
                else if (l->sz == 2)
                {
                    if (l->tok[0] == '.' && l->tok[1] == '.')
                    {
                        if (ch == '.')
                        {
                            l->tok[2] = ch;
                            l->sz = 3;
                            continue;
                        }
                        else
                        {
                            return parser_ferror(&l->rc, "error: expected third '.' in ellipsis\n");
                        }
                    }
                }
                if (rc = emit_token(l)) return rc;
                goto LEX_START;
            }
            break;
        LEX_STRING:
            l->state = LEX_STRING;
        case LEX_STRING:
            for (; i < sz; advance_rowcol(&l->rc, buf[i++]))
            {
                HANDLE_BACKSLASH_NL();
                const char ch = buf[i];
                if (ch == '"')
                {
                    // finish string
                    if (rc = emit_token(l)) return rc;
                    advance_rowcol(&l->rc, buf[i++]);
                    goto LEX_START;
                }
                else if (ch == '\\')
                {
                    // escape
                    advance_rowcol(&l->rc, buf[i++]);
                    goto LEX_STRING1;
                }
                else if (ch == '\n')
                {
                    return parser_ferror(&l->rc, "error: unexpected end of line in string literal\n");
                }
                else
                {
                    if (rc = push_tok_char(l, ch)) return rc;
                }
            }
            break;
        LEX_STRING1:
            l->state = LEX_STRING1;
        case LEX_STRING1:
            if (i < sz)
            {
                HANDLE_BACKSLASH_NL();
                const char ch = buf[i];
                if (ch == '\n')
                {
                    advance_rowcol(&l->rc, buf[i++]);
                }
                else
                {
                    // TODO: handle escape characters
                    if (rc = push_tok_char(l, ch)) return rc;
                    advance_rowcol(&l->rc, buf[i++]);
                }
                goto LEX_STRING;
            }
            break;
        LEX_MULTILINE_COMMENT:
            l->state = LEX_MULTILINE_COMMENT;
        case LEX_MULTILINE_COMMENT:
            for (; i < sz; advance_rowcol(&l->rc, buf[i++]))
            {
                HANDLE_BACKSLASH_NL();
                const char ch = buf[i];
                if (ch == '*')
                {
                    l->sz = 1;
                }
                else if (ch == '/' && l->sz == 1)
                {
                    l->sz = 0;
                    if (rc = emit_token(l)) return rc;
                    advance_rowcol(&l->rc, buf[i++]);
                    goto LEX_START;
                }
                else
                {
                    l->sz = 0;
                }
            }
            break;
        LEX_COMMENT:
            l->state = LEX_COMMENT;
        case LEX_COMMENT:
            for (; i < sz; advance_rowcol(&l->rc, buf[i++]))
            {
                HANDLE_BACKSLASH_NL();
                const char ch = buf[i];
                if (ch == '\r' || ch == '\n')
                {
                    l->sz = 0;
                    if (rc = emit_token(l)) return rc;
                    goto LEX_START;
                }
            }
            break;
        LEX_IDENT:
            l->state = LEX_IDENT;
        case LEX_IDENT:
            for (; i < sz; advance_rowcol(&l->rc, buf[i++]))
            {
                HANDLE_BACKSLASH_NL();
                const char ch = buf[i];
                if (is_ascii_alnumu(ch))
                {
                    if (rc = push_tok_char(l, ch)) return rc;
                }
                else
                {
                    if (rc = emit_token(l)) return rc;
                    goto LEX_START;
                }
            }
            break;
        LEX_NUMBER:
            l->state = LEX_NUMBER;
        case LEX_NUMBER:
            for (; i < sz; advance_rowcol(&l->rc, buf[i++]))
            {
                HANDLE_BACKSLASH_NL();
                const char ch = buf[i];
                if (is_ascii_digit(ch))
                {
                    if (l->sz == sizeof(l->tok) - 1)
                    {
                        if (rc = push_tok_char(l, ch)) return rc;
                    }
                    l->tok[l->sz++] = ch;
                }
                else
                {
                    if (rc = emit_token(l)) return rc;
                    goto LEX_START;
                }
            }
            break;
        default: rc = parser_ferror(&l->rc, "error: unknown lexer state %u\n", l->state);
    }
    return rc;
}

int end_lex(Lexer* l)
{
    if (l->state == LEX_STRING || l->state == LEX_STRING1)
    {
        return parser_ferror(&l->rc, "error: unexpected end of file in string literal\n"), 1;
    }
    if (l->state == LEX_MULTILINE_COMMENT)
    {
        return parser_ferror(&l->rc, "error: unexpected end of file inside comment\n"), 1;
    }
    int rc;
    if (l->sz > 0)
    {
        if (rc = emit_token(l)) return rc;
    }
    l->ws_before = 1;
    l->not_first = 0;
    l->tok_rc = l->rc;
    l->state = LEX_EOF;
    return emit_token(l);
}

const char* lexstate_to_string(unsigned int s)
{
#define Y(E, ...)                                                                                                      \
    case E: return #E;

    switch (s)
    {
        X_LEX_STATES(Y)
        default: return "unknown";
    }
}
