#include <stdio.h>
#include <stdlibe.h>

#include "errors.h"
#include "lexstate.h"
#include "string.h"
#include "tok.h"
#include "unwrap.h"

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
#ifdef _WIN32
    if (ch == '\t')
        l->col = (l->col + 7) / 8 * 8 + 1; // round to next 8-width tab stop
    else
#endif
        if (ch == '\n')
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
    switch (c1)
    {
        case '+': return c2 == '+' || c2 == '=';
        case '-': return c2 == '-' || c2 == '=' || c2 == '>';
        case '*': return c2 == '=';
        case '|': return c2 == '|' || c2 == '=';
        case '&': return c2 == '&' || c2 == '=';
        case '^': return c2 == '=';
        case '=':
        case '!': return c2 == '=';
        case '>':
        case '<': return c2 == '=' || c2 == c1;
        case '#': return c2 == '#';
        case '.': return c2 == '.';
        default: return 0;
    }
}
static int symbol_is_compound3(char c1, char c2, char c3)
{
    if (c1 == '.' && c2 == '.') return c3 == '.';
    if (c1 == '<' && c2 == '<') return c3 == '=';
    if (c1 == '>' && c2 == '>') return c3 == '=';
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
    if (i == sz) return 1;
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
        case LEX_START:
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
                else if (ch == '\'')
                {
                    advance_rowcol(&l->rc, ch);
                    ++i;
                    goto LEX_CHARLIT;
                }
                else
                {
                    l->tok[0] = ch;
                    l->sz = 1;
                    advance_rowcol(&l->rc, ch);
                    ++i;
                    goto LEX_SYMBOL;
                }
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
                    UNWRAP(emit_token(l));
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
                    if (l->tok[0] == '.' && is_ascii_digit(ch))
                    {
                        // actually matches a digit
                        goto LEX_NUMBER;
                    }
                }
                else if (l->sz == 2)
                {
                    if (symbol_is_compound3(l->tok[0], l->tok[1], ch))
                    {
                        l->tok[2] = ch;
                        l->sz = 3;
                        continue;
                    }
                }
                UNWRAP(emit_token(l));
                goto LEX_START;
            }
            break;
        LEX_CHARLIT:
            l->state = LEX_CHARLIT;
        case LEX_CHARLIT:
            for (; i < sz; advance_rowcol(&l->rc, buf[i++]))
            {
                HANDLE_BACKSLASH_NL();
                const char ch = buf[i];
                if (ch == '\n')
                {
                    return parser_ferror(&l->rc, "error: unexpected end of line in character literal\n");
                }
                else if (l->escape)
                {
                    l->escape = 0;
                    // TODO: handle escape characters
                    UNWRAP(push_tok_char(l, ch));
                }
                else if (ch == '\'')
                {
                    // finish string
                    UNWRAP(emit_token(l));
                    advance_rowcol(&l->rc, buf[i++]);
                    goto LEX_START;
                }
                else if (ch == '\\')
                {
                    l->escape = 1;
                    UNWRAP(push_tok_char(l, ch));
                }
                else
                {
                    UNWRAP(push_tok_char(l, ch));
                }
            }
            break;
        LEX_STRING:
            l->state = LEX_STRING;
        case LEX_STRING:
            for (; i < sz; advance_rowcol(&l->rc, buf[i++]))
            {
                HANDLE_BACKSLASH_NL();
                const char ch = buf[i];
                if (ch == '\n')
                {
                    return parser_ferror(&l->rc, "error: unexpected end of line in string literal\n");
                }
                else if (l->escape)
                {
                    l->escape = 0;
                    // TODO: handle escape characters
                    UNWRAP(push_tok_char(l, ch));
                }
                else if (ch == '"')
                {
                    // finish string
                    UNWRAP(emit_token(l));
                    advance_rowcol(&l->rc, buf[i++]);
                    goto LEX_START;
                }
                else if (ch == '\\')
                {
                    l->escape = 1;
                    UNWRAP(push_tok_char(l, ch));
                }
                else
                {
                    UNWRAP(push_tok_char(l, ch));
                }
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
                    UNWRAP(emit_token(l));
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
                    UNWRAP(emit_token(l));
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
                    UNWRAP(push_tok_char(l, ch));
                }
                else
                {
                    UNWRAP(emit_token(l));
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
                if (is_ascii_alnumu(ch))
                {
                    UNWRAP(push_tok_char(l, ch));
                }
                else
                {
                    const char back = l->tok[l->sz - 1];
                    if (back != '\'' && (ch == '\'' || ch == '.'))
                    {
                        UNWRAP(push_tok_char(l, ch));
                    }
                    else if ((back == 'e' || back == 'E' || back == 'p' || back == 'P') && (ch == '+' || ch == '-'))
                    {
                        UNWRAP(push_tok_char(l, ch));
                    }
                    else
                    {
                        UNWRAP(emit_token(l));
                        goto LEX_START;
                    }
                }
            }
            break;
        default:
            parser_print_errors(stderr);
            fflush(NULL);
            abort();
            rc = parser_ferror(&l->rc, "error: unknown lexer state %u\n", l->state);
    }
fail:
    return rc;
}

int end_lex(Lexer* l)
{
    int rc = 0;
    if (l->state == LEX_STRING)
    {
        UNWRAP(parser_ferror(&l->rc, "error: unexpected end of file in string literal\n"));
    }
    if (l->state == LEX_CHARLIT)
    {
        UNWRAP(parser_ferror(&l->rc, "error: unexpected end of file in string literal\n"));
    }
    if (l->state == LEX_MULTILINE_COMMENT)
    {
        UNWRAP(parser_ferror(&l->rc, "error: unexpected end of file inside comment\n"));
    }
    if (l->sz > 0)
    {
        UNWRAP(emit_token(l));
    }
    l->ws_before = 1;
    l->not_first = 0;
    l->tok_rc = l->rc;
    l->state = LEX_EOF;
    UNWRAP(emit_token(l));
fail:
    return rc;
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

int lit_to_uint64(const char* s, uint64_t* out, const struct RowCol* rc)
{
    uint64_t v = 0;
    size_t i = 0;
    if (s[0] == '0')
    {
        // octal, hex, binary, or zero
        if (s[1] == 'x' || s[1] == 'X')
        {
            // hex
            for (i = 2; s[i]; ++i)
            {
                v = (((v & (UINT64_MAX >> 4))) << 4);
                if (s[i] >= '0' && s[i] <= '9')
                    v += s[i] - '0';
                else if (s[i] >= 'a' && s[i] <= 'f')
                    v += s[i] - 'a' + 10;
                else if (s[i] >= 'A' && s[i] <= 'F')
                    v += s[i] - 'A' + 10;
                else
                    break;
            }
        }
        else if (s[1] == 'b' || s[1] == 'B')
        {
            // binary
            for (i = 2; s[i]; ++i)
            {
                v = (((v & (UINT64_MAX >> 1))) << 1);
                if (s[i] == '0')
                    ;
                else if (s[i] == '1')
                    v += 1;
                else
                    break;
            }
        }
        else if (s[1] >= '0' && s[1] <= '9')
        {
            // octal
            for (i = 1; s[i]; ++i)
            {
                v = (((v & (UINT64_MAX >> 3))) << 3);
                if (s[i] >= '0' && s[i] <= '7')
                    v += s[i] - '0';
                else
                    break;
            }
        }
        else if (s[1] == '\0')
        {
            i = 1;
        }
    }
    else
    {
        for (; s[i]; ++i)
        {
            v = (((v & (UINT64_MAX >> 4))) * 10);
            if (s[i] >= '0' && s[i] <= '9')
                v += s[i] - '0';
            else
                break;
        }
    }
    if (s[i] == 'U')
    {
        ++i;
    }
    if (s[i] == 'L' || s[i] == 'l')
    {
        ++i;
    }
    if (s[i] == 'L' || s[i] == 'l')
    {
        ++i;
    }
    if (s[i] != '\0')
    {
        return parser_ferror(rc, "error: unexpected character in number literal: '%c'\n", s[i]);
    }

    *out = v;
    return 0;
}
