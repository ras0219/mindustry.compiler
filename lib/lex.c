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
           ch == '}' || ch == ':' || ch == '!' || ch == '|' || ch == '?' || ch == '%';
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

void init_lexer(Lexer* l, const char* file, int (*f_on_token)(struct Lexer*))
{
    l->f_on_token = f_on_token;
    l->state = LEX_START;
    l->sz = 0;
    l->rc.file = file;
    l->rc.row = 1;
    l->rc.col = 1;
    l->tok_rc = l->rc;
}

static int push_tok_char(Lexer* l, char ch)
{
    if (l->sz == sizeof(l->tok) - 1)
    {
        return parser_ferror(&l->rc, "error: overflowed identifier buffer\n"), 1;
    }
    l->tok[l->sz++] = ch;
    return 0;
}

static int symbol_is_compound(char c1, char c2)
{
    if (c1 == '+') return c2 == '+' || c2 == '=';
    if (c1 == '-') return c2 == '-' || c2 == '=';
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
    return 0;
}

struct KeywordEntry
{
    const char* const txt;
    const size_t len;
    const enum LexerState state;
};

#define KEYWORD(TXT, STATE)                                                                                            \
    {                                                                                                                  \
        TXT, sizeof(TXT) - 1, STATE                                                                                    \
    }

static const struct KeywordEntry s_keywords_table[] = {
    KEYWORD("__attribute__", LEX_ATTRIBUTE),
    KEYWORD("int", LEX_INT),
    KEYWORD("void", LEX_VOID),
    KEYWORD("goto", LEX_RETURN),
    KEYWORD("break", LEX_BREAK),
    KEYWORD("continue", LEX_CONTINUE),
    KEYWORD("switch", LEX_SWITCH),
    KEYWORD("case", LEX_CASE),
    KEYWORD("if", LEX_IF),
    KEYWORD("for", LEX_FOR),
    KEYWORD("while", LEX_WHILE),
    KEYWORD("return", LEX_RETURN),
    KEYWORD("break", LEX_BREAK),
    KEYWORD("do", LEX_DO),
    KEYWORD("auto", LEX_AUTO),
    KEYWORD("__string", LEX_MSTRING),
    KEYWORD("__unit", LEX_UNIT),
    KEYWORD("struct", LEX_STRUCT),
    KEYWORD("short", LEX_INT),
    KEYWORD("char", LEX_INT),
    KEYWORD("long", LEX_INT),
    KEYWORD("register", LEX_REGISTER),
    KEYWORD("const", LEX_CONST),
    KEYWORD("volatile", LEX_VOLATILE),
    KEYWORD("else", LEX_ELSE),
    KEYWORD("static", LEX_STATIC),
};

int lex(Lexer* l, Buffer* buf)
{
    int rc;
    size_t i = 0;
    switch (l->state)
    {
    LEX_START:
        l->state = LEX_START;
        case LEX_START:
        {
            l->in_directive = 0;

            for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
            {
                const char ch = buf->buf[i];
                if (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r') continue;
                if (ch == '#')
                {
                    advance_rowcol(&l->rc, buf->buf[i++]);
                    l->state = LEX_START_DIRECTIVE;
                    l->in_directive = 1;
                    l->tok_rc = l->rc;
                    if (rc = emit_token(l)) return rc;
                    goto LEX_START_DIRECTIVE;
                }
                goto LEX_START2;
            }
            break;
        }
        LEX_START_DIRECTIVE:
            l->state = LEX_START_DIRECTIVE;
        case LEX_START_DIRECTIVE:
            for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
            {
                const char ch = buf->buf[i];
                if (ch == ' ' || ch == '\t') continue;
                if (ch == '\n' || ch == '\r')
                {
                    return parser_ferror(&l->rc, "error: expected preprocessor directive but found newline\n");
                }
                if (is_ascii_alphu(ch))
                {
                    l->tok_rc = l->rc;
                    goto LEX_DIRECTIVE;
                }
                else
                {
                    return parser_ferror(&l->rc, "error: unexpected character in preprocessor directive: '%c'\n", ch);
                }
            }
            break;
        LEX_DIRECTIVE:
            l->state = LEX_DIRECTIVE;
        case LEX_DIRECTIVE:
            for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
            {
                const char ch = buf->buf[i];
                if (is_ascii_alnumu(ch))
                {
                    if (rc = push_tok_char(l, ch)) return rc;
                }
                else
                {
                    const int is_include = l->sz == sizeof("include") - 1 && memcmp(l->tok, "include", l->sz) == 0;
                    if (rc = emit_token(l)) return rc;
                    if (is_include)
                    {
                        goto LEX_DIRECTIVE_INCLUDE_WS;
                    }
                    goto LEX_START2;
                }
            }
            break;
        LEX_DIRECTIVE_INCLUDE_WS:
            l->state = LEX_DIRECTIVE_INCLUDE_WS;
        case LEX_DIRECTIVE_INCLUDE_WS:
            for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
            {
                const char ch = buf->buf[i];
                if (ch == ' ' || ch == '\t') continue;
                if (ch == '\n' || ch == '\r')
                {
                    return parser_ferror(&l->rc, "error: expected include but found newline\n");
                }
                if (ch == '<' || ch == '"')
                {
                    l->tok_rc = l->rc;
                    if (rc = push_tok_char(l, ch)) return rc;
                    advance_rowcol(&l->rc, buf->buf[i++]);
                    goto LEX_DIRECTIVE_INCLUDE;
                }
                goto LEX_START2;
            }
            break;
        LEX_DIRECTIVE_INCLUDE:
            l->state = LEX_DIRECTIVE_INCLUDE;
        case LEX_DIRECTIVE_INCLUDE:
        {
            const char closing_ch = l->tok[0] == '<' ? '>' : '"';
            for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
            {
                const char ch = buf->buf[i];
                if (ch == '\n' || ch == '\r')
                {
                    return parser_ferror(&l->rc, "error: expected '%c' but found newline\n", closing_ch);
                }
                if (rc = push_tok_char(l, ch)) return rc;

                if (ch == closing_ch)
                {
                    if (rc = emit_token(l)) return rc;
                    advance_rowcol(&l->rc, closing_ch);
                    ++i;
                    goto LEX_START2;
                }
            }
            break;
        }
        LEX_START2:
            l->state = LEX_START2;
        case LEX_START2:
            for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
            {
                const char ch = buf->buf[i];
                if (ch == ' ' || ch == '\t') continue;
                if (ch == '\n' || ch == '\r')
                {
                    goto LEX_START;
                }
                if (is_ascii_alphu(ch))
                {
                    l->tok_rc = l->rc;
                    goto LEX_IDENT;
                }
                else if (is_ascii_digit(ch))
                {
                    l->tok_rc = l->rc;
                    goto LEX_NUMBER;
                }
                else if (is_ascii_symbol(ch))
                {
                    l->tok_rc = l->rc;
                    l->tok[0] = ch;
                    l->sz = 1;
                    advance_rowcol(&l->rc, buf->buf[i++]);
                    goto LEX_SYMBOL;
                }
                else if (ch == '"')
                {
                    l->tok_rc = l->rc;
                    advance_rowcol(&l->rc, buf->buf[i++]);
                    goto LEX_STRING;
                }
                else
                {
                    return parser_ferror(&l->rc, "error: unexpected character: '%c'\n", ch), 1;
                }
            }
            break;
        LEX_SYMBOL:
            l->state = LEX_SYMBOL;
        case LEX_SYMBOL:
            for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
            {
                const char ch = buf->buf[i];
                if (l->sz == 1 && l->tok[0] == '/' && ch == '*')
                {
                    // multi-line comment
                    advance_rowcol(&l->rc, buf->buf[i++]);
                    l->sz = 0;
                    goto LEX_MULTILINE_COMMENT;
                }
                if (l->sz == 1 && l->tok[0] == '/' && ch == '/')
                {
                    // single-line comment
                    advance_rowcol(&l->rc, buf->buf[i++]);
                    l->sz = 0;
                    goto LEX_COMMENT;
                }
                if (l->sz == 1 && symbol_is_compound(l->tok[0], ch))
                {
                    l->tok[1] = ch;
                    l->sz = 2;
                    continue;
                }
                if (rc = emit_token(l)) return rc;
                goto LEX_START2;
            }
            break;
        LEX_STRING:
            l->state = LEX_STRING;
        case LEX_STRING:
            for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
            {
                const char ch = buf->buf[i];
                if (ch == '"')
                {
                    // finish string
                    if (rc = emit_token(l)) return rc;
                    advance_rowcol(&l->rc, buf->buf[i++]);
                    goto LEX_START2;
                }
                else if (ch == '\\')
                {
                    // escape
                    advance_rowcol(&l->rc, buf->buf[i++]);
                    goto LEX_STRING1;
                }
                else if (ch == '\n')
                {
                    return parser_ferror(&l->rc, "error: unexpected end of line in string literal\n"), 1;
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
            if (i < buf->sz)
            {
                const char ch = buf->buf[i];
                if (ch == '\n')
                {
                    advance_rowcol(&l->rc, buf->buf[i++]);
                }
                else
                {
                    // TODO: handle escape characters
                    if (rc = push_tok_char(l, ch)) return rc;
                    advance_rowcol(&l->rc, buf->buf[i++]);
                }
                goto LEX_STRING;
            }
            break;
        LEX_MULTILINE_COMMENT:
            l->state = LEX_MULTILINE_COMMENT;
        case LEX_MULTILINE_COMMENT:
            for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
            {
                const char ch = buf->buf[i];
                if (ch == '*')
                {
                    l->sz = 1;
                }
                else if (ch == '/' && l->sz == 1)
                {
                    l->sz = 0;
                    if (rc = emit_token(l)) return rc;
                    advance_rowcol(&l->rc, buf->buf[i++]);
                    goto LEX_START2;
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
            for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
            {
                const char ch = buf->buf[i];
                if (ch == '\n')
                {
                    l->sz = 0;
                    if (rc = emit_token(l)) return rc;
                    advance_rowcol(&l->rc, buf->buf[i++]);
                    goto LEX_START;
                }
            }
            break;
        LEX_IDENT:
            l->state = LEX_IDENT;
        case LEX_IDENT:
            for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
            {
                const char ch = buf->buf[i];
                if (is_ascii_alnumu(ch))
                {
                    if (rc = push_tok_char(l, ch)) return rc;
                }
                else
                {
                    for (size_t i = 0; i < sizeof(s_keywords_table) / sizeof(s_keywords_table[0]); ++i)
                    {
                        if (l->sz == s_keywords_table[i].len && memcmp(l->tok, s_keywords_table[i].txt, l->sz) == 0)
                        {
                            l->state = s_keywords_table[i].state;
                            break;
                        }
                    }
                    if (rc = emit_token(l)) return rc;
                    goto LEX_START2;
                }
            }
            break;
        LEX_NUMBER:
            l->state = LEX_NUMBER;
        case LEX_NUMBER:
            for (; i < buf->sz; advance_rowcol(&l->rc, buf->buf[i++]))
            {
                const char ch = buf->buf[i];
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
                    goto LEX_START2;
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
        return parser_ferror(&l->rc, "error: unexpected end of file in string literal\n"), 1;
    }
    if (l->state == LEX_MULTILINE_COMMENT)
    {
        return parser_ferror(&l->rc, "error: unexpected end of file inside comment\n"), 1;
    }
    if (l->sz > 0)
    {
        int rc;
        if (rc = emit_token(l)) return rc;
    }
    l->tok_rc = l->rc;
    l->state = LEX_EOF;
    return emit_token(l);
}

const char* lexstate_to_string(enum LexerState s)
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
