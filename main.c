#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include "lexstate.h"
#include "malloc.h"
#include "tok.h"

int usage() { printf("Usage: mindustry.compiler <file.mlogp>\n"); }

char s_error_buffer[1024];

typedef struct
{
    Lexer lexer;
    Parser parser;

    struct Array files_opened;
} FrontEnd;

struct SubLexer
{
    Lexer lexer;
    FrontEnd* fe;
};

void parser_ferror(const struct RowCol* rc, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);
    size_t n = snprintf(s_error_buffer, sizeof(s_error_buffer), "%s:%d:%d: ", rc->file, rc->row, rc->col);
    if (n < sizeof(s_error_buffer))
    {
        vsnprintf(s_error_buffer + n, sizeof(s_error_buffer) - n, fmt, argp);
    }
}
static int sublex_on_token(Lexer* l)
{
    struct SubLexer* self = (struct SubLexer*)((char*)l - offsetof(struct SubLexer, lexer));
    if (l->state == LEX_DIRECTIVE)
    {
        // drop directives
        return 0;
    }
    else if (l->state == LEX_MULTILINE_COMMENT)
    {
        // drop comments
        return 0;
    }
    else if (l->state == LEX_EOF)
    {
        // skip EOF -- resume parent file
        return 0;
    }
    else
    {
        return parse(&self->fe->parser, &self->lexer);
    }
}

static int lex_file(FILE* f, Lexer* l)
{
    s_error_buffer[0] = 0;
    int rc = 0;
    Buffer buf;
    while (1)
    {
        buf.sz = fread(buf.buf, 1, sizeof(buf.buf), f);
        if (!buf.sz) break;
        if (rc = lex(l, &buf)) return rc;
    }
    return end_lex(l);
}

int fe_on_token(Lexer* l)
{
    FrontEnd* self = (FrontEnd*)((char*)l - offsetof(FrontEnd, lexer));
    // printf("TOK: '%s' %d\n", self->lexer.tok, self->lexer.state);
    if (l->state == LEX_DIRECTIVE)
    {
        if (strncmp(l->tok, "include ", 8) == 0)
        {
            if (l->tok[8] != '"' || l->tok[l->sz - 1] != '"' || l->sz < 10)
            {
                return parser_ferror(&l->rc, "error: #include only supports '\"'\n"), 1;
            }
            l->tok[l->sz - 1] = '\0';
            char* filename = (char*)malloc(l->sz - 9);
            strcpy(filename, l->tok + 9);
            array_push(&self->files_opened, &filename, sizeof(filename));
            struct SubLexer sublex;
            sublex.fe = self;
            init_lexer(&sublex.lexer, filename, &sublex_on_token);
            FILE* f = fopen(filename, "r");
            if (!f)
            {
                char buf[128];
                snprintf(buf, 128, "%s: failed to open", filename);
                perror(buf);
                return 1;
            }
            int rc = lex_file(f, &sublex.lexer);
            fclose(f);
            return rc;
        }
        return 0;
    }
    else if (l->state == LEX_MULTILINE_COMMENT)
    {
        // drop comments
        return 0;
    }
    else
    {
        return parse(&self->parser, &self->lexer);
    }
}

void init_front_end(FrontEnd* fe)
{
    parser_init(&fe->parser);
    array_init(&fe->files_opened);
}

static int fe_lex_file(FrontEnd* fe, const char* filename)
{
    FILE* f = fopen(filename, "r");
    if (!f)
    {
        char buf[128];
        snprintf(buf, 128, "%s: failed to open", filename);
        perror(buf);
        return 1;
    }
    init_lexer(&fe->lexer, filename, &fe_on_token);
    int rc = lex_file(f, &fe->lexer);
    fclose(f);
    return rc;
}

int main(int argc, const char* const* argv)
{
    if (argc != 2)
    {
        return usage();
    }
    FrontEnd fe;
    init_front_end(&fe);
    int rc = fe_lex_file(&fe, argv[1]);
    if (rc)
    {
        fprintf(stderr, "%s", s_error_buffer);
    }
    return rc;
}