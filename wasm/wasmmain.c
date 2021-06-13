#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cg.h"
#include "lexstate.h"
#include "tok.h"

const char retbuf[128] = "Success!";

typedef struct
{
    Lexer lexer;
    Parser parser;

    struct Array files_open;
    struct Array filenames;
} FrontEnd;

struct SubLexer
{
    Lexer lexer;
    FrontEnd* fe;
};
static int fe_handle_directive(FrontEnd* fe, Lexer* l);
static int sublex_on_token(Lexer* l)
{
    struct SubLexer* self = (struct SubLexer*)((char*)l - offsetof(struct SubLexer, lexer));
    if (l->state == LEX_DIRECTIVE)
    {
        return fe_handle_directive(self->fe, l);
    }
    else if (l->state == LEX_COMMENT)
    {
        // drop comments
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

static size_t path_parent_span(const char* path)
{
    size_t r = 0;
    size_t i = 0;
    char ch;
    while (ch = path[i++])
    {
#ifdef _WIN32_
#error "TODO: path manipulation"
#else
        if (ch == '/') r = i;
#endif
    }
    return r;
}

static int path_is_absolute(const char* path)
{
#ifdef _WIN32_
#error "TODO: path manipulation"
#else
    return *path == '/';
#endif
}

static int fe_handle_directive(FrontEnd* fe, Lexer* l)
{
    if (strncmp(l->tok, "include ", 8) == 0)
    {
        if (l->tok[8] != '"' || l->tok[l->sz - 1] != '"' || l->sz < 10)
        {
            return parser_ferror(&l->tok_rc, "error: #include only supports '\"'\n");
        }
        l->tok[l->sz - 1] = '\0';
        char* const* const names_begin = fe->filenames.data;
        const size_t* const begin = fe->files_open.data;
        const size_t n = fe->files_open.sz / sizeof(size_t);
        for (size_t i = 0; i < n; ++i)
        {
            if (strcmp(names_begin[begin[i]], l->tok + 9) == 0)
            {
                return parser_ferror(&l->tok_rc, "error: attempted to recursively include '%s'\n", l->tok + 9);
            }
        }
        if (path_is_absolute(l->tok + 9))
        {
            return parser_ferror(&l->tok_rc, "error: absolute include paths are not allowed\n");
        }
        array_push(&fe->files_open, &fe->filenames.sz, sizeof(size_t));

        size_t parent_sz = path_parent_span(l->tok_rc.file);
        char* filename = (char*)malloc(l->sz - 9 + parent_sz);
        memcpy(filename, l->tok_rc.file, parent_sz);
        memcpy(filename + parent_sz, l->tok + 9, l->sz - 9);
        array_push(&fe->filenames, &filename, sizeof(filename));
        struct SubLexer sublex;
        sublex.fe = fe;
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
        return 0;
    }
    else if (strncmp(l->tok, "define ", 7) == 0)
    {
        // ignore #defines
        return 0;
    }
    else if (strncmp(l->tok, "ifdef ", 6) == 0)
    {
        // ignore #ifdef
        return 0;
    }
    else if (strcmp(l->tok, "endif") == 0)
    {
        // ignore #endif
        return 0;
    }
    else if (strncmp(l->tok, "pragma ", 7) == 0)
    {
        if (strcmp(l->tok + 7, "once") == 0)
        {
            // ignore #pragma once
            return 0;
        }
        else if (strncmp(l->tok + 7, "memory ", 7) == 0)
        {
            return cg_set_memory_bank(&fe->parser.cg, &l->tok_rc, l->tok + 14);
        }
        return parser_ferror(&l->tok_rc, "error: unknown pragma directive: '#%s'\n", l->tok), 1;
    }
    else
    {
        return parser_ferror(&l->tok_rc, "error: unknown preprocessor directive: '#%s'\n", l->tok), 1;
    }
}

int fe_on_token(Lexer* l)
{
    FrontEnd* self = (FrontEnd*)((char*)l - offsetof(FrontEnd, lexer));
    if (l->state == LEX_DIRECTIVE)
    {
        return fe_handle_directive(self, l);
    }
    else if (l->state == LEX_COMMENT)
    {
        // drop comments
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

void fe_init(FrontEnd* fe)
{
    parser_init(&fe->parser);
    array_init(&fe->filenames);
    array_init(&fe->files_open);
}
void fe_destroy(FrontEnd* fe)
{
    parser_destroy(&fe->parser);
    char** b = fe->filenames.data;
    for (size_t i = 0; i < fe->filenames.sz / sizeof(char*); ++i)
    {
        free(b[i]);
    }
    array_destroy(&fe->filenames);
    array_destroy(&fe->files_open);
}

int wasmmain(_Bool debug)
{
    parser_clear_errors();
    FrontEnd fe;
    fe_init(&fe);
    fe.parser.cg.fdebug = debug ? stderr : NULL;

    FILE* f = fopen("main.c", "r");
    if (!f)
    {
        perror("failed to open main.c: ");
        abort();
    }
    init_lexer(&fe.lexer, "main.c", &fe_on_token);
    int rc = lex_file(f, &fe.lexer);

    fe_destroy(&fe);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    return 0;
}
