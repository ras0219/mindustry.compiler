#include "fe.h"

#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include "be.h"
#include "cg.h"
#include "elaborator.h"
#include "errors.h"
#include "lexstate.h"
#include "parse.h"
#include "stdlibe.h"
#include "tok.h"

struct SubLexer
{
    struct Lexer lexer;
    struct FrontEnd* fe;
};

static int fe_handle_directive(struct FrontEnd* fe, Lexer* l);
static int sublex_on_token(Lexer* l)
{
    struct SubLexer* self = (struct SubLexer*)((char*)l - offsetof(struct SubLexer, lexer));
    if (l->in_directive)
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
        return parser_push(self->fe->parser, &self->lexer);
    }
}

static int lex_file(FILE* f, Lexer* l)
{
    int rc;
    Buffer buf;
    while (1)
    {
        buf.sz = fread(buf.buf, 1, sizeof(buf.buf), f);
        if (!buf.sz) break;
        if ((rc = lex(l, &buf))) return rc;
    }
    return end_lex(l);
}

static size_t path_parent_span(const char* path)
{
    size_t r = 0;
    size_t i = 0;
    char ch;
    while ((ch = path[i++]))
    {
#ifdef _WIN32
        if (ch == '\\' || ch == '/') r = i;
#else
        if (ch == '/') r = i;
#endif
    }
    return r;
}

static int is_upper_lower_alpha(char ch) { return (ch >= 'a' & ch <= 'z') | (ch >= 'A' & ch <= 'Z'); }

static int path_is_absolute(const char* path)
{
#ifdef _WIN32
    return is_upper_lower_alpha(path[0]) && path[1] == ':' && (path[2] == '/' | path[2] == '\\');
#else
    return *path == '/';
#endif
}

#define HANDLE_DIRECTIVE(STR, STATE)                                                                                   \
    if (l->sz == sizeof(STR) - 1 && memcmp(l->tok, STR, sizeof(STR) - 1) == 0)                                         \
    {                                                                                                                  \
        fe->preproc = STATE;                                                                                           \
        return 0;                                                                                                      \
    }

static int fe_handle_directive(struct FrontEnd* fe, Lexer* l)
{
    switch (fe->preproc)
    {
        case PP_INIT:
            if (l->state != LEX_START_DIRECTIVE)
            {
                return parser_ferror(&l->tok_rc, "error: expected end of line\n");
            }
            fe->preproc = PP_HASH;
            return 0;
        case PP_HASH:
            if (l->state != LEX_DIRECTIVE)
            {
                return parser_ice(&l->tok_rc);
            }
            HANDLE_DIRECTIVE("include", PP_INCLUDE);
            HANDLE_DIRECTIVE("pragma", PP_PRAGMA);
            HANDLE_DIRECTIVE("define", PP_IGNORE);
            HANDLE_DIRECTIVE("ifdef", PP_IGNORE);
            HANDLE_DIRECTIVE("endif", PP_IGNORE);
            return parser_ferror(&l->tok_rc, "error: unknown preprocessor directive: '#%s'\n", l->tok);
        case PP_INCLUDE:
        {
            if (l->state != LEX_DIRECTIVE_INCLUDE)
            {
                return parser_ice(&l->tok_rc);
            }
            fe->preproc = PP_INIT;

            if (l->tok[0] != '"' || l->tok[l->sz - 1] != '"')
            {
                return parser_ferror(&l->tok_rc, "error: #include only supports '\"'\n");
            }
            l->tok[l->sz - 1] = '\0';
            const char* const tok_filename = l->tok + 1;
            if (path_is_absolute(tok_filename))
            {
                return parser_ferror(&l->tok_rc, "error: absolute include paths are not allowed\n");
            }

            const size_t parent_sz = path_parent_span(l->tok_rc.file);
            char* filename = (char*)my_malloc(l->sz - 1 + parent_sz);
            memcpy(filename, l->tok_rc.file, parent_sz);
            memcpy(filename + parent_sz, l->tok + 1, l->sz - 1);

            char* const* const names_begin = (char* const*)fe->filenames.data;
            for (size_t i = 0; i < fe->filenames.sz / sizeof(char*); ++i)
            {
                if (strcmp(names_begin[i], filename) == 0)
                {
                    my_free(filename);
                    filename = names_begin[i];
                    goto found;
                }
            }
            array_push_ptr(&fe->filenames, filename);
        found:;
            if (fe->files_open.sz > 50 * sizeof(void*))
            {
                return parser_ferror(&l->tok_rc, "error: maximum include depth exceeded\n");
            }

            FILE* f = fopen(filename, "r");
            if (!f)
            {
                char buf[128];
                snprintf(buf, 128, "%s: failed to open", filename);
                perror(buf);
                return 1;
            }

            struct SubLexer sublex;
            sublex.fe = fe;
            init_lexer(&sublex.lexer, filename, &sublex_on_token);
            array_push_ptr(&fe->files_open, filename);
            int rc = lex_file(f, &sublex.lexer);
            array_pop_ptr(&fe->files_open);
            fclose(f);
            return rc;
        }
        case PP_IGNORE:
            if (l->state == LEX_START_DIRECTIVE)
            {
                fe->preproc = PP_HASH;
            }
            return 0;
        case PP_PRAGMA:
            HANDLE_DIRECTIVE("once", PP_INIT);
            return parser_ferror(&l->tok_rc, "error: unknown pragma directive: %s\n", l->tok), 1;
    }
    abort();
}

static int fe_on_token(Lexer* l)
{
    struct FrontEnd* self = (struct FrontEnd*)((char*)l - offsetof(struct FrontEnd, lexer));
    if (l->in_directive)
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
        return parser_push(self->parser, &self->lexer);
    }
}

void fe_init(struct FrontEnd* fe)
{
    fe->preproc = PP_INIT;
    fe->parser = (struct Parser*)my_malloc(sizeof(struct Parser));
    parser_init(fe->parser);
    fe->elab = (struct Elaborator*)my_malloc(sizeof(struct Elaborator));
    elaborator_init(fe->elab, fe->parser);

    fe->cg = (struct CodeGen*)my_malloc(sizeof(struct CodeGen));
    cg_init(fe->cg);

    fe->be = (struct BackEnd*)my_malloc(sizeof(struct BackEnd));
    be_init(fe->be, fe->parser, fe->elab, fe->cg);

    array_init(&fe->filenames);
    array_init(&fe->files_open);

    fe->fout = NULL;
}
void fe_destroy(struct FrontEnd* fe)
{
    cg_destroy(fe->cg);
    free(fe->cg);
    be_destroy(fe->be);
    free(fe->be);
    elaborator_destroy(fe->elab);
    free(fe->elab);
    parser_destroy(fe->parser);
    free(fe->parser);
    char** b = fe->filenames.data;
    for (size_t i = 0; i < fe->filenames.sz / sizeof(char*); ++i)
    {
        free(b[i]);
    }
    array_destroy(&fe->filenames);
    array_destroy(&fe->files_open);
}

int fe_lex_file(struct FrontEnd* fe, const char* filename)
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
    if (rc) return rc;
    if ((rc = parser_parse(fe->parser))) return rc;
    if ((rc = elaborate(fe->elab))) return rc;
    if ((rc = be_compile(fe->be))) return rc;
    return cg_emit(fe->cg, fe->fout);
}

int fe_lex_file_opened(struct FrontEnd* fe, const char* filename, FILE* f)
{
    init_lexer(&fe->lexer, filename, &fe_on_token);
    return lex_file(f, &fe->lexer);
}
