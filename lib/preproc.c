#include "preproc.h"

#include <string.h>

#include "errors.h"
#include "lexstate.h"
#include "stdlibe.h"
#include "tok.h"
#include "token.h"
#include "unwrap.h"

struct ParsedFile
{
    int pragma_once;
    char* filename;
};
static int pp_sublex_on_token(Lexer* l);

static size_t pp_sp_alloc(struct Preprocessor* p, const char* s, size_t n)
{
    if (n == 0) return 0;
    char* ch = (char*)array_alloc(&p->stringpool, n + 1);
    memcpy(ch, s, n);
    ch[n] = 0;
    return ch - (char*)p->stringpool.data;
}

struct KeywordEntry
{
    const char* const txt;
    const size_t len;
    const enum LexerState state;
};

#define KEYWORD(STATE, TXT) {TXT, sizeof(TXT) - 1, STATE},

static const struct KeywordEntry s_keywords_table[] = {X_LEX_KEYWORDS(KEYWORD)};

#define STREQ_LIT(STR, LEN, LIT) ((LEN) == sizeof(LIT) - 1 && memcmp((STR), LIT, (LEN)) == 0)

static void pp_form_token(struct Preprocessor* p, struct Lexer* l, struct Token* tk)
{
    tk->type = l->state;
    tk->rc = l->tok_rc;
    tk->sp_offset = pp_sp_alloc(p, l->tok, l->sz);

    if (l->state == LEX_IDENT)
    {
        for (size_t i = 0; i < sizeof(s_keywords_table) / sizeof(s_keywords_table[0]); ++i)
        {
            if (s_keywords_table[i].len == l->sz && memcmp(l->tok, s_keywords_table[i].txt, l->sz) == 0)
            {
                tk->type = l->state;
            }
        }
    }
}

struct SubLexer
{
    struct Lexer lexer;
    struct Preprocessor* self;
};

// includes trailing slash, if any
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

static size_t pp_find_insert_file(struct Array* parsedfiles, const char* filename)
{
    const size_t n = strlen(filename);
    struct ParsedFile* const data = (struct ParsedFile*)parsedfiles->data;
    const size_t size = array_size(parsedfiles, sizeof(struct ParsedFile));
    for (size_t i = 0; i < size; ++i)
    {
        if (strcmp(data[i].filename, filename) == 0)
        {
            return i;
        }
    }
    char* b = (char*)my_malloc(n + 1);
    memcpy(b, filename, n + 1);
    struct ParsedFile x = {
        .pragma_once = 0,
        .filename = b,
    };
    array_push(parsedfiles, &x, sizeof(struct ParsedFile));
    return size;
}

#define HANDLE_DIRECTIVE(STR, STATE)                                                                                   \
    if (STREQ_LIT(l->tok, l->sz, STR))                                                                                 \
    {                                                                                                                  \
        pp->preproc = STATE;                                                                                           \
        return 0;                                                                                                      \
    }

static int pp_handle_directive(struct Preprocessor* pp, Lexer* l)
{
    switch (pp->preproc)
    {
        case PP_INIT:
            if (l->state == LEX_IDENT)
            {
                pp->dir_rc = l->tok_rc;
                HANDLE_DIRECTIVE("ifdef", PP_IF);
                HANDLE_DIRECTIVE("if", PP_IF);
                HANDLE_DIRECTIVE("else", PP_ELSE);
                HANDLE_DIRECTIVE("elif", PP_ELIF);
                HANDLE_DIRECTIVE("ifndef", PP_IF);
                HANDLE_DIRECTIVE("endif", PP_ENDIF);
                if (pp->cur_if_false)
                {
                    pp->preproc = PP_IGNORE;
                    return 0;
                }
                if (STREQ_LIT(l->tok, l->sz, "include"))
                {
                    l->expect_header = 1;
                    pp->preproc = PP_INCLUDE;
                    return 0;
                }
                HANDLE_DIRECTIVE("pragma", PP_PRAGMA);
                HANDLE_DIRECTIVE("define", PP_DEFINE);
                HANDLE_DIRECTIVE("undef", PP_UNDEF);
            }
            return parser_ferror(&l->tok_rc, "error: unknown preprocessor directive: '#%s'\n", l->tok);
        case PP_DEFINE:
            if (l->state != LEX_IDENT)
            {
                return parser_ferror(&l->tok_rc, "error: expected macro name after #define\n");
            }
            if (l->sz + 1 > sizeof(pp->to_include)) abort();
            memcpy(pp->to_include, l->tok, l->sz + 1);
            pp->def_start = array_size(&pp->defs_tokens, sizeof(struct Token));
            pp->preproc = PP_DEFINE_CONT;
            return 0;
        case PP_DEFINE_CONT:
        {
            struct Token* tk = (struct Token*)array_alloc(&pp->defs_tokens, sizeof(struct Token));
            pp_form_token(pp, l, tk);
            return 0;
        }
        case PP_UNDEF:
            if (l->state != LEX_IDENT)
            {
                return parser_ferror(&l->tok_rc, "error: expected macro name in #undef\n");
            }
            sm_remove(&pp->defines_map, l->tok);
            pp->preproc = PP_EXPECT_END;
            return 0;
        case PP_IF:
        case PP_ELIF:
        case PP_ELSE:
        case PP_ENDIF:
        case PP_IGNORE: return 0;
        case PP_INCLUDE:
        {
            if (pp->files_open.sz > 50 * sizeof(void*))
            {
                return parser_ferror(&l->tok_rc, "error: maximum include depth exceeded\n");
            }

            if (l->state != LEX_HEADER || l->sz < 2)
            {
                return parser_ice(&l->tok_rc);
            }
            pp->preproc = PP_INCLUDE_EXPECT_END;

            l->tok[l->sz - 1] = '\0';
            if (path_is_absolute(l->tok + 1))
            {
                return parser_ferror(&l->tok_rc, "error: absolute include paths are not allowed\n");
            }

            memcpy(pp->to_include, l->tok + 1, l->sz - 1);
            pp->to_include_sz = l->sz - 2;
            return 0;
        }
        case PP_INCLUDE_EXPECT_END:
            return parser_ferror(&l->tok_rc, "error: expected end of include directive: %s\n", l->tok);
        case PP_EXPECT_END: return parser_ferror(&l->tok_rc, "error: expected end of directive: %s\n", l->tok);
        case PP_PRAGMA:
            if (l->state == LEX_IDENT)
            {
                HANDLE_DIRECTIVE("once", PP_PRAGMA_ONCE);
            }
            return parser_ferror(&l->tok_rc, "error: unknown pragma directive: %s\n", l->tok);
        case PP_PRAGMA_ONCE: return parser_ferror(&l->tok_rc, "error: expected end of directive: %s\n", l->tok);
    }
    abort();
}

static int pp_flush_directive(struct Preprocessor* pp, Lexer* l)
{
    pp->preproc = PP_INIT;
    switch (pp->preproc)
    {
        case PP_EXPECT_END:
        case PP_IGNORE:
        case PP_INIT: break;
        case PP_DEFINE: return parser_ferror(&pp->dir_rc, "error: expected macro name after #define\n");
        case PP_UNDEF: return parser_ferror(&pp->dir_rc, "error: expected macro name after #undef\n");
        case PP_IF:
            array_push(&pp->if_stack, &pp->cur_if_false, 1);
            pp->cur_if_false = 1;
            break;
        case PP_ELIF:
            if (pp->if_stack.sz == 0)
            {
                return parser_ferror(&pp->dir_rc, "error: unexpected #elif outside #if\n");
            }
            break;
        case PP_ELSE:
            if (pp->if_stack.sz == 0)
            {
                return parser_ferror(&pp->dir_rc, "error: unexpected #else outside #if\n");
            }
            pp->cur_if_false = ((char*)pp->if_stack.data)[pp->if_stack.sz - 1];
            break;
        case PP_ENDIF:
            if (pp->if_stack.sz == 0)
            {
                return parser_ferror(&pp->dir_rc, "error: unexpected #endif outside #if\n");
            }
            pp->cur_if_false = ((char*)pp->if_stack.data)[pp->if_stack.sz - 1];
            array_pop(&pp->if_stack, 1);
            break;
        case PP_DEFINE_CONT:
        {
            sm_insert(&pp->defines_map, pp->to_include, pp->def_start);
            break;
        }
        case PP_INCLUDE: return parser_ferror(&pp->dir_rc, "error: expected header to include\n");
        case PP_INCLUDE_EXPECT_END:
        {
            const size_t parent_sz = path_parent_span(pp->dir_rc.file);
            if (parent_sz + pp->to_include_sz >= 256) abort();
            char filename[256];
            memcpy(filename, pp->dir_rc.file, parent_sz);
            memcpy(filename + parent_sz, pp->to_include, pp->to_include_sz + 1);

            FILE* f = fopen(filename, "r");
            if (!f)
            {
                const char* inc = pp->include_paths;
                const size_t inc_size = strlen(inc);

                for (size_t i_start = 0; i_start < inc_size;)
                {
                    const char* i_end = (const char*)memchr(inc + i_start, ';', inc_size - i_start);
                    const size_t n = (i_end ? i_end - inc : inc_size) - i_start;
                    if (n >= sizeof(filename)) abort();
                    memcpy(filename, inc + i_start, n);
                    snprintf(filename + n, sizeof(filename) - n, "/%s", pp->to_include);
                    f = fopen(filename, "r");
                    if (f) break;
                    i_start += n + 1;
                }
            }
            if (!f)
            {
                char buf[128];
                snprintf(buf, 128, "%s: failed to open", pp->to_include);
                perror(buf);
                return 1;
            }

            int rc = preproc_file(pp, f, filename);
            if (!rc)
            {
                // pop EOF from included file
                if (pp->toks.sz > 0 &&
                    ((struct Token*)pp->toks.data)[array_size(&pp->toks, sizeof(struct Token)) - 1].type == LEX_EOF)
                {
                    array_pop(&pp->toks, sizeof(struct Token));
                }
            }
            fclose(f);
            return rc;
        }
        case PP_PRAGMA: return parser_ferror(&pp->dir_rc, "error: expected pragma directive: %s\n", l->tok);
        case PP_PRAGMA_ONCE:
            ((struct ParsedFile*)pp->filenames.data)[pp->cur_file].pragma_once = 1;
            pp->preproc = PP_INIT;
            return 0;
    }
    return 0;
}

// returns SIZE_MAX on not found
static size_t sstk_find(struct StringStk* ss, const char* str, size_t len)
{
    size_t const l_len = array_size(&ss->lengths, sizeof(size_t));
    const size_t* const l_data = (const size_t*)ss->lengths.data;

    size_t prev_len = 0;

    for (size_t i = 0; i < l_len; ++i)
    {
        if (l_data[i] - prev_len == len)
        {
            if (memcmp(l_data[i] + (char*)ss->data.data, str, len) == 0)
            {
                return i;
            }
        }
        prev_len = l_data[i];
    }
    return SIZE_MAX;
}
static size_t sstk_push(struct StringStk* ss, const char* str, size_t len)
{
    size_t const ret = array_size(&ss->lengths, sizeof(size_t));
    array_push(&ss->lengths, &ss->data.sz, sizeof(size_t));
    array_push(&ss->data, str, len);
    return ret;
}
__forceinline static size_t sstk_size(struct StringStk* ss) { return array_size(&ss->lengths, sizeof(size_t)); }

// UB if count > current
static void sstk_shrink(struct StringStk* ss, size_t count)
{
    ss->data.sz = ((size_t*)ss->lengths.data)[count];
    ss->lengths.sz = count * sizeof(size_t);
}
static void sstk_destroy(struct StringStk* ss)
{
    array_destroy(&ss->data);
    array_destroy(&ss->lengths);
}

static __forceinline const char* pp_token_str(const struct Preprocessor* pp, const struct Token* tk)
{
    return (const char*)pp->stringpool.data + tk->sp_offset;
}

static int pp_expand_macro(struct Preprocessor* pp, const char* id, size_t id_sz, size_t tok_seq)
{
    int rc = 0;
    const size_t sstk_orig_size = sstk_size(&pp->macro_stack);
    sstk_push(&pp->macro_stack, id, id_sz);

    struct Token* def_tokens = (struct Token*)pp->defs_tokens.data + tok_seq;
    if (def_tokens[0].type == LEX_DIRECTIVE_DEFINE_FN)
    {
        // function-like macro
    }
    else
    {
        size_t prev = 0;
        size_t extent = 0;
        while (def_tokens[extent].type != LEX_END_DIRECTIVE)
        {
            if (def_tokens[extent].type == LEX_IDENT)
            {
                const char* s = pp_token_str(pp, def_tokens + extent);
                size_t ln = strlen(s);
                size_t* w = sm_get(&pp->defines_map, s);
                if (w)
                {
                    if (extent - prev > 0)
                        array_push(&pp->toks, def_tokens + prev, sizeof(struct Token) * (extent - prev));
                    UNWRAP(pp_expand_macro(pp, s, ln, *w));
                    prev = extent + 1;
                }
            }
            ++extent;
        }
        if (extent - prev > 0) array_push(&pp->toks, def_tokens + prev, sizeof(struct Token) * (extent - prev));
    }
fail:
    sstk_shrink(&pp->macro_stack, sstk_orig_size);
    return rc;
}

static int pp_sublex_on_token(Lexer* l)
{
    int rc = 0;
    struct SubLexer* sublex = (struct SubLexer*)l;
    struct Preprocessor* pp = sublex->self;
    if (pp->in_directive && !l->not_first)
    {
        pp->in_directive = 0;
        UNWRAP(pp_flush_directive(pp, l));
    }
    if (l->state == LEX_COMMENT)
    {
        // drop comments
        return 0;
    }
    if (l->state == LEX_MULTILINE_COMMENT)
    {
        // drop comments
        return 0;
    }
    if (pp->in_directive)
    {
        return pp_handle_directive(pp, l);
    }
    if (!l->not_first && l->state == LEX_SYMBOL && l->tok[0] == '#' && l->tok[1] == '\0')
    {
        // beginning of directive
        pp->in_directive = 1;
        return 0;
    }
    if (pp->cur_if_false)
    {
        // drop tokens inside #if false
        return 0;
    }
    if (pp->paren_count)
    {
        // inside matching macro function call
        if (l->state == LEX_SYMBOL)
        {
            const char ch = l->tok[0];
            if (ch == '(')
            {
                ++pp->paren_count;
            }
            else if (ch == ')')
            {
                --pp->paren_count;
                if (pp->paren_count == 0)
                {
                    // completed parsing macro fn
                }
            }
        }
    }
    if (pp->prev_token_was_macrofn && l->state == LEX_SYMBOL && l->tok[0] == '(')
    {
        // begin matching macro function call
        ++pp->paren_count;
        return 0;
    }
    pp->prev_token_was_macrofn = 0;
    if (l->state == LEX_IDENT)
    {
        // look up macro
        size_t* v = sm_get(&pp->defines_map, l->tok);
        if (v)
        {
            struct Token* def_tokens = (struct Token*)pp->defs_tokens.data + *v;
            if (def_tokens[0].type == LEX_DIRECTIVE_DEFINE_FN)
            {
                // function-like macro
            }
            else
            {
                size_t extent = 0;
                while (def_tokens[extent].type != LEX_END_DIRECTIVE)
                {
                    ++extent;
                }
                array_push(&pp->toks, def_tokens, sizeof(struct Token) * extent);
                return 0;
            }
        }
    }
    struct Token* const tk = (struct Token*)array_alloc(&pp->toks, sizeof(struct Token));
    pp_form_token(pp, l, tk);
fail:
    return rc;
}

void preproc_init(struct Preprocessor* pp, const char* include_paths)
{
    memset(pp, 0, sizeof(struct Preprocessor));
    pp->include_paths = include_paths;
    array_push_byte(&pp->stringpool, '\0');
}

static int lex_file(FILE* f, Lexer* l)
{
    int rc;
    char buf[1024];
    size_t sz;
    while (1)
    {
        sz = fread(buf, 1, sizeof(buf), f);
        if (!sz) break;
        if ((rc = lex(l, buf, sz))) return rc;
    }
    return end_lex(l);
}

int preproc_file(struct Preprocessor* pp, FILE* f, const char* filename)
{
    const size_t prev_cur_file = pp->cur_file;
    pp->cur_file = pp_find_insert_file(&pp->filenames, filename);
    struct ParsedFile* file = (struct ParsedFile*)pp->filenames.data + pp->cur_file;
    int rc = 0;
    if (!file->pragma_once)
    {
        struct SubLexer sublex;
        sublex.self = pp;
        init_lexer(&sublex.lexer, file->filename, &pp_sublex_on_token);
        array_push_ptr(&pp->files_open, file->filename);
        rc = lex_file(f, &sublex.lexer);
        array_pop_ptr(&pp->files_open);
    }
    pp->cur_file = prev_cur_file;
    return rc;
}

void preproc_destroy(struct Preprocessor* pp)
{
    struct ParsedFile* b = pp->filenames.data;
    for (size_t i = 0; i < array_size(&pp->filenames, sizeof(struct ParsedFile)); ++i)
    {
        my_free(b[i].filename);
    }
    array_destroy(&pp->filenames);
    array_destroy(&pp->files_open);
    sm_destroy(&pp->defines_map);
    array_destroy(&pp->macro_arg_idxs);
    array_destroy(&pp->macro_arg_seqs);
    array_destroy(&pp->toks);
    array_destroy(&pp->stringpool);
}
