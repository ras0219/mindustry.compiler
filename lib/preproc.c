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

// returns SIZE_MAX on not found
static size_t sstk_find(struct StringStk* ss, const char* str, size_t len)
{
    if (ss->lengths.sz == 0) return SIZE_MAX;

    size_t const l_len = array_size(&ss->lengths, sizeof(size_t));
    const size_t* const l_data = (const size_t*)ss->lengths.data;

    size_t prev_len = 0;
    for (size_t i = 0; i < l_len; ++i)
    {
        if (l_data[i] - prev_len == len)
        {
            if (memcmp(prev_len + (char*)ss->data.data, str, len) == 0)
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
    array_push(&ss->data, str, len);
    array_push_size_t(&ss->lengths, ss->data.sz);
    return ret;
}
__forceinline static size_t sstk_size(struct StringStk* ss) { return array_size(&ss->lengths, sizeof(size_t)); }

// UB if count > current
static void sstk_shrink(struct StringStk* ss, size_t count)
{
    ss->data.sz = count ? ((size_t*)ss->lengths.data)[count - 1] : 0;
    ss->lengths.sz = count * sizeof(size_t);
}
static void sstk_destroy(struct StringStk* ss)
{
    array_destroy(&ss->data);
    array_destroy(&ss->lengths);
}

__forceinline static const char* pp_sp_str(const struct Preprocessor* pp, size_t offset)
{
    return (const char*)pp->stringpool.data + offset;
}

const char* pp_token_str(const struct Preprocessor* pp, const struct Token* tk) { return pp_sp_str(pp, tk->sp_offset); }

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
    memset(tk, 0, sizeof(struct Token));
    tk->basic_type = l->state;
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
    else if (l->state == LEX_SYMBOL)
    {
        if (l->sz == 1)
            tk->type = TOKEN_SYM1(l->tok[0]);
        else if (l->sz == 2)
            tk->type = TOKEN_SYM2(l->tok[0], l->tok[1]);
        else if (l->sz == 3)
            tk->type = TOKEN_SYM3(l->tok[0], l->tok[1], l->tok[2]);
        else
            abort();
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

struct MacroDef
{
    struct RowCol rc;
    unsigned int is_function : 1;
    unsigned int is_ellipsis : 1;
    unsigned int arity : 6;
    size_t tok_seq_offset;
    size_t tok_seq_extent;
    ptrdiff_t sp_offset;
    size_t sp_len;
};

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
            pp->preproc = PP_DEFINE_CONT_FIRST;
            struct MacroDef* def = array_push_zeroes(&pp->defs_info, sizeof(struct MacroDef));
            def->tok_seq_offset = array_size(&pp->defs_tokens, sizeof(struct Token));
            def->rc = pp->dir_rc;
            def->sp_offset = pp_sp_alloc(pp, l->tok, l->sz);
            def->sp_len = l->sz;
            return 0;
        case PP_DEFINE_CONT_FIRST:
            if (l->ws_before == 0 && l->state == LEX_SYMBOL && l->tok[0] == '(')
            {
                // defining function macro
                pp->preproc = PP_DEFINE_FN;
                struct MacroDef* def = array_back(&pp->defs_info, sizeof(struct MacroDef));
                def->is_function = 1;
                return 0;
            }
            pp->preproc = PP_DEFINE_CONT;
        case PP_DEFINE_CONT:
        {
            struct Token* tk = (struct Token*)array_alloc(&pp->defs_tokens, sizeof(struct Token));
            pp_form_token(pp, l, tk);
            size_t arg_idx = sstk_find(&pp->def_arg_names, l->tok, l->sz);
            if (arg_idx != SIZE_MAX)
            {
                tk->basic_type = LEX_MACRO_ARG_BEGIN;
                tk->type = LEX_MACRO_ARG_BEGIN + arg_idx;
            }
            else if (STREQ_LIT(l->tok, l->sz, "__VA_ARGS__"))
            {
                tk->basic_type = LEX_MACRO_VA_ARGS;
                tk->type = LEX_MACRO_VA_ARGS;
            }
            return 0;
        }
        case PP_DEFINE_FN:
        case PP_DEFINE_FN_COMMA:
            if (pp->preproc != PP_DEFINE_FN_COMMA && l->state == LEX_SYMBOL && l->tok[0] == ')')
            {
                pp->preproc = PP_DEFINE_CONT;
                return 0;
            }
            else if (l->state == LEX_IDENT)
            {
                pp->preproc = PP_DEFINE_FN_ARG;
                if (sstk_find(&pp->def_arg_names, l->tok, l->sz) != SIZE_MAX)
                {
                    return parser_ferror(&l->tok_rc,
                                         "error: duplicate parameter name in macro function parameter list\n");
                }
                sstk_push(&pp->def_arg_names, l->tok, l->sz);
                struct MacroDef* def = array_back(&pp->defs_info, sizeof(struct MacroDef));
                if (def->arity >= 0x3F /* 2^6 - 1 */) abort();
                ++def->arity;
                return 0;
            }
            else if (l->state == LEX_SYMBOL && l->tok[0] == '.' && l->tok[1] == '.' && l->tok[2] == '.')
            {
                pp->preproc = PP_DEFINE_FN_ELLIPSIS;
                struct MacroDef* def = array_back(&pp->defs_info, sizeof(struct MacroDef));
                def->is_ellipsis = 1;
                return 0;
            }
            else
            {
                return parser_ferror(&l->tok_rc, "error: expected parameter name in macro function parameter list\n");
            }
        case PP_DEFINE_FN_ARG:
            if (l->state == LEX_SYMBOL && l->tok[0] == ')')
            {
                pp->preproc = PP_DEFINE_CONT;
                return 0;
            }
            else if (l->state == LEX_SYMBOL && l->tok[0] == ',')
            {
                pp->preproc = PP_DEFINE_FN_COMMA;
                return 0;
            }
            else
            {
                return parser_ferror(&l->tok_rc, "error: expected ',' or ')' in macro function parameter list\n");
            }
        case PP_DEFINE_FN_ELLIPSIS:
            if (l->state == LEX_SYMBOL && l->tok[0] == ')')
            {
                pp->preproc = PP_DEFINE_CONT;
                return 0;
            }
            else
            {
                return parser_ferror(
                    &l->tok_rc,
                    "error: ellipsis must be last parameter; expected ')' in macro function parameter list\n");
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
    enum PreprocessorState const s = pp->preproc;
    pp->preproc = PP_INIT;
    switch (s)
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
        case PP_DEFINE_CONT_FIRST:
        case PP_DEFINE_CONT:
        {
            sstk_shrink(&pp->def_arg_names, 0);
            struct MacroDef* def = array_back(&pp->defs_info, sizeof(struct MacroDef));
            def->tok_seq_extent = array_size(&pp->defs_tokens, sizeof(struct Token)) - def->tok_seq_offset;
            sm_insert(&pp->defines_map,
                      pp_sp_str(pp, def->sp_offset),
                      array_size(&pp->defs_info, sizeof(struct MacroDef)) - 1);
            break;
        }
        case PP_DEFINE_FN:
        case PP_DEFINE_FN_ARG:
        case PP_DEFINE_FN_COMMA:
        case PP_DEFINE_FN_ELLIPSIS:
            return parser_ferror(&pp->dir_rc, "error: unterminated function-like macro definition\n");
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

struct MacroExpand
{
    size_t prev_paren_count;
    size_t macro_arg_offsets_offset;
    size_t macrodef_offset;
};

static int pp_expand_macro(
    struct Preprocessor* pp, const char* id, size_t id_sz, struct Token const* tks, size_t extent);

static int pp_subst_macro_args(struct Preprocessor* pp, struct Array* out, struct MacroExpand* exp)
{
    struct MacroDef* def = (struct MacroDef*)pp->defs_info.data + exp->macrodef_offset;
    size_t* const macro_arg_data = (size_t*)pp->macro_arg_offsets.data + exp->macro_arg_offsets_offset;
    struct Token* const toks = pp->toks.data;
    size_t num_args = array_size(&pp->macro_arg_offsets, sizeof(size_t)) - exp->macro_arg_offsets_offset - 1;
    size_t req_arity = def->arity ? def->arity : 1;
    if ((!def->is_ellipsis && req_arity < num_args) || req_arity > num_args)
    {
        return parser_ferror(&toks[array_size(&pp->toks, sizeof(struct Token)) - 1].rc,
                             "error: incorrect number of arguments in macro call to %s: expected %zu but got %zu\n",
                             pp_sp_str(pp, def->sp_offset),
                             def->arity,
                             num_args);
    }
    struct Token* const data = (struct Token*)pp->defs_tokens.data + def->tok_seq_offset;
    const size_t extent = def->tok_seq_extent;
    for (size_t i = 0; i < extent; ++i)
    {
        if (data[i].basic_type == LEX_MACRO_ARG_BEGIN)
        {
            if (i > 0 && data[i - 1].type == TOKEN_SYM('#'))
            {
                // stringify tokens
            }

            size_t const arg_idx = data[i].type - LEX_MACRO_ARG_BEGIN;
            struct Token* const argseq_start = toks + macro_arg_data[arg_idx] + 1;
            size_t argseq_len = macro_arg_data[arg_idx + 1] - macro_arg_data[arg_idx] - 1;
            array_push(out, argseq_start, argseq_len * sizeof(struct Token));
        }
        else if (data[i].basic_type == LEX_MACRO_VA_ARGS)
        {
            if (def->arity < num_args)
            {
                struct Token* const argseq_start = toks + macro_arg_data[def->arity] + 1;
                size_t argseq_len = macro_arg_data[num_args] - macro_arg_data[def->arity] - 1;
                array_push(out, argseq_start, argseq_len * sizeof(struct Token));
            }
        }
        else if (data[i].type = TOKEN_SYM1('#'))
        {
        }
        else
        {
            array_push(out, data + i, sizeof(struct Token));
        }
    }
    return 0;
}

static int pp_push_tok_seq(struct Preprocessor* pp, const struct Token* toks, size_t len)
{
    int rc = 0;
    for (size_t i = 0; i < len; ++i)
    {
        if (pp->prev_macrodef_idx_p1)
        {
            if (toks[i].type == TOKEN_SYM1('('))
            {
                struct MacroExpand* exp = array_push_zeroes(&pp->macro_fn_exp, sizeof(struct MacroExpand));
                exp->prev_paren_count = pp->paren_count;
                exp->macro_arg_offsets_offset = array_size(&pp->macro_arg_offsets, sizeof(size_t));
                exp->macrodef_offset = pp->prev_macrodef_idx_p1 - 1;
                array_push_size_t(&pp->macro_arg_offsets, array_size(&pp->toks, sizeof(struct Token)) - 1);
                pp->paren_count = 0;
                pp->prev_macrodef_idx_p1 = 0;
                continue;
            }
            pp->prev_macrodef_idx_p1 = 0;
        }
        if (toks[i].type == TOKEN_SYM1('('))
        {
            ++pp->paren_count;
        }
        else if (toks[i].type == TOKEN_SYM1(')'))
        {
            if (pp->paren_count)
                --pp->paren_count;
            else if (pp->macro_fn_exp.sz != 0)
            {
                // complete macro fn call
                struct Array tmp = {};
                struct MacroExpand* exp = array_back(&pp->macro_fn_exp, sizeof(struct MacroExpand));
                size_t fn_tok_offset = ((size_t*)pp->macro_arg_offsets.data)[exp->macro_arg_offsets_offset];
                array_push_size_t(&pp->macro_arg_offsets, array_size(&pp->toks, sizeof(struct Token)));
                UNWRAP(pp_subst_macro_args(pp, &tmp, exp));
                pp->macro_arg_offsets.sz = exp->macro_arg_offsets_offset * sizeof(size_t);
                array_pop(&pp->macro_fn_exp, sizeof(struct MacroExpand));
                // pop off macro token and all argument sequences
                pp->toks.sz = fn_tok_offset * sizeof(struct Token);
                struct MacroDef* def = (struct MacroDef*)&pp->defs_info + exp->macrodef_offset;
                UNWRAP(pp_expand_macro(
                    pp, pp_sp_str(pp, def->sp_offset), def->sp_len, tmp.data, array_size(&tmp, sizeof(struct Token))));
                array_destroy(&tmp);
                continue;
            }
        }
        else if (toks[i].type == TOKEN_SYM1(',') && !pp->paren_count && pp->macro_fn_exp.sz != 0)
        {
            // next macro fn arg
            array_push_size_t(&pp->macro_arg_offsets, array_size(&pp->toks, sizeof(struct Token)));
            array_push(&pp->toks, toks + i, sizeof(toks[i]));
            continue;
        }

        if (!toks[i].noreplace && toks[i].basic_type == LEX_IDENT)
        {
            // look up macro
            const char* s = pp_token_str(pp, toks + i);
            size_t tok_len = strlen(s);
            if (sstk_find(&pp->macro_stack, s, tok_len) != SIZE_MAX)
            {
                struct Token* tk = array_push(&pp->toks, toks + i, sizeof(toks[i]));
                tk->noreplace = 1;
                continue;
            }
            {
                size_t* v = sm_get(&pp->defines_map, s);
                if (v)
                {
                    struct MacroDef* def = (struct MacroDef*)pp->defs_info.data + *v;
                    if (def->is_function)
                    {
                        pp->prev_macrodef_idx_p1 = *v + 1;
                    }
                    else
                    {
                        UNWRAP(pp_expand_macro(pp,
                                               s,
                                               tok_len,
                                               (struct Token*)pp->defs_tokens.data + def->tok_seq_offset,
                                               def->tok_seq_extent));
                        continue;
                    }
                }
            }
        }
        array_push(&pp->toks, toks + i, sizeof(toks[i]));
    }
fail:
    return rc;
}

static int pp_expand_macro(
    struct Preprocessor* pp, const char* id, size_t id_sz, struct Token const* tks, size_t extent)
{
    int rc = 0;
    const size_t sstk_orig_size = sstk_size(&pp->macro_stack);
    sstk_push(&pp->macro_stack, id, id_sz);
    rc = pp_push_tok_seq(pp, tks, extent);
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
    struct Token tk;
    pp_form_token(pp, l, &tk);
    rc = pp_push_tok_seq(pp, &tk, 1);
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
    sstk_destroy(&pp->def_arg_names);
    array_destroy(&pp->defs_info);
    array_destroy(&pp->defs_tokens);
    array_destroy(&pp->macro_fn_exp);
    array_destroy(&pp->macro_arg_offsets);
    array_destroy(&pp->macro_tmp_buf);
    array_destroy(&pp->toks);
    array_destroy(&pp->stringpool);
    sstk_destroy(&pp->macro_stack);
}
