#include "preproc.h"

#include <limits.h>
#include <string.h>
#ifndef _WIN32
#include <sys/errno.h>
#endif

#include "array.h"
#include "errors.h"
#include "lexstate.h"
#include "rowcol.h"
#include "stdlibe.h"
#include "stringmap.h"
#include "stringpool.h"
#include "tok.h"
#include "token.h"
#include "unwrap.h"

enum PreprocessorState
{
    PP_INIT,
    PP_INCLUDE,
    PP_INCLUDE_EXPECT_END,
    PP_PRAGMA,
    PP_PRAGMA_ONCE,
    PP_EXPECT_END,
    PP_DEFINE,
    PP_DEFINE_CONT_FIRST,
    PP_DEFINE_CONT,
    PP_DEFINE_FN,
    PP_DEFINE_FN_ARG,
    PP_DEFINE_FN_COMMA,
    PP_DEFINE_FN_ELLIPSIS,
    PP_UNDEF,
    PP_IFDEF,
    PP_IFNDEF,
    PP_IF,
    PP_ELIF,
    PP_IFDEF_EXPECT_END,
    PP_ELSE,
    PP_ENDIF,
    PP_IGNORE,
};

struct StringStk
{
    struct Array lengths;
    struct Array data;
};
static void sstk_destroy(struct StringStk* ss)
{
    array_destroy(&ss->lengths);
    array_destroy(&ss->data);
}

struct MacroExpand
{
    size_t paren_count;
    size_t macro_arg_offsets_start;
    size_t prev_macrodef_idx_p1;

    unsigned int defined_is_op : 1;
    unsigned int prev_was_defined : 1;
    unsigned int prev_was_defined_paren : 1;
    unsigned int prev_was_defined_paren_id : 1;
};

struct ParsedFile
{
    int pragma_once;
    char* filename;
};

typedef struct Preprocessor
{
    enum PreprocessorState preproc;

    void (*on_include_cb)(struct Preprocessor*, const char* filename, FILE* f);

    unsigned int if_true_depth;
    unsigned int if_false_depth;

    size_t if_expr_begin;
    struct RowCol dir_rc;
    char to_include[128];
    size_t to_include_sz;

    const char* include_paths;

    // Array<char*>
    struct Array files_open;
    // Array<struct ParsedFile>
    struct Array filenames;
    size_t cur_file;

    struct StringMap defines_map;
    struct StringStk def_arg_names;
    struct Array defs_info;
    struct Array defs_tokens;
    struct MacroExpand exp;
    unsigned int in_directive : 1;

    struct StringStk macro_stack;
    struct Array macro_fn_exp;
    // Array<size_t>
    struct Array macro_arg_offsets;
    struct Array macro_tmp_buf;

    struct Array toks;
    struct StringPool stringpool;
    unsigned int debug_print_tokens : 1;
    unsigned int debug_print_ifs : 1;
    unsigned int debug_print_defines : 1;
} Preprocessor;

void preproc_free(struct Preprocessor* pp)
{
    struct ParsedFile* b = pp->filenames.data;
    for (size_t i = 0; i < array_size(&pp->filenames, sizeof(struct ParsedFile)); ++i)
    {
        my_free(b[i].filename);
    }
    array_destroy(&pp->files_open);
    array_destroy(&pp->filenames);
    sm_destroy(&pp->defines_map);
    sstk_destroy(&pp->def_arg_names);
    array_destroy(&pp->defs_info);
    array_destroy(&pp->defs_tokens);
    sstk_destroy(&pp->macro_stack);
    array_destroy(&pp->macro_fn_exp);
    array_destroy(&pp->macro_arg_offsets);
    array_destroy(&pp->macro_tmp_buf);
    array_destroy(&pp->toks);
    sp_destroy(&pp->stringpool);
    my_free(pp);
}

#define STREQ_LIT(STR, LEN, LIT) ((LEN) == sizeof(LIT) - 1 && memcmp((STR), LIT, (LEN)) == 0)

static int pp_sublex_on_token(Lexer* l);

static int pp_push_tok_seq(struct Preprocessor* pp, const struct Token* toks, size_t len);

static int pp_eval_tok_seq(struct Preprocessor* pp, size_t cur, const size_t stop);

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

__forceinline static const char* pp_sp_str(const struct Preprocessor* pp, size_t offset)
{
    return (const char*)pp->stringpool.data.data + offset;
}

const char* pp_token_str(const struct Preprocessor* pp, const struct Token* tk) { return pp_sp_str(pp, tk->sp_offset); }

// s must be null terminated
static size_t pp_sp_alloc(struct Preprocessor* p, const char* s, size_t n)
{
    return sp_insert(&p->stringpool, s, n + 1);
}

static size_t pp_sp_alloc_concat(struct Preprocessor* p, size_t o1, size_t o2)
{
    const char* s1 = pp_sp_str(p, o1);
    const char* s2 = pp_sp_str(p, o2);
    size_t n1 = strlen(s1);
    size_t n2 = strlen(s2);
    char* tmp = malloc(n1 + n2 + 1);
    memcpy(tmp, s1, n1);
    memcpy(tmp + n1, s2, n2 + 1);
    return pp_sp_alloc(p, tmp, n1 + n2);
}

struct KeywordEntry
{
    const char* const txt;
    const size_t len;
    const enum LexerState state;
};

#define KEYWORD(STATE, TXT) {TXT, sizeof(TXT) - 1, STATE},

static const struct KeywordEntry s_keywords_table[] = {X_LEX_KEYWORDS(KEYWORD)};

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
                tk->type = s_keywords_table[i].state;
                break;
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

#ifdef _WIN32
static int is_upper_lower_alpha(char ch) { return (ch >= 'a' & ch <= 'z') | (ch >= 'A' & ch <= 'Z'); }

static int path_is_absolute(const char* path)
{
    return is_upper_lower_alpha(path[0]) && path[1] == ':' && (path[2] == '/' | path[2] == '\\');
}
#else
static int path_is_absolute(const char* path) { return *path == '/'; }
#endif

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
    uint64_t va_evaluated : 1;
    uint64_t arg_evaluated : 63;
    size_t tok_seq_offset;
    size_t tok_seq_extent;
    ptrdiff_t sp_offset;
    size_t sp_len;
};

#define X_MACRO_IF_OP(Y)                                                                                               \
    Y(MACRO_OP_DEFINED, "defined", 5)                                                                                  \
    Y(MACRO_OP_NOT, "defined", 5)                                                                                      \
    Y(MACRO_OP_OR, "||", 5)                                                                                            \
    Y(MACRO_OP_AND, "&&", 5)                                                                                           \
    Y(MACRO_OP_OPAREN, "(", 5)

enum MacroIfOp
{
#define Y_COMMA(E, ...) E,
    X_MACRO_IF_OP(Y_COMMA)
#undef Y_COMMA
};

// static enum MacroIfOp lex_to_if_op(struct Lexer* l)
// {
// #define Y_STREQ(E, LIT, ...) \
//     if (STREQ_LIT(l->tok, l->sz, LIT)) return E;
//     X_MACRO_IF_OP(Y_STREQ)
// #undef Y_STREQ
//     return 0;
// }

static const struct Token* pp_parse_if_expr(struct Preprocessor* pp, const struct Token* cur, int* out_value)
{
    unsigned int add_op = 0;
    int add_value = 0;
    unsigned int mul_op = 0;
    int mul_value = 0;
    unsigned int relop = 0;
    int rel_value = 0;
    unsigned int eqop = 0;
    int eq_value = 0;
    int or_value = 0;
    int and_value = 1;
    int invert = 0;
top:
    if (cur->type == TOKEN_SYM1('('))
    {
        cur = pp_parse_if_expr(pp, cur + 1, out_value);
        if (!cur) return NULL;
        if (cur->type != TOKEN_SYM1(')'))
        {
            parser_tok_error(cur, "error: expected ')' in macro condition\n");
            return NULL;
        }
        ++cur;
    }
    else if (cur->type == LEX_NUMBER)
    {
        uint64_t u;
        if (lit_to_uint64(pp_token_str(pp, cur), &u, &cur->rc)) return NULL;
        if (u > INT_MAX) return parser_tok_error(cur, "error: literal value exceeded INT_MAX: %llu\n", u), NULL;
        *out_value = u;
        ++cur;
    }
    else if (cur->basic_type == LEX_IDENT)
    {
        *out_value = 0;
        ++cur;
    }
    else if (cur->type == TOKEN_SYM1('!'))
    {
        ++invert;
        ++cur;
        goto top;
    }
    else
    {
        parser_tok_error(cur, "error: expected primary expression in macro condition\n");
        return NULL;
    }

    if (invert)
    {
        *out_value = (invert & 1) ^ (*out_value != 0);
    }

    if (mul_op)
    {
        int64_t v;
        switch (mul_op)
        {
            case TOKEN_SYM1('*'): v = (int64_t)mul_value * (int64_t)*out_value; break;
            case TOKEN_SYM1('/'):
                if (*out_value == 0) abort();
                v = (int64_t)mul_value / (int64_t)*out_value;
                break;
            case TOKEN_SYM1('%'):
                if (*out_value <= 0) abort();
                v = (int64_t)mul_value % (int64_t)*out_value;
                break;
            default: abort();
        }
        if (v > INT_MAX || v < INT_MIN) abort();
        *out_value = v;
    }
    mul_op = 0;
    switch (cur->type)
    {
        case TOKEN_SYM1('*'):
        case TOKEN_SYM1('/'):
        case TOKEN_SYM1('%'):
            mul_op = cur->type;
            mul_value = *out_value;
            ++cur;
            goto top;
        default: break;
    }

    if (add_op)
    {
        int64_t v;
        switch (add_op)
        {
            case TOKEN_SYM1('+'): v = (int64_t)add_value + (int64_t)*out_value; break;
            case TOKEN_SYM1('-'): v = (int64_t)add_value - (int64_t)*out_value; break;
            default: abort();
        }
        if (v > INT_MAX || v < INT_MIN) abort();
        *out_value = v;
    }
    add_op = 0;
    switch (cur->type)
    {
        case TOKEN_SYM1('+'):
        case TOKEN_SYM1('-'):
            add_op = cur->type;
            add_value = *out_value;
            ++cur;
            goto top;
        default: break;
    }

    if (relop)
    {
        switch (relop)
        {
            case TOKEN_SYM1('<'): *out_value = rel_value < *out_value; break;
            case TOKEN_SYM1('>'): *out_value = rel_value > *out_value; break;
            case TOKEN_SYM2('<', '='): *out_value = rel_value <= *out_value; break;
            case TOKEN_SYM2('>', '='): *out_value = rel_value >= *out_value; break;
            default: abort();
        }
    }
    relop = 0;
    switch (cur->type)
    {
        case TOKEN_SYM1('<'):
        case TOKEN_SYM1('>'):
        case TOKEN_SYM2('<', '='):
        case TOKEN_SYM2('>', '='):
            relop = cur->type;
            rel_value = *out_value;
            ++cur;
            goto top;
        default: break;
    }

    if (eqop)
    {
        switch (eqop)
        {
            case TOKEN_SYM2('=', '='): *out_value = eq_value == *out_value; break;
            case TOKEN_SYM2('!', '='): *out_value = eq_value != *out_value; break;
            default: abort();
        }
    }
    eqop = 0;
    switch (cur->type)
    {
        case TOKEN_SYM2('=', '='):
        case TOKEN_SYM2('!', '='):
            eqop = cur->type;
            eq_value = *out_value;
            ++cur;
            goto top;
        default: break;
    }

    if (*out_value) *out_value = 1;

    if (!and_value) *out_value = 0;
    and_value = 1;
    if (cur->type == TOKEN_SYM2('&', '&'))
    {
        ++cur;
        and_value = *out_value;
        goto top;
    }

    if (or_value) *out_value = 1;
    or_value = 0;
    if (cur->type == TOKEN_SYM2('|', '|'))
    {
        ++cur;
        or_value = *out_value;
        goto top;
    }

    if (cur->type == TOKEN_SYM1('?'))
    {
        int left, right;
        cur = pp_parse_if_expr(pp, cur + 1, &left);
        if (!cur) return NULL;
        if (cur->type != TOKEN_SYM1(':'))
        {
            parser_tok_error(cur, "error: incomplete ternary operator in macro expression\n");
            return NULL;
        }
        cur = pp_parse_if_expr(pp, cur + 1, &right);
        if (!cur) return NULL;
        *out_value = *out_value ? left : right;
    }

    return cur;
}

// increments either if_false_depth or if_true_depth
static int pp_flush_expr(struct Preprocessor* pp)
{
    int rc = 0;
    struct MacroExpand exp = pp->exp;
    memset(&pp->exp, 0, sizeof(pp->exp));
    pp->exp.defined_is_op = 1;
    size_t expansion_start = array_size(&pp->toks, sizeof(struct Token));
    UNWRAP(pp_eval_tok_seq(pp, pp->if_expr_begin, expansion_start));
    {
        size_t expansion_end = array_size(&pp->toks, sizeof(struct Token));

        if (expansion_start == expansion_end)
        {
            return parser_tok_error(array_back(&pp->toks, sizeof(struct Token)), "error: expected macro expression\n");
        }
        struct Token* back = array_push_zeroes(&pp->toks, sizeof(struct Token));
        back->type = back->basic_type = LEX_EOF;

        int value = 0;
        const struct Token* cur = pp_parse_if_expr(pp, (struct Token*)pp->toks.data + expansion_start, &value);

        UNWRAP(!cur);
        if (cur->type != LEX_EOF)
        {
            UNWRAP(parser_tok_error(cur, "error: expected end of macro condition\n"));
        }

        if (value)
            ++pp->if_true_depth;
        else
            ++pp->if_false_depth;
        if (pp->debug_print_ifs)
        {
            fprintf(stderr, "%s:%d: #if %s\n", pp->dir_rc.file, pp->dir_rc.row, value ? "true" : "false");
        }
        pp->exp = exp;
    }
fail:
    return rc;
}
static int pp_handle_directive(struct Preprocessor* pp, Lexer* l)
{
    switch (pp->preproc)
    {
        case PP_INIT:
            if (l->state == LEX_IDENT)
            {
                pp->dir_rc = l->tok_rc;
                HANDLE_DIRECTIVE("ifdef", PP_IFDEF);
                HANDLE_DIRECTIVE("ifndef", PP_IFNDEF);
                if (STREQ_LIT(l->tok, l->sz, "if"))
                {
                    pp->if_expr_begin = array_size(&pp->toks, sizeof(struct Token));
                    pp->preproc = PP_IF;
                    return 0;
                }
                if (STREQ_LIT(l->tok, l->sz, "endif"))
                {
                    if (pp->if_false_depth + pp->if_true_depth == 0)
                    {
                        return parser_ferror(&pp->dir_rc, "error: unexpected #endif outside #if\n");
                    }
                    pp->preproc = PP_ENDIF;
                    return 0;
                }
                if (STREQ_LIT(l->tok, l->sz, "else"))
                {
                    if (pp->if_false_depth + pp->if_true_depth == 0)
                    {
                        return parser_ferror(&pp->dir_rc, "error: unexpected #else outside #if\n");
                    }
                    pp->preproc = PP_ELSE;
                    return 0;
                }
                if (STREQ_LIT(l->tok, l->sz, "elif"))
                {
                    if (pp->if_false_depth + pp->if_true_depth == 0)
                    {
                        return parser_ferror(&pp->dir_rc, "error: unexpected #elif outside #if\n");
                    }
                    pp->if_expr_begin = array_size(&pp->toks, sizeof(struct Token));
                    pp->preproc = PP_ELIF;
                    return 0;
                }
                if (pp->if_false_depth)
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
                if (STREQ_LIT(l->tok, l->sz, "warning"))
                {
                    parser_fmsg(warn, &l->tok_rc, "#warning\n");
                    pp->preproc = PP_IGNORE;
                    return 0;
                }
                if (STREQ_LIT(l->tok, l->sz, "error"))
                {
                    parser_ferror(&l->tok_rc, "#error\n");
                    pp->preproc = PP_IGNORE;
                    return 0;
                }
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
            if (tk->basic_type == LEX_IDENT)
            {
                struct MacroDef* def = array_back(&pp->defs_info, sizeof(struct MacroDef));
                size_t arg_idx = sstk_find(&pp->def_arg_names, l->tok, l->sz);
                if (arg_idx != SIZE_MAX)
                {
                    tk->basic_type = LEX_MACRO_ARG_BEGIN;
                    tk->type = LEX_MACRO_ARG_BEGIN + arg_idx;
                    // arg_idx < 63
                    def->arg_evaluated |= 1 << arg_idx;
                }
                else if (STREQ_LIT(l->tok, l->sz, "__VA_ARGS__"))
                {
                    tk->basic_type = LEX_MACRO_VA_ARGS;
                    tk->type = LEX_MACRO_VA_ARGS;
                    def->va_evaluated = 1;
                }
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
                struct MacroDef* def = array_back(&pp->defs_info, sizeof(struct MacroDef));
                if (def->arity == 0x3F /* 2^6 - 1 = 63*/)
                {
                    return parser_ferror(&l->tok_rc,
                                         "error: exceeded maximum number of macro function parameters (63)\n");
                }
                ++def->arity;
                sstk_push(&pp->def_arg_names, l->tok, l->sz);
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

            if (pp->debug_print_defines)
            {
                fprintf(stderr, "%s:%d: #undef %s\n", pp->dir_rc.file, pp->dir_rc.row, l->tok);
            }
            sm_remove(&pp->defines_map, l->tok);
            pp->preproc = PP_EXPECT_END;
            return 0;

        case PP_IFDEF:
        case PP_IFNDEF:
        {
            if (l->state != LEX_IDENT)
            {
                return parser_ferror(
                    &l->tok_rc, "error: expected macro name in #if%sdef\n", pp->preproc == PP_IFDEF ? "" : "n");
            }

            if (!pp->if_false_depth)
            {
                ++pp->if_true_depth;
                if ((sm_get(&pp->defines_map, l->tok) != NULL) ^ (pp->preproc == PP_IFDEF))
                    pp->if_false_depth++;
                else
                    pp->if_true_depth++;
                if (pp->debug_print_ifs)
                {
                    fprintf(stderr,
                            "%s:%d: #if %s\n",
                            pp->dir_rc.file,
                            pp->dir_rc.row,
                            pp->if_false_depth ? "false" : "true");
                }
            }
            else
            {
                pp->if_false_depth += 2;
            }
            pp->preproc = PP_IFDEF_EXPECT_END;
            return 0;
        }
        case PP_IF:
        case PP_ELIF: pp_form_token(pp, l, array_alloc(&pp->toks, sizeof(struct Token))); return 0;
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
        case PP_ELSE:
        case PP_ENDIF:
        case PP_IFDEF_EXPECT_END:
        case PP_EXPECT_END: return parser_ferror(&l->tok_rc, "error: expected end of directive: %s\n", l->tok);
        case PP_PRAGMA:
            if (l->state == LEX_IDENT)
            {
                HANDLE_DIRECTIVE("once", PP_PRAGMA_ONCE);
                HANDLE_DIRECTIVE("region", PP_IGNORE);
                HANDLE_DIRECTIVE("endregion", PP_IGNORE);
                HANDLE_DIRECTIVE("pack", PP_IGNORE);
                HANDLE_DIRECTIVE("warning", PP_IGNORE);
                HANDLE_DIRECTIVE("push_macro", PP_IGNORE);
                HANDLE_DIRECTIVE("pop_macro", PP_IGNORE);
            }
            return parser_ferror(&l->tok_rc, "error: unknown pragma directive: %s\n", l->tok);
        case PP_PRAGMA_ONCE: return parser_ferror(&l->tok_rc, "error: expected end of directive: %s\n", l->tok);
    }
    abort();
}
static int preproc_file_impl(struct Preprocessor* pp, FILE* f, const char* filename);
static int pp_include_file(struct Preprocessor* pp, struct Lexer* l)
{
    int rc = 0;
    FILE* f = NULL;
    const size_t parent_sz = path_parent_span(pp->dir_rc.file);
    if (parent_sz + pp->to_include_sz >= 256) abort();
    char filename[256];
    memcpy(filename, pp->dir_rc.file, parent_sz);
    memcpy(filename + parent_sz, pp->to_include, pp->to_include_sz + 1);

    f = fopen(filename, "r");
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
        memcpy(filename, pp->dir_rc.file, parent_sz);
        memcpy(filename + parent_sz, pp->to_include, pp->to_include_sz + 1);

        if (errno == ENOENT)
        {
            UNWRAP(parser_ferror(&pp->dir_rc, "error: could not open %s: No such file or directory\n", filename));
        }

        char buf[128];
        snprintf(buf, 128, "%s: failed to open", pp->to_include);
        perror(buf);
        UNWRAP(1);
    }

    size_t cur_if_sz = pp->if_true_depth;
    UNWRAP(preproc_file_impl(pp, f, filename));
    // pop EOF from included file
    array_pop(&pp->toks, sizeof(struct Token));
    if (pp->if_true_depth != cur_if_sz || pp->if_false_depth != 0)
    {
        UNWRAP(parser_ferror(&pp->dir_rc, "error: mismatched ifdef levels after including %s\n", filename));
    }
fail:
    if (f) fclose(f);
    return rc;
}

static int pp_flush_directive(struct Preprocessor* pp, Lexer* l)
{
    int rc = 0;
    enum PreprocessorState const s = pp->preproc;
    pp->preproc = PP_INIT;
    switch (s)
    {
        case PP_IFDEF_EXPECT_END:
        case PP_EXPECT_END:
        case PP_IGNORE:
        case PP_INIT: break;
        case PP_IFDEF: return parser_ferror(&pp->dir_rc, "error: expected macro name after #ifdef\n");
        case PP_IFNDEF: return parser_ferror(&pp->dir_rc, "error: expected macro name after #ifndef\n");
        case PP_DEFINE: return parser_ferror(&pp->dir_rc, "error: expected macro name after #define\n");
        case PP_UNDEF: return parser_ferror(&pp->dir_rc, "error: expected macro name after #undef\n");
        case PP_ELIF:
            if (pp->if_false_depth == 1)
            {
                --pp->if_false_depth;
                rc = pp_flush_expr(pp);
            }
            else if (!pp->if_false_depth)
            {
                pp->if_true_depth -= 2;
                pp->if_false_depth += 2;
            }
            array_shrink(&pp->toks, pp->if_expr_begin, sizeof(struct Token));
            return rc;
        case PP_IF:
            if (!pp->if_false_depth)
            {
                pp->if_true_depth += 1;
                rc = pp_flush_expr(pp);
            }
            else
                pp->if_false_depth += 2;
            array_shrink(&pp->toks, pp->if_expr_begin, sizeof(struct Token));
            return rc;
        case PP_ELSE:
            if (pp->if_false_depth >= 2)
            {
                return 0;
            }
            else if (pp->if_false_depth == 1)
            {
                --pp->if_false_depth;
                ++pp->if_true_depth;
            }
            else
            {
                pp->if_true_depth -= 2;
                pp->if_false_depth += 2;
            }
            break;
        case PP_ENDIF:
            if (pp->if_false_depth >= 2)
            {
                pp->if_false_depth -= 2;
            }
            else if (pp->if_false_depth)
            {
                --pp->if_false_depth;
                --pp->if_true_depth;
            }
            else
            {
                pp->if_true_depth -= 2;
            }
            break;
        case PP_DEFINE_CONT_FIRST:
        case PP_DEFINE_CONT:
        {
            sstk_shrink(&pp->def_arg_names, 0);
            struct MacroDef* def = array_back(&pp->defs_info, sizeof(struct MacroDef));
            def->tok_seq_extent = array_size(&pp->defs_tokens, sizeof(struct Token)) - def->tok_seq_offset;
            const char* s = pp_sp_str(pp, def->sp_offset);
            if (pp->debug_print_defines)
            {
                fprintf(stderr, "%s:%d: #define %s\n", pp->dir_rc.file, pp->dir_rc.row, s);
            }
            sm_insert(&pp->defines_map, s, array_size(&pp->defs_info, sizeof(struct MacroDef)) - 1);
            break;
        }
        case PP_DEFINE_FN:
        case PP_DEFINE_FN_ARG:
        case PP_DEFINE_FN_COMMA:
        case PP_DEFINE_FN_ELLIPSIS:
            return parser_ferror(&pp->dir_rc, "error: unterminated function-like macro definition\n");
        case PP_INCLUDE: return parser_ferror(&pp->dir_rc, "error: expected header to include\n");
        case PP_INCLUDE_EXPECT_END: return pp_include_file(pp, l);
        case PP_PRAGMA: return parser_ferror(&pp->dir_rc, "error: expected pragma directive: %s\n", l->tok);
        case PP_PRAGMA_ONCE: ((struct ParsedFile*)pp->filenames.data)[pp->cur_file].pragma_once = 1; return 0;
    }
    return 0;
}

static int pp_concat_token(struct Preprocessor* pp, struct Token* dst, const struct Token* src)
{
    if (dst->basic_type == LEX_PLACEHOLDER)
    {
        *dst = *src;
        return 0;
    }
    else if (src->basic_type == LEX_PLACEHOLDER)
    {
        // do nothing
        return 0;
    }
    else if (src->basic_type == LEX_NUMBER)
    {
        if (dst->basic_type == LEX_NUMBER || dst->basic_type == LEX_IDENT)
        {
            dst->sp_offset = pp_sp_alloc_concat(pp, dst->sp_offset, src->sp_offset);
            return 0;
        }
        return parser_tok_error(src, "error: pasting formed an invalid preprocessing token\n");
    }
    else if (src->basic_type == LEX_IDENT && dst->basic_type == LEX_IDENT)
    {
        dst->sp_offset = pp_sp_alloc_concat(pp, dst->sp_offset, src->sp_offset);
        return 0;
    }
    else if (src->basic_type == LEX_SYMBOL && dst->basic_type == LEX_SYMBOL)
    {
        int new_len = token_symlen(src->type) + token_symlen(dst->type);
        if (new_len > 3)
        {
            return parser_tok_error(src, "error: pasting formed an invalid preprocessing token\n");
        }
        dst->type = ((dst->type & 0x7F7F7F) << (8 * token_symlen(src->type))) | src->type;
        dst->sp_offset = pp_sp_alloc_concat(pp, dst->sp_offset, src->sp_offset);
        return 0;
    }
    else
    {
        return parser_tok_error(src, "error: pasting formed an invalid preprocessing token\n");
    }
}

static int pp_complete_fn_macro(struct Preprocessor* pp)
{
    int rc = 0;
    struct Array tmp = {};
    struct MacroExpand exp = pp->exp;
    pp->exp.prev_macrodef_idx_p1 = 0;
    pp->exp.macro_arg_offsets_start = array_size(&pp->macro_arg_offsets, sizeof(size_t));

    struct MacroDef* def = (struct MacroDef*)pp->defs_info.data + exp.prev_macrodef_idx_p1 - 1;
    size_t fn_tok_offset = ((size_t*)pp->macro_arg_offsets.data)[exp.macro_arg_offsets_start] - 2;
    size_t num_args = array_size(&pp->macro_arg_offsets, sizeof(size_t)) - exp.macro_arg_offsets_start - 1;
    size_t req_arity = def->arity ? def->arity : 1;
    if ((!def->is_ellipsis && req_arity < num_args) || req_arity > num_args)
    {
        UNWRAP(parser_tok_error(array_back(&pp->toks, sizeof(struct Token)),
                                "error: incorrect number of arguments in macro call to %s: expected %zu but got %zu\n",
                                pp_sp_str(pp, def->sp_offset),
                                def->arity,
                                num_args));
    }
    const size_t extent = def->tok_seq_extent;
    struct Token* const data = (struct Token*)pp->defs_tokens.data + def->tok_seq_offset;

    int pragma_paren_nesting = 0;
    for (size_t i = 0; i < extent; ++i)
    {
        if (pragma_paren_nesting)
        {
            if (data[i].type == TOKEN_SYM1('('))
                ++pragma_paren_nesting;
            else if (data[i].type == TOKEN_SYM1(')'))
                --pragma_paren_nesting;
            continue;
        }

        if (data[i].basic_type == LEX_MACRO_ARG_BEGIN)
        {
            size_t* const macro_arg_data = (size_t*)pp->macro_arg_offsets.data + exp.macro_arg_offsets_start;
            size_t const arg_idx = data[i].type - LEX_MACRO_ARG_BEGIN;
            struct Token* toks = pp->toks.data;
            if (i > 0 && data[i - 1].type == TOKEN_SYM1('#'))
            {
                // stringify tokens
                struct Array str = {};
                for (size_t i = macro_arg_data[arg_idx]; i < macro_arg_data[arg_idx + 1] - 1; ++i)
                {
                    if (str.sz) array_push_byte(&str, ' ');
                    array_appends(&str, pp_token_str(pp, toks + i));
                }
                array_push_byte(&str, 0);
                struct Token* tgt = array_alloc(&tmp, sizeof(struct Token));
                tgt->rc = toks[macro_arg_data[arg_idx]].rc;
                tgt->basic_type = LEX_STRING;
                tgt->noreplace = 0;
                tgt->type = LEX_STRING;
                tgt->sp_offset = pp_sp_alloc(pp, str.data, str.sz - 1);
                array_destroy(&str);
            }
            else if (i > 0 && data[i - 1].type == TOKEN_SYM2('#', '#'))
            {
                size_t arg_extent = macro_arg_data[arg_idx + 1] - 1 - macro_arg_data[arg_idx];
                if (arg_extent > 0)
                {
                    UNWRAP(pp_concat_token(pp, array_back(&tmp, sizeof(struct Token)), toks + macro_arg_data[arg_idx]));
                    array_push(&tmp, toks + macro_arg_data[arg_idx] + 1, (arg_extent - 1) * sizeof(struct Token));
                }
            }
            else if (i + 1 < extent && data[i + 1].type == TOKEN_SYM2('#', '#'))
            {
                size_t arg_extent = macro_arg_data[arg_idx + 1] - 1 - macro_arg_data[arg_idx];
                if (arg_extent == 0)
                {
                    struct Token* tok = array_push_zeroes(&tmp, sizeof(struct Token));
                    tok->basic_type = LEX_PLACEHOLDER;
                    tok->type = LEX_PLACEHOLDER;
                }
                else
                {
                    array_push(&tmp, toks + macro_arg_data[arg_idx], arg_extent * sizeof(struct Token));
                }
            }
            else
            {
                // eval arg.
                size_t res_start = array_size(&pp->toks, sizeof(struct Token));
                UNWRAP(pp_eval_tok_seq(pp, macro_arg_data[arg_idx], macro_arg_data[arg_idx + 1] - 1));
                array_push(
                    &tmp, (struct Token*)pp->toks.data + res_start, pp->toks.sz - res_start * sizeof(struct Token));
                array_shrink(&pp->toks, res_start, sizeof(struct Token));
            }
        }
        else if (i > 0 && data[i - 1].type == TOKEN_SYM2('#', '#'))
        {
            UNWRAP(pp_concat_token(pp, array_back(&tmp, sizeof(struct Token)), data + i));
        }
        else if (data[i].basic_type == LEX_MACRO_VA_ARGS)
        {
            // eval args.
            size_t res_start = array_size(&pp->toks, sizeof(struct Token));
            for (size_t i = def->arity; i < num_args; ++i)
            {
                size_t* const macro_arg_data = (size_t*)pp->macro_arg_offsets.data + exp.macro_arg_offsets_start;
                UNWRAP(pp_eval_tok_seq(pp, macro_arg_data[i] - (i > def->arity), macro_arg_data[i + 1] - 1));
            }
            array_push(&tmp, (struct Token*)pp->toks.data + res_start, pp->toks.sz - res_start * sizeof(struct Token));
            array_shrink(&pp->toks, res_start, sizeof(struct Token));
        }
        else if (data[i].type == TOKEN_SYM1('#') || data[i].type == TOKEN_SYM2('#', '#'))
        {
        }
        else if (data[i].type == LEX_UPRAGMA || data[i].type == LEX_UUPRAGMA)
        {
            // ignore pragmas
            pragma_paren_nesting = 1;
            if (i + 1 != extent) ++i;
            fprintf(stderr, "skipping pragma\n");
            continue;
        }
        else
        {
            array_push(&tmp, data + i, sizeof(struct Token));
        }
    }
    array_shrink(&pp->macro_arg_offsets, exp.macro_arg_offsets_start, sizeof(size_t));
    // pop off macro token and all argument sequences
    pp->toks.sz = fn_tok_offset * sizeof(struct Token);
    {
        const size_t sstk_orig_size = sstk_size(&pp->macro_stack);
        sstk_push(&pp->macro_stack, pp_sp_str(pp, def->sp_offset), def->sp_len);
        UNWRAP(pp_push_tok_seq(pp, tmp.data, array_size(&tmp, sizeof(struct Token))));
        sstk_shrink(&pp->macro_stack, sstk_orig_size);
    }
fail:
    array_destroy(&tmp);
    return rc;
}

static int pp_handle_tok(struct Preprocessor* pp)
{
    int rc = 0;
    struct Token* tok = array_back(&pp->toks, sizeof(struct Token));
    if (pp->exp.paren_count)
    {
        if (tok->type == TOKEN_SYM1('('))
        {
            ++pp->exp.paren_count;
        }
        else if (tok->type == TOKEN_SYM1(')'))
        {
            --pp->exp.paren_count;
            if (!pp->exp.paren_count)
            {
                // complete macro fn call
                array_push_size_t(&pp->macro_arg_offsets, array_size(&pp->toks, sizeof(struct Token)));
                UNWRAP(pp_complete_fn_macro(pp));
            }
        }
        else if (tok->type == TOKEN_SYM1(',') && pp->exp.paren_count == 1)
        {
            // next macro fn arg
            array_push_size_t(&pp->macro_arg_offsets, array_size(&pp->toks, sizeof(struct Token)));
        }
        return 0;
    }

    if (pp->exp.prev_macrodef_idx_p1)
    {
        if (tok->type == TOKEN_SYM1('('))
        {
            pp->exp.macro_arg_offsets_start = array_size(&pp->macro_arg_offsets, sizeof(size_t));
            array_push_size_t(&pp->macro_arg_offsets, array_size(&pp->toks, sizeof(struct Token)));
            pp->exp.paren_count = 1;
            return 0;
        }
        pp->exp.prev_macrodef_idx_p1 = 0;
    }
    if (pp->exp.prev_was_defined)
    {
        pp->exp.prev_was_defined = 0;
        if (tok->type == TOKEN_SYM1('('))
        {
            pp->exp.prev_was_defined_paren = 1;
            return 0;
        }
        if (tok->basic_type == LEX_IDENT)
        {
            memset(tok - 1, 0, sizeof(struct Token));
            tok[-1].rc = tok->rc;
            tok[-1].basic_type = LEX_NUMBER;
            tok[-1].type = LEX_NUMBER;
            if (sm_get(&pp->defines_map, pp_token_str(pp, tok)))
            {
                tok[-1].sp_offset = pp_sp_alloc(pp, "1", 1);
            }
            else
            {
                tok[-1].sp_offset = pp_sp_alloc(pp, "0", 1);
            }
            array_pop(&pp->toks, sizeof(struct Token));
            return 0;
        }
    }
    if (pp->exp.prev_was_defined_paren)
    {
        pp->exp.prev_was_defined_paren = 0;
        if (tok->basic_type == LEX_IDENT)
        {
            memset(tok - 2, 0, sizeof(struct Token));
            tok[-2].rc = tok->rc;
            tok[-2].basic_type = LEX_NUMBER;
            tok[-2].type = LEX_NUMBER;
            if (sm_get(&pp->defines_map, pp_token_str(pp, tok)))
            {
                tok[-2].sp_offset = pp_sp_alloc(pp, "1", 1);
            }
            else
            {
                tok[-2].sp_offset = pp_sp_alloc(pp, "0", 1);
            }
            pp->exp.prev_was_defined_paren_id = 1;
            return 0;
        }
        else
        {
            return parser_tok_error(tok, "error: expected identifier in defined() operator call\n");
        }
    }
    if (pp->exp.prev_was_defined_paren_id)
    {
        pp->exp.prev_was_defined_paren_id = 0;
        if (tok->type == TOKEN_SYM1(')'))
        {
            // pop '(', <id>, ')'
            array_pop(&pp->toks, sizeof(struct Token));
            array_pop(&pp->toks, sizeof(struct Token));
            array_pop(&pp->toks, sizeof(struct Token));
            return 0;
        }
        else
        {
            return parser_tok_error(tok, "error: expected ')' in defined() operator call\n");
        }
    }
    if (!tok->noreplace && tok->basic_type == LEX_IDENT)
    {
        // look up macro
        const char* s = pp_token_str(pp, tok);
        size_t tok_len = strlen(s);
        if (pp->exp.defined_is_op && STREQ_LIT(s, tok_len, "defined"))
        {
            pp->exp.prev_was_defined = 1;
        }
        else if (sstk_find(&pp->macro_stack, s, tok_len) != SIZE_MAX)
        {
            tok->noreplace = 1;
        }
        else
        {
            size_t* v = sm_get(&pp->defines_map, s);
            if (v)
            {
                struct MacroDef* def = (struct MacroDef*)pp->defs_info.data + *v;
                if (def->is_function)
                {
                    pp->exp.prev_macrodef_idx_p1 = *v + 1;
                }
                else
                {
                    array_pop(&pp->toks, sizeof(struct Token));
                    const size_t sstk_orig_size = sstk_size(&pp->macro_stack);
                    sstk_push(&pp->macro_stack, s, tok_len);
                    UNWRAP(pp_push_tok_seq(
                        pp, (struct Token*)pp->defs_tokens.data + def->tok_seq_offset, def->tok_seq_extent));
                    sstk_shrink(&pp->macro_stack, sstk_orig_size);
                }
            }
        }
    }
fail:
    return rc;
}

static int pp_push_tok_seq(struct Preprocessor* pp, const struct Token* toks, size_t len)
{
    int rc = 0;
    int pragma_paren_nesting = 0;
    for (size_t i = 0; i < len; ++i)
    {
        if (pragma_paren_nesting)
        {
            if (toks[i].type == TOKEN_SYM1('('))
                ++pragma_paren_nesting;
            else if (toks[i].type == TOKEN_SYM1(')'))
                --pragma_paren_nesting;
            continue;
        }

        if (toks[i].basic_type == LEX_PLACEHOLDER)
            continue;
        else if (toks[i].type == LEX_UPRAGMA || toks[i].type == LEX_UUPRAGMA)
        {
            // ignore pragmas
            pragma_paren_nesting = 1;
            if (i + 1 != len) ++i;
            fprintf(stderr, "skipping pragma\n");
            continue;
        }
        array_push(&pp->toks, toks + i, sizeof(toks[i]));
        UNWRAP(pp_handle_tok(pp));
    }
fail:
    return rc;
}

static int pp_eval_tok_seq(struct Preprocessor* pp, size_t cur, size_t stop)
{
    int rc = 0;
    for (; cur < stop; ++cur)
    {
        struct Token* tok = array_alloc(&pp->toks, sizeof(struct Token));
        *tok = ((struct Token*)pp->toks.data)[cur];
        UNWRAP(pp_handle_tok(pp));
    }
    if (pp->exp.paren_count > 0)
    {
        UNWRAP(parser_tok_error(array_back(&pp->toks, sizeof(struct Token)),
                                "error: incomplete argument list in macro call\n"));
    }
    pp->exp.prev_macrodef_idx_p1 = 0;
fail:
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

    if (pp->debug_print_tokens)
    {
        fprintf(stderr,
                "%s:%d:%d: %10s: %u, %u, %u, %s\n",
                l->tok_rc.file,
                l->tok_rc.row,
                l->tok_rc.col,
                lexstate_to_string(l->state),
                pp->in_directive,
                pp->if_false_depth,
                l->not_first,
                l->tok);
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
    if (pp->if_false_depth)
    {
        // drop tokens inside #if false
        return 0;
    }

    {
        size_t sz = array_size(&pp->toks, sizeof(struct Token));
        pp_form_token(pp, l, array_alloc(&pp->toks, sizeof(struct Token)));
        UNWRAP(pp_handle_tok(pp));
        for (; sz < array_size(&pp->toks, sizeof(struct Token)); ++sz)
        {
            ((struct Token*)pp->toks.data)[sz].rc = l->tok_rc;
        }
    }
fail:
    return rc;
}

struct Preprocessor* preproc_alloc(const char* include_paths)
{
    struct Preprocessor* pp = my_malloc(sizeof(struct Preprocessor));
    memset(pp, 0, sizeof(struct Preprocessor));
    pp->include_paths = include_paths;
    sp_init(&pp->stringpool);
    struct Token* tok_one = array_push_zeroes(&pp->defs_tokens, sizeof(struct Token));
    tok_one->type = LEX_NUMBER;
    tok_one->basic_type = LEX_NUMBER;
    tok_one->rc.file = "<builtin>";
    tok_one->sp_offset = '0' * 2;
    return pp;
}

static int lex_file(FILE* f, Lexer* l)
{
    int rc = 0;
    char buf[1024];
    size_t sz;
    while (1)
    {
        sz = fread(buf, 1, sizeof(buf), f);
        if (!sz) break;
        UNWRAP(lex(l, buf, sz));
    }
    UNWRAP(ferror(f));
    UNWRAP(end_lex(l));
fail:
    return rc;
}

static int preproc_file_impl(struct Preprocessor* pp, FILE* f, const char* filename)
{
    const size_t prev_cur_file = pp->cur_file;
    pp->cur_file = pp_find_insert_file(&pp->filenames, filename);
    struct ParsedFile* file = (struct ParsedFile*)pp->filenames.data + pp->cur_file;
    int rc = 0;
    if (file->pragma_once)
    {
        struct Token* tok = array_push_zeroes(&pp->toks, sizeof(struct Token));
        tok->type = LEX_EOF;
        tok->basic_type = LEX_EOF;
        tok->rc.file = file->filename;
    }
    else
    {
        struct SubLexer sublex;
        sublex.self = pp;
        init_lexer(&sublex.lexer, file->filename, &pp_sublex_on_token);
        array_push_ptr(&pp->files_open, file->filename);
        rc = lex_file(f, &sublex.lexer);
        array_pop_ptr(&pp->files_open);
    }
    UNWRAP(rc);
    if (pp->toks.sz == 0 ||
        ((struct Token*)pp->toks.data)[array_size(&pp->toks, sizeof(struct Token)) - 1].type != LEX_EOF)
    {
        fprintf(stderr, "internal compiler error after parsing %s\n", filename);
        abort();
    }
fail:
    pp->cur_file = prev_cur_file;
    return rc;
}

static int lex_text(struct Lexer* l, const char* text)
{
    int rc = 0;
    UNWRAP(lex(l, text, strlen(text)));
    UNWRAP(end_lex(l));
fail:
    return rc;
}

static int preproc_text_impl(struct Preprocessor* pp, const char* text, const char* filename)
{
    const size_t prev_cur_file = pp->cur_file;
    pp->cur_file = pp_find_insert_file(&pp->filenames, filename);
    struct ParsedFile* file = (struct ParsedFile*)pp->filenames.data + pp->cur_file;
    int rc = 0;
    if (file->pragma_once)
    {
        struct Token* tok = array_push_zeroes(&pp->toks, sizeof(struct Token));
        tok->type = LEX_EOF;
        tok->basic_type = LEX_EOF;
        tok->rc.file = file->filename;
    }
    else
    {
        struct SubLexer sublex;
        sublex.self = pp;
        init_lexer(&sublex.lexer, file->filename, &pp_sublex_on_token);
        array_push_ptr(&pp->files_open, file->filename);

        rc = lex_text(&sublex.lexer, text);
        array_pop_ptr(&pp->files_open);
    }
    UNWRAP(rc);
    if (pp->toks.sz == 0 ||
        ((struct Token*)pp->toks.data)[array_size(&pp->toks, sizeof(struct Token)) - 1].type != LEX_EOF)
    {
        fprintf(stderr, "internal compiler error after parsing %s\n", filename);
        abort();
    }
fail:
    pp->cur_file = prev_cur_file;
    return rc;
}

static void preproc_merge_strings(struct Preprocessor* pp)
{
    struct Array buf = {};
    size_t i;
    size_t sz = array_size(&pp->toks, sizeof(struct Token));
    struct Token* toks = pp->toks.data;
    for (i = 1; i < sz; ++i)
    {
        if (toks[i].basic_type == LEX_STRING && toks[i - 1].basic_type == LEX_STRING)
        {
            break;
        }
    }
    for (size_t j = i; j < sz; ++i, ++j)
    {
        if (toks[i - 1].basic_type == LEX_STRING && toks[j].basic_type == LEX_STRING)
        {
            // merge
            array_appends(&buf, pp_token_str(pp, toks + i - 1));
            do
            {
                array_appends(&buf, pp_token_str(pp, toks + j));
                ++j;
            } while (j < sz && toks[j].basic_type == LEX_STRING);
            if (memchr(buf.data, 0, buf.sz))
            {
                abort();
            }
            array_push_byte(&buf, '\0');
            toks[i - 1].sp_offset = sp_insert(&pp->stringpool, buf.data, buf.sz);
            array_clear(&buf);
        }
        toks[i] = toks[j];
    }
    array_shrink(&pp->toks, i, sizeof(struct Token));
    array_destroy(&buf);
}

int preproc_file(struct Preprocessor* pp, FILE* f, const char* filename)
{
    int rc = preproc_file_impl(pp, f, filename);
    if (!rc) preproc_merge_strings(pp);
    return rc;
}

int preproc_text(struct Preprocessor* pp, const char* text)
{
    int rc = preproc_text_impl(pp, text, "<text>");
    if (!rc) preproc_merge_strings(pp);
    return rc;
}

int preproc_define(struct Preprocessor* pp, const char* macro)
{
    int rc = 0;
    struct Array txt = {};
    array_appends(&txt, "#define ");
    array_appends(&txt, macro);
    char* s = memchr(txt.data, '=', txt.sz);
    if (s)
    {
        *s = ' ';
    }
    else
    {
        array_appends(&txt, " 1");
    }
    struct SubLexer sublex;
    sublex.self = pp;
    init_lexer(&sublex.lexer, "<command-line>", &pp_sublex_on_token);
    UNWRAP(lex(&sublex.lexer, txt.data, txt.sz));
    UNWRAP(end_lex(&sublex.lexer));
    // pop trailing LEX_EOF
    array_pop(&pp->toks, sizeof(struct Token));
fail:
    array_destroy(&txt);
    return rc;
}

const struct Token* preproc_tokens(const struct Preprocessor* pp) { return pp->toks.data; }
const char* preproc_stringpool(const struct Preprocessor* pp) { return pp->stringpool.data.data; }

void preproc_dump(const struct Preprocessor* pp)
{
    const struct Token* data = preproc_tokens(pp);
    do
    {
        printf("%40s:%04d:%04d: %-10s %d : %s\n",
               data->rc.file,
               data->rc.row,
               data->rc.col,
               lexstate_to_string(data->basic_type),
               data->noreplace,
               pp_token_str(pp, data));
        if (data->basic_type == LEX_EOF) break;
        ++data;
    } while (1);

    for (size_t i = 0; i < array_size(&pp->defines_map.keys.arr, sizeof(char*)); ++i)
    {
        if (((char**)pp->defines_map.keys.arr.data)[i] == NULL) continue;
        size_t def_offset = arrsz_at(&pp->defines_map.values, i);
        struct MacroDef* def = (struct MacroDef*)pp->defs_info.data + def_offset;
        printf("#define %s(%d)", ((char**)pp->defines_map.keys.arr.data)[i], def->arity);
        for (size_t i = 0; i < def->tok_seq_extent; ++i)
        {
            struct Token* tok = (struct Token*)pp->defs_tokens.data + def->tok_seq_offset + i;
            if (tok->basic_type == LEX_MACRO_ARG_BEGIN)
            {
                printf(" $%u", tok->type - LEX_MACRO_ARG_BEGIN);
            }
            else
                printf(" %s", pp_token_str(pp, tok));
        }
        printf("\n");
    }
}
