#include "parse.h"

#include <limits.h>
#include <stdio.h>
#include <string.h>

#include "ast.h"
#include "cg.h"
#include "errors.h"
#include "lexstate.h"
#include "parsestate.h"
#include "pool.h"
#include "scope.h"
#include "stdlibe.h"
#include "strlist.h"
#include "symbol.h"
#include "tok.h"
#include "token.h"

#define PARSER_FAIL_RC(RC, ...)                                                                                        \
    do                                                                                                                 \
    {                                                                                                                  \
        parser_ferror(RC, __VA_ARGS__);                                                                                \
        cur_tok = NULL;                                                                                                \
        goto fail;                                                                                                     \
    } while (0)

#define PARSER_FAIL_TOK(TOK, ...)                                                                                      \
    do                                                                                                                 \
    {                                                                                                                  \
        parser_tok_error(TOK, __VA_ARGS__);                                                                            \
        cur_tok = NULL;                                                                                                \
        goto fail;                                                                                                     \
    } while (0)

#define PARSER_FAIL(...) PARSER_FAIL_TOK(cur_tok, __VA_ARGS__)

#define PARSER_DO(X)                                                                                                   \
    do                                                                                                                 \
    {                                                                                                                  \
        cur_tok = (X);                                                                                                 \
        if (cur_tok == NULL) goto fail;                                                                                \
    } while (0)

#define PARSER_DO_WITH(X, ...)                                                                                         \
    do                                                                                                                 \
    {                                                                                                                  \
        cur_tok = (X);                                                                                                 \
        if (cur_tok == NULL)                                                                                           \
        {                                                                                                              \
            parser_tok_error(cur_tok, __VA_ARGS__);                                                                    \
            goto fail;                                                                                                 \
        }                                                                                                              \
    } while (0)

#define PARSER_CHECK_NOT(X)                                                                                            \
    do                                                                                                                 \
    {                                                                                                                  \
        if (X)                                                                                                         \
        {                                                                                                              \
            cur_tok = NULL;                                                                                            \
            goto fail;                                                                                                 \
        }                                                                                                              \
    } while (0)

#define PARSER_CHECK(X) PARSER_CHECK_NOT(!(X))

const char* token_str(Parser* p, const struct Token* tk) { return p->tk_strdata + tk->sp_offset; }
static int token_is_sym(Parser* p, const struct Token* tk, char sym) { return tk->type == TOKEN_SYM1(sym); }
static const struct Token* token_consume_sym(Parser* p, const struct Token* tk, char sym, const char* in)
{
    if (tk->type == TOKEN_SYM1(sym))
    {
        return tk + 1;
    }
    return parser_ferror(&tk->rc, "error: expected '%c'%s\n", sym, in), NULL;
}

enum Precedence op_precedence(unsigned int tok_type)
{
    switch (tok_type)
    {
        case TOKEN_SYM2('&', '='):
        case TOKEN_SYM2('|', '='):
        case TOKEN_SYM2('^', '='):
        case TOKEN_SYM2('*', '='):
        case TOKEN_SYM2('/', '='):
        case TOKEN_SYM2('%', '='):
        case TOKEN_SYM3('>', '>', '='):
        case TOKEN_SYM3('<', '<', '='):
        case TOKEN_SYM2('+', '='):
        case TOKEN_SYM2('-', '='):
        case TOKEN_SYM1('='): return PRECEDENCE_ASSIGN;
        case TOKEN_SYM2('<', '='):
        case TOKEN_SYM2('>', '='):
        case TOKEN_SYM1('<'):
        case TOKEN_SYM1('>'): return PRECEDENCE_RELATION;
        case TOKEN_SYM2('!', '='):
        case TOKEN_SYM2('=', '='): return PRECEDENCE_EQUALITY;
        case TOKEN_SYM2('|', '|'): return PRECEDENCE_OR;
        case TOKEN_SYM2('&', '&'): return PRECEDENCE_AND;
        case TOKEN_SYM1('+'):
        case TOKEN_SYM1('-'): return PRECEDENCE_ADD;
        case TOKEN_SYM1('&'): return PRECEDENCE_BIT_AND;
        case TOKEN_SYM1('|'): return PRECEDENCE_BIT_OR;
        case TOKEN_SYM1('^'): return PRECEDENCE_BIT_XOR;
        case TOKEN_SYM2('>', '>'):
        case TOKEN_SYM2('<', '<'): return PRECEDENCE_SHIFT;
        case TOKEN_SYM1('*'):
        case TOKEN_SYM1('/'):
        case TOKEN_SYM1('%'): return PRECEDENCE_MULT;
        case TOKEN_SYM1(','): return PRECEDENCE_COMMA;
        default: return PRECEDENCE_ERROR;
    }
}

static struct ExprBinOp* parse_alloc_binop(Parser* p, const struct Token* tok, struct Expr* lhs, struct Expr* rhs)
{
    struct ExprBinOp* e = (struct ExprBinOp*)pool_alloc_zeroes(&p->ast_pools[EXPR_BINOP], sizeof(struct ExprBinOp));
    e->kind = EXPR_BINOP;
    e->tok = tok;
    e->lhs = lhs;
    e->rhs = rhs;
    if (!tok) abort();
    return e;
}

static struct ExprUnOp* parse_alloc_unop(Parser* p, const struct Token* tok, struct Expr* lhs)
{
    struct ExprUnOp* e = (struct ExprUnOp*)pool_alloc_zeroes(&p->ast_pools[EXPR_UNOP], sizeof(struct ExprUnOp));
    e->kind = EXPR_UNOP;
    e->tok = tok;
    e->lhs = lhs;
    if (!tok) abort();
    return e;
}

static struct ExprBuiltin* parse_alloc_builtin(
    Parser* p, const struct Token* tok, struct Expr* e1, struct Expr* e2, struct DeclSpecs* specs, struct Decl* decl)
{
    struct ExprBuiltin e = {
        .kind = EXPR_BUILTIN,
        .tok = tok,
        .expr1 = e1,
        .expr2 = e2,
        .specs = specs,
        .type = decl,
    };
    if (!tok) abort();
    return pool_push(&p->ast_pools[e.kind], &e, sizeof(e));
}

static struct ExprRef* parse_alloc_expr_ref(Parser* p, const struct Token* tok, Symbol* sym)
{
    struct ExprRef* e = pool_alloc_zeroes(&p->ast_pools[EXPR_REF], sizeof(struct ExprRef));
    e->kind = EXPR_REF;
    e->tok = tok;
    e->sym = sym;
    return e;
}
static struct ExprLit* parse_alloc_expr_lit(Parser* p, const struct Token* tok)
{
    struct ExprLit* e = pool_alloc_zeroes(&p->ast_pools[EXPR_LIT], sizeof(struct ExprLit));
    e->kind = EXPR_LIT;
    e->tok = tok;
    e->text = token_str(p, tok);
    return e;
}
static struct ExprCast* parse_alloc_expr_cast(Parser* p, struct Decl* type, struct Expr* expr)
{
    struct ExprCast* e = pool_alloc_zeroes(&p->ast_pools[EXPR_CAST], sizeof(struct ExprCast));
    e->kind = EXPR_CAST;
    e->expr = expr;
    e->type = type;
    return e;
}
static struct ExprCall* parse_alloc_expr_call(
    Parser* p, const struct Token* tok, struct Expr* fn, size_t off, size_t ext)
{
    struct ExprCall* e = pool_alloc_zeroes(&p->ast_pools[EXPR_CALL], sizeof(struct ExprCall));
    e->kind = EXPR_CALL;
    e->tok = tok;
    e->fn = fn;
    e->param_offset = off;
    e->param_extent = ext;
    return e;
}

static struct Decl* parse_alloc_decl(Parser* p, struct DeclSpecs* specs)
{
    struct Decl* decl = pool_alloc_zeroes(&p->ast_pools[AST_DECL], sizeof(struct Decl));
    decl->kind = AST_DECL;
    decl->specs = specs;
    return decl;
}

static __forceinline void parse_push_expr_seq_arr(struct Parser* p, struct Array* arr)
{
    if (arr->sz == 0) return;
    for (size_t i = 0; i < array_size(arr, sizeof(void*)); ++i)
    {
        if (((struct Ast**)arr->data)[i]->kind == -1) abort();
    }
    array_push(&p->expr_seqs, arr->data, arr->sz);
}
static __forceinline void parse_push_decl_seq(struct Parser* p, struct Decl** data, size_t n)
{
    if (n == 0) return;
    for (size_t i = 0; i < n; ++i)
    {
        if (data[i]->kind == -1) abort();
    }
    array_push(&p->expr_seqs, data, n * sizeof(void*));
}

static const struct Token* parse_expr(Parser* p, const struct Token* cur_tok, struct Expr** ppe, int precedence);

static const struct Token* parse_expr_post_unary(Parser* p,
                                                 const struct Token* cur_tok,
                                                 struct Expr* lhs,
                                                 struct Expr** ppe)
{
top:;
    const struct Token* tok_op = cur_tok;
    if (cur_tok->type == TOKEN_SYM1('('))
    {
        ++cur_tok;
        if (cur_tok->type == TOKEN_SYM1(')'))
        {
            ++cur_tok;
            lhs = (struct Expr*)parse_alloc_expr_call(p, tok_op, lhs, 0, 0);
        }
        else
        {
#define PARAM_COUNT 32
            CallParam params[PARAM_COUNT] = {0};
            size_t i = 0;
            do
            {
                if (i == PARAM_COUNT)
                    return parser_ferror(
                               &cur_tok->rc, "error: too many arguments for function (%d supported)\n", PARAM_COUNT),
                           NULL;
                PARSER_DO(parse_expr(p, cur_tok, &params[i++].expr, PRECEDENCE_ASSIGN));
                if (cur_tok->type == TOKEN_SYM1(','))
                {
                    ++cur_tok;
                    continue;
                }
                if (cur_tok->type == TOKEN_SYM1(')'))
                {
                    ++cur_tok;
                    break;
                }
                PARSER_FAIL("error: expected ',' or ')'\n");
            } while (1);
            size_t offset = array_size(&p->callparams, sizeof(CallParam));
            array_push(&p->callparams, params, sizeof(CallParam) * i);
            lhs = (struct Expr*)parse_alloc_expr_call(p, tok_op, lhs, offset, i);
        }
        goto top;
    }
    if (cur_tok->type == TOKEN_SYM1('['))
    {
        const struct Token* tok = cur_tok++;
        struct Expr* rhs = NULL;
        PARSER_DO(parse_expr(p, cur_tok, &rhs, PRECEDENCE_COMMA));
        PARSER_DO(token_consume_sym(p, cur_tok, ']', " in array expression"));
        struct ExprBinOp* e = parse_alloc_binop(p, tok, lhs, rhs);
        lhs = &e->expr_base;
        goto top;
    }

    if (cur_tok->type == TOKEN_SYM2('+', '+') || cur_tok->type == TOKEN_SYM2('-', '-'))
    {
        struct ExprUnOp* e = parse_alloc_unop(p, cur_tok, lhs);
        e->postfix = 1;
        lhs = &e->expr_base;
        ++cur_tok;
        goto top;
    }
    if (cur_tok->type == TOKEN_SYM2('-', '>') || cur_tok->type == TOKEN_SYM1('.'))
    {
        struct ExprField* e = (struct ExprField*)pool_alloc_zeroes(&p->ast_pools[EXPR_FIELD], sizeof(struct ExprField));
        e->kind = EXPR_FIELD;
        e->tok = cur_tok++;
        e->is_arrow = e->tok->type == TOKEN_SYM2('-', '>');
        if (cur_tok->type != LEX_IDENT)
        {
            PARSER_FAIL("error: expected field name\n");
        }
        e->field_tok = cur_tok;
        e->fieldname = token_str(p, cur_tok);
        e->lhs = lhs;
        e->sym = NULL;
        lhs = &e->expr_base;
        ++cur_tok;
        goto top;
    }
    *ppe = (struct Expr*)lhs;
fail:
    return cur_tok;
}

static int tok_type_is_fundamental(unsigned int type)
{
    switch (type)
    {
#define Y(E, ...) case E:
        X_LEX_TYPE_KEYWORDS(Y)
#undef Y
        return 1;
        default: return 0;
    }
    return 0;
}
static int tok_type_is_nonfundamental(unsigned int type)
{
    switch (type)
    {
        case LEX_LONG: return 1;
        case LEX_UNSIGNED: return 1;
        case LEX_UUSIGNED: return 1;
        case LEX_SHORT: return 1;
        case LEX_SIGNED: return 1;
        default: return 0;
    }
}

static int parse_is_token_a_type(Parser* p, const struct Token* tok)
{
    switch (tok->type)
    {
        case LEX_LONG:
        case LEX_SHORT:
        case LEX_UNSIGNED:
        case LEX_SIGNED:
        case LEX_UUSIGNED:
        case LEX_VOLATILE:
        case LEX_CONST:
        case LEX_STRUCT:
        case LEX_UNION:
        case LEX_ENUM: return 1;
        case LEX_IDENT: return NULL != scope_find(&p->typedef_scope, token_str(p, tok));
        default: return tok_type_is_fundamental(tok->type);
    }
}

static const struct Token* parse_type(Parser* p,
                                      const struct Token* cur_tok,
                                      struct DeclSpecs** pspecs,
                                      struct Decl** pdecl);
static const struct Token* parse_expr_unary_atom(Parser* p, const struct Token* cur_tok, struct Expr** ppe);

static const struct Token* parse_paren_unary(Parser* p, const struct Token* cur_tok, struct Expr** ppe)
{
    struct Expr* lhs;
    if (parse_is_token_a_type(p, cur_tok))
    {
        // cast expression
        struct ExprCast* e = parse_alloc_expr_cast(p, NULL, NULL);
        e->tok = cur_tok;
        PARSER_DO(parse_type(p, cur_tok, &e->specs, &e->type));
        *ppe = &e->expr_base;
        PARSER_DO(token_consume_sym(p, cur_tok, ')', " in cast"));
        return parse_expr_unary_atom(p, cur_tok, &e->expr);
    }
    PARSER_DO(parse_expr(p, cur_tok, &lhs, PRECEDENCE_COMMA));
    PARSER_DO(token_consume_sym(p, cur_tok, ')', " in primary expression"));
    PARSER_DO(parse_expr_post_unary(p, cur_tok, lhs, ppe));
fail:
    return cur_tok;
}

#define X_PREFIX_UNARY_TOKS(Y)                                                                                         \
    Y(TOKEN_SYM1('!'))                                                                                                 \
    Y(TOKEN_SYM1('~'))                                                                                                 \
    Y(TOKEN_SYM1('&'))                                                                                                 \
    Y(TOKEN_SYM1('*'))                                                                                                 \
    Y(TOKEN_SYM1('-'))                                                                                                 \
    Y(TOKEN_SYM1('+'))                                                                                                 \
    Y(TOKEN_SYM2('-', '-'))                                                                                            \
    Y(TOKEN_SYM2('+', '+'))

static struct ExprRef* ref_from_tok(struct Parser* p, const struct Token* tok)
{
    const char* lhs_str = token_str(p, tok);
    struct Binding* const lhs_bind = scope_find(&p->scope, lhs_str);
    if (!lhs_bind)
    {
        return parser_ferror(&tok->rc, "error: '%s' undeclared\n", lhs_str), NULL;
    }
    if (!lhs_bind->sym) abort();
    return parse_alloc_expr_ref(p, tok, lhs_bind->sym);
}

static const struct Token* parse_expr_unary_atom(Parser* p, const struct Token* cur_tok, struct Expr** ppe)
{
top:
    switch (cur_tok->type)
    {
#define Y_CASE(V, ...) case V:
        X_PREFIX_UNARY_TOKS(Y_CASE)
#undef Y_CASE
        {
            struct ExprUnOp* e = parse_alloc_unop(p, cur_tok, NULL);
            *ppe = &e->expr_base;
            ppe = &e->lhs;
            ++cur_tok;
            goto top;
        }
        case TOKEN_SYM1('('): return parse_paren_unary(p, cur_tok + 1, ppe);
        case LEX_IDENT:
        {
            struct ExprRef* lhs_expr = ref_from_tok(p, cur_tok++);
            PARSER_CHECK(lhs_expr);
            return parse_expr_post_unary(p, cur_tok, (struct Expr*)lhs_expr, ppe);
        }
        case LEX_NUMBER:
        case LEX_CHARLIT:
        case LEX_STRING:
        {
            struct ExprLit* lhs_expr = parse_alloc_expr_lit(p, cur_tok++);
            if (lhs_expr->tok->type == LEX_NUMBER)
            {
                lit_to_uint64(lhs_expr->text, &lhs_expr->numeric, &lhs_expr->suffix, &lhs_expr->tok->rc);
            }
            else if (lhs_expr->tok->type == LEX_CHARLIT)
            {
                lhs_expr->numeric = lhs_expr->text[0];
            }
            else
            {
                size_t* v = bsm_get(&p->strlit_map, lhs_expr->text, lhs_expr->tok->tok_len);
                if (v)
                    lhs_expr->sym = (Symbol*)*v;
                else
                {
                    lhs_expr->sym = pool_alloc_zeroes(&p->sym_pool, sizeof(Symbol));
                    lhs_expr->sym->string_constant = lhs_expr;
                    bsm_insert(&p->strlit_map, lhs_expr->text, lhs_expr->tok->tok_len, (size_t)lhs_expr->sym);
                }
            }
            return parse_expr_post_unary(p, cur_tok, (struct Expr*)lhs_expr, ppe);
        }
        case LEX_BUILTIN_CONSTANT_P:
        case LEX_BUILTIN_BSWAP32:
        case LEX_BUILTIN_BSWAP64:;
            const struct Token* tok = cur_tok++;
            PARSER_DO(token_consume_sym(p, cur_tok, '(', " in builtin operator"));
            Expr* lhs;
            PARSER_DO(parse_expr(p, cur_tok, &lhs, PRECEDENCE_COMMA));
            *ppe = &parse_alloc_builtin(p, tok, lhs, NULL, NULL, NULL)->expr_base;
            return token_consume_sym(p, cur_tok, ')', " in builtin operator");
        case LEX_UUVA_START:
        case LEX_UUVA_ARG:
        case LEX_UUVA_COPY:
        case LEX_UUVA_END:
        {
            const struct Token* tok = cur_tok++;
            PARSER_DO(token_consume_sym(p, cur_tok, '(', " in builtin operator"));
            if (cur_tok->type != LEX_IDENT)
                PARSER_FAIL("error: expected va_list as first parameter to %s\n", token_str(p, tok));
            struct ExprRef* lhs = ref_from_tok(p, cur_tok++);
            struct Expr* rhs = NULL;
            struct DeclSpecs* specs = NULL;
            struct Decl* decl = NULL;
            PARSER_CHECK(lhs);
            if (tok->type == LEX_UUVA_START)
            {
                PARSER_DO(token_consume_sym(p, cur_tok, ',', " in builtin operator"));
                if (cur_tok->type != LEX_IDENT)
                    PARSER_FAIL("error: expected function argument as second parameter to %s\n", token_str(p, tok));
                struct ExprRef* rhs_sym = ref_from_tok(p, cur_tok++);
                PARSER_CHECK(rhs_sym);
                rhs = &rhs_sym->expr_base;
            }
            else if (tok->type == LEX_UUVA_ARG)
            {
                PARSER_DO(token_consume_sym(p, cur_tok, ',', " in builtin operator"));
                PARSER_DO(parse_type(p, cur_tok, &specs, &decl));
            }
            else if (tok->type == LEX_UUVA_COPY)
            {
                PARSER_DO(token_consume_sym(p, cur_tok, ',', " in builtin operator"));
                if (cur_tok->type != LEX_IDENT)
                    PARSER_FAIL("error: expected va_list as first parameter to %s\n", token_str(p, tok));
                rhs = &ref_from_tok(p, cur_tok++)->expr_base;
            }
            *ppe = &parse_alloc_builtin(p, tok, &lhs->expr_base, rhs, specs, decl)->expr_base;
            return token_consume_sym(p, cur_tok, ')', " in builtin operator");
        }
        case LEX_SIZEOF:
        {
            struct ExprBuiltin* e = parse_alloc_builtin(p, cur_tok, NULL, NULL, NULL, NULL);
            *ppe = &e->expr_base;
            if (token_is_sym(p, cur_tok + 1, '('))
            {
                if (parse_is_token_a_type(p, cur_tok + 2))
                {
                    PARSER_DO(parse_type(p, cur_tok + 2, &e->specs, &e->type));
                }
                else
                {
                    PARSER_DO(parse_expr(p, cur_tok + 2, &e->expr1, PRECEDENCE_COMMA));
                }
                PARSER_DO(token_consume_sym(p, cur_tok, ')', " in sizeof operator"));
                return parse_expr_post_unary(p, cur_tok, &e->expr_base, ppe);
            }
            else
            {
                ppe = &e->expr1;
                ++cur_tok;
                goto top;
            }
        }
        default: PARSER_FAIL("error: expected expression\n");
    }
fail:
    return cur_tok;
}
static const struct Token* parse_expr_continue(
    Parser* p, const struct Token* cur_tok, struct Expr* lhs, struct Expr** ppe, enum Precedence precedence)
{
    const struct Token* tok_op = cur_tok;
    enum Precedence op_prec = op_precedence(tok_op->type);
    if (op_prec != PRECEDENCE_ERROR)
    {
        if (precedence <= op_prec && op_prec <= PRECEDENCE_ASSIGN)
        {
            struct ExprBinOp* op_expr = parse_alloc_binop(p, tok_op, lhs, NULL);
            *ppe = &op_expr->expr_base;
            PARSER_DO(parse_expr(p, cur_tok + 1, &op_expr->rhs, op_prec));
            return parse_expr_continue(p, cur_tok, &op_expr->expr_base, ppe, precedence);
        }
        else if (precedence < op_prec)
        {
            struct Expr* rhs;
            PARSER_DO(parse_expr(p, cur_tok + 1, &rhs, op_prec));
            struct ExprBinOp* op_expr = parse_alloc_binop(p, tok_op, lhs, rhs);
            return parse_expr_continue(p, cur_tok, &op_expr->expr_base, ppe, precedence);
        }
    }
    else if (precedence <= PRECEDENCE_TERNARY && tok_op->type == TOKEN_SYM1('?'))
    {
        // ternary operator
        struct ExprBinOp* op_expr = parse_alloc_binop(p, tok_op, lhs, NULL);
        *ppe = &op_expr->expr_base;
        struct Expr* on_true;
        PARSER_DO(parse_expr(p, cur_tok + 1, &on_true, PRECEDENCE_COMMA));
        struct ExprBinOp* switch_expr = parse_alloc_binop(p, cur_tok, on_true, NULL);
        op_expr->rhs = &switch_expr->expr_base;
        PARSER_DO(token_consume_sym(p, cur_tok, ':', " in ternary operator"));
        PARSER_DO(parse_expr(p, cur_tok, &switch_expr->rhs, PRECEDENCE_TERNARY));
        return cur_tok;
    }
    *ppe = lhs;
fail:
    return cur_tok;
}
static const struct Token* parse_expr(Parser* p, const struct Token* cur_tok, struct Expr** ppe, int precedence)
{
    struct Expr* lhs;
    if (!(cur_tok = parse_expr_unary_atom(p, cur_tok, &lhs))) return NULL;

    return parse_expr_continue(p, cur_tok, lhs, ppe, precedence);
}

enum AttrKind
{
    ATTR_SYM,
    ATTR_ASM,
    ATTR_NONREENTRANT,
};

#if 0
static const struct Token* parse_attribute(Parser* p, const struct Token* cur_tok, struct Attribute* attr)
{
    enum AttrKind kind;
    if (cur_tok->type != LEX_IDENT) goto error;
    struct Token* attrkind = cur_tok++;
    const char* attrkind_str = token_str(p, attrkind);
    if (strcmp(attrkind_str, "sym") == 0)
    {
        kind = ATTR_SYM;
    }
    else if (strcmp(attrkind_str, "asmstr") == 0)
    {
        kind = ATTR_ASM;
    }
    else if (strcmp(attrkind_str, "nonreentrant") == 0)
    {
        attr->is_nonreentrant = 1;
        return cur_tok;
    }
    else
    {
        goto error;
    }

    if (!token_is_sym(p, cur_tok, '(')) goto error;
    ++cur_tok;
    if (cur_tok->type != LEX_STRING)
    {
        return parser_ferror(&cur_tok->rc, "error: expected attribute parameter\n"), NULL;
    }
    if (kind == ATTR_SYM)
    {
        attr->symname = token_str(p, cur_tok);
    }
    else if (kind == ATTR_ASM)
    {
        attr->asmstr = token_str(p, cur_tok);
    }
    else
    {
        abort();
    }
    ++cur_tok;
    if (!token_is_sym(p, cur_tok, ')')) goto error;
    ++cur_tok;
    return cur_tok;

error:
    return parser_ferror(&cur_tok->rc, "error: ill-formed attribute\n"), NULL;
}
#endif
static const struct Token* parse_msdeclspec(Parser* p, const struct Token* cur_tok, struct Attribute* attr)
{
    PARSER_DO(token_consume_sym(p, cur_tok, '(', " in __declspec"));
    int level = 1;
    while (level)
    {
        if (cur_tok->type == LEX_EOF)
        {
            PARSER_FAIL("error: expected ')' in __declspec\n");
        }
        else if (cur_tok->type == TOKEN_SYM1('('))
            ++level;
        else if (cur_tok->type == TOKEN_SYM1(')'))
            --level;
        ++cur_tok;
    }
fail:
    return cur_tok;
}
static const struct Token* parse_attribute_plist(Parser* p, const struct Token* cur_tok, struct Attribute* attr)
{
    if (!(cur_tok = token_consume_sym(p, cur_tok, '(', " in __attribute__"))) return NULL;
    if (!(cur_tok = token_consume_sym(p, cur_tok, '(', " in __attribute__"))) return NULL;
#if 0
    if (!token_is_sym(p, cur_tok, ')'))
    {
        do
        {
            if (!(cur_tok = parse_attribute(p, cur_tok, attr))) return NULL;
            char ch = token_expect_comma_or_cparen(p, cur_tok);
            if (ch == ')') break;
            if (ch == ',')
            {
                cur_tok++;
                continue;
            }
            if (ch == '\0') return NULL;
        } while (1);
    }
#else
    int depth = 0;
    while (cur_tok->type != LEX_EOF)
    {
        if (token_is_sym(p, cur_tok, ')'))
        {
            if (depth == 0)
                break;
            else
                --depth;
        }
        else if (token_is_sym(p, cur_tok, '('))
        {
            ++depth;
        }
        ++cur_tok;
    }
#endif
    if (!(cur_tok = token_consume_sym(p, cur_tok, ')', " in __attribute__"))) return NULL;
    return token_consume_sym(p, cur_tok, ')', " in __attribute__");
}

static const struct Token* parse_su_body(struct Parser* p, const struct Token* cur_tok, struct DeclSpecs* specs);
static const struct Token* parse_enum_body(struct Parser* p, const struct Token* cur_tok, struct DeclSpecs* specs);

static void symalloc_declspecs(Parser* p, DeclSpecs* s)
{
    if (s->name)
    {
        struct Binding* const cur_bind = scope_find(&p->type_scope, s->name);
        if (cur_bind)
        {
            s->sym = cur_bind->sym;
            s->prev_decl = s->sym->last_decl;
        }
        else
        {
            s->sym = pool_alloc_zeroes(&p->typesym_pool, sizeof(TypeSymbol));
            s->sym->name = s->name;
            scope_insert(&p->type_scope, s->sym, s->name);
        }
    }
    else
    {
        s->sym = pool_alloc_zeroes(&p->typesym_pool, sizeof(TypeSymbol));
    }
    s->sym->last_decl = s;
}

static const struct Token* parse_declspecs(Parser* p, const struct Token* cur_tok, struct DeclSpecs** pspecs)
{
    struct DeclSpecs* s = *pspecs = pool_alloc_zeroes(&p->ast_pools[AST_DECLSPEC], sizeof(DeclSpecs));
    s->kind = AST_DECLSPEC;
    s->parent = p->parent;

    do
    {
        if ((s->is_struct || s->is_union) && token_is_sym(p, cur_tok, '{'))
        {
            PARSER_DO(parse_su_body(p, cur_tok + 1, s));
            PARSER_DO(token_consume_sym(p, cur_tok, '}', " in struct/union definition"));
            continue;
        }
        else if (s->is_enum && token_is_sym(p, cur_tok, '{'))
        {
            PARSER_DO(parse_enum_body(p, cur_tok + 1, s));
            PARSER_DO(token_consume_sym(p, cur_tok, '}', " in enum definition"));
            continue;
        }
        else if (cur_tok->type == LEX_ATTRIBUTE)
        {
            PARSER_DO(parse_attribute_plist(p, cur_tok + 1, &s->attr));
            continue;
        }
        else if (cur_tok->type == LEX_DECLSPEC)
        {
            PARSER_DO(parse_msdeclspec(p, cur_tok + 1, &s->attr));
            continue;
        }
        else if (cur_tok->type == LEX_IDENT)
        {
            // already found core type
            if (s->tok) break;
            s->tok = cur_tok;
            s->name = token_str(p, cur_tok);
            struct Binding* const cur_bind = scope_find(&p->typedef_scope, s->name);
            if (!cur_bind || !cur_bind->sym)
            {
                PARSER_FAIL("error: expected type but found identifier: %s\n", s->name);
            }
            s->_typedef = cur_bind->sym;
        }
        else if (cur_tok->type == LEX_STRUCT)
        {
            s->is_struct = 1;
            goto glob_ident;
        }
        else if (cur_tok->type == LEX_ENUM)
        {
            s->is_enum = 1;
            goto glob_ident;
        }
        else if (cur_tok->type == LEX_UNION)
        {
            s->is_union = 1;
        glob_ident:
            s->tok = cur_tok++;
            if (cur_tok->type == LEX_IDENT)
            {
                s->name = token_str(p, s->tok = cur_tok++);
            }
            symalloc_declspecs(p, s);
            continue;
        }
        else if (tok_type_is_fundamental(cur_tok->type))
        {
            if (s->tok && !tok_type_is_nonfundamental(s->tok->type))
            {
                PARSER_FAIL("error: repeated type declaration specifiers are not allowed (was '%s', got '%s')\n",
                            token_str(p, s->tok),
                            token_str(p, cur_tok));
            }
            s->tok = cur_tok;
        }
        else if (cur_tok->type == LEX_UNSIGNED)
        {
            if (!s->tok) s->tok = cur_tok;
            if (s->is_unsigned || s->is_signed)
                PARSER_FAIL("error: cannot combine unsigned with previous signedness specifier\n");
            s->is_unsigned = 1;
        }
        else if (cur_tok->type == LEX_SIGNED || cur_tok->type == LEX_UUSIGNED)
        {
            if (!s->tok) s->tok = cur_tok;
            if (s->is_unsigned || s->is_signed)
                PARSER_FAIL("error: cannot combine unsigned with previous signedness specifier\n");
            s->is_signed = 1;
        }
        else if (cur_tok->type == LEX_LONG)
        {
            if (!s->tok) s->tok = cur_tok;
            if (s->is_longlong)
                PARSER_FAIL("error: long long long is invalid\n");
            else if (s->is_long)
                s->is_longlong = 1;
            else
                s->is_long = 1;
        }
        else if (cur_tok->type == LEX_SHORT)
        {
            if (!s->tok) s->tok = cur_tok;
            if (s->is_short)
                PARSER_FAIL("error: short short is invalid\n");
            else
                s->is_short = 1;
        }
        else if (cur_tok->type == LEX_CONST)
        {
            if (s->is_const) PARSER_FAIL("error: repeated 'const' declaration specifiers are not allowed\n");
            s->is_const = 1;
        }
        else if (cur_tok->type == LEX_REGISTER)
        {
            if (s->is_register) PARSER_FAIL("error: repeated 'register' declaration specifiers are not allowed\n");
            s->is_register = 1;
        }
        else if (cur_tok->type == LEX_EXTERN)
        {
            if (s->is_extern) PARSER_FAIL("error: repeated 'extern' declaration specifiers are not allowed\n");
            s->is_extern = 1;
        }
        else if (cur_tok->type == LEX_STATIC)
        {
            if (s->is_static) PARSER_FAIL("error: repeated 'static' declaration specifiers are not allowed\n");
            s->is_static = 1;
        }
        else if (cur_tok->type == LEX_INLINE || cur_tok->type == LEX_UUINLINE)
        {
            if (s->is_inline) PARSER_FAIL("error: repeated 'inline' declaration specifiers are not allowed\n");
            s->is_inline = 1;
        }
        else if (cur_tok->type == LEX_STDCALL)
        {
            s->is_stdcall = 1;
        }
        else if (cur_tok->type == LEX_CDECL)
        {
        }
        else if (cur_tok->type == LEX_TYPEDEF)
        {
            if (s->is_typedef) PARSER_FAIL("error: repeated 'typedef' declaration specifiers are not allowed\n");
            s->is_typedef = 1;
        }
        else if (cur_tok->type == LEX_VOLATILE)
        {
            if (s->is_volatile) PARSER_FAIL("error: repeated 'volatile' declaration specifiers are not allowed\n");
            s->is_volatile = 1;
        }
        else if (cur_tok->type == LEX_UUFORCEINLINE)
        {
            // ignore
        }
        else
        {
            break;
        }
        ++cur_tok;
    } while (1);

    if (!s->tok)
    {
        PARSER_FAIL("error: expected type\n");
    }
    if (s->is_long && s->is_short) PARSER_FAIL("error: long short is invalid\n");
    if (s->tok->type == LEX_CHAR)
    {
        if (s->is_long) PARSER_FAIL("error: long char is invalid\n");
        if (s->is_short) PARSER_FAIL("error: short char is invalid\n");
    }

fail:
    return cur_tok;
}

static const struct Token* parse_stmt(Parser* p, const struct Token* cur_tok, struct Ast** p_expr);
static const struct Token* parse_stmt_block(Parser* p, const struct Token* cur_tok, struct StmtBlock** p_expr);

static const struct Token* parse_declarator(Parser* p, const struct Token* cur_tok, struct Decl* decl);

static struct StmtDecls* push_stmt_decl(struct Parser* p, struct DeclSpecs* specs, struct Decl* decl)
{
    struct StmtDecls ret = {
        .kind = STMT_DECLS,
        .specs = specs,
        .offset = array_size(&p->expr_seqs, sizeof(void*)),
        .extent = 1,
    };
    parse_push_decl_seq(p, &decl, 1);
    return pool_push(&p->ast_pools[STMT_DECLS], &ret, sizeof(ret));
}

static const struct Token* parse_declarator_fnargs_knr(Parser* p,
                                                       const struct Token* cur_tok,
                                                       struct DeclFn* fn,
                                                       Array* out_args_array)
{
    Array prototypes = {0};
    while (1)
    {
        if (cur_tok->type != LEX_IDENT)
        {
            PARSER_FAIL("error: expected identifier in prototype list.\n");
        }
        arrsz_push(&prototypes, cur_tok->sp_offset);
        ++cur_tok;
        if (cur_tok->type == TOKEN_SYM1(','))
        {
            ++cur_tok;
            continue;
        }
        PARSER_DO(token_consume_sym(p, cur_tok, ')', " in function prototype"));
        break;
    }
    const size_t n = prototypes.sz / sizeof(size_t);
    const size_t* const protos = prototypes.data;
    array_assign_zeroes(out_args_array, n * sizeof(void*));
    StmtDecls** const data = out_args_array->data;
    for (size_t i = 0; i < n; ++i)
    {
        struct DeclSpecs* arg_specs;

        PARSER_DO(parse_declspecs(p, cur_tok, &arg_specs));
        arg_specs->is_fn_arg = 1;
        struct Decl* arg_decl = parse_alloc_decl(p, arg_specs);
        PARSER_DO(parse_declarator(p, cur_tok, arg_decl));
        const size_t sp = arg_decl->tok->sp_offset;
        size_t j;
        for (j = 0; j < n; ++j)
        {
            if (protos[j] == sp) goto found;
        }
        PARSER_FAIL("error: K&R argument definition does not match one of listed arguments.\n");
    found:
        if (data[j] != NULL) PARSER_FAIL("error: K&R argument redefinition.\n");
        data[j] = push_stmt_decl(p, arg_specs, arg_decl);
        PARSER_DO(token_consume_sym(p, cur_tok, ';', " in function prototype"));
    }

fail:
    array_destroy(&prototypes);
    return cur_tok;
}

/// \param fn uninitialized out param
static const struct Token* parse_declarator_fnargs(Parser* p, const struct Token* cur_tok, struct DeclFn* fn)
{
    struct Array args_array = {0};
    memset(fn, 0, sizeof(DeclFn));
    fn->kind = AST_DECLFN;
    fn->tok = cur_tok;
    ++cur_tok;
    if (cur_tok->type == TOKEN_SYM1(')'))
    {
        ++cur_tok;
    }
    else if (cur_tok->type == LEX_VOID && cur_tok[1].type == TOKEN_SYM1(')'))
    {
        cur_tok += 2;
    }
    else
    {
        if (cur_tok->type == LEX_IDENT)
        {
            struct Binding* const cur_bind = scope_find(&p->typedef_scope, token_str(p, cur_tok));
            if (!cur_bind || !cur_bind->sym)
            {
                // not a typename, K&R style prototype.
                PARSER_DO(parse_declarator_fnargs_knr(p, cur_tok, fn, &args_array));
                goto parsed_arg_array;
            }
        }

        while (1)
        {
            if (cur_tok->type == TOKEN_SYM3('.', '.', '.'))
            {
                fn->is_varargs = 1;
                PARSER_DO(token_consume_sym(p, cur_tok + 1, ')', " after ... in function declaration"));
                break;
            }

            struct DeclSpecs* arg_specs;

            PARSER_DO(parse_declspecs(p, cur_tok, &arg_specs));
            arg_specs->is_fn_arg = 1;
            struct Decl* arg_decl = parse_alloc_decl(p, arg_specs);
            PARSER_DO(parse_declarator(p, cur_tok, arg_decl));
            arrptr_push(&args_array, push_stmt_decl(p, arg_specs, arg_decl));

            if (cur_tok->type == TOKEN_SYM1(','))
            {
                ++cur_tok;
                continue;
            }
            else if (cur_tok->type == TOKEN_SYM1(')'))
            {
                ++cur_tok;
                break;
            }
            PARSER_FAIL("error: expected ',' and further parameter declarations or ')'\n");
        }
    parsed_arg_array:
        fn->offset = array_size(&p->expr_seqs, sizeof(struct Expr*));
        fn->extent = array_size(&args_array, sizeof(StmtDecls*));
        parse_push_expr_seq_arr(p, &args_array);
    }

fail:
    array_destroy(&args_array);
    return cur_tok;
}

static const struct Token* parse_declarator_arr(Parser* p, const struct Token* cur_tok, struct DeclArr** out_declarr)
{
    struct DeclArr* arr = *out_declarr = pool_alloc(&p->ast_pools[AST_DECLARR], sizeof(struct DeclArr));
    memset(arr, 0, sizeof(struct DeclArr));
    arr->kind = AST_DECLARR;
    arr->tok = cur_tok;
    ++cur_tok;
    if (cur_tok->type == TOKEN_SYM1(']'))
    {
        ++cur_tok;
    }
    else
    {
        PARSER_DO(parse_expr(p, cur_tok, &arr->arity, PRECEDENCE_COMMA));
        PARSER_DO(token_consume_sym(p, cur_tok, ']', " in array declaration"));
    }

fail:
    return cur_tok;
}

static const Token* parse_declarator1(
    Parser* p, const Token* cur_tok, const Token** out_id, AstType** out_type, AstType*** out_p_basetype)
{
    AstType* base_expr = NULL;
    AstType** out_base_expr = NULL;
    if (cur_tok->type == LEX_CDECL) ++cur_tok;
    if (cur_tok->type == TOKEN_SYM1('*'))
    {
        struct DeclPtr* base_ptr = pool_alloc_zeroes(&p->ast_pools[AST_DECLPTR], sizeof(struct DeclPtr));
        base_ptr->kind = AST_DECLPTR;
        base_ptr->tok = cur_tok;
        out_base_expr = &base_ptr->type;
        ++cur_tok;
        while (1)
        {
            if (cur_tok->type == TOKEN_SYM1('*'))
            {
                struct DeclPtr* p2 = pool_alloc_zeroes(&p->ast_pools[AST_DECLPTR], sizeof(struct DeclPtr));
                p2->kind = AST_DECLPTR;
                p2->tok = cur_tok;
                p2->type = &base_ptr->ast_type;
                base_ptr = p2;
                ++cur_tok;
            }
            else if (cur_tok->type == LEX_CONST)
            {
                base_ptr->is_const = 1;
                ++cur_tok;
            }
            else if (cur_tok->type == LEX_VOLATILE)
            {
                base_ptr->is_volatile = 1;
                ++cur_tok;
            }
            else if (cur_tok->type == LEX_UURESTRICT || cur_tok->type == LEX_RESTRICT)
            {
                base_ptr->is_restrict = 1;
                ++cur_tok;
            }
            else
                break;
        }
        base_expr = &base_ptr->ast_type;
    }
    if (cur_tok->type == TOKEN_SYM1('('))
    {
        PARSER_DO(parse_declarator1(p, cur_tok + 1, out_id, out_type, &out_type));
        PARSER_DO(token_consume_sym(p, cur_tok, ')', " in declarator"));
    }
    else
    {
        if (cur_tok->type == LEX_CDECL)
        {
            ++cur_tok;
        }
        if (cur_tok->type == LEX_IDENT)
        {
            *out_id = cur_tok++;
        }
        else
        {
            *out_id = NULL;
        }
    }

    do
    {
        if (cur_tok->type == TOKEN_SYM1('('))
        {
            struct DeclFn* declfn = pool_alloc(&p->ast_pools[AST_DECLFN], sizeof(struct DeclFn));
            PARSER_DO(parse_declarator_fnargs(p, cur_tok, declfn));
            *out_type = &declfn->ast_type;
            out_type = &declfn->type;
            continue;
        }
        else if (cur_tok->type == TOKEN_SYM1('['))
        {
            struct DeclArr* declarr = NULL;
            PARSER_DO(parse_declarator_arr(p, cur_tok, &declarr));
            *out_type = &declarr->ast_type;
            out_type = &declarr->type;
            continue;
        }
        break;
    } while (1);

    if (base_expr)
    {
        *out_type = base_expr;
        *out_p_basetype = out_base_expr;
    }
    else
    {
        *out_p_basetype = out_type;
    }
fail:
    return cur_tok;
}

static const struct Token* parse_declarator(Parser* p, const struct Token* cur_tok, struct Decl* decl)
{
    Decl* const prev_parent = p->parent;
    p->parent = decl;
    while (cur_tok->type == LEX_ATTRIBUTE)
    {
        ++cur_tok;
        PARSER_DO(parse_attribute_plist(p, cur_tok, &decl->attr));
    }
    while (cur_tok->type == LEX_DECLSPEC)
    {
        ++cur_tok;
        PARSER_DO(parse_msdeclspec(p, cur_tok, &decl->attr));
    }
    AstType** p_basetype = NULL;
    PARSER_DO(parse_declarator1(p, cur_tok, &decl->tok, &decl->type, &p_basetype));
    *p_basetype = &decl->specs->ast_type;
    while (1)
    {
        if (cur_tok->type == LEX_ATTRIBUTE)
        {
            PARSER_DO(parse_attribute_plist(p, cur_tok + 1, &decl->attr));
            continue;
        }
        else if (cur_tok->type == LEX_UUASM)
        {
            PARSER_DO(parse_msdeclspec(p, cur_tok + 1, &decl->attr));
            continue;
        }
        break;
    }
fail:
    p->parent = prev_parent;
    return cur_tok;
}

static const struct Token* parse_type(Parser* p,
                                      const struct Token* cur_tok,
                                      struct DeclSpecs** pspecs,
                                      struct Decl** pdecl)
{
    scope_push_subscope(&p->scope);
    PARSER_DO(parse_declspecs(p, cur_tok, pspecs));
    *pdecl = parse_alloc_decl(p, *pspecs);
    PARSER_DO(parse_declarator(p, cur_tok, *pdecl));
    if ((*pdecl)->tok)
    {
        PARSER_FAIL_TOK((*pdecl)->tok, "error: type expression should not have a declared identifier\n");
    }

fail:
    scope_pop_subscope(&p->scope);
    return cur_tok;
}

static const struct Token* parse_integral_constant_expr(struct Parser* p, const struct Token* cur_tok)
{
    if (cur_tok->type != LEX_NUMBER)
    {
        PARSER_FAIL("error: expected constant integer expression\n");
    }
    ++cur_tok;
fail:
    return cur_tok;
}

static struct StmtDecls* push_stmt_decls(struct Parser* p, struct DeclSpecs* specs, struct Array* arr)
{
    struct StmtDecls ret = {
        .kind = STMT_DECLS,
        .specs = specs,
        .offset = array_size(&p->expr_seqs, sizeof(void*)),
        .extent = array_size(arr, sizeof(void*)),
    };
    parse_push_expr_seq_arr(p, arr);
    return pool_push(&p->ast_pools[STMT_DECLS], &ret, sizeof(ret));
}

static void make_new_sym(Parser* p, Decl* decl)
{
    decl->sym = pool_alloc_zeroes(&p->sym_pool, sizeof(Symbol));
    if (decl->tok) decl->sym->name = token_str(p, decl->tok);
    decl->sym->last_decl = decl;
}

static const struct Token* parse_enum_body(struct Parser* p, const struct Token* cur_tok, struct DeclSpecs* specs)
{
    struct Array decls = {};
    while (cur_tok->type == LEX_IDENT)
    {
        struct Decl decl = {
            .kind = AST_DECL,
            .tok = cur_tok,
            .specs = specs,
            .type = &specs->ast_type,
        };
        ++cur_tok;
        if (cur_tok->type == TOKEN_SYM1('='))
        {
            struct Expr* init;
            PARSER_DO_WITH(parse_expr(p, cur_tok + 1, &init, PRECEDENCE_ASSIGN), "       in enum declaration\n");
            decl.init = &init->ast;
        }
        struct Decl* enumerator = pool_push(&p->ast_pools[AST_DECL], &decl, sizeof(decl));
        make_new_sym(p, enumerator);
        enumerator->sym->def = enumerator;
        enumerator->sym->is_enum_constant = 1;
        scope_insert(&p->scope, enumerator->sym, enumerator->sym->name);
        arrptr_push(&decls, enumerator);
        if (cur_tok->type == TOKEN_SYM1(','))
        {
            ++cur_tok;
        }
        else
        {
            break;
        }
    }
    specs->enum_init = push_stmt_decls(p, NULL, &decls);
fail:
    array_destroy(&decls);
    return cur_tok;
}
static const struct Token* parse_initializer_list_impl(Parser* p,
                                                       const struct Token* cur_tok,
                                                       struct AstInit** out_expr);
static const struct Token* parse_initializer_list(Parser* p, const struct Token* cur_tok, struct AstInit** out_expr)
{
    const Token* tk = parse_initializer_list_impl(p, cur_tok, out_expr);
    if (tk)
    {
        AstInit* i = *out_expr;
        i->is_braced_strlit =
            i->init && !i->next->init && i->init->kind == EXPR_LIT && i->init->tok->type == LEX_STRING;
    }
    return tk;
}
static const struct Token* parse_initializer_list_impl(Parser* p,
                                                       const struct Token* cur_tok,
                                                       struct AstInit** out_expr)
{
    for (;;)
    {
        if (cur_tok->type == TOKEN_SYM1('}')) break;
        struct AstInit elem = {
            .kind = AST_INIT,
            .tok = cur_tok,
            .designator_offset = array_size(&p->designators, sizeof(struct Designator)),
        };
        do
        {
            if (cur_tok->type == TOKEN_SYM1('.'))
            {
                struct Designator* d = array_push_zeroes(&p->designators, sizeof(struct Designator));
                const struct Token* id = ++cur_tok;
                if (id->type != LEX_IDENT)
                {
                    PARSER_FAIL("error: expected identifier in designated initializer\n");
                }
                d->field = token_str(p, id);
                ++cur_tok;
                continue;
            }
            if (cur_tok->type == TOKEN_SYM1('['))
            {
                struct Designator* d = array_push_zeroes(&p->designators, sizeof(struct Designator));
                PARSER_DO(parse_expr(p, cur_tok + 1, &d->array_expr, PRECEDENCE_COMMA));
                PARSER_DO(token_consume_sym(p, cur_tok, ']', " in designated initializer"));
                continue;
            }
            break;
        } while (1);
        elem.designator_extent = array_size(&p->designators, sizeof(struct Designator)) - elem.designator_offset;
        if (elem.designator_extent)
        {
            PARSER_DO(token_consume_sym(p, cur_tok, '=', " in designated initializer"));
        }
        if (cur_tok->type == TOKEN_SYM1('{'))
        {
            struct AstInit* init;
            PARSER_DO(parse_initializer_list(p, cur_tok + 1, &init));
            elem.init = &init->ast;
        }
        else
        {
            struct Expr* expr;
            PARSER_DO(parse_expr(p, cur_tok, &expr, PRECEDENCE_ASSIGN));
            elem.init = &expr->ast;
        }
        *out_expr = pool_push(&p->ast_pools[AST_INIT], &elem, sizeof(elem));
        out_expr = &((struct AstInit*)*out_expr)->next;
        if (cur_tok->type == TOKEN_SYM1(','))
            ++cur_tok;
        else
            break;
    }

    struct AstInit elem = {
        .kind = AST_INIT,
        .tok = cur_tok,
    };
    *out_expr = pool_push(&p->ast_pools[AST_INIT], &elem, sizeof(elem));

    PARSER_DO(token_consume_sym(p, cur_tok, '}', " in initializer list"));

fail:
    return cur_tok;
}

/// \param in_subscope search and insert only into the topmost scope
/// \returns nonzero on failures
static int insert_declaration(Parser* p, Decl* decl, int in_subscope)
{
    Scope* const scope = scope_in_subscope(&p->su_scope) ? &p->su_scope : &p->scope;
    if (decl->tok)
    {
        const char* name = token_str(p, decl->tok);
        struct Binding* prev_sym = in_subscope ? scope_find_subscope(scope, name) : scope_find(scope, name);
        if (prev_sym)
        {
            decl->sym = prev_sym->sym;
            decl->prev_decl = decl->sym->last_decl;
            decl->sym->last_decl = decl;
#if defined(TRACING_SCOPES)
            fprintf(stderr, "redecl: %s\n", decl->sym->name);
#endif
            if (0)
            {
                // TODO: ensure symbols match
                return parser_tok_error(decl->tok, "error: declaration doesn't match previous\n");
            }
        }
        else
        {
            make_new_sym(p, decl);
            scope_insert(scope, decl->sym, decl->sym->name);
        }
    }
    else
    {
        make_new_sym(p, decl);
    }
#if defined(TRACING_SCOPES)
    if (scope == &p->su_scope)
        fprintf(stderr, "declare su_scope(%d): %s\n", in_subscope, decl->sym->name);
    else
        fprintf(stderr, "declare scope(%d): %s\n", in_subscope, decl->sym->name);
#endif

    return 0;
}

/// \returns nonzero on failures
static int insert_definition(Parser* p, Decl* decl)
{
    if (insert_declaration(p, decl, 1)) return 1;

    if (decl->sym->def)
    {
        if (decl->specs->is_typedef)
        {
            // Todo: ensure matching typedefs
        }
        else
        {
            parser_tok_error(decl->sym->def->tok, "info: previous definition\n");
            if (decl->sym->name)
                parser_tok_error(decl->tok, "error: multiple definition of symbol '%s'\n", decl->sym->name);
            else
                parser_tok_error(decl->tok, "error: multiple definition of symbol\n");
            return 1;
        }
    }
    else
    {
        decl->sym->def = decl;
        decl->sym->parent_su = p->cur_su;
    }
    return 0;
}

/// \returns nonzero on failures
static int insert_typedef(Parser* p, Decl* decl)
{
    if (!decl->tok) abort();

    const char* name = token_str(p, decl->tok);
    struct Binding* prev_sym = scope_find(&p->typedef_scope, name);
    if (prev_sym)
    {
        decl->sym = prev_sym->sym;
        decl->prev_decl = decl->sym->last_decl;
        decl->sym->last_decl = decl;
        if (0)
        {
            // TODO: ensure symbols match
            return parser_tok_error(decl->tok, "error: declaration doesn't match previous\n");
        }
    }
    else
    {
        make_new_sym(p, decl);
        scope_insert(&p->typedef_scope, decl->sym, decl->sym->name);
    }
    return 0;
}

static const struct Token* parse_fnbody(Parser* p, const struct Token* cur_tok, Decl* pdecl)
{
    Decl* const prev_parent = p->parent;
    scope_push_subscope(&p->scope);
    if (pdecl->type->kind != AST_DECLFN)
        PARSER_FAIL_TOK(cur_tok, "error: only function types can be defined with a code block\n");
    DeclFn* fn = (DeclFn*)pdecl->type;
    void** decls = p->expr_seqs.data;
    for (size_t i = 0; i < fn->extent; ++i)
    {
        StmtDecls* argdecl = decls[fn->offset + i];
        PARSER_CHECK_NOT(insert_definition(p, decls[argdecl->offset]));
    }
    p->parent = pdecl;
    struct StmtBlock* init;
    PARSER_DO(parse_stmt_block(p, cur_tok + 1, &init));
    pdecl->init = &init->ast;
    PARSER_DO(token_consume_sym(p, cur_tok, '}', " in function definition"));
fail:
    // Remove function arguments from the scope
    scope_pop_subscope(&p->scope);
    p->parent = prev_parent;

    return cur_tok;
}

static const struct Token* parse_decls(Parser* p,
                                       const struct Token* cur_tok,
                                       struct DeclSpecs* specs,
                                       struct Array* pdecls)
{
    while (1)
    {
        struct Decl* pdecl = parse_alloc_decl(p, specs);
        const struct Token* const declarator_tok = cur_tok;
        PARSER_DO(parse_declarator(p, cur_tok, pdecl));
        arrptr_push(pdecls, pdecl);
        if (cur_tok->type == TOKEN_SYM1('{'))
        {
            if (!pdecl->tok) PARSER_FAIL_TOK(cur_tok, "error: anonymous function declaration not allowed\n");
            if (scope_in_subscope(&p->su_scope))
                PARSER_FAIL_TOK(cur_tok, "error: member function definitions are not allowed\n");
            PARSER_CHECK_NOT(insert_definition(p, pdecl));
            PARSER_DO(parse_fnbody(p, cur_tok, pdecl));
            break;
        }

        if (specs->is_typedef && !pdecl->tok) abort();

        if (specs->is_typedef)
        {
            PARSER_CHECK_NOT(insert_typedef(p, pdecl));
        }
        else if (!specs->is_extern && pdecl->type->kind != AST_DECLFN)
        {
            PARSER_CHECK_NOT(insert_definition(p, pdecl));
        }
        else
        {
            PARSER_CHECK_NOT(insert_declaration(p, pdecl, 0));
        }

        if (!pdecl->sym) abort();

        if (cur_tok->type == TOKEN_SYM1(':'))
        {
            PARSER_DO_WITH(parse_integral_constant_expr(p, cur_tok + 1), "       in bitfield declaration\n");
        }
        else if (!pdecl->sym->name)
        {
            PARSER_FAIL_TOK(cur_tok, "error: only bitfield members can be anonymous\n");
        }

        if (cur_tok->type == TOKEN_SYM1('='))
        {
            ++cur_tok;
            if (cur_tok->type == TOKEN_SYM1('{'))
            {
                // brace initialization
                struct AstInit* init;
                PARSER_DO(parse_initializer_list(p, cur_tok + 1, &init));
                pdecl->init = &init->ast;
            }
            else
            {
                struct Expr* init;
                PARSER_DO(parse_expr(p, cur_tok, &init, PRECEDENCE_ASSIGN));
                pdecl->init = &init->ast;
            }
        }

        if (cur_tok->type == TOKEN_SYM1(','))
        {
            ++cur_tok;
        }
        else if (cur_tok->type == TOKEN_SYM1(';'))
        {
            ++cur_tok;
            break;
        }
        else
        {
            parser_tok_error(specs->tok,
                             "error: unterminated declaration of %zu item(s)\n",
                             array_size(pdecls, sizeof(struct Decl*)));
            parser_tok_error(declarator_tok, "    after this declarator\n");
            PARSER_FAIL("    expected ',' or ';' here\n");
        }
    }
fail:
    return cur_tok;
}

static const struct Token* parse_conditional(Parser* p, const struct Token* cur_tok, struct Expr** p_cond)
{
    if (!(cur_tok = token_consume_sym(p, cur_tok, '(', " in conditional statement"))) return NULL;

    if (!(cur_tok = parse_expr(p, cur_tok, p_cond, PRECEDENCE_COMMA))) return NULL;

    return token_consume_sym(p, cur_tok, ')', " in conditional statement");
}

static const struct Token* parse_stmt_decl(Parser* p, const struct Token* cur_tok, struct Ast** past)
{
    struct Array arr_decls = {};
    struct DeclSpecs* specs;
    PARSER_DO(parse_declspecs(p, cur_tok, &specs));
    if (specs->is_struct || specs->is_union || specs->is_enum)
    {
        if (token_is_sym(p, cur_tok, ';'))
        {
            ++cur_tok;
            goto finish;
        }
    }
    PARSER_DO(parse_decls(p, cur_tok, specs, &arr_decls));
finish:
    *past = &push_stmt_decls(p, specs, &arr_decls)->ast;
fail:
    array_destroy(&arr_decls);
    return cur_tok;
}

static const struct Token* parse_stmt_block(Parser* p, const struct Token* cur_tok, struct StmtBlock** p_expr);

static const struct Token* parse_su_body(struct Parser* p, const struct Token* cur_tok, struct DeclSpecs* specs)
{
#if defined(TRACING_SCOPES)
    fprintf(stderr, "parse_su_body(): push %s\n", specs->name);
#endif
    TypeSymbol* const prev_su = p->cur_su;
    p->cur_su = specs->sym;
    scope_push_subscope(&p->su_scope);
    PARSER_DO(parse_stmt_block(p, cur_tok, &specs->suinit));
    for (size_t i = 0; i < specs->suinit->extent; ++i)
    {
        StmtDecls* decls = ((StmtDecls**)p->expr_seqs.data)[specs->suinit->offset + i];
        if (decls->extent == 0 && !decls->specs->name)
        {
            // Inject anonymous declarations into suinit bodies
            Decl* anon_decl = parse_alloc_decl(p, decls->specs);
            anon_decl->type = &decls->specs->ast_type;
            decls->offset = array_size(&p->expr_seqs, sizeof(void*));
            decls->extent = 1;
            arrptr_push(&p->expr_seqs, anon_decl);
            PARSER_CHECK_NOT(insert_definition(p, anon_decl));
        }
    }

fail:
#if defined(TRACING_SCOPES)
    fprintf(stderr, "parse_su_body(): pop %s\n", specs->name);
#endif
    scope_pop_subscope(&p->su_scope);
    p->cur_su = prev_su;
    return cur_tok;
}

static const struct Token* parse_stmts(Parser* p, const struct Token* cur_tok, size_t* p_offset, size_t* p_extent)
{
    struct Array arr_stmts = {0};
    do
    {
        if (cur_tok->type == LEX_EOF || token_is_sym(p, cur_tok, '}')) break;
        PARSER_DO(parse_stmt(p, cur_tok, array_alloc(&arr_stmts, sizeof(void*))));
        if ((*(struct Ast**)array_back(&arr_stmts, sizeof(void*)))->kind == -1) abort();
    } while (1);
    *p_offset = array_size(&p->expr_seqs, sizeof(void*));
    *p_extent = array_size(&arr_stmts, sizeof(void*));
    parse_push_expr_seq_arr(p, &arr_stmts);
fail:
    array_destroy(&arr_stmts);
    return cur_tok;
}

static const struct Token* parse_stmt_block(Parser* p, const struct Token* cur_tok, struct StmtBlock** p_expr)
{
    struct StmtBlock ret = {
        .kind = STMT_BLOCK,
    };
    PARSER_DO(parse_stmts(p, cur_tok, &ret.offset, &ret.extent));
    *p_expr = pool_push(&p->ast_pools[STMT_BLOCK], &ret, sizeof(ret));
fail:
    return cur_tok;
}

static const struct Token* parse_stmt(Parser* p, const struct Token* cur_tok, struct Ast** p_expr)
{
    if (parse_is_token_a_type(p, cur_tok))
    {
        return parse_stmt_decl(p, cur_tok, p_expr);
    }
    switch (cur_tok->type)
    {
        case LEX_TYPEDEF:
        case LEX_EXTERN:
        case LEX_STATIC:
        case LEX_ATTRIBUTE:
        case LEX_INLINE:
        case LEX_AUTO:
        case LEX_REGISTER:
        {
            return parse_stmt_decl(p, cur_tok, p_expr);
        }
        case LEX_RETURN:
        {
            struct StmtReturn ret = {
                .kind = STMT_RETURN,
                .tok = cur_tok++,
            };
            if (token_is_sym(p, cur_tok, ';'))
            {
                cur_tok++;
            }
            else
            {
                PARSER_DO(parse_expr(p, cur_tok, &ret.expr, PRECEDENCE_COMMA));
                PARSER_DO(token_consume_sym(p, cur_tok, ';', " in return statement"));
            }
            *p_expr = pool_push(&p->ast_pools[STMT_RETURN], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_IF:
        {
            struct StmtIf ret = {
                .kind = STMT_IF,
            };
            scope_push_subscope(&p->scope);
            PARSER_DO(parse_conditional(p, cur_tok + 1, &ret.cond));
            PARSER_DO(parse_stmt(p, cur_tok, &ret.if_body));
            scope_pop_subscope(&p->scope);

            if (cur_tok->type == LEX_ELSE)
            {
                scope_push_subscope(&p->scope);
                PARSER_DO(parse_stmt(p, cur_tok + 1, &ret.else_body));
                scope_pop_subscope(&p->scope);
            }
            else
            {
                ret.else_body = NULL;
            }

            *p_expr = pool_push(&p->ast_pools[STMT_IF], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_CASE:
        {
            struct StmtCase ret = {
                .kind = STMT_CASE,
                .tok = cur_tok++,
            };
            PARSER_DO(parse_expr(p, cur_tok, &ret.expr, PRECEDENCE_ASSIGN));
            PARSER_DO(token_consume_sym(p, cur_tok, ':', "in case statement"));
            *p_expr = pool_push(&p->ast_pools[STMT_CASE], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_DEFAULT:
        {
            struct StmtCase ret = {
                .kind = STMT_CASE,
                .tok = cur_tok++,
            };
            PARSER_DO(token_consume_sym(p, cur_tok, ':', "in default statement"));
            *p_expr = pool_push(&p->ast_pools[STMT_CASE], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_SWITCH:
        {
            struct StmtSwitch ret = {
                .kind = STMT_SWITCH,
                .tok = cur_tok++,
            };
            PARSER_DO(parse_conditional(p, cur_tok, &ret.expr));
            PARSER_DO(token_consume_sym(p, cur_tok, '{', "in switch statement"));
            scope_push_subscope(&p->scope);
            PARSER_DO(parse_stmts(p, cur_tok, &ret.offset, &ret.extent));
            scope_pop_subscope(&p->scope);
            PARSER_DO(token_consume_sym(p, cur_tok, '}', "in switch statement"));
            *p_expr = pool_push(&p->ast_pools[STMT_SWITCH], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_CONTINUE:
        {
            struct StmtContinue ret = {
                .kind = STMT_CONTINUE,
                .tok = cur_tok,
            };
            *p_expr = pool_push(&p->ast_pools[STMT_CONTINUE], &ret, sizeof(ret));
            return token_consume_sym(p, cur_tok + 1, ';', " in continue statement");
        }
        case LEX_BREAK:
        {
            struct StmtBreak ret = {
                .kind = STMT_BREAK,
                .tok = cur_tok,
            };
            *p_expr = pool_push(&p->ast_pools[STMT_BREAK], &ret, sizeof(ret));
            return token_consume_sym(p, cur_tok + 1, ';', " in break statement");
        }
        case LEX_FOR:
        {
            struct StmtLoop ret = {
                .kind = STMT_LOOP,
            };
            scope_push_subscope(&p->scope);
            PARSER_DO(token_consume_sym(p, cur_tok + 1, '(', " in for statement"));
            PARSER_DO(parse_stmt(p, cur_tok, &ret.init));
            if (!token_is_sym(p, cur_tok, ';'))
            {
                PARSER_DO(parse_expr(p, cur_tok, &ret.cond, PRECEDENCE_COMMA));
            }
            PARSER_DO(token_consume_sym(p, cur_tok, ';', " in for statement"));
            if (!token_is_sym(p, cur_tok, ')'))
            {
                PARSER_DO(parse_expr(p, cur_tok, &ret.advance, PRECEDENCE_COMMA));
            }
            if (!(cur_tok = token_consume_sym(p, cur_tok, ')', " in for statement"))) return NULL;
            if (!(cur_tok = parse_stmt(p, cur_tok, &ret.body))) return NULL;
            scope_pop_subscope(&p->scope);
            *p_expr = pool_push(&p->ast_pools[STMT_LOOP], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_WHILE:
        {
            struct StmtLoop ret = {
                .kind = STMT_LOOP,
            };
            scope_push_subscope(&p->scope);
            if (!(cur_tok = parse_conditional(p, cur_tok + 1, &ret.cond))) return NULL;
            if (!(cur_tok = parse_stmt(p, cur_tok, &ret.body))) return NULL;
            scope_pop_subscope(&p->scope);
            *p_expr = pool_push(&p->ast_pools[STMT_LOOP], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_DO:
        {
            struct StmtLoop ret = {
                .kind = STMT_LOOP,
                .is_do_while = 1,
            };
            scope_push_subscope(&p->scope);
            if (!(cur_tok = parse_stmt(p, cur_tok + 1, &ret.body))) return NULL;
            scope_pop_subscope(&p->scope);
            if (cur_tok->type != LEX_WHILE)
            {
                return parser_ferror(&cur_tok->rc, "error: expected 'while'\n"), NULL;
            }
            if (!(cur_tok = parse_conditional(p, cur_tok + 1, &ret.cond))) return NULL;
            if (!(cur_tok = token_consume_sym(p, cur_tok, ';', " in do while statement"))) return NULL;
            *p_expr = pool_push(&p->ast_pools[STMT_LOOP], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_GOTO:
        {
            struct StmtGoto ret = {
                .kind = STMT_GOTO,
                .dst = ++cur_tok,
            };
            if (cur_tok->type != LEX_IDENT)
            {
                return parser_ferror(&cur_tok->rc, "error: expected label to go to\n"), NULL;
            }
            if (!(cur_tok = token_consume_sym(p, cur_tok + 1, ';', " in goto statement"))) return NULL;
            *p_expr = pool_push(&p->ast_pools[STMT_GOTO], &ret, sizeof(ret));
            return cur_tok;
        }
        case TOKEN_SYM1(';'):
        {
            struct Ast stmt = {.kind = STMT_NONE, .tok = cur_tok};
            *p_expr = pool_push(&p->ast_pools[STMT_NONE], &stmt, sizeof(stmt));
            return cur_tok + 1;
        }
        case LEX_IDENT:
        {
            if (token_is_sym(p, cur_tok + 1, ':'))
            {
                struct StmtLabel ret = {
                    .kind = STMT_LABEL,
                    .tok = cur_tok,
                };
                if (!(cur_tok = parse_stmt(p, cur_tok + 2, &ret.stmt))) return NULL;
                *p_expr = pool_push(&p->ast_pools[STMT_LABEL], &ret, sizeof(ret));
                return cur_tok;
            }

            struct Binding* const cur_bind = scope_find(&p->typedef_scope, token_str(p, cur_tok));
            if (cur_bind)
            {
                // this is a declaration
                return parse_stmt_decl(p, cur_tok, p_expr);
            }
            // fallthrough to expression statement
        }
        case LEX_NUMBER:
        case LEX_SIZEOF:
        case LEX_UUVA_START:
        case LEX_UUVA_ARG:
        case LEX_UUVA_END:
        case LEX_UUVA_COPY:
        case LEX_CHARLIT:
        case LEX_STRING:
        case TOKEN_SYM1('('):
#define Y_CASE(V, ...) case V:
            X_PREFIX_UNARY_TOKS(Y_CASE)
#undef Y_CASE
            {
                struct Expr* expr;
                PARSER_DO(parse_expr(p, cur_tok, &expr, PRECEDENCE_COMMA));
                *p_expr = &expr->ast;
                return token_consume_sym(p, cur_tok, ';', " in expression statement");
            }
        case TOKEN_SYM1('{'):
        {
            scope_push_subscope(&p->scope);
            struct StmtBlock* block;
            PARSER_DO(parse_stmt_block(p, cur_tok + 1, &block));
            *p_expr = &block->ast;
            scope_pop_subscope(&p->scope);
            return token_consume_sym(p, cur_tok, '}', " in block");
        }
        default: return parser_ferror(&cur_tok->rc, "error: expected statement\n"), NULL;
    }

fail:
    return cur_tok;
}

void parser_init(struct Parser* p) { memset(p, 0, sizeof(struct Parser)); }
void parser_destroy(struct Parser* p)
{
    autoheap_destroy(&p->strings_to_free);
    scope_destroy(&p->scope);
    scope_destroy(&p->typedef_scope);
    scope_destroy(&p->type_scope);

    for (size_t i = 0; i < AST_KIND_END_POOLS + 1; ++i)
    {
        pool_destroy(&p->ast_pools[i]);
    }
    pool_destroy(&p->sym_pool);
    pool_destroy(&p->typesym_pool);
    bsm_destroy(&p->strlit_map);
    array_destroy(&p->expr_seqs);
    array_destroy(&p->callparams);
    array_destroy(&p->designators);
}

__attribute__((unused)) static void* find_with_stride(void* arr_start, size_t arr_size, void* key, size_t stride)
{
    for (size_t i = 0; i < arr_size; i += stride)
    {
        void* elem = arr_start + i;
        if (*(void**)elem == key) return elem;
    }
    return NULL;
}

int parser_parse(struct Parser* p, const struct Token* cur_tok, const char* tk_strs)
{
    int rc = 0;
    p->tk_strdata = tk_strs;
    p->tok_data = cur_tok;

    scope_push_subscope(&p->scope);
    PARSER_DO(parse_stmt_block(p, cur_tok, &p->top));
    if (cur_tok->type != LEX_EOF)
    {
        PARSER_FAIL("error: expected EOF\n");
    }

fail:
    scope_pop_subscope(&p->scope);
    return rc;
}

void parser_debug_check(struct Parser* p)
{
    struct Expr** expr_seqs = p->expr_seqs.data;
    for (size_t i = 0; i < array_size(&p->expr_seqs, sizeof(struct Expr*)); ++i)
    {
        if ((unsigned)expr_seqs[i]->kind > AST_KIND_END) abort();
    }
}

static void nl_indent(FILE* f, int depth)
{
    fputc('\n', f);
    for (int x = 0; x < depth; ++x)
        fputc(' ', f);
}

static void parser_dump_ast(struct Parser* p, FILE* f, Ast* ast, int depth);
static void parser_dump_type_ast(struct Parser* p, FILE* f, AstType* ast, int depth)
{
    if (ast->kind != AST_DECLSPEC)
        parser_dump_ast(p, f, &ast->ast, depth);
    else
        fprintf(f, "/**/");
}
static void parser_dump_ast(struct Parser* p, FILE* f, Ast* ast, int depth)
{
    if (depth > 2000) abort();
    switch (ast->kind)
    {
        case STMT_BLOCK:
        {
            struct StmtBlock* blk = (void*)ast;
            fprintf(f, "(STMT_BLOCK");
            for (size_t i = 0; i < blk->extent; ++i)
            {
                nl_indent(f, depth);
                parser_dump_ast(p, f, ((Ast**)p->expr_seqs.data)[blk->offset + i], depth + 1);
            }
            fprintf(f, ")");
            break;
        }
        case STMT_DECLS:
        {
            struct StmtDecls* blk = (void*)ast;
            fprintf(f, "(STMT_DECLS");
            if (blk->specs)
            {
                nl_indent(f, depth);
                parser_dump_ast(p, f, &blk->specs->ast, depth + 1);
            }
            for (size_t i = 0; i < blk->extent; ++i)
            {
                nl_indent(f, depth);
                parser_dump_ast(p, f, ((Ast**)p->expr_seqs.data)[blk->offset + i], depth + 1);
            }
            fprintf(f, ")");
            break;
        }
        case STMT_LOOP:
        {
            struct StmtLoop* blk = (void*)ast;
            fprintf(f, "(STMT_LOOP");
            if (blk->init)
            {
                nl_indent(f, depth);
                fprintf(f, "init=");
                parser_dump_ast(p, f, blk->init, depth + 1);
            }
            if (blk->cond)
            {
                nl_indent(f, depth);
                fprintf(f, "cond=");
                parser_dump_ast(p, f, &blk->cond->ast, depth + 1);
            }
            if (blk->advance)
            {
                nl_indent(f, depth);
                fprintf(f, "advance=");
                parser_dump_ast(p, f, &blk->advance->ast, depth + 1);
            }
            if (blk->body)
            {
                nl_indent(f, depth);
                fprintf(f, "body=");
                parser_dump_ast(p, f, blk->body, depth + 1);
            }
            fprintf(f, ")");
            break;
        }
        case STMT_IF:
        {
            struct StmtIf* blk = (void*)ast;
            fprintf(f, "(STMT_IF");
            if (blk->cond)
            {
                nl_indent(f, depth);
                fprintf(f, "cond=");
                parser_dump_ast(p, f, &blk->cond->ast, depth + 1);
            }
            if (blk->if_body)
            {
                nl_indent(f, depth);
                fprintf(f, "if_body=");
                parser_dump_ast(p, f, blk->if_body, depth + 1);
            }
            if (blk->else_body)
            {
                nl_indent(f, depth);
                fprintf(f, "else_body=");
                parser_dump_ast(p, f, blk->else_body, depth + 1);
            }
            fprintf(f, ")");
            break;
        }
        case STMT_RETURN:
        {
            struct StmtReturn* blk = (void*)ast;
            fprintf(f, "(STMT_RETURN");
            if (blk->expr)
            {
                parser_dump_ast(p, f, &blk->expr->ast, depth + 1);
            }
            fprintf(f, ")");
            break;
        }
        case AST_DECL:
        {
            struct Decl* blk = (void*)ast;
            fprintf(f, "(AST_DECL");
            if (blk->tok)
            {
                fprintf(f, " tok='%s'", token_str(p, blk->tok));
            }
            if (blk->type)
            {
                fprintf(f, " type=");
                parser_dump_type_ast(p, f, blk->type, depth + 1);
            }
            if (blk->init)
            {
                nl_indent(f, depth);
                parser_dump_ast(p, f, blk->init, depth + 1);
            }
            fprintf(f, ")");
            break;
        }
        case AST_DECLSPEC:
        {
            DeclSpecs* blk = (void*)ast;
            fprintf(f, "(AST_DECLSPEC");
            if (blk->is_typedef) fprintf(f, " typedef");
            if (blk->tok) fprintf(f, " tok='%s'", token_str(p, blk->tok));
            if (blk->name)
            {
                fprintf(f, " name=%s", blk->name);
            }
            if (blk->suinit)
            {
                fprintf(f, " suinit=");
                parser_dump_ast(p, f, &blk->suinit->ast, depth + 1);
            }
            if (blk->enum_init)
            {
                fprintf(f, " enum_init=");
                parser_dump_ast(p, f, &blk->enum_init->ast, depth + 1);
            }
            fprintf(f, ")");
            break;
        }
        case AST_DECLFN:
        {
            DeclFn* blk = (void*)ast;
            fprintf(f, "(AST_DECLFN");
            if (blk->tok) fprintf(f, " tok='%s'", token_str(p, blk->tok));
            if (blk->is_varargs) fprintf(f, " is_varargs");
            if (blk->type)
            {
                nl_indent(f, depth);
                parser_dump_type_ast(p, f, blk->type, depth + 1);
            }
            fprintf(f, ")");
            break;
        }
        case AST_DECLARR:
        {
            struct DeclArr* blk = (void*)ast;
            fprintf(f, "(AST_DECLARR");
            if (blk->arity)
            {
                nl_indent(f, depth);
                parser_dump_ast(p, f, &blk->arity->ast, depth + 1);
            }
            nl_indent(f, depth);
            parser_dump_type_ast(p, f, blk->type, depth + 1);
            fprintf(f, ")");
            break;
        }
        case AST_DECLPTR:
        {
            struct DeclPtr* blk = (void*)ast;
            fprintf(f, "(AST_DECLPTR ");
            parser_dump_type_ast(p, f, blk->type, depth + 1);
            fprintf(f, ")");
            break;
        }
        case EXPR_BINOP:
        {
            struct ExprBinOp* blk = (void*)ast;
            fprintf(f, "(EXPR_BINOP");
            if (blk->tok) fprintf(f, " tok='%s'", token_str(p, blk->tok));
            if (blk->lhs)
            {
                nl_indent(f, depth);
                parser_dump_ast(p, f, &blk->lhs->ast, depth + 1);
            }

            if (blk->rhs)
            {
                nl_indent(f, depth);
                parser_dump_ast(p, f, &blk->rhs->ast, depth + 1);
            }
            fprintf(f, ")");
            break;
        }
        case EXPR_LIT:
        {
            struct ExprLit* blk = (void*)ast;
            fprintf(f, "(EXPR_LIT");
            if (blk->tok) fprintf(f, " tok='%s'", token_str(p, blk->tok));
            if (blk->numeric)
            {
                fprintf(f, " numeric=%llu", blk->numeric);
            }
            fprintf(f, ")");
            break;
        }
        case EXPR_UNOP:
        {
            struct ExprUnOp* blk = (void*)ast;
            fprintf(f, "(EXPR_UNOP");
            if (blk->tok) fprintf(f, " tok='%s'|%zd", token_str(p, blk->tok), blk->tok - p->tok_data);
            if (blk->postfix) fprintf(f, " postfix");
            if (blk->sizeof_) fprintf(f, " sizeof_=%u", blk->sizeof_);
            if (blk->lhs)
            {
                nl_indent(f, depth);
                parser_dump_ast(p, f, &blk->lhs->ast, depth + 1);
            }
            fprintf(f, ")");
            break;
        }
        case EXPR_FIELD:
        {
            struct ExprField* blk = (void*)ast;
            fprintf(f, "(EXPR_FIELD");
            if (blk->tok) fprintf(f, " tok='%s'|%zd", token_str(p, blk->tok), blk->tok - p->tok_data);
            if (blk->fieldname) fprintf(f, " %s", blk->fieldname);
            if (blk->lhs)
            {
                nl_indent(f, depth);
                parser_dump_ast(p, f, &blk->lhs->ast, depth + 1);
            }
            fprintf(f, ")");
            break;
        }
        case EXPR_CAST:
        {
            struct ExprCast* blk = (void*)ast;
            fprintf(f, "(EXPR_CAST");
            if (blk->type)
            {
                nl_indent(f, depth);
                parser_dump_ast(p, f, &blk->type->ast, depth + 1);
            }
            if (blk->expr)
            {
                nl_indent(f, depth);
                parser_dump_ast(p, f, &blk->expr->ast, depth + 1);
            }
            fprintf(f, ")");
            break;
        }
        case EXPR_REF:
        {
            struct ExprRef* blk = (void*)ast;
            fprintf(f, "(EXPR_REF");
            if (blk->tok) fprintf(f, " tok='%s'", token_str(p, blk->tok));
            fprintf(f, ")");
            break;
        }
        case EXPR_CALL:
        {
            struct ExprCall* blk = (void*)ast;
            fprintf(f, "(EXPR_CALL");
            if (blk->tok) fprintf(f, " tok='%s'", token_str(p, blk->tok));
            if (blk->fn)
            {
                nl_indent(f, depth);
                parser_dump_ast(p, f, &blk->fn->ast, depth + 1);
            }
            for (size_t i = 0; i < blk->param_extent; ++i)
            {
                nl_indent(f, depth);
                parser_dump_ast(p, f, &((CallParam*)p->callparams.data)[blk->param_offset + i].expr->ast, depth + 1);
            }
            fprintf(f, ")");
            break;
        }
        case EXPR_BUILTIN:
        {
            struct ExprBuiltin* blk = (void*)ast;
            fprintf(f, "(EXPR_BUILTIN");
            if (blk->tok) fprintf(f, " tok='%s'", token_str(p, blk->tok));
            if (blk->specs)
            {
                nl_indent(f, depth);
                parser_dump_ast(p, f, &blk->specs->ast, depth + 1);
            }
            if (blk->type)
            {
                nl_indent(f, depth);
                parser_dump_ast(p, f, &blk->type->ast, depth + 1);
            }
            if (blk->expr1)
            {
                nl_indent(f, depth);
                parser_dump_ast(p, f, &blk->expr1->ast, depth + 1);
            }
            if (blk->expr2)
            {
                nl_indent(f, depth);
                parser_dump_ast(p, f, &blk->expr2->ast, depth + 1);
            }
            if (blk->sizeof_size) fprintf(f, " sizeof_size=%zu", blk->sizeof_size);
            fprintf(f, ")");
            break;
        }
        default: fprintf(f, "?%d?", ast->kind); break;
    }
}

static void* dump_sym_pool(void* f, void* bucket, size_t n)
{
    Symbol* syms = bucket;
    for (size_t i = 0; i < n; ++i)
    {
        fprintf((FILE*)f, "(Symbol '%s'", syms[i].name ? syms[i].name : "");
        if (syms[i].def)
        {
            if (syms[i].def->tok) fprintf((FILE*)f, "\n  %s:%d", syms[i].def->tok->rc.file, syms[i].def->tok->rc.row);
        }
        fprintf((FILE*)f, ")\n");
    }

    return NULL;
}

void parser_dump(struct Parser* p, FILE* f)
{
    parser_dump_ast(p, f, &p->top->ast, 1);
    fputc('\n', f);
    pool_foreach_bucket(&p->sym_pool, dump_sym_pool, f);
}
