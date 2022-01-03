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
#include "symbol.h"
#include "tok.h"
#include "token.h"

typedef struct Parser Parser;

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

const char* token_str(Parser* p, const struct Token* tk) { return p->tk_strdata + tk->sp_offset; }
static int token_is_sym(Parser* p, const struct Token* tk, char sym) { return tk->type == TOKEN_SYM1(sym); }
static const struct Token* token_consume_sym(Parser* p, const struct Token* tk, char sym)
{
    if (tk->type == TOKEN_SYM1(sym))
    {
        return tk + 1;
    }
    return parser_ferror(&tk->rc, "error: expected '%c'\n", sym), NULL;
}

static int symbol_is_equivalent_redecl(struct Decl* orig, struct Decl* redecl) { return 1; }

enum Precedence op_precedence(unsigned int tok_type)
{
    switch (tok_type)
    {
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
        case TOKEN_SYM1('*'):
        case TOKEN_SYM1('/'):
        case TOKEN_SYM1('%'): return PRECEDENCE_MULT;
        default: return PRECEDENCE_ERROR;
    }
}

static int op_precedence_assoc_right(enum Precedence p) { return p == PRECEDENCE_ASSIGN; }

static struct ExprOp* parse_alloc_expr_op(Parser* p, const struct Token* tok, struct Expr* lhs, struct Expr* rhs)
{
    struct ExprOp* e = (struct ExprOp*)pool_alloc(&p->ast_pools[EXPR_OP], sizeof(struct ExprOp));
    e->kind.kind = EXPR_OP;
    e->tok = tok;
    e->lhs = lhs;
    e->rhs = rhs;
    return e;
}

const struct RowCol* decl_to_rc(const struct Decl* decl) { return &decl->id->rc; }

static struct ExprSym* parse_alloc_expr_sym(Parser* p, const struct Token* tok, struct Decl* decl)
{
    struct ExprSym* e = (struct ExprSym*)pool_alloc(&p->ast_pools[EXPR_SYM], sizeof(struct ExprSym));
    e->kind.kind = EXPR_SYM;
    e->tok = tok;
    e->decl = decl;
    return e;
}
static struct ExprLit* parse_alloc_expr_lit(Parser* p, const struct Token* tok)
{
    struct ExprLit* e = (struct ExprLit*)pool_alloc(&p->ast_pools[EXPR_LIT], sizeof(struct ExprLit));
    e->kind.kind = EXPR_LIT;
    e->tok = tok;
    e->text = token_str(p, tok);
    return e;
}
static struct ExprCast* parse_alloc_expr_cast(Parser* p, struct Decl* type, struct Expr* expr)
{
    struct ExprCast* e = (struct ExprCast*)pool_alloc(&p->ast_pools[EXPR_CAST], sizeof(struct ExprCast));
    e->kind.kind = EXPR_CAST;
    e->expr = expr;
    e->type = type;
    return e;
}
static struct ExprCall* parse_alloc_expr_call(
    Parser* p, const struct Token* tok, struct Expr* fn, size_t off, size_t ext)
{
    struct ExprCall* e = (struct ExprCall*)pool_alloc(&p->ast_pools[EXPR_CALL], sizeof(struct ExprCall));
    e->kind.kind = EXPR_CALL;
    e->tok = tok;
    e->fn = fn;
    e->offset = off;
    e->extent = ext;
    return e;
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
#define PARAM_COUNT 16
            struct Expr* arg_exprs[PARAM_COUNT];
            size_t i = 0;
            do
            {
                if (i == PARAM_COUNT)
                    return parser_ferror(
                               &cur_tok->rc, "error: too many arguments for function (%d supported)\n", PARAM_COUNT),
                           NULL;
                fprintf(stderr, "in call: %s, %u\n", lexstate_to_string(cur_tok->type), cur_tok->type);
                PARSER_DO(parse_expr(p, cur_tok, arg_exprs + i++, PRECEDENCE_ASSIGN));
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
            size_t offset = p->expr_seqs.sz / sizeof(struct Expr*);
            size_t extent = i;
            array_push(&p->expr_seqs, arg_exprs, i * sizeof(struct Expr*));
            lhs = (struct Expr*)parse_alloc_expr_call(p, tok_op, lhs, offset, extent);
        }
        goto top;
    }
    if (cur_tok->type == TOKEN_SYM1('['))
    {
        struct ExprOp* e = parse_alloc_expr_op(p, cur_tok++, lhs, NULL);
        PARSER_DO(parse_expr(p, cur_tok, &e->rhs, PRECEDENCE_COMMA));
        PARSER_DO(token_consume_sym(p, cur_tok, ']'));
        lhs = (struct Expr*)e;
        goto top;
    }

    if (cur_tok->type == TOKEN_SYM2('+', '+'))
    {
        lhs = (struct Expr*)parse_alloc_expr_op(p, cur_tok, lhs, NULL);
        ++cur_tok;
        goto top;
    }
    if (cur_tok->type == TOKEN_SYM2('-', '-'))
    {
        lhs = (struct Expr*)parse_alloc_expr_op(p, cur_tok, lhs, NULL);
        ++cur_tok;
        goto top;
    }
    if (cur_tok->type == TOKEN_SYM2('-', '>') || cur_tok->type == TOKEN_SYM1('.'))
    {
        struct ExprField* e = (struct ExprField*)pool_alloc(&p->ast_pools[EXPR_FIELD], sizeof(struct ExprField));
        e->kind.kind = EXPR_FIELD;
        e->is_arrow = cur_tok->type == TOKEN_SYM2('-', '>');
        e->tok = cur_tok++;
        if (cur_tok->type != LEX_IDENT)
        {
            PARSER_FAIL("error: expected field name\n");
        }
        e->field_tok = cur_tok;
        e->fieldname = token_str(p, cur_tok);
        e->lhs = lhs;
        e->decl = NULL;
        lhs = (struct Expr*)e;
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
        case LEX_IDENT: return NULL != scope_find(&p->type_scope, token_str(p, tok));
        default: return tok_type_is_fundamental(tok->type);
    }
}

static const struct Token* parse_type(Parser* p, const struct Token* cur_tok, struct Decl** pdecl);
static const struct Token* parse_expr_unary_atom(Parser* p, const struct Token* cur_tok, struct Expr** ppe);

static const struct Token* parse_paren_unary(Parser* p, const struct Token* cur_tok, struct Expr** ppe)
{
    struct Expr* lhs;
    if (parse_is_token_a_type(p, cur_tok))
    {
        // cast expression
        struct Decl* decl;
        struct ExprCast* e;
        PARSER_DO(parse_type(p, cur_tok, &decl));
        e = parse_alloc_expr_cast(p, decl, NULL);
        *ppe = (struct Expr*)e;
        PARSER_DO(token_consume_sym(p, cur_tok, ')'));
        return parse_expr_unary_atom(p, cur_tok, &e->expr);
    }
    PARSER_DO(parse_expr(p, cur_tok, &lhs, PRECEDENCE_COMMA));
    PARSER_DO(token_consume_sym(p, cur_tok, ')'));
    PARSER_DO(parse_expr_post_unary(p, cur_tok, lhs, ppe));
fail:
    return cur_tok;
}

static const struct Token* parse_expr_unary_atom(Parser* p, const struct Token* cur_tok, struct Expr** ppe)
{
top:
    switch (cur_tok->type)
    {
        case TOKEN_SYM1('!'):
        case TOKEN_SYM1('&'):
        case TOKEN_SYM1('*'):
        {
            struct ExprOp* e = parse_alloc_expr_op(p, cur_tok, NULL, NULL);
            *ppe = (struct Expr*)e;
            ppe = &e->lhs;
            ++cur_tok;
            goto top;
        }
        case TOKEN_SYM1('('): return parse_paren_unary(p, cur_tok + 1, ppe);
        case LEX_IDENT:
        {
            const struct Token* lhs = cur_tok++;
            const char* lhs_str = token_str(p, lhs);
            struct Binding* const lhs_bind = scope_find(&p->scope, lhs_str);
            if (!lhs_bind || !lhs_bind->sym)
            {
                return parser_ferror(&lhs->rc, "error: '%s' undeclared\n", lhs_str), NULL;
            }
            struct ExprSym* lhs_expr = parse_alloc_expr_sym(p, lhs, lhs_bind->sym);
            return parse_expr_post_unary(p, cur_tok, (struct Expr*)lhs_expr, ppe);
        }
        case LEX_NUMBER:
        case LEX_STRING:
        {
            struct ExprLit* lhs_expr = parse_alloc_expr_lit(p, cur_tok++);
            return parse_expr_post_unary(p, cur_tok, (struct Expr*)lhs_expr, ppe);
        }
        case LEX_SIZEOF:
        {
            struct ExprOp* e = parse_alloc_expr_op(p, cur_tok, NULL, NULL);
            *ppe = (struct Expr*)e;
            if (token_is_sym(p, cur_tok + 1, '('))
            {
                if (parse_is_token_a_type(p, cur_tok + 2))
                {
                    struct Decl* decl;
                    if (!(cur_tok = parse_type(p, cur_tok + 2, &decl))) return NULL;
                    e->lhs = &decl->kind;
                    if (!(cur_tok = token_consume_sym(p, cur_tok, ')'))) return NULL;
                    return parse_expr_post_unary(p, cur_tok, (struct Expr*)e, ppe);
                }
            }
            ppe = &e->lhs;
            ++cur_tok;
            goto top;
        }
        default: break;
    }
    return parser_ferror(&cur_tok->rc, "error: expected expression\n"), NULL;
}
static const struct Token* parse_expr_continue(
    Parser* p, const struct Token* cur_tok, struct Expr* lhs, struct Expr** ppe, enum Precedence precedence)
{
    const struct Token* tok_op = cur_tok;
    enum Precedence op_prec = op_precedence(tok_op->type);
    if (op_prec != PRECEDENCE_ERROR)
    {
        if (precedence <= op_prec && op_precedence_assoc_right(op_prec))
        {
            if (tok_op->type == TOKEN_SYM1('='))
            {
                struct ExprOp* op_expr = parse_alloc_expr_op(p, tok_op, lhs, NULL);
                *ppe = (struct Expr*)op_expr;
                PARSER_DO(parse_expr(p, cur_tok + 1, &op_expr->rhs, PRECEDENCE_ASSIGN));
            }
            else
            {
                *ppe = lhs;
            }
            return cur_tok;
        }
        else if (precedence < op_prec)
        {
            struct Expr* rhs;
            PARSER_DO(parse_expr(p, cur_tok + 1, &rhs, op_prec));
            struct ExprOp* op_expr = parse_alloc_expr_op(p, tok_op, lhs, rhs);
            return parse_expr_continue(p, cur_tok, (struct Expr*)op_expr, ppe, precedence);
        }
    }
    else if (tok_op->type == TOKEN_SYM1('?'))
    {
        // ternary operator
        struct ExprOp* op_expr = parse_alloc_expr_op(p, tok_op, lhs, NULL);
        *ppe = (struct Expr*)op_expr;
        struct Expr* on_true;
        PARSER_DO(parse_expr(p, cur_tok + 1, &on_true, PRECEDENCE_ASSIGN));
        struct ExprOp* switch_expr = parse_alloc_expr_op(p, cur_tok, on_true, NULL);
        op_expr->rhs = &switch_expr->kind;
        PARSER_DO(token_consume_sym(p, cur_tok, ':'));
        PARSER_DO(parse_expr(p, cur_tok, &switch_expr->rhs, PRECEDENCE_COMMA));
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
    PARSER_DO(token_consume_sym(p, cur_tok, '('));
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
    if (!(cur_tok = token_consume_sym(p, cur_tok, '('))) return NULL;
    if (!(cur_tok = token_consume_sym(p, cur_tok, '('))) return NULL;
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
    if (!(cur_tok = token_consume_sym(p, cur_tok, ')'))) return NULL;
    return token_consume_sym(p, cur_tok, ')');
}

static const struct Token* parse_declspecs(Parser* p, const struct Token* cur_tok, struct DeclSpecs** pspecs)
{
    struct DeclSpecs specs = {.kind = AST_DECLSPEC};

    do
    {
        if (cur_tok->type == LEX_ATTRIBUTE)
        {
            PARSER_DO(parse_attribute_plist(p, cur_tok + 1, &specs.attr));
            continue;
        }
        else if (cur_tok->type == LEX_DECLSPEC)
        {
            PARSER_DO(parse_msdeclspec(p, cur_tok + 1, &specs.attr));
            continue;
        }
        else if (cur_tok->type == LEX_IDENT)
        {
            // already found core type
            if (specs.tok) break;
            specs.name = token_str(p, cur_tok);
            struct Binding* const cur_bind = scope_find(&p->type_scope, specs.name);
            if (!cur_bind)
            {
                if (!specs.tok) PARSER_FAIL("error: expected type but found identifier: %s\n", specs.name);
                break;
            }
            specs.tok = cur_tok;
            specs.type = cur_bind->sym;
        }
        else if (cur_tok->type == LEX_STRUCT)
        {
            specs.is_struct = 1;
            specs.tok = cur_tok++;
            if (cur_tok->type != LEX_IDENT) continue;
            specs.tok = cur_tok;
            specs.name = token_str(p, cur_tok);
        }
        else if (cur_tok->type == LEX_ENUM)
        {
            specs.is_enum = 1;
            specs.tok = cur_tok++;
            if (cur_tok->type != LEX_IDENT) continue;
            specs.tok = cur_tok;
            specs.name = token_str(p, cur_tok);
        }
        else if (cur_tok->type == LEX_UNION)
        {
            specs.is_union = 1;
            specs.tok = cur_tok++;
            if (cur_tok->type != LEX_IDENT) continue;
            specs.tok = cur_tok;
            specs.name = token_str(p, cur_tok);
        }
        else if (tok_type_is_fundamental(cur_tok->type))
        {
            if (specs.tok)
            {
                PARSER_FAIL("error: repeated type declaration specifiers are not allowed (was '%s', got '%s')\n",
                            token_str(p, specs.tok),
                            token_str(p, cur_tok));
            }
            specs.tok = cur_tok;
        }
        else if (cur_tok->type == LEX_SIGNED || cur_tok->type == LEX_UUSIGNED || cur_tok->type == LEX_UNSIGNED ||
                 cur_tok->type == LEX_LONG || cur_tok->type == LEX_SHORT)
        {
            if (specs.tok)
            {
                PARSER_FAIL("error: repeated type declaration specifiers are not allowed (was '%s', got '%s')\n",
                            token_str(p, specs.tok),
                            token_str(p, cur_tok));
            }
            int len = 0;
            int count_signed = 0;
            int count_unsigned = 0;
            int count_long = 0;
            int count_short = 0;
            for (;; ++len)
            {
                if (cur_tok[len].type == LEX_SIGNED || cur_tok[len].type == LEX_UUSIGNED)
                    ++count_signed;
                else if (cur_tok[len].type == LEX_UNSIGNED)
                    ++count_unsigned;
                else if (cur_tok[len].type == LEX_LONG)
                    ++count_long;
                else if (cur_tok[len].type == LEX_SHORT)
                    ++count_short;
                else
                    break;
            }
            if (count_signed) specs.is_signed = 1;
            if (count_unsigned) specs.is_unsigned = 1;
            if (count_long == 1) specs.is_long = 1;
            if (count_long == 2) specs.is_longlong = 1;
            if (count_short) specs.is_short = 1;

            if (cur_tok[len].type == LEX_DOUBLE || cur_tok[len].type == LEX_CHAR || cur_tok[len].type == LEX_UUINT64 ||
                cur_tok[len].type == LEX_INT)
            {
                ++len;
            }
            if (len == 0) abort();

            specs.tok = cur_tok + len - 1;
            cur_tok += len;
            continue;
        }
        else if (cur_tok->type == TOKEN_SYM3('.', '.', '.'))
        {
            if (specs.tok)
            {
                PARSER_FAIL("error: repeated core declaration specifiers are not allowed\n");
            }
            specs.tok = cur_tok;
        }
        else if (cur_tok->type == LEX_CONST)
        {
            if (specs.is_const) PARSER_FAIL("error: repeated 'const' declaration specifiers are not allowed\n");
            specs.is_const = 1;
        }
        else if (cur_tok->type == LEX_REGISTER)
        {
            if (specs.is_register) PARSER_FAIL("error: repeated 'register' declaration specifiers are not allowed\n");
            specs.is_register = 1;
        }
        else if (cur_tok->type == LEX_EXTERN)
        {
            if (specs.is_extern) PARSER_FAIL("error: repeated 'extern' declaration specifiers are not allowed\n");
            specs.is_extern = 1;
        }
        else if (cur_tok->type == LEX_STATIC)
        {
            if (specs.is_static) PARSER_FAIL("error: repeated 'static' declaration specifiers are not allowed\n");
            specs.is_static = 1;
        }
        else if (cur_tok->type == LEX_INLINE)
        {
            if (specs.is_inline) PARSER_FAIL("error: repeated 'inline' declaration specifiers are not allowed\n");
            specs.is_inline = 1;
        }
        else if (cur_tok->type == LEX_STDCALL)
        {
            specs.is_stdcall = 1;
        }
        else if (cur_tok->type == LEX_CDECL)
        {
        }
        else if (cur_tok->type == LEX_TYPEDEF)
        {
            if (specs.is_typedef) PARSER_FAIL("error: repeated 'typedef' declaration specifiers are not allowed\n");
            specs.is_typedef = 1;
        }
        else if (cur_tok->type == LEX_VOLATILE)
        {
            if (specs.is_volatile) PARSER_FAIL("error: repeated 'volatile' declaration specifiers are not allowed\n");
            specs.is_volatile = 1;
        }
        else if (cur_tok->type == LEX_UUFORCEINLINE)
        {
            // ignore
        }
        else
        {
            if (!specs.tok)
            {
                PARSER_FAIL("error: expected type\n");
            }
            break;
        }
        ++cur_tok;
    } while (1);

    *pspecs = pool_push(&p->ast_pools[AST_DECLSPEC], &specs, sizeof(specs));

fail:
    return cur_tok;
}

static const struct Token* parse_stmt(Parser* p, const struct Token* cur_tok, struct Expr** p_expr);
static const struct Token* parse_stmts(Parser* p, const struct Token* cur_tok, struct Expr** p_expr);

static const struct Token* parse_declarator(Parser* p,
                                            const struct Token* cur_tok,
                                            struct DeclSpecs* specs,
                                            struct Decl** pdecl);

static const struct Token* parse_declarator_fnargs(Parser* p, const struct Token* cur_tok, struct DeclFn** out_declfn)
{
    struct DeclFn* fn = *out_declfn = pool_alloc(&p->ast_pools[AST_DECLFN], sizeof(struct DeclFn));
    memset(fn, 0, sizeof(struct DeclFn));
    fn->kind.kind = AST_DECLFN;
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
#define MAX_ARG_DECLS 16
        struct Decl* arg_decls[MAX_ARG_DECLS];
        while (1)
        {
            if (fn->extent == MAX_ARG_DECLS)
            {
                PARSER_FAIL("error: exceeded maximum function arguments (%d)\n", MAX_ARG_DECLS);
            }
            struct DeclSpecs* arg_specs;
            PARSER_DO(parse_declspecs(p, cur_tok, &arg_specs));
            struct Decl** arg_decl = arg_decls + fn->extent++;
            PARSER_DO(parse_declarator(p, cur_tok, arg_specs, arg_decl));
            (*arg_decl)->arg_index = fn->extent;

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
        fn->offset = p->expr_seqs.sz / sizeof(struct Expr*);
        array_push(&p->expr_seqs, arg_decls, fn->extent * sizeof(struct Expr*));
    }

fail:
    return cur_tok;
}

static const struct Token* parse_declarator_arr(Parser* p, const struct Token* cur_tok, struct DeclArr** out_declarr)
{
    struct DeclArr* arr = *out_declarr = pool_alloc(&p->ast_pools[AST_DECLARR], sizeof(struct DeclArr));
    memset(arr, 0, sizeof(struct DeclFn));
    arr->kind.kind = AST_DECLARR;
    arr->tok = cur_tok;
    ++cur_tok;
    if (cur_tok->type == TOKEN_SYM1(']'))
    {
        ++cur_tok;
    }
    else
    {
        PARSER_DO(parse_expr(p, cur_tok, &arr->arity, PRECEDENCE_COMMA));
        PARSER_DO(token_consume_sym(p, cur_tok, ']'));
    }

fail:
    return cur_tok;
}
static const struct Token* parse_declarator1(Parser* p,
                                             const struct Token* cur_tok,
                                             const struct Token** out_id,
                                             struct Expr** out_type,
                                             struct Expr*** out_p_basetype)
{
    struct Expr* base_expr = NULL;
    struct Expr** out_base_expr = NULL;
    if (cur_tok->type == LEX_CDECL) ++cur_tok;
    if (cur_tok->type == TOKEN_SYM1('*'))
    {
        struct DeclPtr* base_ptr = pool_alloc(&p->ast_pools[AST_DECLPTR], sizeof(struct DeclPtr));
        memset(base_ptr, 0, sizeof(struct DeclPtr));
        base_ptr->kind.kind = AST_DECLPTR;
        base_ptr->tok = cur_tok;
        out_base_expr = &base_ptr->type;
        ++cur_tok;
        while (1)
        {
            if (cur_tok->type == TOKEN_SYM1('*'))
            {
                struct DeclPtr* p2 = pool_alloc(&p->ast_pools[AST_DECLPTR], sizeof(struct DeclPtr));
                memset(p2, 0, sizeof(struct DeclPtr));
                p2->kind.kind = AST_DECLPTR;
                p2->tok = cur_tok;
                p2->type = &base_ptr->kind;
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
        base_expr = &base_ptr->kind;
    }
    if (cur_tok->type == TOKEN_SYM1('('))
    {
        PARSER_DO(parse_declarator1(p, cur_tok + 1, out_id, out_type, &out_type));
        PARSER_DO(token_consume_sym(p, cur_tok, ')'));
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
            struct DeclFn* declfn = NULL;
            PARSER_DO(parse_declarator_fnargs(p, cur_tok, &declfn));
            *out_type = &declfn->kind;
            out_type = &declfn->type;
            continue;
        }
        else if (cur_tok->type == TOKEN_SYM1('['))
        {
            struct DeclArr* declarr = NULL;
            PARSER_DO(parse_declarator_arr(p, cur_tok, &declarr));
            *out_type = &declarr->kind;
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

static struct Decl* parse_alloc_decl(Parser* p, struct DeclSpecs* specs)
{
    struct Decl* decl = pool_alloc(&p->ast_pools[AST_DECL], sizeof(struct Decl));
    memset(decl, 0, sizeof(struct Decl));
    decl->kind.kind = AST_DECL;
    decl->specs = specs;
    decl->parent_decl = p->fn;
    return decl;
}

static const struct Token* parse_declarator(Parser* p,
                                            const struct Token* cur_tok,
                                            struct DeclSpecs* specs,
                                            struct Decl** pdecl)
{
    struct Decl* decl = *pdecl = parse_alloc_decl(p, specs);
    struct Decl* const prev_fn = p->fn;
    p->fn = decl;
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
    struct Expr** p_basetype = NULL;
    PARSER_DO(parse_declarator1(p, cur_tok, &decl->id, &decl->type, &p_basetype));
    *p_basetype = &specs->kind;
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
    if (decl->id)
    {
        decl->name = token_str(p, decl->id);
        struct Binding* prev_sym = scope_find(&p->scope, decl->name);
        fprintf(stderr, "%s:%d: decl: %s @ %zu\n", decl->id->rc.file, decl->id->rc.row, decl->name, p->scope.binds.sz);
        scope_insert(&p->scope, decl->name, decl);
        if (prev_sym)
        {
            // ensure symbols match
            if (!symbol_is_equivalent_redecl(prev_sym->sym, decl))
            {
                PARSER_FAIL_TOK(decl->id, "error: declaration doesn't match previous\n");
            }
            if (prev_sym->sym->init)
            {
                decl->def = prev_sym->sym;
            }
            else
            {
                prev_sym->sym->def = decl;
            }
        }
    }
fail:
    p->fn = prev_fn;
    return cur_tok;
}

static const struct Token* parse_type(Parser* p, const struct Token* cur_tok, struct Decl** pdecl)
{
    const size_t scope_sz = scope_size(&p->scope);
    struct DeclSpecs* specs;
    PARSER_DO(parse_declspecs(p, cur_tok, &specs));
    PARSER_DO(parse_declarator(p, cur_tok, specs, pdecl));
    if ((*pdecl)->id)
    {
        PARSER_FAIL_TOK((*pdecl)->id, "error: type expression should not have a declared identifier\n");
    }

fail:
    scope_shrink(&p->scope, scope_sz);
    return cur_tok;
}

static const struct Token* parse_integral_constant_expr(struct Parser* p, const struct Token* cur_tok)
{
    if (cur_tok->type != LEX_NUMBER) PARSER_FAIL_TOK(cur_tok + 1, "error: expected constant integer expression\n");
    ++cur_tok;
fail:
    return cur_tok;
}

static const struct Token* parse_enum_body(struct Parser* p, const struct Token* cur_tok, struct Expr** pexpr)
{
    while (cur_tok->type == LEX_IDENT)
    {
        ++cur_tok;
        if (cur_tok->type == TOKEN_SYM1('='))
        {
            PARSER_DO_WITH(parse_integral_constant_expr(p, cur_tok + 1), "       in enum declaration\n");
        }
        if (cur_tok->type == TOKEN_SYM1(','))
        {
            ++cur_tok;
        }
        else
        {
            break;
        }
    }
fail:
    return cur_tok;
}

static const struct Token* parse_decl(Parser* p, const struct Token* cur_tok, struct Array* pdecls)
{
    struct DeclSpecs* specs;
    const struct Token* const declspec_tok = cur_tok;
    PARSER_DO(parse_declspecs(p, cur_tok, &specs));
    if (specs->is_struct || specs->is_union || specs->is_enum)
    {
        if (token_is_sym(p, cur_tok, ';') || token_is_sym(p, cur_tok, '{'))
        {
            struct Decl* decl = parse_alloc_decl(p, specs);
            decl->id = specs->tok;
            decl->name = specs->name;
            array_push_ptr(pdecls, decl);
            if (token_is_sym(p, cur_tok, '{'))
            {
                const size_t scope_sz = scope_size(&p->scope);
                if (specs->is_struct || specs->is_union)
                {
                    PARSER_DO(parse_stmts(p, cur_tok + 1, &decl->init));
                }
                else if (specs->is_enum)
                {
                    PARSER_DO(parse_enum_body(p, cur_tok + 1, &decl->init));
                }
                PARSER_DO(token_consume_sym(p, cur_tok, '}'));
                scope_shrink(&p->scope, scope_sz);
            }
            if (token_is_sym(p, cur_tok, ';'))
            {
                return cur_tok + 1;
            }
        }
    }
    while (1)
    {
        const size_t scope_sz = scope_size(&p->scope);
        struct Decl* pdecl;
        const struct Token* const declarator_tok = cur_tok;
        PARSER_DO(parse_declarator(p, cur_tok, specs, &pdecl));
        array_push_ptr(pdecls, pdecl);
        if (cur_tok->type == TOKEN_SYM1('{'))
        {
            if (!pdecl->id || !pdecl->name)
            {
                PARSER_FAIL_TOK(cur_tok, "error: anonymous function declaration not allowed\n");
            }
            p->fn = pdecl;
            PARSER_DO(parse_stmts(p, cur_tok + 1, &pdecl->init));
            PARSER_DO(token_consume_sym(p, cur_tok, '}'));
            p->fn = NULL;
            // Remove function arguments from the scope
            scope_shrink(&p->scope, scope_sz);
            scope_insert(&p->scope, pdecl->name, pdecl);
            return cur_tok;
        }

        if (cur_tok->type == TOKEN_SYM1(':'))
        {
            PARSER_DO_WITH(parse_integral_constant_expr(p, cur_tok + 1), "       in bitfield declaration\n");
        }
        else if (!pdecl->name)
        {
            PARSER_FAIL_TOK(cur_tok, "error: only bitfield members can be anonymous\n");
        }

        if (cur_tok->type == TOKEN_SYM1('='))
        {
            PARSER_DO(parse_expr(p, cur_tok + 1, &pdecl->init, PRECEDENCE_ASSIGN));
        }
        // Pop scope changes
        scope_shrink(&p->scope, scope_sz);
        if (specs->is_typedef)
        {
            if (!pdecl->name)
            {
                PARSER_FAIL_TOK(cur_tok, "error: anonymous typedef not allowed\n");
            }
            scope_insert(&p->type_scope, pdecl->name, pdecl);
        }
        else
        {
            if (pdecl->name) scope_insert(&p->scope, pdecl->name, pdecl);
        }

        if (cur_tok->type == TOKEN_SYM1(','))
        {
            ++cur_tok;
            continue;
        }
        else if (cur_tok->type == TOKEN_SYM1(';'))
        {
            return cur_tok + 1;
        }
        parser_tok_error(
            declspec_tok, "error: unterminated declaration of %zu item(s)\n", array_size(pdecls, sizeof(struct Decl*)));
        parser_tok_error(declarator_tok, "    after this declarator\n");
        parser_tok_error(cur_tok, "    expected ',' or ';' here\n");
        return NULL;
    }
fail:
    return cur_tok;
}

static const struct Token* parse_conditional(Parser* p, const struct Token* cur_tok, struct Expr** p_cond)
{
    if (!(cur_tok = token_consume_sym(p, cur_tok, '('))) return NULL;

    if (!(cur_tok = parse_expr(p, cur_tok, p_cond, PRECEDENCE_COMMA))) return NULL;

    return token_consume_sym(p, cur_tok, ')');
}

static struct Expr s_stmt_none = {.kind = STMT_NONE};

static const struct Token* parse_stmt_decl(Parser* p, const struct Token* cur_tok, struct Expr** p_expr)
{
    struct Array arr_decls;
    array_init(&arr_decls);
    PARSER_DO(parse_decl(p, cur_tok, &arr_decls));

    {
        struct StmtDecls ret = {
            .kind = STMT_DECLS,
            .offset = array_size(&p->expr_seqs, sizeof(struct Decl*)),
            .extent = array_size(&arr_decls, sizeof(struct Decl*)),
        };
        array_push(&p->expr_seqs, arr_decls.data, arr_decls.sz);
        *p_expr = pool_push(&p->ast_pools[STMT_DECLS], &ret, sizeof(ret));
    }
fail:
    array_destroy(&arr_decls);
    return cur_tok;
}

static const struct Token* parse_stmts(Parser* p, const struct Token* cur_tok, struct Expr** p_expr)
{
    struct Array arr_stmts = {};
    do
    {
        if (cur_tok->type == LEX_EOF || token_is_sym(p, cur_tok, '}')) break;
        PARSER_DO(parse_stmt(p, cur_tok, array_alloc(&arr_stmts, sizeof(struct Expr*))));
    } while (1);
    struct StmtBlock ret = {
        .kind = STMT_BLOCK,
        .offset = p->expr_seqs.sz / sizeof(struct Expr*),
        .extent = arr_stmts.sz / sizeof(struct Expr*),
    };
    array_push(&p->expr_seqs, arr_stmts.data, arr_stmts.sz);
    *p_expr = pool_push(&p->ast_pools[STMT_BLOCK], &ret, sizeof(ret));
fail:
    array_destroy(&arr_stmts);
    return cur_tok;
}

static const struct Token* parse_stmt(Parser* p, const struct Token* cur_tok, struct Expr** p_expr)
{
    if (parse_is_token_a_type(p, cur_tok))
    {
        return parse_stmt_decl(p, cur_tok, p_expr);
    }
    switch (cur_tok->type)
    {
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
                PARSER_DO(token_consume_sym(p, cur_tok, ';'));
            }
            *p_expr = pool_push(&p->ast_pools[STMT_RETURN], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_IF:
        {
            struct StmtIf ret = {
                .kind = STMT_IF,
            };
            const size_t scope_sz = scope_size(&p->scope);
            PARSER_DO(parse_conditional(p, cur_tok + 1, &ret.cond));
            PARSER_DO(parse_stmt(p, cur_tok, &ret.if_body));
            scope_shrink(&p->scope, scope_sz);

            if (cur_tok->type == LEX_ELSE)
            {
                PARSER_DO(parse_stmt(p, cur_tok + 1, &ret.else_body));
                scope_shrink(&p->scope, scope_sz);
            }
            else
            {
                ret.else_body = NULL;
            }

            *p_expr = pool_push(&p->ast_pools[STMT_IF], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_CONTINUE:
        {
            struct StmtContinue ret = {
                .kind = STMT_CONTINUE,
                .tok = cur_tok,
            };
            *p_expr = pool_push(&p->ast_pools[STMT_CONTINUE], &ret, sizeof(ret));
            return token_consume_sym(p, cur_tok + 1, ';');
        }
        case LEX_BREAK:
        {
            struct StmtBreak ret = {
                .kind = STMT_BREAK,
                .tok = cur_tok,
            };
            *p_expr = pool_push(&p->ast_pools[STMT_BREAK], &ret, sizeof(ret));
            return token_consume_sym(p, cur_tok + 1, ';');
        }
        case LEX_FOR:
        {
            struct StmtLoop ret = {
                .kind = STMT_LOOP,
            };
            size_t scope_sz = scope_size(&p->scope);
            if (!(cur_tok = token_consume_sym(p, cur_tok + 1, '('))) return NULL;
            if (!(cur_tok = parse_stmt(p, cur_tok, &ret.init))) return NULL;
            if (!token_is_sym(p, cur_tok, ';'))
            {
                if (!(cur_tok = parse_expr(p, cur_tok, &ret.cond, PRECEDENCE_COMMA))) return NULL;
            }
            if (!(cur_tok = token_consume_sym(p, cur_tok, ';'))) return NULL;
            if (!token_is_sym(p, cur_tok, ')'))
            {
                if (!(cur_tok = parse_expr(p, cur_tok, &ret.advance, PRECEDENCE_COMMA))) return NULL;
            }
            if (!(cur_tok = token_consume_sym(p, cur_tok, ')'))) return NULL;
            if (!(cur_tok = parse_stmt(p, cur_tok, &ret.body))) return NULL;
            scope_shrink(&p->scope, scope_sz);
            *p_expr = pool_push(&p->ast_pools[STMT_LOOP], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_WHILE:
        {
            struct StmtLoop ret = {
                .kind = STMT_LOOP,
            };
            if (!(cur_tok = parse_conditional(p, cur_tok + 1, &ret.cond))) return NULL;
            size_t scope_sz = scope_size(&p->scope);
            if (!(cur_tok = parse_stmt(p, cur_tok, &ret.body))) return NULL;
            scope_shrink(&p->scope, scope_sz);
            *p_expr = pool_push(&p->ast_pools[STMT_LOOP], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_DO:
        {
            struct StmtLoop ret = {
                .kind = STMT_LOOP,
                .is_do_while = 1,
            };
            size_t scope_sz = scope_size(&p->scope);
            if (!(cur_tok = parse_stmt(p, cur_tok + 1, &ret.body))) return NULL;
            scope_shrink(&p->scope, scope_sz);
            if (cur_tok->type != LEX_WHILE)
            {
                return parser_ferror(&cur_tok->rc, "error: expected 'while'\n"), NULL;
            }
            if (!(cur_tok = parse_conditional(p, cur_tok + 1, &ret.cond))) return NULL;
            if (!(cur_tok = token_consume_sym(p, cur_tok, ';'))) return NULL;
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
            if (!(cur_tok = token_consume_sym(p, cur_tok + 1, ';'))) return NULL;
            *p_expr = pool_push(&p->ast_pools[STMT_GOTO], &ret, sizeof(ret));
            return cur_tok;
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

            struct Binding* const cur_bind = scope_find(&p->type_scope, token_str(p, cur_tok));
            if (cur_bind)
            {
                // this is a declaration
                return parse_stmt_decl(p, cur_tok, p_expr);
            }
        }
        case LEX_NUMBER:
        {
            if (!(cur_tok = parse_expr(p, cur_tok, p_expr, PRECEDENCE_COMMA))) return NULL;
            return token_consume_sym(p, cur_tok, ';');
        }
        case LEX_SYMBOL:
        {
            const char ch = token_str(p, cur_tok)[0];
            if (ch == ';')
            {
                *p_expr = &s_stmt_none;
                return cur_tok + 1;
            }
            else if (ch == '*' || ch == '(' || ch == '&')
            {
                if (!(cur_tok = parse_expr(p, cur_tok, p_expr, PRECEDENCE_COMMA))) return NULL;
                return token_consume_sym(p, cur_tok, ';');
            }
            else if (ch == '{')
            {
                size_t scope_sz = scope_size(&p->scope);
                if (!(cur_tok = parse_stmts(p, cur_tok + 1, p_expr))) return NULL;
                scope_shrink(&p->scope, scope_sz);
                return token_consume_sym(p, cur_tok, '}');
            }
            else
            {
                return parser_ferror(&cur_tok->rc, "error: expected statement\n"), NULL;
            }
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
    scope_destroy(&p->type_scope);

    for (size_t i = 0; i < AST_KIND_END_POOLS; ++i)
    {
        pool_destroy(&p->ast_pools[i]);
    }
    array_destroy(&p->expr_seqs);
    array_destroy(&p->arr_exprs);
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
    p->tk_strdata = tk_strs;

    do
    {
        if (cur_tok->type == LEX_EOF) break;
        struct Expr* e;
        if (!(cur_tok = parse_stmt_decl(p, cur_tok, &e))) return 1;
        array_push(&p->arr_exprs, &e, sizeof(e));
    } while (1);
    scope_shrink(&p->scope, 0);

    return 0;
}
