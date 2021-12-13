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

static struct RowCol s_unknown_rc = {
    .file = "<unknown>",
    .row = 1,
    .col = 1,
};

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

#define PARSER_DO(...)                                                                                                 \
    do                                                                                                                 \
    {                                                                                                                  \
        cur_tok = __VA_ARGS__;                                                                                         \
        if (cur_tok == NULL) goto fail;                                                                                \
    } while (0)

char* token_str(Parser* p, const struct Token* tk) { return (char*)p->stringpool.data + tk->sp_offset; }
static int token_is_sym(Parser* p, const struct Token* tk, char sym)
{
    return tk->type == LEX_SYMBOL && token_str(p, tk)[0] == sym;
}
static char token_expect_comma_or_cparen(Parser* p, const struct Token* tk)
{
    if (tk->type == LEX_SYMBOL)
    {
        const char ch = token_str(p, tk)[0];
        if (ch == ')' || ch == ',') return ch;
    }
    return parser_ferror(&tk->rc, "error: expected ',' or ')'\n"), '\0';
}
static struct Token* token_consume_sym(Parser* p, struct Token* tk, char sym)
{
    if (token_is_sym(p, tk, sym))
    {
        return tk + 1;
    }
    return parser_ferror(&tk->rc, "error: expected '%c'\n", sym), NULL;
}

static void symbol_init(struct Symbol* ret)
{
    memset(ret, 0, sizeof(struct Symbol));
    ret->kind.kind = AST_SYM;
}
static int symbol_is_equivalent_redecl(struct Symbol* orig, struct Symbol* redecl) { return 1; }

enum Precedence op_precedence(const char* op)
{
    switch (op[0])
    {
        case '=':
            if (op[1] == '=')
                return PRECEDENCE_EQUALITY;
            else
                return PRECEDENCE_ASSIGN;
        case '!':
            if (op[1] == '=')
                return PRECEDENCE_EQUALITY;
            else
                return PRECEDENCE_ERROR;
        case '|':
            if (op[1] == '|')
                return PRECEDENCE_OR;
            else
                return PRECEDENCE_ERROR;
        case '&':
            if (op[1] == '&')
                return PRECEDENCE_AND;
            else
                return PRECEDENCE_ERROR;
        case '>':
        case '<': return PRECEDENCE_RELATION;
        case '+':
            if (op[1] == '+') return PRECEDENCE_ERROR;
        case '-':
            return PRECEDENCE_ADD;
            if (op[1] == '-') return PRECEDENCE_ERROR;
        case '*':
        case '%':
        case '/': return PRECEDENCE_MULT;
        default: return PRECEDENCE_ERROR;
    }
}

static int op_precedence_assoc_right(enum Precedence p) { return p == PRECEDENCE_ASSIGN; }

static struct ExprOp* parse_alloc_expr_op(Parser* p, struct Token* tok, struct Expr* lhs, struct Expr* rhs)
{
    struct ExprOp* e = (struct ExprOp*)pool_alloc(&p->ast_pools[EXPR_OP], sizeof(struct ExprOp));
    e->kind.kind = EXPR_OP;
    e->tok = tok;
    e->lhs = lhs;
    e->rhs = rhs;
    return e;
}

struct RowCol* sym_to_rc(struct Symbol* sym) { return sym->decl ? &sym->decl->id->rc : &s_unknown_rc; }

static struct ExprSym* parse_alloc_expr_sym(Parser* p, struct Token* tok, struct Symbol* sym)
{
    struct ExprSym* e = (struct ExprSym*)pool_alloc(&p->ast_pools[EXPR_SYM], sizeof(struct ExprSym));
    e->kind.kind = EXPR_SYM;
    e->tok = tok;
    e->sym = sym;
    return e;
}
static struct ExprLit* parse_alloc_expr_lit(Parser* p, struct Token* tok)
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
static struct ExprCall* parse_alloc_expr_call(Parser* p, struct Token* tok, struct Expr* fn, size_t off, size_t ext)
{
    struct ExprCall* e = (struct ExprCall*)pool_alloc(&p->ast_pools[EXPR_CALL], sizeof(struct ExprCall));
    e->kind.kind = EXPR_CALL;
    e->tok = tok;
    e->fn = fn;
    e->offset = off;
    e->extent = ext;
    return e;
}

static struct Token* parse_expr(Parser* p, struct Token* cur_tok, struct Expr** ppe, int precedence);

static struct Token* parse_expr_post_unary(Parser* p, struct Token* cur_tok, struct Expr* lhs, struct Expr** ppe)
{
    if (cur_tok->type == LEX_SYMBOL)
    {
        struct Token* tok_op = cur_tok;
        char* op = token_str(p, tok_op);

        if (op[0] == '(')
        {
            struct Token* tk = ++cur_tok;
            struct ExprCall* op_expr;
            if (tk->type == LEX_SYMBOL && token_str(p, tk)[0] == ')')
            {
                ++cur_tok;
                op_expr = parse_alloc_expr_call(p, tok_op, lhs, 0, 0);
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
                                   &cur_tok->rc, "error: too many arguments for function (%d supported)", PARAM_COUNT),
                               NULL;
                    if (!(cur_tok = parse_expr(p, cur_tok, arg_exprs + i++, PRECEDENCE_ASSIGN))) return NULL;
                    char ch = token_expect_comma_or_cparen(p, cur_tok++);
                    if (ch == ',')
                        continue;
                    else if (ch == ')')
                        break;
                    else
                        return NULL;
                } while (1);
                size_t offset = p->expr_seqs.sz / sizeof(struct Expr*);
                size_t extent = i;
                array_push(&p->expr_seqs, arg_exprs, i * sizeof(struct Expr*));
                op_expr = parse_alloc_expr_call(p, tok_op, lhs, offset, extent);
            }
            return parse_expr_post_unary(p, cur_tok, (struct Expr*)op_expr, ppe);
        }
        else if (op[0] == '[')
        {
            struct ExprOp* e = parse_alloc_expr_op(p, cur_tok++, lhs, NULL);
            if (!(cur_tok = parse_expr(p, cur_tok, &e->rhs, PRECEDENCE_COMMA))) return NULL;
            if (!(cur_tok = token_consume_sym(p, cur_tok, ']'))) return NULL;
            return parse_expr_post_unary(p, cur_tok, (struct Expr*)e, ppe);
        }
        else if (op[0] == '+' && op[1] == '+')
        {
            struct ExprOp* e = parse_alloc_expr_op(p, cur_tok, lhs, NULL);
            return parse_expr_post_unary(p, cur_tok + 1, (struct Expr*)e, ppe);
        }
        else if (op[0] == '-' && op[1] == '-')
        {
            struct ExprOp* e = parse_alloc_expr_op(p, cur_tok, lhs, NULL);
            return parse_expr_post_unary(p, cur_tok + 1, (struct Expr*)e, ppe);
        }
    }
    *ppe = (struct Expr*)lhs;
    return cur_tok;
}
static int parse_is_token_a_type(Parser* p, struct Token* tok)
{
    switch (tok->type)
    {
        case LEX_MSTRING:
        case LEX_INT:
        case LEX_UNIT:
        case LEX_VOLATILE:
        case LEX_CONST:
        case LEX_STRUCT:
        case LEX_VOID: return 1;
        default: return 0;
    }
}

static struct Token* parse_type(Parser* p, struct Token* cur_tok, struct Decl** pdecl);
static struct Token* parse_expr_unary_atom(Parser* p, struct Token* cur_tok, struct Expr** ppe)
{
    switch (cur_tok->type)
    {
        case LEX_SYMBOL:
        {
            char* op = token_str(p, cur_tok);
            if ((op[0] == '!' || op[0] == '&' || op[0] == '*') && op[1] == '\0')
            {
                struct ExprOp* e = parse_alloc_expr_op(p, cur_tok, NULL, NULL);
                *ppe = (struct Expr*)e;
                return parse_expr_unary_atom(p, cur_tok + 1, &e->lhs);
            }
            else if (op[0] == '(' && op[1] == '\0')
            {
                if (parse_is_token_a_type(p, cur_tok + 1))
                {
                    // cast expression
                    struct Decl* decl;
                    if (!(cur_tok = parse_type(p, cur_tok + 1, &decl))) return NULL;
                    struct ExprCast* e = parse_alloc_expr_cast(p, decl, NULL);
                    *ppe = (struct Expr*)e;
                    if (!(cur_tok = token_consume_sym(p, cur_tok, ')'))) return NULL;
                    return parse_expr_unary_atom(p, cur_tok, &e->expr);
                }
                else if (!(cur_tok = parse_expr(p, cur_tok + 1, ppe, PRECEDENCE_COMMA)))
                {
                    return NULL;
                }
                return token_consume_sym(p, cur_tok, ')');
            }
            break;
        }
        case LEX_IDENT:
        {
            struct Token* lhs = cur_tok++;
            const char* lhs_str = token_str(p, lhs);
            struct Binding* const lhs_bind = scope_find(&p->scope, lhs_str);
            if (!lhs_bind)
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
        default: break;
    }
    return parser_ferror(&cur_tok->rc, "error: expected expression\n"), NULL;
}
static struct Token* parse_expr_continue(
    Parser* p, struct Token* cur_tok, struct Expr* lhs, struct Expr** ppe, enum Precedence precedence)
{
    if (cur_tok->type == LEX_SYMBOL)
    {
        struct Token* tok_op = cur_tok;
        char* op = token_str(p, tok_op);
        enum Precedence op_prec = op_precedence(op);
        if (op_prec)
        {
            if (precedence <= op_prec && op_precedence_assoc_right(op_prec))
            {
                if (op[0] == '=' && op[1] == '\0' && precedence <= PRECEDENCE_ASSIGN)
                {
                    struct ExprOp* op_expr = parse_alloc_expr_op(p, tok_op, lhs, NULL);
                    *ppe = (struct Expr*)op_expr;
                    if (!(cur_tok = parse_expr(p, cur_tok + 1, &op_expr->rhs, PRECEDENCE_ASSIGN))) return NULL;
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
                if (!(cur_tok = parse_expr(p, cur_tok + 1, &rhs, op_prec))) return NULL;
                struct ExprOp* op_expr = parse_alloc_expr_op(p, tok_op, lhs, rhs);
                return parse_expr_continue(p, cur_tok, (struct Expr*)op_expr, ppe, precedence);
            }
        }
    }
    *ppe = lhs;
    return cur_tok;
}
static struct Token* parse_expr(Parser* p, struct Token* cur_tok, struct Expr** ppe, int precedence)
{
    struct Expr* lhs;
    if (!(cur_tok = parse_expr_unary_atom(p, cur_tok, &lhs))) return NULL;

    return parse_expr_continue(p, cur_tok, lhs, ppe, precedence);
}

enum TypeKind
{
    TYPE_UNKNOWN,
    TYPE_SYM,
    TYPE_INT,
    TYPE_VOID,
    TYPE_CHAR,
    TYPE_MSTRING,
    TYPE_FUN,
    TYPE_PTR,
    TYPE_ARR,
};

enum ValueCategory
{
    CAT_UNKNOWN,
    CAT_LVALUE,
    CAT_XVALUE,
    CAT_PRVALUE,
};

enum AttrKind
{
    ATTR_SYM,
    ATTR_ASM,
    ATTR_NONREENTRANT,
};

static struct Token* parse_attribute(Parser* p, struct Token* cur_tok, struct Attribute* attr)
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
static struct Token* parse_attribute_plist(Parser* p, struct Token* cur_tok, struct Attribute* attr)
{
    if (!(cur_tok = token_consume_sym(p, cur_tok, '('))) return NULL;
    if (!(cur_tok = token_consume_sym(p, cur_tok, '('))) return NULL;
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
    if (!(cur_tok = token_consume_sym(p, cur_tok, ')'))) return NULL;
    return token_consume_sym(p, cur_tok, ')');
}

static struct Token* parse_declspecs(Parser* p, struct Token* cur_tok, struct DeclSpecs* specs)
{
    do
    {
        if (cur_tok->type == LEX_IDENT)
        {
            struct Binding* const cur_bind = scope_find(&p->type_scope, token_str(p, cur_tok));
            if (!cur_bind)
            {
                if (!specs->type) return parser_ferror(&cur_tok->rc, "error: expected type before identifier\n"), NULL;
                return cur_tok;
            }
        }
        else if (cur_tok->type == LEX_STRUCT)
        {
            specs->type = cur_tok++;
            if (cur_tok->type != LEX_IDENT) return parser_ferror(&cur_tok->rc, "error: expected typename\n"), NULL;
        }
        else if (cur_tok->type == LEX_VOID || cur_tok->type == LEX_INT || cur_tok->type == LEX_MSTRING ||
                 cur_tok->type == LEX_UNIT)
        {
            if (specs->type)
                return parser_ferror(&cur_tok->rc, "error: repeated core declaration specifiers are not allowed\n"),
                       NULL;
            specs->type = cur_tok;
        }
        else if (cur_tok->type == LEX_CONST)
        {
            if (specs->is_const)
            {
                return parser_ferror(&cur_tok->rc, "error: repeated 'const' declaration specifiers are not allowed\n"),
                       NULL;
            }
            specs->is_const = 1;
        }
        else if (cur_tok->type == LEX_REGISTER)
        {
            if (specs->is_register)
            {
                return parser_ferror(&cur_tok->rc,
                                     "error: repeated 'register' declaration specifiers are not allowed\n"),
                       NULL;
            }
            specs->is_register = 1;
        }
        else if (cur_tok->type == LEX_EXTERN)
        {
            if (specs->is_extern)
            {
                return parser_ferror(&cur_tok->rc, "error: repeated 'extern' declaration specifiers are not allowed\n"),
                       NULL;
            }
            specs->is_extern = 1;
        }
        else if (cur_tok->type == LEX_STDCALL)
        {
            specs->is_stdcall = 1;
        }
        else if (cur_tok->type == LEX_VOLATILE)
        {
            if (specs->is_volatile)
            {
                return parser_ferror(&cur_tok->rc,
                                     "error: repeated 'volatile' declaration specifiers are not allowed\n"),
                       NULL;
            }
            specs->is_volatile = 1;
        }
        else
        {
            if (!specs->type) return parser_ferror(&cur_tok->rc, "error: expected type\n"), NULL;
            return cur_tok;
        }
        ++cur_tok;
    } while (1);
}

enum ExpectIdentifier
{
    EXPECT_IDENTIFIER,
    EXPECT_NO_IDENTIFIER,
    EXPECT_ANY,
};

static struct Token* parse_declarator(Parser* p,
                                      struct Token* cur_tok,
                                      struct DeclSpecs* specs,
                                      struct Decl** pdecl,
                                      enum ExpectIdentifier expect_identifier);

static struct Token* parse_declarator1(Parser* p,
                                       struct Token* cur_tok,
                                       struct Decl* decl,
                                       enum ExpectIdentifier expect_identifier);

static struct Token* parse_declarator_fnargs(Parser* p,
                                             struct Token* cur_tok,
                                             struct Decl* decl,
                                             struct Binding* prev_sym)
{
    struct Decl* const prev_fn = p->fn;
    if (decl->is_array) PARSER_FAIL("error: arrays of functions are not supported\n");
    if (decl->pointer_levels) PARSER_FAIL("error: pointers to functions are not supported\n");
    p->fn = decl;
    decl->is_function = 1;
    ++cur_tok;
    typestr_start_call(&decl->sym.type);
    if (token_is_sym(p, cur_tok, ')'))
    {
        typestr_end_call(&decl->sym.type);
        ++cur_tok;
    }
    else
    {
#define MAX_ARG_DECLS 16
        struct Decl* arg_decls[MAX_ARG_DECLS];
        while (1)
        {
            if (decl->extent == MAX_ARG_DECLS)
            {
                PARSER_FAIL("error: exceeded maximum function arguments (%d)\n", MAX_ARG_DECLS);
            }
            struct DeclSpecs arg_specs = {0};
            PARSER_DO(parse_declspecs(p, cur_tok, &arg_specs));
            struct Decl** p_arg_decl = arg_decls + decl->extent++;
            PARSER_DO(parse_declarator(p, cur_tok, &arg_specs, p_arg_decl, EXPECT_ANY));
            struct Decl* arg_decl = *p_arg_decl;
            arg_decl->is_argument = 1;
            arg_decl->arg_index = decl->extent - 1;
            typestr_add_arg(&decl->sym.type, &arg_decl->sym.type);

            if (cur_tok->type == LEX_SYMBOL)
            {
                const char ch = token_str(p, cur_tok)[0];
                if (ch == ',')
                {
                    ++cur_tok;
                    continue;
                }
                else if (ch == ')')
                {
                    typestr_end_call(&decl->sym.type);
                    ++cur_tok;
                    break;
                }
            }
            PARSER_FAIL("error: expected ',' and further parameter declarations or ')'\n");
        }
        decl->offset = p->expr_seqs.sz / sizeof(struct Expr*);
        array_push(&p->expr_seqs, arg_decls, decl->extent * sizeof(struct Expr*));
    }
    if (prev_sym)
    {
        // ensure symbols match
        if (!symbol_is_equivalent_redecl(prev_sym->sym, &decl->sym))
        {
            PARSER_FAIL_TOK(decl->id, "error: declaration doesn't match previous\n");
        }
        if (prev_sym->sym->decl->init)
        {
            decl->def = prev_sym->sym->decl;
        }
        else
        {
            prev_sym->sym->decl->def = decl;
        }
    }

fail:
    p->fn = prev_fn;
    return cur_tok;
}

static struct Token* parse_declarator_inner(Parser* p,
                                            struct Token* cur_tok,
                                            struct Decl* decl,
                                            enum ExpectIdentifier expect_identifier)
{
    struct Binding* prev_sym = NULL;
    if (token_is_sym(p, cur_tok, '('))
    {
        PARSER_DO(parse_declarator1(p, cur_tok, decl, expect_identifier));
        PARSER_DO(token_consume_sym(p, cur_tok, ')'));
    }
    else if (expect_identifier != EXPECT_NO_IDENTIFIER)
    {
        if (cur_tok->type == LEX_IDENT)
        {
            decl->id = cur_tok++;
            decl->name = token_str(p, decl->id);
            prev_sym = scope_find(&p->scope, decl->name);
            scope_insert(&p->scope, decl->name, &decl->sym);
        }
        else if (expect_identifier == EXPECT_IDENTIFIER)
        {
            PARSER_FAIL("error: expected identifier\n");
        }
    }

    if (token_is_sym(p, cur_tok, '('))
    {
        PARSER_DO(parse_declarator_fnargs(p, cur_tok, decl, prev_sym));
    }
    else if (token_is_sym(p, cur_tok, '['))
    {
        decl->specs.is_const = 0;
        decl->specs.is_volatile = 0;
        ++cur_tok;
        if (token_is_sym(p, cur_tok, ']'))
        {
            if (decl->is_array) PARSER_FAIL("error: arrays of arrays are not supported\n");
            decl->is_array = 1;
            decl->array_arity = ARRAY_ARITY_NONE;
            ++cur_tok;
        }
        else if (cur_tok->type == LEX_NUMBER)
        {
            decl->array_arity = atoi(token_str(p, cur_tok));
            if (decl->array_arity < 1) PARSER_FAIL("error: array arity must be positive\n");
            PARSER_DO(token_consume_sym(p, cur_tok + 1, ']'));
        }
        else
        {
            PARSER_FAIL("error: expected number or ']'\n");
        }
        typestr_add_arr(&decl->sym.type, decl->array_arity);
    }
fail:
    return cur_tok;
}
static struct Token* parse_declarator1(Parser* p,
                                       struct Token* cur_tok,
                                       struct Decl* decl,
                                       enum ExpectIdentifier expect_identifier)
{
    if (cur_tok->type == LEX_ATTRIBUTE)
    {
        ++cur_tok;
        cur_tok = parse_attribute_plist(p, cur_tok, &decl->attr);
        if (!cur_tok) return NULL;
    }
    while (1)
    {
        if (token_is_sym(p, cur_tok, '*'))
        {
            typestr_add_pointer(&decl->sym.type);
            decl->specs.is_const = 0;
            decl->specs.is_volatile = 0;
            ++cur_tok;
        }
        else if (cur_tok->type == LEX_CONST)
        {
            typestr_add_const(&decl->sym.type);
            decl->specs.is_const = 1;
            ++cur_tok;
        }
        else
            break;
    }
    return parse_declarator_inner(p, cur_tok, decl, expect_identifier);
}

static struct TypeStr declspecs_to_type(struct Parser* p, struct DeclSpecs* specs)
{
    struct TypeStr ty;
    if (specs->type->type == LEX_INT)
    {
        ty = s_type_int;
    }
    else if (specs->type->type == LEX_VOID)
    {
        ty = s_type_void;
    }
    else if (specs->type->type == LEX_MSTRING)
    {
        ty = s_type_mstr;
    }
    else if (specs->type->type == LEX_UNIT)
    {
        ty = s_type_unit;
    }
    else if (specs->type->type == LEX_STRUCT)
    {
        const char* str = token_str(p, specs->type + 1);
        size_t len = strlen(str);
        if (len + 2 > sizeof(ty.buf))
        {
            parser_ferror(&specs->type[1].rc, "error: struct name too long\n");
            return s_type_unknown;
        }
        ty.used = len + 2;
        ty.buf[0] = '$';
        memcpy(ty.buf + 1, str, len);
        ty.buf[len + 1] = '$';
    }
    else
    {
        parser_ferror(&specs->type->rc, "error: unknown type\n");
        return s_type_unknown;
    }
    if (specs->is_const) typestr_add_const(&ty);
    return ty;
}

static struct Token* parse_declarator(Parser* p,
                                      struct Token* cur_tok,
                                      struct DeclSpecs* specs,
                                      struct Decl** pdecl,
                                      enum ExpectIdentifier expect_identifier)
{
    struct Decl* decl = *pdecl = pool_alloc(&p->ast_pools[AST_DECL], sizeof(struct Decl));
    memset(decl, 0, sizeof(struct Decl));
    decl->kind.kind = AST_DECL;
    decl->specs = *specs;
    symbol_init(&decl->sym);
    decl->sym.decl = decl;
    decl->sym.type = declspecs_to_type(p, specs);
    decl->parent_decl = p->fn;

    return parse_declarator1(p, cur_tok, decl, expect_identifier);
}

static struct Token* parse_type(Parser* p, struct Token* cur_tok, struct Decl** pdecl)
{
    const size_t scope_sz = scope_size(&p->scope);
    struct DeclSpecs specs = {0};
    if (!(cur_tok = parse_declspecs(p, cur_tok, &specs))) return NULL;
    if (!(cur_tok = parse_declarator(p, cur_tok, &specs, pdecl, EXPECT_NO_IDENTIFIER))) return NULL;
    scope_shrink(&p->scope, scope_sz);
    return cur_tok;
}

static struct Token* parse_decl(Parser* p, struct Token* cur_tok, struct Array* pdecls)
{
    struct DeclSpecs specs = {0};
    if (!(cur_tok = parse_declspecs(p, cur_tok, &specs))) return NULL;
    while (1)
    {
        const size_t scope_sz = scope_size(&p->scope);
        struct Decl* pdecl;
        if (!(cur_tok = parse_declarator(p, cur_tok, &specs, &pdecl, EXPECT_IDENTIFIER))) return NULL;
        array_push(pdecls, &pdecl, sizeof(pdecl));
        if (cur_tok->type == LEX_SYMBOL)
        {
            const char ch = token_str(p, cur_tok)[0];
            if (ch == '{')
            {
                return cur_tok;
            }

            if (ch == '=')
            {
                if (specs.is_static)
                {
                    return parser_ferror(&cur_tok->rc, "error: static initializers are not currently supported\n"),
                           NULL;
                }
                if (pdecl->is_function)
                    return parser_ferror(&cur_tok->rc, "error: function cannot be initialized with '='\n"), NULL;
                if (!(cur_tok = parse_expr(p, cur_tok + 1, &pdecl->init, PRECEDENCE_ASSIGN))) return NULL;
            }
            // Remove all but first declarator from the scope
            scope_shrink(&p->scope, scope_sz + 1);

            if (cur_tok->type != LEX_SYMBOL) return parser_ferror(&cur_tok->rc, "error: expected ',' or ';'\n"), NULL;
            const char ch2 = *token_str(p, cur_tok);
            if (ch2 == ',')
            {
                ++cur_tok;
                continue;
            }
            else if (ch2 == ';')
            {
                return cur_tok;
            }
        }
        return parser_ferror(&cur_tok->rc, "error: expected ',' or ';'\n"), NULL;
    }
}

static struct Token* parse_conditional(Parser* p, struct Token* cur_tok, struct Expr** p_cond)
{
    if (!(cur_tok = token_consume_sym(p, cur_tok, '('))) return NULL;

    if (!(cur_tok = parse_expr(p, cur_tok, p_cond, PRECEDENCE_COMMA))) return NULL;

    return token_consume_sym(p, cur_tok, ')');
}

static struct Expr s_stmt_none = {.kind = STMT_NONE};

static struct Token* parse_stmt(Parser* p, struct Token* cur_tok, struct Expr** p_expr);
static struct Token* parse_stmts(Parser* p, struct Token* cur_tok, struct Expr** p_expr);
static struct Token* parse_stmt_decl(Parser* p, struct Token* cur_tok, struct Expr** p_expr)
{
    const size_t scope_sz = scope_size(&p->scope);
    struct Array arr_decls;
    array_init(&arr_decls);
    PARSER_DO(parse_decl(p, cur_tok, &arr_decls));

    if (token_is_sym(p, cur_tok, '{'))
    {
        if (p->fn)
        {
            PARSER_FAIL("error: cannot define function inside another function\n");
        }
        if (arr_decls.sz != sizeof(struct Decl*))
        {
            PARSER_FAIL("error: functions must be declared alone\n");
        }
        struct Decl* decl = *(struct Decl**)arr_decls.data;
        if (!decl->is_function)
        {
            PARSER_FAIL("error: only functions may be initialized with a code block\n");
        }
        p->fn = decl;
        PARSER_DO(parse_stmts(p, cur_tok + 1, &decl->init));
        PARSER_DO(token_consume_sym(p, cur_tok, '}'));
        // Remove function arguments from the scope
        p->fn = NULL;
        scope_shrink(&p->scope, scope_sz + 1);
    }
    else
    {
        PARSER_DO(token_consume_sym(p, cur_tok, ';'));
    }

    struct StmtDecls ret = {
        .kind = STMT_DECLS,
        .offset = p->expr_seqs.sz / sizeof(struct Decl*),
        .extent = arr_decls.sz / sizeof(struct Decl*),
    };
    array_push(&p->expr_seqs, arr_decls.data, arr_decls.sz);
    *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
fail:
    array_destroy(&arr_decls);
    return cur_tok;
}

static struct Token* parse_stmts(Parser* p, struct Token* cur_tok, struct Expr** p_expr)
{
    struct Array arr_stmts;
    array_init(&arr_stmts);
    do
    {
        if (cur_tok->type == LEX_EOF || token_is_sym(p, cur_tok, '}')) break;
        if (!(cur_tok = parse_stmt(p, cur_tok, array_alloc(&arr_stmts, sizeof(struct Expr*))))) return NULL;
    } while (1);
    struct StmtBlock ret = {
        .kind = STMT_BLOCK,
        .offset = p->expr_seqs.sz / sizeof(struct Expr*),
        .extent = arr_stmts.sz / sizeof(struct Expr*),
    };
    array_push(&p->expr_seqs, arr_stmts.data, arr_stmts.sz);
    array_destroy(&arr_stmts);
    *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
    return cur_tok;
}

static struct Token* parse_stmt(Parser* p, struct Token* cur_tok, struct Expr** p_expr)
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
                .tok = cur_tok,
            };
            if (token_is_sym(p, cur_tok + 1, ';'))
            {
                cur_tok += 2;
            }
            else if (!(cur_tok = parse_expr(p, cur_tok + 1, &ret.expr, PRECEDENCE_COMMA)))
                return NULL;
            *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_IF:
        {
            struct StmtIf ret = {
                .kind = STMT_IF,
            };
            size_t scope_sz = scope_size(&p->scope);
            if (!(cur_tok = parse_conditional(p, cur_tok + 1, &ret.cond))) return NULL;
            if (!(cur_tok = parse_stmt(p, cur_tok, &ret.if_body))) return NULL;
            scope_shrink(&p->scope, scope_sz);

            if (cur_tok->type == LEX_ELSE)
            {
                if (!(cur_tok = parse_stmt(p, cur_tok + 1, &ret.else_body))) return NULL;
                scope_shrink(&p->scope, scope_sz);
            }
            else
            {
                ret.else_body = NULL;
            }

            *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
            return cur_tok;
        }
        case LEX_CONTINUE:
        {
            struct StmtContinue ret = {
                .kind = STMT_CONTINUE,
                .tok = cur_tok,
            };
            *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
            return token_consume_sym(p, cur_tok + 1, ';');
        }
        case LEX_BREAK:
        {
            struct StmtBreak ret = {
                .kind = STMT_BREAK,
                .tok = cur_tok,
            };
            *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
            return token_consume_sym(p, cur_tok + 1, ';');
        }
        case LEX_FOR:
        {
            struct StmtLoop ret = {
                .kind = STMT_LOOP,
            };
            size_t scope_sz = scope_size(&p->scope);
            if (!(cur_tok = token_consume_sym(p, cur_tok + 1, '('))) return NULL;
            struct Token* init_tk = cur_tok;
            if (!(cur_tok = parse_stmt(p, cur_tok, &ret.init))) return NULL;
            if (ret.init->kind == STMT_DECLS)
            {
                struct StmtDecls* init = (struct StmtDecls*)ret.init;
                struct Expr** exprs = p->expr_seqs.data;
                for (size_t i = 0; i < init->extent; ++i)
                {
                    if (exprs[init->offset + i]->kind != AST_DECL) abort();
                    struct Decl* decl = (struct Decl*)exprs[init->offset + i];
                    if (decl->is_function)
                        return parser_ferror(
                                   &decl->id->rc,
                                   "error: declaration of non-variable '%s' in 'for' loop initial declaration\n",
                                   token_str(p, decl->id)),
                               NULL;
                }
            }
            else if (!ast_kind_is_expr(ret.init->kind))
            {
                return parser_ferror(&init_tk->rc, "error: expected expression or declaration in for loop\n"), NULL;
            }
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
            *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
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
            *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
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
            *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
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
            *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
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
                *p_expr = pool_push(&p->ast_pools[ret.kind.kind], &ret, sizeof(ret));
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
}

void parser_init(struct Parser* p)
{
    array_init(&p->stringpool);
    array_init(&p->toks);
    autoheap_init(&p->strings_to_free);
    scope_init(&p->scope);
    scope_init(&p->type_scope);
    p->fn = NULL;

    for (size_t i = 0; i < AST_KIND_END_POOLS; ++i)
    {
        pool_init(&p->ast_pools[i]);
    }
    array_init(&p->expr_seqs);
    array_init(&p->arr_exprs);
}
void parser_destroy(struct Parser* p)
{
    array_destroy(&p->toks);
    array_destroy(&p->stringpool);
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

int parser_parse(struct Parser* p)
{
    struct Token* cur_tok = (struct Token*)p->toks.data;

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

int parser_push(Parser* p, Lexer* l)
{
    struct Token* const tk = (struct Token*)array_alloc(&p->toks, sizeof(struct Token));
    tk->type = l->state;
    tk->rc = l->tok_rc;

    if (l->state == LEX_EOF)
    {
        tk->sp_offset = 0;
    }
    else
    {
        char* const s = array_push(&p->stringpool, l->tok, l->sz + 1);
        tk->sp_offset = s - (char*)p->stringpool.data;
    }
    return 0;
}
