#include "elaborator.h"

#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "autoheap.h"
#include "errors.h"
#include "lexstate.h"
#include "parse.h"
#include "stdlibe.h"
#include "symbol.h"
#include "token.h"
#include "typestr.h"
#include "unwrap.h"

typedef struct ConstValue
{
    ExprLit* base;
    uint64_t byte_offset;
    uint32_t elem_size;
} ConstValue;

static int32_t i32constant_or_err(const TypeStr* ts, const Token* rc)
{
    if (ts->c.is_const)
    {
        if (!ts->c.is_lvalue && !ts->c.is_sym)
        {
            return (int32_t)ts->c.value.lower;
        }
    }
    parser_tok_error(rc, "error: expected integer constant expression\n");
    return 0;
}
static uint64_t u64constant_or_err(const TypeStr* ts, const Token* rc)
{
    if (ts->c.is_const)
    {
        if (!ts->c.is_lvalue && !ts->c.is_sym)
        {
            return ts->c.value.lower;
        }
    }
    parser_tok_error(rc, "error: expected integer constant expression\n");
    return 0;
}

static Symbol* find_field_by_name(TypeSymbol* def, const char* fieldname, size_t* offset)
{
    for (Symbol* field = def->first_member; field; field = field->next_field)
    {
        if (!field->name)
        {
            if (field->def->specs->suinit)
            {
                // anonymous nested struct
                Symbol* inner_field = find_field_by_name(field->def->specs->sym, fieldname, offset);
                if (inner_field)
                {
                    *offset += field->field_offset;
                    return inner_field;
                }
            }
        }
        else
        {
            if (strcmp(field->name, fieldname) == 0)
            {
                *offset = field->field_offset;
                return field;
            }
        }
    }
    return NULL;
}

static int elaborate_decl(struct Elaborator* elab, struct Decl* specs);
static int elaborate_declspecs(struct Elaborator* elab, struct DeclSpecs* specs);
static void elaborate_stmt(struct Elaborator* elab, struct ElaborateDeclCtx* ctx, struct Ast* ast);
static void elaborate_expr(struct Elaborator* elab,
                           struct ElaborateDeclCtx* ctx,
                           struct Expr* top_expr,
                           struct TypeStr* rty);

enum
{
    BINOP_FLAGS_INT = 1,
    BINOP_FLAGS_ARITH = 2,
    BINOP_FLAGS_SCALAR = 4,
    BINOP_FLAGS_ASSIGN = 8,
    BINOP_FLAGS_INTPROMO = 16,
    BINOP_FLAGS_COMMONTYPE = 32,
};

static unsigned int binop_mask(unsigned int tok_type)
{
    switch (tok_type)
    {
        case TOKEN_SYM1('|'):
        case TOKEN_SYM1('&'):
        case TOKEN_SYM1('^'):
        case TOKEN_SYM1('%'): return BINOP_FLAGS_INT | BINOP_FLAGS_INTPROMO | BINOP_FLAGS_COMMONTYPE;
        case TOKEN_SYM2('<', '<'):
        case TOKEN_SYM2('>', '>'): return BINOP_FLAGS_INT | BINOP_FLAGS_INTPROMO;
        case TOKEN_SYM3('<', '<', '='):
        case TOKEN_SYM3('>', '>', '='):
        case TOKEN_SYM2('^', '='):
        case TOKEN_SYM2('&', '='):
        case TOKEN_SYM2('|', '='): return BINOP_FLAGS_INT | BINOP_FLAGS_ASSIGN | BINOP_FLAGS_INTPROMO;
        case TOKEN_SYM1('/'):
        case TOKEN_SYM1('*'): return BINOP_FLAGS_ARITH | BINOP_FLAGS_INTPROMO | BINOP_FLAGS_COMMONTYPE;
        case TOKEN_SYM2('*', '='):
        case TOKEN_SYM2('/', '='): return BINOP_FLAGS_ARITH | BINOP_FLAGS_ASSIGN | BINOP_FLAGS_INTPROMO;
        case TOKEN_SYM2('=', '='):
        case TOKEN_SYM2('<', '='):
        case TOKEN_SYM2('>', '='):
        case TOKEN_SYM2('!', '='):
        case TOKEN_SYM1('<'):
        case TOKEN_SYM1('>'):
        case TOKEN_SYM1('['):
        case TOKEN_SYM2('|', '|'):
        case TOKEN_SYM2('&', '&'): return BINOP_FLAGS_SCALAR;
        case TOKEN_SYM1('+'):
        case TOKEN_SYM1('-'): return BINOP_FLAGS_SCALAR | BINOP_FLAGS_INTPROMO | BINOP_FLAGS_COMMONTYPE;
        case TOKEN_SYM2('+', '='):
        case TOKEN_SYM2('-', '='): return BINOP_FLAGS_SCALAR | BINOP_FLAGS_ASSIGN | BINOP_FLAGS_INTPROMO;
        case TOKEN_SYM1('='): return BINOP_FLAGS_ASSIGN;
        default: return 0;
    }
}

#if 0
static void set_const_signed(TypeStr* l, unsigned s)
{
    if (l->c.is_const)
    {
        if (l->c.is_signed && !s)
        {
            l->c.u = l->c.i;
        }
        else if (!l->c.is_signed && s)
        {
            l->c.i = l->c.u;
        }
        l->c.is_signed = !!s;
    }
}
#endif

static void promote_common_type(TypeStr* l, TypeStr* r)
{
    const unsigned lhs_mask = typestr_mask(l);
    unsigned lhs_width = lhs_mask & TYPE_MASK_WIDTH;
    const unsigned rhs_mask = typestr_mask(r);
    unsigned rhs_width = rhs_mask & TYPE_MASK_WIDTH;
    if (rhs_width < lhs_width)
    {
        typestr_apply_integral_type(r, l);
    }
    else if (rhs_width > lhs_width)
    {
        typestr_apply_integral_type(l, r);
    }
    else if ((lhs_mask ^ rhs_mask) & TYPE_FLAGS_SIGNED)
    {
        if (lhs_mask & TYPE_FLAGS_SIGNED)
        {
            typestr_apply_integral_type(l, r);
        }
        else
        {
            typestr_apply_integral_type(r, l);
        }
    }
}

static const Constant128 s_zero_constant = {0};
static const Constant128 s_one_constant = {.lower = 1};

static Constant128 mp_u64(uint64_t u)
{
    Constant128 n = {.lower = u};
    return n;
}
static Constant128 mp_i64(int64_t u)
{
    Constant128 n = {.lower = u};
    return n;
}
#if 0
static Constant128 mp_add(Constant128 a, Constant128 b)
{
    Constant128 ret = {a.lower + b.lower};
    return ret;
}
#endif
/// a*ca + b
static Constant128 mp_fma(Constant128 a, int ca, Constant128 b)
{
    a.lower *= ca;
    a.lower += b.lower;
    return a;
}
/// a - b*cb
static Constant128 mp_fsm(Constant128 a, Constant128 b, int cb)
{
    b.lower *= cb;
    a.lower -= b.lower;
    return a;
}
static Constant128 mp_sub(Constant128 a, Constant128 b)
{
    Constant128 ret = {a.lower - b.lower};
    return ret;
}
static Constant128 mp_mul(Constant128 a, Constant128 b)
{
    Constant128 ret = {a.lower * b.lower};
    return ret;
}
static Constant128 mp_div(Constant128 a, Constant128 b, const Token* tok)
{
    if (b.lower == 0)
    {
        parser_tok_error(tok, "error: divide by 0 is undefined.\n");
        return s_zero_constant;
    }
    Constant128 ret = {a.lower / b.lower};
    return ret;
}
static Constant128 mp_idiv(Constant128 a, int b, const Token* tok)
{
    if (b == 0)
    {
        parser_tok_error(tok, "error: divide by 0 is undefined.\n");
        return s_zero_constant;
    }
    a.lower /= b;
    return a;
}
static Constant128 mp_mod(Constant128 a, Constant128 b, const Token* tok)
{
    if (b.lower == 0)
    {
        parser_tok_error(tok, "error: remainder by 0 is undefined.\n");
        return s_zero_constant;
    }
    Constant128 ret = {a.lower / b.lower};
    return ret;
}
static Constant128 mp_bor(Constant128 a, Constant128 b)
{
    Constant128 ret = {a.lower | b.lower};
    return ret;
}
#if 0
static Constant128 mp_bnot(Constant128 a)
{
    Constant128 ret = {~a.lower};
    return ret;
}
#endif
static Constant128 mp_band(Constant128 a, Constant128 b)
{
    Constant128 ret = {a.lower & b.lower};
    return ret;
}
static Constant128 mp_bxor(Constant128 a, Constant128 b)
{
    Constant128 ret = {a.lower ^ b.lower};
    return ret;
}
static Constant128 mp_shl(Constant128 a, Constant128 b, const Token* tok)
{
    if (b.lower >= 64)
    {
        parser_tok_error(tok, "error: left-shifting a value by <0 or >=64 is undefined behavior.\n");
        return s_zero_constant;
    }
    a.lower <<= b.lower;
    return a;
}
static Constant128 mp_shr(Constant128 a, Constant128 b, const Token* tok)
{
    if (b.lower >= 64)
    {
        parser_tok_error(tok, "error: right-shifting a value by <0 or >=64 is undefined behavior.\n");
        return s_zero_constant;
    }
    a.lower >>= b.lower;
    return a;
}

static void elaborate_expr_ternary(struct Elaborator* elab,
                                   struct ElaborateDeclCtx* ctx,
                                   struct ExprBinOp* e,
                                   struct Expr* etrue,
                                   struct Expr* efalse,
                                   struct TypeStr* rty)
{
    TypeStr cond_ty, etrue_ty, efalse_ty;
    elaborate_expr(elab, ctx, e->lhs, &cond_ty);
    elaborate_expr(elab, ctx, etrue, &etrue_ty);
    elaborate_expr(elab, ctx, efalse, &efalse_ty);

    const TypeStr cond_ty_orig = cond_ty;
    typestr_decay(&cond_ty);
    const unsigned int cond_mask = typestr_mask(&cond_ty);

    const TypeStr etrue_ty_orig = etrue_ty;
    typestr_decay(&etrue_ty);
    const unsigned int etrue_mask = typestr_mask(&etrue_ty);

    const TypeStr efalse_ty_orig = efalse_ty;
    typestr_decay(&efalse_ty);
    const unsigned int efalse_mask = typestr_mask(&efalse_ty);
    if (!(cond_mask & TYPE_MASK_SCALAR))
    {
        typestr_error1(
            &e->tok->rc, elab->types, "error: expected scalar type in first argument but got '%.*s'\n", &cond_ty_orig);
        *rty = s_type_int;
        return;
    }

    if (etrue_mask & efalse_mask & TYPE_MASK_ARITH)
    {
        promote_common_type(&etrue_ty, &efalse_ty);
        *rty = etrue_ty;
    }
    else if (typestr_match(&etrue_ty, &efalse_ty))
    {
        *rty = etrue_ty;
    }
    else if (etrue_mask & efalse_mask & TYPE_FLAGS_POINTER)
    {
        Constant ct = etrue_ty.c, cf = efalse_ty.c;
        typestr_dereference(&etrue_ty);
        typestr_dereference(&efalse_ty);
        unsigned int combined_cvr = typestr_strip_cvr(&etrue_ty) | typestr_strip_cvr(&efalse_ty);
        etrue_ty.c = ct, efalse_ty.c = cf;
        if (typestr_match(&etrue_ty, &s_type_void))
        {
            *rty = efalse_ty;
            typestr_add_cvr(rty, combined_cvr);
            typestr_add_pointer(rty);
        }
        if (typestr_match(&efalse_ty, &s_type_void))
        {
            *rty = etrue_ty;
            typestr_add_cvr(rty, combined_cvr);
            typestr_add_pointer(rty);
        }
        else if (typestr_match(&efalse_ty, &etrue_ty))
        {
            *rty = etrue_ty;
            typestr_add_cvr(rty, combined_cvr);
            typestr_add_pointer(rty);
        }
        else
        {
            typestr_error2(&e->tok->rc,
                           elab->types,
                           "error: unable to determine common pointer type in ternary between '%.*s' and '%.*s'\n",
                           &etrue_ty_orig,
                           &efalse_ty_orig);
            *rty = s_type_unknown;
        }
    }
    else if ((etrue_mask & TYPE_FLAGS_POINTER) && typestr_is_constant_zero(&efalse_ty))
    {
        *rty = etrue_ty;
    }
    else if ((efalse_mask & TYPE_FLAGS_POINTER) && typestr_is_constant_zero(&etrue_ty))
    {
        *rty = efalse_ty;
    }
    else
    {
        typestr_error2(&e->tok->rc,
                       elab->types,
                       "error: unable to determine common type in ternary between '%.*s' and '%.*s'\n",
                       &etrue_ty_orig,
                       &efalse_ty_orig);
        *rty = s_type_unknown;
    }

    if (cond_ty.c.is_const && !cond_ty.c.is_lvalue && efalse_ty.c.is_const && !efalse_ty.c.is_lvalue &&
        etrue_ty.c.is_const && !etrue_ty.c.is_lvalue)
    {
        if (!cond_ty.c.is_sym && !cond_ty.c.value.lower)
        {
            rty->c = efalse_ty.c;
        }
        else
        {
            rty->c = etrue_ty.c;
        }
    }
    else
    {
        rty->c = s_not_constant;
    }
}

static int cnst_same_base(Constant c1, Constant c2)
{
    void* p1 = (c1.is_sym ? c1.is_lit ? (void*)c1.lit : (void*)c1.sym : NULL);
    void* p2 = (c2.is_sym ? c2.is_lit ? (void*)c2.lit : (void*)c2.sym : NULL);
    return p1 == p2;
}
static int cnst_eq(Constant c1, Constant c2) { return cnst_same_base(c1, c2) && c1.value.lower == c2.value.lower; }
static int cnst_truthy(Constant c1)
{
    return (c1.is_sym ? (NULL != (c1.is_lit ? (void*)c1.lit : (void*)c1.sym)) : 0) || c1.value.lower;
}

static void elaborate_expr_ExprBinOp(struct Elaborator* elab,
                                     struct ElaborateDeclCtx* ctx,
                                     struct ExprBinOp* e,
                                     struct TypeStr* rty)
{
#if defined(TRACING_ELAB)
    fprintf(stderr, "elaborate_expr_ExprBinOp\n");
#endif
    if (e->tok->type == TOKEN_SYM1('?'))
    {
        if (!e->rhs || e->rhs->kind != EXPR_BINOP) abort();
        return elaborate_expr_ternary(elab, ctx, e, ((ExprBinOp*)e->rhs)->lhs, ((ExprBinOp*)e->rhs)->rhs, rty);
    }
    elaborate_expr(elab, ctx, e->lhs, rty);
    const struct TypeStr orig_lhs = *rty;
    typestr_decay(rty);
    unsigned int lhs_mask = typestr_mask(rty);

    struct TypeStr rhs_ty;
    elaborate_expr(elab, ctx, e->rhs, &rhs_ty);
    const struct TypeStr orig_rhs = rhs_ty;
    typestr_decay(&rhs_ty);
    unsigned int rhs_mask = typestr_mask(&rhs_ty);

    const unsigned int binmask = binop_mask(e->tok->type);

    // check lhs mask
    if (binmask & BINOP_FLAGS_INT)
    {
        if (!(lhs_mask & TYPE_FLAGS_INT))
        {
            typestr_error1(
                &e->tok->rc, elab->types, "error: expected integer type in first argument but got '%.*s'\n", &orig_lhs);
            *rty = s_type_int;
            return;
        }
        if (!(rhs_mask & TYPE_FLAGS_INT))
        {
            typestr_error1(&e->tok->rc,
                           elab->types,
                           "error: expected integer type in second argument but got '%.*s'\n",
                           &orig_rhs);
            *rty = s_type_int;
            return;
        }
    }
    else if (binmask & BINOP_FLAGS_ARITH)
    {
        if (!(lhs_mask & TYPE_MASK_ARITH))
        {
            typestr_error1(&e->tok->rc,
                           elab->types,
                           "error: expected arithmetic type in first argument but got '%.*s'\n",
                           &orig_lhs);
            *rty = s_type_int;
            return;
        }
        if (!(rhs_mask & TYPE_MASK_ARITH))
        {
            typestr_error1(&e->tok->rc,
                           elab->types,
                           "error: expected arithmetic type in second argument but got '%.*s'\n",
                           &orig_rhs);
            *rty = s_type_int;
            return;
        }
    }
    else if (binmask & BINOP_FLAGS_SCALAR || e->tok->type == TOKEN_SYM1('?'))
    {
        if (!(lhs_mask & TYPE_MASK_SCALAR))
        {
            typestr_error1(
                &e->tok->rc, elab->types, "error: expected scalar type in first argument but got '%.*s'\n", &orig_lhs);
            *rty = s_type_int;
            return;
        }
        if ((binmask & BINOP_FLAGS_SCALAR) && !(rhs_mask & TYPE_MASK_SCALAR))
        {
            typestr_error1(
                &e->tok->rc, elab->types, "error: expected scalar type in second argument but got '%.*s'\n", &orig_rhs);
            *rty = s_type_int;
            return;
        }
    }

    // perform op-specifics
    const int rty_is_lhs = !!(binmask & BINOP_FLAGS_ASSIGN);
    if (binmask & BINOP_FLAGS_INTPROMO)
    {
        typestr_promote_integer(rty);
        typestr_promote_integer(&rhs_ty);
        if (!((rhs_mask | lhs_mask) & TYPE_FLAGS_POINTER))
        {
            promote_common_type(rty, &rhs_ty);
        }
    }
    switch (e->tok->type)
    {
        case TOKEN_SYM2('*', '='):
        case TOKEN_SYM2('%', '='):
        case TOKEN_SYM2('/', '='):
        case TOKEN_SYM2('&', '='):
        case TOKEN_SYM2('|', '='):
        case TOKEN_SYM3('<', '<', '='):
        case TOKEN_SYM3('>', '>', '='):
        case TOKEN_SYM1('&'):
        case TOKEN_SYM1('|'):
        case TOKEN_SYM1('^'):
        case TOKEN_SYM1('*'):
        case TOKEN_SYM1('%'):
        case TOKEN_SYM2('<', '<'):
        case TOKEN_SYM1('/'):
        case TOKEN_SYM2('>', '>'): break;
        case TOKEN_SYM2('+', '='):
        case TOKEN_SYM2('-', '='):
            if (rhs_mask & TYPE_FLAGS_POINTER)
            {
                typestr_error1(&e->tok->rc,
                               elab->types,
                               "error: expected integer type on right-hand side, but got '%.*s'\n",
                               &orig_rhs);
            }
            e->info = typestr_get_add_size(elab->types, rty, &e->tok->rc);
            break;
        case TOKEN_SYM1('+'):
            if (rhs_mask & lhs_mask & TYPE_FLAGS_POINTER)
            {
                typestr_error2(&e->tok->rc,
                               elab->types,
                               "error: expected only one pointer type, but got '%.*s' and '%.*s'\n",
                               &orig_lhs,
                               &orig_rhs);
            }
            if (rhs_mask & TYPE_FLAGS_POINTER) rty->buf = rhs_ty.buf;
            e->info = typestr_get_add_size(elab->types, rty, &e->tok->rc);
            break;
        case TOKEN_SYM1('-'):
            e->info = typestr_get_add_size(elab->types, rty, &e->tok->rc);
            if (rhs_mask & TYPE_FLAGS_POINTER)
            {
                if (!(lhs_mask & TYPE_FLAGS_POINTER))
                {
                    typestr_error2(&e->tok->rc,
                                   elab->types,
                                   "error: expected both pointer types, but got '%.*s' and '%.*s'\n",
                                   &orig_lhs,
                                   &orig_rhs);
                }
                e->info = -e->info;
                rty->buf = s_type_ptrdiff.buf;
            }
            break;
        case TOKEN_SYM1('>'):
        case TOKEN_SYM1('<'):
        case TOKEN_SYM2('=', '='):
        case TOKEN_SYM2('!', '='):
        case TOKEN_SYM2('>', '='):
        case TOKEN_SYM2('<', '='):
        case TOKEN_SYM2('&', '&'):
        case TOKEN_SYM2('|', '|'): rty->buf = s_type_int.buf; break;
        case TOKEN_SYM1('='): typestr_implicit_conversion(elab->types, &e->tok->rc, &rhs_ty, rty); break;
        case TOKEN_SYM1('['):
            if (rhs_mask & lhs_mask & TYPE_FLAGS_POINTER || !((rhs_mask | lhs_mask) & TYPE_FLAGS_POINTER))
            {
                typestr_error2(&e->tok->rc,
                               elab->types,
                               "error: expected exactly one pointer type, but got '%.*s' and '%.*s'\n",
                               &orig_lhs,
                               &orig_rhs);
            }
            if (rhs_mask & TYPE_FLAGS_POINTER)
            {
                // normalize operation so pointer is always the LHS
                struct Expr* f = e->lhs;
                e->lhs = e->rhs;
                e->rhs = f;
                *rty = rhs_ty;
            }
            typestr_dereference(rty);
            e->info = typestr_get_size(elab->types, rty, &e->tok->rc);
            break;
        case TOKEN_SYM1(','): *rty = rhs_ty; break;
        default:
            fprintf(stderr, "warning: untyped binary operator '%s'\n", token_str(elab->p, e->tok));
            *rty = s_type_unknown;
            break;
    }

    if (rty_is_lhs)
    {
        *rty = orig_lhs;
    }
    else if (rty->c.is_const && rhs_ty.c.is_const && !rty->c.is_lvalue && !rhs_ty.c.is_lvalue)
    {
        switch (e->tok->type)
        {
            case TOKEN_SYM2('=', '='):
                typestr_assign_constant_value(rty, cnst_eq(rty->c, rhs_ty.c) ? s_one_constant : s_zero_constant);
                break;
            case TOKEN_SYM2('&', '&'):
                typestr_assign_constant_value(
                    rty, (cnst_truthy(rty->c) && cnst_truthy(rhs_ty.c)) ? s_one_constant : s_zero_constant);
                break;
            case TOKEN_SYM2('|', '|'):
                typestr_assign_constant_value(
                    rty, (cnst_truthy(rty->c) || cnst_truthy(rhs_ty.c)) ? s_one_constant : s_zero_constant);
                break;
            case TOKEN_SYM1('&'): typestr_assign_constant_value(rty, mp_band(rty->c.value, rhs_ty.c.value)); break;
            case TOKEN_SYM1('|'): typestr_assign_constant_value(rty, mp_bor(rty->c.value, rhs_ty.c.value)); break;
            case TOKEN_SYM1('^'): typestr_assign_constant_value(rty, mp_bxor(rty->c.value, rhs_ty.c.value)); break;
            case TOKEN_SYM1('*'): typestr_assign_constant_value(rty, mp_mul(rty->c.value, rhs_ty.c.value)); break;
            case TOKEN_SYM1('%'):
                typestr_assign_constant_value(rty, mp_mod(rty->c.value, rhs_ty.c.value, e->tok));
                break;
            case TOKEN_SYM1('/'):
                typestr_assign_constant_value(rty, mp_div(rty->c.value, rhs_ty.c.value, e->tok));
                break;
            case TOKEN_SYM2('<', '<'):
                typestr_assign_constant_value(rty, mp_shl(rty->c.value, rhs_ty.c.value, e->tok));
                break;
            case TOKEN_SYM2('>', '>'):
                typestr_assign_constant_value(rty, mp_shr(rty->c.value, rhs_ty.c.value, e->tok));
                break;
            case TOKEN_SYM1('+'):
                if (rhs_mask & TYPE_FLAGS_POINTER)
                    typestr_assign_constant_value(rty, mp_fma(rty->c.value, e->info, rhs_ty.c.value));
                else
                    typestr_assign_constant_value(rty, mp_fma(rhs_ty.c.value, e->info, rty->c.value));
                break;
            case TOKEN_SYM1('-'):
                if ((TYPE_FLAGS_POINTER & lhs_mask & rhs_mask) && rty->c.is_sym && rhs_ty.c.is_sym)
                {
                    if (rty->c.is_lit)
                    {
                        if (!rhs_ty.c.is_lit || rty->c.lit != rhs_ty.c.lit)
                        {
                            parser_tok_error(
                                e->tok,
                                "error: subtracting two pointers from different literals is undefined behavior.\n");
                            return;
                        }
                    }
                    else
                    {
                        if (rhs_ty.c.is_lit || rty->c.sym != rhs_ty.c.sym)
                        {
                            parser_tok_error(
                                e->tok,
                                "error: subtracting two pointers from different aggregates is undefined behavior.\n");
                            return;
                        }
                    }
                    rty->c.is_sym = 0;
                    rty->c.is_lit = 0;
                    typestr_assign_constant_value(rty, mp_idiv(mp_sub(rty->c.value, rhs_ty.c.value), -e->info, e->tok));
                }
                else if (!(TYPE_FLAGS_POINTER & lhs_mask & rhs_mask) && !rty->c.is_sym && !rhs_ty.c.is_sym)
                {
                    typestr_assign_constant_value(rty, mp_fsm(rty->c.value, rhs_ty.c.value, e->info));
                }
                else
                {
                    typestr_error2(
                        &e->tok->rc, elab->types, "error: invalid constants '%.*s' and '%.*s'\n", rty, &rhs_ty);
                    return;
                }
                break;
            default: rty->c = s_not_constant; break;
        }
    }
    else
    {
        rty->c = s_not_constant;
    }
}

static const TypeStr s_valist_ptr = {.buf = {2, TYPE_BYTE_UUVALIST, TYPE_BYTE_POINTER}};

static void elaborate_expr_ExprBuiltin(struct Elaborator* elab,
                                       struct ElaborateDeclCtx* ctx,
                                       struct ExprBuiltin* e,
                                       struct TypeStr* rty)
{
    switch (e->tok->type)
    {
        case LEX_SIZEOF:
            if (e->expr1)
            {
                elaborate_expr(elab, ctx, e->expr1, rty);
            }
            else
            {
                elaborate_declspecs(elab, e->specs);
                elaborate_decl(elab, e->type);
                typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, rty, e->type);
            }
            e->sizeof_size = typestr_get_size(elab->types, rty, &e->tok->rc);
            *rty = s_type_int;
            typestr_assign_constant_value(rty, mp_u64(e->sizeof_size));
            break;
        case LEX_UUVA_START:
            elaborate_expr(elab, ctx, e->expr2, rty);
            elaborate_expr(elab, ctx, e->expr1, rty);
            typestr_decay(rty);
            if (!typestr_match(rty, &s_valist_ptr))
            {
                typestr_error2(
                    &e->tok->rc,
                    elab->types,
                    "error: expected variable of type '%.*s' as first argument to va_start, but got '%.*s'\n",
                    &s_valist_ptr,
                    rty);
            }
            *rty = s_type_void;
            break;
        case LEX_UUVA_ARG:
            elaborate_expr(elab, ctx, e->expr1, rty);
            elaborate_declspecs(elab, e->specs);
            elaborate_decl(elab, e->type);
            typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, rty, e->type);
            break;
        case LEX_UUVA_END: *rty = s_type_void; break;
        case LEX_UUVA_COPY:
            elaborate_expr(elab, ctx, e->expr2, rty);
            elaborate_expr(elab, ctx, e->expr1, rty);
            *rty = s_type_void;
            break;
        default:
            parser_tok_error(e->tok, "error: unimplemented builtin\n");
            *rty = s_type_unknown;
            break;
    }
}

static void elaborate_expr_ExprUnOp(struct Elaborator* elab,
                                    struct ElaborateDeclCtx* ctx,
                                    struct ExprUnOp* e,
                                    struct TypeStr* rty)
{
    elaborate_expr(elab, ctx, e->lhs, rty);
    const struct TypeStr orig_lhs = *rty;
    typestr_decay(rty);
    unsigned int lhs_mask = typestr_mask(rty);
    switch (e->tok->type)
    {
        case TOKEN_SYM1('+'):
            if (!(lhs_mask & TYPE_MASK_ARITH))
            {
                typestr_error1(&e->tok->rc,
                               elab->types,
                               "error: expected arithmetic type in first argument but got '%.*s'\n",
                               &orig_lhs);
                *rty = s_type_int;
            }
            break;
        case TOKEN_SYM1('-'):
            if (!(lhs_mask & TYPE_MASK_ARITH))
            {
                typestr_error1(&e->tok->rc,
                               elab->types,
                               "error: expected arithmetic type in first argument but got '%.*s'\n",
                               &orig_lhs);
                *rty = s_type_int;
            }
            break;
        case TOKEN_SYM1('*'):
            if (typestr_is_pointer(rty))
            {
                typestr_dereference(rty);
            }
            else
            {
                typestr_error1(&e->tok->rc, elab->types, "error: cannot dereference value of type '%.*s'\n", rty);
                *rty = s_type_unknown;
            }
            break;
        case TOKEN_SYM1('&'):
        {
            *rty = orig_lhs;
            if (e->lhs->kind == EXPR_REF && (typestr_mask(&((ExprRef*)e->lhs)->sym->type) & TYPE_MASK_FN_ARR))
            {
            }
            else
            {
                typestr_add_pointer(rty);
            }
            break;
        }
        case TOKEN_SYM2('+', '+'):
        case TOKEN_SYM2('-', '-'):
            if (!(lhs_mask & TYPE_MASK_SCALAR))
            {
                typestr_error1(&e->tok->rc,
                               elab->types,
                               "error: expected scalar type in first argument but got '%.*s'\n",
                               &orig_lhs);
                *rty = s_type_unknown;
            }
            else if (lhs_mask & TYPE_FLAGS_POINTER)
            {
                e->sizeof_ = typestr_get_add_size(elab->types, rty, &e->tok->rc);
            }
            else
            {
                e->sizeof_ = 1;
            }
            break;
        case TOKEN_SYM1('~'):
            if (!(lhs_mask & TYPE_FLAGS_INT))
            {
                typestr_error1(&e->tok->rc,
                               elab->types,
                               "error: expected integer type in first argument but got '%.*s'\n",
                               &orig_lhs);
            }
            *rty = s_type_int;
            break;
        case TOKEN_SYM1('!'):
            if (!(lhs_mask & TYPE_MASK_SCALAR))
            {
                typestr_error1(&e->tok->rc,
                               elab->types,
                               "error: expected scalar type in first argument but got '%.*s'\n",
                               &orig_lhs);
            }
            *rty = s_type_int;
            break;
        default:
            fprintf(stderr, "warning: untyped unary operator '%s'\n", token_str(elab->p, e->tok));
            *rty = s_type_unknown;
            break;
    }
}
static void elaborate_stmts(struct Elaborator* elab, struct ElaborateDeclCtx* ctx, size_t offset, size_t extent)
{
    struct Ast** seqs = elab->p->expr_seqs.data;
    for (size_t i = 0; i < extent; ++i)
    {
        elaborate_stmt(elab, ctx, seqs[offset + i]);
    }
}

static void elaborate_init_ty(struct Elaborator* elab, size_t offset, const TypeStr* dty, struct Ast* ast);
static void elaborate_init(struct Elaborator* elab, size_t offset, struct Decl* decl, struct Ast* ast)
{
    struct TypeStr dty;
    typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, &dty, decl);
    typestr_strip_cvr(&dty);
    elaborate_init_ty(elab, offset, &dty, ast);
}

typedef struct DInitFrame
{
    size_t offset;
    Symbol* field;
    uint8_t is_array : 1;
    uint8_t is_unk : 1;
    uint8_t is_union : 1;
    uint32_t extent;
    uint32_t index;
    uint32_t elem_size;
    /// Always cvr-stripped
    struct TypeStr ty;
} DInitFrame;

typedef struct DInitIter
{
    size_t offset;
    TypeStr cur_ty;
    /// Array<DInitFrame>
    Array stk;
} DInitIter;

static void di_init(DInitIter* i) { memset(i, 0, sizeof(DInitIter)); }
static void di_destroy(DInitIter* i) { array_destroy(&i->stk); }

static int di_fill_frame(DInitFrame* frame,
                         Elaborator* elab,
                         size_t offset,
                         const TypeStr* parent_ty,
                         size_t designator_idx,
                         const RowCol* rc)
{
    memset(frame, 0, sizeof(*frame));
    char b = typestr_byte(parent_ty);
    switch (b)
    {
        case TYPE_BYTE_UNK_ARRAY:
        case TYPE_BYTE_ARRAY:
        {
            frame->is_array = 1;
            frame->ty = *parent_ty;
            typestr_strip_cvr(&frame->ty);
            if (b == TYPE_BYTE_UNK_ARRAY)
            {
                frame->extent = UINT32_MAX;
            }
            else
            {
                frame->extent = typestr_get_offset(&frame->ty);
            }
            typestr_remove_array(&frame->ty);
            typestr_strip_cvr(&frame->ty);
            frame->elem_size = typestr_get_size(elab->types, &frame->ty, rc);
            if (frame->extent == 0)
            {
                return parser_ferror(rc, "error: array must have nonzero extent\n");
            }
            if (designator_idx != SIZE_MAX)
            {
                const Designator* const designator = (const Designator*)elab->p->designators.data + designator_idx;

                if (!designator->array_expr)
                {
                    return parser_ferror(rc, "error: invalid member designator for array object\n");
                }
                TypeStr ts = {0};
                elaborate_expr(elab, NULL, designator->array_expr, &ts);
                int32_t k = i32constant_or_err(&ts, designator->array_expr->tok);
                if (k >= frame->extent)
                {
                    return parser_ferror(
                        rc, "error: array designator exceeds bounds: '%d' >= '%zu'\n", k, frame->extent);
                }
                frame->index = k;
            }
            frame->offset = offset + frame->elem_size * frame->index;
            return 0;
        }
        case TYPE_BYTE_UNION: frame->is_union = 1;
        case TYPE_BYTE_STRUCT:
        {
            TypeSymbol* sym = typestr_get_decl(elab->types, parent_ty);
            if (!sym || !sym->def)
            {
                return parser_ferror(rc, "error: incomplete type\n");
            }
            if (designator_idx == SIZE_MAX)
            {
                frame->field = sym->first_member;
                if (!frame->field) return 1;
                frame->offset = frame->field->field_offset;
            }
            else
            {
                const Designator* const designator = (const Designator*)elab->p->designators.data + designator_idx;

                if (!designator->field)
                {
                    return parser_ferror(rc, "error: invalid array designator for struct/union\n");
                }
                frame->field = find_field_by_name(sym, designator->field, &frame->offset);
                if (!frame->field)
                {
                    return parser_ferror(rc, "error: field not found in structure: '%s'\n", designator->field);
                }
            }
            frame->offset += offset;
            typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, &frame->ty, frame->field->def);
            typestr_strip_cvr(&frame->ty);
            return 0;
        }
        default: return parser_ferror(rc, "error: unimplemented type for initializer list\n");
    }
}

static int di_enter(DInitIter* i, struct Elaborator* elab, const RowCol* rc)
{
    if (0 == i->stk.sz)
    {
        return di_fill_frame(array_alloc(&i->stk, sizeof(DInitFrame)), elab, i->offset, &i->cur_ty, SIZE_MAX, rc);
    }
    DInitFrame* new_frame = array_alloc(&i->stk, sizeof(DInitFrame));
    DInitFrame* prev_frame = new_frame - 1;
    return di_fill_frame(new_frame, elab, prev_frame->offset, &prev_frame->ty, SIZE_MAX, rc);
}

static int di_reset(DInitIter* i, struct Elaborator* elab, size_t offset, const struct TypeStr* dty, const RowCol* rc)
{
    i->offset = offset;
    i->cur_ty = *dty;
    array_clear(&i->stk);
    return di_enter(i, elab, rc);
}

static void di_next(DInitIter* i, Elaborator* elab)
{
    goto loop;
pop:
    array_pop(&i->stk, sizeof(DInitFrame));
loop:;
    if (0 == i->stk.sz) return;
    DInitFrame* back = array_back(&i->stk, sizeof(DInitFrame));
    if (back->is_array)
    {
        ++back->index;
        if (back->index == back->extent) goto pop;
        back->offset += back->elem_size;
    }
    else
    {
        const size_t prev_field_offset = back->field->field_offset;
        if (back->is_union)
            back->field = NULL;
        else
            back->field = back->field->next_field;
        if (back->field == NULL) goto pop;
        back->offset += back->field->field_offset - prev_field_offset;
        typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, &back->ty, back->field->def);
        typestr_strip_cvr(&back->ty);
    }
}

static int di_end(DInitIter* i) { return 0 == i->stk.sz; }

static void elaborate_init_ty_AstInit(struct Elaborator* elab, size_t offset, const TypeStr* dty, struct AstInit* init)
{
    struct DInitIter iter;
    di_init(&iter);
    if (di_reset(&iter, elab, offset, dty, &init->tok->rc)) goto fail;

    for (; init->init; init = init->next, di_next(&iter, elab))
    {
        if (init->designator_extent != 0)
        {
            array_clear(&iter.stk);
            if (di_fill_frame(array_alloc(&iter.stk, sizeof(DInitFrame)),
                              elab,
                              iter.offset,
                              &iter.cur_ty,
                              init->designator_offset,
                              &init->tok->rc))
                goto fail;
            for (size_t k = 1; k < init->designator_extent; ++k)
            {
                DInitFrame* f = array_alloc(&iter.stk, sizeof(DInitFrame));
                if (di_fill_frame(f, elab, f[-1].offset, &f[-1].ty, init->designator_offset + k, &init->tok->rc))
                    goto fail;
            }
        }
        if (di_end(&iter)) break;
        DInitFrame* back = array_back(&iter.stk, sizeof(*back));
        if (init->init->kind == AST_INIT)
        {
            init->offset = back->offset;
            init->sizing = typestr_calc_sizing(elab->types, &back->ty, &init->init->tok->rc);
            elaborate_init_ty_AstInit(elab, back->offset, &back->ty, (AstInit*)init->init);
        }
        else
        {
            if (!ast_kind_is_expr(init->init->kind))
            {
                parser_tok_error(init->init->tok, "error: expected expression in object initialization\n");
                goto fail;
            }
            Expr* expr = (Expr*)init->init;
            // standard expression initialization
            struct TypeStr ts, ts_decay;
            elaborate_expr(elab, NULL, expr, &ts);
            ts_decay = ts;
            typestr_decay(&ts_decay);

            while (typestr_is_aggregate(&back->ty))
            {
                if (typestr_match(&back->ty, &ts)) break;
                if (typestr_match(&back->ty, &ts_decay)) break;
                if (di_enter(&iter, elab, &init->tok->rc)) goto fail;
                back = array_back(&iter.stk, sizeof(*back));
            }
            typestr_implicit_conversion(elab->types, &init->init->tok->rc, &ts, &back->ty);
            init->is_char_arr = typestr_is_aggregate(&back->ty);
            init->offset = back->offset;
            init->sizing = typestr_calc_sizing(elab->types, &back->ty, &init->init->tok->rc);
        }
    }

    if (di_end(&iter) && init->init)
    {
        parser_tok_error(init->tok, "error: too many initializers\n");
        goto fail;
    }

fail:
    di_destroy(&iter);
}

static void elaborate_init_ty(struct Elaborator* elab, size_t offset, const TypeStr* dty, struct Ast* ast)
{
    if (ast->kind == AST_INIT)
    {
        return elaborate_init_ty_AstInit(elab, offset, dty, (struct AstInit*)ast);
    }
    const char tyb = typestr_byte(dty);
    switch (tyb)
    {
        case TYPE_BYTE_FUNCTION: elaborate_stmt(elab, NULL, ast); break;
        case TYPE_BYTE_ARRAY:
        case TYPE_BYTE_UNK_ARRAY:
        {
            TypeStr ts = *dty;
            typestr_remove_array(&ts);
            typestr_strip_cvr(&ts);
            if (!typestr_match(&ts, &s_type_char))
            {
                parser_tok_error(ast->tok, "error: array initializer must be an initializer list\n");
                break;
            }
            if (ast->kind != EXPR_LIT || ast->tok->type != LEX_STRING)
            {
                parser_tok_error(ast->tok,
                                 "error: array initializer must be an initializer list or a string literal\n");
                break;
            }
            elaborate_expr(elab, NULL, (Expr*)ast, &ts);
            break;
        }
        case TYPE_BYTE_UNION:
        case TYPE_BYTE_STRUCT:
        default:
        {
            if (!ast_kind_is_expr(ast->kind))
            {
                parser_tok_error(ast->tok, "error: expected expression in object initialization\n");
                break;
            }
            Expr* expr = (Expr*)ast;
            // standard expression initialization
            struct TypeStr ts;
            elaborate_expr(elab, NULL, expr, &ts);
            typestr_implicit_conversion(elab->types, ast->tok ? &ast->tok->rc : NULL, &ts, dty);
        }
    }
}

#define DISPATCH(X, Y)                                                                                                 \
    case X: return elaborate_stmt_##Y(elab, ctx, (struct Y*)ast)

static void elaborate_stmt_StmtCase(struct Elaborator* elab, struct ElaborateDeclCtx* ctx, struct StmtCase* stmt)
{
    if (stmt->expr)
    {
        TypeStr ts = {0};
        elaborate_expr(elab, ctx, stmt->expr, &ts);
        stmt->value = u64constant_or_err(&ts, stmt->expr->tok);
    }
}

static void elaborate_stmt(struct Elaborator* elab, struct ElaborateDeclCtx* ctx, struct Ast* ast)
{
    if (ast_kind_is_expr(ast->kind))
    {
        struct TypeStr ts;
        return elaborate_expr(elab, ctx, (struct Expr*)ast, &ts);
    }
    ast->elaborated = 1;
    void* top = ast;
    switch (ast->kind)
    {
        DISPATCH(STMT_CASE, StmtCase);
        case STMT_NONE:
        case STMT_BREAK:
        case STMT_CONTINUE:
        case STMT_GOTO: return;
        case STMT_LABEL:
        {
            struct StmtLabel* expr = top;
            return elaborate_stmt(elab, ctx, expr->stmt);
        }
        case STMT_RETURN:
        {
            struct StmtReturn* stmt = top;
            if (stmt->expr)
            {
                struct TypeStr ts;
                elaborate_expr(elab, ctx, stmt->expr, &ts);
                struct TypeStr fn = elab->cur_decl->sym->type;
                typestr_pop_offset(&fn);
                typestr_implicit_conversion(elab->types, &stmt->tok->rc, &ts, &fn);
            }

            return;
        }
        case STMT_IF:
        {
            struct StmtIf* stmt = top;
            struct TypeStr ts;
            elaborate_expr(elab, ctx, stmt->cond, &ts);
            elaborate_stmt(elab, ctx, stmt->if_body);
            if (stmt->else_body)
            {
                elaborate_stmt(elab, ctx, stmt->else_body);
            }
            return;
        }
        case STMT_LOOP:
        {
            struct StmtLoop* e = top;
            struct TypeStr ts;
            if (e->init) elaborate_stmt(elab, ctx, e->init);
            if (e->cond) elaborate_expr(elab, ctx, e->cond, &ts);
            if (e->advance) elaborate_expr(elab, ctx, e->advance, &ts);
            elaborate_stmt(elab, ctx, e->body);
            return;
        }
        case AST_DECL:
        {
            struct Decl* d = top;
            if (!d->type) abort();
            elaborate_decl(elab, d);
            return;
        }
        case STMT_DECLS:
        {
            struct StmtDecls* stmt = top;
            elaborate_declspecs(elab, stmt->specs);
            elaborate_stmts(elab, ctx, stmt->offset, stmt->extent);
            return;
        }
        case STMT_BLOCK:
        {
            struct StmtBlock* stmt = top;
            elaborate_stmts(elab, ctx, stmt->offset, stmt->extent);
            return;
        }
        case STMT_SWITCH:
        {
            struct StmtSwitch* stmt = top;
            struct TypeStr ts;
            elaborate_expr(elab, ctx, stmt->expr, &ts);
            typestr_decay(&ts);
            if (!(typestr_mask(&ts) & TYPE_MASK_SCALAR))
            {
                typestr_error1(&stmt->tok->rc,
                               elab->types,
                               "error: expected scalar type in switch condition but got '%.*s'\n",
                               &ts);
            }
            elaborate_stmts(elab, ctx, stmt->offset, stmt->extent);
            return;
        }
        default: parser_tok_error(NULL, "error: unknown stmt kind: %s\n", ast_kind_to_string(ast->kind)); return;
    }
}

static const size_t s_mint_maximums[] = {INT_MAX, UINT_MAX, LONG_MAX, ULONG_MAX, LLONG_MAX, ULLONG_MAX};
static const char s_uint_types[] = {
    TYPE_BYTE_UINT, TYPE_BYTE_UINT, TYPE_BYTE_ULONG, TYPE_BYTE_ULONG, TYPE_BYTE_ULLONG, TYPE_BYTE_ULLONG};
static const char s_sint_types[] = {
    TYPE_BYTE_INT, TYPE_BYTE_LONG, TYPE_BYTE_LONG, TYPE_BYTE_LLONG, TYPE_BYTE_LLONG, TYPE_BYTE_ULLONG};
static const char s_mint_types[] = {
    TYPE_BYTE_INT, TYPE_BYTE_UINT, TYPE_BYTE_LONG, TYPE_BYTE_ULONG, TYPE_BYTE_LLONG, TYPE_BYTE_ULLONG};

static void typestr_from_numlit(TypeStr* t, unsigned char byte, uint64_t numeric)
{
    t->buf.buf[0] = 1;
    t->buf.buf[1] = byte;
    memset(&t->c, 0, sizeof(t->c));
    t->c.is_const = 1;
    typestr_assign_constant_value(t, mp_u64(numeric));
}

static void typestr_from_strlit(TypeStr* t, ExprLit* lit)
{
    *t = s_type_char;
    typestr_add_array(t, lit->tok->tok_len + 1);
    t->c.is_const = 1;
    t->c.is_sym = 1;
    t->c.is_lit = 1;
    t->c.is_lvalue = 1;
    t->c.lit = lit;
}

static void elaborate_expr_ExprLit(struct Elaborator* elab,
                                   struct ElaborateDeclCtx* ctx,
                                   struct ExprLit* expr,
                                   struct TypeStr* rty)
{
#if defined(TRACING_ELAB)
    fprintf(stderr, " EXPR_LIT\n");
#endif
    *rty = s_type_unknown;
    if (expr->tok->type == LEX_NUMBER)
    {
        int x = 0;
        for (; x < 6; ++x)
        {
            if (expr->numeric <= s_mint_maximums[x]) break;
        }
        int min = expr->suffix & 6;
        if (x < min) x = min;

        unsigned char byte;
        if (expr->suffix & LIT_SUFFIX_MASK_UNSIGNED)
        {
            byte = s_uint_types[x];
        }
        else if (expr->suffix & LIT_SUFFIX_NONE_DECIMAL)
        {
            byte = s_sint_types[x];
        }
        else
        {
            byte = s_mint_types[x];
        }

        typestr_from_numlit(rty, byte, expr->numeric);
    }
    else if (expr->tok->type == LEX_CHARLIT)
    {
        typestr_from_numlit(rty, TYPE_BYTE_INT, expr->numeric);
    }
    else if (expr->tok->type == LEX_STRING)
    {
        typestr_from_strlit(rty, expr);
    }
    else
    {
        parser_tok_error(expr->tok, "error: unknown literal type: %s\n", lexstate_to_string(expr->tok->type));
    }
}

static void elaborate_expr_ExprCall(struct Elaborator* elab,
                                    struct ElaborateDeclCtx* ctx,
                                    struct ExprCall* expr,
                                    struct TypeStr* rty)
{
    elaborate_expr(elab, ctx, expr->fn, rty);
    struct TypeStr orig_fty = *rty;

    FnTypeInfo fn_info = typestr_strip_fn(elab->types, rty);
    if (typestr_is_unknown(rty))
    {
        typestr_error1(&expr->tok->rc, elab->types, "error: expected function type but got '%.*s'\n", &orig_fty);
    }

    CallParam* params = elab->p->callparams.data;
    struct TypeStr arg_expr_ty;
    for (size_t i = 0; i < expr->param_extent; ++i)
    {
        CallParam* const param = params + expr->param_offset + i;
        struct Expr* arg_expr = param->expr;
        if (arg_expr == NULL) abort();
        elaborate_expr(elab, ctx, arg_expr, &arg_expr_ty);
        struct TypeStr orig_arg_expr_ty = arg_expr_ty;
        typestr_decay(&arg_expr_ty);
        if (i < fn_info.extent)
        {
            const struct TypeStr* orig_tt_arg = typestr_get_arg(elab->types, &fn_info, i);
            typestr_implicit_conversion(
                elab->types, arg_expr->tok ? &arg_expr->tok->rc : NULL, &arg_expr_ty, orig_tt_arg);
            param->sizing = typestr_calc_sizing(elab->types, orig_tt_arg, arg_expr->tok ? &arg_expr->tok->rc : NULL);
            param->align = typestr_get_align(elab->types, orig_tt_arg);
        }
        else
        {
            // varargs
            unsigned int lhs_mask = typestr_mask(&arg_expr_ty);
            if (!(lhs_mask & TYPE_MASK_SCALAR))
            {
                typestr_error1(&arg_expr->tok->rc,
                               elab->types,
                               "error: expected scalar type in variadic arguments but got '%.*s'\n",
                               &orig_arg_expr_ty);
            }
            param->sizing.is_signed = 0;
            param->sizing.width = 8;
            param->align = 8;
        }
    }

    if (expr->param_extent < fn_info.extent)
    {
        parser_tok_error(expr->tok,
                         "error: too few arguments in function call: got %zu but expected %zu\n",
                         expr->param_extent,
                         fn_info.extent);
    }
    else if (!fn_info.is_variadic && expr->param_extent > fn_info.extent)
    {
        parser_tok_error(expr->tok,
                         "error: too many arguments in function call: got %zu but expected %zu\n",
                         expr->param_extent,
                         fn_info.extent);
    }
}

static void elaborate_expr_ExprField(struct Elaborator* elab,
                                     struct ElaborateDeclCtx* ctx,
                                     struct ExprField* e,
                                     struct TypeStr* rty)
{
#if defined(TRACING_ELAB)
    fprintf(stderr, " EXPR_FIELD\n");
#endif
    elaborate_expr(elab, ctx, e->lhs, rty);
    if (typestr_is_unknown(rty))
    {
        parser_tok_error(e->lhs->tok, "error: unable to elaborate expression\n");
        return;
    }
    const struct TypeStr orig_lhs = *rty;
    if (e->is_arrow) typestr_dereference(rty);
    unsigned int cvr_mask = typestr_strip_cvr(rty);
    TypeSymbol* sym = typestr_get_decl(elab->types, rty);
    if (sym)
    {
        if (sym->def)
        {
            // find field in decl
            Symbol* field = find_field_by_name(sym, e->fieldname, &e->field_offset);
            if (field)
            {
                e->sym = field;
                typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, rty, field->def);
                typestr_add_cvr(rty, cvr_mask);
            }
            else
            {
                DeclSpecs* first_spec = sym->last_decl;
                while (first_spec->prev_decl)
                    first_spec = first_spec->prev_decl;

                struct Array buf = {0};
                typestr_fmt(elab->types, rty, &buf);
                array_push_byte(&buf, 0);
                parser_tok_error(e->tok, "error: could not find member '%s' in type '%s'\n", e->fieldname, buf.data);
                array_destroy(&buf);
                *rty = s_type_unknown;
            }
        }
        else
        {
            typestr_error1(&e->tok->rc, elab->types, "error: first argument was of incomplete type %.*s\n", rty);
            *rty = s_type_unknown;
        }
    }
    else
    {
        const char* err_fmt;
        if (e->is_arrow)
            err_fmt = "error: expected first argument to be pointer to struct or union type, but got "
                      "'%.*s'\n";
        else
            err_fmt = "error: expected first argument to be of struct or union type, but got '%.*s'\n";
        typestr_error1(&e->tok->rc, elab->types, err_fmt, &orig_lhs);
        *rty = s_type_unknown;
    }
}

static void elaborate_expr(struct Elaborator* elab,
                           struct ElaborateDeclCtx* ctx,
                           struct Expr* top_expr,
                           struct TypeStr* rty)
{
#if defined(TRACING_ELAB)
    if (top_expr->tok)
        fprintf(stderr,
                "{elaborate_expr(%s:%d:%d)\n",
                top_expr->tok->rc.file,
                top_expr->tok->rc.row,
                top_expr->tok->rc.col);
    else
        fprintf(stderr, "{elaborate_expr(?)\n");
#endif
    memset(rty, 0, sizeof(*rty));
    void* top = top_expr;
    switch (top_expr->kind)
    {
        case EXPR_LIT: elaborate_expr_ExprLit(elab, ctx, top, rty); break;
        case EXPR_REF:
        {
            struct ExprRef* esym = top;
            *rty = esym->sym->type;
            break;
        }
        case EXPR_CAST:
        {
            struct ExprCast* expr = top;
            TypeStr orig;
            elaborate_expr(elab, ctx, expr->expr, &orig);
            elaborate_declspecs(elab, expr->specs);
            elaborate_decl(elab, expr->type);
            typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, rty, expr->type);
            rty->c = orig.c;
            if (orig.c.is_const)
            {
                const unsigned orig_mask = typestr_mask(&orig);
                const unsigned rty_mask = typestr_mask(rty);
                if (rty_mask & orig_mask & TYPE_FLAGS_POINTER)
                {
                }
                else if (rty_mask & orig_mask & TYPE_FLAGS_INT)
                {
                    typestr_assign_constant_value(rty, orig.c.value);
                }
                else
                {
                    rty->c = s_not_constant;
                }
            }
            break;
        }
        case EXPR_CALL: elaborate_expr_ExprCall(elab, ctx, top, rty); break;
        case EXPR_FIELD: elaborate_expr_ExprField(elab, ctx, top, rty); break;
        case EXPR_BINOP: elaborate_expr_ExprBinOp(elab, ctx, top, rty); break;
        case EXPR_UNOP: elaborate_expr_ExprUnOp(elab, ctx, top, rty); break;
        case EXPR_BUILTIN: elaborate_expr_ExprBuiltin(elab, ctx, top, rty); break;
        default: parser_tok_error(NULL, "error: unknown expr kind: %s\n", ast_kind_to_string(top_expr->kind)); return;
    }

    top_expr->sizing = typestr_calc_sizing(elab->types, rty, top_expr->tok ? &top_expr->tok->rc : NULL);
#if defined(TRACING_ELAB)
    fprintf(stderr, "}\n");
#endif
}

static __forceinline size_t round_to_alignment(size_t size, size_t align)
{
    size_t n = size + align - 1;
    return n - (n % align);
}

static void elaborate_decltype(Elaborator* elab, AstType* ast)
{
    ast->elaborated = 1;
    switch (ast->kind)
    {
        case AST_DECLARR:
        {
            struct DeclArr* arr = (void*)ast;
            elaborate_decltype(elab, arr->type);
            if (arr->arity != NULL)
            {
                TypeStr ts = {0};
                elaborate_expr(elab, NULL, arr->arity, &ts);
                arr->integer_arity = i32constant_or_err(&ts, arr->arity->tok);
                if (arr->integer_arity == 0)
                {
                    parser_tok_error(arr->arity->tok, "error: array must have positive extent\n");
                    arr->integer_arity = 1;
                }
            }
            break;
        }
        case AST_DECLFN:
        {
            struct DeclFn* fn = (void*)ast;
            elaborate_decltype(elab, fn->type);
            elaborate_stmts(elab, NULL, fn->offset, fn->extent);
            break;
        }
        case AST_DECLPTR:
        {
            struct DeclPtr* ptr = (void*)ast;
            elaborate_decltype(elab, ptr->type);
            break;
        }
        case AST_DECLSPEC: /* already elaborated */ break;
        default: parser_tok_error(ast->tok, "error: unimplemented decltype.\n"); break;
    }
}

static int elaborate_constinit(
    Elaborator* elab, size_t constinit_offset, size_t offset, size_t sz, const Ast* ast, uint8_t is_char_arr)
{
    int rc = 0;
    char* const bytes = elab->constinit.data + constinit_offset + offset;
    if (ast->kind == AST_INIT)
    {
        memset(bytes, 0, sz);
        AstInit* init = (void*)ast;
        while (init->init)
        {
            UNWRAP(elaborate_constinit(
                elab, constinit_offset, init->offset, init->sizing.width, init->init, init->is_char_arr));
            init = init->next;
        }
    }
    else
    {
        if (is_char_arr && ast->kind == EXPR_LIT && ast->tok->type == LEX_STRING)
        {
            ExprLit* lit = (void*)ast;
            size_t n = sz;
            if (n > ast->tok->tok_len + 1) n = ast->tok->tok_len + 1;
            memcpy(bytes, lit->text, n);
            if (sz > n)
            {
                memset(bytes + n, 0, sz - n);
            }
        }
        else if (sz > 8)
        {
            return parser_tok_error(ast->tok, "error: cannot constant initialize large member from expression\n");
        }
        else
        {
            TypeStr ty = {0};
            elaborate_expr(elab, NULL, (Expr*)ast, &ty);
            if (!ty.c.is_const) return parser_tok_error(ast->tok, "error: expected constant expression\n");
            memcpy(bytes, &ty.c.value.lower, sz);
            if (sz == 8)
            {
                if (ty.c.is_lvalue && ty.c.is_sym && ty.c.is_lit)
                {
                    memcpy(elab->constinit_bases.data + constinit_offset + offset, &ty.c.lit, 8);
                }
            }
            else
            {
                if (ty.c.is_lvalue && ty.c.is_sym && ty.c.is_lit)
                {
                    return parser_tok_error(ast->tok, "error: expected integral constant expression\n");
                }
            }
        }
    }
fail:
    return rc;
}

static int elaborate_decl(struct Elaborator* elab, struct Decl* decl)
{
    int rc = 0;
    decl->elaborated = 1;
    elaborate_decltype(elab, decl->type);
    Symbol* const sym = decl->sym;
    if (!sym)
    {
        rc = parser_has_errors();
        goto fail;
    }
    if (!decl->prev_decl)
    {
        typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, &sym->type, decl);

        unsigned int t = typestr_mask(&sym->type);
        sym->is_array_or_fn = !!(t & TYPE_MASK_FN_ARR);
        sym->is_aggregate = !!(t & TYPE_MASK_AGGREGATE);
    }
    if (sym->def == decl)
    {
        // Refresh symbol's type with concrete definition information
        typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, &sym->type, decl);

        if (!decl->specs->is_typedef)
        {
            const char tyb = typestr_byte(&sym->type);
            if (tyb == TYPE_BYTE_FUNCTION)
            {
                if (!decl->init) abort();
                if (decl->type->kind != AST_DECLFN) abort();
                TypeStr ts = sym->type;
                typestr_pop_offset(&ts);
                sym->fn_ret_sizing = typestr_calc_sizing(elab->types, &ts, &decl->tok->rc);
            }
            if (decl->init && !sym->is_enum_constant)
            {
                Decl* prev = elab->cur_decl;
                elab->cur_decl = decl;
                elaborate_init(elab, 0, decl, decl->init);
                elab->cur_decl = prev;
            }
            if (tyb == TYPE_BYTE_UNK_ARRAY)
            {
                typestr_remove_array(&sym->type);
                const size_t elem_size = typestr_get_size(elab->types, &sym->type, &decl->tok->rc);
                if (!decl->init)
                {
                    UNWRAP(parser_tok_error(
                        decl->tok, "error: definition of object with unknown array bounds must have an initializer\n"));
                }
                else if (decl->init->kind == AST_INIT)
                {
                    uint32_t max_assign = 0;
                    for (AstInit* i = (AstInit*)decl->init; i->init; i = i->next)
                    {
                        uint32_t new_max = i->offset / elem_size;
                        if (new_max > max_assign) max_assign = new_max;
                    }
                    if (max_assign == UINT32_MAX) abort();
                    typestr_append_offset(&sym->type, max_assign + 1, TYPE_BYTE_ARRAY);
                }
                else if (decl->init->kind == EXPR_LIT)
                {
                    struct ExprLit* l = (void*)decl->init;
                    typestr_append_offset(&sym->type, strlen(l->text) + 1, TYPE_BYTE_ARRAY);
                }
                else
                {
                    UNWRAP(parser_tok_error(
                        decl->init->tok,
                        "error: array initializer must be either a string literal or an initializer list\n"));
                }
            }

            sym->size =
                typestr_calc_sizing(elab->types, &sym->type, decl->tok ? &decl->tok->rc : &decl->specs->tok->rc);
            sym->align = typestr_get_align(elab->types, &sym->type);

            if (sym->size.width == 0)
            {
                /* type may be incomplete */
                if (decl->specs->is_extern)
                {
                    /* it's extern -- OK */
                }
                else if (decl->specs->is_fn_arg && !((struct Decl*)decl->specs->parent)->init)
                {
                    /* arg of function prototype -- OK */
                }
                else
                {
                    parser_tok_error(decl->tok, "error: definition of object with incomplete size\n");
                    return 0;
                }
            }

            UNWRAP(parser_has_errors());

            if ((!(struct Decl*)decl->specs->parent || decl->specs->is_static) && !decl->specs->is_extern &&
                tyb != TYPE_BYTE_FUNCTION && decl->init)
            {
                // global object with initializer -- constinit
                sym->constinit_offset = elab->constinit.sz;
                array_push_zeroes(&elab->constinit, round_to_alignment(sym->size.width, 8));
                array_push_zeroes(&elab->constinit_bases, round_to_alignment(sym->size.width, 8));
                UNWRAP(elaborate_constinit(
                    elab, sym->constinit_offset, 0, sym->size.width, decl->init, sym->is_aggregate));
            }
        }
    }
    UNWRAP(parser_has_errors());

fail:
    return rc;
}

static int elaborate_declspecs(struct Elaborator* elab, struct DeclSpecs* specs)
{
    int rc = 0;
    specs->elaborated = 1;
    if (specs->_typedef)
    {
    }
    else if (specs->is_enum || specs->is_union || specs->is_struct)
    {
        if (!specs->prev_decl)
        {
            specs->sym->idx = tt_register(elab->types, specs->sym);
            if (specs->is_enum)
            {
                specs->sym->size.is_signed = 1;
                specs->sym->size.width = 4;
                specs->sym->align = 4;
            }
        }

        if (specs->enum_init || specs->suinit)
        {
            if (specs->sym->def)
            {
                parser_tok_error(specs->tok, "error: multiple definitions of type.\n");
                UNWRAP(parser_tok_error(specs->sym->def->tok, "info: previous definition\n"));
            }
            specs->sym->def = specs;
        }

        if (specs->enum_init)
        {
            struct StmtDecls* block = specs->enum_init;
            struct Expr** const seqs = elab->p->expr_seqs.data;
            int enum_value = -1;
            for (size_t i = 0; i < block->extent; ++i)
            {
                if (seqs[i + block->offset]->kind != AST_DECL) abort();
                struct Decl* edecl = (struct Decl*)seqs[i + block->offset];
                UNWRAP(elaborate_decl(elab, edecl));
                if (edecl->init)
                {
                    if (!ast_kind_is_expr(edecl->init->kind))
                    {
                        UNWRAP(parser_tok_error(edecl->init->tok, "error: expected constant integer expression\n"));
                    }

                    TypeStr ts = {0};
                    elaborate_expr(elab, NULL, (Expr*)edecl->init, &ts);
                    enum_value = i32constant_or_err(&ts, edecl->init->tok);
                }
                else
                {
                    ++enum_value;
                }
                edecl->sym->enum_value = enum_value;
                typestr_assign_constant_value(&edecl->sym->type, mp_i64(enum_value));
            }
        }
        else if (specs->suinit)
        {
            // struct/union definition
            struct StmtBlock* block = specs->suinit;
            block->elaborated = 1;

            size_t struct_align = 1;
            size_t struct_size = 0;

            Symbol** p_next_decl = &specs->sym->first_member;

            struct Ast** const seqs = elab->p->expr_seqs.data;
            for (size_t i = 0; i < block->extent; ++i)
            {
                if (seqs[i + block->offset]->kind != STMT_DECLS) abort();
                struct StmtDecls* decls = (struct StmtDecls*)seqs[i + block->offset];
                UNWRAP(elaborate_declspecs(elab, decls->specs));
                for (size_t j = 0; j < decls->extent; ++j)
                {
                    if (seqs[decls->offset + j]->kind != AST_DECL) abort();
                    struct Decl* field = (struct Decl*)seqs[decls->offset + j];
                    UNWRAP(elaborate_decl(elab, field));
                    *p_next_decl = field->sym;
                    p_next_decl = &field->sym->next_field;
                    if (field->type || !field->sym->name)
                    {
                        if (field->init && field->sym->name)
                        {
                            return parser_tok_error(field->tok,
                                                    "error: structure and union fields cannot have initializers\n");
                        }
                        // insert padding
                        struct_size = round_to_alignment(struct_size, field->sym->align);
                        if (specs->is_struct)
                        {
                            field->sym->field_offset = struct_size;
                            struct_size += field->sym->size.width;
                        }
                        else
                        {
                            field->sym->field_offset = 0;
                            if (field->sym->size.width > struct_size) struct_size = field->sym->size.width;
                        }
                        if (struct_align < field->sym->align) struct_align = field->sym->align;
                    }
                }
            }

            if (!specs->sym->first_member)
            {
                return parser_tok_error(block->tok, "error: structures must have at least one member field\n");
            }

            if (struct_size == 0) struct_size = 1;
            struct_size = round_to_alignment(struct_size, struct_align);
            specs->sym->align = struct_align;
            specs->sym->size.width = struct_size;
        }
    }

fail:

    return rc;
}

void elaborator_init(struct Elaborator* elab, struct Parser* p)
{
    memset(elab, 0, sizeof(struct Elaborator));
    elab->p = p;
    elab->types = tt_alloc();
}

int elaborate(struct Elaborator* elab)
{
    struct Parser* const p = elab->p;
    elaborate_stmt(elab, NULL, &p->top->ast);
    return parser_has_errors();
}

void elaborator_destroy(struct Elaborator* elab)
{
    tt_free(elab->types);
    array_destroy(&elab->constinit);
    array_destroy(&elab->constinit_bases);
}
