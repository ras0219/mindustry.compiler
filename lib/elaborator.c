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
#include "seqview.h"
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
        if (!ts->c.is_lvalue && !ts->c.sym)
        {
            return mp_i32(ts->c.value);
        }
    }
    parser_tok_error(rc, "error: expected integer constant expression\n");
    return 0;
}
static uint64_t u64constant_or_err(const TypeStr* ts, const Token* rc)
{
    if (ts->c.is_const)
    {
        if (!ts->c.is_lvalue && !ts->c.sym)
        {
            return mp_u64(ts->c.value);
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
static void elaborate_stmt(struct Elaborator* elab, struct Ast* ast);
static void elaborate_expr(struct Elaborator* elab, struct Expr* top_expr, struct TypeStr* rty);
static void elaborate_expr_decay(struct Elaborator* elab, struct Expr* top_expr, struct TypeStr* rty);
static void elaborate_expr_lvalue(struct Elaborator* elab, struct Expr* top_expr, struct TypeStr* rty);

enum
{
    BINOP_FLAGS_INT = 1 << 0,
    BINOP_FLAGS_ARITH = 1 << 1,
    BINOP_FLAGS_SCALAR = 1 << 2,
    BINOP_FLAGS_ASSIGN = 1 << 3,
    BINOP_FLAGS_INTPROMO = 1 << 4,
    BINOP_FLAGS_COMMONTYPE = 1 << 5,
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
        case TOKEN_SYM2('%', '='):
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
        case TOKEN_SYM1('>'): return BINOP_FLAGS_INTPROMO | BINOP_FLAGS_SCALAR;
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

static void elaborate_expr_ExprTernary(struct Elaborator* elab, ExprTernary* e, struct TypeStr* rty)
{
    TypeStr cond_ty, etrue_ty, efalse_ty;
    elaborate_expr_decay(elab, e->cond, &cond_ty);
    elaborate_expr_decay(elab, e->iftrue, &etrue_ty);
    elaborate_expr_decay(elab, e->iffalse, &efalse_ty);
    const unsigned int cond_mask = typestr_mask(&cond_ty);
    const unsigned int etrue_mask = typestr_mask(&etrue_ty);
    const unsigned int efalse_mask = typestr_mask(&efalse_ty);
    if (!(cond_mask & TYPE_MASK_SCALAR))
    {
        typestr_error1(
            &e->tok->rc, elab->types, "error: expected scalar type in first argument but got '%.*s'\n", &cond_ty);
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
        else if (typestr_match(&efalse_ty, &s_type_void))
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
                           &etrue_ty,
                           &efalse_ty);
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
                       &etrue_ty,
                       &efalse_ty);
        *rty = s_type_unknown;
    }

    if (cond_ty.c.is_const && !cond_ty.c.is_lvalue && efalse_ty.c.is_const && !efalse_ty.c.is_lvalue &&
        etrue_ty.c.is_const && !etrue_ty.c.is_lvalue)
    {
        if (!cond_ty.c.sym && !cond_ty.c.value.lower)
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

static int cnst_same_base(Constant c1, Constant c2) { return c1.sym == c2.sym; }
static int cnst_eq(Constant c1, Constant c2) { return cnst_same_base(c1, c2) && mp_is_eq(c1.value, c2.value); }
static int cnst_truthy(Constant c1) { return c1.sym || mp_is_nonzero(c1.value); }

static void elaborate_expr_ExprBinOp_impl(
    Elaborator* elab, const RowCol* rc, TypeStr* rty, TypeStr* rhs, unsigned int op)
{
    const unsigned int binmask = binop_mask(op);
    unsigned int lhs_mask = typestr_mask(rty);
    unsigned int rhs_mask = typestr_mask(rhs);

    // check lhs mask
    if (binmask & BINOP_FLAGS_INT)
    {
        if (!(lhs_mask & TYPE_FLAGS_INT))
        {
            typestr_error1(rc, elab->types, "error: expected integer type in first argument but got '%.*s'\n", rty);
            *rty = s_type_int;
            return;
        }
        if (!(rhs_mask & TYPE_FLAGS_INT))
        {
            typestr_error1(rc, elab->types, "error: expected integer type in second argument but got '%.*s'\n", rhs);
            *rty = s_type_int;
            return;
        }
    }
    else if (binmask & BINOP_FLAGS_ARITH)
    {
        if (!(lhs_mask & TYPE_MASK_ARITH))
        {
            typestr_error1(rc, elab->types, "error: expected arithmetic type in first argument but got '%.*s'\n", rty);
            *rty = s_type_int;
            return;
        }
        if (!(rhs_mask & TYPE_MASK_ARITH))
        {
            typestr_error1(rc, elab->types, "error: expected arithmetic type in second argument but got '%.*s'\n", rhs);
            *rty = s_type_int;
            return;
        }
    }
    else if (binmask & BINOP_FLAGS_SCALAR)
    {
        if (!(lhs_mask & TYPE_MASK_SCALAR))
        {
            typestr_error1(rc, elab->types, "error: expected scalar type in first argument but got '%.*s'\n", rty);
            *rty = s_type_int;
            return;
        }
        if ((binmask & BINOP_FLAGS_SCALAR) && !(rhs_mask & TYPE_MASK_SCALAR))
        {
            typestr_error1(rc, elab->types, "error: expected scalar type in second argument but got '%.*s'\n", rhs);
            *rty = s_type_int;
            return;
        }
    }

    // perform op-specifics
    if (binmask & BINOP_FLAGS_INTPROMO)
    {
        typestr_promote_integer(rty);
        typestr_promote_integer(rhs);
        if (!((rhs_mask | lhs_mask) & TYPE_FLAGS_POINTER))
        {
            promote_common_type(rty, rhs);
        }
    }
    switch (op)
    {
        case TOKEN_SYM1('&'):
        case TOKEN_SYM1('|'):
        case TOKEN_SYM1('^'):
        case TOKEN_SYM1('*'):
        case TOKEN_SYM1('%'):
        case TOKEN_SYM2('<', '<'):
        case TOKEN_SYM1('/'):
        case TOKEN_SYM2('>', '>'): break;
        case TOKEN_SYM1('>'):
        case TOKEN_SYM1('<'):
        case TOKEN_SYM2('=', '='):
        case TOKEN_SYM2('!', '='):
        case TOKEN_SYM2('>', '='):
        case TOKEN_SYM2('<', '='):
        case TOKEN_SYM2('&', '&'):
        case TOKEN_SYM2('|', '|'): rty->buf = s_type_int.buf; break;
        case TOKEN_SYM1(','): *rty = *rhs; break;
        default:;
            const char o1 = TOKEN_GET_SYM1(op), o2 = TOKEN_GET_SYM2(op), o3 = TOKEN_GET_SYM3(op);
            fprintf(stderr, "warning: untyped binary operator '%c%c%c'\n", o1 ? o1 : ' ', o2 ? o2 : ' ', o3 ? o3 : ' ');
            *rty = s_type_unknown;
            break;
    }
}

static void elaborate_expr_ExprAdd_impl(
    Elaborator* elab, const RowCol* rc, TypeStr* rty, TypeStr* rhs, unsigned int op, int* info)
{
    unsigned int lhs_mask = typestr_mask(rty);
    unsigned int rhs_mask = typestr_mask(rhs);

    // check lhs mask
    if (!(lhs_mask & TYPE_MASK_SCALAR))
    {
        typestr_error1(rc, elab->types, "error: expected scalar type in first argument but got '%.*s'\n", rty);
        *rty = s_type_int;
        return;
    }
    if (!(rhs_mask & TYPE_MASK_SCALAR))
    {
        typestr_error1(rc, elab->types, "error: expected scalar type in second argument but got '%.*s'\n", rhs);
        *rty = s_type_int;
        return;
    }

    // perform op-specifics
    typestr_promote_integer(rty);
    typestr_promote_integer(rhs);
    if (!((rhs_mask | lhs_mask) & TYPE_FLAGS_POINTER))
    {
        promote_common_type(rty, rhs);
    }
    if (op != TOKEN_SYM1('-'))
    {
        // '+' or '['
        if (rhs_mask & lhs_mask & TYPE_FLAGS_POINTER)
        {
            typestr_error2(
                rc, elab->types, "error: expected only one pointer type, but got '%.*s' and '%.*s'\n", rty, rhs);
        }
        *info = typestr_get_add_size(elab->types, rty, rc);
    }
    else
    {
        *info = typestr_get_add_size(elab->types, rty, rc);
        if (rhs_mask & TYPE_FLAGS_POINTER)
        {
            if (!(lhs_mask & TYPE_FLAGS_POINTER))
            {
                typestr_error2(
                    rc, elab->types, "error: expected both pointer types, but got '%.*s' and '%.*s'\n", rty, rhs);
            }
            *info = -*info;
            rty->buf = s_type_ptrdiff.buf;
        }
    }
}

static void elaborate_expr_ExprAssign(struct Elaborator* elab, struct ExprAssign* e, struct TypeStr* rty)
{
    const RowCol* const rc = &e->tok->rc;
    elaborate_expr_lvalue(elab, e->lhs, rty);
    typestr_dereference(rty);
    const struct TypeStr orig_lhs = *rty;
    if (typestr_is_const(rty))
    {
        typestr_error1(rc, elab->types, "error: assignment to const object of type '%.*s'\n", rty);
    }
    struct TypeStr rhs_ty;
    elaborate_expr_decay(elab, e->rhs, &rhs_ty);
    int op = e->tok->type;
    switch (op)
    {
        case TOKEN_SYM1('='): typestr_implicit_conversion(elab->types, rc, &rhs_ty, rty); break;
        case TOKEN_SYM2('+', '='):
        case TOKEN_SYM2('-', '='):
            if (typestr_mask(&rhs_ty) & TYPE_FLAGS_POINTER)
            {
                typestr_error1(
                    rc, elab->types, "error: expected integer type on right-hand side, but got '%.*s'\n", &rhs_ty);
            }
            elaborate_expr_ExprAdd_impl(elab, rc, rty, &rhs_ty, TOKEN_SLICE_SYM1(op), &e->mult);
            break;
        case TOKEN_SYM2('*', '='):
        case TOKEN_SYM2('%', '='):
        case TOKEN_SYM2('/', '='):
        case TOKEN_SYM2('^', '='):
        case TOKEN_SYM2('&', '='):
        case TOKEN_SYM2('|', '='): elaborate_expr_ExprBinOp_impl(elab, rc, rty, &rhs_ty, TOKEN_SLICE_SYM1(op)); break;
        case TOKEN_SYM3('<', '<', '='):
        case TOKEN_SYM3('>', '>', '='):
            elaborate_expr_ExprBinOp_impl(elab, rc, rty, &rhs_ty, TOKEN_SLICE_SYM2(op));
            break;
        default: abort();
    }
    e->is_signed = typestr_calc_sizing(elab->types, &rhs_ty, rc).is_signed;
    *rty = orig_lhs;
}

static void elaborate_expr_ExprAdd(Elaborator* elab, ExprAdd* e, TypeStr* rty)
{
#if defined(TRACING_ELAB)
    fprintf(stderr, "elaborate_expr_ExprAdd\n");
#endif
    const unsigned op = e->tok->type;
    elaborate_expr_decay(elab, e->lhs, rty);
    unsigned int lhs_mask = typestr_mask(rty);
    struct TypeStr rhs_ty;
    elaborate_expr_decay(elab, e->rhs, &rhs_ty);
    unsigned int rhs_mask = typestr_mask(&rhs_ty);
    if ((op == TOKEN_SYM1('+') || op == TOKEN_SYM1('[')) && (rhs_mask & TYPE_FLAGS_POINTER))
    {
        Expr* tmp = e->lhs;
        e->lhs = e->rhs;
        e->rhs = tmp;
        unsigned int tmp_mask = lhs_mask;
        lhs_mask = rhs_mask;
        rhs_mask = tmp_mask;
        TypeStr tmp_ty = *rty;
        *rty = rhs_ty;
        rhs_ty = tmp_ty;
    }
    elaborate_expr_ExprAdd_impl(elab, &e->tok->rc, rty, &rhs_ty, op, &e->mult);
    if (rty->c.is_const && rhs_ty.c.is_const && !rty->c.is_lvalue && !rhs_ty.c.is_lvalue)
    {
        if (op != TOKEN_SYM1('-'))
        {
            typestr_assign_constant_value(rty, mp_fma(rty->c.value, rhs_ty.c.value, e->mult));
        }
        else
        {
            if (e->mult < 0)
            {
                if (rty->c.sym != rhs_ty.c.sym)
                {
                    parser_tok_error(e->tok,
                                     "error: subtracting two pointers from different aggregates is "
                                     "undefined behavior.\n");
                    return;
                }
                rty->c.sym = NULL;
                typestr_assign_constant_value(rty, mp_idiv(mp_sub(rty->c.value, rhs_ty.c.value), -e->mult, e->tok));
            }
            else if (e->mult == 0)
                abort();
            else
            {
                rty->c.value = mp_fsm(rty->c.value, rhs_ty.c.value, e->mult);
            }
        }
    }
    else
    {
        rty->c = s_not_constant;
    }
}

static void elaborate_expr_ExprBinOp(struct Elaborator* elab, struct ExprBinOp* e, struct TypeStr* rty)
{
#if defined(TRACING_ELAB)
    fprintf(stderr, "elaborate_expr_ExprBinOp\n");
#endif
    const RowCol* const rc = &e->tok->rc;
    const unsigned op = e->tok->type;
    elaborate_expr_decay(elab, e->lhs, rty);
    struct TypeStr rhs_ty;
    elaborate_expr_decay(elab, e->rhs, &rhs_ty);
    elaborate_expr_ExprBinOp_impl(elab, rc, rty, &rhs_ty, op);
    e->is_signed = typestr_calc_sizing(elab->types, &rhs_ty, rc).is_signed;
    if (rty->c.is_const && rhs_ty.c.is_const && !rty->c.is_lvalue && !rhs_ty.c.is_lvalue)
    {
        switch (op)
        {
            case TOKEN_SYM2('=', '='): typestr_assign_constant_bool(rty, cnst_eq(rty->c, rhs_ty.c)); break;
            case TOKEN_SYM2('!', '='): typestr_assign_constant_bool(rty, !cnst_eq(rty->c, rhs_ty.c)); break;
            case TOKEN_SYM2('&', '&'):
                typestr_assign_constant_bool(rty, cnst_truthy(rty->c) && cnst_truthy(rhs_ty.c));
                break;
            case TOKEN_SYM2('|', '|'):
                typestr_assign_constant_bool(rty, cnst_truthy(rty->c) || cnst_truthy(rhs_ty.c));
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
            default: rty->c = s_not_constant; break;
        }
    }
    else
    {
        rty->c = s_not_constant;
    }
}

static const TypeStr s_valist_ptr = {.buf = {2, TYPE_BYTE_UUVALIST, TYPE_BYTE_POINTER}};

static void elaborate_expr_ExprBuiltin(struct Elaborator* elab, struct ExprBuiltin* e, struct TypeStr* rty)
{
    switch (e->tok->type)
    {
        case LEX_SIZEOF:
            if (e->expr1)
            {
                elaborate_expr(elab, e->expr1, rty);
            }
            else
            {
                elaborate_declspecs(elab, e->specs);
                elaborate_decl(elab, e->type);
                typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, rty, e->type);
            }
            e->sizeof_size = typestr_get_size(elab->types, rty, &e->tok->rc);
            *rty = s_type_int;
            typestr_assign_constant_value(rty, mp_from_u64(e->sizeof_size));
            break;
        case LEX_UUVA_START:
            elaborate_expr(elab, e->expr2, rty);
            elaborate_expr_lvalue(elab, e->expr1, rty);
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
            elaborate_expr_decay(elab, e->expr1, rty);
            elaborate_declspecs(elab, e->specs);
            elaborate_decl(elab, e->type);
            typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, rty, e->type);
            break;
        case LEX_UUVA_END: *rty = s_type_void; break;
        case LEX_UUVA_COPY:
            elaborate_expr_decay(elab, e->expr1, rty);
            elaborate_expr_decay(elab, e->expr2, rty);
            *rty = s_type_void;
            break;
        case LEX_BUILTIN_CONSTANT_P:
            *rty = s_type_int;
            rty->c.is_const = 1;
            rty->c.value = s_zero_constant;
            break;
        case LEX_BUILTIN_BSWAP32:
        case LEX_BUILTIN_BSWAP64:;
            TypeStr ty;
            elaborate_expr(elab, e->expr1, &ty);
            if (e->tok->type == LEX_BUILTIN_BSWAP32)
                *rty = s_type_uint;
            else
                *rty = s_type_ulong;
            typestr_implicit_conversion(elab->types, &e->tok->rc, &ty, rty);
            break;
        default:
            parser_tok_error(e->tok, "error: unimplemented builtin\n");
            *rty = s_type_unknown;
            break;
    }
}

static void elaborate_expr_ExprUnOp(struct Elaborator* elab, struct ExprUnOp* e, struct TypeStr* rty)
{
    elaborate_expr_decay(elab, e->lhs, rty);
    const struct TypeStr orig_lhs = *rty;
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
        case TOKEN_SYM1('~'):
            if (!(lhs_mask & TYPE_FLAGS_INT))
            {
                typestr_error1(&e->tok->rc,
                               elab->types,
                               "error: expected integer type in first argument but got '%.*s'\n",
                               &orig_lhs);
            }
            typestr_apply_integral_type(rty, &s_type_int);
            break;
        case TOKEN_SYM1('!'):
            if (!(lhs_mask & TYPE_MASK_SCALAR))
            {
                typestr_error1(&e->tok->rc,
                               elab->types,
                               "error: expected scalar type in first argument but got '%.*s'\n",
                               &orig_lhs);
            }
            typestr_apply_integral_type(rty, &s_type_int);
            break;
        default:
            fprintf(stderr, "warning: untyped unary operator '%s'\n", token_str(elab->p, e->tok));
            *rty = s_type_unknown;
            break;
    }

    if (rty->c.is_const)
    {
        if (!rty->c.is_lvalue && !rty->c.sym)
        {
            switch (e->tok->type)
            {
                case TOKEN_SYM1('~'): rty->c.value = mp_bnot(rty->c.value); break;
                case TOKEN_SYM1('!'): rty->c.value = mp_lnot(rty->c.value); break;
                case TOKEN_SYM1('+'): break;
                case TOKEN_SYM1('-'): rty->c.value = mp_neg(rty->c.value); break;
            }
        }
        else
        {
            rty->c = s_not_constant;
        }
    }
}
static void elaborate_expr_ExprDeref(struct Elaborator* elab, struct ExprDeref* e, struct TypeStr* rty)
{
    elaborate_expr_decay(elab, e->lhs, rty);
    if (typestr_is_pointer(rty))
    {
        typestr_dereference(rty);
    }
    else
    {
        typestr_error1(&e->tok->rc, elab->types, "error: cannot dereference value of type '%.*s'\n", rty);
        *rty = s_type_unknown;
    }
}
static void elaborate_expr_lvalue_ExprDeref(Elaborator* elab, ExprDeref* e, TypeStr* rty)
{
    elaborate_expr_decay(elab, e->lhs, rty);
    e->take_address = 1;
}
static void elaborate_expr_ExprAddress(struct Elaborator* elab, struct ExprAddress* e, struct TypeStr* rty)
{
    elaborate_expr_lvalue(elab, e->lhs, rty);
}
static void elaborate_expr_ExprIncr(struct Elaborator* elab, struct ExprIncr* e, struct TypeStr* rty)
{
    elaborate_expr_lvalue(elab, e->lhs, rty);
    typestr_dereference(rty);
    const struct TypeStr orig_lhs = *rty;
    unsigned int lhs_mask = typestr_mask(rty);
    if (!(lhs_mask & TYPE_MASK_SCALAR))
    {
        typestr_error1(
            &e->tok->rc, elab->types, "error: expected scalar type in first argument but got '%.*s'\n", &orig_lhs);
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
    rty->c = s_not_constant;
}
static void elaborate_stmts(struct Elaborator* elab, SeqView stmts)
{
    struct Ast** seqs = elab->p->expr_seqs.data;
    for (size_t i = 0; i < stmts.ext; ++i)
    {
        elaborate_stmt(elab, seqs[stmts.off + i]);
    }
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
    frame->ty = *parent_ty;
    typestr_strip_cvr(&frame->ty);
    char b = frame->ty.buf.buf[frame->ty.buf.buf[0]];
    switch (b)
    {
        case TYPE_BYTE_UNK_ARRAY:
        case TYPE_BYTE_ARRAY:
        {
            frame->is_array = 1;
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
                elaborate_expr(elab, designator->array_expr, &ts);
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
            TypeSymbol* sym = typestr_get_decl(elab->types, &frame->ty);
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
    if (typestr_is_char_array(dty) && init->is_braced_strlit)
    {
        TypeStr ts;
        elaborate_expr(elab, (Expr*)init->init, &ts);
        return;
    }

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
        const RowCol* const rc = init->init->tok ? &init->init->tok->rc : NULL;
        DInitFrame* back = array_back(&iter.stk, sizeof(*back));
        if (init->init->kind == AST_INIT)
        {
            init->offset = back->offset;
            init->sizing = typestr_calc_sizing(elab->types, &back->ty, rc);
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
            elaborate_expr(elab, expr, &ts);
            ts_decay = ts;
            const int ts_decay_addr_taken = typestr_decay(&ts_decay);
            const int ts_is_strlit = expr->kind == EXPR_LIT && expr->tok->type == LEX_STRING;

            while (typestr_is_aggregate(&back->ty))
            {
                if (typestr_match(&back->ty, &ts)) break;
                if (ts_is_strlit && typestr_is_char_array(&back->ty)) goto skip_conversion;
                if (typestr_match(&back->ty, &ts_decay))
                {
                    expr->take_address = ts_decay_addr_taken;
                    break;
                }
                if (di_enter(&iter, elab, &init->tok->rc)) goto fail;
                back = array_back(&iter.stk, sizeof(*back));
            }
            typestr_implicit_conversion(elab->types, rc, &ts_decay, &back->ty);
            expr->take_address = ts_decay_addr_taken;
        skip_conversion:
            init->is_aggregate_init = typestr_is_aggregate(&back->ty);
            init->offset = back->offset;
            init->sizing = typestr_calc_sizing(elab->types, &back->ty, rc);
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

static void elaborate_init_ty(struct Elaborator* elab, size_t offset, const TypeStr* dty, Constant* c, struct Ast* ast)
{
    if (ast->kind == AST_INIT)
    {
        return elaborate_init_ty_AstInit(elab, offset, dty, (struct AstInit*)ast);
    }
    const char tyb = typestr_byte(dty);
    switch (tyb)
    {
        case TYPE_BYTE_FUNCTION: elaborate_stmt(elab, ast); break;
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
            elaborate_expr(elab, (Expr*)ast, &ts);
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
            elaborate_expr_decay(elab, expr, &ts);
            typestr_implicit_conversion(elab->types, ast->tok ? &ast->tok->rc : NULL, &ts, dty);
            if (typestr_is_const(dty))
            {
                *c = ts.c;
            }
        }
    }
}

#define DISPATCH(X, Y)                                                                                                 \
    case AST_KIND_##Y:                                                                                                 \
        X##Y(elab, (struct Y*)ast);                                                                                    \
        break

#define DISPATCH_STMT(Y) DISPATCH(elaborate_stmt_, Y)

static void elaborate_stmt_StmtCase(struct Elaborator* elab, struct StmtCase* stmt)
{
    if (stmt->expr)
    {
        TypeStr ts = {0};
        elaborate_expr(elab, stmt->expr, &ts);
        stmt->value = u64constant_or_err(&ts, stmt->expr->tok);
    }
}

static void elaborate_stmt(struct Elaborator* elab, struct Ast* ast)
{
    if (ast_kind_is_expr(ast->kind))
    {
        struct TypeStr ts;
        return elaborate_expr(elab, (struct Expr*)ast, &ts);
    }
    ast->elaborated = 1;
    void* top = ast;
    switch (ast->kind)
    {
        DISPATCH_STMT(StmtCase);
        case STMT_NONE:
        case STMT_BREAK:
        case STMT_CONTINUE:
        case STMT_GOTO: return;
        case STMT_LABEL:
        {
            struct StmtLabel* expr = top;
            return elaborate_stmt(elab, expr->stmt);
        }
        case STMT_RETURN:
        {
            struct StmtReturn* stmt = top;
            if (stmt->expr)
            {
                struct TypeStr ts;
                elaborate_expr_decay(elab, stmt->expr, &ts);
                struct TypeStr fn = elab->cur_decl->sym->type;
                typestr_pop_offset(&fn);
                typestr_implicit_conversion(elab->types, token_rc(stmt->tok), &ts, &fn);
            }

            return;
        }
        case STMT_IF:
        {
            struct StmtIf* stmt = top;
            struct TypeStr ts;
            elaborate_expr(elab, stmt->cond, &ts);
            elaborate_stmt(elab, stmt->if_body);
            if (stmt->else_body)
            {
                elaborate_stmt(elab, stmt->else_body);
            }
            return;
        }
        case STMT_LOOP:
        {
            struct StmtLoop* e = top;
            struct TypeStr ts;
            if (e->init) elaborate_stmt(elab, e->init);
            if (e->cond) elaborate_expr(elab, e->cond, &ts);
            if (e->advance) elaborate_expr(elab, e->advance, &ts);
            elaborate_stmt(elab, e->body);
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
            Decl* const* const decls = (void*)((void**)elab->p->expr_seqs.data + stmt->seq.off);
            for (size_t i = 0; i < stmt->seq.ext; ++i)
                elaborate_decl(elab, decls[i]);
            return;
        }
        case STMT_BLOCK:
        {
            struct StmtBlock* stmt = top;
            elaborate_stmts(elab, stmt->seq);
            return;
        }
        case STMT_SWITCH:
        {
            struct StmtSwitch* stmt = top;
            struct TypeStr ts;
            elaborate_expr_decay(elab, stmt->expr, &ts);
            if (!(typestr_mask(&ts) & TYPE_MASK_SCALAR))
            {
                typestr_error1(token_rc(stmt->tok),
                               elab->types,
                               "error: expected scalar type in switch condition but got '%.*s'\n",
                               &ts);
            }
            elaborate_stmts(elab, stmt->seq);
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
    typestr_assign_constant_value(t, mp_from_u64(numeric));
}

static void typestr_from_strlit(TypeStr* t, ExprLit* lit)
{
    Symbol* sym = lit->sym;
    if (sym->string_constant == lit)
    {
        sym->type = s_type_char;
        typestr_add_array(&sym->type, lit->tok->tok_len + 1);
        sym->type.c.is_const = 1;
        sym->type.c.is_lvalue = 1;
        sym->type.c.sym = sym;
    }
    *t = sym->type;
}

static void elaborate_expr_ExprLit(struct Elaborator* elab,

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

                                    struct ExprCall* expr,
                                    struct TypeStr* rty)
{
    elaborate_expr_decay(elab, expr->fn, rty);
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
        elaborate_expr_decay(elab, arg_expr, &arg_expr_ty);
        struct TypeStr orig_arg_expr_ty = arg_expr_ty;
        const RowCol* const rc = arg_expr->tok ? &arg_expr->tok->rc : NULL;
        if (i < fn_info.extent)
        {
            const struct TypeStr* orig_tt_arg = typestr_get_arg(elab->types, &fn_info, i);
            typestr_implicit_conversion(elab->types, rc, &arg_expr_ty, orig_tt_arg);
            param->sizing = typestr_calc_sizing(elab->types, orig_tt_arg, rc);
            param->align = typestr_get_align(elab->types, orig_tt_arg);
        }
        else
        {
            // varargs
            unsigned int lhs_mask = typestr_mask(&arg_expr_ty);
            if (!(lhs_mask & TYPE_MASK_SCALAR))
            {
                typestr_error1(rc,
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

static void elaborate_expr_impl(struct Elaborator* elab, struct Expr* top_expr, struct TypeStr* rty);

static void elaborate_expr(struct Elaborator* elab, struct Expr* top_expr, struct TypeStr* rty)
{
    elaborate_expr_impl(elab, top_expr, rty);
    if (rty->buf.buf[0])
    {
        top_expr->sizing = typestr_calc_sizing_zero_void(elab->types, rty, token_rc(top_expr->tok));
    }
    top_expr->c = rty->c;
    top_expr->elaborated = 1;
}

static void expr_addressof(Expr* e, TypeStr* ty)
{
    e->take_address = 1;
    typestr_addressof(ty);
}

static void elaborate_expr_lvalue_ExprRef(Elaborator* elab, ExprRef* e, TypeStr* rty)
{
    *rty = e->sym->type;
    expr_addressof(&e->expr_base, rty);
}
static void elaborate_expr_lvalue_ExprAssign(Elaborator* elab, ExprAssign* expr, TypeStr* rty)
{
    elaborate_expr_ExprAssign(elab, expr, rty);
    expr_addressof(&expr->expr_base, rty);
}

static void elaborate_expr_ExprField_lhs(struct Elaborator* elab, struct ExprField* e, struct TypeStr* rty)
{
    if (typestr_is_unknown(rty)) return;
    const struct TypeStr orig_lhs = *rty;
    typestr_dereference(rty);
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

static void elaborate_expr_ExprField(struct Elaborator* elab, struct ExprField* e, struct TypeStr* rty)
{
#if defined(TRACING_ELAB)
    fprintf(stderr, " EXPR_FIELD\n");
#endif

    if (e->is_arrow)
    {
        elaborate_expr_decay(elab, e->lhs, rty);
    }
    else
    {
        elaborate_expr(elab, e->lhs, rty);
        expr_addressof(e->lhs, rty);
    }
    elaborate_expr_ExprField_lhs(elab, e, rty);
}

static void elaborate_expr_lvalue_ExprField(Elaborator* elab, ExprField* e, TypeStr* rty)
{
    if (e->is_arrow)
    {
        elaborate_expr_decay(elab, e->lhs, rty);
    }
    else
    {
        elaborate_expr_lvalue(elab, e->lhs, rty);
    }
    elaborate_expr_ExprField_lhs(elab, e, rty);
    typestr_add_pointer(rty);
    e->take_address = 1;
}

static void elaborate_expr_lvalue(struct Elaborator* elab, struct Expr* expr, struct TypeStr* rty)
{
#define DISPATCH_EXPR_LVALUE(Y)                                                                                        \
    case AST_KIND_##Y: elaborate_expr_lvalue_##Y(elab, (struct Y*)expr, rty); break

    switch (expr->kind)
    {
        DISPATCH_EXPR_LVALUE(ExprRef);
        DISPATCH_EXPR_LVALUE(ExprAssign);
        DISPATCH_EXPR_LVALUE(ExprDeref);
        DISPATCH_EXPR_LVALUE(ExprField);
        default:
            elaborate_expr(elab, expr, rty);
            typestr_error1(
                token_rc(expr->tok), elab->types, "error: expected lvalue but got expression of type %.*s\n", rty);
            *rty = s_type_unknown;
            break;
    }
    expr->sizing = typestr_calc_elem_sizing(elab->types, rty, token_rc(expr->tok));
    expr->c = rty->c;
    expr->elaborated = 1;
}

static void elaborate_expr_decay(struct Elaborator* elab, struct Expr* expr, struct TypeStr* rty)
{
    elaborate_expr_impl(elab, expr, rty);
    if (typestr_decay(rty))
    {
        expr->take_address = 1;
    }
    expr->sizing = typestr_calc_sizing_zero_void(elab->types, rty, token_rc(expr->tok));
    expr->c = rty->c;
    expr->elaborated = 1;
}

static void elaborate_expr_ExprRef(Elaborator* elab, ExprRef* e, TypeStr* rty)
{
    *rty = e->sym->type;
    if (e->sym->size.width == 0)
    {
        e->sym->size = typestr_calc_sizing(elab->types, rty, token_rc(e->tok));
    }
}
static void elaborate_expr_ExprCast(Elaborator* elab, ExprCast* e, TypeStr* rty)
{
    TypeStr orig;
    elaborate_expr_decay(elab, e->expr, &orig);
    elaborate_declspecs(elab, e->specs);
    elaborate_decl(elab, e->type);
    typestr_from_decltype_Decl(elab->p->expr_seqs.data, elab->types, rty, e->type);
    rty->c = orig.c;
    if (orig.c.is_const)
    {
        const unsigned orig_mask = typestr_mask(&orig);
        const unsigned rty_mask = typestr_mask(rty);
        if (rty_mask & orig_mask & TYPE_FLAGS_POINTER)
        {
        }
        else if ((rty_mask & TYPE_FLAGS_POINTER) && typestr_is_constant_zero(&orig))
        {
        }
        else if ((rty_mask & TYPE_FLAGS_POINTER) && (orig_mask & TYPE_MASK_FN_ARR) && orig.c.is_lvalue)
        {
            rty->c = orig.c;
            rty->c.is_lvalue = 0;
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
}

#define DISPATCH_EXPR(Y)                                                                                               \
    case AST_KIND_##Y: elaborate_expr_##Y(elab, (struct Y*)expr, rty); break

static void elaborate_expr_impl(struct Elaborator* elab, struct Expr* expr, struct TypeStr* rty)
{
#if defined(TRACING_ELAB)
    if (expr->tok)
        fprintf(stderr, "{elaborate_expr(%s:%d:%d)\n", expr->tok->rc.file, expr->tok->rc.row, expr->tok->rc.col);
    else
        fprintf(stderr, "{elaborate_expr(?)\n");
#endif
    memset(rty, 0, sizeof(*rty));
    switch (expr->kind)
    {
        DISPATCH_EXPR(ExprLit);
        DISPATCH_EXPR(ExprRef);
        DISPATCH_EXPR(ExprCast);
        DISPATCH_EXPR(ExprCall);
        DISPATCH_EXPR(ExprField);
        DISPATCH_EXPR(ExprBinOp);
        DISPATCH_EXPR(ExprTernary);
        DISPATCH_EXPR(ExprUnOp);
        DISPATCH_EXPR(ExprDeref);
        DISPATCH_EXPR(ExprAddress);
        DISPATCH_EXPR(ExprAdd);
        DISPATCH_EXPR(ExprAssign);
        DISPATCH_EXPR(ExprIncr);
        DISPATCH_EXPR(ExprBuiltin);
        default: parser_tok_error(NULL, "error: unknown expr kind: %s\n", ast_kind_to_string(expr->kind)); return;
    }
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
                elaborate_expr(elab, arr->arity, &ts);
                int arity = i32constant_or_err(&ts, arr->arity->tok);
                if (arity <= 0)
                {
                    parser_tok_error(arr->arity->tok, "error: array must have positive extent (was %d)\n", arity);
                    arr->integer_arity = 1;
                }
                else
                {
                    arr->integer_arity = arity;
                }
            }
            break;
        }
        case AST_DECLFN:
        {
            struct DeclFn* fn = (void*)ast;
            elaborate_decltype(elab, fn->type);
            if (!fn->is_param_list)
            {
                elaborate_stmts(elab, fn->seq);
            }
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
    Elaborator* elab, size_t constinit_offset, size_t offset, size_t sz, const Ast* ast, uint8_t is_aggregate_init)
{
    int rc = 0;
    char* const bytes = elab->constinit.data + constinit_offset + offset;
    if (ast->kind == AST_INIT)
    {
        AstInit* init = (void*)ast;
        if (is_aggregate_init && init->is_braced_strlit)
        {
            ast = init->init;
            goto expr_init;
        }

        memset(bytes, 0, sz);
        while (init->init)
        {
            UNWRAP(elaborate_constinit(
                elab, constinit_offset, init->offset, init->sizing.width, init->init, init->is_aggregate_init));
            init = init->next;
        }
    }
    else
    {
    expr_init:
        if (is_aggregate_init && ast->kind == EXPR_LIT && ast->tok->type == LEX_STRING)
        {
            ExprLit* lit = (void*)ast;
            size_t n = sz;
            if (n > lit->tok->tok_len + 1) n = lit->tok->tok_len + 1;
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
            elaborate_expr(elab, (Expr*)ast, &ty);
            if (!ty.c.is_const)
            {
                return parser_tok_error(ast->tok, "error: expected constant expression\n");
            }
            memcpy(bytes, &ty.c.value.lower, sz);
            if (sz == 8)
            {
                if (ty.c.is_lvalue && ty.c.sym)
                {
                    memcpy(elab->constinit_bases.data + constinit_offset + offset, &ty.c.sym, 8);
                }
            }
            else
            {
                if (ty.c.is_lvalue)
                {
                    return parser_tok_error(ast->tok, "error: expected integral constant expression\n");
                }
            }
        }
    }

fail:
    return rc;
}

static int elaborate_decl(Elaborator* const elab, Decl* const decl)
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

                elaborate_stmts(elab, decl->decl_list);

                TypeStr ts = sym->type;
                typestr_pop_offset(&ts);
                sym->fn_ret_sizing = typestr_calc_sizing_zero_void(elab->types, &ts, token_rc(decl->tok));
            }
            if (decl->init && !sym->is_enum_constant)
            {
                Decl* prev = elab->cur_decl;
                elab->cur_decl = decl;
                elaborate_init_ty(elab, 0, &sym->type, &sym->type.c, decl->init);
                elab->cur_decl = prev;
            }
            if (tyb == TYPE_BYTE_UNK_ARRAY)
            {
                const int is_char_array = typestr_is_char_array(&sym->type);
                typestr_remove_array(&sym->type);
                const size_t elem_size = typestr_get_size(elab->types, &sym->type, &decl->tok->rc);
                if (!decl->init)
                {
                    if (!decl->sym->next_field && decl->sym->parent_su)
                    {
                        typestr_append_offset(&sym->type, 0, TYPE_BYTE_ARRAY);
                    }
                    else
                    {
                        UNWRAP(parser_tok_error(
                            decl->tok,
                            "error: definition of object with unknown array bounds must have an initializer\n"));
                    }
                }
                else
                {
                    Ast* init = decl->init;
                    if (init->kind == AST_INIT)
                    {
                        AstInit* i = (AstInit*)init;
                        if (is_char_array && i->is_braced_strlit)
                        {
                            init = i->init;
                            goto strlit_init;
                        }
                        else
                        {
                            uint32_t max_assign = 0;
                            for (; i->init; i = i->next)
                            {
                                uint32_t new_max = i->offset / elem_size;
                                if (new_max > max_assign) max_assign = new_max;
                            }
                            if (max_assign == UINT32_MAX) abort();
                            typestr_append_offset(&sym->type, max_assign + 1, TYPE_BYTE_ARRAY);
                        }
                    }
                    else if (init->kind == EXPR_LIT && init->tok->type == LEX_STRING)
                    {
                    strlit_init:
                        typestr_append_offset(&sym->type, init->tok->tok_len + 1, TYPE_BYTE_ARRAY);
                    }
                    else
                    {
                        UNWRAP(parser_tok_error(
                            init->tok,
                            "error: array initializer must be either a string literal or an initializer list\n"));
                    }
                }
                sym->type.c.is_const = 1;
                sym->type.c.is_lvalue = 1;
                sym->type.c.sym = sym;
            }

            sym->align = typestr_get_align(elab->types, &sym->type);
            if (tyb == TYPE_BYTE_UNK_ARRAY && !decl->sym->next_field && decl->sym->parent_su)
            {
            }
            else
            {
                sym->size = typestr_calc_sizing(elab->types, &sym->type, token_rc(decl->tok));

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
    else
    {
        if (!decl->specs->is_extern && !decl->specs->is_typedef)
        {
            sym->size = typestr_calc_sizing(elab->types, &sym->type, token_rc(decl->tok));
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
            Decl** const seqs = (Decl**)((void**)elab->p->expr_seqs.data + block->seq.off);
            int enum_value = -1;
            for (size_t i = 0; i < block->seq.ext; ++i)
            {
#ifndef NDEBUG
                if (seqs[i]->kind != AST_DECL) abort();
#endif
                struct Decl* edecl = seqs[i];
                UNWRAP(elaborate_decl(elab, edecl));
                if (edecl->init)
                {
                    if (!ast_kind_is_expr(edecl->init->kind))
                    {
                        UNWRAP(parser_tok_error(edecl->init->tok, "error: expected constant integer expression\n"));
                    }

                    TypeStr ts = {0};
                    elaborate_expr(elab, (Expr*)edecl->init, &ts);
                    enum_value = i32constant_or_err(&ts, edecl->init->tok);
                }
                else
                {
                    ++enum_value;
                }
                edecl->sym->enum_value = enum_value;
                typestr_assign_constant_value(&edecl->sym->type, mp_from_i64(enum_value));
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

            void** expr_seqs = elab->p->expr_seqs.data;
            StmtDecls** const seqs = (StmtDecls**)expr_seqs + block->seq.off;
            for (size_t i = 0; i < block->seq.ext; ++i)
            {
                if (seqs[i]->kind != STMT_DECLS) abort();
                struct StmtDecls* decls = seqs[i];
                UNWRAP(elaborate_declspecs(elab, decls->specs));
                Decl** const decl_seqs = (Decl**)expr_seqs + decls->seq.off;
                for (size_t j = 0; j < decls->seq.ext; ++j)
                {
                    struct Decl* field = decl_seqs[j];
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
    if (!p->top) abort();
    elaborate_stmt(elab, &p->top->ast);
    return parser_has_errors();
}

void elaborator_destroy(struct Elaborator* elab)
{
    tt_free(elab->types);
    array_destroy(&elab->constinit);
    array_destroy(&elab->constinit_bases);
}
