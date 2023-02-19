#include "checker.h"

#include <string.h>

#include "array.h"
#include "ast.h"
#include "elaborator.h"
#include "errors.h"
#include "interval.h"
#include "lexstate.h"
#include "parse.h"
#include "stdlibe.h"
#include "symbol.h"
#include "token.h"

typedef struct ValueInfo
{
    const Symbol* sym;
    size_t sym_offset;
    Interval val;
    unsigned char or_integer : 1;
    unsigned char or_symaddr : 1;
    unsigned char or_uninitialized : 1;
    unsigned char or_any : 1;
    unsigned char or_null : 1;
} ValueInfo;

static const ValueInfo s_valueinfo_void = {0};
static const ValueInfo s_valueinfo_uninit = {.or_uninitialized = 1};
static const ValueInfo s_valueinfo_any = {.or_any = 1};
// static const ValueInfo s_valueinfo_null = {.or_null = 1};

static void valinfo_init_interval(ValueInfo* info, Interval v)
{
    memset(info, 0, sizeof(ValueInfo));
    info->or_integer = 1;
    info->val = v;
}
static void valinfo_init_symaddr(ValueInfo* info, const Symbol* sym)
{
    memset(info, 0, sizeof(ValueInfo));
    info->or_symaddr = 1;
    info->sym = sym;
}
static void valinfo_init_integer(ValueInfo* info, Sizing sz, uint64_t value)
{
    memset(info, 0, sizeof(ValueInfo));
    info->or_integer = 1;
    info->val.sz = sz;
    info->val.base = value;
}

static int value_is_truthy(ValueInfo* result)
{
    return result->or_any || result->or_symaddr || (result->or_integer && interval_contains_nonzero(result->val));
}
static int value_is_falsy(ValueInfo* result)
{
    return result->or_any || result->or_null || (result->or_integer && interval_contains_0(result->val));
}

static void value_cast_to_bool(ValueInfo* result)
{
    if (result->or_any)
    {
        *result = s_valueinfo_any;
    }
    else
    {
        int tru = value_is_truthy(result);
        int fal = value_is_falsy(result);
        if (!fal)
            valinfo_init_interval(result, s_interval_one);
        else if (!tru)
            valinfo_init_interval(result, s_interval_zero);
        else
            valinfo_init_interval(result, s_interval_zero_one);
    }
}

typedef struct CheckContext
{
    // Array<Symbol*>
    Array syms;
    // Array<ValueInfo>
    Array info;
} CheckContext;
static void chkctx_destroy(CheckContext* ctx)
{
    array_destroy(&ctx->syms);
    array_destroy(&ctx->info);
}

struct Checker
{
    const Elaborator* elab;
    const Parser* p;
    Array fmt_tmp;
    CheckContext ctx;
};

void check_merge_values(Checker* chk, ValueInfo* v, const ValueInfo* w)
{
    if (v->or_any || w->or_any)
    {
        *v = s_valueinfo_any;
        return;
    }

    if (w->or_integer)
    {
        if (v->or_integer)
        {
            v->val = interval_merge(v->val, w->val);
        }
        else
        {
            v->val = w->val;
            v->or_integer = 1;
        }
    }
    if (w->or_null) v->or_null = 1;
    if (w->or_symaddr)
    {
        if (v->or_symaddr && v->sym != w->sym)
            v->sym = NULL;
        else
        {
            v->or_symaddr = 1;
            v->sym = w->sym;
        }
    }
    if (w->or_uninitialized) v->or_uninitialized = 1;
}

static Interval chk_neg_ofchk(Checker* chk, Interval i, const Token* tok)
{
    if (i.sz.is_signed && interval_neg_ofchk(i))
    {
        array_clear(&chk->fmt_tmp);
        interval_fmt(&chk->fmt_tmp, i);
        array_push_byte(&chk->fmt_tmp, '\0');
        parser_tok_error(tok, "error: possible signed overflow\n    Range: %s\n", chk->fmt_tmp.data);
    }
    return interval_neg(i);
}

static Interval chk_add_ofchk(Checker* chk, Interval i, Interval j, Sizing sz, const Token* tok)
{
    Interval i2 = interval_cast(i, sz);
    Interval j2 = interval_cast(j, sz);
    if (sz.is_signed && interval_add_ofchk(i2, j2))
    {
        array_clear(&chk->fmt_tmp);
        array_appends(&chk->fmt_tmp, "    Range 1: ");
        interval_fmt(&chk->fmt_tmp, i);
        array_appends(&chk->fmt_tmp, "\n    Range 2: ");
        interval_fmt(&chk->fmt_tmp, j);
        array_push_byte(&chk->fmt_tmp, '\0');
        parser_tok_error(tok, "error: possible signed overflow\n%s\n", chk->fmt_tmp.data);
    }
    return interval_add(i2, j2);
}
static Interval chk_sub_ofchk(Checker* chk, Interval i, Interval j, Sizing sz, const Token* tok)
{
    if (sz.is_signed && interval_sub_ofchk(i, j))
    {
        array_clear(&chk->fmt_tmp);
        array_appends(&chk->fmt_tmp, "    Range 1: ");
        interval_fmt(&chk->fmt_tmp, i);
        array_appends(&chk->fmt_tmp, "\n    Range 2: ");
        interval_fmt(&chk->fmt_tmp, j);
        array_push_byte(&chk->fmt_tmp, '\0');
        parser_tok_error(tok, "error: possible signed overflow\n%s\n", chk->fmt_tmp.data);
    }
    return interval_sub(i, j);
}

static ValueInfo* chkctx_get_info(CheckContext* ctx, const Symbol* sym)
{
    const size_t i = arrptr_find(&ctx->syms, sym);
    if (i == arrptr_size(&ctx->syms))
    {
        return NULL;
    }
    return (ValueInfo*)ctx->info.data + i;
}

// static void valinfo_cast_int(ValueInfo* info, Sizing sz)
// {
//     static const unsigned long long s_imin_for_sizing[9] = {0, INT8_MIN, INT16_MIN, 0, INT32_MIN, 0, 0, 0,
//     INT64_MIN}; static const unsigned long long s_imax_for_sizing[9] = {0, INT8_MAX, INT16_MAX, 0, INT32_MAX, 0, 0,
//     0, INT64_MAX}; static const unsigned long long s_umin_for_sizing[9] = {
//         0, UINT8_MIN, UINT16_MIN, 0, UINT32_MIN, 0, 0, 0, UINT64_MIN};
//     static const unsigned long long s_umax_for_sizing[9] = {
//         0, UINT8_MIN, UINT16_MIN, 0, UINT32_MIN, 0, 0, 0, UINT64_MIN};

//     if (!info->or_integer) return;
//     const Sizing p = info->val_sizing;
//     const unsigned long long wrap = info->val_maxoff + info->val_base;
//     if (p.is_signed)
//     {
//         if (info->val_base <= s_imax_for_sizing[p.width])
//         {
//             // do nothing
//         }
//         else
//         {

//         }

//         if (wrap > s_imax_for_sizing[p.width])
//         {
//             wrap - s_imax_for_sizing[p.width] + s_imax
//         }
//         if (sz.width >= info->val_sizing.width)
//         {
//         }
//     }
//     if (sz.is_signed)
//     {
//     }
//     else
//     {
//         if (sz.width == 4)
//         {
//         }
//     }
//     info->val_sizing = sz;
// }

static void chkctx_assign_value(CheckContext* ctx, const Symbol* sym, const ValueInfo* v)
{
    const size_t i = arrptr_find(&ctx->syms, sym);
    if (i == arrptr_size(&ctx->syms))
    {
        arrptr_push(&ctx->syms, sym);
        array_push(&ctx->info, v, sizeof(*v));
    }
    else
    {
        ((ValueInfo*)ctx->info.data)[i] = *v;
    }
}

struct Checker* checker_alloc(const struct Elaborator* elab)
{
    Checker* chk = my_malloc(sizeof(Checker));
    memset(chk, 0, sizeof(*chk));
    chk->elab = elab;
    chk->p = elab->p;
    return chk;
}

#define DISPATCH(prefix, type, ...)                                                                                    \
    case AST_KIND_##type: return prefix##type(chk, (const type*)__VA_ARGS__)

static void check_stmt(Checker* chk, const Ast* ast);
static void check_expr(Checker* chk, const Expr* e, ValueInfo* result);

static void check_ExprLit(Checker* chk, const ExprLit* e, ValueInfo* result)
{
    if (e->tok->type == LEX_NUMBER || e->tok->type == LEX_CHARLIT)
    {
        valinfo_init_integer(result, e->sizing, e->numeric);
    }
    else if (e->tok->type == LEX_STRING)
    {
        // be_compile_ExprLit_Sym(be, e->sym);
        // *out = e->sym->addr;
        parser_tok_error(e->tok, "error: unimplemented literal type (%d)\n", e->tok->type);
        *result = s_valueinfo_any;
    }
    else
    {
        parser_tok_error(e->tok, "error: unimplemented literal type (%d)\n", e->tok->type);
        *result = s_valueinfo_any;
    }
}

static void check_ExprBuiltin(Checker* chk, const ExprBuiltin* e, ValueInfo* result)
{
    switch (e->tok->type)
    {
        case LEX_PROVE:
            check_expr(chk, e->expr1, result);
            if (result->or_integer)
            {
                array_clear(&chk->fmt_tmp);
                interval_fmt(&chk->fmt_tmp, result->val);
                array_push_byte(&chk->fmt_tmp, '\0');
                fprintf(stderr, "Prove: Range: %s\n", (char*)chk->fmt_tmp.data);
                if (interval_contains_0(result->val))
                {
                    array_clear(&chk->fmt_tmp);
                    interval_fmt(&chk->fmt_tmp, result->val);
                    array_push_byte(&chk->fmt_tmp, '\0');
                    parser_tok_error(e->tok, "error: cannot prove non-zero\n    Range: %s\n", chk->fmt_tmp.data);
                }
            }
            if (result->or_symaddr)
            {
                if (result->sym)
                {
                    fprintf(stderr, "Prove: Sym: %s\n", result->sym->name ? result->sym->name : "(anon)");
                }
                else
                {
                    fprintf(stderr, "Prove: Sym unknown\n");
                }
            }
            if (result->or_null)
            {
                parser_tok_error(e->tok, "error: cannot prove non-null\n");
            }
            if (result->or_uninitialized)
            {
                parser_tok_error(e->tok, "error: cannot prove initialized\n");
            }
            break;
        default:
            parser_tok_error(e->tok, "error: unimplemented Builtin checker (%s)\n", lexstate_to_string(e->tok->type));
    }
    *result = s_valueinfo_void;
}

static void check_ExprAddress(Checker* chk, const ExprAddress* e, ValueInfo* result)
{
    check_expr(chk, e->lhs, result);
}

static void check_ExprBinOp_or_and(Checker* chk, const ExprBinOp* e, ValueInfo* result)
{
    if (result->or_any) return;
    value_cast_to_bool(result);
    if (!result->or_integer) abort();
    int nonzero = interval_contains_nonzero(result->val);
    int zero = interval_contains_0(result->val);
    if (e->tok->type == TOKEN_SYM2('&', '&'))
    {
        if (nonzero)
        {
            check_expr(chk, e->rhs, result);
            value_cast_to_bool(result);
            if (zero) result->val = interval_merge(s_interval_zero, result->val);
        }
    }
    if (e->tok->type == TOKEN_SYM2('|', '|'))
    {
        if (zero)
        {
            check_expr(chk, e->rhs, result);
            value_cast_to_bool(result);
            if (nonzero) result->val = interval_merge(s_interval_one, result->val);
        }
    }
}

static void check_ExprBinOp(Checker* chk, const ExprBinOp* e, ValueInfo* result)
{
    check_expr(chk, e->lhs, result);
    if (e->tok->type == TOKEN_SYM1(',')) return check_expr(chk, e->rhs, result);
    if (e->tok->type == TOKEN_SYM2('&', '&') || e->tok->type == TOKEN_SYM2('|', '|'))
        return check_ExprBinOp_or_and(chk, e, result);
    ValueInfo rhs;
    check_expr(chk, e->rhs, &rhs);
    if (rhs.or_any) *result = s_valueinfo_any;
    if (result->or_any) return;
    if (!rhs.or_integer || !result->or_integer)
    {
        parser_tok_error(e->tok, "error: unimplemented ExprBinOp on non-integers\n");
        *result = s_valueinfo_any;
        return;
    }
    result->val = interval_cast(result->val, e->common_sz);
    rhs.val = interval_cast(rhs.val, e->common_sz);
    switch (e->tok->type)
    {
        case TOKEN_SYM1('/'):
            if (interval_contains_0(rhs.val))
            {
                array_clear(&chk->fmt_tmp);
                interval_fmt(&chk->fmt_tmp, rhs.val);
                array_push_byte(&chk->fmt_tmp, '\0');
                parser_tok_error(e->tok,
                                 "error: possible divide by zero\n"
                                 "    Divisor range: %s\n",
                                 chk->fmt_tmp.data);
                *result = s_valueinfo_any;
                return;
            }
            else if (e->sizing.is_signed && interval_contains(result->val, INT64_MIN) && interval_contains(rhs.val, -1))
            {
                array_clear(&chk->fmt_tmp);
                interval_fmt(&chk->fmt_tmp, rhs.val);
                array_push_byte(&chk->fmt_tmp, '\0');
                parser_tok_error(e->tok,
                                 "error: possible signed overflow\n"
                                 "    Divisor range: %s\n",
                                 chk->fmt_tmp.data);
                *result = s_valueinfo_any;
                return;
            }
            else
            {
                if (e->sizing.is_signed)
                {
                    valinfo_init_interval(result, interval_div(result->val, rhs.val));
                }
                else
                {
                    valinfo_init_interval(result, interval_udiv(result->val, rhs.val));
                }
            }
            break;
        case TOKEN_SYM1('*'): valinfo_init_interval(result, interval_mult(result->val, rhs.val)); break;
        case TOKEN_SYM1('<'):
            valinfo_init_interval(result,
                                  result->val.sz.is_signed ? interval_lt(result->val, rhs.val)
                                                           : interval_ltu(result->val, rhs.val));
            break;
        case TOKEN_SYM1('>'):
            valinfo_init_interval(result,
                                  result->val.sz.is_signed ? interval_lt(rhs.val, result->val)
                                                           : interval_ltu(rhs.val, result->val));
            break;
        case TOKEN_SYM2('<', '='):
            valinfo_init_interval(result,
                                  result->val.sz.is_signed ? interval_lte(result->val, rhs.val)
                                                           : interval_lteu(result->val, rhs.val));
            break;
        case TOKEN_SYM2('>', '='):
            valinfo_init_interval(result,
                                  result->val.sz.is_signed ? interval_lte(rhs.val, result->val)
                                                           : interval_lteu(rhs.val, result->val));
            break;
        case TOKEN_SYM2('=', '='):
            valinfo_init_interval(result,
                                  result->val.sz.is_signed ? interval_eq(result->val, rhs.val)
                                                           : interval_equ(result->val, rhs.val));
            break;
        default:
            parser_tok_error(e->tok, "error: unimplemented ExprBinOp type (%s)\n", token_str(chk->p, e->tok));
            *result = s_valueinfo_any;
    }
}
static void check_ExprUnOp(Checker* chk, const ExprUnOp* e, ValueInfo* result)
{
    check_expr(chk, e->lhs, result);
    if (result->or_any) return;
    if (!result->or_integer)
    {
        parser_tok_error(e->tok, "error: unimplemented ExprUnOp on non-integers\n");
        *result = s_valueinfo_any;
        return;
    }
    switch (e->tok->type)
    {
        case TOKEN_SYM1('-'): result->val = chk_neg_ofchk(chk, result->val, e->tok); return;
        default:
            parser_tok_error(e->tok, "error: unimplemented ExprUnOp type (%s)\n", token_str(chk->p, e->tok));
            *result = s_valueinfo_any;
    }
}

static void check_ExprAdd(Checker* chk, const ExprAdd* e, ValueInfo* result)
{
    ValueInfo rhs;
    check_expr(chk, e->lhs, result);
    check_expr(chk, e->rhs, &rhs);
    if (rhs.or_any) *result = s_valueinfo_any;
    if (result->or_any) return;
    if (!result->or_integer || !rhs.or_integer)
    {
        parser_tok_error(e->tok, "error: expected integer in ExprAdd check\n");
        *result = s_valueinfo_any;
        return;
    }
    if (e->tok->type == TOKEN_SYM1('-'))
    {
        // subtract
        result->val = chk_sub_ofchk(chk, result->val, rhs.val, e->sizing, e->tok);
    }
    else
    {
        // add
        result->val = chk_add_ofchk(chk, result->val, rhs.val, e->sizing, e->tok);
    }
}

static void check_ExprRef(Checker* chk, const ExprRef* e, ValueInfo* result)
{
    if (e->take_address)
    {
        valinfo_init_symaddr(result, e->sym);
    }
    else
    {
        ValueInfo* info = chkctx_get_info(&chk->ctx, e->sym);
        if (info)
        {
            if (info->or_uninitialized)
            {
                parser_tok_error(e->tok, "error: possible uninitialized read: %s\n", e->sym->name);
                *result = s_valueinfo_any;
            }
            else
                *result = *info;
        }
        else
        {
            parser_tok_error(e->tok, "error: no info for symbol: %s\n", e->sym->name);
            *result = s_valueinfo_any;
        }
    }
}

static void check_expr(Checker* chk, const Expr* e, ValueInfo* result)
{
    switch (e->kind)
    {
#define DISPATCH_CHECK(type) DISPATCH(check_, type, e, result)
        DISPATCH_CHECK(ExprLit);
        DISPATCH_CHECK(ExprRef);
        DISPATCH_CHECK(ExprAddress);
        DISPATCH_CHECK(ExprBinOp);
        DISPATCH_CHECK(ExprAdd);
        DISPATCH_CHECK(ExprBuiltin);
        DISPATCH_CHECK(ExprUnOp);
#undef DISPATCH_CHECK
        default:
            parser_tok_error(e->tok, "error: unimplemented ast to check: %s\n", ast_kind_to_string(e->kind));
            *result = s_valueinfo_any;
    }
}

static void check_DeclSpecs(Checker* chk, const DeclSpecs* e) { }
static void check_Decl(Checker* chk, const Decl* e)
{
    const Symbol* const sym = e->sym;
    if (sym->def == e)
    {
        // fn
        const unsigned tym = typestr_mask(&sym->type);
        if (tym & TYPE_FLAGS_FUNCTION)
        {
            check_stmt(chk, e->init);
        }
        else if (tym & TYPE_FLAGS_INT)
        {
            chkctx_assign_value(&chk->ctx, sym, &s_valueinfo_uninit);
            if (e->init)
            {
                if (ast_kind_is_expr(e->init->kind))
                {
                    ValueInfo v;
                    check_expr(chk, (const Expr*)e->init, &v);
                    chkctx_assign_value(&chk->ctx, sym, &v);
                }
                else
                {
                    parser_tok_error(e->init->tok, "error: unimplemented initializer to check\n");
                }
            }
        }
        else
        {
            parser_tok_error(e->tok, "error: unimplemented ast to check\n");
        }
    }
}
static void check_StmtDecls(Checker* chk, const StmtDecls* e)
{
    check_DeclSpecs(chk, e->specs);
    Decl* const* const decls = (void*)((void**)chk->p->expr_seqs.data + e->seq.off);
    for (size_t i = 0; i < e->seq.ext; ++i)
        check_Decl(chk, decls[i]);
}
static void check_StmtBlock(Checker* chk, const StmtBlock* e)
{
    Ast* const* const asts = (void*)((void**)chk->p->expr_seqs.data + e->seq.off);
    for (size_t i = 0; i < e->seq.ext; ++i)
        check_stmt(chk, asts[i]);
}

static void check_stmt(Checker* chk, const Ast* ast)
{
    if (ast_kind_is_expr(ast->kind))
    {
        ValueInfo v;
        return check_expr(chk, (struct Expr*)ast, &v);
    }
    switch (ast->kind)
    {
#define DISPATCH_CHECK(type) DISPATCH(check_, type, ast)
        DISPATCH_CHECK(StmtDecls);
        DISPATCH_CHECK(StmtBlock);
        default: parser_tok_error(ast->tok, "error: unknown stmt kind: %s\n", ast_kind_to_string(ast->kind)); return;
    }
}

int checker_check(Checker* chk)
{
    struct Parser* const p = chk->elab->p;
    if (!p->top) abort();
    check_stmt(chk, &p->top->ast);
    return parser_has_errors();
}
void checker_free(struct Checker* chk)
{
    chkctx_destroy(&chk->ctx);
    array_destroy(&chk->fmt_tmp);
    my_free(chk);
}
