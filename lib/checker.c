#include "checker.h"

#include <string.h>

#include "array.h"
#include "ast.h"
#include "elaborator.h"
#include "errors.h"
#include "interval.h"
#include "lexstate.h"
#include "parse.h"
#include "ptrmap.h"
#include "stdlibe.h"
#include "symbol.h"
#include "token.h"

enum ValueInfoKind
{
    value_info_bottom,
    value_info_void,
    value_info_uninitialized,
    value_info_integer,
    value_info_address,
};

static const char* value_info_kind_to_string(enum ValueInfoKind k)
{
#define case_str(x)                                                                                                    \
    case x: return #x
    switch (k)
    {
        case_str(value_info_bottom);
        case_str(value_info_void);
        case_str(value_info_uninitialized);
        case_str(value_info_integer);
        case_str(value_info_address);
        default: abort();
    }
}

typedef struct ValueInfo
{
    enum ValueInfoKind kind;
    const Symbol* sym;
    size_t sym_offset;
    TypeStrBuf sym_type;
    Interval val;
    size_t ref_ssa;
    unsigned char or_null : 1;
    unsigned char or_obj : 1;
} ValueInfo;

static const ValueInfo s_valueinfo_void = {.kind = value_info_void};
static const ValueInfo s_valueinfo_uninit = {.kind = value_info_uninitialized};
static const ValueInfo s_valueinfo_any = {.kind = value_info_bottom};
// static const ValueInfo s_valueinfo_null = {.or_null = 1};

static void valinfo_init_interval(ValueInfo* info, Interval v)
{
    memset(info, 0, sizeof(ValueInfo));
    info->kind = value_info_integer;
    info->val = v;
}
static void valinfo_init_symaddr(ValueInfo* info, const Symbol* sym)
{
    memset(info, 0, sizeof(ValueInfo));
    info->kind = value_info_address;
    info->sym = sym;
    info->sym_type = sym->type.buf;
}
static void valinfo_init_integer(ValueInfo* info, Sizing sz, uint64_t value)
{
    memset(info, 0, sizeof(ValueInfo));
    info->kind = value_info_integer;
    info->val.sz = sz;
    info->val.base = value;
}
#if 0
static void valinfo_lt(ValueInfo* out, const ValueInfo* lhs, const ValueInfo* rhs, const RowCol* rc)
{
    if (lhs->kind == value_info_bottom || rhs->kind == value_info_bottom) *out = s_valueinfo_any;
    if (lhs->kind == value_info_integer && rhs->kind == value_info_integer)
    {
        valinfo_init_interval(out, interval_lt(lhs->val, rhs->val));
    }
    else
    {
        parser_ferror(rc, "error: cannot compare non-integer values\n");
        *out = s_valueinfo_any;
    }
}

static void valinfo_lte(ValueInfo* out, const ValueInfo* lhs, const ValueInfo* rhs, const RowCol* rc)
{
    if (lhs->kind == value_info_bottom || rhs->kind == value_info_bottom) *out = s_valueinfo_any;
    if (lhs->kind == value_info_integer && rhs->kind == value_info_integer)
    {
        valinfo_init_interval(out, interval_lte(lhs->val, rhs->val));
    }
    else
    {
        parser_ferror(rc, "error: cannot compare non-integer values\n");
        *out = s_valueinfo_any;
    }
}

static int value_is_truthy(ValueInfo* result)
{
    switch (result->kind)
    {
        case value_info_integer: return interval_contains_nonzero(result->val);
        case value_info_address: return result->or_obj;
        default: return 0;
    }
}
static int value_is_falsy(ValueInfo* result)
{
    switch (result->kind)
    {
        case value_info_integer: return interval_contains_0(result->val);
        case value_info_address: return result->or_null;
        default: return 0;
    }
}
static void value_split_true_false(const ValueInfo* src, ValueInfo* true_info, ValueInfo* false_info)
{
    *true_info = *src;
    *false_info = *src;
    switch (src->kind)
    {
        case value_info_void:
        case value_info_bottom:
        case value_info_uninitialized: break;
        case value_info_integer:
            if (interval_contains_0(src->val))
            {
                false_info->val.base = 0;
                false_info->val.maxoff = 0;
            }
            else
                *false_info = s_valueinfo_any;

            if (interval_contains_nonzero(src->val))
            {
                interval_remove_0(&true_info->val, true_info->val);
            }
            else
                *true_info = s_valueinfo_any;
            break;
        case value_info_address:
            if (src->or_null)
                false_info->or_obj = 0;
            else
                *false_info = s_valueinfo_any;
            if (src->or_obj)
                true_info->or_null = 0;
            else
                *true_info = s_valueinfo_any;
            break;
    }
}
#endif
static void value_cast_to_bool(ValueInfo* result)
{
    switch (result->kind)
    {
        case value_info_void:
        case value_info_bottom:
        case value_info_uninitialized: *result = s_valueinfo_any; break;
        case value_info_integer:
            if (!interval_contains_0(result->val))
            {
                valinfo_init_interval(result, s_interval_one);
            }
            else if (!interval_contains_nonzero(result->val))
            {
                valinfo_init_interval(result, s_interval_zero);
            }
            else
            {
                valinfo_init_interval(result, s_interval_zero_one);
            }
            break;
        case value_info_address:
            if (!result->or_null)
                valinfo_init_interval(result, s_interval_one);
            else if (!result->or_obj)
                valinfo_init_interval(result, s_interval_zero);
            else
                valinfo_init_interval(result, s_interval_zero_one);
            break;
    }
}

typedef struct CheckContext
{
    PtrMap sym_to_ssa;
    // Array<ValueInfo>
    Array info;
    struct CheckContext* parent;
    unsigned char is_void : 1;
} CheckContext;

static void chkctx_destroy(CheckContext* ctx)
{
    ptrmap_destroy(&ctx->sym_to_ssa);
    array_destroy(&ctx->info);
}

struct Checker
{
    const Elaborator* elab;
    const Parser* p;
    Array fmt_tmp;
    Array ssa_info;
    Array ssa_rc;
    CheckContext* ctx;
};

// static __forceinline const ValueInfo* check_ssa_info(Checker* chk, size_t ssa)
// {
//     if (ssa == 0) abort();
//     --ssa;
//     return (const ValueInfo*)chk->ssa_info.data + ssa;
// }
static __forceinline size_t check_push_ssa(Checker* chk, const ValueInfo* info, const RowCol* rc)
{
    ValueInfo* i = array_push(&chk->ssa_info, info, sizeof(*info));
    arrptr_push(&chk->ssa_rc, rc);
    return i->ref_ssa = arrptr_size(&chk->ssa_rc);
}

static void check_any_from_type(Checker* chk, ValueInfo* out, const TypeStrBuf* ty, const RowCol* rc)
{
    memset(out, 0, sizeof(*out));
    switch (tsb_byte(ty))
    {
        case TYPE_BYTE_POINTER:
            out->kind = value_info_address;
            out->sym_type = *ty;
            out->or_null = 1;
            out->or_obj = 1;
            break;
        case TYPE_BYTE_INT:
            out->kind = value_info_integer;
            out->val = s_interval_i32;
            break;
        case TYPE_BYTE_UINT:
            out->kind = value_info_integer;
            out->val = s_interval_u32;
            break;
        case TYPE_BYTE_LLONG:
            out->kind = value_info_integer;
            out->val = s_interval_i64;
            break;
        case TYPE_BYTE_ULLONG:
            out->kind = value_info_integer;
            out->val = s_interval_u64;
            break;
        default:
            tsb_error1(rc, chk->elab->types, "error: cannot determine range from type: %.*s\n", ty);
            *out = s_valueinfo_any;
    }
}
static void check_any_from_decl(Checker* chk, ValueInfo* out, const Decl* decl, const RowCol* rc)
{
    return check_any_from_type(chk, out, &decl->sym->type.buf, rc);
}

static ValueInfo* chkctx_get_ssa_info(const CheckContext* ctx, size_t ssa)
{
    ValueInfo* const infos = ctx->info.data;
    const size_t n = array_size(&ctx->info, sizeof(ValueInfo));

    for (size_t i = 0; i < n; ++i)
    {
        ValueInfo* info = infos + i;
        if (info->ref_ssa == ssa)
        {
            return info;
        }
    }
    return NULL;
}

static const ValueInfo* check_get_ssa_info_rec(const Checker* chk, const CheckContext* ctx, size_t ssa)
{
    while (ctx)
    {
        ValueInfo* const infos = ctx->info.data;
        const size_t n = array_size(&ctx->info, sizeof(ValueInfo));

        for (size_t i = 0; i < n; ++i)
        {
            ValueInfo* info = infos + i;
            if (info->ref_ssa == ssa)
            {
                return info;
            }
        }
        ctx = ctx->parent;
    }
    return (const ValueInfo*)chk->ssa_info.data + ssa - 1;
}

// static ValueInfo* chkctx_get_info(CheckContext* ctx, const Symbol* sym)
// {
//     size_t* ssa = ptrmap_find(&ctx->sym_to_ssa, sym);
//     return ssa ? chkctx_get_ssa_info(ctx, *ssa) : NULL;
// }

static void check_merge_values(Checker* chk, ValueInfo* v, const ValueInfo* w, const RowCol* rc)
{
    if (w->kind == value_info_bottom) *v = s_valueinfo_any;
    if (v->kind == value_info_bottom) return;

    if (v->kind != w->kind)
    {
        parser_ferror(rc,
                      "error: cannot merge values of different types (%s vs %s)\n",
                      value_info_kind_to_string(v->kind),
                      value_info_kind_to_string(w->kind));
        *v = s_valueinfo_any;
        return;
    }

    if (v->kind == w->kind)
    {
        switch (v->kind)
        {
            case value_info_address:
                if (!tsb_match(&v->sym_type, &w->sym_type))
                {
                    parser_ferror(rc, "error: cannot merge pointers to different types\n");
                    *v = s_valueinfo_any;
                }
                else
                {
                    if (w->or_obj)
                    {
                        if (v->or_obj)
                        {
                            if (w->sym != v->sym) v->sym = NULL;
                        }
                        else
                        {
                            v->or_obj = 1;
                            v->sym = w->sym;
                        }
                    }
                    if (w->or_null) v->or_null = 1;
                }
                break;
            case value_info_integer: v->val = interval_merge(v->val, w->val); break;
            case value_info_uninitialized:
            case value_info_void: break;
            default: parser_ferror(rc, "error: unimplemented merge\n"); *v = s_valueinfo_any;
        }
    }
}

// static void chkctx_intersect_value(CheckContext* ctx, const Symbol* sym, const ValueInfo* v)
// {
//     const size_t i = arrptr_find(&ctx->syms, sym);
//     if (i == arrptr_size(&ctx->syms))
//     {
//         arrptr_push(&ctx->syms, sym);
//         array_push(&ctx->info, v, sizeof(*v));
//     }
//     else
//     {
//         if (ctx->info.data == NULL) abort();
//         ((ValueInfo*)ctx->info.data)[i] = *v;
//     }
// }

static void check_assign_value(Checker* chk, const Symbol* sym, const ValueInfo* v, const RowCol* rc)
{
    ptrmap_set(&chk->ctx->sym_to_ssa, sym, check_push_ssa(chk, v, rc));
}

static void chkctx_replace_ssa(CheckContext* ctx, const ValueInfo* v)
{
    if (!v->ref_ssa) abort();
    const size_t n_i = array_size(&ctx->info, sizeof(ValueInfo));
    ValueInfo* const info = ctx->info.data;
    for (size_t i = 0; i < n_i; ++i)
    {
        if (info[i].ref_ssa == v->ref_ssa)
        {
            info[i] = *v;
            return;
        }
    }
    array_push(&ctx->info, v, sizeof(*v));
}

static void check_flatten_context(Checker* chk, CheckContext* ctx)
{
    if (!ctx->parent) abort();
    ptrmap_insert_all(&ctx->parent->sym_to_ssa, &ctx->sym_to_ssa);
    const size_t n_i = array_size(&ctx->info, sizeof(ValueInfo));
    const ValueInfo* const info = ctx->info.data;
    for (size_t i = 0; i < n_i; ++i)
    {
        chkctx_replace_ssa(ctx->parent, info + i);
    }
}

static void chkctx_clone(CheckContext* chk, const CheckContext* other)
{
    chk->parent = other->parent;
    array_copy(&chk->info, &other->info);
    ptrmap_copy(&chk->sym_to_ssa, &other->sym_to_ssa);
}

// /// @return 0 on failure
// static size_t chkctx_get_ssa(const CheckContext* ctx, const Symbol* sym)
// {
//     const size_t* p = ptrmap_find(&ctx->sym_to_ssa, sym);
//     return p ? *p : 0;
// }

/// @return 0 on failure
static size_t chkctx_get_ssa_rec(const CheckContext* ctx, const Symbol* sym)
{
    do
    {
        const size_t* p = ptrmap_find(&ctx->sym_to_ssa, sym);
        if (p) return *p;
        ctx = ctx->parent;
    } while (ctx);
    return 0;
}

static void check_merge_context_matching(Checker* chk, CheckContext* ctx1, const CheckContext* ctx2, const RowCol* rc)
{
    const size_t n = ptrmap_size(&ctx1->sym_to_ssa);
    for (size_t i = 0; i < n; ++i)
    {
        const Symbol* sym = ptrmap_nth_ptr(&ctx1->sym_to_ssa, i);
        size_t* ssa = ptrmap_nth_val(&ctx1->sym_to_ssa, i);
        size_t ssa2 = chkctx_get_ssa_rec(ctx2, sym);
        if (ssa2 && ssa2 != *ssa)
        {
            ValueInfo w = *check_get_ssa_info_rec(chk, ctx1, *ssa);
            const ValueInfo* v2 = check_get_ssa_info_rec(chk, ctx2, ssa2);
            check_merge_values(chk, &w, v2, rc);
            *ssa = check_push_ssa(chk, &w, rc);
        }
    }
}

static void check_merge_context_missing(Checker* chk, CheckContext* ctx1, const CheckContext* ctx2, const RowCol* rc)
{
    if (!ctx1->parent) abort();
    const size_t n = ptrmap_size(&ctx2->sym_to_ssa);
    for (size_t i = 0; i < n; ++i)
    {
        const Symbol* sym = ptrmap_nth_ptr(&ctx2->sym_to_ssa, i);
        size_t* ssa = ptrmap_nth_val(&ctx2->sym_to_ssa, i);
        // skip common symbols, handled separately
        if (ptrmap_find(&ctx1->sym_to_ssa, sym)) continue;
        size_t ssa1 = chkctx_get_ssa_rec(ctx1->parent, sym);
        if (ssa1 && ssa1 != *ssa)
        {
            const ValueInfo* v1 = check_get_ssa_info_rec(chk, ctx1, *ssa);
            if (!v1) abort();
            ValueInfo w = *v1;
            const ValueInfo* v2 = check_get_ssa_info_rec(chk, ctx2, ssa1);
            check_merge_values(chk, &w, v2, rc);
            ptrmap_set(&ctx1->sym_to_ssa, sym, check_push_ssa(chk, &w, rc));
        }
    }
}

static void check_merge_context(Checker* chk, CheckContext* ctx1, const CheckContext* ctx2, const RowCol* rc)
{
    check_merge_context_matching(chk, ctx1, ctx2, rc);
    check_merge_context_missing(chk, ctx1, ctx2, rc);

    ValueInfo* info = ctx1->info.data;
    const size_t m = array_size(&ctx1->info, sizeof(ValueInfo));
    size_t k = 0;
    for (size_t j = 0; j < m; ++j)
    {
        // merge refinements
        const ValueInfo* v = chkctx_get_ssa_info(ctx2, info[j].ref_ssa);
        if (v)
        {
            check_merge_values(chk, info + j, v, rc);
            if (k != j) info[k] = info[j];
            ++k;
        }
    }
    array_shrink(&ctx1->info, k, sizeof(ValueInfo));
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
        parser_tok_error(tok, "error: possible signed overflow in add\n%s\n", chk->fmt_tmp.data);
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

static ValueInfo* check_refine_ssa(Checker* chk, CheckContext* ctx, size_t ssa)
{
    ValueInfo* info = chkctx_get_ssa_info(ctx, ssa);
    if (!info)
    {
        info = array_push(&ctx->info, check_get_ssa_info_rec(chk, ctx->parent, ssa), sizeof(ValueInfo));
    }
    return info;
}

// static ValueInfo* check_refine_sym(Checker* chk, CheckContext* ctx, const Symbol* sym)
// {
//     return check_refine_ssa(chk, ctx, chkctx_get_ssa(ctx, sym));
// }

static void check_refine_ssa_true(Checker* chk, CheckContext* ctx, size_t ssa)
{
    ValueInfo* info = check_refine_ssa(chk, ctx, ssa);
    switch (info->kind)
    {
        case value_info_address:
            info->or_null = 0;
            if (!info->or_obj) ctx->is_void = 1;
            break;
        case value_info_integer:
            if (!interval_remove_0(&info->val, info->val)) ctx->is_void = 1;
            break;
        default: break;
    }
}
static void check_refine_ssa_false(Checker* chk, CheckContext* ctx, size_t ssa)
{
    ValueInfo* info = check_refine_ssa(chk, ctx, ssa);
    switch (info->kind)
    {
        case value_info_address:
            info->or_obj = 0;
            if (!info->or_null) ctx->is_void = 1;
            break;
        case value_info_integer:
            if (!interval_remove_nonzero(&info->val, info->val)) ctx->is_void = 1;
            break;
        default: break;
    }
}
static void check_refine_ssa_rel(Checker* chk,
                                 CheckContext* ctx,
                                 size_t ssa,
                                 const ValueInfo* rhs,
                                 unsigned int token_type,
                                 int is_signed,
                                 const RowCol* rc)
{
    ValueInfo* info = check_refine_ssa(chk, ctx, ssa);
    if (rhs->kind == value_info_bottom || info->kind == value_info_bottom) return;
    if (rhs->kind != info->kind)
    {
        parser_ferror(rc, "error: cannot compare values of different domains\n");
        ctx->is_void = 1;
        return;
    }
    switch (rhs->kind)
    {
        case value_info_address:
            if ((info->or_null || rhs->or_null) && token_type != TOKEN_SYM2('=', '=') &&
                token_type != TOKEN_SYM2('!', '='))
            {
                parser_ferror(rc, "error: can only compare for equality of nullable pointers\n");
                ctx->is_void = 1;
                return;
            }
            switch (token_type)
            {
                case TOKEN_SYM2('=', '='):
                    if (info->or_null != rhs->or_null && info->or_obj != rhs->or_obj)
                        ctx->is_void = 1;
                    else if (!rhs->or_obj)
                        info->or_obj = 0;
                    else if (!rhs->or_null)
                        info->or_null = 0;
                    break;
                case TOKEN_SYM2('!', '='):
                    if (!(info->or_null && rhs->or_obj) && !(info->or_obj && rhs->or_null))
                        ctx->is_void = 1;
                    else if (!rhs->or_obj)
                        info->or_null = 0;
                    else if (!rhs->or_null)
                        info->or_obj = 0;
                    break;
                default:
                    parser_ferror(rc, "error: unimplemented pointer comparison\n");
                    ctx->is_void = 1;
                    return;
            }
            break;
        case value_info_integer:
            switch (token_type)
            {
                int res;
                case TOKEN_SYM2('=', '='):
                    if (!interval_relation_eq(&info->val, rhs->val)) ctx->is_void = 1;
                    break;
                case TOKEN_SYM2('!', '='):
                    if (!interval_relation_neq(&info->val, rhs->val)) ctx->is_void = 1;
                    break;
                case TOKEN_SYM1('<'):
                    res = is_signed ? interval_relation_lti(&info->val, interval_signed_max(rhs->val))
                                    : interval_relation_ltu(&info->val, interval_unsigned_max(rhs->val));
                    if (!res) ctx->is_void = 1;
                    break;
                case TOKEN_SYM2('>', '='):
                    res = is_signed ? interval_relation_gtei(&info->val, interval_signed_max(rhs->val))
                                    : interval_relation_gteu(&info->val, interval_unsigned_max(rhs->val));
                    if (!res) ctx->is_void = 1;
                    break;
                case TOKEN_SYM1('>'):
                    res = is_signed ? interval_relation_gti(&info->val, interval_signed_max(rhs->val))
                                    : interval_relation_gtu(&info->val, interval_unsigned_max(rhs->val));
                    if (!res) ctx->is_void = 1;
                    break;
                case TOKEN_SYM2('<', '='):
                    res = is_signed ? interval_relation_ltei(&info->val, interval_signed_max(rhs->val))
                                    : interval_relation_lteu(&info->val, interval_unsigned_max(rhs->val));
                    if (!res) ctx->is_void = 1;
                    break;
                default:
                    parser_ferror(rc, "error: unimplemented integer comparison\n");
                    ctx->is_void = 1;
                    return;
            }
            break;
        default: break;
    }
}

static unsigned int negate_relation(unsigned int token_type)
{
    switch (token_type)
    {
        case TOKEN_SYM2('!', '='): return TOKEN_SYM2('=', '=');
        case TOKEN_SYM2('=', '='): return TOKEN_SYM2('!', '=');
        case TOKEN_SYM2('>', '='): return TOKEN_SYM1('<');
        case TOKEN_SYM2('<', '='): return TOKEN_SYM1('>');
        case TOKEN_SYM1('<'): return TOKEN_SYM2('>', '=');
        case TOKEN_SYM1('>'): return TOKEN_SYM2('<', '=');
        default: abort();
    }
}
static unsigned int negate_reverse_relation(unsigned int token_type)
{
    switch (token_type)
    {
        case TOKEN_SYM2('!', '='): return TOKEN_SYM2('=', '=');
        case TOKEN_SYM2('=', '='): return TOKEN_SYM2('!', '=');
        case TOKEN_SYM2('>', '='): return TOKEN_SYM1('>');
        case TOKEN_SYM2('<', '='): return TOKEN_SYM1('<');
        case TOKEN_SYM1('<'): return TOKEN_SYM2('<', '=');
        case TOKEN_SYM1('>'): return TOKEN_SYM2('>', '=');
        default: abort();
    }
}
static unsigned int reverse_relation(unsigned int token_type)
{
    switch (token_type)
    {
        case TOKEN_SYM2('=', '='): return TOKEN_SYM2('=', '=');
        case TOKEN_SYM2('!', '='): return TOKEN_SYM2('!', '=');
        case TOKEN_SYM2('>', '='): return TOKEN_SYM2('<', '=');
        case TOKEN_SYM2('<', '='): return TOKEN_SYM2('>', '=');
        case TOKEN_SYM1('<'): return TOKEN_SYM1('>');
        case TOKEN_SYM1('>'): return TOKEN_SYM1('<');
        default: abort();
    }
}

static void check_read_sym(Checker* chk, const Symbol* sym, ValueInfo* result, const RowCol* rc)
{
    size_t ssa = chkctx_get_ssa_rec(chk->ctx, sym);
    const ValueInfo* info = check_get_ssa_info_rec(chk, chk->ctx, ssa);
    if (info)
    {
        if (info->kind == value_info_uninitialized)
        {
            parser_ferror(rc, "error: possible uninitialized read: %s\n", sym->name);
            *result = s_valueinfo_any;
        }
        else
        {
            *result = *info;
        }
    }
    else
    {
        parser_ferror(rc, "error: no info for symbol: %s\n", sym->name);
        *result = s_valueinfo_any;
    }
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
static void check_cond(Checker* chk, const Expr* e, ValueInfo* result, CheckContext* true_ctx, CheckContext* false_ctx);

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
            switch (result->kind)
            {
                case value_info_address:
                    if (result->or_null)
                    {
                        parser_tok_error(e->tok, "error: cannot prove non-null\n");
                    }
                    else if (result->sym)
                    {
                        fprintf(stderr, "Prove: Sym: %s\n", result->sym->name ? result->sym->name : "(anon)");
                    }
                    else
                    {
                        fprintf(stderr, "Prove: Sym unknown\n");
                    }
                    break;
                case value_info_integer:
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
                case value_info_bottom: break;
                default: parser_tok_error(e->tok, "error: cannot prove value\n");
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

static void check_bool_expr(Checker* chk, const Expr* e, ValueInfo* result)
{
    check_expr(chk, e, result);
    value_cast_to_bool(result);
}

static void check_ExprAndOr(Checker* chk, const ExprAndOr* e, ValueInfo* result)
{
    check_bool_expr(chk, e->lhs, result);
    if (result->kind == value_info_bottom) return;
    int nonzero = interval_contains_nonzero(result->val);
    int zero = interval_contains_0(result->val);
    if (e->tok->type == TOKEN_SYM2('&', '&'))
    {
        if (nonzero)
        {
            check_bool_expr(chk, e->rhs, result);
            if (result->kind == value_info_bottom) return;
            if (zero) result->val = interval_merge(s_interval_zero, result->val);
        }
    }
    if (e->tok->type == TOKEN_SYM2('|', '|'))
    {
        if (zero)
        {
            check_bool_expr(chk, e->rhs, result);
            if (result->kind == value_info_bottom) return;
            if (nonzero) result->val = interval_merge(s_interval_one, result->val);
        }
    }
}
static void check_cond_ExprAndOr(
    Checker* chk, const ExprAndOr* e, ValueInfo* result, CheckContext* true_ctx, CheckContext* false_ctx)
{
    CheckContext true_ctx2 = {0};
    CheckContext false_ctx2 = {0};
    CheckContext dup = {0};
    check_cond(chk, e->lhs, result, true_ctx, false_ctx);
    value_cast_to_bool(result);
    if (result->kind == value_info_bottom) goto fail;
    int nonzero = interval_contains_nonzero(result->val);
    int zero = interval_contains_0(result->val);
    if (e->tok->type == TOKEN_SYM2('&', '&'))
    {
        if (nonzero)
        {
            CheckContext* ctx = chk->ctx;
            chk->ctx = true_ctx;
            true_ctx2.parent = true_ctx;
            false_ctx2.parent = true_ctx;
            check_cond(chk, e->rhs, result, &true_ctx2, &false_ctx2);
            chk->ctx = ctx;
            value_cast_to_bool(result);
            if (zero) result->val = interval_merge(s_interval_zero, result->val);

            chkctx_clone(&dup, true_ctx);
            false_ctx2.parent = &dup;
            check_flatten_context(chk, &false_ctx2);
            check_flatten_context(chk, &true_ctx2);
            check_merge_context(chk, false_ctx, &dup, token_rc(e->tok));
        }
    }
    else
    {
        if (zero)
        {
            CheckContext* ctx = chk->ctx;
            chk->ctx = false_ctx;
            true_ctx2.parent = false_ctx;
            false_ctx2.parent = false_ctx;
            check_cond(chk, e->rhs, result, &true_ctx2, &false_ctx2);
            chk->ctx = ctx;
            value_cast_to_bool(result);
            if (nonzero) result->val = interval_merge(s_interval_one, result->val);

            chkctx_clone(&dup, false_ctx);
            true_ctx2.parent = &dup;
            check_flatten_context(chk, &false_ctx2);
            check_flatten_context(chk, &true_ctx2);
            check_merge_context(chk, true_ctx, &dup, token_rc(e->tok));
        }
    }
fail:
    chkctx_destroy(&false_ctx2);
    chkctx_destroy(&true_ctx2);
    chkctx_destroy(&dup);
}

static void check_ExprBinOp_finish(Checker* chk, const ExprBinOp* e, ValueInfo* result, ValueInfo* rhs)
{
    if (rhs->kind == value_info_bottom) *result = s_valueinfo_any;
    if (result->kind == value_info_bottom) return;
    if (rhs->kind != value_info_integer || result->kind != value_info_integer)
    {
        parser_tok_error(e->tok, "error: unimplemented ExprBinOp on non-integers\n");
        *result = s_valueinfo_any;
        return;
    }
    result->val = interval_cast(result->val, e->common_sz);
    rhs->val = interval_cast(rhs->val, e->common_sz);
    switch (e->tok->type)
    {
        case TOKEN_SYM1('/'):
            if (interval_contains_0(rhs->val))
            {
                array_clear(&chk->fmt_tmp);
                interval_fmt(&chk->fmt_tmp, rhs->val);
                array_push_byte(&chk->fmt_tmp, '\0');
                parser_tok_error(e->tok,
                                 "error: possible divide by zero\n"
                                 "    Divisor range: %s\n",
                                 chk->fmt_tmp.data);
                *result = s_valueinfo_any;
                return;
            }
            else if (e->sizing.is_signed && interval_contains(result->val, INT64_MIN) &&
                     interval_contains(rhs->val, -1))
            {
                array_clear(&chk->fmt_tmp);
                interval_fmt(&chk->fmt_tmp, rhs->val);
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
                    valinfo_init_interval(result, interval_div(result->val, rhs->val));
                }
                else
                {
                    valinfo_init_interval(result, interval_udiv(result->val, rhs->val));
                }
            }
            break;
        case TOKEN_SYM1('*'): valinfo_init_interval(result, interval_mult(result->val, rhs->val)); break;
        case TOKEN_SYM1('<'):
            valinfo_init_interval(result,
                                  result->val.sz.is_signed ? interval_lt(result->val, rhs->val)
                                                           : interval_ltu(result->val, rhs->val));
            break;
        case TOKEN_SYM1('>'):
            valinfo_init_interval(result,
                                  result->val.sz.is_signed ? interval_lt(rhs->val, result->val)
                                                           : interval_ltu(rhs->val, result->val));
            break;
        case TOKEN_SYM2('<', '='):
            valinfo_init_interval(result,
                                  result->val.sz.is_signed ? interval_lte(result->val, rhs->val)
                                                           : interval_lteu(result->val, rhs->val));
            break;
        case TOKEN_SYM2('>', '='):
            valinfo_init_interval(result,
                                  result->val.sz.is_signed ? interval_lte(rhs->val, result->val)
                                                           : interval_lteu(rhs->val, result->val));
            break;
        case TOKEN_SYM2('=', '='):
            valinfo_init_interval(result,
                                  result->val.sz.is_signed ? interval_eq(result->val, rhs->val)
                                                           : interval_equ(result->val, rhs->val));
            break;
        default:
            parser_tok_error(e->tok, "error: unimplemented ExprBinOp type (%s)\n", token_str(chk->p, e->tok));
            *result = s_valueinfo_any;
    }
}

static void check_ExprBinOp(Checker* chk, const ExprBinOp* e, ValueInfo* result)
{
    check_expr(chk, e->lhs, result);
    if (e->tok->type == TOKEN_SYM1(',')) return check_expr(chk, e->rhs, result);
    ValueInfo rhs;
    check_expr(chk, e->rhs, &rhs);
    check_ExprBinOp_finish(chk, e, result, &rhs);
}

static void check_cond_ExprBinOp(
    Checker* chk, const ExprBinOp* e, ValueInfo* result, CheckContext* true_ctx, CheckContext* false_ctx)
{
    check_expr(chk, e->lhs, result);
    if (e->tok->type == TOKEN_SYM1(',')) return check_cond(chk, e->rhs, result, true_ctx, false_ctx);
    ValueInfo rhs;
    check_expr(chk, e->rhs, &rhs);
    if (result->ref_ssa)
    {
        check_refine_ssa_rel(
            chk, true_ctx, result->ref_ssa, &rhs, e->tok->type, e->common_sz.is_signed, token_rc(e->tok));
        check_refine_ssa_rel(chk,
                             false_ctx,
                             result->ref_ssa,
                             &rhs,
                             negate_relation(e->tok->type),
                             e->common_sz.is_signed,
                             token_rc(e->tok));
    }
    if (rhs.ref_ssa)
    {
        check_refine_ssa_rel(chk,
                             true_ctx,
                             rhs.ref_ssa,
                             result,
                             reverse_relation(e->tok->type),
                             e->common_sz.is_signed,
                             token_rc(e->tok));

        check_refine_ssa_rel(chk,
                             false_ctx,
                             rhs.ref_ssa,
                             result,
                             negate_reverse_relation(e->tok->type),
                             e->common_sz.is_signed,
                             token_rc(e->tok));
    }
    check_ExprBinOp_finish(chk, e, result, &rhs);
}

static void check_ExprUnOp(Checker* chk, const ExprUnOp* e, ValueInfo* result)
{
    check_expr(chk, e->lhs, result);
    switch (result->kind)
    {
        case value_info_bottom: return;
        case value_info_integer:
            switch (e->tok->type)
            {
                case TOKEN_SYM1('-'): result->val = chk_neg_ofchk(chk, result->val, e->tok); return;
                default:
                    parser_tok_error(e->tok, "error: unimplemented ExprUnOp type (%s)\n", token_str(chk->p, e->tok));
                    *result = s_valueinfo_any;
            }
            break;
        default:
            parser_tok_error(e->tok, "error: unimplemented ExprUnOp on non-integers\n");
            *result = s_valueinfo_any;
            return;
    }
}

static void check_ExprAdd(Checker* chk, const ExprAdd* e, ValueInfo* result)
{
    ValueInfo rhs;
    check_expr(chk, e->lhs, result);
    check_expr(chk, e->rhs, &rhs);
    if (rhs.kind == value_info_bottom) *result = s_valueinfo_any;
    if (result->kind == value_info_bottom) return;
    if (result->kind != value_info_integer)
    {
        parser_tok_error(e->lhs->tok, "error: expected integer in ExprAdd check\n");
        *result = s_valueinfo_any;
        return;
    }
    if (rhs.kind != value_info_integer)
    {
        parser_tok_error(e->rhs->tok, "error: expected integer in ExprAdd check\n");
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

static void check_ExprDeref(Checker* chk, const ExprDeref* e, ValueInfo* result)
{
    check_expr(chk, e->lhs, result);
    switch (result->kind)
    {
        case value_info_bottom: return;
        case value_info_address:
            if (result->or_null)
            {
                parser_tok_error(e->tok,
                                 result->or_obj ? "error: possible null pointer dereference\n"
                                                : "error: definite null pointer dereference\n");
                *result = s_valueinfo_any;
                return;
            }
            if (!e->take_address)
            {
                if (result->sym)
                {
                    check_read_sym(chk, result->sym, result, token_rc(e->tok));
                }
                else
                {
                    TypeStrBuf buf = result->sym_type;
                    tsb_remove_pointer(&buf);
                    check_any_from_type(chk, result, &buf, token_rc(e->tok));
                }
            }
            break;
        default: parser_tok_error(e->tok, "error: cannot dereference non-pointer value\n"); return;
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
        check_read_sym(chk, e->sym, result, token_rc(e->tok));
    }
}

static int can_alias(const TypeStrBuf* dst, const TypeStrBuf* src)
{
    char sbyte = tsb_byte(src);
    if (sbyte == TYPE_BYTE_CHAR || sbyte == TYPE_BYTE_SCHAR || sbyte == TYPE_BYTE_UCHAR)
        // character pointer aliases everything
        return 1;
    // todo: better inner pointer analysis
    return sbyte == tsb_byte(dst);
}

static void check_invalidate_tbaa(Checker* chk, const TypeStrBuf* tsb, const ValueInfo* v, const RowCol* rc)
{
    const size_t n = ptrmap_size(&chk->ctx->sym_to_ssa);
    for (size_t i = 0; i < n; ++i)
    {
        const Symbol* sym = ptrmap_nth_ptr(&chk->ctx->sym_to_ssa, i);
        size_t* ssa = ptrmap_nth_val(&chk->ctx->sym_to_ssa, i);
        if (v->ref_ssa == *ssa) continue;
        if (tsb_get_cvr(&sym->type.buf) & TYPESTR_CVR_C) continue; // cannot assign to const objects
        if (!can_alias(&sym->type.buf, tsb)) continue;
        ValueInfo w = *check_get_ssa_info_rec(chk, chk->ctx, *ssa);
        check_merge_values(chk, &w, v, rc);
        *ssa = check_push_ssa(chk, &w, rc);
    }
}

static void check_ExprAssign(Checker* chk, const ExprAssign* e, ValueInfo* result)
{
    check_expr(chk, e->lhs, result);
    ValueInfo rhs;
    check_expr(chk, e->rhs, &rhs);
    if (result->kind == value_info_address)
    {
        if (result->sym)
        {
            check_assign_value(chk, result->sym, &rhs, token_rc(e->tok));
        }
        else
        {
            TypeStrBuf pointed_ty;
            tsb_copy_elem_type(&pointed_ty, &result->sym_type);
            if (tsb_is_unknown(&pointed_ty))
            {
                parser_tok_error(e->tok, "error: cannot track assignment through unknown pointer\n");
                *result = s_valueinfo_any;
                chk->ctx->is_void = 1;
            }
            else
            {
                check_invalidate_tbaa(chk, &pointed_ty, &rhs, token_rc(e->tok));
            }
        }
    }
}

static void check_ExprField(Checker* chk, const ExprField* e, ValueInfo* result)
{
    check_expr(chk, e->lhs, result);
    if (result->kind == value_info_address)
    {
        if (result->or_null)
        {
            parser_tok_error(e->tok,
                             result->or_obj ? "error: possible null pointer dereference\n"
                                            : "error: definite null pointer dereference\n");
            *result = s_valueinfo_any;
            return;
        }

        result->sym_offset += e->field_offset;
    }
    else if (result->kind != value_info_bottom)
    {
        parser_tok_error(e->tok, "error: cannot dereference non-pointer value\n");
        *result = s_valueinfo_any;
    }
}

static void check_fnarg(Checker* chk, const Expr* e, int is_nonnull)
{
    ValueInfo arg;
    check_expr(chk, e, &arg);
    if (arg.kind == value_info_integer)
    {
        if (is_nonnull && interval_contains_0(arg.val))
        {
            parser_tok_error(e->tok, "error: function requires nonnull argument\n");
        }
    }
    if (arg.kind == value_info_address)
    {
        if (is_nonnull && arg.or_null)
        {
            parser_tok_error(e->tok, "error: function requires nonnull argument\n");
        }
        if (!arg.sym)
            parser_tok_error(e->tok, "error: unable to check pointer param validity\n");
        else
        {
            size_t ssa = chkctx_get_ssa_rec(chk->ctx, arg.sym);
            if (!ssa) abort();
            const ValueInfo* sym_val = check_get_ssa_info_rec(chk, chk->ctx, ssa);
            if (!sym_val) abort();
            if (sym_val->kind == value_info_uninitialized)
                parser_tok_error(e->tok, "error: passing pointer to uninitialized is not allowed\n");
        }
    }
}

static void check_ExprCall(Checker* chk, const ExprCall* e, ValueInfo* result)
{
    check_expr(chk, e->fn, result);
    if (result->kind == value_info_address)
    {
        if (result->or_null)
        {
            parser_tok_error(e->tok,
                             result->or_obj ? "error: possible null pointer dereference\n"
                                            : "error: definite null pointer dereference\n");
            *result = s_valueinfo_any;
            return;
        }

        const Attribute* attr = NULL;

        if (result->sym)
        {
            attr = &result->sym->last_decl->attr;
        }

        const CallParam* params = parser_params(chk->elab->p, e);
        for (size_t i = 0; i < e->param_extent; ++i)
        {
            const CallParam* const param = params + i;
            int is_nonnull = i < sizeof(attr->nonnull_addrs) && attr ? attr->nonnull_addrs[i] : 0;
            check_fnarg(chk, param->expr, is_nonnull);
        }
    }
    else if (result->kind != value_info_bottom)
    {
        parser_tok_error(e->tok, "error: cannot dereference non-pointer value\n");
        *result = s_valueinfo_any;
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
        DISPATCH_CHECK(ExprAndOr);
        DISPATCH_CHECK(ExprAdd);
        DISPATCH_CHECK(ExprDeref);
        DISPATCH_CHECK(ExprBuiltin);
        DISPATCH_CHECK(ExprUnOp);
        DISPATCH_CHECK(ExprField);
        DISPATCH_CHECK(ExprCall);
        DISPATCH_CHECK(ExprAssign);
#undef DISPATCH_CHECK
        default:
            parser_tok_error(e->tok, "error: unimplemented ast to check: %s\n", ast_kind_to_string(e->kind));
            *result = s_valueinfo_any;
    }
}

static void check_cond_impl(
    Checker* chk, const Expr* e, ValueInfo* result, CheckContext* true_ctx, CheckContext* false_ctx)
{
    switch (e->kind)
    {
#define DISPATCH_CHECK(type) DISPATCH(check_cond_, type, e, result, true_ctx, false_ctx)
        DISPATCH_CHECK(ExprBinOp);
        DISPATCH_CHECK(ExprAndOr);
#undef DISPATCH_CHECK
        default: return check_expr(chk, e, result);
    }
}

static void check_cond(Checker* chk, const Expr* e, ValueInfo* result, CheckContext* true_ctx, CheckContext* false_ctx)
{
    check_cond_impl(chk, e, result, true_ctx, false_ctx);
    if (result->ref_ssa)
    {
        check_refine_ssa_true(chk, true_ctx, result->ref_ssa);
        check_refine_ssa_false(chk, false_ctx, result->ref_ssa);
    }
}

static void check_DeclSpecs(Checker* chk, const DeclSpecs* e) { }
static void check_FnParam(Checker* chk, const Decl* d, int is_nonnull)
{
    ValueInfo info;
    check_any_from_decl(chk, &info, d, token_rc(d->tok));
    if (is_nonnull && info.kind == value_info_address && info.or_obj) info.or_null = 0;
    check_assign_value(chk, d->sym, &info, token_rc(d->tok));
}
static void check_Decl(Checker* chk, const Decl* e)
{
    const Symbol* const sym = e->sym;
    if (sym->def == e)
    {
        // fn
        const unsigned tym = typestr_mask(&sym->type);
        if (tym & TYPE_FLAGS_FUNCTION)
        {
            if (e->type->kind != AST_DECLFN) abort();
            const DeclFn* fn = (void*)e->type;

            const StmtDecls* const* stmtdecls = (const StmtDecls* const*)chk->elab->p->expr_seqs.data;
            const Decl* const* decls = (const Decl* const*)chk->elab->p->expr_seqs.data;
            FOREACH_SEQ(i, e->decl_list) { check_FnParam(chk, decls[i], 0); }
            size_t j = 0;
            FOREACH_SEQ(i, fn->seq)
            {
                int is_nonnull = j < sizeof(e->attr.nonnull_addrs) && e->attr.nonnull_addrs[j];
                const StmtDecls* stmt = stmtdecls[i];
                if (stmt->ast.kind != STMT_DECLS) abort();
                if (stmt->seq.ext != 1) abort();
                if (decls[stmt->seq.off]->ast.kind != AST_DECL) abort();
                check_FnParam(chk, decls[stmt->seq.off], is_nonnull);
                ++j;
            }

            check_stmt(chk, e->init);
        }
        else
        {
            check_assign_value(chk, sym, &s_valueinfo_uninit, token_rc(e->tok));
            if (e->init)
            {
                if (ast_kind_is_expr(e->init->kind))
                {
                    ValueInfo v;
                    check_expr(chk, (const Expr*)e->init, &v);
                    check_assign_value(chk, sym, &v, token_rc(e->tok));
                }
                else
                {
                    parser_tok_error(e->init->tok, "error: unimplemented check initializer\n");
                }
            }
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
static void check_StmtIf(Checker* chk, const StmtIf* e)
{
    ValueInfo result;
    CheckContext true_ctx = {.parent = chk->ctx}, false_ctx = {.parent = chk->ctx};
    check_cond(chk, e->cond, &result, &true_ctx, &false_ctx);
    chk->ctx = &true_ctx;
    check_stmt(chk, e->if_body);
    chk->ctx = &false_ctx;
    if (e->else_body) check_stmt(chk, e->else_body);
    chk->ctx = true_ctx.parent;
    check_merge_context(chk, &false_ctx, &true_ctx, token_rc(e->tok));
    check_flatten_context(chk, &false_ctx);
    chkctx_destroy(&true_ctx);
    chkctx_destroy(&false_ctx);
}
static void check_StmtReturn(Checker* chk, const StmtReturn* e)
{
    ValueInfo result;
    if (e->expr)
    {
        check_expr(chk, e->expr, &result);
        chk->ctx->is_void = 1;
    }
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
        DISPATCH_CHECK(StmtIf);
        DISPATCH_CHECK(StmtReturn);
        DISPATCH_CHECK(StmtBlock);
        default: parser_tok_error(ast->tok, "error: unknown stmt kind: %s\n", ast_kind_to_string(ast->kind)); return;
    }
}

int checker_check(Checker* chk)
{
    struct Parser* const p = chk->elab->p;
    if (!p->top) abort();
    CheckContext top = {0};
    chk->ctx = &top;
    check_stmt(chk, &p->top->ast);
    chkctx_destroy(&top);
    return parser_has_errors();
}
void checker_free(struct Checker* chk)
{
    array_destroy(&chk->fmt_tmp);
    array_destroy(&chk->ssa_info);
    my_free(chk);
}
