#pragma once

#include "compilermacros.h"
#include "constant.h"
#include "fwd.h"
#include "mp.h"
#include "sizing.h"
#include "typestrbuf.h"

typedef struct TypeStr
{
    TypeStrBuf buf;
    Constant c;
} TypeStr;

void typestr_promote_integer(struct TypeStr* rty);

void typestr_implicit_conversion2(struct TypeTable* types,
                                  const struct RowCol* rc,
                                  const struct TypeStr* orig_from,
                                  const struct TypeStrBuf* orig_to);

static __forceinline void typestr_implicit_conversion(struct TypeTable* types,
                                                      const struct RowCol* rc,
                                                      const struct TypeStr* orig_from,
                                                      const struct TypeStr* orig_to)
{
    return typestr_implicit_conversion2(types, rc, orig_from, &orig_to->buf);
}

void typestr_add_pointer(struct TypeStr* s);
static __forceinline void typestr_add_cvr(TypeStr* s, unsigned int mask) { return tsb_add_cvr(&s->buf, mask); }

void typestr_addressof(struct TypeStr* s);

void typestr_from_decltype_Decl(const void* const* expr_seqs,
                                struct TypeTable* tt,
                                struct TypeStr* s,
                                const struct Decl* d);

static __forceinline int typestr_is_constant_zero(const struct TypeStr* ts)
{
    return ts->c.is_const && !ts->c.is_lvalue && !ts->c.sym && 0 == ts->c.value.lower;
}

static __forceinline unsigned long long typestr_get_size(const struct TypeTable* types,
                                                         const struct TypeStr* ts,
                                                         const struct RowCol* rc)
{
    return tsb_get_size_i(types, &ts->buf, ts->buf.buf[0], rc);
}

static __forceinline unsigned long long typestr_get_align(const struct TypeTable* types, const struct TypeStr* ts)
{
    return tsb_get_align_i(types, &ts->buf, ts->buf.buf[0]);
}

static __forceinline unsigned long long typestr_get_add_size(const struct TypeTable* types,
                                                             const struct TypeStr* ts,
                                                             const struct RowCol* rc)
{
    return tsb_get_add_size(types, &ts->buf, rc);
}

static __forceinline struct Sizing typestr_try_calc_sizing(const struct TypeTable* types,
                                                           const struct TypeStr* ts,
                                                           const struct RowCol* rc)
{
    return tsb_try_calc_sizing(types, &ts->buf, rc);
}
static __forceinline struct Sizing typestr_calc_sizing(const struct TypeTable* types,
                                                       const struct TypeStr* ts,
                                                       const struct RowCol* rc)
{
    return tsb_calc_sizing(types, &ts->buf, rc);
}

static __forceinline struct Sizing typestr_calc_sizing_zero_void(const struct TypeTable* types,
                                                                 const struct TypeStr* ts,
                                                                 const struct RowCol* rc)
{
    return tsb_calc_sizing_zero_void(types, &ts->buf, rc);
}

static __forceinline struct Sizing typestr_calc_elem_sizing(const struct TypeTable* types,
                                                            const struct TypeStr* ts,
                                                            const struct RowCol* rc)
{
    return tsb_calc_elem_sizing(types, &ts->buf, rc);
}

// ignores CVR
static __forceinline unsigned char typestr_byte(const struct TypeStr* ts) { return tsb_byte(&ts->buf); }
static __forceinline unsigned int typestr_mask(const struct TypeStr* ts) { return tsb_mask(&ts->buf); }

void typestr_apply_integral_type(TypeStr* dst, const TypeStr* src);
void typestr_assign_constant_bool(TypeStr* t, int n);
void typestr_assign_constant_value(TypeStr* t, Constant128 n);

static const struct TypeStr s_type_unknown = {0};
static const struct TypeStr s_type_void = {.buf = {1, TYPE_BYTE_VOID}};
static const struct TypeStr s_type_int = {.buf = {1, TYPE_BYTE_INT}};
static const struct TypeStr s_type_uint = {.buf = {1, TYPE_BYTE_UINT}};
static const struct TypeStr s_type_ulong = {.buf = {1, TYPE_BYTE_ULONG}};
static const struct TypeStr s_type_ptrdiff = {.buf = {1, TYPE_BYTE_LLONG}};
static const struct TypeStr s_type_char = {.buf = {1, TYPE_BYTE_CHAR}};

void typestr_fmt(const struct TypeTable* tt, const struct TypeStr* ts, struct Array* buf);

static __forceinline unsigned int typestr_get_offset(const struct TypeStr* ts) { return tsb_get_offset(&ts->buf); }

static __forceinline int typestr_is_aggregate(const struct TypeStr* ts)
{
    return !!(typestr_mask(ts) & TYPE_MASK_AGGREGATE);
}
static __forceinline int typestr_is_unknown(const TypeStr* ts) { return ts->buf.buf[0] == 0; }
int typestr_is_char_array(const struct TypeStr* ts);

static __forceinline unsigned int typestr_strip_cvr(struct TypeStr* ts) { return tsb_strip_cvr(&ts->buf); }
static __forceinline unsigned int typestr_get_cvr(const TypeStr* ts) { return tsb_get_cvr(&ts->buf); }
void typestr_remove_array(struct TypeStr* ts);
void typestr_dereference(struct TypeStr* ts);
static __forceinline int typestr_is_const(const struct TypeStr* ts) { return typestr_get_cvr(ts) & TYPESTR_CVR_C != 0; }

// fmt should contain exactly one %.*s
void typestr_error1(const struct RowCol* rc, const struct TypeTable* e, const char* fmt, const struct TypeStr* ts);

// fmt should contain exactly two %.*s
void typestr_error2(const struct RowCol* rc,
                    const struct TypeTable* e,
                    const char* fmt,
                    const struct TypeStr* t1,
                    const struct TypeStr* t2);

static __forceinline unsigned int typestr_pop_offset(struct TypeStr* ts) { return tsb_pop_offset(&ts->buf); }

static __forceinline struct TypeSymbol* typestr_get_decl(struct TypeTable* tt, const struct TypeStr* ts)
{
    return tsb_get_decl(tt, &ts->buf);
}

void tsb_copy_elem_type(TypeStrBuf* out, const TypeStrBuf* in);

/// \return nonzero if address was taken
int typestr_decay(struct TypeStr* t);

typedef struct FnTypeInfo
{
    unsigned int offset;
    unsigned short extent;
    unsigned short is_variadic;
} FnTypeInfo;

FnTypeInfo typestr_strip_fn(const struct TypeTable* tt, TypeStr* t);

const TypeStrBuf* typestr_get_arg(const struct TypeTable* tt, const FnTypeInfo* info, unsigned index);

struct TypeTable* tt_alloc();
void tt_free(struct TypeTable* tt);
unsigned int tt_register(struct TypeTable* tt, struct TypeSymbol* tsym);
