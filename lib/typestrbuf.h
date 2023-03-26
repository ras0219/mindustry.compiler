#pragma once

#include "compilermacros.h"
#include "fwd.h"

enum
{
    TYPESTR_BUF_SIZE = 16,
};

typedef struct TypeStrBuf
{
    // length counted, first byte is length
    unsigned char buf[TYPESTR_BUF_SIZE];
} TypeStrBuf;

#define X_TYPE_BYTE_INT(Y)                                                                                             \
    Y(TYPE_BYTE_CHAR, 'C')                                                                                             \
    Y(TYPE_BYTE_SCHAR, 'h')                                                                                            \
    Y(TYPE_BYTE_UCHAR, 'H')                                                                                            \
    Y(TYPE_BYTE_INT, 'i')                                                                                              \
    Y(TYPE_BYTE_SHORT, 's')                                                                                            \
    Y(TYPE_BYTE_LONG, 'l')                                                                                             \
    Y(TYPE_BYTE_LLONG, 'y')                                                                                            \
    Y(TYPE_BYTE_UINT, 'I')                                                                                             \
    Y(TYPE_BYTE_USHORT, 'S')                                                                                           \
    Y(TYPE_BYTE_ULONG, 'L')                                                                                            \
    Y(TYPE_BYTE_ULLONG, 'Y')

#define X_TYPE_BYTE_ARITH(Y)                                                                                           \
    X_TYPE_BYTE_INT(Y)                                                                                                 \
    Y(TYPE_BYTE_FLOAT, 'F')                                                                                            \
    Y(TYPE_BYTE_DOUBLE, 'D')                                                                                           \
    Y(TYPE_BYTE_LDOUBLE, 'E')

#define X_TYPE_BYTE(Y)                                                                                                 \
    Y(TYPE_BYTE_VOID, 'V')                                                                                             \
    Y(TYPE_BYTE_UUVALIST, '_')                                                                                         \
    Y(TYPE_BYTE_VARIADIC, '.')                                                                                         \
    X_TYPE_BYTE_ARITH(Y)                                                                                               \
    Y(TYPE_BYTE_STRUCT, '$')                                                                                           \
    Y(TYPE_BYTE_UNION, 'u')                                                                                            \
    Y(TYPE_BYTE_ENUM, 'e')                                                                                             \
    Y(TYPE_BYTE_CONST, 'c')                                                                                            \
    Y(TYPE_BYTE_VOLATILE, 'v')                                                                                         \
    Y(TYPE_BYTE_RESTRICT, 'r')                                                                                         \
    Y(TYPE_BYTE_POINTER, 'p')                                                                                          \
    Y(TYPE_BYTE_ARRAY, '[')                                                                                            \
    Y(TYPE_BYTE_UNK_ARRAY, ']')                                                                                        \
    Y(TYPE_BYTE_FUNCTION, '(')                                                                                         \
    Y(TYPE_BYTE_INVALID, '\0')

#define Y_COMMA(E, CH) E = CH,
enum
{
    X_TYPE_BYTE(Y_COMMA)
};
#undef Y_COMMA

enum
{
    TYPE_FLAGS_VOID = 1,
    TYPE_FLAGS_CHAR = 1 << 1,
    TYPE_FLAGS_INT = 1 << 2,
    TYPE_FLAGS_POINTER = 1 << 3,
    TYPE_FLAGS_STRUCT = 1 << 4,
    TYPE_FLAGS_UNION = 1 << 5,
    TYPE_FLAGS_FUNCTION = 1 << 6,
    TYPE_FLAGS_VAR = 1 << 7,
    TYPE_FLAGS_FLOAT = 1 << 8,
    TYPE_FLAGS_ARRAY = 1 << 9,
    TYPE_FLAGS_SIGNED = 1 << 10,
    TYPE_FLAGS_PROMOTE_INT = 1 << 11,
    TYPE_FLAGS_WIDTH1 = 0 << 12,
    TYPE_FLAGS_WIDTH2 = 1 << 12,
    TYPE_FLAGS_WIDTH4 = 2 << 12,
    TYPE_FLAGS_WIDTH8 = 3 << 12,

    TYPE_MASK_WIDTH = 3 << 12,
    TYPE_MASK_HAS_FIELDS = TYPE_FLAGS_UNION | TYPE_FLAGS_STRUCT,
    TYPE_MASK_ARITH = TYPE_FLAGS_FLOAT | TYPE_FLAGS_INT,
    TYPE_MASK_SCALAR = TYPE_FLAGS_POINTER | TYPE_MASK_ARITH,
    TYPE_MASK_OBJECT = TYPE_FLAGS_VOID | TYPE_FLAGS_INT | TYPE_FLAGS_POINTER | TYPE_FLAGS_STRUCT | TYPE_FLAGS_UNION,
    TYPE_MASK_FN_ARR = TYPE_FLAGS_FUNCTION | TYPE_FLAGS_ARRAY,
    TYPE_MASK_AGGREGATE = TYPE_FLAGS_ARRAY | TYPE_FLAGS_STRUCT | TYPE_FLAGS_UNION,
};

static __forceinline unsigned int typestr_bits_from_flags(unsigned flags)
{
    unsigned r = 8;
    if (flags & TYPE_FLAGS_WIDTH2) r *= 2;
    if (flags & TYPE_FLAGS_WIDTH4) r *= 4;
    return r;
}

extern const unsigned int s_typestr_mask_data[256];

// ignores CVR
unsigned char tsb_byte(const TypeStrBuf* ts);

static __forceinline unsigned int tsb_mask(const TypeStrBuf* ts) { return s_typestr_mask_data[tsb_byte(ts)]; }
void tsb_fmt(const struct TypeTable* tt, const TypeStrBuf* ts, struct Array* buf);
static __forceinline int tsb_is_unknown(const TypeStrBuf* ts) { return ts->buf[0] == 0; }

void tsb_append_offset(TypeStrBuf* s, unsigned int offset, char offset_type);
void tsb_add_array(TypeStrBuf* s, unsigned int n);
void tsb_add_pointer(TypeStrBuf* s);
void tsb_remove_pointer(TypeStrBuf* s);
void tsb_add_cvr(TypeStrBuf* s, unsigned int mask);
unsigned int tsb_get_cvr(const TypeStrBuf* ts);
unsigned int tsb_strip_cvr(TypeStrBuf* ts);
unsigned int tsb_get_offset(const TypeStrBuf* ts);
unsigned int tsb_pop_offset(TypeStrBuf* ts);
int tsb_decay(TypeStrBuf* t);
void tsb_from_decltype_Decl(const void* const* expr_seqs, struct TypeTable* tt, TypeStrBuf* s, const struct Decl* d);

struct TypeSymbol* tsb_get_decl(struct TypeTable* tt, const TypeStrBuf* ts);

unsigned long long tsb_get_size_i(const struct TypeTable* types, const TypeStrBuf* ts, int i, const struct RowCol* rc);

__forceinline unsigned long long tsb_get_size(const struct TypeTable* types,
                                              const TypeStrBuf* ts,
                                              const struct RowCol* rc)
{
    return tsb_get_size_i(types, ts, ts->buf[0], rc);
}

unsigned long long tsb_get_align_i(const struct TypeTable* types, const TypeStrBuf* ts, int i);

__forceinline unsigned long long tsb_get_align(const struct TypeTable* types, const TypeStrBuf* ts)
{
    return tsb_get_align_i(types, ts, ts->buf[0]);
}

unsigned long long tsb_get_add_size(const struct TypeTable* types, const TypeStrBuf* ts, const struct RowCol* rc);

enum
{
    TYPESTR_CVR_C = 1,
    TYPESTR_CVR_V = 2,
    TYPESTR_CVR_R = 4,
};

// fmt should contain exactly one %.*s
void tsb_error1(const struct RowCol* rc, const struct TypeTable* e, const char* fmt, const TypeStrBuf* ts);

// fmt should contain exactly two %.*s
void tsb_error2(
    const struct RowCol* rc, const struct TypeTable* e, const char* fmt, const TypeStrBuf* t1, const TypeStrBuf* t2);

int tsb_match(const TypeStrBuf* a, const TypeStrBuf* b);

struct Sizing tsb_calc_sizing(const struct TypeTable* types, const TypeStrBuf* ts, const struct RowCol* rc);
struct Sizing tsb_calc_sizing_zero_void(const struct TypeTable* types, const TypeStrBuf* ts, const struct RowCol* rc);
struct Sizing tsb_calc_elem_sizing(const struct TypeTable* types, const TypeStrBuf* ts, const struct RowCol* rc);
