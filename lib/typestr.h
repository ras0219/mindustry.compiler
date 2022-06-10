#pragma once

#include "compilermacros.h"

struct RowCol;
struct Array;
struct Decl;
struct DeclSpecs;
struct TypeTable;
struct TypeSymbol;
struct AstType;

enum
{
    TYPESTR_BUF_SIZE = 16,
};

typedef struct Constant128
{
    unsigned long long lower;
    unsigned char is_signed;
} Constant128;

typedef struct Constant
{
    union
    {
        struct Symbol* sym;
        struct ExprLit* lit;
    };
    Constant128 value;
    unsigned char is_const : 1;
    unsigned char is_lvalue : 1;
    unsigned char is_sym : 1;
    unsigned char is_lit : 1;
} Constant;

static const struct Constant s_not_constant = {0};

typedef struct TypeStrBuf
{
    // length counted, first byte is length
    unsigned char buf[TYPESTR_BUF_SIZE];
} TypeStrBuf;

typedef struct TypeStr
{
    TypeStrBuf buf;
    Constant c;
} TypeStr;

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

void typestr_promote_integer(struct TypeStr* rty);

void typestr_implicit_conversion(struct TypeTable* types,
                                 const struct RowCol* rc,
                                 const struct TypeStr* orig_from,
                                 const struct TypeStr* orig_to);

void typestr_append_offset(struct TypeStr* s, unsigned int offset, char offset_type);

void typestr_add_array(struct TypeStr* s, unsigned int n);

void typestr_add_pointer(struct TypeStr* s);

void typestr_add_cvr(struct TypeStr* s, unsigned int mask);

void typestr_from_decltype_Decl(const struct Decl* const* expr_seqs,
                                struct TypeTable* tt,
                                struct TypeStr* s,
                                const struct Decl* d);

__forceinline static int typestr_is_constant_zero(const struct TypeStr* ts)
{
    return ts->c.is_const && !ts->c.is_lvalue && !ts->c.is_sym && 0 == ts->c.value.lower;
}

unsigned long long typestr_get_size_i(const struct TypeTable* types, const TypeStr* ts, int i, const struct RowCol* rc);

__forceinline unsigned long long typestr_get_size(const struct TypeTable* types,
                                                  const struct TypeStr* ts,
                                                  const struct RowCol* rc)
{
    return typestr_get_size_i(types, ts, ts->buf.buf[0], rc);
}

unsigned long long typestr_get_align_i(const struct TypeTable* types, const struct TypeStr* ts, int i);

__forceinline unsigned long long typestr_get_align(const struct TypeTable* types, const struct TypeStr* ts)
{
    return typestr_get_align_i(types, ts, ts->buf.buf[0]);
}

unsigned long long typestr_get_add_size(const struct TypeTable* types,
                                        const struct TypeStr* ts,
                                        const struct RowCol* rc);

struct Sizing typestr_calc_sizing(const struct TypeTable* types, const struct TypeStr* ts, const struct RowCol* rc);

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

    TYPE_COMMON_FLAGS_CHAR = TYPE_FLAGS_CHAR | TYPE_FLAGS_INT | TYPE_FLAGS_PROMOTE_INT | TYPE_FLAGS_WIDTH1,
    TYPE_COMMON_FLAGS_SHORT = TYPE_FLAGS_INT | TYPE_FLAGS_PROMOTE_INT | TYPE_FLAGS_WIDTH2,
    TYPE_COMMON_FLAGS_INT = TYPE_FLAGS_INT | TYPE_FLAGS_WIDTH4,
    TYPE_COMMON_FLAGS_LONG = TYPE_FLAGS_INT | TYPE_FLAGS_WIDTH8,
};

__forceinline static unsigned int typestr_bits_from_flags(unsigned flags)
{
    unsigned r = 8;
    if (flags & TYPE_FLAGS_WIDTH2) r *= 2;
    if (flags & TYPE_FLAGS_WIDTH4) r *= 4;
    return r;
}

extern const unsigned int s_typestr_mask_data[256];

// ignores CVR
unsigned char typestr_byte(const struct TypeStr* ts);

__forceinline static unsigned int typestr_mask(const struct TypeStr* ts)
{
    return s_typestr_mask_data[typestr_byte(ts)];
}

Constant128 mp_cast(Constant128 a, unsigned flags);

void typestr_apply_integral_type(TypeStr* dst, const TypeStr* src);
void typestr_assign_constant_value(TypeStr* t, Constant128 n);

static const struct TypeStr s_type_unknown = {0};
static const struct TypeStr s_type_void = {.buf = {1, TYPE_BYTE_VOID}};
static const struct TypeStr s_type_int = {.buf = {1, TYPE_BYTE_INT}};
static const struct TypeStr s_type_ptrdiff = {.buf = {1, TYPE_BYTE_LLONG}};
static const struct TypeStr s_type_char = {.buf = {1, TYPE_BYTE_CHAR}};

unsigned int typestr_get_offset_i(const struct TypeStr* ts, int i);

__forceinline static unsigned int typestr_get_offset(const struct TypeStr* ts)
{
    return typestr_get_offset_i(ts, ts->buf.buf[0]);
}

void typestr_fmt(const struct TypeTable* tt, const struct TypeStr* ts, struct Array* buf);

static __forceinline int typestr_is_pointer(const struct TypeStr* ts) { return typestr_byte(ts) == TYPE_BYTE_POINTER; }
static __forceinline int typestr_is_fn(const struct TypeStr* ts) { return typestr_byte(ts) == TYPE_BYTE_FUNCTION; }
static __forceinline int typestr_is_variadic(const struct TypeStr* ts)
{
    return typestr_byte(ts) == TYPE_BYTE_VARIADIC;
}
static __forceinline int typestr_is_aggregate(const struct TypeStr* ts)
{
    return !!(typestr_mask(ts) & TYPE_MASK_AGGREGATE);
}
static __forceinline int typestr_is_unknown(const struct TypeStr* ts) { return ts->buf.buf[0] == 0; }

enum
{
    TYPESTR_CVR_C = 1,
    TYPESTR_CVR_V = 2,
    TYPESTR_CVR_R = 4,
};
unsigned int typestr_strip_cvr(struct TypeStr* ts);
void typestr_remove_array(struct TypeStr* ts);
void typestr_dereference(struct TypeStr* ts);

// fmt should contain exactly one %.*s
void typestr_error1(const struct RowCol* rc, const struct TypeTable* e, const char* fmt, const struct TypeStr* ts);

// fmt should contain exactly two %.*s
void typestr_error2(const struct RowCol* rc,
                    const struct TypeTable* e,
                    const char* fmt,
                    const struct TypeStr* t1,
                    const struct TypeStr* t2);

// Does not compare constant values
int typestr_match(const struct TypeStr* tgt, const struct TypeStr* src);

unsigned int typestr_pop_offset(struct TypeStr* ts);

struct TypeSymbol* typestr_get_decl(struct TypeTable* tt, const struct TypeStr* ts);

void typestr_decay(struct TypeStr* t);

typedef struct FnTypeInfo
{
    unsigned int offset;
    unsigned short extent;
    unsigned short is_variadic;
} FnTypeInfo;

FnTypeInfo typestr_strip_fn(const struct TypeTable* tt, TypeStr* t);

const struct TypeStr* typestr_get_arg(const struct TypeTable* tt, const FnTypeInfo* info, unsigned index);

struct TypeTable* tt_alloc();
void tt_free(struct TypeTable* tt);
unsigned int tt_register(struct TypeTable* tt, struct TypeSymbol* tsym);
