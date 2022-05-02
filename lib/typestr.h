#pragma once

#include "compilermacros.h"

enum
{
    TYPESTR_BUF_SIZE = 32,
};

typedef struct TypeStr
{
    // length counted, first byte is length
    char buf[TYPESTR_BUF_SIZE];
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
    Y(TYPE_BYTE_POINTER, 'p')                                                                                          \
    Y(TYPE_BYTE_ARRAY, '[')                                                                                            \
    Y(TYPE_BYTE_UNK_ARRAY, ']')                                                                                        \
    Y(TYPE_BYTE_FUNCTION, '(')

#define Y_COMMA(E, CH) E = CH,
enum
{
    X_TYPE_BYTE(Y_COMMA)
};
#undef Y_COMMA

// ignores CVR
__forceinline char typestr_byte(const struct TypeStr* ts)
{
    int i = ts->buf[0];
    if (ts->buf[i] == 'r') --i;
    if (ts->buf[i] == 'v') --i;
    if (ts->buf[i] == 'c') --i;
    return ts->buf[i];
}

#if 0
__forceinline void typestr_add_const(struct TypeStr* ts) { return ts->buf[ts->buf[0]] == 'c'; }

__forceinline int typestr_add_pointer(struct TypeStr* ts)
{
    if (!ts->used) return 1;
    if (ts->used == sizeof(ts->buf)) return 1;
    ts->buf[ts->used++] = 'p';
    return 0;
}
__forceinline int typestr_start_call(struct TypeStr* ts)
{
    if (!ts->used) return 1;
    if (ts->used == sizeof(ts->buf)) return 1;
    ts->buf[ts->used++] = '(';
    return 0;
}
int typestr_add_arg(struct TypeStr* ts, struct TypeStr* arg);

__forceinline int typestr_end_call(struct TypeStr* ts)
{
    if (!ts->used) return 1;
    if (ts->used == sizeof(ts->buf)) return 1;
    ts->buf[ts->used++] = ')';
    return 0;
}
int typestr_add_arr(struct TypeStr* ts, int arity);
#endif
#if 0
static struct TypeStr s_type_literal_int = {
    .buf = {'I', 'c'},
    .used = 2,
};
static struct TypeStr s_type_literal_cstr = {
    .buf = {'C', 'c', 'p'},
    .used = 3,
};
static struct TypeStr s_type_int = {
    .buf = {'I'},
    .used = 1,
};
static struct TypeStr s_type_long = {
    .buf = {'l'},
    .used = 1,
};
static struct TypeStr s_type_short = {
    .buf = {'s'},
    .used = 1,
};
static struct TypeStr s_type_void = {
    .buf = {'V'},
    .used = 1,
};
static struct TypeStr s_type_char = {
    .buf = {'C'},
    .used = 1,
};
static struct TypeStr s_type_mstr = {
    .buf = {'M'},
    .used = 1,
};
static struct TypeStr s_type_unit = {
    .buf = {'U'},
    .used = 1,
};
#endif