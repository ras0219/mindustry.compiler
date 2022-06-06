#pragma once

#include "compilermacros.h"

enum
{
    TYPESTR_BUF_SIZE = 32,
};

typedef struct TypeStr
{
    // length counted, first byte is length
    unsigned char buf[TYPESTR_BUF_SIZE];
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
