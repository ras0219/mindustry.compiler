#pragma once

#include "fwd.h"

enum TACO
{
    TACO_NOP,
    TACO_ASSIGN,
    TACO_ADD,
    TACO_SUB,
    TACO_DIV,
    TACO_MULT,
    TACO_GOTO,
    TACO_BRZ,
    TACO_PARAM,
    TACO_CALL,
    TACO_RETURN,
    TACO_LEA,
    TACO_STORE,
    TACO_LOAD,
    TACO_ARG,
};

#define X_TACA_KIND(Y)                                                                                                 \
    Y(TACA_VOID)                                                                                                       \
    Y(TACA_LITERAL)                                                                                                    \
    Y(TACA_IMM)                                                                                                        \
    Y(TACA_NAME)                                                                                                       \
    Y(TACA_FRAME)                                                                                                      \
    Y(TACA_ARG)                                                                                                        \
    Y(TACA_REF)                                                                                                        \
    Y(TACA_CONST)                                                                                                      \
    Y(TACA_PARAM)

#define Y_SUM(Z) +1
#define Y_COMMA(Z) Z,
enum
{
    TACA_KIND_COUNT = X_TACA_KIND(Y_SUM),
};

enum TACAKind
{
    X_TACA_KIND(Y_COMMA)
};
#undef Y_COMMA
#undef Y_SUM

struct TACAddress
{
    enum TACAKind kind : 8;
    union
    {
        const char* literal;
        const char* name;
        size_t imm;
        size_t param_idx;
        size_t ref;
        size_t const_idx;
        size_t arg_idx;
        size_t frame_offset;
    };
};

struct TACEntry
{
    enum TACO op : 8;
    struct TACAddress arg1, arg2;
};
