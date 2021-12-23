#pragma once

#include "fwd.h"

#define X_TACO_KIND(Y)                                                                                                 \
    Y(TACO_NOP)                                                                                                        \
    Y(TACO_ASSIGN)                                                                                                     \
    Y(TACO_ADD)                                                                                                        \
    Y(TACO_SUB)                                                                                                        \
    Y(TACO_DIV)                                                                                                        \
    Y(TACO_MULT)                                                                                                       \
    Y(TACO_GOTO)                                                                                                       \
    Y(TACO_BRZ)                                                                                                        \
    Y(TACO_PARAM)                                                                                                      \
    Y(TACO_CALL)                                                                                                       \
    Y(TACO_RETURN)                                                                                                     \
    Y(TACO_LEA)                                                                                                        \
    Y(TACO_STORE)                                                                                                      \
    Y(TACO_LOAD)                                                                                                       \
    Y(TACO_ARG)

#define X_TACA_KIND(Y)                                                                                                 \
    Y(TACA_VOID)                                                                                                       \
    Y(TACA_LITERAL)                                                                                                    \
    Y(TACA_IMM)                                                                                                        \
    Y(TACA_NAME)                                                                                                       \
    Y(TACA_NAME_ADDR)                                                                                                  \
    Y(TACA_FRAME)                                                                                                      \
    Y(TACA_FRAME_ADDR)                                                                                                 \
    Y(TACA_ARG)                                                                                                        \
    Y(TACA_ARG_ADDR)                                                                                                   \
    Y(TACA_REF)                                                                                                        \
    Y(TACA_CONST)                                                                                                      \
    Y(TACA_PARAM)

#define Y_SUM(Z) +1
#define Y_COMMA(Z) Z,
enum
{
    TACA_KIND_COUNT = X_TACA_KIND(Y_SUM),
    TACO_KIND_COUNT = X_TACO_KIND(Y_SUM),
};

enum TACOKind
{
    X_TACO_KIND(Y_COMMA)
};

enum TACAKind
{
    X_TACA_KIND(Y_COMMA)
};
#undef Y_COMMA
#undef Y_SUM

const char* taca_to_string(enum TACAKind k);
const char* taco_to_string(enum TACOKind k);

struct TACAddress
{
    enum TACAKind kind;
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
    enum TACOKind op;
    struct TACAddress arg1, arg2;
};
