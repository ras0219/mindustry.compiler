#pragma once

#include <stddef.h>
#include <stdint.h>

#include "fwd.h"
#include "sizing.h"

#define X_TACO_KIND(Y)                                                                                                 \
    Y(TACO_NOP)                                                                                                        \
    Y(TACO_LABEL)                                                                                                      \
    Y(TACO_ASSIGN)                                                                                                     \
    Y(TACO_ADD)                                                                                                        \
    Y(TACO_SUB)                                                                                                        \
    Y(TACO_DIV)                                                                                                        \
    Y(TACO_MOD)                                                                                                        \
    Y(TACO_EQ)                                                                                                         \
    Y(TACO_NEQ)                                                                                                        \
    Y(TACO_LTEQ)                                                                                                       \
    Y(TACO_LT)                                                                                                         \
    Y(TACO_LTEQU)                                                                                                      \
    Y(TACO_LTU)                                                                                                        \
    Y(TACO_MULT)                                                                                                       \
    Y(TACO_BAND)                                                                                                       \
    Y(TACO_BOR)                                                                                                        \
    Y(TACO_BXOR)                                                                                                       \
    Y(TACO_BNOT)                                                                                                       \
    Y(TACO_SHL)                                                                                                        \
    Y(TACO_SHR)                                                                                                        \
    Y(TACO_ARR)                                                                                                        \
    Y(TACO_GOTO)                                                                                                       \
    Y(TACO_BRZ)                                                                                                        \
    Y(TACO_BRNZ)                                                                                                       \
    Y(TACO_CTBZ)                                                                                                       \
    Y(TACO_JUMP)                                                                                                       \
    Y(TACO_PHI)                                                                                                        \
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
    Y(TACA_LNAME)                                                                                                      \
    Y(TACA_ALABEL)                                                                                                     \
    Y(TACA_LLABEL)                                                                                                     \
    Y(TACA_FRAME)                                                                                                      \
    Y(TACA_ARG)                                                                                                        \
    Y(TACA_REF)                                                                                                        \
    Y(TACA_CONST)                                                                                                      \
    Y(TACA_PARAM)                                                                                                      \
    Y(TACA_REG)

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

/*
 * ^^^ parent frame
 * | TACA_ARG
 * | ret addr
 * +---------- rsp at entry
 * | TACA_FRAME
 * | TACA_PARAM
 * +---------- rsp at subroutine call
 */

#define X_REGISTER(Y)                                                                                                  \
    Y(REG_RAX)                                                                                                         \
    Y(REG_RBX)                                                                                                         \
    Y(REG_RCX)                                                                                                         \
    Y(REG_RDX)                                                                                                         \
    Y(REG_RDI)                                                                                                         \
    Y(REG_RSI)                                                                                                         \
    Y(REG_R8)                                                                                                          \
    Y(REG_R9)                                                                                                          \
    Y(REG_R10)                                                                                                         \
    Y(REG_R11)

enum Register
{
    X_REGISTER(Y_COMMA)
};

#undef Y_COMMA
#undef Y_SUM

const char* taca_to_string(enum TACAKind k);
const char* taco_to_string(enum TACOKind k);
const char* register_to_string(enum Register k);

typedef struct TACAddress
{
    enum TACAKind kind;
    uint8_t is_addr : 1;
    Sizing sizing;
    union
    {
        const char* literal;
        const char* name;
        size_t imm;
        size_t reg;
        size_t ref;
        size_t const_idx;
        size_t arg_offset;
        size_t frame_offset;
        size_t param_offset;
        size_t alabel;
    };
} TACAddress;

void debug_taca(struct Array* arr, const TACAddress* addr);

typedef struct TACEntry
{
    enum TACOKind op;
    struct TACAddress arg1, arg2;
    const struct RowCol* rc;
} TACEntry;

void debug_tace(struct Array* arr, const TACEntry* tace);
