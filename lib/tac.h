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
};

enum TACAKind
{
    TACA_VOID,
    TACA_LITERAL,
    TACA_IMM,
    TACA_NAME,
    TACA_REF,
    TACA_CONST,
    TACA_PARAM,
};

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
    };
};

struct TACEntry
{
    enum TACO op : 8;
    struct TACAddress arg1, arg2;
};
