#pragma once

#include "ast.h"

struct DeclSpecs
{
    struct Token* type;

    int is_const : 1;
    int is_volatile : 1;
    int is_restrict : 1;

    int is_register : 1;
    int is_extern : 1;
    int is_static : 1;

    int is_typedef : 1;
};

struct RegMap
{
    int is_dirty : 1;
    int stack_addr;
    struct FreeVar rename;
    struct RegMap* next;
    struct RegMap** prev;
};

struct Symbol
{
    struct Expr kind;

    struct DeclSpecs specs;

    int incomplete : 1;
    int is_function : 1;
    int is_nonreentrant : 1;
    size_t arg_offset;
    size_t arg_count;
    const struct Token* token;
    const char* intrinsic_asm_str;

    // register mapping
    struct RegMap reg;
};
