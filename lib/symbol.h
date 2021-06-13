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

struct Attribute
{
    const char* symname;
    const char* asmstr;
    int is_nonreentrant : 1;
};

struct RegMap
{
    int is_dirty : 1;
    int is_const : 1;
    int is_global : 1;
    int stack_addr;
    struct RegMap* next;
    struct RegMap** prev;
    struct FreeVar rename;
    struct FreeVar mem_loc;
};

struct TypeStr
{
    char buf[31];
#ifndef NDEBUG
    char buf_zero_pad;
#endif
    unsigned char used : 5;
};
struct Symbol
{
    struct Expr kind;
    struct Decl* decl;
    struct TypeStr type;

    int incomplete : 1;
    int is_register : 1;
    int address_taken : 1;

    // register mapping
    struct RegMap reg;
};

struct ArrSpan
{
    size_t offset;
    size_t extent;
};

#define ARRAY_ARITY_NONE (-1)

struct Decl
{
    struct Expr kind;

    struct Token* id;
    struct Attribute attr;
    struct DeclSpecs specs;
    struct Decl* def;
    struct Expr* init;
    struct Decl* parent_decl;

    int pointer_levels;
    int array_arity;
    int is_array : 1;
    int is_function : 1;
    int takes_local_addresses : 1;
    int is_nonreentrant : 1;
    int is_stackless : 1;
    size_t offset;
    size_t extent;

    struct Symbol sym;

    // elaboration information
    int elab_index;
};

struct Decl* decl_get_def(struct Decl*);
