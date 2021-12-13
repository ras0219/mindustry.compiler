#pragma once

#include "ast.h"
#include "typestr.h"

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

    int is_stdcall : 1;
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

struct RowCol* sym_to_rc(struct Symbol*);

#define ARRAY_ARITY_NONE (-1)

struct Decl
{
    struct Expr kind;

    struct Token* id;
    const char* name;
    struct Attribute attr;
    struct DeclSpecs specs;
    struct Decl* def;
    struct Expr* init;
    // encompassing function
    struct Decl* parent_decl;

    int pointer_levels;
    int array_arity;
    int is_array : 1;
    int is_function : 1;
    int is_argument : 1;
    int takes_local_addresses : 1;
    int is_nonreentrant : 1;
    int is_stackless : 1;
    int arg_index;
    size_t offset;
    size_t extent;

    struct Symbol sym;

    // elaboration information
    int elab_index;

    // backend information
    size_t frame_offset;
};

struct Decl* decl_get_def(struct Decl*);
