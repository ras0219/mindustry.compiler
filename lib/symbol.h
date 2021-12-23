#pragma once

#include "ast.h"
#include "typestr.h"

struct Attribute
{
    const char* symname;
    const char* asmstr;
    int is_nonreentrant : 1;
};

struct DeclSpecs
{
    struct Expr kind;

    struct Token* tok;
    struct Decl* type;
    const char* name;
    struct Attribute attr;

    int is_struct : 1;
    int is_enum : 1;
    int is_union : 1;
    int is_const : 1;
    int is_volatile : 1;
    int is_unsigned : 1;
    int is_signed : 1;

    int is_register : 1;
    int is_extern : 1;
    int is_static : 1;

    int is_typedef : 1;

    int is_stdcall : 1;
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

#define ARRAY_ARITY_NONE (-1)

struct DeclPtr
{
    struct Expr kind;
    struct Token* tok;
    struct Expr* type;
    struct Attribute attr;
    int is_const : 1;
    int is_volatile : 1;
    int is_restrict : 1;
};

struct DeclFn
{
    struct Expr kind;
    struct Token* tok;
    struct Expr* type;
    size_t offset;
    size_t extent;
};

struct DeclArr
{
    struct Expr kind;
    struct Token* tok;
    struct Expr* type;
    struct Expr* arity;
};

struct Decl
{
    struct Expr kind;

    struct Token* id;
    const char* name;
    struct Attribute attr;
    struct DeclSpecs* specs;
    struct Decl* def;
    struct Expr* init;
    // encompassing function
    struct Decl* parent_decl;
    // 0 means not an argument, 1 means first argument, etc
    int arg_index;

    struct Expr* type;

    // elaboration information
    int elab_index;

    // backend information
    size_t frame_offset;
    size_t size;
    size_t align;
};

struct RowCol* decl_to_rc(struct Decl*);

struct Decl* decl_get_def(struct Decl*);
