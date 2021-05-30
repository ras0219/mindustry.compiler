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
    int stack_addr;
    struct RegMap* next;
    struct RegMap** prev;
    struct FreeVar rename;
};

struct Symbol
{
    struct Expr kind;
    struct Decl* decl;
    struct Type* type;

    int incomplete : 1;
    int is_nonreentrant : 1;

    // register mapping
    struct RegMap reg;
};

struct ArrSpan
{
    size_t offset;
    size_t extent;
};

struct Decl
{
    struct Expr kind;

    struct Token* id;
    struct Attribute attr;
    struct DeclSpecs specs;
    struct Decl* def;
    struct Expr* init;

    int is_function : 1;
    size_t offset;
    size_t extent;

    struct Symbol sym;

    // elaboration information
    int elab_index;
};

struct Decl* decl_get_def(struct Decl*);
