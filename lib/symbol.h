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

    const struct Token* tok;
    struct Decl* type;
    const char* name;
    struct Attribute attr;

    unsigned int is_struct : 1;
    unsigned int is_enum : 1;
    unsigned int is_union : 1;
    unsigned int is_const : 1;
    unsigned int is_volatile : 1;
    unsigned int is_unsigned : 1;
    unsigned int is_long : 1;
    unsigned int is_longlong : 1;
    unsigned int is_short : 1;
    unsigned int is_signed : 1;

    unsigned int is_register : 1;
    unsigned int is_extern : 1;
    unsigned int is_inline : 1;
    unsigned int is_static : 1;
    unsigned int is_typedef : 1;
    unsigned int is_stdcall : 1;
};

struct DeclPtr
{
    struct Expr kind;
    const struct Token* tok;
    struct Expr* type;
    struct Attribute attr;
    int is_const : 1;
    int is_volatile : 1;
    int is_restrict : 1;
};

struct DeclFn
{
    struct Expr kind;
    const struct Token* tok;
    struct Expr* type;
    unsigned is_varargs : 1;
    size_t offset;
    size_t extent;
};

struct DeclArr
{
    struct Expr kind;
    const struct Token* tok;
    struct Expr* type;
    struct Expr* arity;
    unsigned int integer_arity;
};

struct Decl
{
    struct Expr kind;

    const struct Token* id;
    const char* name;
    // valid only if name == NULL
    size_t anon_idx;
    struct Attribute attr;
    struct DeclSpecs* specs;
    struct Decl* def;
    struct Expr* init;
    // encompassing function
    struct Decl* parent_decl;
    // 0 means not an argument, 1 means first argument, etc
    unsigned int arg_index;
    unsigned int is_enum_constant : 1;
    union
    {
        int enum_value;
        unsigned int arity;
    };

    struct Expr* type;

    // elaboration information
    int elab_index;

    // backend information
    size_t frame_offset;
    size_t size;
    size_t align;
};

const struct RowCol* decl_to_rc(const struct Decl*);

struct Decl* decl_get_def(struct Decl*);
