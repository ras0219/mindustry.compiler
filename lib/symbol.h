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
    INHERIT_AST;

    const char* name;
    /* stored +1 so 0 means uninitialized */
    uint32_t tt_idx;

    /* encompassing function or struct */
    struct Ast* parent;

    struct Decl* _typedef;
    struct StmtBlock* suinit;
    struct Decl* first_member;
    struct StmtDecls* enum_init;
    struct DeclSpecs* def;
    size_t size;
    size_t align;

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
    INHERIT_AST;

    struct Ast* type;
    struct Attribute attr;
    int is_const : 1;
    int is_volatile : 1;
    int is_restrict : 1;
};

struct DeclFn
{
    INHERIT_AST;

    struct Ast* type;
    unsigned is_varargs : 1;
    size_t offset;
    size_t extent;
};

struct DeclArr
{
    INHERIT_AST;

    struct Ast* type;
    struct Expr* arity;
    unsigned int integer_arity;
};

struct Decl
{
    INHERIT_AST;

    const char* name;
    struct Attribute attr;
    struct DeclSpecs* specs;
    struct Ast* type;

    struct Decl* def;
    struct Ast* init;

    // 0 means not an argument, 1 means first argument, etc
    unsigned int arg_index;
    union
    {
        unsigned int arity;
        int32_t fn_ret_sizing;
    };

    // elaboration information
    int elab_index;
    size_t size;
    size_t align;
    struct Decl* next_field;

    unsigned int is_enum_constant : 1;
    int enum_value;

    // backend information
    size_t frame_offset;
};
