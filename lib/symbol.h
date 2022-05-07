#pragma once

#include "ast.h"
#include "typestr.h"

typedef struct Attribute
{
} Attribute;

typedef struct TypeSymbol
{
    const char* name;
    struct DeclSpecs* last_decl;
    struct DeclSpecs* def;
    struct Symbol* first_member;
    size_t idx;
    size_t size;
    size_t align;
} TypeSymbol;

typedef struct DeclSpecs
{
    INHERIT_AST;

    /// encompassing function or struct
    Ast* parent;
    const char* name;
    struct Attribute attr;
    struct Symbol* _typedef;
    struct StmtBlock* suinit;
    struct StmtDecls* enum_init;
    struct DeclSpecs* prev_decl;
    struct TypeSymbol* sym;

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
} DeclSpecs;

#define AST_STRUCT_AST_DECLSPEC DeclSpecs
#define AST_KIND_DeclSpecs AST_DECLSPEC

struct DeclPtr
{
    INHERIT_AST;

    struct Ast* type;
    int is_const : 1;
    int is_volatile : 1;
    int is_restrict : 1;
};

typedef struct DeclFn
{
    INHERIT_AST;

    struct Ast* type;
    unsigned is_varargs : 1;
    size_t offset;
    size_t extent;
} DeclFn;

typedef struct DeclArr
{
    INHERIT_AST;

    struct Ast* type;
    struct Expr* arity;
    // elaboration
    unsigned int integer_arity;
} DeclArr;

#define AST_STRUCT_AST_DECLARR DeclArr
#define AST_KIND_DeclArr AST_DECLARR

typedef struct Decl
{
    INHERIT_AST;

    Attribute attr;
    DeclSpecs* specs;
    Ast* type;
    Ast* init;

    struct Decl* prev_decl;
    struct Symbol* sym;
} Decl;

#define AST_STRUCT_AST_DECL Decl
#define AST_KIND_Decl AST_DECL

typedef struct Symbol
{
    const char* name;
    Decl* last_decl;
    Decl* def;
    struct Symbol* next_field;

    // 0 means not an argument, 1 means first argument, etc
    unsigned int arg_index;

    // elaboration information
    TypeStr type;

    int32_t fn_ret_sizing;
    size_t size;
    size_t align;

    unsigned int is_enum_constant : 1;
    int enum_value;

    // backend information
    size_t frame_offset;
} Symbol;
