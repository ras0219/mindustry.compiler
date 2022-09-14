#pragma once

#include "ast.h"
#include "tac.h"
#include "typestr.h"

struct Decl;

typedef struct Attribute
{
    char ch;
} Attribute;

typedef struct TypeSymbol
{
    const char* name;
    struct DeclSpecs* last_decl;
    struct DeclSpecs* def;
    struct Symbol* first_member;
    size_t idx;
    Sizing size;
    size_t align;
} TypeSymbol;

typedef struct AstType
{
    INHERIT_AST;
} AstType;

#define INHERIT_ASTTYPE                                                                                                \
    union                                                                                                              \
    {                                                                                                                  \
        struct AstType ast_type;                                                                                       \
        struct                                                                                                         \
        {                                                                                                              \
            INHERIT_AST;                                                                                               \
        };                                                                                                             \
    }

typedef struct DeclSpecs
{
    INHERIT_ASTTYPE;

    /// encompassing function declaration
    struct Decl* parent;
    const char* name;
    struct Attribute attr;
    struct Symbol* _typedef;
    struct StmtBlock* suinit;
    struct StmtDecls* enum_init;
    struct DeclSpecs* prev_decl;
    struct TypeSymbol* sym;

    unsigned char is_struct : 1;
    unsigned char is_enum : 1;
    unsigned char is_union : 1;
    unsigned char is_const : 1;
    unsigned char is_volatile : 1;
    unsigned char is_unsigned : 1;
    unsigned char is_long : 1;
    unsigned char is_longlong : 1;
    unsigned char is_short : 1;
    unsigned char is_signed : 1;

    unsigned char is_register : 1;
    unsigned char is_extern : 1;
    unsigned char is_inline : 1;
    unsigned char is_static : 1;
    unsigned char is_typedef : 1;
    unsigned char is_stdcall : 1;
    unsigned char is_fn_arg : 1;
} DeclSpecs;

#define AST_STRUCT_AST_DECLSPEC DeclSpecs
#define AST_KIND_DeclSpecs AST_DECLSPEC

struct DeclPtr
{
    INHERIT_ASTTYPE;
    struct AstType* type;

    int is_const : 1;
    int is_volatile : 1;
    int is_restrict : 1;
};

typedef struct DeclFn
{
    INHERIT_ASTTYPE;
    struct AstType* type;

    unsigned char is_varargs : 1;
    // parameter list
    unsigned char is_param_list : 1;

    size_t offset;
    size_t extent;
} DeclFn;

#define AST_STRUCT_AST_DECLFN DeclFn
#define AST_KIND_DeclFn AST_DECLFN

typedef struct DeclArr
{
    INHERIT_ASTTYPE;
    struct AstType* type;

    struct Expr* arity;
    // elaboration
    unsigned int integer_arity;
} DeclArr;

#define AST_STRUCT_AST_DECLARR DeclArr
#define AST_KIND_DeclArr AST_DECLARR

typedef struct Decl
{
    INHERIT_AST;
    struct AstType* type;

    Attribute attr;
    DeclSpecs* specs;
    Ast* init;

    // View<StmtDecls*>
    struct
    {
        size_t offset;
        size_t extent;
    } decl_list;

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
    struct TypeSymbol* parent_su;
    struct Symbol* next_field;
    unsigned char is_enum_constant : 1;
    struct ExprLit* string_constant;

    // elaboration information
    unsigned char is_aggregate : 1;
    unsigned char is_array_or_fn : 1;
    TypeStr type;

    Sizing fn_ret_sizing;
    size_t field_offset;
    Sizing size;
    size_t align;

    size_t constinit_offset;
    int enum_value;

    // backend information

    /// For objects, this is the location on the stack frame where the object resides
    /// For functions, this is the location of the saved %rdi pointer
    TACAddress addr;
} Symbol;
