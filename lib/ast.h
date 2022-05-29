#pragma once

#include <stddef.h>
#include <stdint.h>

#include "compilermacros.h"
#include "freevar.h"
#include "litsuffix.h"
#include "sizing.h"

#define X_AST_POOL_KIND(Y)                                                                                             \
    Y(EXPR_REF)                                                                                                        \
    Y(EXPR_FIELD)                                                                                                      \
    Y(EXPR_LIT)                                                                                                        \
    Y(EXPR_CAST)                                                                                                       \
    Y(EXPR_BINOP)                                                                                                      \
    Y(EXPR_UNOP)                                                                                                       \
    Y(EXPR_CALL)                                                                                                       \
    Y(EXPR_BUILTIN)                                                                                                    \
    Y(AST_INIT)                                                                                                        \
    Y(AST_DINIT)                                                                                                       \
    Y(AST_DECL)                                                                                                        \
    Y(AST_DECLSPEC)                                                                                                    \
    Y(AST_DECLFN)                                                                                                      \
    Y(AST_DECLARR)                                                                                                     \
    Y(AST_DECLPTR)                                                                                                     \
    Y(STMT_DECLS)                                                                                                      \
    Y(STMT_RETURN)                                                                                                     \
    Y(STMT_GOTO)                                                                                                       \
    Y(STMT_IF)                                                                                                         \
    Y(STMT_SWITCH)                                                                                                     \
    Y(STMT_CASE)                                                                                                       \
    Y(STMT_LOOP)                                                                                                       \
    Y(STMT_BLOCK)                                                                                                      \
    Y(STMT_LABEL)                                                                                                      \
    Y(STMT_BREAK)                                                                                                      \
    Y(STMT_CONTINUE)                                                                                                   \
    Y(STMT_NONE)

#define X_AST_UNPOOL_KIND(Y) Y(TYPE_BUILTIN_INT) Y(TYPE_BUILTIN_CHAR)

#define X_AST_KIND(Y) X_AST_POOL_KIND(Y) X_AST_UNPOOL_KIND(Y)

#define Y_COMMA(Z) Z,
enum AstKind
{
    X_AST_KIND(Y_COMMA)
};
#define Y_LAST(Z) *0 + Z
enum
{
    AST_KIND_END_POOLS = 0 X_AST_POOL_KIND(Y_LAST),
    AST_KIND_END = 0 X_AST_KIND(Y_LAST),
    AST_KIND_COUNT,
};
#undef Y_LAST
#undef Y_COMMA

int ast_kind_is_expr(enum AstKind k);
const char* ast_kind_to_string(enum AstKind k);

typedef struct Ast
{
    enum AstKind kind;
    const struct Token* tok;
    unsigned int elaborated : 1;
} Ast;

#define INHERIT_AST                                                                                                    \
    union                                                                                                              \
    {                                                                                                                  \
        struct Ast ast;                                                                                                \
        struct                                                                                                         \
        {                                                                                                              \
            enum AstKind kind;                                                                                         \
            const struct Token* tok;                                                                                   \
            unsigned int elaborated : 1;                                                                               \
        };                                                                                                             \
    }

typedef struct Expr
{
    INHERIT_AST;

    Sizing sizing;
} Expr;

#define INHERIT_EXPR                                                                                                   \
    union                                                                                                              \
    {                                                                                                                  \
        struct Expr expr_base;                                                                                         \
        struct                                                                                                         \
        {                                                                                                              \
            INHERIT_AST;                                                                                               \
            Sizing sizing;                                                                                             \
        };                                                                                                             \
    }

typedef struct ExprLit
{
    INHERIT_EXPR;

    const char* text;
    uint64_t numeric;
    enum LitSuffix suffix;
} ExprLit;

#define AST_STRUCT_EXPR_LIT ExprLit
#define AST_KIND_ExprLit EXPR_LIT

struct ExprCast
{
    INHERIT_EXPR;

    struct Expr* expr;
    struct DeclSpecs* specs;
    struct Decl* type;
};
#define AST_STRUCT_EXPR_CAST ExprCast
#define AST_KIND_ExprCast EXPR_CAST

struct ExprBuiltin
{
    INHERIT_EXPR;

    struct Expr *expr1, *expr2;
    struct DeclSpecs* specs;
    struct Decl* type;
    size_t sizeof_size;
};
#define AST_STRUCT_EXPR_BUILTIN ExprBuiltin
#define AST_KIND_ExprBuiltin EXPR_BUILTIN

typedef struct ExprRef
{
    INHERIT_EXPR;

    struct Symbol* sym;
} ExprRef;
#define AST_STRUCT_EXPR_REF ExprRef
#define AST_KIND_ExprRef EXPR_REF

struct ExprField
{
    INHERIT_EXPR;

    int is_arrow : 1;
    const struct Token* field_tok;
    const char* fieldname;
    struct Expr* lhs;

    /* filled by elaboration */
    struct Symbol* sym;
};
#define AST_STRUCT_EXPR_FIELD ExprField
#define AST_KIND_ExprField EXPR_FIELD

struct ExprBinOp
{
    INHERIT_EXPR;

    struct Expr* lhs;
    struct Expr* rhs;
    // additional info for certain operations
    //  * '[','+', size of element
    int info;
};
#define AST_STRUCT_EXPR_BINOP ExprBinOp
#define AST_KIND_ExprBinOp EXPR_BINOP

struct ExprUnOp
{
    INHERIT_EXPR;

    struct Expr* lhs;
    // additional info for certain operations
    //  * '++'/'--', 1 if postfix
    //  * 'sizeof', size of decl
    uint32_t postfix : 1;
    uint32_t sizeof_;
};
#define AST_STRUCT_EXPR_UNOP ExprUnOp
#define AST_KIND_ExprUnOp EXPR_UNOP

typedef struct ExprCall
{
    INHERIT_EXPR;
    struct Expr* fn;
    size_t param_offset;
    size_t param_extent;
} ExprCall;
#define AST_STRUCT_EXPR_CALL ExprCall
#define AST_KIND_ExprCall EXPR_CALL

typedef struct CallParam
{
    Expr* expr;

    // elaboration info to track implicit conversion
    Sizing sizing;
    int32_t align;
} CallParam;

typedef struct Designator
{
    const char* field;
    struct Expr* array_expr;
} Designator;

typedef struct AstInit
{
    INHERIT_AST;

    // null signifies end of list
    struct Ast* init;
    size_t designator_offset;
    size_t designator_extent;
    struct AstInit* next;

    Sizing sizing;
    uint32_t offset;
    uint8_t is_char_arr;
} AstInit;
#define AST_STRUCT_AST_INIT AstInit
#define AST_KIND_AstInit AST_INIT

struct StmtReturn
{
    INHERIT_AST;

    // may be null
    struct Expr* expr;
};

typedef struct StmtDecls StmtDecls;
struct StmtDecls
{
    INHERIT_AST;

    struct DeclSpecs* specs;

    size_t offset;
    size_t extent;
};
#define AST_STRUCT_STMT_DECLS StmtDecls
#define AST_KIND_StmtDecls STMT_DECLS

struct StmtIf
{
    INHERIT_AST;

    struct Expr* cond;
    struct Ast* if_body;
    // may be null
    struct Ast* else_body;
};
struct StmtSwitch
{
    INHERIT_AST;

    struct Expr* expr;
    // body
    size_t offset;
    size_t extent;
};
struct StmtCase
{
    INHERIT_AST;

    struct Expr* expr;
    // filled by elaboration to the numeric case value
    size_t value;
};
struct StmtGoto
{
    INHERIT_AST;

    const struct Token* dst;
};
struct StmtLoop
{
    INHERIT_AST;

    struct Expr* cond;
    struct Ast* body;
    // null for while/dowhile
    struct Ast* init;
    // null for while/dowhile
    struct Expr* advance;

    int is_do_while : 1;
};
struct StmtBlock
{
    INHERIT_AST;

    size_t offset;
    size_t extent;
};
#define AST_STRUCT_STMT_BLOCK StmtBlock
#define AST_KIND_StmtBlock STMT_BLOCK

// TODO: represent designator-list
struct ASTDInit
{
    struct Expr kind;
    const struct Token* tok;
    struct Expr* designator;
    struct Expr* next;
};
struct StmtLabel
{
    INHERIT_AST;

    struct Ast* stmt;
};
struct StmtBreak
{
    INHERIT_AST;
};
struct StmtContinue
{
    INHERIT_AST;
};
