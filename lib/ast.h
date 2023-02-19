#pragma once

#include <stddef.h>
#include <stdint.h>

#include "compilermacros.h"
#include "freevar.h"
#include "litsuffix.h"
#include "seqview.h"
#include "sizing.h"
#include "typestr.h"

#define X_FOREACH_EXPR(Y)                                                                                              \
    Y(EXPR_REF)                                                                                                        \
    Y(EXPR_FIELD)                                                                                                      \
    Y(EXPR_LIT)                                                                                                        \
    Y(EXPR_CAST)                                                                                                       \
    Y(EXPR_BINOP)                                                                                                      \
    Y(EXPR_ADD)                                                                                                        \
    Y(EXPR_ASSIGN)                                                                                                     \
    Y(EXPR_TERNARY)                                                                                                    \
    Y(EXPR_UNOP)                                                                                                       \
    Y(EXPR_DEREF)                                                                                                      \
    Y(EXPR_ADDRESS)                                                                                                    \
    Y(EXPR_INCR)                                                                                                       \
    Y(EXPR_CALL)                                                                                                       \
    Y(EXPR_BUILTIN)

#define X_AST_POOL_KIND(Y)                                                                                             \
    X_FOREACH_EXPR(Y)                                                                                                  \
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

#define AST_FIELDS                                                                                                     \
    const struct Token* tok;                                                                                           \
    enum AstKind kind;                                                                                                 \
    unsigned char elaborated

typedef struct Ast
{
    AST_FIELDS;
} Ast;

#define INHERIT_AST                                                                                                    \
    union                                                                                                              \
    {                                                                                                                  \
        struct Ast ast;                                                                                                \
        struct                                                                                                         \
        {                                                                                                              \
            AST_FIELDS;                                                                                                \
        };                                                                                                             \
    }

#define EXPR_FIELDS                                                                                                    \
    INHERIT_AST;                                                                                                       \
    /* filled by elaboration */                                                                                        \
    unsigned char take_address;                                                                                        \
    Sizing sizing;                                                                                                     \
    struct Constant c

typedef struct Expr
{
    EXPR_FIELDS;
} Expr;

#define INHERIT_EXPR                                                                                                   \
    union                                                                                                              \
    {                                                                                                                  \
        struct Expr expr_base;                                                                                         \
        struct                                                                                                         \
        {                                                                                                              \
            EXPR_FIELDS;                                                                                               \
        };                                                                                                             \
    }

typedef struct ExprLit
{
    INHERIT_EXPR;

    struct Symbol* sym;
    const char* text;
    uint64_t numeric;
    enum LitSuffix suffix;
} ExprLit;

#define AST_STRUCT_EXPR_LIT ExprLit
#define AST_KIND_ExprLit EXPR_LIT

typedef struct ExprCast
{
    INHERIT_EXPR;

    struct Expr* expr;
    struct DeclSpecs* specs;
    struct Decl* type;
} ExprCast;
#define AST_STRUCT_EXPR_CAST ExprCast
#define AST_KIND_ExprCast EXPR_CAST

typedef struct ExprBuiltin
{
    INHERIT_EXPR;

    struct Expr *expr1, *expr2;
    struct DeclSpecs* specs;
    struct Decl* type;
    size_t sizeof_size;
} ExprBuiltin;
#define AST_STRUCT_EXPR_BUILTIN ExprBuiltin
#define AST_KIND_ExprBuiltin EXPR_BUILTIN

typedef struct ExprRef
{
    INHERIT_EXPR;

    struct Symbol* sym;
} ExprRef;
#define AST_STRUCT_EXPR_REF ExprRef
#define AST_KIND_ExprRef EXPR_REF

typedef struct ExprField
{
    INHERIT_EXPR;

    int is_arrow : 1;
    const struct Token* field_tok;
    const char* fieldname;
    struct Expr* lhs;

    /* filled by elaboration */
    struct Symbol* sym;
    size_t field_offset;
} ExprField;
#define AST_STRUCT_EXPR_FIELD ExprField
#define AST_KIND_ExprField EXPR_FIELD

typedef struct ExprBinOp
{
    INHERIT_EXPR;

    struct Expr* lhs;
    struct Expr* rhs;

    Sizing common_sz;
} ExprBinOp;
#define AST_STRUCT_EXPR_BINOP ExprBinOp
#define AST_KIND_ExprBinOp EXPR_BINOP

typedef struct ExprAdd
{
    INHERIT_EXPR;

    struct Expr* lhs;
    struct Expr* rhs;
    // additional info for certain operations
    //  * '[','+', size of element (positive)
    //  * '-', size of element (negative)
    int32_t mult;
} ExprAdd;
#define AST_STRUCT_EXPR_ADD ExprAdd
#define AST_KIND_ExprAdd EXPR_ADD

typedef struct ExprAssign
{
    INHERIT_EXPR;

    struct Expr* lhs;
    struct Expr* rhs;
    // for addition and subtraction, multiplier (positive) or divisor (negative)
    int32_t mult;
    // whether the op should be performed with signed numbers
    Sizing common_sz;
} ExprAssign;
#define AST_STRUCT_EXPR_ASSIGN ExprAssign
#define AST_KIND_ExprAssign EXPR_ASSIGN

typedef struct ExprTernary
{
    INHERIT_EXPR;

    struct Expr* cond;
    struct Expr* iftrue;
    struct Expr* iffalse;
} ExprTernary;
#define AST_STRUCT_EXPR_TERNARY ExprTernary
#define AST_KIND_ExprTernary EXPR_TERNARY

typedef struct ExprUnOp
{
    INHERIT_EXPR;

    struct Expr* lhs;
    // additional info for certain operations
    //  * 'sizeof', size of decl
    uint32_t sizeof_;
} ExprUnOp;
#define AST_STRUCT_EXPR_UNOP ExprUnOp
#define AST_KIND_ExprUnOp EXPR_UNOP

typedef struct ExprDeref
{
    INHERIT_EXPR;

    struct Expr* lhs;
} ExprDeref;
#define AST_STRUCT_EXPR_DEREF ExprDeref
#define AST_KIND_ExprDeref EXPR_DEREF

typedef struct ExprAddress
{
    INHERIT_EXPR;

    struct Expr* lhs;
} ExprAddress;
#define AST_STRUCT_EXPR_ADDRESS ExprAddress
#define AST_KIND_ExprAddress EXPR_ADDRESS

typedef struct ExprIncr
{
    INHERIT_EXPR;

    uint32_t postfix : 1;
    uint32_t sizeof_;

    struct Expr* lhs;
} ExprIncr;
#define AST_STRUCT_EXPR_INCR ExprIncr
#define AST_KIND_ExprIncr EXPR_INCR

typedef struct ExprCall
{
    INHERIT_EXPR;
    struct Expr* fn;
    size_t param_offset, param_extent;
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
    uint8_t is_aggregate_init;
    uint8_t is_braced_strlit;
} AstInit;
#define AST_STRUCT_AST_INIT AstInit
#define AST_KIND_AstInit AST_INIT

typedef struct StmtReturn
{
    INHERIT_AST;

    // may be null
    struct Expr* expr;
} StmtReturn;

typedef struct StmtDecls
{
    INHERIT_AST;

    struct DeclSpecs* specs;

    SeqView seq;
} StmtDecls;
#define AST_STRUCT_STMT_DECLS StmtDecls
#define AST_KIND_StmtDecls STMT_DECLS

typedef struct StmtIf
{
    INHERIT_AST;

    struct Expr* cond;
    struct Ast* if_body;
    // may be null
    struct Ast* else_body;
} StmtIf;

typedef struct StmtProve
{
    INHERIT_AST;

    struct Expr* cond;
} StmtProve;
#define AST_STRUCT_STMT_PROVE StmtProve
#define AST_KIND_StmtProve STMT_PROVE

typedef struct StmtSwitch
{
    INHERIT_AST;

    struct Expr* expr;
    // body
    SeqView seq;
} StmtSwitch;
#define AST_STRUCT_STMT_SWITCH StmtSwitch
#define AST_KIND_StmtSwitch STMT_SWITCH

typedef struct StmtCase
{
    INHERIT_AST;

    struct Expr* expr;
    // filled by elaboration to the numeric case value
    size_t value;
} StmtCase;
#define AST_STRUCT_STMT_CASE StmtCase
#define AST_KIND_StmtCase STMT_CASE

typedef struct StmtGoto
{
    INHERIT_AST;

    const struct Token* dst;
} StmtGoto;
#define AST_STRUCT_STMT_GOTO StmtGoto
#define AST_KIND_StmtGoto STMT_GOTO
typedef struct StmtLoop
{
    INHERIT_AST;

    struct Expr* cond;
    struct Ast* body;
    // null for while/dowhile
    struct Ast* init;
    // null for while/dowhile
    struct Expr* advance;

    int is_do_while : 1;
} StmtLoop;
#define AST_STRUCT_STMT_LOOP StmtLoop
#define AST_KIND_StmtLoop STMT_LOOP
typedef struct StmtBlock
{
    INHERIT_AST;

    SeqView seq;
} StmtBlock;
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
