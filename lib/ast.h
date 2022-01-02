#pragma once

#include <stddef.h>

#include "freevar.h"

#define X_AST_POOL_KIND(Y)                                                                                             \
    Y(EXPR_SYM)                                                                                                        \
    Y(EXPR_FIELD)                                                                                                      \
    Y(EXPR_LIT)                                                                                                        \
    Y(EXPR_CAST)                                                                                                       \
    Y(EXPR_OP)                                                                                                         \
    Y(EXPR_CALL)                                                                                                       \
    Y(AST_DECL)                                                                                                        \
    Y(AST_DECLSPEC)                                                                                                    \
    Y(AST_DECLFN)                                                                                                      \
    Y(AST_DECLARR)                                                                                                     \
    Y(AST_DECLPTR)                                                                                                     \
    Y(STMT_DECLS)                                                                                                      \
    Y(STMT_RETURN)                                                                                                     \
    Y(STMT_GOTO)                                                                                                       \
    Y(STMT_IF)                                                                                                         \
    Y(STMT_LOOP)                                                                                                       \
    Y(STMT_BLOCK)                                                                                                      \
    Y(STMT_LABEL)                                                                                                      \
    Y(STMT_BREAK)                                                                                                      \
    Y(STMT_CONTINUE)

#define X_AST_UNPOOL_KIND(Y) Y(STMT_NONE) Y(TYPE_BUILTIN_INT) Y(TYPE_BUILTIN_CHAR)

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

struct Expr
{
    enum AstKind kind;
};

struct ExprLit
{
    struct Expr kind;

    const struct Token* tok;
    const char* text;
};

struct ExprCast
{
    struct Expr kind;

    const struct Token* tok;
    struct Expr* expr;
    struct Decl* type;
};

struct ExprSym
{
    struct Expr kind;

    const struct Token* tok;
    struct Decl* decl;
};

struct ExprField
{
    struct Expr kind;
    int is_arrow : 1;
    const struct Token* tok;
    const struct Token* field_tok;
    const char* fieldname;
    struct Expr* lhs;
    struct Decl* decl;
};

struct ExprOp
{
    struct Expr kind;

    const struct Token* tok;
    struct Expr* lhs;
    // NULL for unary operators
    struct Expr* rhs;
};

struct ExprCall
{
    struct Expr kind;
    const struct Token* tok;
    struct Expr* fn;
    size_t offset;
    size_t extent;
};

struct StmtReturn
{
    struct Expr kind;
    const struct Token* tok;
    // may be null
    struct Expr* expr;
};
struct StmtDecls
{
    struct Expr kind;
    size_t offset;
    size_t extent;
};
struct StmtIf
{
    struct Expr kind;
    struct Expr* cond;
    struct Expr* if_body;
    // may be null
    struct Expr* else_body;
};
struct StmtGoto
{
    struct Expr kind;
    const struct Token* dst;
};
struct StmtLoop
{
    struct Expr kind;
    struct Expr* cond;
    struct Expr* body;
    // null for while/dowhile
    struct Expr* init;
    // null for while/dowhile
    struct Expr* advance;

    int is_do_while : 1;
};
struct StmtBlock
{
    struct Expr kind;
    size_t offset;
    size_t extent;
};
struct StmtLabel
{
    struct Expr kind;
    const struct Token* tok;
    struct Expr* stmt;
};
struct StmtBreak
{
    struct Expr kind;
    const struct Token* tok;
};
struct StmtContinue
{
    struct Expr kind;
    const struct Token* tok;
};

const struct RowCol* expr_to_rc(const struct Expr* expr);
