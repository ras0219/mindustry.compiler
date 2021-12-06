#pragma once

#include <stddef.h>

#include "freevar.h"

enum AstKind
{
    EXPR_SYM,
    EXPR_LIT,
    EXPR_CAST,
    EXPR_OP,
    EXPR_CALL,
    AST_DECL,
    STMT_DECLS,
    STMT_RETURN,
    STMT_GOTO,
    STMT_IF,
    STMT_LOOP,
    STMT_BLOCK,
    STMT_LABEL,
    STMT_BREAK,
    STMT_CONTINUE,

    AST_KIND_END_POOLS,

    STMT_NONE = AST_KIND_END_POOLS,
    AST_SYM,
};

int ast_kind_is_expr(enum AstKind k);

struct Expr
{
    enum AstKind kind;
};

struct ExprLit
{
    struct Expr kind;

    const struct Token* tok;
};

struct ExprCast
{
    struct Expr kind;

    struct Expr* expr;
    struct Decl* type;
};

struct ExprSym
{
    struct Expr kind;

    const struct Token* tok;
    struct Symbol* sym;
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
