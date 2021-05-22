#pragma once

#include <stddef.h>

enum AstKind
{
    EXPR_SYM,
    EXPR_LIT,
    EXPR_OP,
    EXPR_CALL,
};

struct Expr
{
    enum AstKind kind;
};

struct ExprLit
{
    enum AstKind kind;

    const struct Token* tok;
};

struct ExprSym
{
    enum AstKind kind;

    const struct Token* tok;
    struct Symbol* sym;
};

struct ExprOp
{
    enum AstKind kind;

    const struct Token* tok;
    struct Expr *lhs, *rhs;
};

struct ExprCall
{
    enum AstKind kind;
    const struct Token* tok;
    struct Expr* fn;
    size_t offset;
    size_t extent;
};
