#pragma once

#include <stddef.h>

#include "freevar.h"

enum AstKind
{
    EXPR_SYM,
    EXPR_LIT,
    EXPR_OP,
    EXPR_CALL,
    AST_SYM,
};

struct Expr
{
    enum AstKind kind;
};

struct ExprLit
{
    struct Expr kind;

    const struct Token* tok;
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
    struct Expr *lhs, *rhs;
};

struct ExprCall
{
    struct Expr kind;
    const struct Token* tok;
    struct Expr* fn;
    size_t offset;
    size_t extent;
};
