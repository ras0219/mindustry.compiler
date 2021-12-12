#pragma once

#include <stddef.h>

#include "freevar.h"

#define X_AST_KIND(Y)                                                                                                  \
    Y(EXPR_SYM)                                                                                                        \
    Y(EXPR_LIT)                                                                                                        \
    Y(EXPR_CAST)                                                                                                       \
    Y(EXPR_OP)                                                                                                         \
    Y(EXPR_CALL)                                                                                                       \
    Y(AST_DECL)                                                                                                        \
    Y(STMT_DECLS)                                                                                                      \
    Y(STMT_RETURN)                                                                                                     \
    Y(STMT_GOTO)                                                                                                       \
    Y(STMT_IF)                                                                                                         \
    Y(STMT_LOOP)                                                                                                       \
    Y(STMT_BLOCK)                                                                                                      \
    Y(STMT_LABEL)                                                                                                      \
    Y(STMT_BREAK)                                                                                                      \
    Y(STMT_CONTINUE)

#define Y_COMMA(Z) Z,
enum AstKind
{
    X_AST_KIND(Y_COMMA)

        AST_KIND_END_POOLS,

    STMT_NONE = AST_KIND_END_POOLS,
    AST_SYM,
};

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

struct StmtReturn
{
    struct Expr kind;
    struct Token* tok;
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
    struct Token* dst;
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
    struct Token* tok;
    struct Expr* stmt;
};
struct StmtBreak
{
    struct Expr kind;
    struct Token* tok;
};
struct StmtContinue
{
    struct Expr kind;
    struct Token* tok;
};

const struct RowCol* expr_to_rc(struct Expr* expr);
