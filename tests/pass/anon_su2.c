#pragma once

#define X_FOREACH_EXPR(Y)                                                                                              \
    Y(EXPR_REF)                                                                                                        \
    Y(EXPR_FIELD)                                                                                                      \
    Y(EXPR_LIT)                                                                                                        \
    Y(EXPR_STRLIT)                                                                                                     \
    Y(EXPR_CAST)                                                                                                       \
    Y(EXPR_BINOP)                                                                                                      \
    Y(EXPR_ANDOR)                                                                                                      \
    Y(EXPR_ADD)                                                                                                        \
    Y(EXPR_ASSIGN)                                                                                                     \
    Y(EXPR_TERNARY)                                                                                                    \
    Y(EXPR_COMMA)                                                                                                      \
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
    unsigned long long id;                                                                                             \
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

#define EXPR_FIELDS INHERIT_AST

typedef struct Expr
{
    EXPR_FIELDS;
} Expr;
