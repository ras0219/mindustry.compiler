#include "ast.h"

#include "rowcol.h"
#include "token.h"

static struct RowCol s_ast_unknown_rc = {
    .file = "<unknown>",
    .row = 1,
    .col = 1,
};

const struct RowCol* expr_to_rc(struct Expr* e)
{
    switch (e->kind)
    {
        case EXPR_SYM: return &((struct ExprSym*)e)->tok->rc;
        case EXPR_CALL: return &((struct ExprCall*)e)->tok->rc;
        case EXPR_LIT: return &((struct ExprLit*)e)->tok->rc;
        case EXPR_OP: return &((struct ExprOp*)e)->tok->rc;
        default: return &s_ast_unknown_rc;
    }
}

int ast_kind_is_expr(enum AstKind k)
{
    switch (k)
    {
        case EXPR_SYM:
        case EXPR_LIT:
        case EXPR_OP:
        case EXPR_CALL: return 1;
        default: return 0;
    }
}

#define Y_CASE_TO_STR(Z)                                                                                               \
    case Z: return #Z;
const char* ast_kind_to_string(enum AstKind k)
{
    switch (k)
    {
        X_AST_KIND(Y_CASE_TO_STR)
        default: return "unknown";
    }
}
