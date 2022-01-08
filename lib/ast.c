#include "ast.h"

#include "rowcol.h"
#include "symbol.h"
#include "token.h"

static struct RowCol s_ast_unknown_rc = {
    .file = "<unknown>",
    .row = 1,
    .col = 1,
};

#define EXPR_TO_RC(K, T)                                                                                               \
    case K: return &((const struct T*)e)->tok->rc
const struct RowCol* expr_to_rc(const struct Expr* e)
{
    switch (e->kind)
    {
        EXPR_TO_RC(EXPR_LIT, ExprLit);
        EXPR_TO_RC(EXPR_SYM, ExprSym);
        EXPR_TO_RC(EXPR_FIELD, ExprSym);
        EXPR_TO_RC(EXPR_OP, ExprOp);
        EXPR_TO_RC(STMT_RETURN, StmtReturn);
        EXPR_TO_RC(AST_DECLARR, DeclArr);
        EXPR_TO_RC(AST_DECLFN, DeclFn);
        EXPR_TO_RC(AST_DECLPTR, DeclPtr);
        EXPR_TO_RC(AST_INIT, ASTInit);
        case AST_DECL: return &((const struct Decl*)e)->specs->tok->rc;
        case EXPR_CAST: return expr_to_rc(((const struct ExprCast*)e)->expr);
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
