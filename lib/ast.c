#include "ast.h"

#include "rowcol.h"
#include "symbol.h"
#include "token.h"

int ast_kind_is_expr(enum AstKind k)
{
    switch (k)
    {
        case EXPR_SYM:
        case EXPR_FIELD:
        case EXPR_LIT:
        case EXPR_CAST:
        case EXPR_BINOP:
        case EXPR_UNOP:
        case EXPR_BUILTIN:
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
