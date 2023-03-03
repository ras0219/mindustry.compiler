#include "ast.h"

#include "rowcol.h"
#include "symbol.h"
#include "token.h"

int ast_kind_is_expr(enum AstKind k)
{
    static const unsigned char s_is_expr[AST_KIND_COUNT] = {
#define Y(A) [A] = 1,
        X_FOREACH_EXPR(Y)
#undef Y
    };
    return s_is_expr[k];
}

#define Y_CASE_TO_STR(Z) [Z] = #Z,

const char* ast_kind_to_string(enum AstKind k)
{
    static const char s_strings[AST_KIND_COUNT][32] = {X_AST_KIND(Y_CASE_TO_STR)};
    return s_strings[k];
#undef Y_CASE_TO_STR
}
