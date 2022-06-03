#pragma once

#include <stdio.h>

#include "array.h"
#include "ast.h"
#include "autoheap.h"
#include "freevar.h"
#include "fwd.h"
#include "pool.h"
#include "scope.h"

typedef struct Parser
{
    struct AutoHeap strings_to_free;

    struct Ast* parent;
    size_t anon_idx;

    struct Scope scope;
    struct Scope type_scope;
    struct Scope typedef_scope;

    // exprs
    struct Pool ast_pools[AST_KIND_END_POOLS + 1];
    /// Pool<Symbol>
    struct Pool sym_pool;
    /// Pool<TypeSymbol>
    struct Pool typesym_pool;
    /// Array<Expr*>
    struct Array expr_seqs;
    /// Array<Designator> from AstInit::designator_offset
    struct Array designators;
    /// Array<CallParam> from ExprCall::param_offset
    struct Array callparams;

    // top-level expressions
    struct StmtBlock* top;

    const char* tk_strdata;
    const struct Token* tok_data;
} Parser;

enum Precedence
{
    PRECEDENCE_ERROR,
    PRECEDENCE_COMMA,
    PRECEDENCE_ASSIGN,
    PRECEDENCE_TERNARY,
    PRECEDENCE_OR,
    PRECEDENCE_AND,
    PRECEDENCE_BIT_OR,
    PRECEDENCE_BIT_XOR,
    PRECEDENCE_BIT_AND,
    PRECEDENCE_EQUALITY,
    PRECEDENCE_RELATION,
    PRECEDENCE_SHIFT,
    PRECEDENCE_ADD,
    PRECEDENCE_MULT,
};

enum Precedence op_precedence(unsigned int tok_type);

void parser_init(struct Parser* p);
void parser_destroy(struct Parser* p);
/// @param tk must terminate with \c LEX_EOF Token
int parser_parse(struct Parser* p, const struct Token* tk, const char* tk_strs);
const char* token_str(struct Parser* p, const struct Token* tk);

void parser_debug_check(struct Parser* p);
void parser_dump(struct Parser* p, FILE* f);
