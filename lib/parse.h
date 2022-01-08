#pragma once

#include "array.h"
#include "ast.h"
#include "autoheap.h"
#include "freevar.h"
#include "fwd.h"
#include "pool.h"
#include "scope.h"

struct Parser
{
    struct AutoHeap strings_to_free;

    struct Decl* fn;
    size_t anon_idx;

    struct Scope scope;
    struct Scope type_scope;

    // exprs
    struct Pool ast_pools[AST_KIND_END_POOLS + 1];
    struct Array expr_seqs;

    // top-level expressions
    struct Array arr_exprs;

    const char* tk_strdata;
};

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
// tk array must terminate with LEX_EOF
int parser_parse(struct Parser* p, const struct Token* tk, const char* tk_strs);
const char* token_str(struct Parser* p, const struct Token* tk);

void parser_debug_check(struct Parser* p);
