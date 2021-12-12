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
    struct Array toks;
    struct Array stringpool;
    struct AutoHeap strings_to_free;

    struct Decl* fn;

    struct Scope scope;
    struct Scope type_scope;

    // exprs
    struct Pool ast_pools[AST_KIND_END_POOLS];
    struct Array expr_seqs;

    // top-level expressions
    struct Array arr_exprs;
};

enum Precedence
{
    PRECEDENCE_ERROR,
    PRECEDENCE_COMMA,
    PRECEDENCE_ASSIGN,
    PRECEDENCE_OR,
    PRECEDENCE_AND,
    PRECEDENCE_EQUALITY,
    PRECEDENCE_RELATION,
    PRECEDENCE_ADD,
    PRECEDENCE_MULT,
};

enum Precedence op_precedence(const char* op);

void parser_init(struct Parser* p);
void parser_destroy(struct Parser* p);
int parser_push(struct Parser* p, struct Lexer* l);
int parser_parse(struct Parser* p);
char* token_str(struct Parser* p, const struct Token* tk);
