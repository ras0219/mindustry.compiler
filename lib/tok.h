#pragma once

#include <stddef.h>
#include <stdio.h>

#include "array.h"
#include "ast.h"
#include "freevar.h"
#include "fwd.h"
#include "pool.h"

struct Buffer
{
    size_t sz;
    char buf[1024];
};

struct RowCol
{
    const char* file;
    int row;
    int col;
};

int parser_has_errors();
int parser_ferror(const struct RowCol* rc, const char* fmt, ...);
int parser_ice(const struct RowCol* rc);
void parser_print_errors(FILE* f);
void parser_clear_errors();

#define MAX_TOKEN_SIZE 128

struct Lexer
{
    int (*f_on_token)(struct Lexer*);
    int state;
    struct RowCol tok_rc;
    struct RowCol rc;
    size_t sz;
    char tok[MAX_TOKEN_SIZE];
};

void init_lexer(Lexer* l, const char* file, int (*f_on_token)(struct Lexer*));
int lex(Lexer* l, Buffer* buf);
int end_lex(Lexer* l);

#define PARSER_STACK_SIZE 16
#define TREE_NODES_SIZE 128
#define STRING_POOL_SIZE 1024

struct Scope
{
    struct Array strings;
    struct Array binds;
};

struct CodeGenLabel
{
    ptrdiff_t str_offset;
    ptrdiff_t str_len;
    size_t line;
};

struct CodeGen
{
    size_t lines;
    struct Array text;
    struct Array labels;
    struct Array label_strs;
    struct FreeVar memory;
    FILE* fdebug;
    FILE* fout;
};

typedef struct Parser
{
    struct Array toks;
    struct Array stringpool;
    int free_var_counter;
    struct Array strings_to_free;

    char fn_label_prefix[16];
    // set if fn is non-reentrant
    struct FreeVar fn_ret_var;
    struct Decl* fn;
    int globals_size;

    struct Scope scope;
    struct Scope type_scope;

    struct CodeGen cg;
    struct RegMap* first_active_reg;

    // exprs
    struct Pool ast_pools[AST_KIND_END_POOLS];
    struct Array expr_seqs;

    // types
    struct Pool type_pool;
    struct Array type_seqs;
} Parser;

void parser_init(Parser* p);
void parser_destroy(Parser* p);
int parse(Parser* p, Lexer* l);
