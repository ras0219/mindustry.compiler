#pragma once

#include <stddef.h>

#include "array.h"
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

int parser_ferror(const struct RowCol* rc, const char* fmt, ...);
int parser_ice(const struct RowCol* rc);

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

struct FreeVar
{
    char buf[24];
};

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

    struct Scope scope;
    struct Scope type_scope;

    struct CodeGen cg;
    struct Symbol* first_active_sym;

    // exprs
    struct Pool expr_op_pool;
    struct Pool expr_sym_pool;
    struct Pool expr_call_pool;
    struct Pool expr_lit_pool;
    struct Array expr_seqs;
} Parser;

void parser_init(Parser* p);
void parser_destroy(Parser* p);
int parse(Parser* p, Lexer* l);

void cg_init(struct CodeGen* cg);
void cg_destroy(struct CodeGen* cg);
void cg_write_bin_entry(struct CodeGen* cg);
void cg_write_push_ret(struct CodeGen* cg, struct FreeVar* ret_addr);
void cg_write_return(struct CodeGen* cg, struct FreeVar* ret_addr);
void cg_write_inst_jump(struct CodeGen* cg, const char* dst);
void cg_write_inst_jump_op(struct CodeGen* cg, const char* tgt, const char* op, const char* a, const char* b);
void cg_write_inst_set(struct CodeGen* cg, const char* dst, const char* src);
void cg_write_inst_op(struct CodeGen* cg, const char* op, const char* dst, const char* a, const char* b);
void cg_write_inst_add(struct CodeGen* cg, const char* dst, const char* a, int n);
void cg_write_inst(struct CodeGen* cg, const char* inst);
void cg_emit(struct CodeGen* cg);
void cg_mark_label(struct CodeGen* cg, const char* sym);
int cg_set_memory_bank(struct CodeGen* cg, const struct RowCol* rc, const char* mem);
int cg_read_mem(struct CodeGen* cg, const char* addr, const char* reg, const struct RowCol* rc);
int cg_write_mem(struct CodeGen* cg, const char* addr, const char* val, const struct RowCol* rc);
int cg_store(struct CodeGen* cg, int offset, const char* val, const struct RowCol* rc);
int cg_load(struct CodeGen* cg, int offset, const char* dst, const struct RowCol* rc);
