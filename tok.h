#pragma once

#include <stddef.h>

#include "array.h"
#include "fwd.h"

struct Buffer
{
    size_t sz;
    char buf[1024];
};

struct RowCol
{
    int row;
    int col;
};

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

void init_lexer(Lexer* l, int (*f_on_token)(struct Lexer*));
int lex(Lexer* l, Buffer* buf);
int end_lex(Lexer* l);

#define PARSER_STACK_SIZE 16
#define TREE_NODES_SIZE 128
#define STRING_POOL_SIZE 1024

typedef struct TreeNode
{
    int type;
    const char* str;
    struct TreeNode_t* lhs;
    struct TreeNode_t* rhs;
} TreeNode;

typedef struct
{
    int state;
    TreeNode* ast;
} ParserFrame;

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
};

typedef struct Parser
{
    struct Array toks;
    struct Array stringpool;
    int free_var_counter;
    struct Array strings_to_free;

    char fn_label_prefix[16];

    struct Scope scope;
    struct Scope type_scope;

    struct CodeGen cg;
} Parser;

void parser_init(Parser* p);
int parse(Parser* p, Lexer* l);

void cg_init(struct CodeGen* cg);
void cg_write_bin_entry(struct CodeGen* cg);
void cg_write_push_ret(struct CodeGen* cg);
void cg_write_return(struct CodeGen* cg);
void cg_write_inst_jump(struct CodeGen* cg, const char* dst);
void cg_write_inst_set(struct CodeGen* cg, const char* dst, const char* src);
void cg_write_inst(struct CodeGen* cg, const char* inst);
void cg_emit(struct CodeGen* cg);
void cg_mark_label(struct CodeGen* cg, const char* sym);
