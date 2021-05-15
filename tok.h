#pragma once

#include <stddef.h>
#include "fwd.h"

struct Buffer_t
{
    size_t sz;
    char buf[1024];
};

struct RowCol
{
    int row;
    int col;
};

struct Lexer
{
    int (*f_on_token)(struct Lexer *);
    int state;
    struct RowCol tok_rc;
    struct RowCol rc;
    size_t sz;
    char tok[128];
};

void init_lexer(Lexer *l, int (*f_on_token)(struct Lexer *));
int lex(Lexer *l, Buffer *buf);
int end_lex(Lexer *l);

#define PARSER_STACK_SIZE 16
#define TREE_NODES_SIZE 128
#define STRING_POOL_SIZE 1024

typedef struct TreeNode_t
{
    int type;
    const char *str;
    struct TreeNode_t *lhs;
    struct TreeNode_t *rhs;
} TreeNode;

typedef struct
{
    int state;
    TreeNode *ast;
} ParserFrame;

struct Array
{
    size_t sz;
    size_t cap;
    void *data;
};

struct ValDest
{
    const char *dst_name;
};

struct Parser_t
{
    struct Array toks;
    struct Array stringpool;
    int free_var_counter;
    struct Array strings_to_free;
};

void init_parser(Parser *p);
int parse(Parser *p, Lexer *l);
