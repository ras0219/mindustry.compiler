#pragma once

#include <stddef.h>

#include "array.h"
#include "rowcol.h"

struct Buffer
{
    size_t sz;
    char buf[1024];
};

typedef struct Buffer Buffer;

#define MAX_TOKEN_SIZE 128

struct Lexer
{
    int (*f_on_token)(struct Lexer*);
    unsigned int state : 8;
    unsigned int in_directive : 1;
    struct RowCol tok_rc;
    struct RowCol rc;
    // length of tok
    size_t sz;
    // null-terminated when calling f_on_token
    char tok[MAX_TOKEN_SIZE];
};

typedef struct Lexer Lexer;

void init_lexer(Lexer* l, const char* file, int (*f_on_token)(struct Lexer*));
int lex(Lexer* l, Buffer* buf);
int end_lex(Lexer* l);
