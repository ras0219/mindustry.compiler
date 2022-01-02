#pragma once

#include <stddef.h>

#include "array.h"
#include "rowcol.h"

struct Lexer
{
    int (*f_on_token)(struct Lexer*);
    unsigned int state : 8;
    unsigned int expect_header : 1;
    unsigned int ws_sensitive : 1;
    unsigned int ws_before : 1;
    unsigned int not_first : 1;
    unsigned int backslash : 1;
    unsigned int skip_next : 1;

    struct RowCol tok_rc;
    struct RowCol rc;
    // length of tok
    size_t sz;
    // null-terminated when calling f_on_token
    char tok[1024];
};

typedef struct Lexer Lexer;

void init_lexer(Lexer* l, const char* file, int (*f_on_token)(struct Lexer*));
int lex(Lexer* l, const char* buf, size_t sz);
int end_lex(Lexer* l);
