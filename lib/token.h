#pragma once

#include <stdint.h>

#include "compilermacros.h"
#include "rowcol.h"

struct Token
{
    struct RowCol rc;
    // enum LexerState
    unsigned int basic_type : 7;
    unsigned int noreplace : 1;
    unsigned int type : 24;
    unsigned int tok_len;
    ptrdiff_t sp_offset;
};

#define TOKEN_SYM1(A) (0x80 + (unsigned int)(A))
#define TOKEN_SYM2(A, B) (0x80 + (unsigned int)(A) + 0x100 * (unsigned int)(B))
#define TOKEN_SYM3(A, B, C) (0x80 + (unsigned int)(A) + 0x100 * (unsigned int)(B) + 0x10000 * (unsigned int)(C))
#define TOKEN_GET_SYM1(A) ((unsigned int)(A) & 0x7F))
#define TOKEN_GET_SYM2(A) ((unsigned int)(A) & 0x7F00) >> 8)
#define TOKEN_GET_SYM3(A) ((unsigned int)(A) & 0x7F0000) >> 16)

__forceinline int token_symlen(unsigned int type) { return 1 + (type >= 0x100) + (type >= 0x10000); }
