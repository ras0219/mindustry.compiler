#pragma once

#include <stdint.h>

#include "rowcol.h"

struct Token
{
    struct RowCol rc;
    // enum LexerState
    unsigned int type : 8;
    ptrdiff_t sp_offset;
};
