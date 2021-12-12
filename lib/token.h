#pragma once

#include <stdint.h>

#include "rowcol.h"

struct Token
{
    struct RowCol rc;
    // enum LexerState
    int type;
    ptrdiff_t sp_offset;
};
