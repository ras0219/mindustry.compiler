#pragma once

#include "fwd.h"

enum LexerState_t
{
    LEX_START,
    LEX_IDENT,
    LEX_NUMBER,
    LEX_COMMENT,
    // only used to emit
    LEX_SYMBOL,
    LEX_EOF,
};
