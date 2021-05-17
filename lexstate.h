#pragma once

#include "fwd.h"

enum LexerState_t
{
    LEX_START,
    LEX_IDENT,
    LEX_NUMBER,
    LEX_STRING,
    LEX_STRING1,
    LEX_COMMENT,
    LEX_MULTILINE_COMMENT,
    LEX_SYMBOL,
    LEX_EOF,
    // keywords
    LEX_ATTRIBUTE,
    LEX_INT,
    LEX_VOID,
    LEX_GOTO,
    LEX_RETURN,
    LEX_IF,
    LEX_ELSE,
    LEX_FOR,
    LEX_WHILE,
    LEX_DO,
    LEX_STRUCT,
    LEX_CONST,
    LEX_VOLATILE,
    LEX_REGISTER,
    LEX_AUTO,
};

const char* lexstate_to_string(enum LexerState_t s);
