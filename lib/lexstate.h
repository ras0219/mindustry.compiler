#pragma once

#include "fwd.h"

enum LexerState
{
    LEX_START,
    LEX_START2,
    LEX_IDENT,
    LEX_NUMBER,
    LEX_STRING,
    LEX_STRING1,
    LEX_DIRECTIVE,
    LEX_COMMENT,
    LEX_MULTILINE_COMMENT,
    LEX_SYMBOL,
    LEX_EOF,
    // keywords
    LEX_INT,
    LEX_VOID,
    LEX_GOTO,
    LEX_RETURN,
    LEX_BREAK,
    LEX_SWITCH,
    LEX_CONTINUE,
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
    // extensions
    LEX_ATTRIBUTE,
    LEX_MSTRING,
    LEX_UNIT,
};

const char* lexstate_to_string(enum LexerState s);
