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
    LEX_START_DIRECTIVE,
    LEX_DIRECTIVE,
    LEX_DIRECTIVE_INCLUDE_WS,
    LEX_DIRECTIVE_INCLUDE,
    LEX_COMMENT,
    LEX_MULTILINE_COMMENT,
    LEX_SYMBOL,
    LEX_EOF,
    // keywords
    LEX_INT,
    LEX_SHORT,
    LEX_CHAR,
    LEX_LONG,
    LEX_UNSIGNED,
    LEX_SIGNED,
    LEX_VOID,
    LEX_GOTO,
    LEX_RETURN,
    LEX_BREAK,
    LEX_SWITCH,
    LEX_CASE,
    LEX_CONTINUE,
    LEX_IF,
    LEX_ELSE,
    LEX_STATIC,
    LEX_FOR,
    LEX_WHILE,
    LEX_DO,
    LEX_STRUCT,
    LEX_CONST,
    LEX_VOLATILE,
    LEX_REGISTER,
    LEX_EXTERN,
    LEX_AUTO,
    LEX_STDCALL,
    LEX_CDECL,
    // extensions
    LEX_ATTRIBUTE,
    LEX_MSTRING,
    LEX_UNIT,
};

const char* lexstate_to_string(enum LexerState s);
