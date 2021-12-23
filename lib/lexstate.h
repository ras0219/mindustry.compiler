#pragma once

#include "fwd.h"

#define X_LEX_KEYWORDS(Y)                                                                                              \
    Y(LEX_INT, "int")                                                                                                  \
    Y(LEX_BOOL, "_Bool")                                                                                               \
    Y(LEX_SHORT, "short")                                                                                              \
    Y(LEX_CHAR, "char")                                                                                                \
    Y(LEX_LONG, "long")                                                                                                \
    Y(LEX_UNSIGNED, "unsigned")                                                                                        \
    Y(LEX_SIGNED, "signed")                                                                                            \
    Y(LEX_VOID, "void")                                                                                                \
    Y(LEX_GOTO, "goto")                                                                                                \
    Y(LEX_RETURN, "return")                                                                                            \
    Y(LEX_BREAK, "break")                                                                                              \
    Y(LEX_SWITCH, "switch")                                                                                            \
    Y(LEX_CASE, "case")                                                                                                \
    Y(LEX_CONTINUE, "continue")                                                                                        \
    Y(LEX_IF, "if")                                                                                                    \
    Y(LEX_ELSE, "else")                                                                                                \
    Y(LEX_STATIC, "static")                                                                                            \
    Y(LEX_FOR, "for")                                                                                                  \
    Y(LEX_SIZEOF, "sizeof")                                                                                            \
    Y(LEX_WHILE, "while")                                                                                              \
    Y(LEX_DO, "do")                                                                                                    \
    Y(LEX_STRUCT, "struct")                                                                                            \
    Y(LEX_ENUM, "enum")                                                                                                \
    Y(LEX_UNION, "union")                                                                                              \
    Y(LEX_CONST, "const")                                                                                              \
    Y(LEX_VOLATILE, "volatile")                                                                                        \
    Y(LEX_REGISTER, "register")                                                                                        \
    Y(LEX_EXTERN, "extern")                                                                                            \
    Y(LEX_AUTO, "auto")                                                                                                \
    Y(LEX_TYPEDEF, "typedef")                                                                                          \
    Y(LEX_UURESTRICT, "__restrict")                                                                                    \
    Y(LEX_RESTRICT, "restrict")                                                                                        \
    Y(LEX_UUVALIST, "__builtin_va_list")                                                                               \
    Y(LEX_UUFORCEINLINE, "__forceinline")                                                                              \
    Y(LEX_STDCALL, "__stdcall")                                                                                        \
    Y(LEX_ATTRIBUTE, "__attribute__")                                                                                  \
    Y(LEX_MSTRING, "__string")                                                                                         \
    Y(LEX_UNIT, "__unit")                                                                                              \
    Y(LEX_CDECL, "__cdecl")

#define X_LEX_STATES(Z)                                                                                                \
    X_LEX_KEYWORDS(Z)                                                                                                  \
    Z(LEX_START, "")                                                                                                   \
    Z(LEX_IDENT, "")                                                                                                   \
    Z(LEX_NUMBER, "")                                                                                                  \
    Z(LEX_STRING, "")                                                                                                  \
    Z(LEX_STRING1, "")                                                                                                 \
    Z(LEX_HEADER, "")                                                                                                  \
    Z(LEX_COMMENT, "")                                                                                                 \
    Z(LEX_MULTILINE_COMMENT, "")                                                                                       \
    Z(LEX_SYMBOL, "")                                                                                                  \
    Z(LEX_EOF, "")

enum LexerState
{
#define Y_COMMA(E, ...) E,
    X_LEX_STATES(Y_COMMA)
#undef Y_COMMA
};

const char* lexstate_to_string(enum LexerState s);
