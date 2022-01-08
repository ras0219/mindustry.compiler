#pragma once

#include <stdint.h>

#include "fwd.h"

#define X_LEX_TYPE_KEYWORDS(Y)                                                                                         \
    Y(LEX_INT, "int")                                                                                                  \
    Y(LEX_BOOL, "_Bool")                                                                                               \
    Y(LEX_CHAR, "char")                                                                                                \
    Y(LEX_FLOAT, "float")                                                                                              \
    Y(LEX_DOUBLE, "double")                                                                                            \
    Y(LEX_VOID, "void")                                                                                                \
    Y(LEX_UUINT64, "__int64")                                                                                          \
    /*Y(LEX_UUINT64T, "__int64_t")                                                                                     \
    Y(LEX_UUUINT64T, "__uint64_t")                                                                                     \
    Y(LEX_UUINT32T, "__int32_t")                                                                                       \
    Y(LEX_UUUINT32T, "__uint32_t")                                                                                     \
    Y(LEX_UUINT16T, "__int16_t")                                                                                       \
    Y(LEX_UUUINT16T, "__uint16_t")                                                                                     \
    Y(LEX_UUDARWIN_SIZET, "__darwin_size_t")                                                                           \
    Y(LEX_UUDARWIN_NATURAL, "__darwin_natural_t")                                                                      \
    Y(LEX_UUDARWIN_TIMET, "__darwin_time_t")                                                                           \
    Y(LEX_UUDARWIN_VALIST, "__darwin_va_list")*/                                                                       \
    Y(LEX_UUVALIST, "__builtin_va_list")

#define X_LEX_KEYWORDS(Y)                                                                                              \
    X_LEX_TYPE_KEYWORDS(Y)                                                                                             \
    Y(LEX_SHORT, "short")                                                                                              \
    Y(LEX_LONG, "long")                                                                                                \
    Y(LEX_UNSIGNED, "unsigned")                                                                                        \
    Y(LEX_SIGNED, "signed")                                                                                            \
    Y(LEX_UUSIGNED, "__signed")                                                                                        \
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
    Y(LEX_DEFAULT, "default")                                                                                          \
    Y(LEX_STRUCT, "struct")                                                                                            \
    Y(LEX_ENUM, "enum")                                                                                                \
    Y(LEX_UNION, "union")                                                                                              \
    Y(LEX_CONST, "const")                                                                                              \
    Y(LEX_VOLATILE, "volatile")                                                                                        \
    Y(LEX_REGISTER, "register")                                                                                        \
    Y(LEX_EXTERN, "extern")                                                                                            \
    Y(LEX_AUTO, "auto")                                                                                                \
    Y(LEX_TYPEDEF, "typedef")                                                                                          \
    Y(LEX_UUVA_START, "__builtin_va_start")                                                                            \
    Y(LEX_UUVA_END, "__builtin_va_end")                                                                                \
    Y(LEX_UUVA_ARG, "__builtin_va_arg")                                                                                \
    Y(LEX_UURESTRICT, "__restrict")                                                                                    \
    Y(LEX_RESTRICT, "restrict")                                                                                        \
    Y(LEX_UUFORCEINLINE, "__forceinline")                                                                              \
    Y(LEX_INLINE, "inline")                                                                                            \
    Y(LEX_UUINLINE, "__inline")                                                                                        \
    Y(LEX_STDCALL, "__stdcall")                                                                                        \
    Y(LEX_UUASM, "__asm")                                                                                              \
    Y(LEX_ATTRIBUTE, "__attribute__")                                                                                  \
    Y(LEX_DECLSPEC, "__declspec")                                                                                      \
    Y(LEX_UPRAGMA, "_Pragma")                                                                                          \
    Y(LEX_UUPRAGMA, "__pragma")                                                                                        \
    Y(LEX_CDECL, "__cdecl")

#define X_LEX_STATES(Z)                                                                                                \
    Z(LEX_START, "")                                                                                                   \
    Z(LEX_IDENT, "")                                                                                                   \
    Z(LEX_NUMBER, "")                                                                                                  \
    Z(LEX_STRING, "")                                                                                                  \
    Z(LEX_CHARLIT, "")                                                                                                 \
    Z(LEX_HEADER, "")                                                                                                  \
    Z(LEX_COMMENT, "")                                                                                                 \
    Z(LEX_MULTILINE_COMMENT, "")                                                                                       \
    Z(LEX_SYMBOL, "")                                                                                                  \
    Z(LEX_EOF, "")                                                                                                     \
    X_LEX_KEYWORDS(Z)                                                                                                  \
    Z(LEX_MACRO_VA_ARGS, "")                                                                                           \
    Z(LEX_MACRO_ARG_BEGIN, "")                                                                                         \
    Z(LEX_PLACEHOLDER, "")

enum LexerState
{
#define Y_COMMA(E, ...) E,
    X_LEX_STATES(Y_COMMA)
#undef Y_COMMA
};

const char* lexstate_to_string(unsigned int s);

int lit_to_uint64(const char* s, uint64_t* out, const struct RowCol* rc);
