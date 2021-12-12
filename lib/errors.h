#pragma once

#include <stdarg.h>
#include <stdio.h>

struct RowCol;
struct Token;

int parser_has_errors();
int parser_tok_error(const struct Token* tok, const char* fmt, ...);
int parser_ferror(const struct RowCol* rc, const char* fmt, ...);
int parser_vferror(const struct RowCol* rc, const char* fmt, va_list);
int parser_ice(const struct RowCol* rc);
int parser_ice_tok(const struct Token* tok);
void parser_print_errors(FILE* f);
void parser_clear_errors();
