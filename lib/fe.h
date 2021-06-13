#pragma once

#include "array.h"
#include "tok.h"

typedef struct
{
    Lexer lexer;
    Parser parser;

    struct Array files_open;
    struct Array filenames;
} FrontEnd;

void fe_init(FrontEnd* fe);
void fe_destroy(FrontEnd* fe);
int fe_lex_file(FrontEnd* fe, const char* filename);
int fe_lex_file_opened(FrontEnd* fe, const char* filename, FILE* f);
