#pragma once

#include "array.h"
#include "stdio.h"
#include "tok.h"

enum PreprocessorState
{
    PP_INIT,
    PP_HASH,
    PP_INCLUDE,
    PP_PRAGMA,
    PP_IGNORE,
};

struct FrontEnd
{
    struct Lexer lexer;
    struct Parser* parser;
    struct Elaborator* elab;
    struct BackEnd* be;
    struct CodeGen* cg;
    enum PreprocessorState preproc;

    // Array<char*>
    struct Array files_open;
    // Array<char*>
    struct Array filenames;

    FILE* fout;
};

void fe_init(struct FrontEnd* fe);
void fe_destroy(struct FrontEnd* fe);
int fe_lex_file(struct FrontEnd* fe, const char* filename);
int fe_lex_file_opened(struct FrontEnd* fe, const char* filename, FILE* f);
