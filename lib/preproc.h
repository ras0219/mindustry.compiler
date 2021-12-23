#pragma once

#include <stdio.h>

#include "array.h"
#include "rowcol.h"
#include "stringmap.h"

enum PreprocessorState
{
    PP_INIT,
    PP_INCLUDE,
    PP_INCLUDE_EXPECT_END,
    PP_PRAGMA,
    PP_PRAGMA_ONCE,
    PP_EXPECT_END,
    PP_DEFINE,
    PP_DEFINE_CONT,
    PP_UNDEF,
    PP_IF,
    PP_ELIF,
    PP_ELSE,
    PP_ENDIF,
    PP_IGNORE,
};

struct StringStk
{
    struct Array lengths;
    struct Array data;
};

struct Preprocessor
{
    enum PreprocessorState preproc;

    void (*on_include_cb)(struct Preprocessor*, const char* filename, FILE* f);

    char cur_if_false;
    struct Array if_stack;
    struct RowCol dir_rc;
    char to_include[128];
    size_t to_include_sz;
    size_t def_start;

    const char* include_paths;

    // Array<char*>
    struct Array files_open;
    // Array<struct ParsedFile>
    struct Array filenames;
    size_t cur_file;

    struct StringMap defines_map;
    struct Array defs_tokens;
    unsigned int prev_token_was_macrofn : 1;
    unsigned int in_directive : 1;

    // concat of null terminated strings
    struct StringStk macro_stack;
    struct Array macro_arg_idxs;
    struct Array macro_arg_seqs;
    int paren_count;

    struct Array toks;
    struct Array stringpool;
};

void preproc_init(struct Preprocessor* pp, const char* include_paths);
int preproc_file(struct Preprocessor* pp, FILE* f, const char* filename);
void preproc_destroy(struct Preprocessor* pp);
