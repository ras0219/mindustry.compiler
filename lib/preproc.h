#pragma once
#pragma once

#include <stdio.h>

#include "array.h"
#include "fwd.h"
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
    PP_DEFINE_CONT_FIRST,
    PP_DEFINE_CONT,
    PP_DEFINE_FN,
    PP_DEFINE_FN_ARG,
    PP_DEFINE_FN_COMMA,
    PP_DEFINE_FN_ELLIPSIS,
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

    const char* include_paths;

    // Array<char*>
    struct Array files_open;
    // Array<struct ParsedFile>
    struct Array filenames;
    size_t cur_file;

    struct StringMap defines_map;
    struct StringStk def_arg_names;
    struct Array defs_info;
    struct Array defs_tokens;
    size_t prev_macrodef_idx_p1;
    unsigned int in_directive : 1;

    struct StringStk macro_stack;
    struct Array macro_fn_exp;
    // Array<size_t>, points to the token before the beginning of the arg sequence
    struct Array macro_arg_offsets;
    struct Array macro_tmp_buf;
    size_t paren_count;

    struct Array toks;
    struct Array stringpool;
};

void preproc_init(struct Preprocessor* pp, const char* include_paths);
int preproc_file(struct Preprocessor* pp, FILE* f, const char* filename);
void preproc_destroy(struct Preprocessor* pp);
const char* pp_token_str(const struct Preprocessor* pp, const struct Token* tk);
