#pragma once

#include <stdint.h>
#include <stdlib.h>

typedef struct JsonRecord
{
    unsigned char kind;
    unsigned char is_end;
    size_t offset;
    size_t n;
    int row;
    int col;
} JsonRecord;

typedef struct JsonParse
{
    // internal state
    int row;
    int col;
    uint64_t stk;
    unsigned char state;
} JsonParse;

#define FOREACH_JSON_CB(X)                                                                                             \
    X(number)                                                                                                          \
    X(string)                                                                                                          \
    X(key)                                                                                                             \
    X(object_begin)                                                                                                    \
    X(object_end)                                                                                                      \
    X(array_begin)                                                                                                     \
    X(array_end)                                                                                                       \
    X(kw_true)                                                                                                         \
    X(kw_false)                                                                                                        \
    X(kw_null)

enum
{
#define Y(x) jsonr_##x,
    FOREACH_JSON_CB(Y)
#undef Y
};

extern const char* const jsonr_to_string[];

enum JsonParseResult
{
    json_err_success,
    json_err_state,
    json_err_expected_true,
    json_err_expected_false,
    json_err_expected_null,
    json_err_expected_eof,
    json_err_unexpected_eof,
    json_err_unexpected_newline,
    json_err_max_depth,
    json_err_expected_endobj,
    json_err_expected_endarr,
    json_err_expected_colon,
    json_err_expected_key,
    json_err_expected_element,
};

// int json_parse(JsonParse* j, const JsonSAXVTable* t, void* userp, const char* data, size_t n);
enum JsonParseResult json_parse_end(JsonParse* j,
                                    const char* data,
                                    size_t n_data,
                                    size_t* data_used,
                                    JsonRecord* records,
                                    size_t n_records,
                                    size_t* records_used);
