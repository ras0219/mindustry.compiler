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

#define FOREACH_JSON_ERR(X)                                                                                            \
    X(json_err_success)                                                                                                \
    X(json_err_state)                                                                                                  \
    X(json_err_expected_true)                                                                                          \
    X(json_err_expected_false)                                                                                         \
    X(json_err_expected_null)                                                                                          \
    X(json_err_expected_eof)                                                                                           \
    X(json_err_unexpected_eof)                                                                                         \
    X(json_err_unexpected_newline)                                                                                     \
    X(json_err_max_depth)                                                                                              \
    X(json_err_expected_endobj)                                                                                        \
    X(json_err_expected_endarr)                                                                                        \
    X(json_err_expected_colon)                                                                                         \
    X(json_err_expected_digit)                                                                                         \
    X(json_err_unexpected_digit)                                                                                       \
    X(json_err_expected_key)                                                                                           \
    X(json_err_expected_element)

enum JsonParseResult
{
#define Y(x) x,
    FOREACH_JSON_ERR(Y)
#undef Y
};

extern const char* const json_err_to_string[];

enum JsonParseResult json_parse_partial(JsonParse* j,
                                        const char* data,
                                        size_t n_data,
                                        size_t* data_used,
                                        JsonRecord* records,
                                        size_t n_records,
                                        size_t* records_used);

enum JsonParseResult json_parse_end(JsonParse* j,
                                    const char* data,
                                    size_t n_data,
                                    size_t* data_used,
                                    JsonRecord* records,
                                    size_t n_records,
                                    size_t* records_used);

enum
{
    jsond_left,
    jsond_right,
    jsond_both,
};

// out must be (n1 + n2) elements
void json_diff(
    const JsonRecord* r1, size_t n1, const char* doc1, const JsonRecord* r2, size_t n2, const char* doc2, uint8_t* out);
