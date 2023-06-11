#pragma once

#include <stdint.h>
#include <stdlib.h>

typedef struct JsonParse
{
    int row;
    int col;

    // internal state
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
    X(kw_null)                                                                                                         \
    X(error)

typedef int (*json_sax_number_cb)(void* userp, const char* encoded, size_t n, int is_end);

typedef struct JsonSAXVTable
{
    const json_sax_number_cb number;
    int (*const string)(void* userp, const char* encoded, size_t n, int is_end);
    int (*const key)(void* userp, const char* encoded, size_t n, int is_end);
    int (*const object_begin)(void* userp);
    int (*const object_end)(void* userp);
    int (*const array_begin)(void* userp);
    int (*const array_end)(void* userp);
    int (*const kw_true)(void* userp);
    int (*const kw_false)(void* userp);
    int (*const kw_null)(void* userp);
    int (*const error)(void* userp, const char* errmsg);
} JsonSAXVTable;

// int json_parse(JsonParse* j, const JsonSAXVTable* t, void* userp, const char* data, size_t n);
int json_parse_end(JsonParse* j, const JsonSAXVTable* t, void* userp, const char* data, size_t n);
