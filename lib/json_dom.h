#pragma once

#include "array.h"
#include "json.h"

typedef struct JsonDOM
{
    Array text;
    Array records;
    char add_comma;
} JsonDOM;

enum JsonParseResult jsondom_fill_records(JsonDOM* dom, JsonParse* jp);
void jsondom_diff(JsonDOM* dom1, JsonDOM* dom2, uint8_t* out);

void jsondom_destroy(JsonDOM* dom);
void jsondom_write_u64(JsonDOM* dom, uint64_t i);
void jsondom_write_i64(JsonDOM* dom, int64_t i);
void jsondom_write_string(JsonDOM* dom, const char* s, size_t n);
#define JSONDOM_WRITE_STRING(dom, s) (jsondom_write_string(dom, s, sizeof(s) - 1))
void jsondom_write_key(JsonDOM* dom, const char* s, size_t n);
#define JSONDOM_WRITE_KEY(dom, s) (jsondom_write_key(dom, s, sizeof(s) - 1))
void jsondom_write_true(JsonDOM* dom);
void jsondom_write_null(JsonDOM* dom);
void jsondom_write_false(JsonDOM* dom);
void jsondom_write_open_obj(JsonDOM* dom);
void jsondom_write_close_obj(JsonDOM* dom);
void jsondom_write_open_arr(JsonDOM* dom);
void jsondom_write_close_arr(JsonDOM* dom);
