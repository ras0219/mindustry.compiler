#pragma once

#include <stdint.h>

typedef struct Sizing
{
    uint32_t is_signed : 1, width : 31;
} Sizing;

static const Sizing s_sizing_ptr = {.width = 8};
static const Sizing s_sizing_int = {
    .is_signed = 1,
    .width = 4,
};
static const Sizing s_sizing_uint = {
    .width = 4,
};

static const uint64_t s_umax_sizing[] = {
    [1] = UINT8_MAX,
    [2] = UINT16_MAX,
    [4] = UINT32_MAX,
    [8] = UINT64_MAX,
};
static const uint64_t s_imax_sizing[] = {
    [1] = INT8_MAX,
    [2] = INT16_MAX,
    [4] = INT32_MAX,
    [8] = INT64_MAX,
};
static const uint64_t s_imaxp1_sizing[] = {
    [1] = INT8_MAX + 1ULL,
    [2] = INT16_MAX + 1ULL,
    [4] = INT32_MAX + 1ULL,
    [8] = INT64_MAX + 1ULL,
};

static const uint64_t s_imin_sizing[] = {
    [1] = INT8_MIN,
    [2] = INT16_MIN,
    [4] = INT32_MIN,
    [8] = INT64_MIN,
};

static const int64_t s_i64_imin_sizing[] = {
    [1] = INT8_MIN,
    [2] = INT16_MIN,
    [4] = INT32_MIN,
    [8] = INT64_MIN,
};
static const uint64_t s_i64_imax_sizing[] = {
    [1] = INT8_MAX,
    [2] = INT16_MAX,
    [4] = INT32_MAX,
    [8] = INT64_MAX,
};
