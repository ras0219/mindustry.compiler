#pragma once

#include "compilermacros.h"
#include "fwd.h"
#include "sizing.h"
#include "stdint.h"

typedef struct Interval
{
    uint64_t base, maxoff;
    Sizing sz;
} Interval;

// static const Interval s_interval_zero = {.base = 0, .maxoff = 0};
static const Interval s_interval_all = {.base = 0, .maxoff = UINT64_MAX, .sz.width = 8};
static const Interval s_interval_neg = {.base = INT64_MIN, .maxoff = INT64_MAX, .sz.width = 8};
static const Interval s_interval_pos = {.base = 1, .maxoff = INT64_MAX - 1, .sz.width = 8};
static const Interval s_interval_nonneg = {.base = 0, .maxoff = INT64_MAX, .sz.width = 8};
// static const Interval s_intervals_neg_sz[] = {
//     [1] = {.base = INT8_MIN, .maxoff = INT8_MAX - 1, .sz.width = 8},
//     [2] = {.base = INT16_MIN, .maxoff = INT16_MAX - 1, .sz.width = 2},
//     [4] = {.base = INT32_MIN, .maxoff = INT32_MAX - 1, .sz.width = 4},
//     [8] = {.base = INT64_MIN, .maxoff = INT64_MAX - 1, .sz.width = 8},
// };
static const Interval s_intervals_nonneg_sz[] = {
    [1] = {.base = 0, .maxoff = INT8_MAX},
    [2] = {.base = 0, .maxoff = INT16_MAX},
    [4] = {.base = 0, .maxoff = INT32_MAX},
    [8] = {.base = 0, .maxoff = INT64_MAX},
};
static const Interval s_intervals_pos_sz[] = {
    [1] = {.base = 1, .maxoff = INT8_MAX - 1},
    [2] = {.base = 1, .maxoff = INT16_MAX - 1},
    [4] = {.base = 1, .maxoff = INT32_MAX - 1},
    [8] = {.base = 1, .maxoff = INT64_MAX - 1},
};

static __forceinline int interval_wrapped(Interval i) { return UINT64_MAX - i.maxoff < i.base; }

int interval_contains_0(Interval i);

static __forceinline int interval_contains(Interval i, uint64_t j) { return i.maxoff >= j - i.base; }

// static int interval_is_intersect(Interval i, Interval j)
// {
//     const uint64_t base = j.base - i.base;
//     return i.maxoff >= base || i.maxoff >= base + j.maxoff;
// }

void interval_fmt(struct Array* buf, Interval i);
Interval interval_merge(Interval i, Interval j);
int interval_intersection(Interval i, Interval j, Interval* out);

Interval interval_cast(Interval i, Sizing sz);

Interval interval_sub(Interval i, Interval j);
int interval_sub_ofchk(Interval i, Interval j);
Interval interval_add(Interval i, Interval j);
int interval_add_ofchk(Interval i, Interval j);
Interval interval_neg(Interval i);
int interval_neg_ofchk(Interval i);
Interval interval_udiv(Interval i, Interval j);
Interval interval_mult(Interval i, Interval j);
Interval interval_div(Interval i, Interval j);

int64_t interval_signed_min(Interval i);
int64_t interval_signed_max(Interval i);
