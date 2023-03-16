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

static const Interval s_interval_zero = {.base = 0, .maxoff = 0, .sz.width = 4, .sz.is_signed = 1};
static const Interval s_interval_one = {.base = 1, .maxoff = 0, .sz.width = 4, .sz.is_signed = 1};
static const Interval s_interval_zero_one = {.base = 0, .maxoff = 1, .sz.width = 4, .sz.is_signed = 1};

static const Interval s_interval_i64 = {.base = INT64_MIN, .maxoff = UINT64_MAX, .sz.width = 8, .sz.is_signed = 1};
static const Interval s_interval_i32 = {
    .base = INT32_MAX + 1ULL, .maxoff = UINT32_MAX, .sz.width = 4, .sz.is_signed = 1};
static const Interval s_interval_i16 = {
    .base = INT16_MAX + 1ULL, .maxoff = UINT16_MAX, .sz.width = 2, .sz.is_signed = 1};
static const Interval s_interval_i8 = {.base = INT8_MAX + 1ULL, .maxoff = UINT8_MAX, .sz.width = 1, .sz.is_signed = 1};

static const Interval s_interval_u64 = {.base = 0, .maxoff = UINT64_MAX, .sz.width = 8};
static const Interval s_interval_u32 = {.base = 0, .maxoff = UINT32_MAX, .sz.width = 4};
static const Interval s_interval_u16 = {.base = 0, .maxoff = UINT16_MAX, .sz.width = 2};
static const Interval s_interval_u8 = {.base = 0, .maxoff = UINT8_MAX, .sz.width = 1};

static __forceinline int interval_wrapped(Interval i) { return UINT64_MAX - i.maxoff < i.base; }

int interval_contains_0(Interval i);
int interval_contains_nonzero(Interval i);

int interval_remove_0(Interval* out, Interval i);
int interval_remove_nonzero(Interval* out, Interval i);

static __forceinline int interval_contains(Interval i, uint64_t j) { return i.maxoff >= j - i.base; }

// static int interval_is_intersect(Interval i, Interval j)
// {
//     const uint64_t base = j.base - i.base;
//     return i.maxoff >= base || i.maxoff >= base + j.maxoff;
// }

void interval_fmt(struct Array* buf, Interval i);
Interval interval_merge(Interval i, Interval j);
int interval_intersection(Interval i, Interval j, Interval* out);

enum interval_intersection_result
{
    interval_only_inner,
    interval_only_outer,
    interval_outer_inner,
};
enum interval_intersection_result interval_intersect_lti(Interval i, int64_t j, Interval* inner, Interval* outer);
enum interval_intersection_result interval_intersect_ltu(Interval i, uint64_t j, Interval* inner, Interval* outer);
enum interval_intersection_result interval_intersect_ltei(Interval i, int64_t j, Interval* inner, Interval* outer);
enum interval_intersection_result interval_intersect_lteu(Interval i, uint64_t j, Interval* inner, Interval* outer);
enum interval_intersection_result interval_intersect_eq(Interval i, uint64_t j, Interval* inner, Interval* outer);
enum interval_intersection_result interval_intersect_false(Interval i, Interval* inner, Interval* outer);

int interval_relation_lti(Interval* out, int64_t j);
int interval_relation_ltu(Interval* out, uint64_t j);
int interval_relation_ltei(Interval* out, int64_t j);
int interval_relation_lteu(Interval* out, uint64_t j);
int interval_relation_gti(Interval* out, int64_t j);
int interval_relation_gtu(Interval* out, uint64_t j);
int interval_relation_gtei(Interval* out, int64_t j);
int interval_relation_gteu(Interval* out, uint64_t j);

int interval_relation_eq(Interval* out, Interval i);
int interval_relation_neq(Interval* out, Interval j);

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
uint64_t interval_unsigned_min(Interval i);
uint64_t interval_unsigned_max(Interval i);

typedef struct IntervalLimitsI64
{
    int64_t min;
    int64_t max;
} IntervalLimitsI64;
IntervalLimitsI64 interval_signed_limits(Interval i);
Interval interval_from_signed_limits(int64_t imin, int64_t imax, uint32_t width);

typedef struct IntervalLimitsU64
{
    uint64_t min;
    uint64_t max;
} IntervalLimitsU64;
IntervalLimitsU64 interval_unsigned_limits(Interval i);
Interval interval_from_unsigned_limits(uint64_t umin, uint64_t umax, uint32_t width);

Interval interval_lt(Interval a, Interval b);
Interval interval_ltu(Interval a, Interval b);
Interval interval_lte(Interval a, Interval b);
Interval interval_lteu(Interval a, Interval b);
Interval interval_eq(Interval a, Interval b);
Interval interval_equ(Interval a, Interval b);
