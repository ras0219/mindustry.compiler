#include "interval.h"

#include "array.h"
#include "stdlibe.h"
#include "string.h"

// static int interval_is_intersect(Interval i, Interval j)
// {
//     const uint64_t base = j.base - i.base;
//     return i.maxoff >= base || i.maxoff >= base + j.maxoff;
// }

// static uint64_t interval_signed_min(Interval i)
// {
//     if (interval_contains(i, INT64_MIN))
//         return INT64_MIN;
//     else
//         return i.base;
// }
// static uint64_t interval_i64_max(Interval i)
// {
//     if (interval_contains(i, INT64_MAX))
//         return INT64_MAX;
//     else
//         return i.base + i.maxoff;
// }

int interval_contains_0(Interval i)
{
    return i.base == 0 || ((i.base + i.maxoff) & s_umax_sizing[i.sz.width]) < i.base;
}
int interval_contains_nonzero(Interval i) { return i.base || i.maxoff; }

int interval_remove_0(Interval* out, Interval i)
{
    *out = i;
    if (out->maxoff == s_umax_sizing[out->sz.width])
    {
        out->maxoff = s_umax_sizing[out->sz.width] - 1;
        out->base = 1;
        return 1;
    }
    else if (out->base == 0)
    {
        if (out->maxoff)
        {
            ++out->base;
            --out->maxoff;
            return 1;
        }
        else
            return 0;
    }
    else
    {
        return 1;
    }
}

int interval_remove_nonzero(Interval* out, Interval i)
{
    *out = i;
    out->base = 0;
    out->maxoff = 0;
    return interval_contains_0(i);
}

static __forceinline uint64_t u64_sat_add(uint64_t a, uint64_t b) { return UINT64_MAX - a < b ? UINT64_MAX : a + b; }

// static __forceinline int64_t interval_signed_base_to_i64(Interval i)
// {
//     return (i.base ^ s_imaxp1_sizing[i.sz.width]) - s_imaxp1_sizing[i.sz.width];
// }

static __forceinline int64_t interval_signed_to_i64(uint64_t i, uint32_t width)
{
    return ((i & s_umax_sizing[width]) ^ s_imaxp1_sizing[width]) - s_imaxp1_sizing[width];
}
static __forceinline int64_t interval_signed_from_i64(uint64_t i, uint32_t width)
{
    return ((i + s_imaxp1_sizing[width]) ^ s_imaxp1_sizing[width]) & s_umax_sizing[width];
}

void interval_fmt(Array* buf, Interval i)
{
    if (i.sz.is_signed)
        // array_appendf(buf,
        //               "%lld to %lld",
        //               interval_signed_min(i),
        //               interval_signed_max(i));
        array_appendf(buf, "%llu (+%llu)", i.base, i.maxoff);
    else
        array_appendf(buf, "%llu to %llu", i.base, (i.base + i.maxoff) & s_umax_sizing[i.sz.width]);
}
// static uint64_t icast(uint64_t n, Sizing sz)
// {
//     switch (sz.width)
//     {
//         case 1: return sz.is_signed && n > INT8_MAX ? (n | INT8_MIN) : (n & UINT8_MAX);
//         case 2: return sz.is_signed && n > INT16_MAX ? (n | INT16_MIN) : (n & UINT16_MAX);
//         case 4: return sz.is_signed && n > INT32_MAX ? (n | INT32_MIN) : (n & UINT32_MAX);
//         case 8: return n;
//         default: abort();
//     }
// }
Interval interval_merge(Interval i, Interval j)
{
    const uint64_t i_to_j = u64_sat_add(j.maxoff, j.base - i.base);
    const uint64_t j_to_i = u64_sat_add(i.maxoff, i.base - j.base);
    const Interval ret_i = {.base = i.base, .maxoff = i.maxoff < i_to_j ? i_to_j : i.maxoff};
    const Interval ret_j = {.base = j.base, .maxoff = j.maxoff < j_to_i ? j_to_i : j.maxoff};
    if (ret_i.maxoff > ret_j.maxoff)
        return ret_j;
    else
        return ret_i;
}

int interval_intersection(Interval i, Interval j, Interval* out)
{
    memset(out, 0, sizeof(*out));
    if (i.maxoff >= j.base - i.base)
    {
        out->base = j.base;
        out->sz = i.sz;
        uint64_t j_to_i = i.maxoff - (j.base - i.base);
        out->maxoff = j.maxoff < j_to_i ? j.maxoff : j_to_i;
        return 1;
    }
    else if (j.maxoff >= i.base - j.base)
    {
        out->base = i.base;
        out->sz = i.sz;
        uint64_t i_to_j = j.maxoff - (i.base - j.base);
        out->maxoff = i.maxoff < i_to_j ? i.maxoff : i_to_j;
        return 1;
    }
    else
        return 0;
}

int interval_relation_lti(Interval* out, int64_t j)
{
    int64_t n = interval_signed_min(*out);
    int64_t m = interval_signed_max(*out);
    if (n >= j)
    {
        return 0;
    }
    else if (m >= j)
    {
        *out = interval_from_signed_limits(n, j - 1, out->sz.width);
    }
    return 1;
}
int interval_relation_ltu(Interval* out, uint64_t j)
{
    uint64_t n = interval_unsigned_min(*out);
    uint64_t m = interval_unsigned_max(*out);
    if (n >= j)
    {
        return 0;
    }
    else if (m >= j)
    {
        *out = interval_from_unsigned_limits(n, j - 1, out->sz.width);
    }
    return 1;
}

int interval_relation_ltei(Interval* out, int64_t j)
{
    int64_t n = interval_signed_min(*out);
    int64_t m = interval_signed_max(*out);
    if (n > j)
    {
        return 0;
    }
    else if (m > j)
    {
        *out = interval_from_signed_limits(n, j, out->sz.width);
    }
    return 1;
}
int interval_relation_lteu(Interval* out, uint64_t j)
{
    uint64_t n = interval_unsigned_min(*out);
    uint64_t m = interval_unsigned_max(*out);
    if (m > j)
    {
        return 0;
    }
    else if (n > j)
    {
        *out = interval_from_unsigned_limits(n, j, out->sz.width);
    }
    return 1;
}
int interval_relation_gti(Interval* out, int64_t j)
{
    int64_t n = interval_signed_min(*out);
    int64_t m = interval_signed_max(*out);
    if (m <= j)
    {
        return 0;
    }
    else if (n <= j)
    {
        *out = interval_from_signed_limits(j + 1, m, out->sz.width);
    }
    return 1;
}
int interval_relation_gtu(Interval* out, uint64_t j)
{
    uint64_t n = interval_unsigned_min(*out);
    uint64_t m = interval_unsigned_max(*out);
    if (m <= j)
    {
        return 0;
    }
    else if (n <= j)
    {
        *out = interval_from_unsigned_limits(j + 1, m, out->sz.width);
    }
    return 1;
}

int interval_relation_gtei(Interval* out, int64_t j)
{
    int64_t n = interval_signed_min(*out);
    int64_t m = interval_signed_max(*out);
    if (m < j)
    {
        return 0;
    }
    else if (n < j)
    {
        *out = interval_from_signed_limits(j, m, out->sz.width);
    }
    return 1;
}
int interval_relation_gteu(Interval* out, uint64_t j)
{
    uint64_t n = interval_unsigned_min(*out);
    uint64_t m = interval_unsigned_max(*out);
    if (m < j)
    {
        return 0;
    }
    else if (n < j)
    {
        *out = interval_from_unsigned_limits(j, m, out->sz.width);
    }
    return 1;
}

int interval_relation_eq(Interval* out, Interval j) { return interval_intersection(*out, j, out); }
int interval_relation_neq(Interval* out, Interval j)
{
    if (j.maxoff == 0 && j.base == 0)
    {
        return interval_remove_0(out, *out);
    }
    return 1;
}

enum interval_intersection_result interval_intersect_lti(Interval i, int64_t j, Interval* inner, Interval* outer)
{
    *inner = i;
    *outer = i;
    if (i.sz.is_signed)
    {
        int64_t n = interval_signed_min(i);
        int64_t m = interval_signed_max(i);
        if (m < j)
        {
            return interval_only_inner;
        }
        else if (n >= j)
        {
            return interval_only_outer;
        }
        else
        {
            inner->base = n;
            inner->maxoff = j - n - 1;
            outer->base = j;
            outer->maxoff = m - j;
            return interval_outer_inner;
        }
    }
    else
    {
        if (j <= 0)
        {
            return interval_only_outer;
        }
        return interval_intersect_ltu(i, j, inner, outer);
    }
}
enum interval_intersection_result interval_intersect_ltu(Interval i, uint64_t j, Interval* inner, Interval* outer)
{
    *inner = i;
    *outer = i;
    if (i.sz.is_signed)
    {
        if (j > INT64_MAX)
        {
            return interval_only_inner;
        }
        return interval_intersect_lti(i, j, inner, outer);
    }
    else
    {
        uint64_t n = interval_unsigned_min(i);
        uint64_t m = interval_unsigned_max(i);
        if (m < j)
        {
            return interval_only_inner;
        }
        else if (n >= j)
        {
            return interval_only_outer;
        }
        else
        {
            inner->base = n;
            inner->maxoff = j - n - 1;
            outer->base = j;
            outer->maxoff = m - j;
            return interval_outer_inner;
        }
    }
}
enum interval_intersection_result interval_intersect_eq(Interval i, uint64_t j, Interval* inner, Interval* outer);
enum interval_intersection_result interval_intersect_false(Interval i, Interval* inner, Interval* outer);

// static int interval_split_nonneg(Interval i, Interval* out)
// {
//     return interval_intersection(i, s_intervals_nonneg_sz[i.sz.width], out);
// }
// static int interval_split_neg(Interval i, Interval* out)
// {
//     return interval_intersection(i, s_intervals_neg_sz[i.sz.width], out);
// }

Interval interval_cast_unsigned(Interval i) { return i; }

Interval interval_cast_signed(Interval i) { return i; }

Interval interval_cast_width(Interval i, uint32_t width)
{
    if (width < i.sz.width)
    {
        i.base &= s_umax_sizing[width];
    }
    else if (width > i.sz.width && i.base > s_imax_sizing[i.sz.width])
    {
        i.base = i.base - s_umax_sizing[i.sz.width] + s_umax_sizing[width];
    }
    if (i.maxoff > s_umax_sizing[width]) i.maxoff = s_umax_sizing[width];
    i.sz.width = width;
    return i;
}
// Interval interval_cast(Interval i, Sizing sz)
// {
//     if (sz.is_signed == i.sz.is_signed && i.sz.width <= sz.width)
//     {
//         i.sz.width = sz.width;
//         return i;
//     }
//     if (!sz.is_signed && !i.sz.is_signed)
//     {
//     }
//     if (!sz.is_signed)
//     {
//         i = interval_cast_unsigned(i);
//         i.sz.width = sz.width;
//         return i;
//     }
//     if (!i.sz.is_signed && !sz.is_signed)
//     {
//         Interval ret = {
//             .base = i.base & s_umax_sizing[sz.width],
//             .maxoff = i.maxoff < s_umax_sizing[sz.width] ? i.maxoff : s_umax_sizing[sz.width],
//             .sz = sz,
//         };
//         return ret;
//     }

//     Interval ret = {.base = i.base, .maxoff = i.maxoff, .sz = sz};
//     if (i.sz.width < sz.width && sz.is_signed == i.sz.is_signed)
//     {
//         return ret;
//     }
//     if (interval_contains(i, INT64_MIN))
//     {
//     }
//     return ret;
// }

static __forceinline uint64_t interval_sat_add(uint64_t a, uint64_t b, uint64_t max)
{
    if (a > max) abort();
    if (b > max) abort();
    return max - a < b ? max : a + b;
}

static __forceinline uint64_t interval_invert_signed_base(Interval i) { return i.base ^ s_imaxp1_sizing[i.sz.width]; }

static __forceinline Interval interval_to_offset_u64(Interval i)
{
    if (i.sz.is_signed)
    {
        i.base = interval_invert_signed_base(i) + (INT64_MAX + 1ULL) - s_imaxp1_sizing[i.sz.width];
    }
    else
    {
        i.base += INT64_MAX + 1ULL;
    }
    i.sz.is_signed = 0;
    i.sz.width = 8;
    return i;
}

Interval interval_add_u64(Interval i, Interval j)
{
    Interval res = {
        .base = i.base + j.base,
        .maxoff = u64_sat_add(i.maxoff, j.maxoff),
        .sz.width = 8,
    };
    return res;
}
Interval interval_add_signed(Interval i, Interval j, int* signed_of)
{
    // static const uint64_t s_offsets[] = {
    //     [1] = INT64_MAX + 1ULL,
    //     [2] = INT64_MAX + 1ULL,
    //     [4] = INT64_MAX + 1ULL,
    //     [8] = 0,
    // };

    *signed_of = 0;
    if (j.base == 0 && j.maxoff == 0) return i;
    if (i.base == 0 && i.maxoff == 0) return j;

    Interval a = interval_to_offset_u64(i);
    Interval b = interval_to_offset_u64(j);
    Interval c = interval_add_u64(a, b);
    const uint64_t a_max = a.base + a.maxoff;
    const uint64_t b_max = b.base + b.maxoff;
    if (a_max < a.base) *signed_of |= 1;
    if (b_max < b.base) *signed_of |= 1;
    if (a.base < INT64_MAX + 1ULL && b.base < INT64_MAX + 1ULL)
    {
        *signed_of |= c.base < (INT64_MAX + 1ULL);
    }
    if (a_max > INT64_MAX + 1ULL && b_max > INT64_MAX + 1ULL)
    {
        *signed_of |= c.base + c.maxoff >= (INT64_MAX + 1ULL);
    }

    return c;

    Interval res = {
        .base = (i.base + j.base) & s_umax_sizing[i.sz.width],
        .maxoff = interval_sat_add(i.maxoff, j.maxoff, s_umax_sizing[i.sz.width]),
        .sz = i.sz,
    };
    return res;
}

Interval interval_cast(Interval i, Sizing sz)
{
    if (sz.width < i.sz.width)
    {
        i.base = i.base & s_umax_sizing[sz.width];
        if (i.maxoff > s_umax_sizing[sz.width]) i.maxoff = s_umax_sizing[sz.width];
    }
    else if (sz.width > i.sz.width)
    {
        if (i.sz.is_signed)
        {
            if (i.base ^ s_imaxp1_sizing[i.sz.width] > s_umax_sizing[i.sz.width] - i.maxoff)
            {
                // signed overflow, reset to full range of smaller type
                i.base = s_imin_sizing[i.sz.width];
                i.maxoff = s_umax_sizing[i.sz.width];
            }
            else
            {
                i.base = interval_signed_to_i64(i.base, i.sz.width);
                i.base = interval_signed_from_i64(i.base, sz.width);
            }
        }
        else
        {
            if (i.base > s_umax_sizing[i.sz.width] - i.maxoff)
            {
                // unsigned overflow, reset to full range of smaller type
                i.base = 0;
                i.maxoff = s_umax_sizing[i.sz.width];
            }
        }
    }
    i.sz = sz;
    return i;
}

int interval_add_ofchk(Interval i, Interval j)
{
    int64_t in = interval_signed_min(i);
    int64_t jn = interval_signed_min(j);
    if (in < 0 && jn < 0 && in < s_i64_imin_sizing[i.sz.width] - jn) return 1;

    int64_t ix = interval_signed_max(i);
    int64_t jx = interval_signed_max(j);
    if (ix > 0 && jx > 0 && ix > s_i64_imax_sizing[i.sz.width] - jx) return 1;
    return 0;
}
Interval interval_add_impl(Interval i, Interval j, int* signed_of)
{
    *signed_of = 0;
    i.base = (j.base + i.base) & s_umax_sizing[i.sz.width];
    i.maxoff = interval_sat_add(j.maxoff, i.maxoff, s_umax_sizing[i.sz.width]);
    return i;
}
Interval interval_add(Interval i, Interval j)
{
    int x;
    return interval_add_impl(i, j, &x);
}

int interval_sub_ofchk(Interval i, Interval j) { return 0; }
Interval interval_sub(Interval i, Interval j) { return interval_add(i, interval_neg(j)); }

int interval_neg_ofchk(Interval i) { return 0; }
Interval interval_neg(Interval i)
{
    Interval res = {
        .base = (-i.base) & s_umax_sizing[i.sz.width],
        .maxoff = i.maxoff,
        .sz = i.sz,
    };
    return res;
}
Interval interval_udiv(Interval i, Interval j)
{
    const uint64_t lower = i.base / (j.base + j.maxoff);
    const uint64_t upper = (i.maxoff + i.base) / j.base;
    Interval res = {
        .base = lower,
        .maxoff = upper - lower,
        .sz = i.sz,
    };
    return res;
}

static __forceinline uint64_t u64_sat_mult(uint64_t a, uint64_t b) { return a > UINT64_MAX / b ? UINT64_MAX : a * b; }

Interval interval_umult(Interval i, Interval j)
{
    // [a, b] * [c, d] = [a*c, b*d]
    // [a, a+b] * [c, c+d] = [a*c, a*c+ad+bc+bd]
    // {a, b} * {c, d} => {a*c, a*d + b*c + b*d}

    uint64_t e = u64_sat_mult(i.base, j.maxoff);
    uint64_t f = u64_sat_mult(j.base, i.maxoff);
    uint64_t g = u64_sat_mult(i.maxoff, j.maxoff);
    i.maxoff = u64_sat_add(u64_sat_add(e, f), g);
    i.base *= j.base;
    return i;
}

static __forceinline int interval_is_signed_wrap(Interval i)
{
    return s_umax_sizing[i.sz.width] - i.maxoff < interval_invert_signed_base(i);
}
int64_t interval_signed_min(Interval i)
{
    if (!i.sz.is_signed) abort();
    if (interval_is_signed_wrap(i))
        return s_i64_imin_sizing[i.sz.width];
    else
        return interval_signed_to_i64(i.base, i.sz.width);
}
int64_t interval_signed_max(Interval i)
{
    if (interval_is_signed_wrap(i))
        return s_i64_imax_sizing[i.sz.width];
    else
        return interval_signed_to_i64(i.base + i.maxoff, i.sz.width);
}
static __forceinline int interval_is_unsigned_wrap(Interval i) { return s_umax_sizing[i.sz.width] - i.maxoff < i.base; }
uint64_t interval_unsigned_min(Interval i)
{
    if (i.sz.is_signed) abort();
    if (interval_is_unsigned_wrap(i))
        return 0;
    else
        return i.base;
}
uint64_t interval_unsigned_max(Interval i) { return interval_sat_add(i.base, i.maxoff, s_umax_sizing[i.sz.width]); }
IntervalLimitsI64 interval_signed_limits(Interval i)
{
    IntervalLimitsI64 r = {
        .max = interval_signed_max(i),
        .min = interval_signed_min(i),
    };
    return r;
}
IntervalLimitsU64 interval_unsigned_limits(Interval i)
{
    IntervalLimitsU64 r = {
        .max = interval_unsigned_max(i),
        .min = interval_unsigned_min(i),
    };
    return r;
}

Interval interval_from_signed_limits(int64_t imin, int64_t imax, uint32_t width)
{
    Interval i = {
        .base = interval_signed_from_i64(imin, width),
        .maxoff = imax - imin,
        .sz.is_signed = 1,
        .sz.width = width,
    };
    if (i.maxoff > s_umax_sizing[width]) i.maxoff = s_umax_sizing[width];
    return i;
}

Interval interval_from_unsigned_limits(uint64_t umin, uint64_t umax, uint32_t width)
{
    Interval i = {
        .base = umin,
        .maxoff = umax - umin,
        .sz.width = width,
    };
    if (i.maxoff > s_umax_sizing[width]) i.maxoff = s_umax_sizing[width];
    return i;
}

static __forceinline int64_t i64_sat_mult(int64_t a, int64_t b)
{
    if (a == 0 || b == 0) return 0;
    if (a < 0)
    {
        if (b < 0)
        {
            if (b < INT64_MAX / a) return INT64_MAX;
        }
        else
        {
            if (a < INT64_MIN / b) return INT64_MIN;
        }
    }
    else
    {
        if (b < 0)
        {
            if (b < INT64_MIN / a) return INT64_MIN;
        }
        else
        {
            if (a > INT64_MAX / b) return INT64_MAX;
        }
    }
    return a > UINT64_MAX / b ? UINT64_MAX : a * b;
}

static int64_t min4_i64(int64_t a, int64_t b, int64_t c, int64_t d)
{
    if (a > b) a = b;
    if (c > d) c = d;
    return a < c ? a : c;
}
static int64_t max4_i64(int64_t a, int64_t b, int64_t c, int64_t d)
{
    if (a < b) a = b;
    if (c < d) c = d;
    return a > c ? a : c;
}

Interval interval_imult(Interval i, Interval j)
{
    int64_t in = interval_signed_min(i);
    int64_t ix = interval_signed_max(i);
    int64_t jn = interval_signed_min(j);
    int64_t jx = interval_signed_max(j);

    int64_t nn = i64_sat_mult(in, jn);
    int64_t xx = i64_sat_mult(ix, jx);
    int64_t nx = i64_sat_mult(in, jx);
    int64_t xn = i64_sat_mult(ix, jn);

    int64_t kn = min4_i64(nn, xx, nx, xn);
    int64_t kx = max4_i64(nn, xx, nx, xn);

    return interval_from_signed_limits(kn, kx, i.sz.width);
}
Interval interval_mult(Interval i, Interval j) { return i.sz.is_signed ? interval_imult(i, j) : interval_umult(i, j); }

Interval interval_div(Interval i, Interval j)
{
    if (interval_contains(j, 0)) return s_interval_all;

    Interval j_neg, j_pos, i_neg, i_pos;
    int i_has_neg = interval_intersection(i, s_interval_neg, &i_neg);
    if (i_has_neg) i_neg = interval_neg(i_neg);
    int i_has_pos = interval_intersection(i, s_interval_nonneg, &i_pos);
    int j_has_neg = interval_intersection(j, s_interval_neg, &j_neg);
    if (j_has_neg) j_neg = interval_neg(j_neg);
    int j_has_pos = interval_intersection(j, s_interval_pos, &j_pos);
    int first = 1;
    Interval result;
    if (i_has_pos && j_has_pos)
    {
        result = interval_udiv(i_pos, j_pos);
        first = 0;
    }
    if (i_has_neg && j_has_pos)
    {
        Interval x = interval_neg(interval_udiv(i_neg, j_pos));
        result = first ? x : interval_merge(result, x);
        first = 0;
    }
    if (i_has_pos && j_has_neg)
    {
        Interval x = interval_neg(interval_udiv(i_pos, j_neg));
        result = first ? x : interval_merge(result, x);
        first = 0;
    }
    if (i_has_neg && j_has_neg)
    {
        Interval x = interval_udiv(i_neg, j_neg);
        result = first ? x : interval_merge(result, x);
    }
    if (first) abort();
    return result;
}
Interval interval_lt(Interval a, Interval b)
{
    IntervalLimitsI64 l = interval_signed_limits(a);
    IntervalLimitsI64 r = interval_signed_limits(b);
    if (l.max < r.min)
        return s_interval_one;
    else if (l.min >= r.max)
        return s_interval_zero;
    else
        return s_interval_zero_one;
}
Interval interval_ltu(Interval a, Interval b)
{
    IntervalLimitsU64 l = interval_unsigned_limits(a);
    IntervalLimitsU64 r = interval_unsigned_limits(b);
    if (l.max < r.min)
        return s_interval_one;
    else if (l.min >= r.max)
        return s_interval_zero;
    else
        return s_interval_zero_one;
}
Interval interval_lte(Interval a, Interval b)
{
    IntervalLimitsI64 l = interval_signed_limits(a);
    IntervalLimitsI64 r = interval_signed_limits(b);
    if (l.max <= r.min)
        return s_interval_one;
    else if (l.min > r.max)
        return s_interval_zero;
    else
        return s_interval_zero_one;
}
Interval interval_lteu(Interval a, Interval b)
{
    IntervalLimitsU64 l = interval_unsigned_limits(a);
    IntervalLimitsU64 r = interval_unsigned_limits(b);
    if (l.max <= r.min)
        return s_interval_one;
    else if (l.min > r.max)
        return s_interval_zero;
    else
        return s_interval_zero_one;
}
Interval interval_eq(Interval a, Interval b)
{
    if (a.maxoff == 0 && b.maxoff == 0 && a.base == b.base) return s_interval_one;
    IntervalLimitsI64 l = interval_signed_limits(a);
    IntervalLimitsI64 r = interval_signed_limits(b);
    if (l.max < r.min || l.min > r.max)
        return s_interval_zero;
    else
        return s_interval_zero_one;
}
Interval interval_equ(Interval a, Interval b)
{
    if (a.maxoff == 0 && b.maxoff == 0 && a.base == b.base) return s_interval_one;
    IntervalLimitsU64 l = interval_unsigned_limits(a);
    IntervalLimitsU64 r = interval_unsigned_limits(b);
    if (l.max < r.min || l.min > r.max)
        return s_interval_zero;
    else
        return s_interval_zero_one;
}
