#pragma once

#include "compilermacros.h"

struct Token;

typedef struct Constant128
{
    unsigned long long lower;
    unsigned long long upper;
} Constant128;

extern const Constant128 s_one_constant;
extern const Constant128 s_zero_constant;

Constant128 mp_cast(Constant128 a, unsigned flags);
Constant128 mp_u64(unsigned long long u);
Constant128 mp_i64(long long u);

/// a + b*c
Constant128 mp_fma(Constant128 a, Constant128 b, int c);
/// a - b*c
Constant128 mp_fsm(Constant128 a, Constant128 b, int c);
Constant128 mp_sub(Constant128 a, Constant128 b);
Constant128 mp_mul(Constant128 a, Constant128 b);
Constant128 mp_div(Constant128 a, Constant128 b, const struct Token* tok);
Constant128 mp_idiv(Constant128 a, int b, const struct Token* tok);
Constant128 mp_mod(Constant128 a, Constant128 b, const struct Token* tok);
Constant128 mp_bor(Constant128 a, Constant128 b);
Constant128 mp_bnot(Constant128 a);
Constant128 mp_lnot(Constant128 a);
Constant128 mp_band(Constant128 a, Constant128 b);
Constant128 mp_bxor(Constant128 a, Constant128 b);
Constant128 mp_shl(Constant128 a, Constant128 b, const struct Token* tok);
Constant128 mp_shr(Constant128 a, Constant128 b, const struct Token* tok);
Constant128 mp_neg(Constant128 a);
__forceinline static int mp_is_eq(Constant128 a, Constant128 b) { return a.lower == b.lower && a.upper == b.upper; }
__forceinline static int mp_is_nonzero(Constant128 a) { return a.lower || a.upper; }
__forceinline static int mp_is_lt(Constant128 a, Constant128 b)
{
    return a.upper > b.upper || (a.upper == b.upper && a.lower < b.lower);
}

void mpa_add(Constant128* a, Constant128 b);
