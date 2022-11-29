#pragma once

#include <stdint.h>

#include "compilermacros.h"

struct Token;

typedef struct Constant128
{
    unsigned long long lower;
    unsigned long long upper;
} Constant128;

extern const Constant128 s_one_constant;
extern const Constant128 s_zero_constant;

__forceinline static uint8_t mp_u8(Constant128 a) { return (uint8_t)a.lower; }
__forceinline static uint16_t mp_u16(Constant128 a) { return (uint16_t)a.lower; }
__forceinline static uint32_t mp_u32(Constant128 a) { return (uint32_t)a.lower; }
__forceinline static uint64_t mp_u64(Constant128 a) { return (uint64_t)a.lower; }
__forceinline static int8_t mp_i8(Constant128 a) { return (int8_t)a.lower; }
__forceinline static int16_t mp_i16(Constant128 a) { return (int16_t)a.lower; }
__forceinline static int32_t mp_i32(Constant128 a) { return (int32_t)a.lower; }
__forceinline static int64_t mp_i64(Constant128 a) { return (int64_t)a.lower; }

Constant128 mp_cast(Constant128 a, unsigned flags);
Constant128 mp_from_u64(uint64_t u);
Constant128 mp_from_i64(int64_t u);

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
