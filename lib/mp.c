#include "mp.h"

#include <limits.h>
#include <stdint.h>
#include <stdlib.h>

#include "errors.h"
#include "typestr.h"

const Constant128 s_zero_constant = {0};
const Constant128 s_one_constant = {.lower = 1};

static void mp_mul_i(Constant128* a, Constant128* b)
{
    const uint64_t bsr = (b->lower >> 1) + (b->upper << 63);
    const uint64_t asr = (a->lower >> 1) + (a->upper << 63);
    const uint64_t aup = a->lower * bsr + asr * (b->lower & 1);
    a->upper = aup >> 63;
    a->lower = (aup << 1) + (a->lower & b->lower & 1);
}

static void mp_neg_i(Constant128* a)
{
    a->lower = ~a->lower + 1;
    if (a->lower != 0) a->upper = !a->upper;
}

Constant128 mp_cast(Constant128 a, unsigned flags)
{
    switch (flags & (TYPE_MASK_WIDTH))
    {
        case TYPE_FLAGS_SIGNED | TYPE_FLAGS_WIDTH1: a.lower = (int8_t)a.lower; break;
        case TYPE_FLAGS_SIGNED | TYPE_FLAGS_WIDTH2: a.lower = (int16_t)a.lower; break;
        case TYPE_FLAGS_SIGNED | TYPE_FLAGS_WIDTH4: a.lower = (int32_t)a.lower; break;
        case TYPE_FLAGS_SIGNED | TYPE_FLAGS_WIDTH8: a.lower = (int64_t)a.lower; break;

        case TYPE_FLAGS_WIDTH1: a.lower = (uint8_t)a.lower; break;
        case TYPE_FLAGS_WIDTH2: a.lower = (uint16_t)a.lower; break;
        case TYPE_FLAGS_WIDTH4: a.lower = (uint32_t)a.lower; break;
        case TYPE_FLAGS_WIDTH8: a.lower = (uint64_t)a.lower; break;
        default: abort();
    }
    a.upper = (flags & TYPE_FLAGS_SIGNED) && (a.lower >> 63);
    return a;
}

Constant128 mp_from_u64(uint64_t u)
{
    Constant128 n = {.lower = u};
    return n;
}
Constant128 mp_from_i64(int64_t u)
{
    Constant128 n = {.upper = u < 0, .lower = u};
    return n;
}

void mpa_add(Constant128* a, Constant128 b)
{
    a->lower += b.lower;
    if (b.upper || a->upper)
    {
        a->upper = a->lower >> 63;
    }
}

Constant128 mp_mul(Constant128 a, Constant128 b)
{
    mp_mul_i(&a, &b);
    return a;
}

Constant128 mp_fma(Constant128 a, Constant128 b, int c)
{
    mpa_add(&a, mp_mul(b, mp_from_i64(c)));
    return a;
}
Constant128 mp_fsm(Constant128 a, Constant128 b, int c)
{
    mpa_add(&a, mp_neg(mp_mul(b, mp_from_i64(c))));
    return a;
}
Constant128 mp_sub(Constant128 a, Constant128 b)
{
    mpa_add(&a, mp_neg(b));
    return a;
}

Constant128 mp_div(Constant128 a, Constant128 b, const struct Token* tok)
{
    if (b.lower == 0)
    {
        parser_tok_error(tok, "error: divide by 0 is undefined.\n");
        return s_zero_constant;
    }
    int neg = a.upper ^ b.upper;
    if (a.upper)
    {
        mp_neg_i(&a);
    }
    else if (b.upper)
    {
        mp_neg_i(&b);
    }
    a.lower /= b.lower;
    if (neg) mp_neg_i(&a);
    return a;
}
Constant128 mp_idiv(Constant128 a, int b, const struct Token* tok) { return mp_div(a, mp_from_i64(b), tok); }
Constant128 mp_mod(Constant128 a, Constant128 b, const struct Token* tok)
{
    if (b.lower == 0)
    {
        parser_tok_error(tok, "error: remainder by 0 is undefined.\n");
        return s_zero_constant;
    }
    int neg = a.upper;
    if (a.upper)
    {
        mp_neg_i(&a);
    }
    else if (b.upper)
    {
        mp_neg_i(&b);
    }
    Constant128 c = {.lower = a.lower / b.lower * b.lower};
    c = mp_sub(a, c);
    if (neg)
    {
        mp_neg_i(&c);
    }
    return c;
}
Constant128 mp_bor(Constant128 a, Constant128 b)
{
    Constant128 ret = {.lower = a.lower | b.lower, .upper = a.upper | b.upper};
    return ret;
}
Constant128 mp_bnot(Constant128 a)
{
    Constant128 ret = {.lower = ~a.lower, .upper = !a.upper};
    return ret;
}
Constant128 mp_lnot(Constant128 a)
{
    Constant128 ret = {!a.lower && !a.upper};
    return ret;
}
Constant128 mp_band(Constant128 a, Constant128 b)
{
    Constant128 ret = {.lower = a.lower & b.lower, .upper = a.upper & b.upper};
    return ret;
}
Constant128 mp_bxor(Constant128 a, Constant128 b)
{
    Constant128 ret = {.lower = a.lower ^ b.lower, .upper = a.upper ^ b.upper};
    return ret;
}
Constant128 mp_shl(Constant128 a, Constant128 b, const struct Token* tok)
{
    if (b.upper)
    {
        parser_tok_error(tok, "error: shifting a value by <0 is undefined behavior.\n");
        return s_zero_constant;
    }
    if (b.lower >= 64)
    {
        parser_tok_error(tok, "error: shifting a value by >=64 is undefined behavior.\n");
        return s_zero_constant;
    }
    a.lower <<= b.lower;
    return a;
}
Constant128 mp_shr(Constant128 a, Constant128 b, const struct Token* tok)
{
    if (b.upper)
    {
        parser_tok_error(tok, "error: shifting a value by <0 is undefined behavior.\n");
        return s_zero_constant;
    }
    if (b.lower == 0) return a;
    if (b.lower >= 64)
    {
        parser_tok_error(tok, "error: shifting a value by >=64 is undefined behavior.\n");
        return s_zero_constant;
    }
    a.lower >>= b.lower;
    if (a.upper) a.lower |= -1LL << (64 - b.lower);
    return a;
}
Constant128 mp_neg(Constant128 a)
{
    mp_neg_i(&a);
    return a;
}