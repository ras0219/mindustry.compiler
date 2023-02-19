#pragma once

#include "compilermacros.h"
#include "fwd.h"
#include "mp.h"

typedef struct Constant
{
    struct Symbol* sym;
    Constant128 value;
    unsigned char is_const : 1;
    unsigned char is_lvalue : 1;
} Constant;

static const Constant s_not_constant = {0};
static const Constant s_one_constant = {.is_const = 1, .value.lower = 1};
static const Constant s_zero_constant = {.is_const = 1};

void constant_load_lvalue(Constant* c);
void constant_addressof(Constant* c);

__forceinline Constant constant_sym_lvalue(struct Symbol* s)
{
    const Constant c = {
        .sym = s,
        .is_const = 1,
        .is_lvalue = 1,
    };
    return c;
}

__forceinline Constant constant_i64(int64_t i)
{
    const Constant c = {
        .is_const = 1,
        .value = mp_from_i64(i),
    };
    return c;
}

__forceinline Constant constant_bool(int i)
{
    Constant ret = {.is_const = 1, .value.lower = !!i};
    return ret;
}
