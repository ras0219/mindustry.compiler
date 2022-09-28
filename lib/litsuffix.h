#pragma once

typedef enum LitSuffix
{
    LIT_SUFFIX_NONE,
    LIT_SUFFIX_U,
    LIT_SUFFIX_L,
    LIT_SUFFIX_LU,
    LIT_SUFFIX_LL,
    LIT_SUFFIX_LLU,

    LIT_SUFFIX_NONE_DECIMAL = 8,
    LIT_SUFFIX_U_DECIMAL,
    LIT_SUFFIX_L_DECIMAL,
    LIT_SUFFIX_LU_DECIMAL,
    LIT_SUFFIX_LL_DECIMAL,
    LIT_SUFFIX_LLU_DECIMAL,

    LIT_SUFFIX_MASK_UNSIGNED = 1,
} LitSuffix;

const char* suffix_to_string(LitSuffix s);
