#pragma once

#include <stdint.h>

typedef struct Sizing
{
    uint32_t is_signed : 1, width : 31;
} Sizing;