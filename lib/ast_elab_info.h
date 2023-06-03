#pragma once

#include "constant.h"
#include "sizing.h"

typedef struct AstElabInfo
{
    unsigned char take_address;
    Sizing sizing;
    struct Constant c;
} AstElabInfo;
