#pragma once

#include "array.h"

typedef struct LinesStream
{
    int row;
    // length of line
    size_t len;
    // start of next line
    size_t nstart;
    // data for line, null terminated
    Array buf;
    // file pointer
    void* f;
} LinesStream;

/// @returns nonzero if received next line
int lstream_next(LinesStream* s);
