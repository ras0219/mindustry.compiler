#pragma once

#include <stdlib.h>

struct Array;

typedef struct SplitLineInfo
{
    size_t start;
    size_t end;
    int row;
} SplitLineInfo;

void split_lines(const char* s, size_t n, struct Array* buf);
void remove_empty_lines(struct Array* buf);
