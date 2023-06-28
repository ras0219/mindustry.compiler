#pragma once

#include <stdlib.h>

enum
{
    diff_left,
    diff_right,
    diff_both,
};

/// `in` must be of size n1 * n2 and `in[x][y]` should be the equality of A[x] and B[y]
/// `out` must be of size (n1 + n2)
void diff(const unsigned char* in, unsigned char* out, size_t n1, size_t n2);
