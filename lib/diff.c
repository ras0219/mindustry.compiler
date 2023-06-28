#include "diff.h"

#include <stdint.h>

void diff(const unsigned char* in, unsigned char* out, size_t n1, size_t n2)
{
    const size_t rows = n1 + 1;
    const size_t cols = n2 + 1;

    uint64_t* diff = malloc(rows * cols * sizeof(size_t));

    for (size_t i2 = 0; i2 < cols; ++i2)
    {
        diff[i2] = i2;
    }
    for (size_t i1 = 1; i1 < rows; ++i1)
    {
        const size_t offset = i1 * cols;
        const size_t prev_offset = offset - cols;
        diff[offset] = i1;
        for (size_t i2 = 1; i2 < cols; ++i2)
        {
            int t = diff[prev_offset + i2] + 1, u = diff[offset + i2 - 1] + 1, v = diff[prev_offset + i2 - 1];
            t = t < u ? t : u;

            if (v < t && in[(i1 - 1) * n2 + i2 - 1])
            {
                t = v;
            }
            diff[offset + i2] = t;
        }
    }
    uint8_t* out_ptr = out + n1 + n2;
    size_t i1 = n1, i2 = n2;
    while (i1 > 0 && i2 > 0)
    {
        if (diff[i1 * cols + i2] == diff[(i1 - 1) * cols + i2 - 1] && in[(i1 - 1) * n2 + i2 - 1])
        {
            *--out_ptr = diff_both;
            *--out_ptr = diff_both;
            --i1;
            --i2;
        }
        else if (diff[i1 * cols + i2] == diff[(i1 - 1) * cols + i2] + 1)
        {
            *--out_ptr = diff_left;
            --i1;
        }
        else
        {
            *--out_ptr = diff_right;
            --i2;
        }
    }
    while (i1 > 0)
    {
        *--out_ptr = diff_left;
        --i1;
    }
    while (i2 > 0)
    {
        *--out_ptr = diff_right;
        --i2;
    }
    if (out_ptr != out) abort();
    free(diff);
}
