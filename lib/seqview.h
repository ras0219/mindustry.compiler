#pragma once

#include <stdint.h>

typedef struct SeqView
{
    size_t off;
    size_t ext;
} SeqView;

static __forceinline size_t _seqview_start(SeqView seq, size_t* p)
{
    *p = seq.ext + seq.off;
    return seq.off;
}

#define FOREACH_SEQ(i, seq) for (size_t _end##i, i = _seqview_start((seq), &_end##i); i < _end##i; ++i)
