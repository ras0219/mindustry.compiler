#include "unittest.h"

#include <stdio.h>

#include "array.h"

void unittest_print_stack(const struct TestState* state)
{
    struct TestFrame* frames = (struct TestFrame*)state->stack.data;
    for (size_t i = 0; i < state->stack.sz / sizeof(struct TestFrame); ++i)
    {
        fprintf(stderr, "%s:%zu: from %s\n", frames[i].file, frames[i].line, frames[i].func);
    }
}