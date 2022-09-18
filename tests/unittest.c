#include "unittest.h"

#include <stdio.h>

#include "array.h"
#include "string.h"

const char* g_datadir = NULL;
size_t g_datadir_sz = 0;

void unittest_print_stack(const struct TestState* state)
{
    struct TestFrame* frames = (struct TestFrame*)state->stack.data;
    for (size_t i = 0; i < state->stack.sz / sizeof(struct TestFrame); ++i)
    {
        fprintf(stderr, "%s:%zu: from %s\n", frames[i].file, frames[i].line, frames[i].func);
    }
    fprintf(stderr, "%.*s", (int)state->info.sz, (char*)state->info.data);
}

int unittest_require_eq_impl(
    struct TestState* state, const char* file, int line, const char* expected_str, const char* actual_str, int a, int b)
{
    ++state->assertions;

    if (a != b)
    {
        REQUIRE_FAIL_IMPL(file, line, "'%s == %s' was '%d == %d'", expected_str, actual_str, a, b);
    }

    return 0;
fail:
    return 1;
}

int unittest_require_zu_eq_impl(struct TestState* state,
                                const char* file,
                                int line,
                                const char* expected_str,
                                const char* actual_str,
                                size_t a,
                                size_t b)
{
    ++state->assertions;

    if (a != b)
    {
        REQUIRE_FAIL_IMPL(file, line, "'%s == %s' was '%zu == %zu'", expected_str, actual_str, a, b);
    }

    return 0;
fail:
    return 1;
}

int unittest_require_mem_eq_impl(struct TestState* state,
                                 const char* file,
                                 int line,
                                 const char* expected_str,
                                 const char* expected,
                                 int expected_len,
                                 const char* actual_str,
                                 const char* actual,
                                 int actual_len)
{
    state->assertions++;
    if (expected_len != actual_len || memcmp(expected, actual, expected_len) != 0)
        REQUIRE_FAIL_IMPL(file,
                          line,
                          "'%.*s' != '%.*s'\n"
                          "    expected:      \"%.*s\"(%d)\n"
                          "    actual:        \"%.*s\"(%d)\n"
                          "    expected-expr: %s\n"
                          "    actual-expr:   %s\n",
                          expected_len,
                          expected,
                          actual_len,
                          actual,
                          expected_len,
                          expected,
                          expected_len,
                          actual_len,
                          actual,
                          actual_len,
                          expected_str,
                          actual_str);
    return 0;
fail:
    return 1;
}