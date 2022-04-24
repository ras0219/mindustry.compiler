#pragma once

#include "array.h"

struct TestFrame
{
    const char* file;
    const char* func;
    size_t line;
};

struct TestState
{
    int tests;
    int testfails;
    int assertions;
    int assertionfails;
    struct Array stack;
};

#define SUBTEST(expr)                                                                                                  \
    do                                                                                                                 \
    {                                                                                                                  \
        struct TestFrame* frame = array_alloc(&state->stack, sizeof(struct TestFrame));                                \
        frame->file = __FILE__;                                                                                        \
        frame->func = __func__;                                                                                        \
        frame->line = __LINE__;                                                                                        \
        int _subtest_rc = (expr);                                                                                      \
        array_pop(&state->stack, sizeof(struct TestFrame));                                                            \
        if (_subtest_rc) goto fail;                                                                                    \
    } while (0)

void unittest_print_stack(const struct TestState* state);

#define REQUIRE(expr)                                                                                                  \
    do                                                                                                                 \
    {                                                                                                                  \
        state->assertions++;                                                                                           \
        int _expr_v = !(expr);                                                                                         \
        if (_expr_v)                                                                                                   \
        {                                                                                                              \
            unittest_print_stack(state);                                                                               \
            fprintf(stderr, "%s:%d: error: '%s' was zero\n", __FILE__, __LINE__, #expr);                               \
            state->assertionfails++;                                                                                   \
            goto fail;                                                                                                 \
        }                                                                                                              \
    } while (0)

#define REQUIRE_EQ(expected, actual)                                                                                   \
    do                                                                                                                 \
    {                                                                                                                  \
        state->assertions++;                                                                                           \
        int _expr_a = (expected);                                                                                      \
        int _expr_b = (actual);                                                                                        \
        if (_expr_a != _expr_b)                                                                                        \
        {                                                                                                              \
            unittest_print_stack(state);                                                                               \
            fprintf(stderr,                                                                                            \
                    "%s:%d: error: '%s == %s' was '%d == %d'\n",                                                       \
                    __FILE__,                                                                                          \
                    __LINE__,                                                                                          \
                    #expected,                                                                                         \
                    #actual,                                                                                           \
                    _expr_a,                                                                                           \
                    _expr_b);                                                                                          \
            state->assertionfails++;                                                                                   \
            goto fail;                                                                                                 \
        }                                                                                                              \
    } while (0)

#define REQUIRE_STR_EQ(expected, actual)                                                                               \
    do                                                                                                                 \
    {                                                                                                                  \
        state->assertions++;                                                                                           \
        const char* _expr_a = (expected);                                                                              \
        const char* _expr_b = (actual);                                                                                \
        if (strcmp(_expr_a, _expr_b) != 0)                                                                             \
        {                                                                                                              \
            unittest_print_stack(state);                                                                               \
            fprintf(stderr,                                                                                            \
                    "%s:%d: error: '%s eq %s' was '\"%s\" eq \"%s\"'\n",                                               \
                    __FILE__,                                                                                          \
                    __LINE__,                                                                                          \
                    #expected,                                                                                         \
                    #actual,                                                                                           \
                    _expr_a,                                                                                           \
                    _expr_b);                                                                                          \
            state->assertionfails++;                                                                                   \
            goto fail;                                                                                                 \
        }                                                                                                              \
    } while (0)

#define REQUIREZ(expr)                                                                                                 \
    do                                                                                                                 \
    {                                                                                                                  \
        state->assertions++;                                                                                           \
        int _expr_v = (expr);                                                                                          \
        if (_expr_v)                                                                                                   \
        {                                                                                                              \
            unittest_print_stack(state);                                                                               \
            fprintf(stderr, "%s:%d: error: '%s' was nonzero (%d)\n", __FILE__, __LINE__, #expr, _expr_v);              \
            state->assertionfails++;                                                                                   \
            goto fail;                                                                                                 \
        }                                                                                                              \
    } while (0)

#define REQUIRE_NULL(expr)                                                                                             \
    do                                                                                                                 \
    {                                                                                                                  \
        state->assertions++;                                                                                           \
        const void* _expr_v = (expr);                                                                                  \
        if (_expr_v)                                                                                                   \
        {                                                                                                              \
            unittest_print_stack(state);                                                                               \
            fprintf(stderr, "%s:%d: error: '%s' was non-null\n", __FILE__, __LINE__, #expr);                           \
            state->assertionfails++;                                                                                   \
            goto fail;                                                                                                 \
        }                                                                                                              \
    } while (0)

#define RUN_TEST(test)                                                                                                 \
    do                                                                                                                 \
    {                                                                                                                  \
        state->tests++;                                                                                                \
        if (test(state)) state->testfails++;                                                                           \
    } while (0)
