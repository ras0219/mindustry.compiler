#pragma once

#include "array.h"

struct TestFrame
{
    const char* file;
    const char* func;
    size_t line;
};

typedef struct TestState
{
    const char* colorsuc;
    const char* colorerr;
    const char* colorreset;
    int tests;
    int testfails;
    int assertions;
    int assertionfails;
    struct Array stack;
    struct Array info;
} TestState;

#define STRINGIFY_HELPER(X) #X
#define STRINGIFY(X) STRINGIFY_HELPER(X)

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

#define REQUIRE_FAIL_IMPL(fmt, ...)                                                                                    \
    fprintf(stderr, "%s%s:%d: error: " fmt "\n%s", state->colorerr, __FILE__, __LINE__, __VA_ARGS__, state->colorreset)

#define REQUIRE_IMPL(expr, expr_str)                                                                                   \
    do                                                                                                                 \
    {                                                                                                                  \
        state->assertions++;                                                                                           \
        int _expr_v = !(expr);                                                                                         \
        if (_expr_v)                                                                                                   \
        {                                                                                                              \
            unittest_print_stack(state);                                                                               \
            REQUIRE_FAIL_IMPL("error: '%s' was zero", expr_str);                                                       \
            state->assertionfails++;                                                                                   \
            goto fail;                                                                                                 \
        }                                                                                                              \
    } while (0)

#define REQUIRE(expr) REQUIRE_IMPL(expr, #expr)

#define REQUIRE_EQ_IMPL(expected, expected_str, actual, actual_str)                                                    \
    do                                                                                                                 \
    {                                                                                                                  \
        state->assertions++;                                                                                           \
        int _expr_a = (expected);                                                                                      \
        int _expr_b = (actual);                                                                                        \
        if (_expr_a != _expr_b)                                                                                        \
        {                                                                                                              \
            unittest_print_stack(state);                                                                               \
            REQUIRE_FAIL_IMPL("error: '%s == %s' was '%d == %d'", expected_str, actual_str, _expr_a, _expr_b);         \
            state->assertionfails++;                                                                                   \
            goto fail;                                                                                                 \
        }                                                                                                              \
    } while (0)

#define REQUIRE_EQ(expected, actual) REQUIRE_EQ_IMPL(expected, #expected, actual, #actual)

#define REQUIRE_ZU_EQ_IMPL(expected, expected_str, actual, actual_str)                                                 \
    do                                                                                                                 \
    {                                                                                                                  \
        state->assertions++;                                                                                           \
        size_t _expr_a = (expected);                                                                                   \
        size_t _expr_b = (actual);                                                                                     \
        if (_expr_a != _expr_b)                                                                                        \
        {                                                                                                              \
            unittest_print_stack(state);                                                                               \
            REQUIRE_FAIL_IMPL("error: '%s == %s' was '%zu == %zu'", expected_str, actual_str, _expr_a, _expr_b);       \
            state->assertionfails++;                                                                                   \
            goto fail;                                                                                                 \
        }                                                                                                              \
    } while (0)

#define REQUIRE_ZU_EQ(expected, actual) REQUIRE_ZU_EQ_IMPL(expected, #expected, actual, #actual)

#define REQUIRE_PTR_EQ(expected, actual)                                                                               \
    do                                                                                                                 \
    {                                                                                                                  \
        state->assertions++;                                                                                           \
        const void* _expr_a = (expected);                                                                              \
        const void* _expr_b = (actual);                                                                                \
        if (_expr_a != _expr_b)                                                                                        \
        {                                                                                                              \
            unittest_print_stack(state);                                                                               \
            fprintf(stderr, "%s:%d: error: '%s == %s' was false\n", __FILE__, __LINE__, #expected, #actual);           \
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

// https://www.chiark.greenend.org.uk/~sgtatham/mp/
#define REQUIRE_AST_IMPL(_type, _var, _expr, _expr_type, _expr_str, N)                                                 \
    if (1)                                                                                                             \
    {                                                                                                                  \
        goto _with_ast_start##N;                                                                                       \
    }                                                                                                                  \
    else                                                                                                               \
        for (struct _type * _var;;)                                                                                    \
            if (1)                                                                                                     \
            {                                                                                                          \
                break;                                                                                                 \
            }                                                                                                          \
            else if (1)                                                                                                \
            {                                                                                                          \
                _with_ast_start##N:;                                                                                   \
                struct _expr_type* _with_ast_e##N = (_expr);                                                           \
                REQUIRE_IMPL(_with_ast_e##N, _expr_str);                                                               \
                REQUIRE_EQ_IMPL(                                                                                       \
                    AST_KIND_##_type, STRINGIFY(AST_KIND_##_type), _with_ast_e##N->kind, "(" _expr_str ")->kind");     \
                _var = (struct _type*)_with_ast_e##N;                                                                  \
                goto _with_ast_body##N;                                                                                \
            }                                                                                                          \
            else                                                                                                       \
                _with_ast_body##N:

#define INFO_IMPL(N, _fmt, ...)                                                                                        \
    if (1)                                                                                                             \
    {                                                                                                                  \
        goto _info_start##N;                                                                                           \
    }                                                                                                                  \
    else                                                                                                               \
        for (size_t _info_pos;;)                                                                                       \
            if (1)                                                                                                     \
            {                                                                                                          \
                state->info.sz = _info_pos;                                                                            \
                break;                                                                                                 \
            }                                                                                                          \
            else if (1)                                                                                                \
            {                                                                                                          \
                _info_start##N:;                                                                                       \
                _info_pos = state->info.sz;                                                                            \
                array_appendf(&state->info, "%s:%d: info: ", __FILE__, __LINE__);                                      \
                array_appendf(&state->info, _fmt, __VA_ARGS__);                                                        \
                goto _info_body##N;                                                                                    \
            }                                                                                                          \
            else                                                                                                       \
                _info_body##N:

#define INFO2(N, fmt, ...) INFO_IMPL(N, fmt, __VA_ARGS__)
#define INFO(fmt, ...) INFO2(__COUNTER__, fmt, __VA_ARGS__)

#define REQUIRE_AST_IMPL1(_type, _var, _expr, _expr_type, _expr_str, N)                                                \
    REQUIRE_AST_IMPL(_type, _var, _expr, _expr_type, _expr_str, N)

#define REQUIRE_AST(_type, _var, _expr) REQUIRE_AST_IMPL1(_type, _var, _expr, Ast, #_expr, __COUNTER__)

#define REQUIRE_EXPR(_type, _var, _expr) REQUIRE_AST_IMPL1(_type, _var, _expr, Expr, #_expr, __COUNTER__)
