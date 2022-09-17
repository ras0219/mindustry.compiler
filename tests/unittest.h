#pragma once

#include "array.h"

extern const char* g_datadir;
extern size_t g_datadir_sz;

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

#define REQUIRE_FAIL_MSG_IMPL(file, line, fmt, ...)                                                                    \
    fprintf(stderr, "%s%s:%d: error: " fmt "%s\n", state->colorerr, file, line, ##__VA_ARGS__, state->colorreset);

#define REQUIRE_FAIL_IMPL(file, line, fmt, ...)                                                                        \
    do                                                                                                                 \
    {                                                                                                                  \
        unittest_print_stack(state);                                                                                   \
        REQUIRE_FAIL_MSG_IMPL(file, line, fmt, ##__VA_ARGS__);                                                         \
        state->assertionfails++;                                                                                       \
        goto fail;                                                                                                     \
    } while (0)

#define REQUIRE_FAIL(fmt, ...) REQUIRE_FAIL_IMPL(__FILE__, __LINE__, fmt, ##__VA_ARGS__)

#define REQUIRE_IMPL(expr, expr_str)                                                                                   \
    do                                                                                                                 \
    {                                                                                                                  \
        state->assertions++;                                                                                           \
        int _expr_v = !(expr);                                                                                         \
        if (_expr_v) REQUIRE_FAIL("'%s' was zero", expr_str);                                                          \
    } while (0)

#define REQUIRE(expr) REQUIRE_IMPL(expr, #expr)

int unittest_require_eq_impl(struct TestState* state,
                             const char* file,
                             int line,
                             const char* expected_str,
                             const char* actual_str,
                             int a,
                             int b);

#define REQUIRE_EQ_IMPL(file, line, expected, expected_str, actual, actual_str)                                        \
    do                                                                                                                 \
    {                                                                                                                  \
        if (unittest_require_eq_impl(state, file, line, expected_str, actual_str, (expected), (actual))) goto fail;    \
    } while (0)

#define REQUIRE_EQ(expected, actual) REQUIRE_EQ_IMPL(__FILE__, __LINE__, expected, #expected, actual, #actual)

int unittest_require_zu_eq_impl(struct TestState* state,
                                const char* file,
                                int line,
                                const char* expected_str,
                                const char* actual_str,
                                size_t a,
                                size_t b);

#define REQUIRE_ZU_EQ_IMPL(file, line, expected, expected_str, actual, actual_str)                                     \
    do                                                                                                                 \
    {                                                                                                                  \
        if (unittest_require_zu_eq_impl(state, file, line, expected_str, actual_str, (expected), (actual))) goto fail; \
    } while (0)

#define REQUIRE_ZU_EQ(expected, actual) REQUIRE_ZU_EQ_IMPL(__FILE__, __LINE__, expected, #expected, actual, #actual)

#define REQUIRE_PTR_EQ(expected, actual)                                                                               \
    do                                                                                                                 \
    {                                                                                                                  \
        state->assertions++;                                                                                           \
        const void* _expr_a = (expected);                                                                              \
        const void* _expr_b = (actual);                                                                                \
        if (_expr_a != _expr_b) REQUIRE_FAIL("'%s == %s' was false", #expected, #actual);                              \
    } while (0)

#define REQUIRE_STR_EQ(expected, actual)                                                                               \
    do                                                                                                                 \
    {                                                                                                                  \
        state->assertions++;                                                                                           \
        const char* _expr_a = (expected);                                                                              \
        const char* _expr_b = (actual);                                                                                \
        if (strcmp(_expr_a, _expr_b) != 0)                                                                             \
            REQUIRE_FAIL("'%s eq %s' was '\"%s\" eq \"%s\"'", #expected, #actual, _expr_a, _expr_b);                   \
    } while (0)

#define REQUIRE_MEM_EQ_IMPL(file, line, expected_str, expected, expected_len, actual_str, actual, actual_len)          \
    do                                                                                                                 \
    {                                                                                                                  \
        state->assertions++;                                                                                           \
        const char* _expr_a = (expected);                                                                              \
        const char* _expr_b = (actual);                                                                                \
        int _len_a = (int)(expected_len);                                                                              \
        int _len_b = (int)(actual_len);                                                                                \
        if (_len_a != _len_b || memcmp(_expr_a, _expr_b, _len_a) != 0)                                                 \
            REQUIRE_FAIL_IMPL(file,                                                                                    \
                              line,                                                                                    \
                              "'%.*s' != '%.*s'\n"                                                                     \
                              "    expected:      \"%.*s\"(%d)\n"                                                      \
                              "    actual:        \"%.*s\"(%d)\n"                                                      \
                              "    expected-expr: %s\n"                                                                \
                              "    actual-expr:   %s\n",                                                               \
                              _len_a,                                                                                  \
                              _expr_a,                                                                                 \
                              _len_b,                                                                                  \
                              _expr_b,                                                                                 \
                              _len_a,                                                                                  \
                              _expr_a,                                                                                 \
                              _len_a,                                                                                  \
                              _len_b,                                                                                  \
                              _expr_b,                                                                                 \
                              _len_b,                                                                                  \
                              expected_str,                                                                            \
                              actual_str);                                                                             \
    } while (0)

#define REQUIRE_MEM_EQ(expected, expected_len, actual, actual_len)                                                     \
    REQUIRE_MEM_EQ_IMPL(__FILE__, __LINE__, #expected, expected, expected_len, #actual, actual, actual_len)

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
                REQUIRE_EQ_IMPL(__FILE__,                                                                              \
                                __LINE__,                                                                              \
                                AST_KIND_##_type,                                                                      \
                                STRINGIFY(AST_KIND_##_type),                                                           \
                                _with_ast_e##N->kind,                                                                  \
                                "(" _expr_str ")->kind");                                                              \
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

#define REQUIRE_LINES_IMPL(_expr, _expr_str, N)                                                                        \
    if (1)                                                                                                             \
    {                                                                                                                  \
        goto _require_lines##N;                                                                                        \
    }                                                                                                                  \
    else                                                                                                               \
        for (const char* _require_lines;;)                                                                             \
            if (1)                                                                                                     \
            {                                                                                                          \
                REQUIRE_STR_EQ("", _require_lines);                                                                    \
                break;                                                                                                 \
            }                                                                                                          \
            else if (1)                                                                                                \
            {                                                                                                          \
                _require_lines##N:;                                                                                    \
                _require_lines = (_expr);                                                                              \
                REQUIRE_IMPL(_require_lines, _expr_str);                                                               \
                goto _require_lines_body##N;                                                                           \
            }                                                                                                          \
            else                                                                                                       \
                _require_lines_body##N:

#define REQUIRE_LINES(_expr) REQUIRE_LINES_IMPL(_expr, #_expr, __COUNTER__)

#define REQUIRE_LINE(expected)                                                                                         \
    do                                                                                                                 \
    {                                                                                                                  \
        const char* _require_line = (expected);                                                                        \
        size_t _require_line_s = strlen(_require_line);                                                                \
        const char* _require_line_e = strchr(_require_lines, '\n');                                                    \
        if (_require_line_e)                                                                                           \
        {                                                                                                              \
            REQUIRE_MEM_EQ(_require_line, _require_line_s, _require_lines, _require_line_e - _require_lines);          \
            _require_lines = _require_line_e + 1;                                                                      \
        }                                                                                                              \
        else                                                                                                           \
        {                                                                                                              \
            REQUIRE_STR_EQ(_require_line, _require_lines);                                                             \
            _require_lines = "";                                                                                       \
        }                                                                                                              \
    } while (0)
