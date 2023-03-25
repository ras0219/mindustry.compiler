#pragma once

#define PARSER_FAIL_RC(RC, ...)                                                                                        \
    do                                                                                                                 \
    {                                                                                                                  \
        parser_ferror(RC, __VA_ARGS__);                                                                                \
        cur_tok = NULL;                                                                                                \
        goto fail;                                                                                                     \
    } while (0)

#define PARSER_FAIL_TOK(TOK, ...)                                                                                      \
    do                                                                                                                 \
    {                                                                                                                  \
        parser_tok_error(TOK, __VA_ARGS__);                                                                            \
        cur_tok = NULL;                                                                                                \
        goto fail;                                                                                                     \
    } while (0)

#define PARSER_FAIL(...) PARSER_FAIL_TOK(cur_tok, __VA_ARGS__)

#define PARSER_DO(X)                                                                                                   \
    do                                                                                                                 \
    {                                                                                                                  \
        cur_tok = (X);                                                                                                 \
        if (cur_tok == NULL)                                                                                           \
        {                                                                                                              \
            goto fail;                                                                                                 \
        }                                                                                                              \
    } while (0)

#define PARSER_DO_WITH(X, ...)                                                                                         \
    do                                                                                                                 \
    {                                                                                                                  \
        cur_tok = (X);                                                                                                 \
        if (cur_tok == NULL)                                                                                           \
        {                                                                                                              \
            parser_tok_error(cur_tok, __VA_ARGS__);                                                                    \
            goto fail;                                                                                                 \
        }                                                                                                              \
    } while (0)

#define PARSER_CHECK_NOT(X)                                                                                            \
    do                                                                                                                 \
    {                                                                                                                  \
        if (X)                                                                                                         \
        {                                                                                                              \
            cur_tok = NULL;                                                                                            \
            goto fail;                                                                                                 \
        }                                                                                                              \
    } while (0)

#define PARSER_CHECK(X) PARSER_CHECK_NOT(!(X))
