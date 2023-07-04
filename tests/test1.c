#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "ast.h"
#include "be.h"
#include "cg.h"
#include "checker.h"
#include "diff.h"
#include "dirent.h"
#include "elaborator.h"
#include "errors.h"
#include "json_dom.h"
#include "lexstate.h"
#include "parse.h"
#include "parse_macros.h"
#include "path.h"
#include "preproc.h"
#include "stdlibe.h"
#include "stream.h"
#include "strlist.h"
#include "symbol.h"
#include "tac.h"
#include "token.h"
#include "unittest.h"
#include "unwrap.h"

#define REQUIRE_SIZING_EQ(expected, actual)                                                                            \
    do                                                                                                                 \
    {                                                                                                                  \
        const Sizing _require_s_e = (expected);                                                                        \
        const Sizing _require_s_a = (actual);                                                                          \
        REQUIRE_EQ_IMPL(                                                                                               \
            __FILE__, __LINE__, _require_s_e.width, #expected ".width", _require_s_a.width, #actual ".width");         \
        REQUIRE_EQ_IMPL(__FILE__,                                                                                      \
                        __LINE__,                                                                                      \
                        _require_s_e.is_signed,                                                                        \
                        #expected ".is_signed",                                                                        \
                        _require_s_a.is_signed,                                                                        \
                        #actual ".is_signed");                                                                         \
    } while (0)

static const Sizing s_sizing_schar = {.width = 1, .is_signed = 1};
static const Sizing s_sizing_uchar = {.width = 1};

typedef struct StandardTest
{
    struct Parser* parser;
    struct Preprocessor* pp;
    struct Elaborator* elab;
    struct BackEnd* be;
    struct CodeGen* cg;
} StandardTest;

static void stdtest_destroy(StandardTest* test)
{
    if (parser_has_errors()) parser_print_msgs(stderr);
    if (test->elab)
    {
        elaborator_destroy(test->elab);
        my_free(test->elab);
    }
    if (test->parser) parser_destroy(test->parser), my_free(test->parser);
    if (test->pp) preproc_free(test->pp);
    if (test->be) be_destroy(test->be), my_free(test->be);
    if (test->cg) cg_destroy(test->cg), my_free(test->cg);
}

static void format_sizing(Array* buf, Sizing sz) { array_appendf(buf, "%c%u", sz.is_signed ? 'i' : 'u', sz.width); }

static void format_taca(Array* buf, TACAddress addr)
{
    if (addr.kind == TACA_VOID)
    {
        array_appends(buf, "TACA_VOID");
        return;
    }
    array_appendf(buf, "%s", taca_to_string(addr.kind));
    if (addr.sizing.width != 0)
    {
        array_push_byte(buf, ' ');
        format_sizing(buf, addr.sizing);
    }
    if (addr.is_addr) array_appends(buf, " is_addr");
    if (addr.offset) array_appendf(buf, " %zu", addr.offset);
    switch (addr.kind)
    {
        case TACA_REG: array_appendf(buf, " %s", register_to_string(addr.reg)); break;
        case TACA_FRAME: break;
        case TACA_PARAM: break;
        case TACA_ARG: break;
        case TACA_REF: array_appendf(buf, " %zu", addr.ref); break;
        case TACA_IMM: array_appendf(buf, " %zu", addr.imm); break;
        case TACA_NAME: array_appendf(buf, " %s", addr.name); break;
        case TACA_LNAME: array_appendf(buf, " %s", addr.name); break;
        case TACA_ALABEL: array_appendf(buf, " %zu", addr.alabel); break;
        case TACA_CONST: array_appendf(buf, " %zu", addr.const_idx); break;
        default: array_appends(buf, " unknown"); break;
    }
}

static void format_all_tac(Array* out, const BackEnd* be)
{
    const TACEntry* e = be->code.data;
    const size_t n = array_size(&be->code, sizeof(struct TACEntry));

    for (size_t i = 0; i < n; ++i)
    {
        if (e[i].op == TACO_ASSIGN)
            array_appendf(out, "TACO_ASSIGN %u\n  ", e[i].assign_width);
        else
            array_appendf(out, "%s\n  ", taco_to_string(e[i].op));
        format_taca(out, e[i].arg1);
        array_appends(out, "\n  ");
        format_taca(out, e[i].arg2);
        array_push_byte(out, '\n');
    }
}

static int read_contents(Array* out, const char* path)
{
    errno = 0;
    FILE* f = fopen(path, "rb");
    if (!f) return errno;
    (void)fseek(f, 0L, SEEK_END);
    off_t len = ftello(f);
    if (errno) goto fail;
    (void)fseek(f, 0L, SEEK_SET);
    array_reserve(out, out->sz + len);
    out->sz += fread(out->data + out->sz, 1, len, f);
fail:
    fclose(f);
    return errno;
}

static int is_trimchar(char ch) { return ch == '\n' || ch == '\r'; }

// `out` : Array<size_t>
static void trimmed_lines(const char* buf, size_t buf_sz, Array* out)
{
    size_t s = 0;
    for (;;)
    {
        while (s < buf_sz && is_trimchar(buf[s]))
            ++s;
        if (s == buf_sz) return;
        size_t j = s + 1;
        while (j < buf_sz && !is_trimchar(buf[j]))
            ++j;
        arrsz_push(out, s);
        arrsz_push(out, j);
        if (j == buf_sz) return;
        s = j + 1;
    }
}

int require_lines_eq(
    struct TestState* state, const char* ebuf1, size_t ebuf1sz, const char* ebuf2, size_t ebuf2sz, const char* filename)
{
    int rc = 1;
    unsigned char* cmp = NULL;
    // Array<size_t>
    Array lines1 = {0}, lines2 = {0};

    trimmed_lines(ebuf1, ebuf1sz, &lines1);
    trimmed_lines(ebuf2, ebuf2sz, &lines2);

    size_t n1 = arrsz_size(&lines1) / 2, n2 = arrsz_size(&lines2) / 2;

    const size_t* l1 = lines1.data;
    const size_t* l2 = lines2.data;

    cmp = malloc(n1 * n2 + n1 + n2);
    unsigned char* diff_seq = cmp + n1 * n2;
    for (size_t i = 0; i < n1; ++i)
    {
        size_t o = i * n2;
        for (size_t j = 0; j < n2; ++j)
        {
            size_t len_i = l1[i * 2 + 1] - l1[i * 2], len_j = l2[j * 2 + 1] - l2[j * 2];
            cmp[o + j] = (len_i == len_j && 0 == memcmp(ebuf1 + l1[i * 2], ebuf2 + l2[j * 2], len_i));
        }
    }

    diff(cmp, diff_seq, n1, n2);

    size_t hunks = 0;

    size_t i = 0, j = 0;
    for (; i + j < n1 + n2;)
    {
        const size_t s = i + j;
        if (diff_seq[s] == diff_both)
        {
            ++i;
            ++j;
            continue;
        }
        const size_t context = i > 3 ? 3 : i;
        size_t seen_both = 0;
        size_t hunk_end = s + 1;
        for (; seen_both < 7 && hunk_end < n1 + n2; ++hunk_end)
        {
            if (diff_seq[hunk_end] == diff_both)
            {
                ++hunk_end;
                ++seen_both;
            }
            else
            {
                seen_both = 0;
            }
        }
        if (seen_both > 3)
        {
            hunk_end -= (seen_both - 3) * 2;
        }
        REQUIRE_FAIL_MSG_IMPL(filename, (int)(i + 1), "incorrect output");
        ++state->assertionfails;
        ++hunks;
        fprintf(stderr, "@@ %zu %zu @@\n", i + 1 - context, j + 1 - context);
        for (size_t t = 0; t < context; ++t)
        {
            size_t l = i + t - context;
            fprintf(stderr, " %.*s\n", (int)(l1[l * 2 + 1] - l1[l * 2]), ebuf1 + l1[l * 2]);
        }
        for (; i + j < hunk_end;)
        {
            const int c = diff_seq[i + j];
            if (c == diff_both)
            {
                fprintf(stderr, " %.*s\n", (int)(l1[i * 2 + 1] - l1[i * 2]), ebuf1 + l1[i * 2]);
                ++i;
                ++j;
            }
            else if (c == diff_left)
            {
                fprintf(stderr, "-%.*s\n", (int)(l1[i * 2 + 1] - l1[i * 2]), ebuf1 + l1[i * 2]);
                ++i;
            }
            else
            {
                fprintf(stderr, "+%.*s\n", (int)(l2[j * 2 + 1] - l2[j * 2]), ebuf2 + l2[j * 2]);
                ++j;
            }
        }
    }
    rc = hunks > 0;
    free(cmp);
    array_destroy(&lines1);
    array_destroy(&lines2);
    return rc;
}

static int test_tac(struct TestState* state, BackEnd* be, const char* base)
{
    int rc = 0;
    Array lines = {0};
    Array path = {0}, buf = {0}, buf2 = {0};
    array_appends(&path, base);
    array_push(&path, ".tac", 5);
    if (read_contents(&buf2, path.data) != ENOENT)
    {
        format_all_tac(&buf, be);

        if (rc = require_lines_eq(state, buf2.data, buf2.sz, buf.data, buf.sz, path.data)) goto fail;
        array_clear(&buf);
    }

    array_pop(&path, 5);
    array_push(&path, ".s", 3);
    array_clear(&buf2);
    if (read_contents(&buf2, path.data) != ENOENT)
    {
        array_clear(&lines);
        trimmed_lines(be->cg->code.data, be->cg->code.sz, &lines);
        const size_t n = arrsz_size(&lines);
        const size_t* l = lines.data;
        for (size_t i = 0; i < n; i += 2)
        {
            size_t len = l[i + 1] - l[i];
            if (!len) continue;
            const char* line = (const char*)be->cg->code.data + l[i];
            size_t s = 0;
            while (s < len && line[s] == ' ')
                ++s;
            if (s < len && (line[s] == '.' || line[s] == '#')) continue;
            array_push(&buf, line, len);
            array_push_byte(&buf, '\n');
        }
        if (rc = require_lines_eq(state, buf2.data, buf2.sz, buf.data, buf.sz, path.data)) goto fail;
        array_clear(&buf);
    }

    array_pop(&path, 1);
    array_push(&path, "data", 5);
    array_clear(&buf2);
    if (read_contents(&buf2, path.data) != ENOENT)
    {
        array_clear(&lines);
        trimmed_lines(be->cg->data.data, be->cg->data.sz, &lines);
        const size_t n = arrsz_size(&lines);
        const size_t* l = lines.data;
        for (size_t i = 0; i < n; i += 2)
        {
            size_t len = l[i + 1] - l[i];
            if (!len) continue;
            const char* line = (const char*)be->cg->data.data + l[i];
            size_t s = 0;
            while (s < len && line[s] == ' ')
                ++s;
            if (s < len && line[s] == '#') continue;
            array_push(&buf, line, len);
            array_push_byte(&buf, '\n');
        }
        if (rc = require_lines_eq(state, buf2.data, buf2.sz, buf.data, buf.sz, path.data)) goto fail;
        array_clear(&buf);
    }
fail:
    array_destroy(&buf2);
    array_destroy(&buf);
    array_destroy(&path);
    array_destroy(&lines);
    return rc;
}

static void array_append_jsonr(Array* out, const JsonRecord* r, const char* text)
{
    if (r->kind == jsonr_key || r->kind == jsonr_string) array_push_byte(out, '"');
    array_push(out, text + r->offset, r->n);
    if (r->kind == jsonr_string)
        array_push_byte(out, '"');
    else if (r->kind == jsonr_key)
        array_push(out, "\":", 2);
}

static int test_file(struct TestState* state, const char* path)
{
    int rc = 1;
    JsonDOM dom = {0};
    Array astfile = {0}, buf = {0};
    Preprocessor *pp = NULL, *ast_pp = NULL;
    Checker* chk = NULL;
    Parser parser = {0};
    Elaborator elab = {0};
    BackEnd be = {0};
    CodeGen cg = {0};
    FILE* f = fopen(path, "r");
    FILE *f2 = NULL, *f3 = NULL;
    if (!f) REQUIRE_FAIL("failed to open test file %s", path);

    struct
    {
        char data[128];
    } static_incpath_data;
    // hack, to fix
    Array fake_array = {
        .cap = sizeof(static_incpath_data),
        .sz = 0,
        .data = &static_incpath_data,
    };
    assign_path_join(&buf, g_datadir, g_datadir_sz, "tests/pass/inc", sizeof("tests/pass/inc") - 1);
    strlist_append(&fake_array, buf.data, buf.sz - 1);
    array_clear(&buf);
    parser_clear_errors();
    pp = preproc_alloc();
    preproc_include_paths(pp, &fake_array);
    if (preproc_file(pp, f, path))
    {
        REQUIRE_FAIL_IMPL(path, 1, "%s", "failed to preprocess");
    }
    REQUIREZ(parser_parse(&parser, preproc_tokens(pp), preproc_stringpool(pp)));
    parser_debug_check(&parser);
    if (parser_has_errors())
    {
        REQUIRE_FAIL_IMPL(path, 1, "%s", "failed to parse");
    }

    elaborator_init(&elab, &parser);
    if (elaborate(&elab))
    {
        REQUIRE_FAIL_IMPL(path, 1, "%s", "failed to elaborate");
    }

    array_appends(&astfile, path);
    array_push(&astfile, ".ast.json", 10);
    if (!read_contents(&dom.text, astfile.data))
    {
        JsonParse jp = {0};
        JsonDOM actual = {0};
        ast_to_json(&parser, &actual);
        enum JsonParseResult pr = jsondom_fill_records(&dom, &jp);
        if (pr != json_err_success)
        {
            PRINTF_ERR(
                "%s:%d:%d: json parse error %d: %s", (char*)astfile.data, jp.row, jp.col, pr, json_err_to_string[pr]);
            goto fail;
        }
        size_t difflen = (dom.records.sz + actual.records.sz) / sizeof(JsonRecord);
        uint8_t* diff_result = malloc(difflen);
        jsondom_diff(&actual, &dom, diff_result);
        const JsonRecord* dom_records = dom.records.data;
        const JsonRecord* actual_records = actual.records.data;
        int printing = 0;
        size_t it_a = 0, it_d = 0;
        Array line = {0};
        for (size_t i = 0; i < difflen; ++i)
        {
            if (diff_result[i] == jsond_both)
            {
                if (printing)
                {
                    array_push(&line, "     ", 5);
                    array_append_jsonr(&line, actual_records + it_a, actual.text.data);
                    array_push_byte(&line, '\n');
                    --printing;
                }
                ++i;
                ++it_a;
                ++it_d;
            }
            else
            {
                if (!printing)
                {
                    // print previous context
                    for (size_t x = it_a > 3 ? it_a - 3 : 0; x < it_a; ++x)
                    {
                        array_push(&line, "     ", 5);
                        array_append_jsonr(&line, actual_records + x, actual.text.data);
                        array_push_byte(&line, '\n');
                    }
                }
                printing = 3;
                if (diff_result[i] == jsond_left)
                {
                    array_push(&line, "-    ", 5);
                    array_append_jsonr(&line, actual_records + it_a, actual.text.data);
                    array_push_byte(&line, '\n');
                    PRINTF_ERR("%s:%d:%d: error: unexpected json elem '%.*s'",
                               (char*)astfile.data,
                               dom_records[it_d].row,
                               dom_records[it_d].col,
                               (int)actual_records[it_a].n,
                               (char*)actual.text.data + (int)actual_records[it_a].offset);
                    ++it_a;
                }
                else if (diff_result[i] == jsond_right)
                {
                    array_push(&line, "+    ", 5);
                    array_append_jsonr(&line, dom_records + it_d, dom.text.data);
                    array_push_byte(&line, '\n');
                    PRINTF_ERR("%s:%d:%d: error: expected json elem '%.*s'",
                               (char*)astfile.data,
                               dom_records[it_d].row,
                               dom_records[it_d].col,
                               (int)dom_records[it_d].n,
                               (char*)dom.text.data + dom_records[it_d].offset);
                    ++it_d;
                }
                else
                {
                    abort();
                }
            }
        }
        fwrite(line.data, line.sz, 1, stderr);
        array_destroy(&line);
        jsondom_destroy(&actual);
        free(diff_result);
        fclose(f2);
        f2 = NULL;
    }
    array_pop(&astfile, 10);

    array_push(&astfile, ".checks", 8);
    if (f3 = fopen(astfile.data, "rb"))
    {
        chk = checker_alloc(&elab);
        if (checker_check(chk))
        {
            REQUIRE_FAIL_IMPL(path, 1, "%s", "failed to check");
        }
    }

    cg_init(&cg);
    cg.target = CG_TARGET_MACOS_GAS;
    be_init(&be, &parser, &elab, &cg);
    if (be_compile(&be))
    {
        REQUIRE_FAIL_IMPL(path, 1, "%s", "failed to compile");
    }

    if (test_tac(state, &be, path)) goto fail;

    rc = 0;
fail:
    if (parser_has_warnings() || parser_has_errors()) parser_print_msgs(stderr), parser_clear_errors();
    cg_destroy(&cg);
    be_destroy(&be);
    if (chk) checker_free(chk);
    elaborator_destroy(&elab);
    parser_destroy(&parser);
    if (ast_pp) preproc_free(ast_pp);
    if (pp) preproc_free(pp);
    if (f3) fclose(f3);
    if (f2) fclose(f2);
    if (f) fclose(f);
    array_destroy(&buf);
    array_destroy(&astfile);
    jsondom_destroy(&dom);
    return rc;
}

static int test_file_fail(struct TestState* state, const char* path)
{
    int rc = 1;
    Array msgpath = {0};
    Preprocessor* pp = NULL;
    Parser parser = {0};
    Elaborator elab = {0};
    FILE *msgfile = NULL, *memfile = NULL;
    FILE* f = fopen(path, "r");
    if (!f) REQUIRE_FAIL("failed to open test file %s", path);

    parser_clear_errors();
    pp = preproc_alloc();
    if (preproc_file(pp, f, last_path_element(path)))
    {
        rc = 0;
        goto failed;
    }
    if (parser_parse(&parser, preproc_tokens(pp), preproc_stringpool(pp)))
    {
        rc = 0;
        goto failed;
    }
    parser_debug_check(&parser);
    if (parser_has_errors())
    {
        rc = 0;
        goto failed;
    }

    elaborator_init(&elab, &parser);
    if (elaborate(&elab))
    {
        rc = 0;
        goto failed;
    }
    REQUIRE_FAIL_IMPL(path, 1, "was expected to fail, but passed");

failed:
    array_appends(&msgpath, path);
    array_push(&msgpath, ".txt", 5);
    if (msgfile = fopen(msgpath.data, "rb"))
    {
        char msgbuf[1024], membuf[1024];

        const size_t membuf_sz = parser_print_msgs_mem(membuf, sizeof(membuf));
        const size_t msgbuf_sz = fread(msgbuf, 1, sizeof(msgbuf), msgfile);

        if (rc = require_lines_eq(state, msgbuf, msgbuf_sz, membuf, membuf_sz, msgpath.data)) goto fail;
    }
    rc = 0;

fail:
    array_destroy(&msgpath);
    parser_clear_errors();
    elaborator_destroy(&elab);
    parser_destroy(&parser);
    if (pp) preproc_free(pp);
    if (f) fclose(f);
    if (msgfile) fclose(msgfile);
    if (memfile) fclose(memfile);
    return rc;
}

static void foreach_c_file(struct TestState* state,
                           const char* subdir,
                           int (*cb)(struct TestState* state, const char* path))
{
    Array filebuf = {0};
    Array arr = {0};
    assign_path_join(&arr, g_datadir, g_datadir_sz, subdir, strlen(subdir));
    DIR* dir = opendir(arr.data);
    if (!dir)
    {
        fprintf(stderr, "error: opendir(): ");
        perror(arr.data);
        exit(1);
    }
    const size_t base_sz = arr.sz - 1;
    struct dirent* ent;
    while (ent = readdir(dir))
    {
#ifdef __APPLE__
        size_t len = ent->d_namlen;
#else
        size_t len = strlen(ent->d_name);
#endif
        if (len < 2) continue;

        // Only test files ending in .c
        if (ent->d_name[len - 2] != '.' || ent->d_name[len - 1] != 'c') continue;

        array_shrink(&arr, base_sz, 1);
        path_combine(&arr, ent->d_name, len);
        array_push_byte(&arr, '\0');

        ++state->tests;
        if (cb(state, arr.data)) ++state->testfails;
    }

    closedir(dir);
    array_destroy(&arr);
    array_destroy(&filebuf);
}

#define REQUIRE_ENUM(expected, actual, stringify)                                                                      \
    do                                                                                                                 \
    {                                                                                                                  \
        state->assertions++;                                                                                           \
        int _expr_a = (expected);                                                                                      \
        int _expr_b = (actual);                                                                                        \
        if (_expr_a != _expr_b)                                                                                        \
        {                                                                                                              \
            unittest_print_stack(state);                                                                               \
            fprintf(stderr,                                                                                            \
                    "%s:%d: error: '%s == %s' was '%s == %s'\n",                                                       \
                    __FILE__,                                                                                          \
                    __LINE__,                                                                                          \
                    STRINGIFY(expected),                                                                               \
                    STRINGIFY(actual),                                                                                 \
                    (stringify)(_expr_a),                                                                              \
                    (stringify)(_expr_b));                                                                             \
            state->assertionfails++;                                                                                   \
            goto fail;                                                                                                 \
        }                                                                                                              \
    } while (0)

int require_taco(TestState* state,
                 int expected,
                 const char* expected_str,
                 int actual,
                 const char* actual_str,
                 const char* file,
                 int line)
{
    if (expected != actual)
    {
        REQUIRE_FAIL_IMPL(file,
                          line,
                          "'%s == %s' was '%s == %s'",
                          expected_str,
                          actual_str,
                          taco_to_string(expected),
                          taco_to_string(actual));
    }
    return 0;
fail:
    return 1;
}

int require_taca(TestState* state, struct TACAddress* expected, struct TACAddress* actual, const char* file, int line)
{
    int rc = 1;
    struct Array buf_expected = {0};
    struct Array buf_actual = {0};
    debug_taca(&buf_expected, expected);
    debug_taca(&buf_actual, actual);
    REQUIRE_MEM_EQ_IMPL(
        file, line, "expected", buf_expected.data, buf_expected.sz, "actual", buf_actual.data, buf_actual.sz);
    rc = 0;
fail:
    array_destroy(&buf_expected);
    array_destroy(&buf_actual);
    return rc;
}

int require_tace(TestState* state, struct TACEntry* expected, struct TACEntry* actual, const char* file, int line)
{
    int rc = 0;
    rc |= require_taco(state, expected->op, "expected", actual->op, "actual", file, line - 3);
    INFO("arg1\n", 1) { rc |= require_taca(state, &expected->arg1, &actual->arg1, file, line - 2); }
    INFO("arg2\n", 2) { rc |= require_taca(state, &expected->arg2, &actual->arg2, file, line - 1); }
    return rc;
}
int require_tace2(
    TestState* state, StandardTest* test, struct TACEntry* expected, const char* file, int line, size_t* index)
{
    int rc = 0;
    if (*index < array_size(&test->be->code, sizeof(struct TACEntry)))
    {
        UNWRAP(require_tace(state, expected, (TACEntry*)test->be->code.data + (*index)++, file, line));
    }
    else
    {
        REQUIRE_FAIL("expected additional TACEntry (found %zu)", (size_t)*index);
    }
fail:
    return rc;
}

#define REQUIRE_NEXT_TACE(...)                                                                                         \
    do                                                                                                                 \
    {                                                                                                                  \
        struct TACEntry expected = __VA_ARGS__;                                                                        \
        if (require_tace2(state, test, &expected, __FILE__, __LINE__, &index)) goto fail;                              \
    } while (0)

#define REQUIRE_END_TACE() REQUIRE_ZU_EQ(index, array_size(&test->be->code, sizeof(struct TACEntry)))

static size_t end_of_line(size_t offset, const char* const data, const size_t data_len)
{
    const char* const eol = memchr(data + offset, '\n', data_len - offset);
    return eol ? eol - data : data_len;
}

static size_t after_end_of_line(size_t offset, const char* const data, const size_t data_len)
{
    const char* const eol = memchr(data + offset, '\n', data_len - offset);
    return eol ? eol - data + 1 : data_len;
}

static size_t start_of_line_text(size_t offset, const char* const data, const size_t data_len)
{
    do
    {
        while (offset < data_len && (data[offset] == ' ' || data[offset] == '\n'))
            ++offset;
        if (offset < data_len && (data[offset] == '#' || data[offset] == '.'))
            offset = after_end_of_line(offset, data, data_len);
        else
            return offset;
    } while (1);
}

static int require_next_text(TestState* state,
                             StandardTest* test,
                             const char* file,
                             unsigned line,
                             size_t* index,
                             const char* expected,
                             const char* expected_str)
{
    *index = start_of_line_text(*index, test->cg->code.data, test->cg->code.sz);
    if (*index < test->cg->code.sz)
    {
        size_t expected_len = strlen(expected);
        const char* actual = (const char*)test->cg->code.data + *index;
        size_t actual_len = end_of_line(*index, test->cg->code.data, test->cg->code.sz) - *index;
        REQUIRE_MEM_EQ_IMPL(file, line, expected_str, expected, expected_len, "actual", actual, actual_len);
        *index += actual_len;
        if (*index != test->cg->code.sz) ++*index;
    }
    else
    {
        REQUIRE_FAIL_IMPL(file, line, "%s", "expected more text.");
    }
    return 0;
fail:
    return 1;
}

#define REQUIRE_NEXT_TEXT(...)                                                                                         \
    do                                                                                                                 \
    {                                                                                                                  \
        if (require_next_text(state, test, __FILE__, __LINE__, &index, (__VA_ARGS__), #__VA_ARGS__)) goto fail;        \
    } while (0)

#define REQUIRE_END_TEXT()                                                                                             \
    do                                                                                                                 \
    {                                                                                                                  \
        index = start_of_line_text(index, test->cg->code.data, test->cg->code.sz);                                     \
        if (index < test->cg->code.sz)                                                                                 \
        {                                                                                                              \
            const char* actual = (const char*)test->cg->code.data + index;                                             \
            size_t actual_len = end_of_line(index, test->cg->code.data, test->cg->code.sz) - index;                    \
            REQUIRE_FAIL("expected end of text but found \"%.*s\"", (int)actual_len, actual);                          \
        }                                                                                                              \
    } while (0)

int test_cg_assign(TestState* state, StandardTest* test)
{
    int rc = 1;
    test->cg = my_malloc(sizeof(struct CodeGen));
    cg_init(test->cg);
    test->cg->target = CG_TARGET_MACOS_GAS;
    TACEntry taces[] = {
        // stores from reg
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1},
            {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RDI},
            .assign_width = 8,
        },
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1},
            {TACA_REG, .sizing = s_sizing_uint, .reg = REG_RDI},
            .assign_width = 4,
        },
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1},
            {TACA_REG, .sizing.width = 2, .reg = REG_RDI},
            .assign_width = 2,
        },
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1},
            {TACA_REG, .sizing = s_sizing_uchar, .reg = REG_RDI},
            .assign_width = 1,
        },
        // loads to reg
        {
            TACO_ASSIGN,
            {TACA_REG, .is_addr = 1, .reg = REG_RDI},
            {TACA_FRAME, .sizing = s_sizing_ptr},
            .assign_width = 8,
        },
        {
            TACO_ASSIGN,
            {TACA_REG, .is_addr = 1, .reg = REG_RDI},
            {TACA_FRAME, .sizing = s_sizing_uint},
            .assign_width = 4,
        },
        // memory load+store
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1, .offset = 20},
            {TACA_FRAME, .sizing = s_sizing_uint, .offset = 24},
            .assign_width = 4,
        },
        // leaq
        {
            TACO_ASSIGN,
            {TACA_REG, .is_addr = 1, .reg = REG_RAX},
            {TACA_FRAME, .is_addr = 1, .offset = 24},
            .assign_width = 8,
        },
        // assign through: mov %ebx, 0(%rax)
        {
            TACO_ASSIGN,
            {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RAX},
            {TACA_REG, .sizing = s_sizing_uint, .reg = REG_RBX},
            .assign_width = 4,
        },
        // store address
        {
            TACO_ASSIGN,
            {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RAX},
            {TACA_FRAME, .is_addr = 1, .offset = 1},
            .assign_width = 8,
        },
        // calc address
        {
            TACO_ASSIGN,
            {TACA_REG, .is_addr = 1, .reg = REG_RAX},
            {TACA_ARG, .is_addr = 1},
            .assign_width = 8,
        },
        {
            TACO_ASSIGN,
            {TACA_PARAM, .is_addr = 1},
            {TACA_REG, .sizing = s_sizing_ptr, .reg = REG_RAX},
            .assign_width = 8,
        },
        // memset 0
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 0},
            .assign_width = 32,
        },
        // memcpy
        {
            TACO_ASSIGN,
            {TACA_FRAME, .sizing = s_sizing_ptr},
            {TACA_FRAME, .sizing.width = 32, .offset = 8},
            .assign_width = 32,
        },
        // assign through ref
        {
            TACO_ASSIGN,
            {TACA_REF, .sizing = s_sizing_ptr, .ref = 0},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
            .assign_width = 4,
        },
        // mixed sizes
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1, .offset = 4},
            {TACA_FRAME, .sizing = s_sizing_schar},
            .assign_width = 4,
        },
        // small odd size load
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1, .offset = 4},
            {TACA_FRAME, .sizing = 0, 3},
            .assign_width = 8,
        },
        // small odd size store
        {
            TACO_ASSIGN,
            {TACA_FRAME, .is_addr = 1, .offset = 4},
            {TACA_FRAME, .sizing = 0, 8},
            .assign_width = 3,
        },
        // high reg use
        {
            TACO_ASSIGN,
            {TACA_NAME, .sizing = s_sizing_ptr, .name = "out"},
            {TACA_NAME, .sizing = 0, 3, .name = "in"},
            .assign_width = 3,
        },
        // high reg use 2
        {
            TACO_ASSIGN,
            {TACA_NAME, .is_addr = 1, .name = "out"},
            {TACA_NAME, .sizing = 0, 3, .name = "in"},
            .assign_width = 3,
        },
    };
    rc = cg_gen_taces(test->cg, taces, sizeof(taces) / sizeof(taces[0]), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    size_t index = 0;
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    REQUIRE_NEXT_TEXT("mov %rdi, 8(%rsp)");
    REQUIRE_NEXT_TEXT("mov %edi, 8(%rsp)");
    REQUIRE_NEXT_TEXT("mov %di, 8(%rsp)");
    REQUIRE_NEXT_TEXT("mov %dil, 8(%rsp)");

    REQUIRE_NEXT_TEXT("mov 8(%rsp), %rdi");

    REQUIRE_NEXT_TEXT("mov 8(%rsp), %edi");

    REQUIRE_NEXT_TEXT("mov 32(%rsp), %r11d");
    REQUIRE_NEXT_TEXT("mov %r11d, 28(%rsp)");

    REQUIRE_NEXT_TEXT("leaq 32(%rsp), %rax");

    REQUIRE_NEXT_TEXT("mov %ebx, (%rax)");

    REQUIRE_NEXT_TEXT("leaq 9(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11, (%rax)");

    REQUIRE_NEXT_TEXT("leaq 128(%rsp), %rax");
    REQUIRE_NEXT_TEXT("mov %rax, 0(%rsp)");

    // memset 0
    REQUIRE_NEXT_TEXT("leaq 8(%rsp), %rdi");
    REQUIRE_NEXT_TEXT("mov $32, %rcx");
    REQUIRE_NEXT_TEXT("xor %rax, %rax");
    REQUIRE_NEXT_TEXT("rep stosb");

    // memcpy
    REQUIRE_NEXT_TEXT("leaq 16(%rsp), %rsi");
    REQUIRE_NEXT_TEXT("mov 8(%rsp), %rdi");
    REQUIRE_NEXT_TEXT("mov $32, %rcx");
    REQUIRE_NEXT_TEXT("cld");
    REQUIRE_NEXT_TEXT("rep movsb");

    // assign through ref
    REQUIRE_NEXT_TEXT("mov 112(%rsp), %rdi");
    REQUIRE_NEXT_TEXT("movl $1, (%rdi)");

    // mixed sizes
    REQUIRE_NEXT_TEXT("movsb 8(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov %r11d, 12(%rsp)");

    // small odd size load
    REQUIRE_NEXT_TEXT("movzw 8(%rsp), %r11");
    REQUIRE_NEXT_TEXT("movzb 10(%rsp), %r10");
    REQUIRE_NEXT_TEXT("shl $16, %r10");
    REQUIRE_NEXT_TEXT("or %r10, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 12(%rsp)");

    // small odd size store
    REQUIRE_NEXT_TEXT("movzw 8(%rsp), %r11");
    REQUIRE_NEXT_TEXT("movzb 10(%rsp), %r10");
    REQUIRE_NEXT_TEXT("shl $16, %r10");
    REQUIRE_NEXT_TEXT("or %r10, %r11");

    REQUIRE_NEXT_TEXT("mov 12(%rsp), %r10");
    REQUIRE_NEXT_TEXT("mov %r11w, (%r10)");
    REQUIRE_NEXT_TEXT("mov %r11, %rax");
    REQUIRE_NEXT_TEXT("shr $16, %rax");
    REQUIRE_NEXT_TEXT("mov %al, 2(%r10)");

    // high reg use
    REQUIRE_NEXT_TEXT("movq _in@GOTPCREL(%rip), %r11");
    REQUIRE_NEXT_TEXT("movq _out@GOTPCREL(%rip), %r10");
    REQUIRE_NEXT_TEXT("leaq (%r11), %rsi");
    REQUIRE_NEXT_TEXT("mov (%r10), %rdi");
    REQUIRE_NEXT_TEXT("mov $3, %rcx");
    REQUIRE_NEXT_TEXT("cld");
    REQUIRE_NEXT_TEXT("rep movsb");

    // high reg use
    REQUIRE_NEXT_TEXT("movq _in@GOTPCREL(%rip), %r11");
    REQUIRE_NEXT_TEXT("leaq (%r11), %rsi");
    REQUIRE_NEXT_TEXT("mov _out@GOTPCREL(%rip), %rdi");
    REQUIRE_NEXT_TEXT("mov $3, %rcx");
    REQUIRE_NEXT_TEXT("cld");
    REQUIRE_NEXT_TEXT("rep movsb");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    return rc;
}

int test_cg_refs(TestState* state, StandardTest* test)
{
    int rc = 1;
    test->cg = my_malloc(sizeof(struct CodeGen));
    cg_init(test->cg);
    test->cg->target = CG_TARGET_MACOS_GAS;
    TACEntry taces[] = {
        {
            TACO_ADD,
            {TACA_LNAME, .is_addr = 1, .name = "a"},
            {TACA_LNAME, .sizing = s_sizing_int, .name = "b"},
        },
        {
            TACO_ADD,
            {TACA_NAME, .is_addr = 1, .name = "a"},
            {TACA_NAME, .sizing = s_sizing_int, .name = "b"},
        },
        {
            TACO_ASSIGN,
            {TACA_NAME, .is_addr = 1, .name = "a"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 10},
            .assign_width = 8,
        },
        {
            TACO_ASSIGN,
            {TACA_NAME, .is_addr = 1, .name = "a"},
            {TACA_FRAME, .sizing = s_sizing_int},
            .assign_width = 8,
        },
        {
            TACO_CALL,
            {TACA_NAME, .is_addr = 1, .name = "a"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
        },
        {
            TACO_CALL,
            {TACA_LNAME, .is_addr = 1, .name = "a"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
        },
        {
            TACO_CALL,
            {TACA_NAME, .sizing = 0, 8, .name = "a"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
        },
        {
            TACO_CALL,
            {TACA_LNAME, .sizing = 0, 8, .name = "a"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 1},
        },
    };
    rc = cg_gen_taces(test->cg, taces, sizeof(taces) / sizeof(taces[0]), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    size_t index = 0;
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    REQUIRE_NEXT_TEXT("leaq _a(%rip), %r11");
    REQUIRE_NEXT_TEXT("movsl _b(%rip), %r10");
    REQUIRE_NEXT_TEXT("add %r10, %r11");

    REQUIRE_NEXT_TEXT("movq _b@GOTPCREL(%rip), %r11");
    REQUIRE_NEXT_TEXT("mov _a@GOTPCREL(%rip), %r10");
    REQUIRE_NEXT_TEXT("movsl (%r11), %rax");
    REQUIRE_NEXT_TEXT("add %rax, %r10");

    REQUIRE_NEXT_TEXT("mov _a@GOTPCREL(%rip), %rdi");
    REQUIRE_NEXT_TEXT("movq $10, (%rdi)");

    REQUIRE_NEXT_TEXT("movsl 0(%rsp), %r11");
    REQUIRE_NEXT_TEXT("mov _a@GOTPCREL(%rip), %r10");
    REQUIRE_NEXT_TEXT("mov %r11, (%r10)");

    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq _a");
    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq _a");
    REQUIRE_NEXT_TEXT("movq _a@GOTPCREL(%rip), %r11");
    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq *(%r11)");
    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq *_a(%rip)");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    return rc;
}

int test_cg_call(TestState* state, StandardTest* test)
{
    int rc = 1;
    test->cg = my_malloc(sizeof(struct CodeGen));
    cg_init(test->cg);
    test->cg->target = CG_TARGET_MACOS_GAS;
    TACEntry taces[] = {
        {
            TACO_CALL,
            {TACA_NAME, .is_addr = 1, .name = "f"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
        {
            TACO_CALL,
            {TACA_NAME, .sizing = s_sizing_ptr, .name = "f"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
        {
            TACO_CALL,
            {TACA_LNAME, .is_addr = 1, .name = "f"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
        {
            TACO_CALL,
            {TACA_LNAME, .sizing = s_sizing_ptr, .name = "f"},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
    };
    rc = cg_gen_taces(test->cg, taces, sizeof(taces) / sizeof(taces[0]), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    size_t index = 0;
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq _f");

    REQUIRE_NEXT_TEXT("movq _f@GOTPCREL(%rip), %r11");
    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq *(%r11)");

    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq _f");

    REQUIRE_NEXT_TEXT("movb $0, %al");
    REQUIRE_NEXT_TEXT("callq *_f(%rip)");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    return rc;
}

int test_cg_add(TestState* state, StandardTest* test)
{
    int rc = 1;
    test->cg = my_malloc(sizeof(struct CodeGen));
    cg_init(test->cg);
    test->cg->target = CG_TARGET_MACOS_GAS;
    TACEntry taces[] = {
        {
            TACO_ADD,
            {TACA_FRAME, .is_addr = 1},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
        {
            TACO_ADD,
            {TACA_FRAME, .sizing = s_sizing_int},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
        {
            TACO_ADD,
            {TACA_FRAME, .sizing = s_sizing_schar},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
    };
    rc = cg_gen_taces(test->cg, taces, sizeof(taces) / sizeof(taces[0]), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    size_t index = 0;
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    REQUIRE_NEXT_TEXT("leaq 0(%rsp), %r11");
    REQUIRE_NEXT_TEXT("add $7, %r11");

    REQUIRE_NEXT_TEXT("movsl 0(%rsp), %r11");
    REQUIRE_NEXT_TEXT("add $7, %r11");

    REQUIRE_NEXT_TEXT("movsb 0(%rsp), %r11");
    REQUIRE_NEXT_TEXT("add $7, %r11");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    return rc;
}

int test_cg_bitmath(TestState* state, StandardTest* test)
{
    int rc = 1;
    test->cg = my_malloc(sizeof(struct CodeGen));
    cg_init(test->cg);
    TACEntry taces[] = {
        {
            TACO_BAND,
            {TACA_FRAME, .is_addr = 1},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
        {
            TACO_BOR,
            {TACA_REF, .sizing = s_sizing_int, .ref = 0},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
        {
            TACO_BXOR,
            {TACA_REF, .sizing = s_sizing_int, .ref = 1},
            {TACA_IMM, .sizing = s_sizing_int, .imm = 7},
        },
        {
            TACO_BNOT,
            {TACA_REF, .sizing = s_sizing_int, .ref = 2},
            0,
        },
    };
    rc = cg_gen_taces(test->cg, taces, sizeof(taces) / sizeof(taces[0]), 100);
    if (rc)
    {
        parser_print_errors(stderr);
    }
    REQUIRE_EQ(0, rc);

    size_t index = 0;
    REQUIRE_NEXT_TEXT("subq $120, %rsp");

    REQUIRE_NEXT_TEXT("leaq 0(%rsp), %r11");
    REQUIRE_NEXT_TEXT("andq $7, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 104(%rsp)");

    REQUIRE_NEXT_TEXT("movsl 104(%rsp), %r11");
    REQUIRE_NEXT_TEXT("orq $7, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 104(%rsp)");

    REQUIRE_NEXT_TEXT("movsl 104(%rsp), %r11");
    REQUIRE_NEXT_TEXT("xorq $7, %r11");
    REQUIRE_NEXT_TEXT("mov %r11, 104(%rsp)");

    REQUIRE_NEXT_TEXT("movsl 104(%rsp), %rax");
    REQUIRE_NEXT_TEXT("not %rax");

    REQUIRE_NEXT_TEXT("addq $120, %rsp");
    REQUIRE_NEXT_TEXT("ret");
    REQUIRE_END_TEXT();

    rc = 0;
fail:
    return rc;
}

void run_interval_tests(struct TestState* state);
void run_json_tests(struct TestState* state);

int main(int argc, char** argv)
{
    struct TestState _state = {.colorsuc = "", .colorerr = "", .colorreset = ""};
    struct TestState* state = &_state;

    const char* const clicolorforce = getenv("CLICOLOR_FORCE");
    const char* const clicolor = getenv("CLICOLOR");

    if (clicolorforce && 0 != strcmp(clicolorforce, "0") || !clicolor || 0 != strcmp(clicolor, "0"))
    {
        _state.colorsuc = "\033[32;1m";
        _state.colorerr = "\033[31;1m";
        _state.colorreset = "\033[m";
    }

    if (argc == 0)
        abort();
    else if (argc == 1)
    {
        fprintf(stderr, "No data dir specified.\n The first parameter to %s must be the data dir.\n", argv[0]);
        exit(1);
    }
    else
    {
        g_datadir = argv[1];
        g_datadir_sz = strlen(argv[1]);
    }

    foreach_c_file(state, "tests/pass", test_file);
    foreach_c_file(state, "tests/fail", test_file_fail);

    run_interval_tests(state);
    run_json_tests(state);

    typedef int (*stdtest_t)(struct TestState*, struct StandardTest*);

    static const stdtest_t stdtests[] = {
        test_cg_assign,
        test_cg_call,
        test_cg_add,
        test_cg_bitmath,
        test_cg_refs,
    };

    for (size_t i = 0; i < sizeof(stdtests) / sizeof(stdtests[0]); ++i)
    {
        struct StandardTest test = {0};
        state->tests++;
        if (stdtests[i](state, &test))
        {
            state->testfails++;
        }
        stdtest_destroy(&test);
    }

    const char* color = (state->testfails + state->assertionfails == 0) ? _state.colorsuc : _state.colorerr;

    printf("%s%d tests. %d failed. %d assertions. %d failed.%s\n",
           color,
           state->tests,
           state->testfails,
           state->assertions,
           state->assertionfails,
           _state.colorreset);

    array_destroy(&state->info);
    array_destroy(&state->stack);

    return state->testfails > 0 || state->assertionfails > 0;
}
