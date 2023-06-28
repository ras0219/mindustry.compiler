#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "json.h"
#include "json_writer.h"
#include "path.h"
#include "unittest.h"

int require_lines_eq(struct TestState* state,
                     const char* ebuf1,
                     size_t ebuf1sz,
                     const char* ebuf2,
                     size_t ebuf2sz,
                     const char* filename);

static int read_contents(Array* out, const char* path)
{
    errno = 0;
    FILE* f = fopen(path, "r");
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

static void foreach_json_pass_file(struct TestState* state,
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

        // Only test files ending in .json.pass
        if (len < 10 || 0 != memcmp(".json.pass", ent->d_name + len - 10, 10)) continue;

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

static int foreach_jpf_cb(struct TestState* state, const char* p)
{
    int rc = 1;
    Array doc_path = {0};
    array_assign(&doc_path, p, strlen(p) - 5);
    array_push_byte(&doc_path, 0);
    Array doc = {0}, cbs = {0}, buf = {0};
    if (read_contents(&cbs, p) == ENOENT) abort();
    if (read_contents(&doc, doc_path.data) == ENOENT) abort();

    int parse_failed = 0;
    JsonParse parse = {0};
    size_t read, used_records;

    for (size_t start = 0; start < doc.sz && !parse_failed; start += read)
    {
        const size_t n_records = 10;
        JsonRecord records[10];

        enum JsonParseResult (*f)(JsonParse * j,
                                  const char* data,
                                  size_t n,
                                  size_t* data_used,
                                  JsonRecord* records,
                                  size_t n_records,
                                  size_t* records_used) = doc.sz - start > 100 ? json_parse_partial : json_parse_end;
        // loop through 100 bytes at a time
        parse_failed = f(&parse,
                         doc.data + start,
                         doc.sz - start > 100 ? 100 : doc.sz - start,
                         &read,
                         records,
                         n_records,
                         &used_records);

        for (size_t i = 0; i < used_records; ++i)
        {
            const char* str = jsonr_to_string[records[i].kind];
            if (records[i].kind == jsonr_key || records[i].kind == jsonr_string || records[i].kind == jsonr_number)
            {
                array_appendf(&buf,
                              "%d %d %s %d %.*s\n",
                              records[i].row,
                              records[i].col,
                              str,
                              records[i].is_end,
                              (int)records[i].n,
                              doc.data + start + records[i].offset);
            }
            else
            {
                array_appendf(&buf, "%d %d %s\n", records[i].row, records[i].col, str);
            }
        }
    }

    if (require_lines_eq(state, cbs.data, cbs.sz, buf.data, buf.sz, p)) goto fail;
    if (parse_failed)
    {
        PRINTF_ERR("%s:%d:%d: error: parse failed", (char*)doc_path.data, parse.row, parse.col);
        REQUIRE_FAIL("failed to parse");
    }
    rc = 0;
fail:
    array_destroy(&doc_path);
    array_destroy(&doc);
    array_destroy(&cbs);
    array_destroy(&buf);
    return rc;
}

static int test_json_write_string(TestState* state)
{
    int rc = 1;
    unsigned char str[256];
    for (size_t i = 0; i < 256; ++i)
        str[i] = i;
#define BUF_SZ 400
    char buf[BUF_SZ] = {0};
    char expected[BUF_SZ] = "\"\\u0000\\u0001\\u0002\\u0003\\u0004\\u0005\\u0006\\u0007"
                            "\\b\\t\\n\\u000B\\f\\r\\u000E\\u000F"
                            "\\u0010\\u0011\\u0012\\u0013\\u0014\\u0015\\u0016\\u0017"
                            "\\u0018\\u0019\\u001A\\u001B\\u001C\\u001D\\u001E\\u001F"
                            " !\\\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRST"
                            "UVWXYZ[\\\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\x7F";
    for (size_t i = 0; i < 128; ++i)
    {
        expected[271 + i] = (unsigned char)(i + 128);
    }
    expected[BUF_SZ - 1] = '"';

    for (size_t i = BUF_SZ; i > 0; --i)
    {
        size_t n = json_write_string(buf, i - 1, (const char*)str, 256);
        REQUIRE_EQ(BUF_SZ, n);
        REQUIRE_MEM_EQ(expected, i - 1, buf, i - 1);
    }
    rc = 0;
fail:
    return rc;
}

static int test_json_write_number(TestState* state)
{
    int rc = 1;
    char str[25] = {0};
    for (size_t i = 0; i < 24; ++i)
    {
        REQUIRE_EQ(20, json_write_u64(str, i, UINT64_MAX));
        REQUIRE_EQ(str[i], 0);
    }
    REQUIRE_STR_EQ("18446744073709551615", str);
    REQUIRE_EQ(1, json_write_i64(str, 25, 0));
    REQUIRE_EQ(20, json_write_i64(str, 25, INT64_MIN));
    str[20] = 0;
    REQUIRE_STR_EQ("-9223372036854775808", str);
    rc = 0;
fail:
    return rc;
}

static int test_json_diff(TestState* state)
{
    int rc = 1;
    JsonParse p1 = {0}, p2 = {0};
    const char *doc1 = "[1, 2, 3, [9, 4], 5]", *doc2 = "[1, 2, 3, 4, 5, 6]";
    size_t doc1_n, doc2_n;
    JsonRecord r1[10];
    JsonRecord r2[10];
    size_t r1_n, r2_n;
    REQUIRE_EQ(json_err_success, json_parse_end(&p1, doc1, strlen(doc1), &doc1_n, r1, 10, &r1_n));
    REQUIRE_EQ(json_err_success, json_parse_end(&p2, doc2, strlen(doc2), &doc2_n, r2, 10, &r2_n));
    REQUIRE_EQ(10, r1_n);
    REQUIRE_EQ(8, r2_n);

    uint8_t seq[20];
    int unexpected = 0, expected = 0;

    json_diff(r1, r1_n, doc1, r2, r2_n, doc2, seq);
    size_t i1 = 0, i2 = 0;
    for (size_t i = 0; i < r1_n + r2_n; ++i)
    {
        if (seq[i] == jsond_left)
        {
            ++i1;
            ++unexpected;
        }
        else if (seq[i] == jsond_right)
        {
            ++i2;
            ++expected;
        }
        else if (seq[i] == jsond_both)
        {
            ++i1;
            ++i2;
            ++i;
        }
    }
    (void)i1;
    (void)i2;
    REQUIRE_EQ(1, expected);
    REQUIRE_EQ(3, unexpected);

    rc = 0;
fail:
    return rc;
}

void run_json_tests(TestState* state)
{
    foreach_json_pass_file(state, "tests/json", &foreach_jpf_cb);
    RUN_TEST(test_json_write_string);
    RUN_TEST(test_json_write_number);
    RUN_TEST(test_json_diff);
}
