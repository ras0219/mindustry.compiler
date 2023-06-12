#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "json.h"
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

    JsonParse parse = {0};
    const size_t n_records = 30;
    JsonRecord records[30];

    size_t read, used_records;
    int parse_failed = json_parse_end(&parse, doc.data, doc.sz, &read, records, n_records, &used_records);

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
                          doc.data + records[i].offset);
        }
        else
        {
            array_appendf(&buf, "%d %d %s\n", records[i].row, records[i].col, str);
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

void run_json_tests(TestState* state) { foreach_json_pass_file(state, "tests/json", &foreach_jpf_cb); }
