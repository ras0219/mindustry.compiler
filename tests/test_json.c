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

typedef struct JSC
{
    Array buf;
    JsonParse p;
} JSC;

static int jsc_cb1(void* userp, const char* encoded, size_t n, int is_end, const char* kind)
{
    JSC* jsc = userp;
    array_appendf(&jsc->buf, "%d %d %s %d %.*s\n", jsc->p.row, jsc->p.col, kind, is_end, (int)n, encoded);
    return 0;
}
static int jsc_number(void* userp, const char* encoded, size_t n, int is_end)
{
    return jsc_cb1(userp, encoded, n, is_end, "number");
}
static int jsc_string(void* userp, const char* encoded, size_t n, int is_end)
{
    return jsc_cb1(userp, encoded, n, is_end, "string");
}
static int jsc_key(void* userp, const char* encoded, size_t n, int is_end)
{
    return jsc_cb1(userp, encoded, n, is_end, "key");
}
static int jsc_cb0(void* userp, const char* kind)
{
    JSC* jsc = userp;
    array_appendf(&jsc->buf, "%d %d %s\n", jsc->p.row, jsc->p.col, kind);
    return 0;
}

static int jsc_object_begin(void* userp) { return jsc_cb0(userp, "object_begin"); }
static int jsc_object_end(void* userp) { return jsc_cb0(userp, "object_end"); }
static int jsc_array_begin(void* userp) { return jsc_cb0(userp, "array_begin"); }
static int jsc_array_end(void* userp) { return jsc_cb0(userp, "array_end"); }
static int jsc_kw_true(void* userp) { return jsc_cb0(userp, "kw_true"); }
static int jsc_kw_false(void* userp) { return jsc_cb0(userp, "kw_false"); }
static int jsc_kw_null(void* userp) { return jsc_cb0(userp, "kw_null"); }
static int jsc_error(void* userp, const char* errmsg)
{
    JSC* jsc = userp;
    array_appendf(&jsc->buf, "%d %d error %s\n", jsc->p.row, jsc->p.col, errmsg);
    return 0;
}
static const JsonSAXVTable s_json_record_cbs = {
#define Y(x) .x = &jsc_##x,
    FOREACH_JSON_CB(Y)
#undef Y
};

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
    Array doc = {0}, cbs = {0};
    if (read_contents(&cbs, p) == ENOENT) abort();
    if (read_contents(&doc, doc_path.data) == ENOENT) abort();

    JSC jsc = {0};
    int parse_failed = json_parse_end(&jsc.p, &s_json_record_cbs, &jsc, doc.data, doc.sz);

    if (require_lines_eq(state, cbs.data, cbs.sz, jsc.buf.data, jsc.buf.sz, p)) goto fail;
    REQUIRE(!parse_failed);
    rc = 0;
fail:
    array_destroy(&doc_path);
    array_destroy(&doc);
    array_destroy(&cbs);
    return rc;
}

void run_json_tests(TestState* state) { foreach_json_pass_file(state, "tests/json", &foreach_jpf_cb); }
