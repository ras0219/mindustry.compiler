#include "json_dom.h"

#include "json_writer.h"

enum JsonParseResult jsondom_fill_records(JsonDOM* dom, JsonParse* jp)
{
    enum JsonParseResult rc = json_err_success;
    size_t text_it = 0;
    size_t text_read, record_written;
    while (1)
    {
        array_alloc(&dom->records, sizeof(JsonRecord));
        array_pop(&dom->records, sizeof(JsonRecord));
        rc = json_parse_end(jp,
                            dom->text.data + text_it,
                            dom->text.sz - text_it,
                            &text_read,
                            array_end(&dom->records),
                            (dom->records.cap - dom->records.sz) / sizeof(JsonRecord),
                            &record_written);
        JsonRecord* r = array_end(&dom->records);
        for (size_t i = 0; i < record_written; ++i)
        {
            // fixup records for being deeper in the document
            r[i].offset += text_it;
        }
        array_commit(&dom->records, record_written * sizeof(JsonRecord));
        text_it += text_read;
        if (rc != json_err_success)
        {
            break;
        }
        if (dom->text.sz == text_it) break;
    }
    return rc;
}

void jsondom_diff(JsonDOM* dom1, JsonDOM* dom2, uint8_t* out)
{
    json_diff(dom1->records.data,
              dom1->records.sz / sizeof(JsonRecord),
              dom1->text.data,
              dom2->records.data,
              dom2->records.sz / sizeof(JsonRecord),
              dom2->text.data,
              out);
}

void jsondom_destroy(JsonDOM* dom)
{
    array_destroy(&dom->text);
    array_destroy(&dom->records);
}

static void jsondom_maybe_comma(JsonDOM* dom)
{
    if (dom->add_comma)
        array_push_byte(&dom->text, ',');
    else
        dom->add_comma = 1;
}

void jsondom_write_u64(JsonDOM* dom, uint64_t i)
{
    jsondom_maybe_comma(dom);
    const size_t offset = dom->text.sz;
    size_t avail = dom->text.cap - dom->text.sz;
    size_t n = json_write_u64(dom->text.data + dom->text.sz, avail, i);
    if (n > avail)
    {
        json_write_u64(array_alloc(&dom->text, n), n, i);
    }
    else
    {
        dom->text.sz += n;
    }
    JsonRecord r = {
        .kind = jsonr_number,
        .is_end = 1,
        .offset = offset,
        .n = n,
        .row = 1,
        .col = 1,
    };
    array_push(&dom->records, &r, sizeof(r));
}
void jsondom_write_i64(JsonDOM* dom, int64_t i)
{
    jsondom_maybe_comma(dom);
    const size_t offset = dom->text.sz;
    size_t avail = dom->text.cap - dom->text.sz;
    size_t n = json_write_i64(dom->text.data + dom->text.sz, avail, i);
    if (n > avail)
    {
        json_write_i64(array_alloc(&dom->text, n), n, i);
    }
    else
    {
        dom->text.sz += n;
    }
    JsonRecord r = {
        .kind = jsonr_number,
        .is_end = 1,
        .offset = offset,
        .n = n,
        .row = 1,
        .col = 1,
    };
    array_push(&dom->records, &r, sizeof(r));
}
void jsondom_write_string(JsonDOM* dom, const char* s, size_t l)
{
    jsondom_maybe_comma(dom);
    const size_t offset = dom->text.sz;
    size_t avail = dom->text.cap - dom->text.sz;
    size_t n = json_write_string(dom->text.data + dom->text.sz, avail, s, l);
    if (n > avail)
    {
        json_write_string(array_alloc(&dom->text, n), n, s, l);
    }
    else
    {
        dom->text.sz += n;
    }
    JsonRecord r = {
        .kind = jsonr_string,
        .is_end = 1,
        .offset = offset,
        .n = n,
        .row = 1,
        .col = 1,
    };
    array_push(&dom->records, &r, sizeof(r));
}
void jsondom_write_key(JsonDOM* dom, const char* s, size_t n)
{
    jsondom_write_string(dom, s, n);
    ((JsonRecord*)array_back(&dom->records, sizeof(JsonRecord)))->kind = jsonr_key;
    array_push_byte(&dom->text, ':');
    dom->add_comma = 0;
}
void jsondom_write_true(JsonDOM* dom)
{
    jsondom_maybe_comma(dom);
    JsonRecord r = {
        .kind = jsonr_kw_true,
        .is_end = 1,
        .offset = dom->text.sz,
        .n = 4,
        .row = 1,
        .col = 1,
    };
    array_push(&dom->records, &r, sizeof(r));
    array_push(&dom->text, "true", 4);
}
void jsondom_write_false(JsonDOM* dom)
{
    jsondom_maybe_comma(dom);
    JsonRecord r = {
        .kind = jsonr_kw_false,
        .is_end = 1,
        .offset = dom->text.sz,
        .n = 5,
        .row = 1,
        .col = 1,
    };
    array_push(&dom->records, &r, sizeof(r));
    array_push(&dom->text, "false", 5);
}
void jsondom_write_null(JsonDOM* dom)
{
    jsondom_maybe_comma(dom);
    JsonRecord r = {
        .kind = jsonr_kw_null,
        .is_end = 1,
        .offset = dom->text.sz,
        .n = 4,
        .row = 1,
        .col = 1,
    };
    array_push(&dom->records, &r, sizeof(r));
    array_push(&dom->text, "null", 4);
}
void jsondom_write_open_obj(JsonDOM* dom)
{
    jsondom_maybe_comma(dom);
    JsonRecord r = {
        .kind = jsonr_object_begin,
        .is_end = 1,
        .offset = dom->text.sz,
        .n = 1,
        .row = 1,
        .col = 1,
    };
    array_push(&dom->records, &r, sizeof(r));
    array_push(&dom->text, "{", 1);
    dom->add_comma = 0;
}
void jsondom_write_close_obj(JsonDOM* dom)
{
    JsonRecord r = {
        .kind = jsonr_object_end,
        .is_end = 1,
        .offset = dom->text.sz,
        .n = 1,
        .row = 1,
        .col = 1,
    };
    array_push(&dom->records, &r, sizeof(r));
    array_push(&dom->text, "}", 1);
    dom->add_comma = 1;
}
void jsondom_write_open_arr(JsonDOM* dom)
{
    jsondom_maybe_comma(dom);
    JsonRecord r = {
        .kind = jsonr_array_begin,
        .is_end = 1,
        .offset = dom->text.sz,
        .n = 1,
        .row = 1,
        .col = 1,
    };
    array_push(&dom->records, &r, sizeof(r));
    array_push(&dom->text, "[", 1);
    dom->add_comma = 0;
}
void jsondom_write_close_arr(JsonDOM* dom)
{
    JsonRecord r = {
        .kind = jsonr_array_end,
        .is_end = 1,
        .offset = dom->text.sz,
        .n = 1,
        .row = 1,
        .col = 1,
    };
    array_push(&dom->records, &r, sizeof(r));
    array_push(&dom->text, "]", 1);
    dom->add_comma = 1;
}
