#include "json.h"

#include "charclass.h"
#include "string.h"

enum
{
    state_start = 0,
    state_end,
    // { ^ }
    // { ^ "" : e }
    state_obj,
    // { "" ^ : e }
    state_obj2,
    // { "" : e ^ }
    // { "" : e ^ , "" : e }
    state_obj3,
    // { "" : e , ^ "" : e }
    state_obj4,
    // [ ^ ]
    // [ ^ e ]
    state_arr,
    // [ e ^ ]
    // [ e ^ , e ]
    state_arr2,

    stateclass_ignore_ws = state_arr2,
    /// --- end class ignore ws

    state_t,
    state_tr,
    state_tru,

    state_f,
    state_fa,
    state_fal,
    state_fals,

    state_n,
    state_nu,
    state_nul,

    stateclass_kw_start = state_t,
    stateclass_kw_end = state_nul,
    /// --- end class kw

    // "...^..."
    state_key,
    state_str,
    stateclass_str_end = state_str,

    // "...\^..."
    state_key_esc,
    state_str_esc,

    stateclass_str_esc_end = state_str_esc,

    state_err,
};

static int json_stk_push_obj(JsonParse* j)
{
    if (j->stk > (uint64_t)INT64_MAX) return 1;
    if (!j->stk)
    {
        j->stk = 2;
        return 0;
    }
    j->stk <<= 1;
    return 0;
}

static int json_stk_push_arr(JsonParse* j)
{
    int r = json_stk_push_obj(j);
    if (!r) j->stk |= 1;
    return r;
}

enum
{
    json_stk_obj = 0,
    json_stk_arr = 1,
    json_stk_underflow,
};

static int json_stk_pop(JsonParse* j)
{
    if (j->stk < 2) return json_stk_underflow;
    int r = j->stk & 1;
    j->stk >>= 1;
    return r;
}

static void json_adv_rc(JsonParse* j, char ch)
{
    if (ch == '\n')
    {
        j->row++;
        j->col = 1;
    }
    else
    {
        ++j->col;
    }
}
static void json_adv_rc_range(JsonParse* j, const char* x, size_t n)
{
    if (n == 0) return;
    if (n == 1) return json_adv_rc(j, x[0]);

    const char* const e = x + n;
    while (n > 1)
    {
        const char* i = memchr(x, '\n', n);
        if (!i)
        {
            j->col += n;
            return;
        }
        j->row++;
        j->col = 1;
        n = e - i - 1;
        x = i + 1;
    }
    if (n == 1) return json_adv_rc(j, x[0]);
}

struct kw_data
{
    char expected;
    char complete;
};

static const struct kw_data s_kw_data[] = {
    {'r', 0},
    {'u', 0},
    {'e', 1},

    {'a', 0},
    {'l', 0},
    {'s', 0},
    {'e', 1},

    {'u', 0},
    {'l', 0},
    {'l', 1},
};

static const char* s_kw_names[] = {
    "error: expected 'true'",
    "error: expected 'true'",
    "error: expected 'true'",

    "error: expected 'false'",
    "error: expected 'false'",
    "error: expected 'false'",
    "error: expected 'false'",

    "error: expected 'null'",
    "error: expected 'null'",
    "error: expected 'null'",
};

int json_parse_end(JsonParse* j, const JsonSAXVTable* t, void* userp, const char* data, size_t n)
{
    if (n == SIZE_MAX) n = strlen(data);
    if (j->row == 0) j->row = 1;
    if (j->col == 0) j->col = 1;

    const char* errmsg = NULL;

    size_t i = 0;
    for (; i < n; ++i)
    {
        unsigned char ch = data[i];
        unsigned char class = s_char_classes[ch];

        if (j->state <= stateclass_ignore_ws)
        {
            if (class & char_class_ws)
            {
                json_adv_rc(j, ch);
                continue;
            }

            switch (j->state)
            {
                case state_start:
                state_start:
                    if (ch == 'n')
                    {
                        if (t->kw_null) t->kw_null(userp);
                        j->state = state_n;
                    }
                    else if (ch == 'f')
                    {
                        if (t->kw_false) t->kw_false(userp);
                        j->state = state_f;
                    }
                    else if (ch == 't')
                    {
                        if (t->kw_true) t->kw_true(userp);
                        j->state = state_t;
                    }
                    else if (ch == '"')
                    {
                        j->state = state_str;
                    }
                    else if (ch == '{')
                    {
                        if (t->object_begin) t->object_begin(userp);
                        j->state = state_obj;
                    }
                    else if (ch == '[')
                    {
                        if (t->array_begin) t->array_begin(userp);
                        j->state = state_arr;
                    }
                    else
                    {
                        goto err_expected_elem;
                    }
                    break;
                case state_end: goto err_expected_eof;
                case state_obj:
                    if (ch == '"')
                    {
                        j->state = state_key;
                        break;
                    }
                    if (ch != '}') goto err_expected_endobj;
                    if (t->object_end) t->object_end(userp);
                    goto consume_reduce;
                case state_obj2:
                    if (ch != ':') goto err_expected_colon;
                    if (json_stk_push_obj(j)) goto err_push_stk;
                    j->state = state_start;
                    break;
                case state_obj3:
                    if (ch == ',')
                    {
                        j->state = state_obj4;
                        break;
                    }
                    if (ch != '}') goto err_expected_endobj;
                    if (t->object_end) t->object_end(userp);
                    goto consume_reduce;
                case state_obj4:
                    if (ch == '"')
                    {
                        j->state = state_key;
                        break;
                    }
                    if (ch == '}') goto err_illegal_trailing_comma;
                    goto err_expected_key;
                case state_arr:
                    if (ch != ']')
                    {
                        if (json_stk_push_arr(j)) goto err_push_stk;
                        j->state = state_start;
                        goto state_start;
                    }
                    if (t->array_end) t->array_end(userp);
                    goto consume_reduce;
                case state_arr2:
                    if (ch == ',')
                    {
                        if (json_stk_push_arr(j)) goto err_push_stk;
                        j->state = state_start;
                        break;
                    }
                    else if (ch != ']')
                        goto err_expected_endarr;
                    if (t->array_end) t->array_end(userp);
                    goto consume_reduce;
                default: abort();
            }
        }
        else if (j->state <= stateclass_kw_end)
        {
            const struct kw_data k = s_kw_data[j->state - stateclass_kw_start];
            if (ch == k.expected)
            {
                if (k.complete) goto consume_reduce;
                ++j->state;
                json_adv_rc(j, ch);
                continue;
            }
            else
            {
                errmsg = s_kw_names[j->state - stateclass_kw_start];
                goto err_kw;
            }
        }
        else if (j->state <= stateclass_str_esc_end) // includes str states
        {
            size_t k = i;
            for (; k < n; ++k)
            {
                if (j->state > stateclass_str_end)
                {
                    j->state -= 2;
                }
                else
                {
                    if (data[k] == '\\') j->state += 2;
                    if (data[k] == '"') break;
                }
            }

            const json_sax_text_cb cb = j->state == state_key ? t->key : t->string;
            if (cb) cb(userp, data + i, k - i, k < n);
            json_adv_rc_range(j, data + i, k - i);
            i = k;
            if (k < n)
            {
                if (j->state == state_key)
                {
                    j->state = state_obj2;
                }
                else
                    goto consume_reduce;
            }
        }
        else
        {
            goto error;
        }

        json_adv_rc(j, ch);
        continue;
    consume_reduce:;
        json_adv_rc(j, ch);
        int s = json_stk_pop(j);
        if (s == json_stk_underflow)
            j->state = state_end;
        else if (s == json_stk_obj)
            j->state = state_obj3;
        else
            j->state = state_arr2;
    }
    if (j->state != state_end) goto err_unexpected_eof;
    return 0;
err_kw:
    if (t->error) t->error(userp, errmsg);
    goto error;
err_expected_eof:
    if (t->error) t->error(userp, "error: expected end of document");
    goto error;
err_unexpected_eof:
    if (t->error) t->error(userp, "error: unexpected end of document");
    goto error;
err_push_stk:
    if (t->error) t->error(userp, "error: maximum depth exceeded");
    goto error;
err_expected_endobj:
    if (t->error) t->error(userp, "error: expected '}'");
    goto error;
err_expected_endarr:
    if (t->error) t->error(userp, "error: expected ']'");
    goto error;
err_expected_colon:
    if (t->error) t->error(userp, "error: expected ':'");
    goto error;
err_expected_key:
    if (t->error) t->error(userp, "error: expected key");
    goto error;
err_illegal_trailing_comma:
    if (t->error) t->error(userp, "error: illegal trailing comma");
    goto error;
err_expected_elem:
    if (t->error) t->error(userp, "error: expected element");
    goto error;
error:
    j->state = state_err;
    return 1;
}
