#include "json.h"

#include "charclass.h"
#include "string.h"

enum
{
    state_start = 0,
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
    // { "...^..." : e}
    state_key,
    // { "...\^..." : e}
    state_key_esc,
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

int json_parse_end(JsonParse* j, const JsonSAXVTable* t, void* userp, const char* data, size_t n)
{
    if (n == SIZE_MAX) n = strlen(data);
    if (j->row == 0) j->row = 1;
    if (j->col == 0) j->col = 1;

    size_t i = 0;
    for (; i < n; ++i)
    {
        unsigned char ch = data[i];
        unsigned char class = s_char_classes[ch];

        switch (j->state)
        {
            case state_start:
                if (class & char_class_ws) break;

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
                else if (ch == '{')
                {
                    if (t->object_begin) t->object_begin(userp);
                    j->state = state_obj;
                }
                else
                {
                    goto err_expected_elem;
                }
                break;
            case state_t:
                if (ch != 'r') goto err_expected_true;
                j->state = state_tr;
                break;
            case state_tr:
                if (ch != 'u') goto err_expected_true;
                j->state = state_tru;
                break;
            case state_tru:
                if (ch != 'e') goto err_expected_true;
                goto reduce;
            case state_n:
                if (ch != 'u') goto err_expected_null;
                j->state = state_nu;
                break;
            case state_nu:
                if (ch != 'l') goto err_expected_null;
                j->state = state_nul;
                break;
            case state_nul:
                if (ch != 'l') goto err_expected_null;
                goto reduce;
            case state_f:
                if (ch != 'a') goto err_expected_false;
                j->state = state_fa;
                break;
            case state_fa:
                if (ch != 'l') goto err_expected_false;
                j->state = state_fal;
                break;
            case state_fal:
                if (ch != 's') goto err_expected_false;
                j->state = state_fals;
                break;
            case state_fals:
                if (ch != 'e') goto err_expected_false;
                goto reduce;
            case state_end:
                if (class & char_class_ws) break;
                goto err_expected_eof;
            case state_obj:
                if (class & char_class_ws) break;
                if (ch == '"')
                {
                    j->state = state_key;
                    break;
                }
                if (ch != '}') goto err_expected_endobj;
                if (t->object_end) t->object_end(userp);
                goto reduce;
            case state_key_esc: j->state = state_key; break;
            case state_key:;
                size_t k = i;
                for (; k < n; ++k)
                {
                    if (data[k] == '"') break;
                }
                if (t->key) t->key(userp, data + i, k - i, k < n);
                json_adv_rc_range(j, data + i, k - i);
                i = k;
                if (k < n) j->state = state_obj2;
                break;
            case state_obj2:
                if (class & char_class_ws) break;
                if (ch != ':') goto err_expected_colon;
                if (json_stk_push_obj(j)) goto err_push_obj;
                j->state = state_start;
                break;
            case state_obj3:
                if (class & char_class_ws) break;
                if (ch == ',') j->state = state_obj4;
                if (ch != '}') goto err_expected_endobj;
                if (t->object_end) t->object_end(userp);
                goto reduce;
            default: abort();
        }
        json_adv_rc(j, ch);
        continue;
    reduce:;
        json_adv_rc(j, ch);
        int s = json_stk_pop(j);
        if (s == json_stk_underflow)
            j->state = state_end;
        else if (s == json_stk_obj)
            j->state = state_obj3;
        else
            j->state = state_err;
    }
    if (j->state != state_end) goto err_unexpected_eof;
    return 0;
err_expected_true:
    if (t->error) t->error(userp, "error: expected 'true'");
    goto error;
err_expected_false:
    if (t->error) t->error(userp, "error: expected 'false'");
    goto error;
err_expected_null:
    if (t->error) t->error(userp, "error: expected 'null'");
    goto error;
err_expected_eof:
    if (t->error) t->error(userp, "error: expected end of document");
    goto error;
err_unexpected_eof:
    if (t->error) t->error(userp, "error: unexpected end of document");
    goto error;
err_push_obj:
    if (t->error) t->error(userp, "error: maximum depth exceeded");
    goto error;
err_expected_endobj:
    if (t->error) t->error(userp, "error: expected '}'");
    goto error;
err_expected_colon:
    if (t->error) t->error(userp, "error: expected ':'");
    goto error;
err_expected_elem:
    if (t->error) t->error(userp, "error: expected element");
    goto error;
error:
    j->state = state_err;
    return 1;
}
