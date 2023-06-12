#include "json.h"

#include "charclass.h"
#include "string.h"

const char* const jsonr_to_string[] = {
#define Y(x) #x,
    FOREACH_JSON_CB(Y)
#undef Y
};

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
    /// --- end class str

    // - ^ 0 . dig exp
    // - ^ 1... . dig exp
    state_num_minus,
    // 0 ^
    // 0 ^ . dig exp
    // 0 ^ eE +- dig
    state_num_zero,
    // 1 ^
    // 1 ^ dig . dig exp
    // 1 ^ . dig exp
    // 1 ^ eE +- dig
    state_num_1,
    // 1 . ^ dig exp
    state_num_frac,
    // 1 . dig ^
    // 1 . dig ^ dig exp
    // 1 . dig ^ eE +- dig
    state_num_frac2,

    // eE ^ dig
    // eE ^ +- dig
    state_num_exp,

    // eE ^ dig
    state_num_exp_sign,

    // eE dig ^
    // eE dig ^ dig
    state_num_exp_dig,

    stateclass_num_end = state_num_exp_dig,
    /// --- end class num

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

static const char s_kw_names[] = {
    json_err_expected_true,
    json_err_expected_true,
    json_err_expected_true,

    json_err_expected_false,
    json_err_expected_false,
    json_err_expected_false,
    json_err_expected_false,

    json_err_expected_null,
    json_err_expected_null,
    json_err_expected_null,
};

static void jsonr_write_0(JsonParse* j, JsonRecord* record, int kind)
{
    memset(record, 0, sizeof(*record));
    record->row = j->row;
    record->col = j->col;
    record->kind = kind;
}
static void jsonr_write_1(JsonParse* j, JsonRecord* record, int kind, size_t offset, size_t n, int is_end)
{
    jsonr_write_0(j, record, kind);
    record->offset = offset;
    record->n = n;
    record->is_end = is_end;
}
static void jsonr_write_1_at(
    JsonParse* j, JsonRecord* record, int kind, size_t offset, size_t n, int is_end, int row, int col)
{
    memset(record, 0, sizeof(*record));
    record->row = row;
    record->col = col;
    record->kind = kind;
    record->offset = offset;
    record->n = n;
    record->is_end = is_end;
}

enum JsonParseResult json_parse_end(JsonParse* j,
                                    const char* data,
                                    size_t n,
                                    size_t* data_used,
                                    JsonRecord* records,
                                    size_t n_records,
                                    size_t* records_used)
{
    enum JsonParseResult rc = json_err_success;
    size_t r = 0;
    if (j->row == 0) j->row = 1;
    if (j->col == 0) j->col = 1;

    size_t i = 0;
    size_t number_start = 0;
    int number_row = j->row;
    int number_col = j->col;
    for (; i < n; ++i)
    {
        for (; r < n_records;)
        {
            unsigned char ch = data[i];
            unsigned char class = s_char_classes[ch];

            if (j->state <= stateclass_ignore_ws)
            {
                if (class & char_class_ws) goto consume;

                switch (j->state)
                {
                    case state_start:
                    state_start:
                        if (ch == 'n')
                        {
                            jsonr_write_0(j, records + r++, jsonr_kw_null);
                            j->state = state_n;
                        }
                        else if (ch == 'f')
                        {
                            jsonr_write_0(j, records + r++, jsonr_kw_false);
                            j->state = state_f;
                        }
                        else if (ch == 't')
                        {
                            jsonr_write_0(j, records + r++, jsonr_kw_true);
                            j->state = state_t;
                        }
                        else if (ch == '"')
                        {
                            j->state = state_str;
                        }
                        else if (ch == '{')
                        {
                            jsonr_write_0(j, records + r++, jsonr_object_begin);
                            j->state = state_obj;
                        }
                        else if (ch == '[')
                        {
                            jsonr_write_0(j, records + r++, jsonr_array_begin);
                            j->state = state_arr;
                        }
                        else if (ch == '-')
                        {
                            number_start = i;
                            number_row = j->row;
                            number_col = j->col;
                            j->state = state_num_minus;
                        }
                        else if (ch == '0')
                        {
                            number_start = i;
                            number_row = j->row;
                            number_col = j->col;
                            j->state = state_num_zero;
                        }
                        else if (class & char_class_digit)
                        {
                            number_start = i;
                            number_row = j->row;
                            number_col = j->col;
                            j->state = state_num_1;
                        }
                        else
                        {
                            rc = json_err_expected_element;
                            goto error;
                        }
                        break;
                    case state_end: rc = json_err_expected_eof; goto error;
                    case state_obj:
                        if (ch == '"')
                        {
                            j->state = state_key;
                            break;
                        }
                        if (ch != '}')
                        {
                            rc = json_err_expected_endobj;
                            goto error;
                        }
                        jsonr_write_0(j, records + r++, jsonr_object_end);
                        goto consume_reduce;
                    case state_obj2:
                        if (ch != ':')
                        {
                            rc = json_err_expected_colon;
                            goto error;
                        }

                        if (json_stk_push_obj(j))
                        {
                            rc = json_err_max_depth;
                            goto error;
                        }

                        j->state = state_start;
                        break;
                    case state_obj3:
                        if (ch == ',')
                        {
                            j->state = state_obj4;
                            break;
                        }
                        if (ch != '}')
                        {
                            rc = json_err_expected_endobj;
                            goto error;
                        }
                        jsonr_write_0(j, records + r++, jsonr_object_end);
                        goto consume_reduce;
                    case state_obj4:
                        if (ch == '"')
                        {
                            j->state = state_key;
                            break;
                        }
                        rc = json_err_expected_key;
                        goto error;
                    case state_arr:
                        if (ch != ']')
                        {
                            if (json_stk_push_arr(j))
                            {
                                rc = json_err_max_depth;
                                goto error;
                            }
                            j->state = state_start;
                            goto state_start;
                        }
                        jsonr_write_0(j, records + r++, jsonr_array_end);
                        goto consume_reduce;
                    case state_arr2:
                        if (ch == ',')
                        {
                            if (json_stk_push_arr(j))
                            {
                                rc = json_err_max_depth;
                                goto error;
                            }
                            j->state = state_start;
                            break;
                        }
                        else if (ch != ']')
                        {
                            rc = json_err_expected_endarr;
                            goto error;
                        }
                        jsonr_write_0(j, records + r++, jsonr_array_end);
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
                }
                else
                {
                    rc = s_kw_names[j->state - stateclass_kw_start];
                    goto error;
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

                const int kind = j->state == state_key ? jsonr_key : jsonr_string;
                jsonr_write_1(j, records + r++, kind, i, k - i, k < n);
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
            else if (j->state <= stateclass_num_end)
            {
                switch (j->state)
                {
                    case state_num_minus:
                        if (ch == '0')
                        {
                            j->state = state_num_zero;
                            break;
                        }
                        if (class & char_class_digit)
                        {
                            j->state = state_num_1;
                            break;
                        }
                        rc = json_err_expected_digit;
                        goto error;
                    case state_num_zero:
                        if (ch == '.')
                        {
                            j->state = state_num_frac;
                            break;
                        }
                        if (ch == 'e' || ch == 'E')
                        {
                            j->state = state_num_exp;
                            break;
                        }
                        jsonr_write_1_at(
                            j, records + r++, jsonr_number, number_start, i - number_start, 1, number_row, number_col);
                        goto reduce;
                    case state_num_1:
                        if (class & char_class_digit) break;
                        if (ch == 'e' || ch == 'E')
                        {
                            j->state = state_num_exp;
                            break;
                        }
                        if (ch == '.')
                        {
                            j->state = state_num_frac;
                            break;
                        }
                        jsonr_write_1_at(
                            j, records + r++, jsonr_number, number_start, i - number_start, 1, number_row, number_col);
                        goto reduce;
                    case state_num_frac:
                        if (class & char_class_digit)
                        {
                            j->state = state_num_frac2;
                            break;
                        }
                        rc = json_err_expected_digit;
                        goto error;
                    case state_num_frac2:
                        if (class & char_class_digit) break;
                        if (ch == 'e' || ch == 'E')
                        {
                            j->state = state_num_exp;
                            break;
                        }
                        jsonr_write_1_at(
                            j, records + r++, jsonr_number, number_start, i - number_start, 1, number_row, number_col);
                        goto reduce;
                    case state_num_exp:
                        if (ch == '-' || ch == '+')
                        {
                            j->state = state_num_exp_sign;
                            break;
                        }
                        // fallthrough
                    case state_num_exp_sign:
                        if (class & char_class_digit)
                        {
                            j->state = state_num_exp_dig;
                            break;
                        }
                        rc = json_err_expected_digit;
                        goto error;
                    case state_num_exp_dig:
                        if (class & char_class_digit) break;
                        jsonr_write_1_at(
                            j, records + r++, jsonr_number, number_start, i - number_start, 1, number_row, number_col);
                        goto reduce;
                    default: abort();
                }
            }
            else
            {
                abort();
            }

        consume:
            json_adv_rc(j, ch);
            break;

        consume_reduce:;
            int s = json_stk_pop(j);
            if (s == json_stk_underflow)
                j->state = state_end;
            else if (s == json_stk_obj)
                j->state = state_obj3;
            else
                j->state = state_arr2;
            goto consume;

        reduce:;
            s = json_stk_pop(j);
            if (s == json_stk_underflow)
                j->state = state_end;
            else if (s == json_stk_obj)
                j->state = state_obj3;
            else
                j->state = state_arr2;
        }
    }

    if (i == n)
    {
        if (j->state == state_num_zero || j->state == state_num_1 || j->state == state_num_frac2 ||
            j->state == state_num_exp_dig)
        {
            if (r == n_records) abort();
            jsonr_write_1_at(j, records + r++, jsonr_number, number_start, i - number_start, 1, number_row, number_col);
        }
        else if (j->state != state_end)
        {
            rc = json_err_unexpected_eof;
            goto error;
        }
    }
cleanup:
    *data_used = i;
    *records_used = r;
    return rc;
error:
    j->state = state_err;
    goto cleanup;
}
