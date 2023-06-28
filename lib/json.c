#include "json.h"

#include "diff.h"
#include "foreach128.h"
#include "string.h"

enum
{
    jsonchar_other,
    jsonchar_ws,
    jsonchar_obj_open,
    jsonchar_obj_close,
    jsonchar_arr_open,
    jsonchar_arr_close,
    jsonchar_zero,
    jsonchar_nz_digit,
    jsonchar_decimal,
    jsonchar_quote,
    jsonchar_colon,
    jsonchar_comma,
    jsonchar_minus,
    jsonchar_plus,
    jsonchar_backslash,
    jsonchar_slash,
    jsonchar_space,
    jsonchar_e,
    jsonchar_E,
    jsonchar_t,
    jsonchar_r,
    jsonchar_u,
    jsonchar_f,
    jsonchar_a,
    jsonchar_l,
    jsonchar_s,
    jsonchar_n,
    jsonchar_b,
    jsonchar_other_hex,
    jsonchar_lt0x20,

    JSONCHAR_MAX = jsonchar_lt0x20
};

const unsigned char s_jsonchar[256] = {
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    ['\t'] = jsonchar_ws,
    ['\n'] = jsonchar_ws,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    ['\r'] = jsonchar_ws,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    jsonchar_lt0x20,
    [' '] = jsonchar_space,
    ['.'] = jsonchar_decimal,
    ['"'] = jsonchar_quote,
    [','] = jsonchar_comma,
    [':'] = jsonchar_colon,
    ['['] = jsonchar_arr_open,
    [']'] = jsonchar_arr_close,
    ['{'] = jsonchar_obj_open,
    ['}'] = jsonchar_obj_close,
    ['\\'] = jsonchar_backslash,
    ['/'] = jsonchar_slash,
    ['+'] = jsonchar_plus,
    ['-'] = jsonchar_minus,
    ['A'] = jsonchar_other_hex,
    ['B'] = jsonchar_other_hex,
    ['C'] = jsonchar_other_hex,
    ['D'] = jsonchar_other_hex,
    ['E'] = jsonchar_E,
    ['F'] = jsonchar_other_hex,
    ['a'] = jsonchar_a,
    ['b'] = jsonchar_b,
    ['c'] = jsonchar_other_hex,
    ['d'] = jsonchar_other_hex,
    ['e'] = jsonchar_e,
    ['f'] = jsonchar_f,
    ['l'] = jsonchar_l,
    ['n'] = jsonchar_n,
    ['r'] = jsonchar_r,
    ['s'] = jsonchar_s,
    ['t'] = jsonchar_t,
    ['u'] = jsonchar_u,
};

enum
{
    jsonchar_class_control = 1,
    jsonchar_class_ws = 2,
    jsonchar_class_digit = 4,
    jsonchar_class_hex = 8,
    jsonchar_class_plusminus = 16,
    jsonchar_class_exp = 32,
    jsonchar_class_escape = 64,
};

static const unsigned char s_jsonchar_class[256] = {
#define Y(x)                                                                                                           \
    ((x <= 0x20) ? jsonchar_class_control : 0) |                                                                       \
        ((x == '\r' || x == '\n' || x == '\t' || x == ' ') ? jsonchar_class_ws : 0) |                                  \
        ((x >= '0' && x <= '9') ? jsonchar_class_digit : 0) |                                                          \
        (((x >= '0' && x <= '9') || (x >= 'A' && x <= 'F') || (x >= 'a' && x <= 'f')) ? jsonchar_class_hex : 0) |      \
        ((x == '+' || x == '-') ? jsonchar_class_plusminus : 0) | ((x == 'e' || x == 'E') ? jsonchar_class_exp : 0) |  \
        ((x == '"' || x == '\\' || x == 't' || x == 'r' || x == 'f' || x == 'b' || x == 'n' || x == '/')               \
             ? jsonchar_class_escape                                                                                   \
             : 0),

    X_FOREACH_128(Y)
#undef Y
};

#define Y(x) #x,
const char* const jsonr_to_string[] = {FOREACH_JSON_CB(Y)};
const char* const json_err_to_string[] = {FOREACH_JSON_ERR(Y)};
#undef Y

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

    // "...\^..."
    state_key_esc,
    state_str_esc,

    // "...\u^..."
    state_key_u0,
    state_str_u0,
    // "...\ux^..."
    state_key_u1,
    state_str_u1,
    // "...\uxx^..."
    state_key_u2,
    state_str_u2,
    // "...\uxxx^..."
    state_key_u3,
    state_str_u3,

    stateclass_str_start = state_key,
    stateclass_str_end = state_str_u3,
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

    stateclass_num_start = state_num_minus,
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

static void json_reduce(JsonParse* j)
{
    if (j->state == state_key)
    {
        j->state = state_obj2;
        return;
    }
    int s = json_stk_pop(j);
    if (s == json_stk_underflow)
        j->state = state_end;
    else if (s == json_stk_obj)
        j->state = state_obj3;
    else
        j->state = state_arr2;
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

struct JsonElemStart
{
    size_t start;
    int row;
    int col;
};

struct JsonLexResult
{
    size_t i;
    int err;
    int reduce;
};
static struct JsonLexResult json_parse_number_impl(JsonParse* j, size_t i, const char* data, size_t n)
{
    enum
    {
        // clang-format off
        jn_other, jn_0, jn_19, jn_p, jn_e, jn_s
        // clang-format on
    };
    static const unsigned char s_jsonnum_classify[128] = {
        // clang-format off
        ['0'] = jn_0, ['1'] = jn_19, ['2'] = jn_19, ['3'] = jn_19, ['4'] = jn_19,
        ['5'] = jn_19, ['6'] = jn_19, ['7'] = jn_19, ['8'] = jn_19, ['9'] = jn_19,
        ['.'] = jn_p, ['e'] = jn_e, ['E'] = jn_e, ['+'] = jn_s, ['-'] = jn_s,
        // clang-format on
    };
    // 0-63 => transition to state
    // 64-127 => error
    // 128 => reduce without consuming
    enum
    {
        ERR_D = 64 + json_err_expected_digit
    };
    static const unsigned char s_jsonnum_state_table[][6] = {
#define AT(s) [(s)-stateclass_num_start]
        AT(state_num_minus) = {ERR_D, state_num_zero, state_num_1, ERR_D, ERR_D, ERR_D},
        AT(state_num_zero) = {128, 128, 128, state_num_frac, state_num_exp, 128},
        AT(state_num_1) = {128, state_num_1, state_num_1, state_num_frac, state_num_exp, 128},
        AT(state_num_frac) = {ERR_D, state_num_frac2, state_num_frac2, ERR_D, ERR_D, ERR_D},
        AT(state_num_frac2) = {128, state_num_frac2, state_num_frac2, 128, state_num_exp, 128},
        AT(state_num_exp) = {ERR_D, state_num_exp_dig, state_num_exp_dig, ERR_D, ERR_D, state_num_exp_sign},
        AT(state_num_exp_sign) = {ERR_D, state_num_exp_dig, state_num_exp_dig, ERR_D, ERR_D, ERR_D},
        AT(state_num_exp_dig) = {128, state_num_exp_dig, state_num_exp_dig, 128, 128},
    };

    struct JsonLexResult ret = {.i = i};
    unsigned char ch;
    for (; ret.i < n; json_adv_rc(j, ch), ++ret.i)
    {
        ch = data[ret.i];
        int action = s_jsonnum_state_table AT(j->state)[ch < 128 ? s_jsonnum_classify[ch] : jn_other];
        if (action < 64)
        {
            j->state = action;
        }
        else if (action < 128)
        {
            ret.err = action - 64;
            return ret;
        }
        else
        {
            ret.reduce = 1;
            return ret;
        }
    }
    return ret;
#undef AT
}

static struct JsonLexResult json_parse_string_impl(JsonParse* j, size_t i, const char* data, size_t n)
{
    struct JsonLexResult ret = {.i = i};
    unsigned char ch;
    for (; ret.i < n; json_adv_rc(j, ch), ++ret.i)
    {
        ch = data[ret.i];
        unsigned s = (j->state - stateclass_str_start) >> 1;
        switch (s)
        {
            case (state_str - stateclass_str_start) >> 1:
                if (ch == '\\')
                {
                    j->state += state_str_esc - state_str;
                }
                else if (ch == '"')
                {
                    ++ret.i;
                    json_adv_rc(j, ch);
                    ret.reduce = 1;
                    return ret;
                }
                break;
            case (state_str_esc - stateclass_str_start) >> 1: j->state += state_str - state_str_esc; break;
            case (state_str_u0 - stateclass_str_start) >> 1:
            case (state_str_u1 - stateclass_str_start) >> 1:
            case (state_str_u2 - stateclass_str_start) >> 1:
            case (state_str_u3 - stateclass_str_start) >> 1:
            default: abort();
        }
    }
    return ret;
}

static enum JsonParseResult json_parse_partial_impl(JsonParse* j,
                                                    const char* data,
                                                    size_t n,
                                                    size_t* data_used,
                                                    JsonRecord* records,
                                                    size_t n_records,
                                                    size_t* records_used,
                                                    struct JsonElemStart* estart)
{
    enum JsonParseResult rc = json_err_success;
    size_t r = 0;
    if (j->row == 0) j->row = 1;
    if (j->col == 0) j->col = 1;

    size_t i = 0;
    estart->start = 0;
    estart->row = j->row;
    estart->col = j->col;
    for (; i < n && r < n_records;)
    {
        unsigned char ch = data[i];
        unsigned char class = s_jsonchar_class[ch];

        if (j->state <= stateclass_ignore_ws)
        {
            if (class & jsonchar_class_ws) goto consume;

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
                        estart->start = i;
                        estart->row = j->row;
                        estart->col = j->col;
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
                        estart->start = i;
                        estart->row = j->row;
                        estart->col = j->col;
                        j->state = state_num_minus;
                    }
                    else if (ch == '0')
                    {
                        estart->start = i;
                        estart->row = j->row;
                        estart->col = j->col;
                        j->state = state_num_zero;
                    }
                    else if (class & jsonchar_class_digit)
                    {
                        estart->start = i;
                        estart->row = j->row;
                        estart->col = j->col;
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
                        estart->start = i;
                        estart->row = j->row;
                        estart->col = j->col;
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
                        estart->start = i;
                        estart->row = j->row;
                        estart->col = j->col;
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
        else if (j->state <= stateclass_str_end)
        {
            struct JsonLexResult res = json_parse_string_impl(j, i, data, n);
            i = res.i;
            if (res.err)
            {
                rc = res.err;
                goto error;
            }
            else if (res.reduce)
            {
                const int kind = j->state == state_key ? jsonr_key : jsonr_string;
                jsonr_write_1_at(j, records + r++, kind, estart->start, i - estart->start, 1, estart->row, estart->col);
                goto reduce;
            }
            goto cleanup;
        }
        else if (j->state <= stateclass_num_end)
        {
            struct JsonLexResult res = json_parse_number_impl(j, i, data, n);
            i = res.i;
            if (res.err)
            {
                rc = res.err;
                goto error;
            }
            else if (res.reduce)
            {
                jsonr_write_1_at(
                    j, records + r++, jsonr_number, estart->start, i - estart->start, 1, estart->row, estart->col);
                goto reduce;
            }
            else
            {
                if (i != n) abort();
                break;
            }
        }
        else
        {
            abort();
        }

    consume:
        json_adv_rc(j, ch);
        ++i;
        if (i >= n) break;
        continue;

    consume_reduce:
        json_reduce(j);
        goto consume;

    reduce:
        json_reduce(j);
    }

cleanup:
    *data_used = i;
    *records_used = r;
    return rc;
error:
    j->state = state_err;
    goto cleanup;
}

static void json_parse_partial_report(
    JsonParse* j, size_t n, JsonRecord* records, size_t n_records, size_t* records_used, struct JsonElemStart* e)
{
    if (j->state == state_num_zero || j->state == state_num_1 || j->state == state_num_frac2 ||
        j->state == state_num_exp_dig)
    {
        if (*records_used == n_records) abort();
        jsonr_write_1_at(j, records + (*records_used)++, jsonr_number, e->start, n - e->start, 0, e->row, e->col);
    }
    else if (j->state >= stateclass_str_start && j->state <= stateclass_str_end)
    {
        if (*records_used == n_records) abort();
        const int kind = ((j->state - stateclass_str_start) & 1) ? jsonr_string : jsonr_key;
        jsonr_write_1_at(j, records + (*records_used)++, kind, e->start, n - e->start, 0, e->row, e->col);
    }
}

enum JsonParseResult json_parse_end(JsonParse* j,
                                    const char* data,
                                    size_t n,
                                    size_t* data_used,
                                    JsonRecord* records,
                                    size_t n_records,
                                    size_t* records_used)
{
    if (n_records == 0) return 0;
    struct JsonElemStart e;
    enum JsonParseResult rc = json_parse_partial_impl(j, data, n, data_used, records, n_records, records_used, &e);
    if (!rc)
    {
        if (*data_used == n)
        {
            if (j->state == state_num_zero || j->state == state_num_1 || j->state == state_num_frac2 ||
                j->state == state_num_exp_dig)
            {
                if (*records_used == n_records) abort();
                jsonr_write_1_at(j, records + (*records_used)++, jsonr_number, e.start, n - e.start, 1, e.row, e.col);
            }
            else if (j->state != state_end)
            {
                rc = json_err_unexpected_eof;
                j->state = state_err;
            }
        }
        else
        {
            json_parse_partial_report(j, *data_used, records, n_records, records_used, &e);
        }
    }
    return rc;
}

enum JsonParseResult json_parse_partial(JsonParse* j,
                                        const char* data,
                                        size_t n,
                                        size_t* data_used,
                                        JsonRecord* records,
                                        size_t n_records,
                                        size_t* records_used)
{
    struct JsonElemStart e;
    enum JsonParseResult rc = json_parse_partial_impl(j, data, n, data_used, records, n_records, records_used, &e);
    if (!rc)
    {
        json_parse_partial_report(j, *data_used, records, n_records, records_used, &e);
    }
    return rc;
}

// 0 if equal, 1 if different
static int jsonr_compare(const JsonRecord* r1, const char* doc1, const JsonRecord* r2, const char* doc2)
{
    if (r1->kind != r2->kind) return 1;
    if (r1->kind == jsonr_number || r1->kind == jsonr_string || r1->kind == jsonr_key)
    {
        if (r1->n != r2->n) return 1;
        if (0 != memcmp(doc1 + r1->offset, doc2 + r2->offset, r1->n)) return 1;
    }
    return 0;
}

void json_diff(
    const JsonRecord* r1, size_t n1, const char* doc1, const JsonRecord* r2, size_t n2, const char* doc2, uint8_t* out)
{
    uint8_t* comparisons = malloc(n1 * n2);
    for (size_t i = 0; i < n1; ++i)
    {
        const size_t o = i * n2;
        for (size_t j = 0; j < n2; ++j)
        {
            comparisons[o + j] = (0 == jsonr_compare(r1 + i, doc1, r2 + j, doc2));
        }
    }
    diff(comparisons, out, n1, n2);
    free(comparisons);
}
