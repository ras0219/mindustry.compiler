#pragma once

enum
{
    TYPESTR_BUF_SIZE = 32,
};

struct TypeStr
{
    // length counted, first byte is length
    char buf[TYPESTR_BUF_SIZE];
};

#if 0
__forceinline void typestr_add_const(struct TypeStr* ts) { return ts->buf[ts->buf[0]] == 'c'; }

__forceinline int typestr_add_pointer(struct TypeStr* ts)
{
    if (!ts->used) return 1;
    if (ts->used == sizeof(ts->buf)) return 1;
    ts->buf[ts->used++] = 'p';
    return 0;
}
__forceinline int typestr_start_call(struct TypeStr* ts)
{
    if (!ts->used) return 1;
    if (ts->used == sizeof(ts->buf)) return 1;
    ts->buf[ts->used++] = '(';
    return 0;
}
int typestr_add_arg(struct TypeStr* ts, struct TypeStr* arg);

__forceinline int typestr_end_call(struct TypeStr* ts)
{
    if (!ts->used) return 1;
    if (ts->used == sizeof(ts->buf)) return 1;
    ts->buf[ts->used++] = ')';
    return 0;
}
int typestr_add_arr(struct TypeStr* ts, int arity);
#endif
#if 0
static struct TypeStr s_type_literal_int = {
    .buf = {'I', 'c'},
    .used = 2,
};
static struct TypeStr s_type_literal_cstr = {
    .buf = {'C', 'c', 'p'},
    .used = 3,
};
static struct TypeStr s_type_int = {
    .buf = {'I'},
    .used = 1,
};
static struct TypeStr s_type_long = {
    .buf = {'l'},
    .used = 1,
};
static struct TypeStr s_type_short = {
    .buf = {'s'},
    .used = 1,
};
static struct TypeStr s_type_void = {
    .buf = {'V'},
    .used = 1,
};
static struct TypeStr s_type_char = {
    .buf = {'C'},
    .used = 1,
};
static struct TypeStr s_type_mstr = {
    .buf = {'M'},
    .used = 1,
};
static struct TypeStr s_type_unit = {
    .buf = {'U'},
    .used = 1,
};
#endif