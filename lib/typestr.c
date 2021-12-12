#include "typestr.h"

#include <stdio.h>
#include <string.h>

int typestr_add_const(struct TypeStr* ts)
{
    if (!ts->used) return 1;
    char c = ts->buf[ts->used - 1];
    if (c == 'c') return 0;
    if (ts->used == sizeof(ts->buf)) return 1;
    ts->buf[ts->used++] = 'c';
    return 0;
}
int typestr_add_arg(struct TypeStr* ts, struct TypeStr* arg)
{
    if (!ts->used) return 1;
    if (ts->buf[ts->used - 1] != '(')
    {
        if (ts->used == sizeof(ts->buf)) return 1;
        ts->buf[ts->used++] = ',';
    }
    if (ts->used + arg->used > sizeof(ts->buf)) return 1;
    memcpy(ts->buf + ts->used, arg->buf, arg->used);
    ts->used += arg->used;
    return 0;
}

int typestr_add_arr(struct TypeStr* ts, int arity)
{
    if (!ts->used) return 1;
    if (arity < 0)
    {
        if (ts->used + 2 > sizeof(ts->buf)) return 1;
        ts->buf[ts->used++] = '[';
        ts->buf[ts->used++] = ']';
    }
    else
    {
        char buf[32];
        int to_write = snprintf(buf, 32, "[%x]", arity);
        if (ts->used + to_write > sizeof(ts->buf)) return 1;
        memcpy(ts->buf + ts->used, buf, to_write);
        ts->used += to_write;
    }
    return 0;
}
