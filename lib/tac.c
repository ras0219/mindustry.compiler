#include "tac.h"

#include <stdlib.h>

#include "array.h"

const char* taca_to_string(enum TACAKind k)
{
#define Y(Z)                                                                                                           \
    case Z: return #Z;

    switch (k)
    {
        X_TACA_KIND(Y)
        default: abort();
    }
}
const char* taco_to_string(enum TACOKind k)
{
    switch (k)
    {
        X_TACO_KIND(Y)
        default: abort();
    }
}
const char* register_to_string(enum Register k)
{
    switch (k)
    {
        X_REGISTER(Y)
        default: abort();
    }
}
#undef Y

void debug_tace(Array* arr, const TACEntry* tace)
{
    array_push_byte(arr, '(');
    array_appends(arr, taco_to_string(tace->op));
    array_push_byte(arr, ' ');
    debug_taca(arr, &tace->arg1);
    array_push_byte(arr, ' ');
    debug_taca(arr, &tace->arg2);
    array_push_byte(arr, ')');
}

void debug_sizing(Array* arr, Sizing sz) { array_appendf(arr, "%c%d", sz.is_signed ? 'i' : 'u', sz.width); }

void debug_taca(Array* arr, const TACAddress* addr)
{
    array_push_byte(arr, '(');
    array_appends(arr, taca_to_string(addr->kind));
    if (addr->is_addr) array_appends(arr, " is_addr");
    if (addr->sizing.width != 0)
    {
        array_push_byte(arr, ' ');
        debug_sizing(arr, addr->sizing);
    }
    switch (addr->kind)
    {
        case TACA_FRAME: array_appendf(arr, " %d", addr->frame_offset); break;
        case TACA_IMM: array_appendf(arr, " %zu", addr->imm); break;
        case TACA_ARG: array_appendf(arr, " %zu", addr->arg_offset); break;
        case TACA_REG: array_appendf(arr, " %s", register_to_string(addr->reg)); break;
        case TACA_REF: array_appendf(arr, " %zu", addr->ref); break;
        case TACA_ALABEL: array_appendf(arr, " %zu", addr->alabel); break;
        case TACA_LLABEL: array_appendf(arr, " %s", addr->literal); break;
        case TACA_PARAM: array_appendf(arr, " %zu", addr->param_offset); break;
        case TACA_NAME: array_appendf(arr, " \"%s\"", addr->name ? addr->name : "(null)"); break;
        case TACA_LNAME: array_appendf(arr, " \"%s\"", addr->name ? addr->name : "(null)"); break;
        case TACA_CONST: array_appendf(arr, " %zu", addr->const_idx); break;
        case TACA_VOID: break;
        default: array_appendf(arr, " unimplemented", addr); break;
    }
    array_push_byte(arr, ')');
}
