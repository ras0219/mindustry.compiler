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
    array_push_byte(arr, '{');
    array_appends(arr, taco_to_string(tace->op));
    array_push_byte(arr, ',');
    array_push_byte(arr, ' ');
    debug_taca(arr, &tace->arg1);
    array_push_byte(arr, ',');
    array_push_byte(arr, ' ');
    debug_taca(arr, &tace->arg2);
    array_push_byte(arr, '}');
}

void debug_taca(Array* arr, const TACAddress* addr)
{
    array_push_byte(arr, '{');
    array_appends(arr, taca_to_string(addr->kind));
    if (addr->is_addr) array_appends(arr, ", .is_addr = 1");
    if (addr->sizing.width != 0)
    {
        array_appendf(arr, ", .sizing = %c, %d", addr->sizing.is_signed ? '1' : '0', addr->sizing.width);
    }
    switch (addr->kind)
    {
        case TACA_FRAME: array_appendf(arr, ", .frame_offset = %d}", addr->frame_offset); break;
        case TACA_IMM: array_appendf(arr, ", .imm = %zu}", addr->imm); break;
        case TACA_ARG: array_appendf(arr, ", .arg_offset = %zu}", addr->arg_offset); break;
        case TACA_REG: array_appendf(arr, ", .reg = %s}", register_to_string(addr->reg)); break;
        case TACA_REF: array_appendf(arr, ", .ref = %zu}", addr->ref); break;
        case TACA_ALABEL: array_appendf(arr, ", .alabel = %zu}", addr->alabel); break;
        case TACA_LLABEL: array_appendf(arr, ", .literal = %s}", addr->literal); break;
        case TACA_PARAM: array_appendf(arr, ", .param_offset = %zu}", addr->param_offset); break;
        case TACA_NAME: array_appendf(arr, ", .name = \"%s\"}", addr->name ? addr->name : "(null)"); break;
        case TACA_LNAME: array_appendf(arr, ", .name = \"%s\"}", addr->name ? addr->name : "(null)"); break;
        case TACA_CONST: array_appendf(arr, ", .const_idx = %zu}", addr->const_idx); break;
        case TACA_VOID: array_push_byte(arr, '}'); break;
        default: array_appendf(arr, ", unimplemented = %p}", addr); break;
    }
}
