#include "symbol.h"

struct Decl* decl_get_def(struct Decl* decl)
{
    while (decl->def)
        decl = decl->def;
    return decl;
}
