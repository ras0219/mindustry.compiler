#pragma once

#include <stdint.h>
#include <stdio.h>

#include "fwd.h"

typedef struct Checker Checker;

Checker* checker_alloc(const struct Elaborator* elab);
int checker_check(Checker* chk);
void checker_free(Checker* chk);
