#pragma once

#include "array.h"

void maybe_append_pathsep(Array* buf);
void path_combine(Array* buf, const char* e1, size_t s1);
// Null-terminates
void assign_path_join(Array* buf, const char* e1, size_t s1, const char* e2, size_t s2);
