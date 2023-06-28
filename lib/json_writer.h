#pragma once

#include <stdint.h>
#include <stdlib.h>

#include "compilermacros.h"

size_t json_write_u64(char* buf, size_t buf_sz, uint64_t i);
size_t json_write_i64(char* buf, size_t buf_sz, int64_t i);
size_t json_write_string(char* buf, size_t buf_sz, const char* s, size_t n);
