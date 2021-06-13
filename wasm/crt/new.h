#pragma once

#include "stddef.h"

[[nodiscard]] void* operator new(size_t sz);
void operator delete(void* p) noexcept;

[[nodiscard]] void* operator new[](size_t sz);
void operator delete[](void* p) noexcept;

[[nodiscard]] inline void* operator new(size_t, void* ptr) noexcept { return ptr; }
