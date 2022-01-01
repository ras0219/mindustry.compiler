#pragma once

struct StringPool;

struct StringPoolView
{
    const char* data;
};

struct StringPoolView sp_view(struct StringPool* sp);
__forceinline const char* sp_get(struct StringPoolView sv, size_t index) { return sv.data + index; }
