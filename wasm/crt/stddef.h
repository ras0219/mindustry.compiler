#pragma once

typedef long ptrdiff_t;
typedef unsigned long size_t;
#define offsetof(t, d) __builtin_offsetof(t, d)

#define NULL ((void*)0)

#if defined(__cplusplus)
namespace std
{
    typedef decltype(__nullptr) nullptr_t;
}

using ::std::nullptr_t;
#endif