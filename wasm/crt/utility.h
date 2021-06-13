#pragma once

#if defined(__wasm)
namespace std
{
    template<class T>
    void swap(T& a, T& b)
    {
        T t = std::move(a);
        a = std::move(b);
        b = std::move(t);
    }
} // namespace std
#else
#include <utility>
#endif
