#pragma once

#if defined(__wasm)
namespace std
{
    template<class T>
    struct remove_reference
    {
        using type = T;
    };
    template<class T>
    struct remove_reference<T&>
    {
        using type = T;
    };
    template<class T>
    struct remove_reference<T&&>
    {
        using type = T;
    };
    template<class T>
    using remove_reference_t = typename remove_reference<T>::type;

    template<bool, class T = void>
    struct enable_if
    {
    };

    template<class T>
    struct enable_if<true, T>
    {
        using type = T;
    };

    template<bool Test, class T = void>
    using enable_if_t = typename enable_if<Test, T>::type;

    template<class T>
    constexpr T&& forward(remove_reference_t<T>& t) noexcept
    {
        return static_cast<T&&>(t);
    }

    template<class T>
    constexpr T&& forward(remove_reference_t<T>&& t) noexcept
    {
        return static_cast<T&&>(t);
    }

    template<class T>
    constexpr remove_reference_t<T>&& move(T&& t) noexcept
    {
        return static_cast<remove_reference_t<T>&&>(t);
    }

    template<class T>
    constexpr T&& declval() noexcept;

    template<class T>
    struct type_identity
    {
        using type = T;
    };

    template<class T>
    using type_identity_t = typename type_identity<T>::type;
} // namespace std
#else
#include <type_traits>
#endif
