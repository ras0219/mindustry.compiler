#include "string.h"

void* memset(void* dst, int value, size_t size)
{
    char* char_buffer = (char*)dst;
    for (int i = 0; i < size; ++i)
    {
        char_buffer[i] = value;
    }
    return dst;
}

void* memcpy(void* __restrict dst, const void* __restrict src, size_t size)
{
    char* char_dst = (char*)dst;
    char* char_src = (char*)src;
    for (size_t i = 0; i < size; ++i)
    {
        char_dst[i] = char_src[i];
    }
    return dst;
}

int memcmp(const void* buf1, const void* buf2, size_t sz)
{
    const char* b1 = (const char*)buf1;
    const char* b2 = (const char*)buf2;
    while (sz--)
    {
        int diff = ((int)*b1++) - ((int)*b2++);
        if (diff) return diff;
    }
    return 0;
}

void* memchr(const void* _s, int c, size_t n)
{
    const char* s = _s;
    for (size_t i = 0; i < n; ++i)
    {
        if (s[i] == c) return (void*)(s + i);
    }
    return NULL;
}
char* strcpy(char* __restrict dst, const char* __restrict src)
{
    while ((*dst++ = *src++))
        ;
    return dst - 1;
}

int strcmp(const char* __s1, const char* __s2)
{
    while (1)
    {
        char c1 = *__s1++;
        char c2 = *__s2++;
        int diff = c2 - c1;
        if (diff) return diff < 0 ? -1 : 1;
        if (!c1) return 0;
    }
}
int strncmp(const char* __s1, const char* __s2, size_t __n)
{
    for (size_t i = 0; i < __n; ++i)
    {
        char c1 = __s1[i];
        char c2 = __s2[i];
        int diff = c2 - c1;
        if (diff) return diff < 0 ? -1 : 1;
        if (!c1) return 0;
    }
    return 0;
}

size_t strlen(const char* __s)
{
    int i = 0;
    while (__s[i++])
        ;
    return i - 1;
}