#pragma once

#include "stddef.h"

#if defined(__cplusplus)
extern "C"
{
#endif

    struct _IO_FILE;
    typedef struct _IO_FILE FILE;

    extern FILE* fopen(const char* __restrict filename, const char* __restrict modes)
        __attribute__((__warn_unused_result__));
    extern int fclose(FILE* stream);
    extern int fflush(FILE* stream);

    extern int printf(const char* __restrict __fmt, ...);

    extern int fprintf(FILE* __restrict __stream, const char* __restrict __format, ...);

    extern int snprintf(char* __restrict __s, size_t __maxlen, const char* __restrict __format, ...)
        __attribute__((__format__(__printf__, 3, 4)));

    extern int vfprintf(FILE* __restrict __s, const char* __restrict __format, __builtin_va_list __arg);

    extern int vsnprintf(char* __restrict __s,
                         size_t __maxlen,
                         const char* __restrict __format,
                         __builtin_va_list __arg) __attribute__((__format__(__printf__, 3, 0)));

    extern size_t fwrite(const void* __restrict __ptr, size_t __size, size_t __n, FILE* __restrict __s);

    extern size_t fread(void* __restrict __ptr, size_t __size, size_t __n, FILE* __restrict __stream)
        __attribute__((__warn_unused_result__));

    extern int sscanf(const char* __restrict __s, const char* __restrict __format, ...);

    extern int atoi(const char* __nptr) __attribute__((pure)) __attribute__((__nonnull__(1)))
    __attribute__((__warn_unused_result__));
    extern void perror(const char* __s);

    extern int fputs(const char* __restrict __s, FILE* __restrict __stream);
    extern int fputc(char ch, FILE* __stream);

    extern int puts(const char* __s);
    extern int putchar(char ch);

    extern FILE* stdin;
    extern FILE* stdout;
    extern FILE* stderr;
#define stdin stdin
#define stdout stdout
#define stderr stderr

#if defined(__cplusplus)
}
#endif
