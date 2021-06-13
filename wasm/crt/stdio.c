#include "stdio.h"

#include "limits.h"
#include "stdarg.h"
#include "stdlib.h"
#include "string.h"

FILE* stdin = (void*)1;
FILE* stdout = (void*)2;
FILE* stderr = (void*)3;

struct _IO_FILE
{
    char* buf;
    size_t sz;
};

static char s_space_pad[32] = {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
                               ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '};
static char s_zero_pad[32] = {'0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0',
                              '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'};

static void putchar_repeat(FILE* __restrict out, char pad[32], size_t n)
{
    while (n >= 32)
    {
        fwrite(pad, 1, 32, out);
        n -= 32;
    }
    if (n > 0)
    {
        fwrite(pad, 1, n, out);
    }
}

// characters are filled from the end
static int format_uint_buf(char buf[20], unsigned int n)
{
    if (n == 0)
    {
        buf[19] = '0';
        return 1;
    }
    // Max unsigned 64-bit number: 18446744073709551615, or 20 digits
    char* cur = buf + 20;
    do
    {
        --cur;
        *cur = (char)('0' + n % 10);
        n /= 10;
    } while (n > 0);
    return buf + 20 - cur;
}
static int format_uint(FILE* __restrict out, unsigned int n, int zero_pad, int min_width)
{
    char buf[20];
    int w = format_uint_buf(buf, n);
    if (w < min_width) putchar_repeat(out, zero_pad ? s_zero_pad : s_space_pad, min_width - w);
    fwrite(buf + 20 - w, 1, w, out);
    return w > min_width ? w : min_width;
}

static int format_int(FILE* __restrict out, int n, int zero_pad, int min_width)
{
    if (n < 0)
    {
        char buf[20];
        int w = format_uint_buf(buf, 1 + (unsigned int)(-(n + 1)));
        if (zero_pad)
        {
            fputc('-', out);
            if (w + 1 < min_width) putchar_repeat(out, s_zero_pad, min_width - (w + 1));
        }
        else
        {
            if (w + 1 < min_width) putchar_repeat(out, s_space_pad, min_width - (w + 1));
            fputc('-', out);
        }
        fwrite(buf + 20 - w, 1, w, out);
        return w + 1 > min_width ? w + 1 : min_width;
    }
    else
    {
        return format_uint(out, (unsigned int)n, zero_pad, min_width);
    }
}

static char to16hex(unsigned char c) { return c < 10 ? '0' + c : 'a' + c - 10; }

// characters are filled from the end
static int format_hex_buf(char buf[sizeof(unsigned int) * 2], unsigned int n)
{
    char* dst = buf + sizeof(n) * 2;
    if (n == 0)
    {
        *--dst = '0';
    }
    else
    {
        for (int i = 0; i < sizeof(n) * 2; ++i)
        {
            *--dst = to16hex((unsigned char)(n & 0x0F));
            n >>= 4;
            if (n == 0) break;
        }
    }

    return buf + sizeof(n) * 2 - dst;
}

static int format_hex(FILE* __restrict out, unsigned int n, int zero_pad, int min_width)
{
    char buf[sizeof(unsigned int) * 2];
    int w = format_hex_buf(buf, n);
    if (w < min_width) putchar_repeat(out, zero_pad ? s_zero_pad : s_space_pad, min_width - w);
    fwrite(buf + sizeof(buf) - w, 1, w, out);
    return w > min_width ? w : min_width;
}

extern FILE* fopen_js(const char* filename, size_t filename_len);
extern int fclose_js(FILE* stream);

FILE* fopen(const char* __restrict filename, const char* __restrict modes)
{
    size_t fn_len = strlen(filename);
    return fopen_js(filename, fn_len);
}
int fclose(FILE* stream) { return fclose_js(stream); }
int fflush(FILE* f) { return 0; }

int vfprintf(FILE* __restrict stream, const char* __restrict format, __builtin_va_list argp)
{
    int written = 0;
    while (*format != '\0')
    {
        if (*format == '%')
        {
            format++;
            int zero_pad = 0;
            int width = 0;
            char length = '\0';
            if (*format == '0')
            {
                zero_pad = 1;
                format++;
            }
            if (*format >= '0' && *format <= '9')
            {
                width = *format++ - '0';
            }
            if (*format == 'z')
            {
                length = *format++;
            }

            if (*format == '%')
            {
                written++;
                fputc('%', stream);
            }
            else if (*format == 'c')
            {
                char char_to_print = va_arg(argp, int);
                fputc(char_to_print, stream);
                written++;
            }
            else if (*format == 'd')
            {
                int i = va_arg(argp, int);
                written += format_int(stream, i, zero_pad, width);
            }
            else if (*format == 'u')
            {
                unsigned int i = va_arg(argp, unsigned int);
                written += format_uint(stream, i, zero_pad, width);
            }
            else if (*format == 'x')
            {
                unsigned int i = va_arg(argp, unsigned int);
                written += format_hex(stream, i, zero_pad, width);
            }
            else if (*format == 's')
            {
                const char* s = va_arg(argp, const char*);
                size_t len = strlen(s);
                fwrite(s, 1, len, stream);
                written += len;
            }
            else if (*format == '.')
            {
                if (*++format == '*' && *++format == 's')
                {
                    int i = va_arg(argp, int);
                    const char* s = va_arg(argp, const char*);
                    size_t len = 0;
                    for (; s[len] && len < i; len++)
                        ;
                    fwrite(s, 1, len, stream);
                    written += len;
                }
                else
                {
                    abort();
                }
            }
            else
            {
                puts("Not implemented: %");
                fputc(*format, stdout);
                abort();
            }
        }
        else
        {
            fputc(*format, stream);
            written++;
        }
        format++;
    }
    return written;
}

int printf(const char* __restrict format, ...)
{
    va_list argp;
    va_start(argp, format);
    int rc = vfprintf(stdout, format, argp);
    va_end(argp);
    return rc;
}

int fprintf(FILE* __restrict stream, const char* __restrict format, ...)
{
    va_list argp;
    va_start(argp, format);
    int rc = vfprintf(stream, format, argp);
    va_end(argp);
    return rc;
}

int snprintf(char* __restrict s, size_t maxlen, const char* __restrict format, ...)
{
    va_list argp;
    va_start(argp, format);
    int rc = vsnprintf(s, maxlen, format, argp);
    va_end(argp);
    return rc;
}

int vsnprintf(char* __restrict s, size_t maxlen, const char* __restrict format, __builtin_va_list argp)
{
    struct _IO_FILE f = {s, maxlen};
    if (maxlen == 0) return vfprintf(&f, format, argp);
    f.sz--;
    int rc = vfprintf(&f, format, argp);
    s[rc >= maxlen ? maxlen - 1 : rc] = '\0';
    return rc;
}

extern size_t fread_js(char* data, size_t sz, size_t n, FILE* fd);

size_t fread(void* __restrict p, size_t sz, size_t n, FILE* __restrict stream)
{
    if (64 > (size_t)stream)
    {
        return fread_js(p, sz, n, stream);
    }
    else
    {
        size_t m = stream->sz / sz;
        if (m > n) m = n;
        memcpy(p, stream->buf, m * sz);
        stream->buf += m * sz;
        stream->sz -= m * sz;
        return m;
    }
}

extern void fwrite_js(const char* data, size_t bytes, FILE* fd);

size_t fwrite(const void* __restrict p, size_t sz, size_t n, FILE* __restrict stream)
{
    if (64 > (size_t)stream)
    {
        fwrite_js(p, sz * n, stream);
        return n;
    }
    else
    {
        size_t m = stream->sz / sz;
        if (m > n) m = n;
        memcpy(stream->buf, p, m * sz);
        stream->buf += m * sz;
        stream->sz -= m * sz;
        return m;
    }
}

int vsscanf(const char* __restrict s, const char* __restrict fmt, __builtin_va_list argp)
{
    int written = 0;
    while (*fmt != '\0' && *s != '\0')
    {
        if (*fmt == '%')
        {
            fmt++;
            if (*fmt == 'x')
            {
                fmt++;
                written++;
                const char* s_start = s;
                char ch = *s;
                unsigned int v = 0;
                while (ch)
                {
                    if (ch >= '0' && ch <= '9')
                    {
                        v = (v << 4) + (ch - '0');
                        ch = *++s;
                    }
                    else if (ch >= 'a' && ch <= 'f')
                    {
                        v = (v << 4) + (ch - 'a') + 10;
                        ch = *++s;
                    }
                    else if (ch >= 'A' && ch <= 'F')
                    {
                        v = (v << 4) + (ch - 'A') + 10;
                        ch = *++s;
                    }
                    else
                        break;
                    if (v >= 0x10000000) break;
                }
                if (s == s_start) return written;
                unsigned int* p = va_arg(argp, unsigned int*);
                *p = v;
                ++written;
            }
            else
            {
                abort();
            }
        }
        else if (*fmt == *s)
        {
            fmt++;
            s++;
        }
        else
        {
            return written;
        }
    }
    return written;
}

int sscanf(const char* __restrict s, const char* __restrict fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);
    int rc = vsscanf(s, fmt, argp);
    va_end(argp);
    return rc;
}

int atoi(const char* __nptr)
{
    _Bool minus = 0;
    int ret = 0;
    char ch = *__nptr;
    if (ch == '-')
    {
        ++__nptr;
        minus = 1;
    }
    while (ch = *__nptr++)
    {
        if (ch >= '0' && ch <= '9')
        {
            ret = ret * 10 + (ch - '0');
        }
        if (ret > (INT_MAX - 10) / 10) break;
    }
    return minus ? -ret : ret;
}

void perror(const char* __s)
{
    fputs("error: ", stderr);
    fputs(__s, stderr);
}

int fputs(const char* __restrict __s, FILE* __restrict __stream) { return fwrite(__s, strlen(__s), 1, __stream); }
int fputc(char ch, FILE* __stream) { return fwrite(&ch, 1, 1, __stream); }

int puts(const char* __s) { return fputs(__s, stdout); }

int putchar(char ch) { return fputc(ch, stdout); }
