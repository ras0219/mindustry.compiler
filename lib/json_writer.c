#include "json_writer.h"

#include <string.h>

// `in` must be no larger than 99,999,999
static uint32_t double_dabble_8(uint32_t in)
{
    uint64_t reg = in;
    reg <<= 3;
    for (size_t i = 3; i < 32; ++i)
    {
        uint64_t r2 = reg + 0x3333333300000000ULL;
        uint64_t r3 = (~r2) & 0x8888888800000000ULL;
        reg = r2 - (r3 >> 2) - (r3 >> 3);
        reg <<= 1;
    }
    return reg >> 32;
}
static void bcd_unpack_8(uint32_t in, char* out)
{
    for (int i = 7; i >= 0; --i)
    {
        out[i] = (in & 0xF) + '0';
        in >>= 4;
    }
}
static void double_dabble_24_unpack(uint64_t in, char* buf)
{
    bcd_unpack_8(double_dabble_8(in % 100000000ULL), buf + 16);
    in /= 100000000ULL;
    bcd_unpack_8(double_dabble_8(in % 100000000ULL), buf + 8);
    in /= 100000000ULL;
    bcd_unpack_8(double_dabble_8(in % 100000000ULL), buf);
}

size_t json_write_u64(char* buf, size_t buf_sz, uint64_t x)
{
    if (x == 0)
    {
        if (buf_sz) buf[0] = '0';
        return 1;
    }
    char tmp[24];
    double_dabble_24_unpack(x, tmp);
    size_t i = 0;
    while (tmp[i] == '0')
        ++i;
    size_t sz = 24 - i;
    buf_sz = buf_sz < sz ? buf_sz : sz;
    memcpy(buf, tmp + i, buf_sz);
    return sz;
}
size_t json_write_i64(char* buf, size_t buf_sz, int64_t x)
{
    if (x >= 0) return json_write_u64(buf, buf_sz, x);
    if (buf_sz) buf[0] = '-';
    return json_write_u64(buf + 1, buf_sz ? buf_sz - 1 : 0, -(uint64_t)x) + 1;
}

static const unsigned char s_escape_class[128] = {
    [0x00] = 0xFF,
    [0x01] = 0xFF,
    [0x02] = 0xFF,
    [0x03] = 0xFF,
    [0x04] = 0x21, // \t == 9, \b == 8
    [0x05] = 0xF3, // \n == 10
    [0x06] = 0x54, // \r == 13, \f == 12
    [0x07] = 0xFF,
    [0x08] = 0xFF,
    [0x09] = 0xFF,
    [0x0A] = 0xFF,
    [0x0B] = 0xFF,
    [0x0C] = 0xFF,
    [0x0D] = 0xFF,
    [0x0E] = 0xFF,
    [0x0F] = 0xFF,
    ['"' / 2] = 0x06,  // " == 34
    ['\\' / 2] = 0x07, // \ == 92
};

static const char s_escape_char[] = {'b', 't', 'n', 'f', 'r', '"', '\\'};

#define TO_HEX(x) ((x) < 10 ? '0' + (x) : 'A' + (x)-10)

size_t json_write_string(char* buf, size_t buf_sz, const char* s, size_t n)
{
    if (buf_sz) buf[0] = '"';
    size_t r = 1;
    for (size_t i = 0; i < n; ++i)
    {
        unsigned char ch = s[i];
        unsigned char cl = s_escape_class[ch >> 1];
        cl >>= (ch & 1) << 2;
        cl &= 0xF;
        if (cl == 0)
        {
            if (r < buf_sz) buf[r] = ch;
            ++r;
        }
        else
        {
            if (r < buf_sz) buf[r] = '\\';
            ++r;
            if (cl == 0xF)
            {
                unsigned char ch1 = (ch & 0xF0) >> 4;
                unsigned char ch2 = ch & 0x0F;
                const char seq[] = {'u', '0', '0', TO_HEX(ch1), TO_HEX(ch2)};
                if (r < buf_sz) switch (buf_sz - r)
                    {
                        default: buf[r + 4] = seq[4];
                        case 4: buf[r + 3] = seq[3];
                        case 3: buf[r + 2] = seq[2];
                        case 2: buf[r + 1] = seq[1];
                        case 1: buf[r] = seq[0];
                    }
                r += 5;
            }
            else
            {
                if (r < buf_sz) buf[r] = s_escape_char[cl - 1];
                ++r;
            }
        }
    }
    if (r < buf_sz) buf[r] = '"';
    ++r;
    return r;
}
