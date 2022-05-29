#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"

int lit_to_uint64(const char* s, uint64_t* out, const struct RowCol* rc)
{
    uint64_t v = 0;
    size_t i = 0;
    if (s[0] == '0')
    {
        // octal, hex, binary, or zero
        if (s[1] == 'x' || s[1] == 'X')
        {
            // hex
            for (i = 2; s[i]; ++i)
            {
                v = (((v & (UINT64_MAX >> 4))) << 4);
                if (s[i] >= '0' && s[i] <= '9')
                    v += s[i] - '0';
                else if (s[i] >= 'a' && s[i] <= 'f')
                    v += s[i] - 'a' + 10;
                else if (s[i] >= 'A' && s[i] <= 'F')
                    v += s[i] - 'A' + 10;
                else
                    break;
            }
        }
        else if (s[1] == 'b' || s[1] == 'B')
        {
            // binary
            for (i = 2; s[i]; ++i)
            {
                v = (((v & (UINT64_MAX >> 1))) << 1);
                if (s[i] == '0')
                    ;
                else if (s[i] == '1')
                    v += 1;
                else
                    break;
            }
        }
        else if (s[1] >= '0' && s[1] <= '9')
        {
            // octal
            for (i = 1; s[i]; ++i)
            {
                v = (((v & (UINT64_MAX >> 3))) << 3);
                if (s[i] >= '0' && s[i] <= '7')
                    v += s[i] - '0';
                else
                    break;
            }
        }
        else if (s[1] == '\0')
        {
            i = 1;
        }
    }
    else
    {
        for (; s[i]; ++i)
        {
            if (s[i] >= '0' && s[i] <= '9')
            {
                size_t w = s[i] - '0';
                if ((UINT64_MAX - w) / 10 < v) abort();
                v = v * 10 + w;
            }
            else
                break;
        }
    }
    if (s[i] == 'U')
    {
        ++i;
    }
    if (s[i] == 'L' || s[i] == 'l')
    {
        ++i;
    }
    if (s[i] == 'L' || s[i] == 'l')
    {
        ++i;
    }
    if (s[i] != '\0')
    {
        return printf("error: unexpected character in number literal: '%c'\n", s[i]);
    }

    *out = v;
    return 0;
}

#define str2(x) #x
#define str(x) str2(x)

int main()
{
    uint64_t u = 0;
    int rc = lit_to_uint64("123", &u, NULL);
    printf("parse(123, %llu)=%d\n", u, rc);
    rc = lit_to_uint64("1", &u, NULL);
    printf("parse(1, %llu)=%d\n", u, rc);
    rc = lit_to_uint64("12", &u, NULL);
    printf("parse(12, %llu)=%d\n", u, rc);
    rc = lit_to_uint64(str(UINT64_MAX), &u, NULL);
    printf("parse(" str(UINT64_MAX) ", %llu)=%d\n", u, rc);
    return 0;
}