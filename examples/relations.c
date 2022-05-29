#include "stdio.h"

int main()
{
#define FOR_OP(op)                                                                                                     \
    printf(#op "\n   0 1 2\n +------\n");                                                                              \
    for (int x = 0; x < 3; ++x)                                                                                        \
    {                                                                                                                  \
        printf("%d|", x);                                                                                              \
        for (int y = 0; y < 3; ++y)                                                                                    \
        {                                                                                                              \
            printf(" %d", x op y);                                                                                     \
        }                                                                                                              \
        printf("\n");                                                                                                  \
    }

    FOR_OP(==)
    FOR_OP(!=)
    FOR_OP(<)
    FOR_OP(>)
    FOR_OP(<=)
    FOR_OP(>=)

    return 0;
}
