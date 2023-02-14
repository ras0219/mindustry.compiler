#include "stdio.h"

int main()
{
#define FOR_OP(op)                                                                                                     \
    printf(#op "\n   0 1 2 3\n +--------\n");                                                                          \
    for (int x = 0; x < 4; ++x)                                                                                        \
    {                                                                                                                  \
        printf("%d|", x);                                                                                              \
        for (int y = 0; y < 4; ++y)                                                                                    \
        {                                                                                                              \
            printf(" %d", x op y);                                                                                     \
        }                                                                                                              \
        printf("\n");                                                                                                  \
    }                                                                                                                  \
    fflush(NULL);

    FOR_OP(==)
    FOR_OP(!=)
    FOR_OP(<)
    FOR_OP(>)
    FOR_OP(<=)
    FOR_OP(>=)
    FOR_OP(|)
    FOR_OP(&)
    FOR_OP(^)
    FOR_OP(+)
    FOR_OP(*)

    return 0;
}
