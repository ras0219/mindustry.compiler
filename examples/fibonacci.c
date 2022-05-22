#include "stdio.h"

int fibonacci(int x)
{
    if (x < 3) return 1;
    return fibonacci(x - 1) + fibonacci(x - 2);
}

int main()
{
    for (int x = 1; x < 10; ++x)
    {
        printf("%4d: %d\n", x, fibonacci(x));
    }
    return 0;
}
