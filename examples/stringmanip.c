#include "stdio.h"

typedef struct Buffer
{
    char* data;
    size_t sz;
    size_t cap;
} Buffer;

int main()
{
    int z = 0;
    Buffer buf = {0, z};
    for (int x = 0; x < sizeof(buf); ++x)
    {
        printf("%d: %d\n", x, ((char*)&buf)[x]);
    }
    return 0;
}
