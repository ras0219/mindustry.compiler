int arr1[sizeof(int) * 8];
int arr2[1 + 1024 % (sizeof(int) * 8)];
int arr3[1024 / (sizeof(int) * 8)];
int arr4[1024 == 1024];
int arr5[10 ? 20 : 30];
typedef int int_t;
int arr6[sizeof(int_t) % 10];
#define __DARWIN_howmany(x, y) ((((x) % (y)) == 0) ? ((x) / (y)) : (((x) / (y)) + 1)) /* y's == x bits? */

typedef struct fd_set
{
    int fds_bits[__DARWIN_howmany(1024, sizeof(int) * 8)];
} fd_set;
