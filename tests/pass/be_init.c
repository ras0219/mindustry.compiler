struct S
{
    int x, y, z, w;
};
struct A
{
    struct S s1, s2;
};

struct W
{
    const char* s;
    char buf[128];
};

void foo(struct W* p)
{
    struct A s1 = {0};
    struct W w = {
        .s = p->buf,
    };
}
