enum A
{
    a1 = 5,
    a2,
    a3 = a2 + 20
};
typedef struct
{
    enum A a;
} W;
int main()
{
    enum A x = a3;
    int y = (x == a2);
    if ((unsigned int)y == a1) sizeof(enum A);
    W w, *pw = &w;
    pw->a = a1;
}
struct N
{
    enum
    {
        N1 = 4
    } e;
};
char ch[N1];
