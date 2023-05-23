struct S
{
    char d[3];
};

extern struct S a, b, *c;

extern void dbl(void);
static void dbl2(void) { }
void dbl3(void);

int square(int num)
{
    a = b;

    *c = b;
    dbl();
    dbl2();
    dbl3();
}
