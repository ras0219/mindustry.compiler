void f();
void (*p_f)() = f;
void (*p_f2)() = (void (*)())f;
enum
{
    A = 0xffff & ~(1 << 5)
};
