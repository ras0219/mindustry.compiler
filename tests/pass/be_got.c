void (*g)();
extern void h();
extern void (*i)();
void f()
{
    f();
    g();
    h();
    i();

    g = f;
    g = g;
    g = h;
    g = i;
}