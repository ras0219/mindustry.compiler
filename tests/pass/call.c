struct A
{
    char buf[32];
};
struct A mm_call(struct A a);
struct A mi_call(int a);
int im_call(struct A a);
int ii_call(int a);
extern void (*f)();
void main()
{
    ii_call(10);
    im_call(mm_call(mi_call(5)));
    f();
}
