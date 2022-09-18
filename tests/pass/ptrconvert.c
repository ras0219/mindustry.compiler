typedef int Int;
int bar(int* i);
int foo(const Int* i);
int main()
{
    int* x = (void*)0;
    foo(x);
    bar(x);
    const int* y = (void*)0;
    foo(y);
}
