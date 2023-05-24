int foo();
struct Arr
{
    int y;
};
const struct Arr arr[] = {1, 2, 3};
extern const struct Arr arr[];
struct Foo
{
    int x;
    int y;
};
int main()
{
    ((struct Foo*)0)->y;
    const struct Arr *p = arr, *q = &arr;
    p = arr;
    p = &arr;
    sizeof(arr);
}
