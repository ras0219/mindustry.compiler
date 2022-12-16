void foo()
{
    char txt6[] = "hello";
    struct Point
    {
        int x, y;
    } points5[] = {[3] = 1, 2, 3};
    char txt7[] = {"hello"};
    static const char mode[][5] = {"EPRT", "PORT"};
    struct Foo
    {
        int a;
        int last[];
    };
}

// char txt6[] = "hello";
// struct Point
// {
//     int x, y;
// } points5[] = {[3] = 1, 2, 3};
// char txt7[] = {"hello"};
// static const char mode[][5] = {"EPRT", "PORT"};
// struct Foo
// {
//     int a;
//     int last[];
// };
