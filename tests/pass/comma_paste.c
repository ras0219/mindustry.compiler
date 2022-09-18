#define X(...) int z, ##__VA_ARGS__;
struct
{
    X()
};
struct
{
    X(a, b)
};
