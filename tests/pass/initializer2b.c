struct example
{
    struct addr_t
    {
        int port;
    } addr;
    union
    {
        char a8[4];
        short a16[2];
    } in_u;
};
struct addr_t ex0 = {80};
struct example ex1 = {
    {80},
    {{127, 0, 0, 1}},
};
struct example ex2 = {80, 127, 0, 0, 1};
