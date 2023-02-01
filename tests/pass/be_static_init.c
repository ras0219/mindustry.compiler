enum
{
    v1 = 2
};
static int data[] = {1, v1, 3};
static const char* const s_reg_names[] = {"%rax", "%rbx"};
extern void bar() { }
static void foo() { }
static void* ptrs[] = {foo, bar};
