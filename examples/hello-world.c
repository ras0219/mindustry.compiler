#include "mindustry.h"

struct unit __attribute__((sym("message1")))* message1;

int main() {
    print("hello, world!");
    print_flush(message1);
    return 1;
}
