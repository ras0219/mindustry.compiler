#include "mindustry.h"

const int __attribute__((sym("message1"))) message1;
const int __attribute__((sym("\"waiting...\""))) waiting_msg;
const int __attribute__((sym("\": \""))) qty_msg;
const int __attribute__((sym("nucleus1"))) core;
const int __attribute__((sym("unloader1"))) unloader;
const int __attribute__((sym("conveyor1"))) conveyor;

int __attribute__((nonreentrant)) check_send(int mat)
{
    int n = sensor(core, mat);
    if (n > 11000)
    {
        control_enabled(conveyor, 1);
        control_configure(unloader, mat);
        print(mat);
        print(qty_msg);
        print(n);
        print_flush(message1);
        return 1;
    }
    return 0;
}

void __attribute__((nonreentrant)) main()
{
    if (check_send(thorium) > 0)
        ;
    else if (check_send(graphite) > 0)
        ;
    else if (check_send(silicon) > 0)
        ;
    else if (check_send(metaglass) > 0)
        ;
    else if (check_send(titanium) > 0)
        ;
    else if (check_send(lead) > 0)
        ;
    else if (check_send(copper) > 0)
        ;
    else
    {
        print(waiting_msg);
        print_flush(message1);
        control_enabled(conveyor, 0);
    }
}
