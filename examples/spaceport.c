int __attribute__((asmstr("sensor %r %0 %1"))) sensor(int dst, int sense);
int __attribute__((asmstr("control configure %0 %1 0 0 0"))) control_configure(int dst, int mat);
/* control the enabled state of 'tgt' */
void __attribute__((asmstr("control enabled %0 %1 0 0 0"))) control_enabled(int tgt, int enabled);

/* queue 'str' to the print buffer */
void __attribute__((asmstr("print %0"))) print(int str);

/* flush the print buffer to 'device' */
void __attribute__((asmstr("printflush %0"))) print_flush(int device);

const int __attribute__((sym("@thorium"))) thorium;
const int __attribute__((sym("@lead"))) lead;
const int __attribute__((sym("@graphite"))) graphite;
const int __attribute__((sym("@copper"))) copper;
const int __attribute__((sym("@silicon"))) silicon;
const int __attribute__((sym("@titanium"))) titanium;
const int __attribute__((sym("@metaglass"))) metaglass;

const int __attribute__((sym("message1"))) message1;
const int __attribute__((sym("\"waiting...\""))) waiting_msg;
const int __attribute__((sym("\": \""))) qty_msg;
const int __attribute__((sym("nucleus1"))) core;
const int __attribute__((sym("unloader1"))) unloader;
const int __attribute__((sym("conveyor1"))) conveyor;

int check_send(int mat)
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

void main()
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
