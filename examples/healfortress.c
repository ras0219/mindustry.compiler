#include "mindustry.h"

struct unit* message;

void main()
{
    struct unit* my_mega = null;
    while (1)
    {
        while (1)
        {
            message = null;
            for (int i = 0; i < num_links; i++)
            {
                struct unit* block = get_link(i);
                struct unittype* type = sensor_type(block);
                if (type == type_message) message = block;
            }
            if (my_mega == null)
            {
                unit_bind_next(unit_mega);
                if (bound_unit == null)
                {
                    print("ERROR: Binding poly...");
                    print_flush(message);
                    continue;
                }

                int flag = sensor(bound_unit, sense_flag);
                if (flag != 0)
                {
                    print("ERROR: Poly in use...");
                    print_flush(message);
                    unit_bind_next(unit_mega);
                    continue;
                }
                unit_flag(1);
                my_mega = bound_unit;
            }

            print("Bound poly...");
            print_flush(message);
            break;
        }

        unit_approach(this_x, this_y, 10);
        while (1)
        {
            if (my_mega == null) break;
            while (1)
            {
                if (my_mega == null) break;
                unit_bind_next(unit_fortress);

                struct unit* fortress = bound_unit;
                while (sensor(fortress, sense_health) < sensor(fortress, sense_max_health))
                {
                    while (1)
                    {
                        unit_bind_next(unit_mega);
                        if (my_mega == null) break;
                        if (bound_unit == my_mega) break;
                    }
                    int x = sensor_x(fortress);
                    int y = sensor_y(fortress);
                    unit_approach(x, y, 10);
                    unit_targetp(fortress, 1);
                }
            }
        }
    }
}