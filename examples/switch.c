#include "mindustry.h"

void main()
{
    while (1)
    {
        int enabled = 0;
        for (int i = 0; i < num_links; i++)
        {
            struct unit* block = get_link(i);
            struct unittype* type = sensor_type(block);
            if (type == type_switch) enabled = enabled || sensor(block, sense_enabled);
        }
        for (int i = 0; i < num_links; i++)
        {
            struct unit* block = get_link(i);
            struct unittype* type = sensor_type(block);
            if (type != type_switch)
            {
                control_enabled(block, enabled);
            }
        }
    }
}
