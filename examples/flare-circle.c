#include "mindustry.h"

#pragma memory memory1

void main()
{
    int radius = 50;
    struct unit* enemy;
    while (1)
    {
        struct unit* u0 = null;
        int d = 0;
        int unit_count = 1;
        int loops = 0;
        while (1)
        {
            unit_bind_next(unit_flare);
            if (u0 == null)
            {
                u0 = bound_unit;
                loops = 0;
            }
            else if (u0 == bound_unit)
            {
                unit_count = d;
                d = 0;
            }
            int degrees = (d % 4) * 90 + loops / radius;
            unit_move(this_x + cos(degrees) * radius, this_y + sin(degrees) * radius);

            enemy = unit_radar(radar_filter_enemy, radar_filter_any, radar_filter_any);
            while (enemy != null && !sensor(enemy, sense_dead))
            {
                unit_bind_next(unit_flare);
                int x = sensor_x(enemy);
                int y = sensor_y(enemy);
                unit_move(x, y);
                unit_targetp(enemy, 1);
            }
            d = d + 1;

            loops = loops + 10 + unit_count * 2;
        }
    }
}
