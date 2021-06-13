#include "mindustry.h"

void main()
{
    while (1)
    {
        unit_bind_next(unit_nova);
        int max_range = 0;
        if (sensor(bound_unit, sense_health) * 100 / sensor(bound_unit, sense_max_health) < 50)
        {
            unit_move(this_x, this_y);
            unit_boost(1);
        }
        else
        {
            struct unit* enemy = unit_radar(radar_filter_enemy, radar_filter_flying, radar_filter_any);
            if (enemy != null)
            {
                unit_boost(0);
                int x = sensor_x(enemy);
                int y = sensor_y(enemy);
                unit_targetp(enemy, 1);
                unit_move(x, y);
            }
            else
            {
                unit_boost(1);
                unit_approach(this_x, this_y, 5);
            }
        }

        unit_bind_next(unit_flare);
        struct unit* enemy = unit_radar(radar_filter_enemy, radar_filter_flying, radar_filter_any);
        if (enemy != null)
        {
            unit_targetp(enemy, 1);
        }
        else
        {
            unit_target(0, 0, 0);
            if (!unit_within(this_x, this_y, 20))
            {
                unit_approach(this_x, this_y, 10);
            }
        }
    }
}