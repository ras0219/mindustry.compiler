#include "mindustry.h"

int building_radius(struct unittype* bld)
{
    if (bld == type_duo) return 0;
    if (bld == type_spectre) return 2;
    return 1;
}

void main()
{
    while (1)
    {
        unit_bind_next(unit_fortress);
        int max_range = 28;
        if (sensor(bound_unit, sense_health) * 100 / sensor(bound_unit, sense_max_health) < 50)
        {
            max_range = 50;
        }
        struct unit* enemy = unit_radar(radar_filter_enemy, radar_filter_ground, radar_filter_any);
        if (enemy == null)
        {
            // enemy = unit_radar(radar_filter_enemy, radar_filter_any, radar_filter_any);
        }
        if (enemy != null)
        {
            int x = sensor_x(enemy);
            int y = sensor_y(enemy);
            unit_targetp(enemy, 1);
            unit_approach(x, y, max_range);
            continue;
        }
        int mx = sensor_x(bound_unit);
        int my = sensor_y(bound_unit);
        if (unit_locate_turret() == 1 && unit_within(unit_locate_outx, unit_locate_outy, 40))
        {
            struct unit* turret = unit_get_block(unit_locate_outx, unit_locate_outy);
            struct unittype* turret_type = sensor_type(turret);
            int rad = building_radius(turret_type);
            int dx = mx - unit_locate_outx;
            int dy = my - unit_locate_outy;
            int dd = sqrt(dx * dx + dy * dy);
            int tx = unit_locate_outx + dx * rad / dd;
            int ty = unit_locate_outy + dy * rad / dd;
            unit_target(tx, ty, 1);
            unit_approach(tx, ty, max_range);
            continue;
        }
        if (unit_locate_core(bool_true) == 1 && unit_within(unit_locate_outx, unit_locate_outy, 40))
        {
            struct unit* turret = unit_get_block(unit_locate_outx, unit_locate_outy);
            struct unittype* turret_type = sensor_type(turret);
            max_range += building_radius(turret_type);
            unit_target(unit_locate_outx, unit_locate_outy, 1);
            unit_approach(unit_locate_outx, unit_locate_outy, max_range);
            continue;
        }

        unit_target(0, 0, 0);
        unit_pathfind();
    }
}