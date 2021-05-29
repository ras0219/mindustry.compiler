#include "mindustry.h"

#define __string struct string

int base;
int base_x;
int base_y;
int message;

int async_unit_approach(int x, int y, int dist)
{
    int ux = sensor_x(bound_unit) - x;
    int uy = sensor_y(bound_unit) - y;
    int dist2 = dist + 1;
    int d = ux * ux + uy * uy > dist2 * dist2;
    if (d)
    {
        unit_approach(x, y, dist);
    }
    return d;
}
int async_purge_inventory()
{
    int r = sensor(bound_unit, sense_totalitems) > 0;
    if (r)
    {
        unit_drop_item(base, 30);
    }
    return r;
}

int __attribute__((nonreentrant)) fill(int bld, int mat, int minlvl)
{
    while (1)
    {
        if (bound_unit == null) return 1;
        int m = sensor(bld, mat);
        if (m >= minlvl) return 0;

        int s2 = sensor(bound_unit, mat);
        if (s2 > 0)
        {
            int x = sensor_x(bld);
            int y = sensor_y(bld);
            if (async_unit_approach(x, y, 4)) continue;
            unit_drop_item(bld, 30);
        }
        else
        {
            if (async_unit_approach(base_x, base_y, 5)) continue;
            if (async_purge_inventory()) continue;
            unit_take_item(base, mat, 30);
        }
    }
}

void config_base(int b)
{
    base = b;
    base_x = sensor_x(base);
    base_y = sensor_y(base);
}

void __attribute__((nonreentrant)) main()
{
    while (1)
    {
        base = null;
        message = null;
        int i = 0;
        while (i < num_links)
        {
            int block = get_link(i);
            int type = sensor_type(block);
            if (type == type_message)
            {
                message = block;
            }
            if (type == type_nucleus || type == type_foundation || type == type_shard) config_base(block);
            i = i + 1;
        }
        if (base == null)
        {
            print("ERROR: No base configured");
            print_flush(message);
            continue;
        }
        print("Bound base...");
        print_flush(message);
        if (bound_unit == null)
        {
            print("ERROR: Binding flare...");
            print_flush(message);
            unit_bind_next(unit_flare);
            continue;
        }
        print("Bound flare...");
        print_flush(message);
        i = 0;
        while (i < num_links)
        {
            int block = get_link(i);
            int type = sensor_type(block);
            if (type == type_air_factory)
            {
                print("Serving air factory");
                print_flush(message);
                int x = sensor(block, sense_config);
                if (x == unit_mono)
                {
                    if (fill(block, silicon, 30)) continue;
                    if (fill(block, lead, 15)) continue;
                }
                if (x == unit_flare)
                {
                    if (fill(block, silicon, 15)) continue;
                }
            }
            if (type == type_add_reconstructor)
            {
                print("Serving additive reconstructor");
                print_flush(message);
                if (fill(block, silicon, 40)) continue;
                if (fill(block, graphite, 40)) continue;
            }
            if (type == type_multi_reconstructor)
            {
                print("Serving multiplicative reconstructor");
                print_flush(message);
                if (fill(block, silicon, 130)) continue;
                if (fill(block, titanium, 80)) continue;
                if (fill(block, metaglass, 40)) continue;
            }
            i = i + 1;
        }
    }
}
