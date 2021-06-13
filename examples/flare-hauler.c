#include "mindustry.h"

struct unit* base;
int base_x;
int base_y;
struct unit* message;

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
        unit_drop_item(base, 99);
    }
    return r;
}

int fill(struct unit* bld, struct material* mat, int minlvl)
{
    while (1)
    {
        if (bound_unit == null) return 1;
        int m = sensor_mat(bld, mat);
        if (m >= minlvl) return 0;

        int s2 = sensor_mat(bound_unit, mat);
        if (s2 > 0)
        {
            int x = sensor_x(bld);
            int y = sensor_y(bld);
            if (async_unit_approach(x, y, 5)) continue;
            unit_drop_item(bld, 99);
        }
        else
        {
            if (async_unit_approach(base_x, base_y, 5)) continue;
            if (async_purge_inventory()) continue;
            unit_take_item(base, mat, 99);
        }
    }
}
int fill_ammo(struct unit* bld, struct material* mat, int minlvl)
{
    while (1)
    {
        if (bound_unit == null) return 1;
        int m = sensor(bld, sense_ammo);
        if (m >= minlvl) return 0;

        int s2 = sensor_mat(bound_unit, mat);
        if (s2 > 0)
        {
            int x = sensor_x(bld);
            int y = sensor_y(bld);
            if (async_unit_approach(x, y, 5)) continue;
            unit_drop_item(bld, 99);
        }
        else
        {
            if (async_unit_approach(base_x, base_y, 5)) continue;
            if (async_purge_inventory()) continue;
            unit_take_item(base, mat, 99);
        }
    }
}

void config_base(struct unit* b)
{
    base = b;
    base_x = sensor_x(base);
    base_y = sensor_y(base);
}

void help_ground_factory(struct unit* block)
{
    print("Serving ground factory");
    print_flush(message);
    while (1)
    {
        if (bound_unit == null) return;
        struct unittype* x = sensor_config_unittype(block);
        if (x == unit_dagger)
        {
            if (fill(block, silicon, 10)) continue;
            if (fill(block, lead, 10)) continue;
        }
        return;
    }
}
void help_air_factory(struct unit* block)
{
    print("Serving air factory");
    print_flush(message);
    while (1)
    {
        if (bound_unit == null) return;
        struct unittype* x = sensor_config_unittype(block);
        if (x == unit_mono)
        {
            if (fill(block, silicon, 30)) continue;
            if (fill(block, lead, 15)) continue;
        }
        if (x == unit_flare)
        {
            if (fill(block, silicon, 15)) continue;
        }
        return;
    }
}
void help_add_recon(struct unit* block)
{
    print("Serving additive reconstructor");
    print_flush(message);
    while (1)
    {
        if (bound_unit == null) return;
        if (fill(block, silicon, 40)) continue;
        if (fill(block, graphite, 40)) continue;
        return;
    }
}
void help_foreshadow(struct unit* block)
{
    print("Serving foreshadow");
    print_flush(message);
    while (1)
    {
        if (bound_unit == null) return;
        if (fill_ammo(block, surge_alloy, 40)) continue;
        return;
    }
}
void help_scatter(struct unit* block)
{
    print("Serving scatter");
    print_flush(message);
    while (1)
    {
        if (bound_unit == null) return;
        if (fill_ammo(block, metaglass, 15)) continue;
        return;
    }
}
void help_mult_recon(struct unit* block)
{
    print("Serving multiplicative reconstructor");
    print_flush(message);
    while (1)
    {
        if (bound_unit == null) return;
        if (fill(block, silicon, 130)) continue;
        if (fill(block, titanium, 80)) continue;
        if (fill(block, metaglass, 40)) continue;
        return;
    }
}

void main()
{
    int have_unit = 0;
    while (1)
    {
        message = null;
        for (int i = 0; i < num_links; i++)
        {
            struct unit* block = get_link(i);
            struct unittype* type = sensor_type(block);
            if (type == type_message) message = block;
        }
        if (bound_unit == null)
        {
            have_unit = 0;
            unit_bind_next(unit_flare);
            if (bound_unit == null)
            {
                print("ERROR: Binding flare...");
                print_flush(message);
                continue;
            }
        }
        if (!have_unit)
        {
            int flag = sensor(bound_unit, sense_flag);
            if (flag != 0)
            {
                print("ERROR: Flare in use...");
                print_flush(message);
                unit_bind_next(unit_flare);
                continue;
            }
            unit_flag(1);
            have_unit = 1;
        }

        print("Bound flare...");
        print_flush(message);

        unit_locate_core(bool_false);
        base = unit_locate_bld;
        base_x = unit_locate_outx;
        base_y = unit_locate_outy;

        for (int i = 0; i < num_links; i++)
        {
            if (bound_unit == null) break;
            struct unit* block = get_link(i);
            struct unittype* type = sensor_type(block);
            if (type == type_ground_factory)
                help_ground_factory(block);
            else if (type == type_air_factory)
                help_air_factory(block);
            else if (type == type_add_reconstructor)
                help_add_recon(block);
            else if (type == type_multi_reconstructor)
                help_mult_recon(block);
            else if (type == foreshadow)
                help_foreshadow(block);
            else if (type == scatter)
                help_scatter(block);
        }
    }
}
