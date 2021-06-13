#include "mindustry.h"

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

void sleep(int n)
{
    for (int s = 0; s < n; s++)
        ;
}

void build(int start_x, int start_y)
{
    if (async_unit_approach(start_x, start_y, 4) == 0)
    {
        for (int y = 0; y < 10; y += 5)
        {
            for (int x = 0; x < 8; x += 2)
            {
                int x2 = start_x + x;
                int y2 = start_y + y;
                unit_build(x2, y2, type_mechanical_drill, 0, 0);
                sleep(100);
                unit_build(x2, y2 + 2, type_titanium_conveyor, 0, 0);
                sleep(50);
                unit_build(x2 + 1, y2 + 2, type_titanium_conveyor, 0, 0);
                sleep(50);
                unit_build(x2, y2 + 3, type_mechanical_drill, 0, 0);
                sleep(100);
            }
        }
    }
}

void mine()
{
    while (unit_locate_ore(copper))
    {
        if (bound_unit == null) break;
        print("found copper: ");
        printi(unit_locate_outx);
        print(", ");
        printi(unit_locate_outy);
        print_flush(message);

        int dx = unit_locate_outx - this_x;
        dx = dx * dx;
        int dy = unit_locate_outy - this_y;
        dy = dy * dy;
        if (dx > 900 || dy > 22500)
        {
            unit_move(this_x, this_y);
            continue;
        }
        while (!unit_within(unit_locate_outx, unit_locate_outy, 10))
            unit_move(unit_locate_outx, unit_locate_outy);
        int offset_x = unit_locate_outx % 2;
        int offset_y = unit_locate_outy % 5;
        int align_x = unit_locate_outx - offset_x;
        int align_y = unit_locate_outy - offset_y;
        if (offset_y < 2)
        {
            unit_build(align_x, align_y, type_pneumatic_drill, 0, 0);
            sleep(100);
        }
        unit_build(align_x, align_y + 2, type_titanium_conveyor, 0, 0);
        sleep(50);
        unit_build(align_x + 1, align_y + 2, type_titanium_conveyor, 0, 0);
        sleep(50);
        if (offset_y > 2)
        {
            unit_build(align_x, align_y + 3, type_pneumatic_drill, 0, 0);
            sleep(100);
        }
        unit_build(unit_locate_outx, unit_locate_outy, type_titanium_conveyor, 0, 0);
        sleep(50);
    }
}

#pragma memory memory1

void main()
{
    int have_unit = 0;
    while (1)
    {
        message = null;
        int i = 0;
        while (i < num_links)
        {
            struct unit* block = get_link(i);
            struct unittype* type = sensor_type(block);
            if (type == type_message) message = block;
            i = i + 1;
        }
        if (bound_unit == null)
        {
            have_unit = 0;
            unit_bind_next(unit_poly);
            if (bound_unit == null)
            {
                print("ERROR: Binding poly...");
                print_flush(message);
                continue;
            }
        }
        if (!have_unit)
        {
            int flag = sensor(bound_unit, sense_flag);
            if (flag != 0)
            {
                print("ERROR: Poly in use...");
                print_flush(message);
                unit_bind_next(unit_poly);
                continue;
            }
            unit_flag(1);
            have_unit = 1;
        }

        print("Bound poly...");
        print_flush(message);

        mine();
        // build(this_x, this_y + 2);
    }
}
