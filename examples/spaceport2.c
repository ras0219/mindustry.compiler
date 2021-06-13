#include "mindustry.h"

struct unit* message;
struct unit* base;
int level;

struct material* check_send(struct material* mat)
{
    int n = sensor_mat(base, mat);
    if (n > level)
    {
        return mat;
    }
    return null_material;
}

void main()
{
    while (1)
    {
        message = null;
        base = null;

        for (int i = 0; i < num_links; i++)
        {
            struct unit* block = get_link(i);
            struct unittype* type = sensor_type(block);
            if (type == type_nucleus)
            {
                base = block;
                level = 11000;
            }
            else if (type == type_foundation)
            {
                base = block;
                level = 7000;
            }
            else if (type == type_shard)
            {
                base = block;
                level = 3000;
            }
            else if (type == type_message)
                message = block;
        }
        if (base == null)
        {
            print("no base");
            print_flush(message);
            continue;
        }
        struct material* s_mat;
        if (s_mat = check_send(thorium))
            ;
        else if (s_mat = check_send(phase_fabric))
            ;
        else if (s_mat = check_send(blast_compound))
            ;
        else if (s_mat = check_send(pyratite))
            ;
        else if (s_mat = check_send(graphite))
            ;
        else if (s_mat = check_send(silicon))
            ;
        else if (s_mat = check_send(metaglass))
            ;
        else if (s_mat = check_send(titanium))
            ;
        else if (s_mat = check_send(lead))
            ;
        else if (s_mat = check_send(copper))
            ;

        int x = 0;
        for (int i = 0; i < num_links; i++)
        {
            struct unit* block = get_link(i);
            struct unittype* type = sensor_type(block);
            if (type == type_unloader)
            {
                x++;
                if (s_mat == null_material)
                {
                    control_enabled(block, 0);
                }
                else
                {
                    control_enabled(block, 1);
                    control_configure(block, s_mat);
                }
            }
        }
        print("unloaders: ");
        printi(x);
        print_flush(message);
    }
}
