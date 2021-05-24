#include "mindustry.h"

#define __string int

#pragma memory memory1

int __attribute__((sym("cell1"))) cell1;
int __attribute__((sym("conveyor1"))) conveyor1;
int __attribute__((sym("display1"))) display1;
int __attribute__((sym("node2"))) node2;

const int __attribute__((sym("player"))) player_filter;
const int __attribute__((sym("enemy"))) enemy_filter;
const int __attribute__((sym("totalItems"))) totalitems;
const int __attribute__((sym("message1"))) message1;
const int __attribute__((sym("factory1"))) factory1;
const int __attribute__((sym("nucleus1"))) nucleus1;
const int __attribute__((sym("reconstructor1"))) reconstructor1;
const int __attribute__((sym("reconstructor2"))) reconstructor2;
const __string __attribute__((sym("\"thorium: \""))) thorium_msg;
const __string __attribute__((sym("\" \""))) space_str;

// int __attribute__((nonreentrant)) handle_mat(__string f, int mat, int level)
// {
//     int n = sensor(f, mat);
//     if (n < level)
//     {
//         int s2 = sensor(__unit, mat);
//         if (s2 > 0)
//         {
//             int x = sense_x(f);
//             int y = sense_y(f);
//             unit_approach(x, y, 5);
//             unit_drop_item(f, 30);
//         }
//         else
//         {
//             int x = sense_x(nucleus1);
//             int y = sense_y(nucleus1);
//             unit_approach(x, y, 5);
//             if (sensor(f, totalitems) > 0)
//             {
//                 unit_drop_item(nucleus1, 30);
//             }
//             unit_take_item(nucleus1, mat, 30);
//         }
//         return 1;
//     }
//     return 0;
// }

// void __attribute__((nonreentrant)) main()
// {
//     if (handle_mat(factory1, thorium, 60))
//         ;
//     if (handle_mat(factory1, thorium, 60))
//         ;
// }

int __attribute__((nonreentrant)) foo(int a, int b, int c) { return a + b; }

void main() { foo(foo(1, 2, 5), foo(3, 4, 6), 0); }