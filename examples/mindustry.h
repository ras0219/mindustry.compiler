#pragma once

#ifdef __GNUC__
#define __string const char*
#endif

/* write the value 'value' to the 'addr' address in memory 'dst' */
void __attribute__((asmstr("write %0 %1 %2"))) write(int dst, int addr, int value);

/* control the enabled state of 'tgt' */
void __attribute__((asmstr("control enabled %0 %1 0 0 0"))) control_enabled(int tgt, int enabled);
void __attribute__((asmstr("control configure %0 %1 0 0 0"))) control_configure(int dst, int mat);
/* tell @unit to stop moving and stop shooting */
void __attribute__((asmstr("ucontrol stop"))) unit_stop();
/* tell @unit to move to x, y */
void __attribute__((asmstr("ucontrol move %0 %1 0 0 0"))) unit_move(int x, int y);
/* tell @unit to approach x, y at radius 'r'*/
void __attribute__((asmstr("ucontrol approach %0 %1 %2 0 0"))) unit_approach(int x, int y, int r);
/* tell @unit to revert to standard AI */
void __attribute__((asmstr("ucontrol pathfind"))) unit_pathfind();
/* tell @unit to shoot at a position. NOTE: will not shoot if pos is out of range. */
void __attribute__((asmstr("ucontrol target %0 %1 %2"))) unit_shoot(int x, int y, int shoot);
void __attribute__((asmstr("ucontrol itemTake %0 %1 %2"))) unit_take_item(int building, int item, int amt);
void __attribute__((asmstr("ucontrol itemDrop %0 %1"))) unit_drop_item(int building, int amt);
/* binds '\@unit' to the next unit of 'type'; e.g. __poly or __mono */
void __attribute__((asmstr("ubind %0"))) unit_bind_next(int type);
/* flush the draw buffer to 'device' */
void __attribute__((asmstr("drawflush %0"))) draw_flush(int device);
/* queue drawing a line to the draw buffer */
void __attribute__((asmstr("draw line %0 %1 %2 %3"))) draw_line(int x1, int y1, int x2, int y2);
/* queue drawing a filled rectangle to the draw buffer */
void __attribute__((asmstr("draw rect %0 %1 %2 %3"))) draw_rect(int x1, int y1, int x2, int y2);
/* clear the draw buffer */
void __attribute__((asmstr("draw clear %0 %1 %2"))) draw_clear(int r, int g, int b);
/* queue drawing a filled rectangle to the draw buffer */
void __attribute__((asmstr("draw color %0 %1 %2"))) draw_color(int r, int g, int b);

/* queue 'str' to the print buffer */
void __attribute__((asmstr("print %0"))) print(__string str);
void __attribute__((asmstr("print %0"))) printi(int i);

/* flush the print buffer to 'device' */
void __attribute__((asmstr("printflush %0"))) print_flush(int device);

/* sense 'sense' from 'dst' and return the result */
int __attribute__((asmstr("sensor %r %0 %1"))) sensor(int dst, int sense);
int __attribute__((asmstr("sensor %r %0 @x"))) sensor_x(int dst);
int __attribute__((asmstr("sensor %r %0 @y"))) sensor_y(int dst);
int __attribute__((asmstr("sensor %r %0 @type"))) sensor_type(int dst);

/*  */
int __attribute__((asmstr("uradar %0 any any distance 0 1 %r"))) unit_radar(int filter);

/* Gets the linked tiles by index */
int __attribute__((asmstr("getlink %r %0"))) get_link(int index);

/* Gets the number of linked tiles */
const int __attribute__((sym("@links"))) num_links;

const int __attribute__((sym("@unit"))) bound_unit;
const int __attribute__((sym("@flare"))) unit_flare;
const int __attribute__((sym("@poly"))) unit_poly;
const int __attribute__((sym("@mono"))) unit_mono;
const int __attribute__((sym("@mega"))) unit_mega;

const int __attribute__((sym("null"))) null;
const int __attribute__((sym("@thisx"))) this_x;
const int __attribute__((sym("@thisy"))) this_y;

const int __attribute__((sym("@thorium"))) thorium;
const int __attribute__((sym("@lead"))) lead;
const int __attribute__((sym("@graphite"))) graphite;
const int __attribute__((sym("@copper"))) copper;
const int __attribute__((sym("@silicon"))) silicon;
const int __attribute__((sym("@titanium"))) titanium;
const int __attribute__((sym("@metaglass"))) metaglass;

const int __attribute__((sym("@config"))) sense_config;
const int __attribute__((sym("@configure"))) sense_configure;
const int __attribute__((sym("totalItems"))) sense_totalitems;

const int __attribute__((sym("@message"))) type_message;
const int __attribute__((sym("@power-node-large"))) type_power_node_large;
const int __attribute__((sym("@conveyor"))) type_conveyor;
const int __attribute__((sym("@core-shard"))) type_shard;
const int __attribute__((sym("@core-foundation"))) type_foundation;
const int __attribute__((sym("@core-nucleus"))) type_nucleus;
const int __attribute__((sym("@logic-processor"))) type_logic_processor;
const int __attribute__((sym("@air-factory"))) type_air_factory;
const int __attribute__((sym("@additive-reconstructor"))) type_add_reconstructor;
const int __attribute__((sym("@multiplicative-reconstructor"))) type_multi_reconstructor;
