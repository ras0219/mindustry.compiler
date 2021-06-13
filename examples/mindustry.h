#pragma once

#ifdef __GNUC__
#define __string const char*
#endif

/* write the value 'value' to the 'addr' address in memory 'dst' */
void __attribute__((asmstr("write %0 %1 %2"))) write(struct unit* dst, int addr, int value);

/* control the enabled state of 'tgt' */
void __attribute__((asmstr("control enabled %0 %1 0 0 0"))) control_enabled(struct unit* tgt, int enabled);
void __attribute__((asmstr("control configure %0 %1 0 0 0"))) control_configure(struct unit* dst, struct material* mat);
/* tell @unit to stop moving and stop shooting */
void __attribute__((asmstr("ucontrol stop"))) unit_stop();
/* tell @unit to move to x, y */
void __attribute__((asmstr("ucontrol move %0 %1 0 0 0"))) unit_move(int x, int y);
/* tell @unit to mine x, y */
void __attribute__((asmstr("ucontrol mine %0 %1 0 0 0"))) unit_mine(int x, int y);
/* tell @unit to approach x, y at radius 'r'*/
void __attribute__((asmstr("ucontrol approach %0 %1 %2 0 0"))) unit_approach(int x, int y, int r);
/* tell @unit to revert to standard AI */
void __attribute__((asmstr("ucontrol pathfind"))) unit_pathfind();
/* tell @unit to shoot at a position. NOTE: will not shoot if pos is out of range. */
void __attribute__((asmstr("ucontrol target %0 %1 %2"))) unit_target(int x, int y, int shoot);
void __attribute__((asmstr("ucontrol targetp %0 %1 0 0 0"))) unit_targetp(struct unit* target, int shoot);
void __attribute__((asmstr("ucontrol itemTake %0 %1 %2")))
unit_take_item(struct unit* building, struct material* item, int amt);
void __attribute__((asmstr("ucontrol itemDrop %0 %1"))) unit_drop_item(struct unit* building, int amt);
void __attribute__((asmstr("ucontrol build %0 %1 %2 %3 %4")))
unit_build(int x, int y, struct unittype* block, int rotation, int config);

void __attribute__((asmstr("ucontrol boost %0 0 0 0 0"))) unit_boost(int enabled);

struct unit __attribute__((asmstr("ucontrol getBlock %0 %1 0 %r 0"))) * unit_get_block(int x, int y);

int __attribute__((asmstr("ucontrol within %0 %1 %2 %r 0"))) unit_within(int x, int y, int radius);
void __attribute__((asmstr("ucontrol flag %0 0 0 0 0"))) unit_flag(int flag);

int __attribute__((asmstr("ulocate ore core true %0 outx outy %r building")))
unit_locate_ore(const struct material* mat);
int __attribute__((asmstr("ulocate building turret true 0 outx outy %r building"))) unit_locate_turret();
int __attribute__((asmstr("ulocate building core %0 0 outx outy %r building"))) unit_locate_core(struct boolean* enemy);
int __attribute__((sym("outx"))) unit_locate_outx;
int __attribute__((sym("outy"))) unit_locate_outy;
struct unit __attribute__((sym("building"))) * unit_locate_bld;

int __attribute__((asmstr("op sqrt %r %0 0"))) sqrt(int x);

int __attribute__((asmstr("op sin %r %0 0"))) sin(int degrees);
int __attribute__((asmstr("op cos %r %0 0"))) cos(int degrees);
int __attribute__((asmstr("op tan %r %0 0"))) tan(int degrees);

/* binds '\@unit' to the next unit of 'type'; e.g. __poly or __mono */
void __attribute__((asmstr("ubind %0"))) unit_bind_next(struct unittype* type);
/* flush the draw buffer to 'device' */
void __attribute__((asmstr("drawflush %0"))) draw_flush(struct unit* device);
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
void __attribute__((asmstr("print %0"))) print_ut(struct unittype* ut);
void __attribute__((asmstr("print %0"))) printi(int i);

/* flush the print buffer to 'device' */
void __attribute__((asmstr("printflush %0"))) print_flush(struct unit* device);

/* sense 'sense' from 'dst' and return the result */
int __attribute__((asmstr("sensor %r %0 %1"))) sensor(struct unit* dst, struct sense* sense);
int __attribute__((asmstr("sensor %r %0 %1"))) sensor_mat(struct unit* dst, struct material* sense);
struct unittype __attribute__((asmstr("sensor %r %0 @config"))) * sensor_config_unittype(struct unit* dst);
int __attribute__((asmstr("sensor %r %0 @x"))) sensor_x(struct unit* dst);
int __attribute__((asmstr("sensor %r %0 @y"))) sensor_y(struct unit* dst);
struct unittype __attribute__((asmstr("sensor %r %0 @type"))) * sensor_type(struct unit* dst);

/*  */
struct unit __attribute__((asmstr("uradar %0 %1 %2 distance 0 1 %r"))) *
    unit_radar(struct filter* f1, struct filter* f2, struct filter* f3);

/* Gets the linked tiles by index */
struct unit __attribute__((asmstr("getlink %r %0"))) * get_link(int index);

/* Gets the number of linked tiles */
const int __attribute__((sym("@links"))) num_links;

struct unit __attribute__((sym("@unit"))) * bound_unit;
struct unittype __attribute__((sym("@nova")))* const unit_nova;
struct unittype __attribute__((sym("@crawler")))* const unit_crawler;
struct unittype __attribute__((sym("@dagger")))* const unit_dagger;
struct unittype __attribute__((sym("@mace")))* const unit_mace;
struct unittype __attribute__((sym("@fortress")))* const unit_fortress;
struct unittype __attribute__((sym("@flare")))* const unit_flare;
struct unittype __attribute__((sym("@poly")))* const unit_poly;
struct unittype __attribute__((sym("@mono")))* const unit_mono;
struct unittype __attribute__((sym("@mega")))* const unit_mega;

struct unit __attribute__((sym("null")))* const null;
struct material __attribute__((sym("null")))* const null_material;
struct unit __attribute__((sym("@this"))) * this_unit;
const int __attribute__((sym("@thisx"))) this_x;
const int __attribute__((sym("@thisy"))) this_y;

struct material __attribute__((sym("@scrap")))* const scrap;
struct material __attribute__((sym("@thorium")))* const thorium;
struct material __attribute__((sym("@lead")))* const lead;
struct material __attribute__((sym("@graphite")))* const graphite;
struct material __attribute__((sym("@copper")))* const copper;
struct material __attribute__((sym("@silicon")))* const silicon;
struct material __attribute__((sym("@titanium")))* const titanium;
struct material __attribute__((sym("@metaglass")))* const metaglass;
struct material __attribute__((sym("@phase-fabric")))* const phase_fabric;
struct material __attribute__((sym("@blast-compound")))* const blast_compound;
struct material __attribute__((sym("@surge-alloy")))* const surge_alloy;
struct material __attribute__((sym("@pyratite")))* const pyratite;

struct sense __attribute__((sym("@config")))* const sense_config;
struct sense __attribute__((sym("@flag")))* const sense_flag;
struct sense __attribute__((sym("@health")))* const sense_health;
struct sense __attribute__((sym("@maxHealth")))* const sense_max_health;
struct sense __attribute__((sym("@configure")))* const sense_configure;
struct sense __attribute__((sym("@totalItems")))* const sense_totalitems;
struct sense __attribute__((sym("@shooting")))* const sense_shooting;
struct sense __attribute__((sym("@enabled")))* const sense_enabled;
struct sense __attribute__((sym("@dead")))* const sense_dead;
struct sense __attribute__((sym("@ammo")))* const sense_ammo;

struct filter __attribute__((sym("enemy")))* const radar_filter_enemy;
struct filter __attribute__((sym("ground")))* const radar_filter_ground;
struct filter __attribute__((sym("flying")))* const radar_filter_flying;
struct filter __attribute__((sym("any")))* const radar_filter_any;

struct unittype __attribute__((sym("@message")))* const type_message;
struct unittype __attribute__((sym("@mechanical-drill")))* const type_mechanical_drill;
struct unittype __attribute__((sym("@pneumatic-drill")))* const type_pneumatic_drill;
struct unittype __attribute__((sym("@junction")))* const type_junction;
struct unittype __attribute__((sym("@titanium-conveyor")))* const type_titanium_conveyor;
struct unittype __attribute__((sym("@power-node-large")))* const type_power_node_large;
struct unittype __attribute__((sym("@conveyor")))* const type_conveyor;
struct unittype __attribute__((sym("@core-shard")))* const type_shard;
struct unittype __attribute__((sym("@core-foundation")))* const type_foundation;
struct unittype __attribute__((sym("@core-nucleus")))* const type_nucleus;
struct unittype __attribute__((sym("@unloader")))* const type_unloader;
struct unittype __attribute__((sym("@logic-processor")))* const type_logic_processor;
struct unittype __attribute__((sym("@air-factory")))* const type_air_factory;
struct unittype __attribute__((sym("@ground-factory")))* const type_ground_factory;
struct unittype __attribute__((sym("@additive-reconstructor")))* const type_add_reconstructor;
struct unittype __attribute__((sym("@multiplicative-reconstructor")))* const type_multi_reconstructor;
struct unittype __attribute__((sym("@switch")))* const type_switch;

struct unittype __attribute__((sym("@foreshadow")))* const foreshadow;
struct unittype __attribute__((sym("@scatter")))* const scatter;
struct unittype __attribute__((sym("@duo")))* const type_duo;
struct unittype __attribute__((sym("@salvo")))* const type_salvo;
struct unittype __attribute__((sym("@lancer")))* const type_lancer;
struct unittype __attribute__((sym("@arc")))* const type_arc;
struct unittype __attribute__((sym("@spectre")))* const type_spectre;

struct boolean __attribute__((sym("true")))* const bool_true;
struct boolean __attribute__((sym("false")))* const bool_false;
