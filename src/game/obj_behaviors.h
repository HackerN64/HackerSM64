#ifndef OBJ_BEHAVIORS_H
#define OBJ_BEHAVIORS_H

#include <PR/ultratypes.h>

#include "engine/surface_collision.h"
#include "macros.h"
#include "types.h"

extern s8 sYoshiDead;
extern void set_yoshi_as_not_dead(void);
extern f32 absf_2(f32 f);
extern void turn_obj_away_from_surface(f32 velX, f32 velZ, f32 nX, UNUSED f32 nY, f32 nZ, f32 *objYawX, f32 *objYawZ);
extern s8 obj_find_wall(f32 objNewX, f32 objY, f32 objNewZ, f32 objVelX, f32 objVelZ);
extern s8 turn_obj_away_from_steep_floor(struct Surface *objFloor, f32 floorY, f32 objVelX, f32 objVelZ);
extern void obj_orient_graph(struct Object *obj, f32 normalX, f32 normalY, f32 normalZ);
extern void calc_obj_friction(f32 *objFriction, f32 floor_nY);
extern void calc_new_obj_vel_and_pos_y(struct Surface *objFloor, f32 objFloorY, f32 objVelX, f32 objVelZ);
extern void calc_new_obj_vel_and_pos_y_underwater(struct Surface *objFloor, f32 floorY, f32 objVelX, f32 objVelZ, f32 waterY);
extern void obj_splash(s32 waterY, s32 objY);
extern s16 object_step(void);
extern s16 object_step_without_floor_orient(void);
extern void obj_move_xyz_using_fvel_and_yaw(struct Object *obj);
extern s8 is_point_within_radius_of_mario(f32 x, f32 y, f32 z, s32 dist);
extern s8 is_point_close_to_object(struct Object *obj, f32 x, f32 y, f32 z, s32 dist);
extern s8 obj_return_home_if_safe(struct Object *obj, f32 homeX, f32 y, f32 homeZ, s32 dist);
extern void obj_return_and_displace_home(struct Object *obj, f32 homeX, UNUSED f32 homeY, f32 homeZ, s32 baseDisp);
extern s8 obj_check_if_facing_toward_angle(u32 base, u32 goal, s16 range);
extern s8 obj_find_wall_displacement(Vec3f dist, f32 x, f32 y, f32 z, f32 radius);
extern void obj_spawn_yellow_coins(struct Object *obj, s8 nCoins);
extern s8 obj_flicker_and_disappear(struct Object *obj, s16 lifeSpan);
extern s8 current_mario_room_check(s16 room);
extern s16 trigger_obj_dialog_when_facing(s32 *inDialog, s16 dialogID, f32 dist, s32 actionArg);
extern void obj_check_floor_death(s16 collisionFlags, struct Surface *floor);
extern s8 obj_lava_death(void);
extern void spawn_orange_number(s8 behParam, s16 relX, s16 relY, s16 relZ);
extern struct Surface *sObjFloor;
void set_object_visibility(struct Object *obj, s32 dist);
void set_yoshi_as_not_dead(void);
s32 coin_step(s16 *collisionFlagsPtr);
void moving_coin_flicker(void);
void coin_collected(void);
void bhv_moving_yellow_coin_init(void);
void bhv_moving_yellow_coin_loop(void);
void bhv_moving_blue_coin_init(void);
void bhv_moving_blue_coin_loop(void);
void bhv_blue_coin_sliding_jumping_init(void);
void blue_coin_sliding_away_from_mario(void); /* likely unused */
void blue_coin_sliding_slow_down(void); /* likely unused */
void bhv_blue_coin_sliding_loop(void); /* likely unused */
void bhv_blue_coin_jumping_loop(void); /* likely unused */
void bhv_seaweed_init(void);
void bhv_seaweed_bundle_init(void);
void bhv_bobomb_init(void);
void bobomb_spawn_coin(void);
void bobomb_act_explode(void);
void bobomb_check_interactions(void);
void bobomb_act_patrol(void);
void bobomb_act_chase_mario(void);
void bobomb_act_launched(void);
void generic_bobomb_free_loop(void);
void stationary_bobomb_free_loop(void);
void bobomb_free_loop(void);
void bobomb_held_loop(void);
void bobomb_dropped_loop(void);
void bobomb_thrown_loop(void);
void curr_obj_random_blink(s32 *blinkTimer);
void bhv_bobomb_loop(void);
void bhv_bobomb_fuse_smoke_init(void);
void bhv_bobomb_buddy_init(void);
void bobomb_buddy_act_idle(void);
void bobomb_buddy_cannon_dialog(s16 dialogFirstText, s16 dialogSecondText);
void bobomb_buddy_act_talk(void);
void bobomb_buddy_act_turn_to_talk(void);
void bobomb_buddy_actions(void);
void bhv_bobomb_buddy_loop(void);
void bhv_cannon_closed_init(void);
void cannon_door_act_opening(void);
void bhv_cannon_closed_loop(void);
void bhv_whirlpool_init(void);
void whirlpool_set_hitbox(void);
void bhv_whirlpool_loop(void);
void bhv_jet_stream_loop(void);
void bhv_homing_amp_init(void);
void bhv_homing_amp_loop(void);
void bhv_circling_amp_init(void);
void bhv_circling_amp_loop(void);
void bhv_butterfly_init(void);
void butterfly_step(s32 speed);
void butterfly_calculate_angle(void);
void butterfly_act_rest(void);
void butterfly_act_follow_mario(void);
void butterfly_act_return_home(void);
void bhv_butterfly_loop(void);
void bhv_hoot_init(void);
f32  hoot_find_next_floor(f32 dist);
void hoot_floor_bounce(void);
void hoot_free_step(s16 fastOscY, s32 speed);
void hoot_player_set_yaw(void);
void hoot_carry_step(s32 speed, UNUSED f32 xPrev, UNUSED f32 zPrev);
void hoot_surface_collision(f32 xPrev, UNUSED f32 yPrev, f32 zPrev);
void hoot_act_ascent(f32 xPrev, f32 zPrev);
void hoot_action_loop(void);
void hoot_turn_to_home(void);
void hoot_awake_loop(void);
void bhv_hoot_loop(void);
void bhv_beta_holdable_object_init(void); /* unused */
void bhv_beta_holdable_object_loop(void); /* unused */
void bhv_object_bubble_init(void);
void bhv_object_bubble_loop(void);
void bhv_object_water_wave_init(void);
void bhv_object_water_wave_loop(void);
void bhv_explosion_init(void);
void bhv_explosion_loop(void);
void bhv_bobomb_bully_death_smoke_init(void);
void bhv_bobomb_explosion_bubble_init(void);
void bhv_bobomb_explosion_bubble_loop(void);
void bhv_small_bully_init(void);
void bhv_big_bully_init(void);
void bully_check_mario_collision(void);
void bully_act_chase_mario(void);
void bully_act_knockback(void);
void bully_act_back_up(void);
void bully_backup_check(s16 collisionFlags);
void bully_play_stomping_sound(void);
void bully_step(void);
void bully_spawn_coin(void);
void bully_act_level_death(void);
void bhv_bully_loop(void);
void big_bully_spawn_minion(s32 arg0, s32 arg1, s32 arg2, s16 arg3);
void bhv_big_bully_with_minions_init(void);
void big_bully_spawn_star(void);
void bhv_big_bully_with_minions_loop(void);
f32 water_ring_calc_mario_dist(void);
void water_ring_init(void);
void bhv_jet_stream_water_ring_init(void);
void water_ring_check_collection(f32 avgScale, struct Object* ringManager);
void water_ring_set_scale(f32 avgScale);
void water_ring_act_collected(void);
void water_ring_act_not_collected(void);
void bhv_jet_stream_water_ring_loop(void);
void spawn_manta_ray_ring_manager(void); /* unused */
void water_ring_spawner_act_inactive(void);
void bhv_jet_stream_ring_spawner_loop(void);
void bhv_manta_ray_water_ring_init(void);
void manta_water_ring_act_not_collected(void);
void bhv_manta_ray_water_ring_loop(void);
void bhv_bowser_bomb_loop(void);
void bhv_bowser_bomb_explosion_loop(void);
void bhv_bowser_bomb_smoke_loop(void);
void bhv_celebration_star_init(void);
void celeb_star_act_spin_around_mario(void);
void celeb_star_act_face_camera(void);
void bhv_celebration_star_loop(void);
void bhv_celebration_star_sparkle_loop(void);
void bhv_star_key_collection_puff_spawner_loop(void);
void bhv_lll_drawbridge_spawner_loop(void);
void bhv_lll_drawbridge_loop(void);
void bhv_small_bomp_init(void);
void bhv_small_bomp_loop(void);
void bhv_large_bomp_init(void);
void bhv_large_bomp_loop(void);
void bhv_wf_sliding_platform_init(void);
void bhv_wf_sliding_platform_loop(void);
void bhv_moneybag_init(void);
void moneybag_check_mario_collision(void);
void moneybag_jump(s8 collisionFlags);
void moneybag_act_move_around(void);
void moneybag_act_return_home(void);
void moneybag_act_disappear(void);
void moneybag_act_death(void);
void bhv_moneybag_loop(void);
void bhv_moneybag_hidden_loop(void);
void bhv_bowling_ball_init(void);
void bowling_ball_set_hitbox(void);
void bowling_ball_set_waypoints(void);
void bhv_bowling_ball_roll_loop(void);
void bhv_bowling_ball_initializeLoop(void);
void bhv_bowling_ball_loop(void);
void bhv_generic_bowling_ball_spawner_init(void);
void bhv_generic_bowling_ball_spawner_loop(void);
void bhv_thi_bowling_ball_spawner_loop(void);
void bhv_bob_pit_bowling_ball_init(void);
void bhv_bob_pit_bowling_ball_loop(void);
void bhv_free_bowling_ball_init(void); /* likely unused */
void bhv_free_bowling_ball_roll_loop(void); /* likely unused */
void bhv_free_bowling_ball_loop(void); /* likely unused */
void bhv_rr_cruiser_wing_init(void);
void bhv_rr_cruiser_wing_loop(void);
void spawn_default_star(f32 x, f32 y, f32 z);

#endif // OBJ_BEHAVIORS_H
