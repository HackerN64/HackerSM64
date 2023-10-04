#ifndef OBJ_BEHAVIORS_H
#define OBJ_BEHAVIORS_H

#include <PR/ultratypes.h>

#include "engine/surface_collision.h"
#include "macros.h"
#include "types.h"

// Object helpers still in obj_behaviors
void obj_orient_graph(struct Object *obj, f32 normalX, f32 normalY, f32 normalZ);
void calc_obj_friction(f32 *objFriction, f32 floor_nY);
void calc_new_obj_vel_and_pos_y(struct Surface *objFloor, f32 objFloorY, f32 objVelX, f32 objVelZ);
void calc_new_obj_vel_and_pos_y_underwater(struct Surface *objFloor, f32 floorY, f32 objVelX, f32 objVelZ, f32 waterY);
void obj_update_pos_vel_xz(void);
void obj_splash(s32 waterY, s32 objY);
void obj_move_xyz_using_fvel_and_yaw(struct Object *obj);
void set_object_visibility(struct Object *obj, s32 dist);
void obj_return_and_displace_home(struct Object *obj, f32 homeX, UNUSED f32 homeY, f32 homeZ, s32 baseDisp);
void obj_spawn_yellow_coins(struct Object *obj, s8 nCoins);
void obj_check_floor_death(s16 collisionFlags, struct Surface *floor);
void spawn_orange_number(s8 behParam, s16 relX, s16 relY, s16 relZ);
void obj_set_dist_from_home(f32 distFromHome);
void obj_perform_position_op(s32 op);
void cur_obj_spin_all_dimensions(f32 pitchSpeed, f32 rollSpeed);
void obj_rotate_yaw_and_bounce_off_walls(s16 targetYaw, s16 turnAmount);
void obj_compute_vel_from_move_pitch(f32 speed);
void cur_obj_init_anim_extend(s32 animIndex);
void obj_roll_to_match_yaw_turn(s16 targetYaw, s16 maxRoll, s16 rollSpeed);
void obj_die_if_health_non_positive(void);
UNUSED void obj_unused_die(void);
void obj_set_knockback_action(s32 attackType);
void obj_set_squished_action(void);
void obj_act_knockback(UNUSED f32 baseScale);
void obj_act_squished(f32 baseScale);
void treat_far_home_as_mario(f32 threshold);
s32 is_point_within_radius_of_mario(f32 x, f32 y, f32 z, s32 dist);
s32 is_point_close_to_object(struct Object *obj, f32 x, f32 y, f32 z, s32 dist);
s32 obj_return_home_if_safe(struct Object *obj, f32 homeX, f32 y, f32 homeZ, s32 dist);
s32 obj_check_if_facing_toward_angle(u32 base, u32 goal, s16 range);
s32 obj_find_wall_displacement(Vec3f dist, f32 x, f32 y, f32 z, f32 radius);
s32 obj_flicker_and_disappear(struct Object *obj, s16 lifeSpan);
s32 current_mario_room_check(RoomData room);
s32 trigger_obj_dialog_when_facing(s32 *inDialog, s16 dialogID, f32 dist, s32 actionArg);
s32 obj_lava_death(void);
s32 debug_sequence_tracker(s16 debugInputSequence[]);
s32 obj_is_rendering_enabled(void);
s32 obj_update_race_proposition_dialog(s16 dialogID);
s32 obj_is_near_to_and_facing_mario(f32 maxDist, s16 maxAngleDiff);
s32 clamp_s16(s16 *value, s16 minimum, s16 maximum);
s32 clamp_f32(f32 *value, f32 minimum, f32 maximum);
s32 cur_obj_init_anim_and_check_if_end(s32 animIndex);
s32 cur_obj_init_anim_check_frame(s32 animIndex, s32 frame);
s32 cur_obj_set_anim_if_at_end(s32 animIndex);
s32 cur_obj_play_sound_at_anim_range(s8 startFrame1, s8 startFrame2, u32 sound);
s32 approach_f32_ptr(f32 *px, f32 target, f32 delta);
s32 obj_forward_vel_approach(f32 target, f32 delta);
s32 obj_y_vel_approach(f32 target, f32 delta);
s32 obj_move_pitch_approach(s16 target, s16 delta);
s32 obj_face_pitch_approach(s16 targetPitch, s16 deltaPitch);
s32 obj_face_yaw_approach(s16 targetYaw, s16 deltaYaw);
s32 obj_face_roll_approach(s16 targetRoll, s16 deltaRoll);
s32 obj_grow_then_shrink(f32 *scaleVel, f32 shootFireScale, f32 endScale);
s32 oscillate_toward(s32 *value, f32 *vel, s32 target, f32 velCloseToZero, f32 accel, f32 slowdown);
s32 obj_resolve_object_collisions(s32 *targetYaw);
s32 obj_bounce_off_walls_edges_objects(s32 *targetYaw);
s32 obj_resolve_collisions_and_turn(s16 targetYaw, s16 turnSpeed);
s32 obj_die_if_above_lava_and_health_non_positive(void);
s32 obj_update_standard_actions(f32 scale);
s32 obj_check_attacks(struct ObjectHitbox *hitbox, s32 attackedMarioAction);
s32 obj_move_for_one_second(s32 endAction);

// declared in individual behavior files
// TODO: move into behavior specific headers
extern s32 mario_is_far_below_object(f32 min); // king_bobomb.c
extern void set_yoshi_as_not_dead(void); // yoshi.c
void create_respawner(ModelID32 model, const BehaviorScript *behToSpawn, s32 minSpawnDist); // respawner.c

#endif // OBJ_BEHAVIORS_H
