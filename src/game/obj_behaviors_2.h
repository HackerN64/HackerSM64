#ifndef OBJ_BEHAVIORS_2_H
#define OBJ_BEHAVIORS_2_H

#include <PR/ultratypes.h>

#include "types.h"

#define ATTACK_HANDLER_NOP 0
#define ATTACK_HANDLER_DIE_IF_HEALTH_NON_POSITIVE 1
#define ATTACK_HANDLER_KNOCKBACK 2
#define ATTACK_HANDLER_SQUISHED 3
#define ATTACK_HANDLER_SPECIAL_KOOPA_LOSE_SHELL 4
#define ATTACK_HANDLER_SET_SPEED_TO_ZERO 5
#define ATTACK_HANDLER_SPECIAL_WIGGLER_JUMPED_ON 6
#define ATTACK_HANDLER_SPECIAL_HUGE_GOOMBA_WEAKLY_ATTACKED 7
#define ATTACK_HANDLER_SQUISHED_WITH_BLUE_COIN 8
#define POS_OP_SAVE_POSITION 0
#define POS_OP_COMPUTE_VELOCITY 1
#define POS_OP_RESTORE_POSITION 2
/* BSS (declared to force order) */
extern s32 sNumActiveFirePiranhaPlants;
extern s32 sNumKilledFirePiranhaPlants;
extern f32 sObjSavedPosX;
extern f32 sObjSavedPosY;
extern f32 sObjSavedPosZ;
extern struct Object *sMontyMoleHoleList;
extern s32 sMontyMoleKillStreak;
extern f32 sMontyMoleLastKilledPosX;
extern f32 sMontyMoleLastKilledPosY;
extern f32 sMontyMoleLastKilledPosZ;
extern struct Object *sMasterTreadmill;
void shelled_koopa_attack_handler(s32 attackType);
void obj_spit_fire(s16 relativePosX, s16 relativePosY, s16 relativePosZ, f32 scale, s32 model, f32 startSpeed, f32 endSpeed, s16 movePitch);
void obj_set_speed_to_zero(void);
extern s32 obj_is_rendering_enabled(void);
extern s16 obj_get_pitch_from_vel(void);
extern s32 obj_update_race_proposition_dialog(s16 dialogID);
extern void obj_set_dist_from_home(f32 distFromHome);
extern s32 obj_is_near_to_and_facing_mario(f32 maxDist, s16 maxAngleDiff);
extern void obj_perform_position_op(s32 op);
extern void platform_on_track_update_pos_or_spawn_ball(s32 ballIndex, f32 x, f32 y, f32 z);
extern void cur_obj_spin_all_dimensions(f32 arg0, f32 arg1);
extern void obj_rotate_yaw_and_bounce_off_walls(s16 targetYaw, s16 turnAmount);
extern s16 obj_get_pitch_to_home(f32 latDistToHome);
extern void obj_compute_vel_from_move_pitch(f32 speed);
extern s32 clamp_s16(s16 *value, s16 minimum, s16 maximum);
extern s32 clamp_f32(f32 *value, f32 minimum, f32 maximum);
extern void cur_obj_init_anim_extend(s32 arg0);
extern s32 cur_obj_init_anim_and_check_if_end(s32 arg0);
extern s32 cur_obj_init_anim_check_frame(s32 arg0, s32 arg1);
extern s32 cur_obj_set_anim_if_at_end(s32 arg0);
extern s32 cur_obj_play_sound_at_anim_range(s8 arg0, s8 arg1, u32 sound);
extern s16 obj_turn_pitch_toward_mario(f32 targetOffsetY, s16 turnAmount);
extern s32 approach_f32_ptr(f32 *px, f32 target, f32 delta);
extern s32 obj_forward_vel_approach(f32 target, f32 delta);
extern s32 obj_y_vel_approach(f32 target, f32 delta);
extern s32 obj_move_pitch_approach(s16 target, s16 delta);
extern s32 obj_face_pitch_approach(s16 targetPitch, s16 deltaPitch);
extern s32 obj_face_yaw_approach(s16 targetYaw, s16 deltaYaw);
extern s32 obj_face_roll_approach(s16 targetRoll, s16 deltaRoll);
extern s32 obj_smooth_turn(s16 *angleVel, s32 *angle, s16 targetAngle, f32 targetSpeedProportion, s16 accel, s16 minSpeed, s16 maxSpeed);
extern void obj_roll_to_match_yaw_turn(s16 targetYaw, s16 maxRoll, s16 rollSpeed);
extern s16 random_linear_offset(s16 base, s16 range);
extern s16 random_mod_offset(s16 base, s16 step, s16 mod);
extern s16 obj_random_fixed_turn(s16 delta);
extern s32 obj_grow_then_shrink(f32 *scaleVel, f32 shootFireScale, f32 endScale);
extern s32 oscillate_toward(s32 *value, f32 *vel, s32 target, f32 velCloseToZero, f32 accel, f32 slowdown);
extern void obj_update_blinking(s32 *blinkTimer, s16 baseCycleLength, s16 cycleLengthRange, s16 blinkLength);
extern s32 obj_resolve_object_collisions(s32 *targetYaw);
extern s32 obj_bounce_off_walls_edges_objects(s32 *targetYaw);
extern s32 obj_resolve_collisions_and_turn(s16 targetYaw, s16 turnSpeed);
extern void obj_die_if_health_non_positive(void);
extern void obj_set_knockback_action(s32 attackType);
extern void obj_set_squished_action(void);
extern s32 obj_die_if_above_lava_and_health_non_positive(void);
extern s32 obj_handle_attacks(struct ObjectHitbox *hitbox, s32 attackedMarioAction, u8 *attackHandlers);
extern void obj_act_knockback(UNUSED f32 baseScale);
extern void obj_act_squished(f32 baseScale);
extern s32 obj_update_standard_actions(f32 scale);
extern s32 obj_check_attacks(struct ObjectHitbox *hitbox, s32 attackedMarioAction);
extern s32 obj_move_for_one_second(s32 endAction);
extern void treat_far_home_as_mario(f32 threshold);
extern void obj_spit_fire(s16 relativePosX, s16 relativePosY, s16 relativePosZ, f32 scale, s32 model, f32 startSpeed, f32 endSpeed, s16 movePitch);

#endif // OBJ_BEHAVIORS_2_H
