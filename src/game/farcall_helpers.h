#ifndef OBJ_BEHAVIORS_2_NONSTATIC_H
#define OBJ_BEHAVIORS_2_NONSTATIC_H

#include "object_fields.h"
#include "object_constants.h"
#include "object_helpers.h"
#include "object_collision.h"
#include "obj_behaviors.h"
#include "object_list_processor.h"
#include "interaction.h"
#include "behavior_data.h"
#include "engine/behavior_script.h"
#include "engine/math_util.h"
#include "engine/surface_load.h"
#include "engine/surface_collision.h"
#include "sound_init.h"
#include "spawn_sound.h"
#include "farcall.h"
#include "audio/external.h"
#include "level_update.h"
#include "mario.h"
#include "save_file.h"
#include "game_init.h"
#include "obj_behaviors_2.h"
#include "level_headers.h"

s32 obj_is_rendering_enabled(void);
s32 obj_get_pitch_from_vel(void);
s32 obj_update_race_proposition_dialog(s16 dialogID);
void obj_set_dist_from_home(f32 distFromHome);
s32 obj_is_near_to_and_facing_mario(f32 maxDist, s16 maxAngleDiff);
void obj_perform_position_op(s32 op);
void cur_obj_spin_all_dimensions(f32 pitchSpeed, f32 rollSpeed);
void obj_rotate_yaw_and_bounce_off_walls(s16 targetYaw, s16 turnAmount);
s32 obj_get_pitch_to_home(f32 latDistToHome);
void obj_compute_vel_from_move_pitch(f32 speed);
s32 clamp_s16(s16 *value, s16 minimum, s16 maximum);
s32 clamp_f32(f32 *value, f32 minimum, f32 maximum);
void cur_obj_init_anim_extend(s32 animIndex);
s32 cur_obj_init_anim_and_check_if_end(s32 animIndex);
s32 cur_obj_init_anim_check_frame(s32 animIndex, s32 frame);
s32 cur_obj_set_anim_if_at_end(s32 animIndex);
s32 cur_obj_play_sound_at_anim_range(s8 startFrame1, s8 startFrame2, u32 sound);
s32 obj_turn_pitch_toward_mario(f32 targetOffsetY, s16 turnAmount);
s32 approach_f32_ptr(f32 *px, f32 target, f32 delta);
s32 obj_forward_vel_approach(f32 target, f32 delta);
s32 obj_y_vel_approach(f32 target, f32 delta);
s32 obj_move_pitch_approach(s16 target, s16 delta);
s32 obj_face_pitch_approach(s16 targetPitch, s16 deltaPitch);
s32 obj_face_yaw_approach(s16 targetYaw, s16 deltaYaw);
s32 obj_face_roll_approach(s16 targetRoll, s16 deltaRoll);
s32 obj_smooth_turn(s16 *angleVel, s32 *angle, s16 targetAngle, f32 targetSpeedProportion,
                           s16 accel, s16 minSpeed, s16 maxSpeed);
void obj_roll_to_match_yaw_turn(s16 targetYaw, s16 maxRoll, s16 rollSpeed);
s32 random_linear_offset(s16 base, s16 range);
s32 random_mod_offset(s16 base, s16 step, s16 mod);
s32 obj_random_fixed_turn(s16 delta);
s32 obj_grow_then_shrink(f32 *scaleVel, f32 shootFireScale, f32 endScale);
s32 oscillate_toward(s32 *value, f32 *vel, s32 target, f32 velCloseToZero, f32 accel, f32 slowdown);
void obj_update_blinking(s32 *blinkTimer, s16 baseCycleLength, s16 cycleLengthRange, s16 blinkLength);
s32 obj_resolve_object_collisions(s32 *targetYaw);
s32 obj_bounce_off_walls_edges_objects(s32 *targetYaw);
s32 obj_resolve_collisions_and_turn(s16 targetYaw, s16 turnSpeed);
void obj_die_if_health_non_positive(void);
void obj_unused_die(void);
void obj_set_knockback_action(s32 attackType);
void obj_set_squished_action(void);
s32 obj_die_if_above_lava_and_health_non_positive(void);
s32 obj_handle_attacks(struct ObjectHitbox *hitbox, s32 attackedMarioAction, u8 *attackHandlers);
void obj_act_knockback(UNUSED f32 baseScale);
void obj_act_squished(f32 baseScale);
s32 obj_update_standard_actions(f32 scale);
s32 obj_check_attacks(struct ObjectHitbox *hitbox, s32 attackedMarioAction);
s32 obj_move_for_one_second(s32 endAction);
void treat_far_home_as_mario(f32 threshold);

#endif
