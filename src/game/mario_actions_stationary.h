#ifndef MARIO_ACTIONS_STATIONARY
#define MARIO_ACTIONS_STATIONARY

#include <PR/ultratypes.h>

#include "types.h"

enum ActionStatesIdle { // act_idle, act_metal_water_standing
    ACT_STATE_IDLE_HEAD_LEFT,
    ACT_STATE_IDLE_HEAD_RIGHT,
    ACT_STATE_IDLE_HEAD_CENTER,
    ACT_STATE_IDLE_RESET_OR_SLEEP
};

enum ActionStatesStartSleeping { // act_start_sleeping
    ACT_STATE_START_SLEEPING_IDLE,
    ACT_STATE_START_SLEEPING_SCRATCH,
    ACT_STATE_START_SLEEPING_YAWN,
    ACT_STATE_START_SLEEPING_SITTING,
    ACT_STATE_START_SLEEPING_SLEEP
};

enum ActionStatesSleeping { // act_sleeping
    ACT_SLEEPING_STATE_IDLE,
    ACT_SLEEPING_STATE_START_LYING,
    ACT_SLEEPING_STATE_LYING
};

enum ActionStatesShivering { // act_shivering
    ACT_STATE_SHIVERING_WARMING_HAND,
    ACT_STATE_SHIVERING_SHAKE,
    ACT_STATE_SHIVERING_RETURN_TO_IDLE
};

enum ActionStatesTwirlLand { // act_twirl_land
    ACT_STATE_TWIRL_LAND_0,
    ACT_STATE_TWIRL_LAND_1
};

enum ActionStatesGroundPoundLand { // act_ground_pound_land
    ACT_STATE_GROUND_POUND_LAND_0,
    ACT_STATE_GROUND_POUND_LAND_1
};

enum ActionStatesFirstPerson { // act_first_person
    ACT_STATE_FIRST_PERSON_SET_MODE,
    ACT_STATE_FIRST_PERSON_IDLE
};

s32 check_common_idle_cancels(struct MarioState *m);
s32 check_common_hold_idle_cancels(struct MarioState *m);
s32 act_idle(struct MarioState *m);
void play_anim_sound(struct MarioState *m, u32 actionState, s32 animFrame, u32 sound);
s32 act_start_sleeping(struct MarioState *m);
s32 act_sleeping(struct MarioState *m);
s32 act_waking_up(struct MarioState *m);
s32 act_shivering(struct MarioState *m);
s32 act_coughing(struct MarioState *m);
s32 act_standing_against_wall(struct MarioState *m);
s32 act_in_quicksand(struct MarioState *m);
s32 act_crouching(struct MarioState *m);
s32 act_panting(struct MarioState *m);
void stopping_step(struct MarioState *m, s32 animID, u32 action);
s32 act_braking_stop(struct MarioState *m);
s32 act_butt_slide_stop(struct MarioState *m);
s32 act_hold_butt_slide_stop(struct MarioState *m);
s32 act_slide_kick_slide_stop(struct MarioState *m);
s32 act_start_crouching(struct MarioState *m);
s32 act_stop_crouching(struct MarioState *m);
s32 act_start_crawling(struct MarioState *m);
s32 act_stop_crawling(struct MarioState *m);
s32 act_shockwave_bounce(struct MarioState *m);
s32 landing_step(struct MarioState *m, s32 animID, u32 action);
s32 check_common_landing_cancels(struct MarioState *m, u32 action);
s32 act_jump_land_stop(struct MarioState *m);
s32 act_double_jump_land_stop(struct MarioState *m);
s32 act_side_flip_land_stop(struct MarioState *m);
s32 act_freefall_land_stop(struct MarioState *m);
s32 act_triple_jump_land_stop(struct MarioState *m);
s32 act_backflip_land_stop(struct MarioState *m);
s32 act_lava_boost_land(struct MarioState *m);
s32 act_long_jump_land_stop(struct MarioState *m);
s32 act_hold_jump_land_stop(struct MarioState *m);
s32 act_hold_freefall_land_stop(struct MarioState *m);
s32 act_air_throw_land(struct MarioState *m);
s32 act_twirl_land(struct MarioState *m);
s32 act_ground_pound_land(struct MarioState *m);
s32 act_first_person(struct MarioState *m);
s32 check_common_stationary_cancels(struct MarioState *m);
s32 mario_execute_stationary_action(struct MarioState *m);

#endif // MARIO_ACTIONS_STATIONARY
