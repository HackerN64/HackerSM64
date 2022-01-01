#ifndef MARIO_ACTIONS_AIRBORNE_H
#define MARIO_ACTIONS_AIRBORNE_H

#include <PR/ultratypes.h>

#include "types.h"

enum ActionStatesGroundPound { // act_ground_pound
    ACT_STATE_GROUND_POUND_SPIN,
    ACT_STATE_GROUND_POUND_FALL
};

enum ActionArgsGroundPound { // act_ground_pound
    ACT_ARG_GROUND_POUND_NORMAL,
    ACT_ARG_GROUND_POUND_FROM_TRIPLE_JUMP
};

enum ActionStatesGettingBlown { // act_getting_blown
    ACT_STATE_GETTING_BLOWN_ACCEL_BACKWARDS,
    ACT_STATE_GETTING_BLOWN_SLOW_DOWN
};

enum ActionStatesButtSlideAir { // act_butt_slide_air, act_hold_butt_slide_air
    ACT_STATE_BUTT_SLIDE_AIR_SMALL_BOUNCE,
    ACT_STATE_BUTT_SLIDE_AIR_START_SLIDING
};

enum ActionStatesLavaBoost { // act_lava_boost
    ACT_STATE_LAVA_BOOST_HIT_LAVA,
    ACT_STATE_LAVA_BOOST_SLOW_DOWN,
    ACT_STATE_LAVA_BOOST_SET_LANDING_ACTION
};

enum ActionStatesSlideKick { // act_slide_kick
    ACT_STATE_SLIDE_KICK_SLIDING,
    ACT_STATE_SLIDE_KICK_END
};

enum ActionStatesJumpKick { // act_jump_kick
    ACT_STATE_JUMP_KICK_PLAY_SOUND_AND_ANIM,
    ACT_STATE_JUMP_KICK_KICKING
};

enum ActionStatesFlying { // act_flying
    ACT_STATE_FLYING_SPIN,
    ACT_STATE_FLYING_FLY
};

enum ActionArgsFlying { // act_flying
    ACT_ARG_FLYING_FROM_CANNON,
    ACT_ARG_FLYING_DEFAULT,
    ACT_ARG_FLYING_TOTWC
};

enum ActionStatesRidingHoot { // act_riding_hoot
    ACT_STATE_RIDING_HOOT_GRABBING,
    ACT_STATE_RIDING_HOOT_HANGING
};

enum ActionStatesFlyingTripleJump { // act_flying_triple_jump
    ACT_STATE_FLYING_TRIPLE_JUMP_START,
    ACT_STATE_FLYING_TRIPLE_JUMP_SPIN
};

enum ActionStatesVerticalWind { // act_vertical_wind
    ACT_STATE_VERTICAL_WIND_SPINNING,
    ACT_STATE_VERTICAL_WIND_AIRBORNE
};

enum ActionStatesSpecialTripleJump { // act_special_triple_jump
    ACT_STATE_SPECIAL_TRIPLE_JUMP_SPINNING,
    ACT_STATE_SPECIAL_TRIPLE_JUMP_LANDING
};

enum ActionArgsFreefall { // act_freefall
    ACT_ARG_FREEFALL_GENERAL,
    ACT_ARG_FREEFALL_FROM_SLIDE,
    ACT_ARG_FREEFALL_FROM_SLIDE_KICK
};

s32 mario_execute_airborne_action(struct MarioState *m);

#endif // MARIO_ACTIONS_AIRBORNE_H
