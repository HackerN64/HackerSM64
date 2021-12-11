#ifndef MARIO_ACTIONS_AUTOMATIC_H
#define MARIO_ACTIONS_AUTOMATIC_H

#include <PR/ultratypes.h>

#include "types.h"

enum ActionStatesInCannon { // act_in_cannon
    ACT_STATE_IN_CANNON_INIT,
    ACT_STATE_IN_CANNON_WAIT_FOR_CANNON,
    ACT_STATE_IN_CANNON_READY
};

enum PolePositions { // set_pole_position
    POLE_NONE,
    POLE_TOUCHED_FLOOR,
    POLE_FELL_OFF
};

enum HangingSteps { // perform_hanging_step
    HANG_NONE,
    HANG_HIT_CEIL_OR_OOB,
    HANG_LEFT_CEIL
};

s32 mario_execute_automatic_action(struct MarioState *m);

#endif // MARIO_ACTIONS_AUTOMATIC_H
