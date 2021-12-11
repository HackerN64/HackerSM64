#ifndef MARIO_ACTIONS_SUBMERGED_H
#define MARIO_ACTIONS_SUBMERGED_H

#include <PR/ultratypes.h>

#include "types.h"

enum ActionStatesBreaststroke { // act_breaststroke, act_hold_breaststroke
    ACT_STATE_BREASTSTROKE_START,
    ACT_STATE_BREASTSTROKE_CONTINUE
};

enum ActionStatesWaterPunch { // act_water_punch
    ACT_STATE_WATER_PUNCH_START,
    ACT_STATE_WATER_PUNCH_MISSED,
    ACT_STATE_WATER_PUNCH_PICK_UP
};

enum ActionStatesDrowning { // act_drowning
    ACT_STATE_DROWNING_EYES_HALF_CLOSED,
    ACT_STATE_DROWNING_EYES_DEAD
};

enum ActionStatesWaterPlunge { // act_water_plunge
    ACT_STATE_WATER_PLUNGE_FALL,
    ACT_STATE_WATER_PLUNGE_SINK
};

enum WaterPlungeFlags { // act_water_plunge
    PLUNGE_FLAGS_NONE       = (0 << 0), // 0x0
    PLUNGE_FLAG_HOLDING_OBJ = (1 << 0), // 0x1
    PLUNGE_FLAG_DIVING      = (1 << 1), // 0x2
    PLUNGE_FLAG_METAL_CAP   = (1 << 2), // 0x4
};

s32 mario_execute_submerged_action(struct MarioState *m);

#endif // MARIO_ACTIONS_SUBMERGED_H
