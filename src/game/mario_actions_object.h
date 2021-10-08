#ifndef MARIO_ACTIONS_OBJECT_H
#define MARIO_ACTIONS_OBJECT_H

#include <PR/ultratypes.h>

#include "types.h"

enum ActionStatesPunching { // act_punching
    ACT_STATE_PUNCHING_CAN_JUMP_KICK,
    ACT_STATE_PUNCHING_NO_JUMP_KICK
};

enum ActionStatesPickingUp { // act_picking_up, act_picking_up_bowser
    ACT_STATE_PICKING_UP_GRAB,
    ACT_STATE_PICKING_UP_HOLDING
};

enum ActionArgsPunchSequence { // mario_update_punch_sequence
    ACT_ARG_PUNCH_SEQUENCE_YAH,
    ACT_ARG_PUNCH_SEQUENCE_FIRST_PUNCH,
    ACT_ARG_PUNCH_SEQUENCE_FIRST_PUNCH_FAST,
    ACT_ARG_PUNCH_SEQUENCE_WAH,
    ACT_ARG_PUNCH_SEQUENCE_SECOND_PUNCH,
    ACT_ARG_PUNCH_SEQUENCE_SECOND_PUNCH_FAST,
    ACT_ARG_PUNCH_SEQUENCE_GROUND_KICK,
    ACT_ARG_PUNCH_SEQUENCE_BREAKDANCE = 9,
};

s32 mario_update_punch_sequence(struct MarioState *m);
s32 mario_execute_object_action(struct MarioState *m);

#endif // MARIO_ACTIONS_OBJECT_H
