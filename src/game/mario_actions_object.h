#ifndef MARIO_ACTIONS_OBJECT_H
#define MARIO_ACTIONS_OBJECT_H

#include <PR/ultratypes.h>

#include "types.h"

// act_punching
#define ACT_PUNCHING_STATE_CAN_JUMP_KICK    0x0
#define ACT_PUNCHING_STATE_NO_JUMP_KICK     0x1
// act_picking_up
#define ACT_PICKING_UP_STATE_GRAB           0x0
#define ACT_PICKING_UP_STATE_HAS_OBJ        0x1
// act_picking_up_bowser
#define ACT_PICKING_UP_BOWSER_STATE_GRAB    0x0
#define ACT_PICKING_UP_BOWSER_STATE_HOLDING 0x1

s32 mario_update_punch_sequence(struct MarioState *m);
s32 mario_execute_object_action(struct MarioState *m);

#endif // MARIO_ACTIONS_OBJECT_H
