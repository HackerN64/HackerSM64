#ifndef MARIO_ACTIONS_MOVING
#define MARIO_ACTIONS_MOVING

#include <PR/ultratypes.h>

#include "types.h"

enum WalkSpeeds {
	WALK_SPEED_START_TIPTOE,
	WALK_SPEED_TIPTOE,
	WALK_SPEED_WALKING,
	WALK_SPEED_RUNNING,
};

enum HoldWalkSpeeds {
	HOLD_WALK_SPEED_SLOW,
	HOLD_WALK_SPEED_WALKING,
	HOLD_WALK_SPEED_RUN,
};

enum ActionStatesWalking { // act_walking
    ACT_STATE_WALKING_NO_WALL,
    ACT_STATE_WALKING_REACH_WALL
};

enum ActionStatesMovePunching { // act_move_punching
    ACT_STATE_MOVE_PUNCHING_CAN_JUMP_KICK,
    ACT_STATE_MOVE_PUNCHING_NO_JUMP_KICK
};

enum ActionStatesPushOrSidleWall { // push_or_sidle_wall
    ACT_STATE_PUSH_OR_SIDLE_WALL_PUSHING,
    ACT_STATE_PUSH_OR_SIDLE_WALL_SIDLING
};

void play_step_sound(struct MarioState *m, s16 frame1, s16 frame2);
s32 mario_execute_moving_action(struct MarioState *m);

#endif // MARIO_ACTIONS_MOVING
