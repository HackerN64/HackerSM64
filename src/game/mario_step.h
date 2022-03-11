#ifndef MARIO_STEP_H
#define MARIO_STEP_H

#include <PR/ultratypes.h>

#include "types.h"

enum GroundStep {
    GROUND_STEP_LEFT_GROUND,
    GROUND_STEP_NONE,
    GROUND_STEP_HIT_WALL,
    GROUND_STEP_HIT_WALL_STOP_QSTEPS = GROUND_STEP_HIT_WALL,
    GROUND_STEP_HIT_WALL_CONTINUE_QSTEPS
};

enum AirStepCheck {
    AIR_STEP_CHECK_NONE,
    AIR_STEP_CHECK_LEDGE_GRAB,
    AIR_STEP_CHECK_HANG
};

enum AirStep {
    AIR_STEP_NONE,
    AIR_STEP_LANDED,
    AIR_STEP_HIT_WALL,
    AIR_STEP_GRABBED_LEDGE,
    AIR_STEP_GRABBED_CEILING,
    AIR_STEP_UNK,
    AIR_STEP_HIT_LAVA_WALL,
    AIR_STEP_HIT_CEILING
};

enum WaterStep {
    WATER_STEP_NONE,
    WATER_STEP_HIT_FLOOR,
    WATER_STEP_HIT_CEILING,
    WATER_STEP_CANCELLED,
    WATER_STEP_HIT_WALL
};

struct BullyCollisionData {
    /*0x00*/ f32 conversionRatio;
    /*0x04*/ f32 radius;
    /*0x08*/ f32 posX;
    /*0x0C*/ f32 posZ;
    /*0x10*/ f32 velX;
    /*0x14*/ f32 velZ;
};

extern struct Surface gWaterSurfacePseudoFloor;

f32 get_additive_y_vel_for_jumps(void);
void stub_mario_step_1(struct MarioState *m);
void stub_mario_step_2(void);

void mario_bonk_reflection(struct MarioState *m, u32 negateSpeed);
void transfer_bully_speed(struct BullyCollisionData *obj1, struct BullyCollisionData *obj2);
void init_bully_collision_data(struct BullyCollisionData *data, f32 posX, f32 posZ,
                                          f32 forwardVel, s16 yaw, f32 conversionRatio, f32 radius);
u32 mario_update_quicksand(struct MarioState *m, f32 sinkingSpeed);
u32 mario_push_off_steep_floor(struct MarioState *m, u32 action, u32 actionArg);
u32 mario_update_moving_sand(struct MarioState *m);
u32 mario_update_windy_ground(struct MarioState *m);
void stop_and_set_height_to_floor(struct MarioState *m);
s32 stationary_ground_step(struct MarioState *m);
s32 perform_ground_step(struct MarioState *m);
s32 is_ungrabbable_floor(struct Surface *floor);
s32 perform_air_step(struct MarioState *m, u32 stepArg);

#endif // MARIO_STEP_H
