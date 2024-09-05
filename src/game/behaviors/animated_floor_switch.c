#include <ultra64.h>
#include "behavior_data.h"
#include "global_object_fields.h"
#include "game/object_helpers.h"
#include "game/spawn_sound.h"
#include "levels/bitdw/header.h"
#include "levels/bits/header.h"
#include "levels/rr/header.h"

/* Floor Switch Press Animation */
#define /*0x0F4*/ oFloorSwitchPressAnimationTickTimer        OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oFloorSwitchPressAnimationDoubleFrame      OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oFloorSwitchPressAnimationDoResetTime      OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oFloorSwitchPressAnimationSwitchNotTicking OBJECT_FIELD_S32(0x1E)

struct FloorSwitchTriggeredAnimationFrame {
    const Collision *collision;
    ModelID16 model;
};

struct FloorSwitchTriggeredAnimationFrame sFloorSwitchTriggeredAnimationFrames[][5] = {
    {
        { bits_seg7_collision_0701B734, MODEL_BITS_STAIRCASE_FRAME4 },
        { bits_seg7_collision_0701B59C, MODEL_BITS_STAIRCASE_FRAME3 },
        { bits_seg7_collision_0701B404, MODEL_BITS_STAIRCASE_FRAME2 },
        { bits_seg7_collision_0701B26C, MODEL_BITS_STAIRCASE_FRAME1 },
        { bits_seg7_collision_0701B0D4, MODEL_BITS_STAIRCASE },
    },
    {
        { bitdw_seg7_collision_0700FD9C, MODEL_BITDW_STAIRCASE },
        { bitdw_seg7_collision_0700FC7C, MODEL_BITDW_STAIRCASE_FRAME1 },
        { bitdw_seg7_collision_0700FB5C, MODEL_BITDW_STAIRCASE_FRAME2 },
        { bitdw_seg7_collision_0700FA3C, MODEL_BITDW_STAIRCASE_FRAME3 },
        { bitdw_seg7_collision_0700F91C, MODEL_BITDW_STAIRCASE_FRAME4 },
    },
    {
        { rr_seg7_collision_0702A6B4, MODEL_RR_TRICKY_TRIANGLES_FRAME4 },
        { rr_seg7_collision_0702A32C, MODEL_RR_TRICKY_TRIANGLES_FRAME3 },
        { rr_seg7_collision_07029FA4, MODEL_RR_TRICKY_TRIANGLES_FRAME2 },
        { rr_seg7_collision_07029C1C, MODEL_RR_TRICKY_TRIANGLES_FRAME1 },
        { rr_seg7_collision_07029924, MODEL_RR_TRICKY_TRIANGLES },
    },
};

s16 sAnimatesOnFloorSwitchPressTimers[] = { 250, 200, 200 };

void bhv_animates_on_floor_switch_press_init(void) {
    o->parentObj = cur_obj_nearest_object_with_behavior(bhvFloorSwitchAnimatesObject);
}

void bhv_animates_on_floor_switch_press_loop(void) {
    if (o->oFloorSwitchPressAnimationSwitchNotTicking != 0) {
        if (o->parentObj->oAction != 2) {
            o->oFloorSwitchPressAnimationSwitchNotTicking = 0;
        }

        if (o->oFloorSwitchPressAnimationDoResetTime != 0) {
            o->oFloorSwitchPressAnimationTickTimer = sAnimatesOnFloorSwitchPressTimers[o->oBehParams2ndByte];
        } else {
            o->oFloorSwitchPressAnimationTickTimer = 0;
        }
    } else if (o->parentObj->oAction == 2) {
        o->oFloorSwitchPressAnimationDoResetTime ^= 1;
        o->oFloorSwitchPressAnimationSwitchNotTicking = 1;
    }

    if (o->oFloorSwitchPressAnimationTickTimer != 0) {
        if (o->oFloorSwitchPressAnimationTickTimer < 60) {
            cur_obj_play_sound_1(SOUND_GENERAL2_SWITCH_TICK_SLOW);
        } else {
            cur_obj_play_sound_1(SOUND_GENERAL2_SWITCH_TICK_FAST);
        }

        if (--o->oFloorSwitchPressAnimationTickTimer == 0) {
            o->oFloorSwitchPressAnimationDoResetTime = 0;
        }

        if (o->oFloorSwitchPressAnimationDoubleFrame < 9) {
            o->oFloorSwitchPressAnimationDoubleFrame++;
        }
    } else if ((o->oFloorSwitchPressAnimationDoubleFrame -= 2) < 0) {
        o->oFloorSwitchPressAnimationDoubleFrame = 0;
        o->oFloorSwitchPressAnimationDoResetTime = 1;
    }

    o->collisionData = segmented_to_virtual(
        sFloorSwitchTriggeredAnimationFrames[o->oBehParams2ndByte][o->oFloorSwitchPressAnimationDoubleFrame / 2].collision);

    cur_obj_set_model(sFloorSwitchTriggeredAnimationFrames[o->oBehParams2ndByte][o->oFloorSwitchPressAnimationDoubleFrame / 2].model);
}
