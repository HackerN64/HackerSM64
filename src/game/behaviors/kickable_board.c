#include <ultra64.h>
#include "global_object_fields.h"
#include "engine/math_util.h"
#include "engine/surface_load.h"
#include "game/interaction.h"
#include "game/object_helpers.h"
#include "game/spawn_sound.h"

/* Kickable Board */
#define /*0x0F4*/ oKickableBoardRockingAngleAmount OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oKickableBoardRockingTimer       OBJECT_FIELD_S32(0x1C)

void init_kickable_board_rock(void) {
    o->oKickableBoardRockingTimer = 1600;
    o->oKickableBoardRockingAngleAmount = 0;
}

void bhv_kickable_board_loop(void) {
    s32 attackValue;

    switch (o->oAction) {
        case KICKABLE_BOARD_ACT_IDLE_VERTICAL:
            o->oFaceAnglePitch = 0;

            if (check_mario_attacking()) {
                init_kickable_board_rock();
                o->oAction++;
            }

            load_object_collision_model();
            break;

        case KICKABLE_BOARD_ACT_ROCKING:
            o->oFaceAnglePitch = 0;
            load_object_collision_model();
            o->oFaceAnglePitch = -sins(o->oKickableBoardRockingAngleAmount) * o->oKickableBoardRockingTimer;

            if (o->oTimer > 30) {
                attackValue = check_mario_attacking();
                if (attackValue) {
                    if (gMarioObject->oPosY > o->oPosY + 160.0f && attackValue == WF_ATTACK_AIR) {
                        o->oAction++;
                        cur_obj_play_sound_2(SOUND_GENERAL_BUTTON_PRESS_2);
                    } else {
                        o->oTimer = 0;
                    }
                }
            }

            if (o->oTimer != 0) {
                o->oKickableBoardRockingTimer -= 8;
                if (o->oKickableBoardRockingTimer < 0) {
                    o->oAction = KICKABLE_BOARD_ACT_IDLE_VERTICAL;
                }
            } else {
                init_kickable_board_rock();
            }

            if (!(o->oKickableBoardRockingAngleAmount & 0x7FFF)) {
                cur_obj_play_sound_2(SOUND_GENERAL_BUTTON_PRESS_2);
            }

            o->oKickableBoardRockingAngleAmount += 0x400;
            break;

        case KICKABLE_BOARD_ACT_FALLING:
            cur_obj_become_intangible();
            cur_obj_set_model(MODEL_WF_KICKABLE_BOARD_FELLED);

            o->oAngleVelPitch -= 0x80;
            o->oFaceAnglePitch += o->oAngleVelPitch;

            if (o->oFaceAnglePitch < -0x4000) {
                o->oFaceAnglePitch = -0x4000;
                o->oAngleVelPitch = 0;
                o->oAction++;

                cur_obj_shake_screen(SHAKE_POS_SMALL);
                cur_obj_play_sound_2(SOUND_GENERAL_KICKABLE_BOARD_FALL);
            }

            load_object_collision_model();
            break;

        case KICKABLE_BOARD_ACT_IDLE_HORIZONTAL:
            load_object_collision_model();
            break;
    }

    o->header.gfx.throwMatrix = NULL;
}
