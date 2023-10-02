#include <ultra64.h>
#include "global_object_fields.h"
#include "object_helpers.h"

#define /*0x0F4*/ oYoshiBlinkTimer OBJECT_FIELD_S32(0x1B)
#define /*0x0FC*/ oYoshiChosenHome OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oYoshiTargetYaw  OBJECT_FIELD_S32(0x1E)
/*Custom general defines:
s16 variables are also supported, and using them effectly can double the number of available members. The full list of s16 defines is:
#define /*0x0F4*/ oF4                                           OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oF8                                           OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oFC                                           OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ o100                                          OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ o104                                          OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ o108                                          OBJECT_FIELD_S32(0x20)
#define /*0x10C*/ o10C                                          OBJECT_FIELD_S32(0x21)
#define /*0x110*/ o110                                          OBJECT_FIELD_S32(0x22)
#define /*0x0F4*/ oFloatF4                                      OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oFloatF8                                      OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oFloatFC                                      OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oFloat100                                     OBJECT_FIELD_F32(0x1E)
#define /*0x104*/ oFloat104                                     OBJECT_FIELD_F32(0x1F)
#define /*0x108*/ oFloat108                                     OBJECT_FIELD_F32(0x20)
#define /*0x10C*/ oFloat10C                                     OBJECT_FIELD_F32(0x21)
#define /*0x110*/ oFloat110                                     OBJECT_FIELD_F32(0x22)
#define /*0x0F4*/ oObjF4                                        OBJECT_FIELD_OBJ(0x1B)
#define /*0x0F8*/ oObjF8                                        OBJECT_FIELD_OBJ(0x1C)
#define /*0x0FC*/ oObjFC                                        OBJECT_FIELD_OBJ(0x1D)
#define /*0x100*/ oObj100                                       OBJECT_FIELD_OBJ(0x1E)
#define /*0x104*/ oObj104                                       OBJECT_FIELD_OBJ(0x1F)
#define /*0x108*/ oObj108                                       OBJECT_FIELD_OBJ(0x20)
#define /*0x10C*/ oObj10C                                       OBJECT_FIELD_OBJ(0x21)
#define /*0x110*/ oObj110                                       OBJECT_FIELD_OBJ(0x22)
#define /*0x0F4*/ oSurfF4                                   OBJECT_FIELD_SURFACE(0x1B)
#define /*0x0F8*/ oSurfF8                                   OBJECT_FIELD_SURFACE(0x1C)
#define /*0x0FC*/ oSurfFC                                   OBJECT_FIELD_SURFACE(0x1D)
#define /*0x100*/ oSurf100                                  OBJECT_FIELD_SURFACE(0x1E)
#define /*0x104*/ oSurf104                                  OBJECT_FIELD_SURFACE(0x1F)
#define /*0x108*/ oSurf108                                  OBJECT_FIELD_SURFACE(0x20)
#define /*0x10C*/ oSurf10C                                  OBJECT_FIELD_SURFACE(0x21)
#define /*0x110*/ oSurf110                                  OBJECT_FIELD_SURFACE(0x22)
#define /*0x0F4*/ os16F4                                        OBJECT_FIELD_S16(0x1B, 0)
#define /*0x0F6*/ os16F6                                        OBJECT_FIELD_S16(0x1B, 1)
#define /*0x0F8*/ os16F8                                        OBJECT_FIELD_S16(0x1C, 0)
#define /*0x0FA*/ os16FA                                        OBJECT_FIELD_S16(0x1C, 1)
#define /*0x0FC*/ os16FC                                        OBJECT_FIELD_S16(0x1D, 0)
#define /*0x0FE*/ os16FE                                        OBJECT_FIELD_S16(0x1D, 1)
#define /*0x100*/ os16100                                       OBJECT_FIELD_S16(0x1E, 0)
#define /*0x102*/ os16102                                       OBJECT_FIELD_S16(0x1E, 1)
#define /*0x104*/ os16104                                       OBJECT_FIELD_S16(0x1F, 0)
#define /*0x106*/ os16106                                       OBJECT_FIELD_S16(0x1F, 1)
#define /*0x108*/ os16108                                       OBJECT_FIELD_S16(0x20, 0)
#define /*0x10A*/ os1610A                                       OBJECT_FIELD_S16(0x20, 1)
#define /*0x10C*/ os1610C                                       OBJECT_FIELD_S16(0x21, 0)
#define /*0x10E*/ os1610E                                       OBJECT_FIELD_S16(0x21, 1)
#define /*0x110*/ os16110                                       OBJECT_FIELD_S16(0x22, 0)
#define /*0x112*/ os16112                                       OBJECT_FIELD_S16(0x22, 1)
// yoshi.inc.c

// X/Z coordinates of Yoshi's homes that he switches between.
// Note that this doesn't contain the Y coordinate since the castle roof is flat,
// so o->oHomeY is never updated.
static s16 sYoshiHomeLocations[] = { 0, -5625, -1364, -5912, -1403, -4609, -1004, -5308 };

void bhv_yoshi_init(void) {
    o->oGravity = 2.0f;
    o->oFriction = 0.9f;
    o->oBuoyancy = 1.3f;
    o->oInteractionSubtype = INT_SUBTYPE_NPC;

#if !defined(ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS) || defined(UNLOCK_ALL)
    if (sYoshiDead == TRUE) {
#else
    if ((save_file_get_total_star_count(gCurrSaveFileNum - 1, COURSE_NUM_TO_INDEX(COURSE_MIN), COURSE_NUM_TO_INDEX(COURSE_MAX)) < 120)
        || sYoshiDead == TRUE) {
#endif
        o->activeFlags = ACTIVE_FLAG_DEACTIVATED;
    }
}

void yoshi_walk_loop(void) {
    s16 animFrame = o->header.gfx.animInfo.animFrame;

    o->oForwardVel = 10.0f;
    object_step();
    o->oMoveAngleYaw = approach_s16_symmetric(o->oMoveAngleYaw, o->oYoshiTargetYaw, 0x500);

    if (is_point_close_to_object(o, o->oHomeX, 3174.0f, o->oHomeZ, 200)) {
        o->oAction = YOSHI_ACT_IDLE;
    }

    cur_obj_init_animation(1);

    if (animFrame == 0 || animFrame == 15) {
        cur_obj_play_sound_2(SOUND_GENERAL_YOSHI_WALK);
    }

    if (o->oInteractStatus == INT_STATUS_INTERACTED) {
        o->oAction = YOSHI_ACT_TALK;
    }

    if (o->oPosY < 2100.0f) {
        create_respawner(MODEL_YOSHI, bhvYoshi, 3000);
        o->activeFlags = ACTIVE_FLAG_DEACTIVATED;
    }
}

void yoshi_idle_loop(void) {
    if (o->oTimer > 90) {
        s16 chosenHome = random_float() * 3.99f;

        if (o->oYoshiChosenHome == chosenHome) {
            return;
        } else {
            o->oYoshiChosenHome = chosenHome;
        }

        o->oHomeX = sYoshiHomeLocations[o->oYoshiChosenHome * 2];
        o->oHomeZ = sYoshiHomeLocations[o->oYoshiChosenHome * 2 + 1];
        o->oYoshiTargetYaw = atan2s(o->oHomeZ - o->oPosZ, o->oHomeX - o->oPosX);
        o->oAction = YOSHI_ACT_WALK;
    }

    cur_obj_init_animation(0);

    if (o->oInteractStatus == INT_STATUS_INTERACTED) {
        o->oAction = YOSHI_ACT_TALK;
    }

    // Credits; Yoshi appears at this position overlooking the castle near the end of the credits
    if (gPlayerCameraState->cameraEvent == CAM_EVENT_START_ENDING ||
        gPlayerCameraState->cameraEvent == CAM_EVENT_START_END_WAVING) {
        o->oAction = YOSHI_ACT_CREDITS;
        o->oPosX = -1798.0f;
        o->oPosY = 3174.0f;
        o->oPosZ = -3644.0f;
    }
}

void yoshi_talk_loop(void) {
    if ((s16) o->oMoveAngleYaw == (s16) o->oAngleToMario) {
        cur_obj_init_animation(0);
        if (set_mario_npc_dialog(MARIO_DIALOG_LOOK_FRONT) == MARIO_DIALOG_STATUS_SPEAK) {
            o->activeFlags |= ACTIVE_FLAG_INITIATED_TIME_STOP;
            if (cutscene_object_with_dialog(CUTSCENE_DIALOG, o, DIALOG_161) != 0) {
                o->activeFlags &= ~ACTIVE_FLAG_INITIATED_TIME_STOP;
                o->oInteractStatus = INT_STATUS_NONE;
                o->oHomeX = sYoshiHomeLocations[2];
                o->oHomeZ = sYoshiHomeLocations[3];
                o->oYoshiTargetYaw = atan2s(o->oHomeZ - o->oPosZ, o->oHomeX - o->oPosX);
                o->oAction = YOSHI_ACT_GIVE_PRESENT;
            }
        }
    } else {
        cur_obj_init_animation(1);
        play_puzzle_jingle();
        o->oMoveAngleYaw = approach_s16_symmetric(o->oMoveAngleYaw, o->oAngleToMario, 0x500);
    }
}

void yoshi_walk_and_jump_off_roof_loop(void) {
    s16 animFrame = o->header.gfx.animInfo.animFrame;

    o->oForwardVel = 10.0f;
    object_step();
    cur_obj_init_animation(1);

    if (o->oTimer == 0) {
        cutscene_object(CUTSCENE_STAR_SPAWN, o);
    }

    o->oMoveAngleYaw = approach_s16_symmetric(o->oMoveAngleYaw, o->oYoshiTargetYaw, 0x500);

    if (is_point_close_to_object(o, o->oHomeX, 3174.0f, o->oHomeZ, 200)) {
        cur_obj_init_animation(2);
        cur_obj_play_sound_2(SOUND_GENERAL_ENEMY_ALERT1);
        o->oForwardVel = 50.0f;
        o->oVelY = 40.0f;
        o->oMoveAngleYaw = -0x3FFF;
        o->oAction = YOSHI_ACT_FINISH_JUMPING_AND_DESPAWN;
    }

    if (animFrame == 0 || animFrame == 15) {
        cur_obj_play_sound_2(SOUND_GENERAL_YOSHI_WALK);
    }
}

void yoshi_finish_jumping_and_despawn_loop(void) {
    cur_obj_extend_animation_if_at_end();
    obj_move_xyz_using_fvel_and_yaw(o);

    o->oVelY -= 2.0f;

    if (o->oPosY < 2100.0f) {
        set_mario_npc_dialog(MARIO_DIALOG_STOP);
        gObjCutsceneDone = TRUE;
        sYoshiDead = TRUE;
        o->activeFlags = ACTIVE_FLAG_DEACTIVATED;
    }
}

void yoshi_give_present_loop(void) {
#ifdef ENABLE_LIVES
    s32 globalTimer = gGlobalTimer;

    if (gHudDisplay.lives == MAX_NUM_LIVES) {
        play_sound(SOUND_GENERAL_COLLECT_1UP, gGlobalSoundSource);
        gSpecialTripleJump = TRUE;
        o->oAction = YOSHI_ACT_WALK_JUMP_OFF_ROOF;
        return;
    }

    if (!(globalTimer & 3)) {
        play_sound(SOUND_MENU_YOSHI_GAIN_LIVES, gGlobalSoundSource);
        gMarioState->numLives++;
    }
#else
    play_sound(SOUND_GENERAL_COLLECT_1UP, gGlobalSoundSource);
    gSpecialTripleJump = TRUE;
    o->oAction = YOSHI_ACT_WALK_JUMP_OFF_ROOF;
    return;
#endif
}

void bhv_yoshi_loop(void) {
    switch (o->oAction) {
        case YOSHI_ACT_IDLE:
            yoshi_idle_loop();
            break;

        case YOSHI_ACT_WALK:
            yoshi_walk_loop();
            break;

        case YOSHI_ACT_TALK:
            yoshi_talk_loop();
            break;

        case YOSHI_ACT_WALK_JUMP_OFF_ROOF:
            yoshi_walk_and_jump_off_roof_loop();
            break;

        case YOSHI_ACT_FINISH_JUMPING_AND_DESPAWN:
            yoshi_finish_jumping_and_despawn_loop();
            break;

        case YOSHI_ACT_GIVE_PRESENT:
            yoshi_give_present_loop();
            break;

        case YOSHI_ACT_CREDITS:
            cur_obj_init_animation(0);
            break;
    }

    curr_obj_random_blink(&o->oYoshiBlinkTimer);
}
