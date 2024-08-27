#include "audio/external.h"
#include "engine/math_util.h"
#include "engine/surface_collision.h"
#include "game/camera.h"
#include "game/level_update.h"
#include "camera_cutscene.h"
#include "camera_math.h"

/**
 * Triggers Mario to enter a dialog state. This is used to make Mario look at the focus of a cutscene,
 * for example, bowser.
 * @param state 0 = stop, 1 = start, 2 = start and look up, and 3 = start and look down
 *
 * @return if Mario left the dialog state, return CUTSCENE_LOOP, else return gCutsceneTimer
 */
s16 cutscene_common_set_dialog_state(s32 state) {
    s16 timer = gCutsceneTimer;
    // If the dialog ended, return CUTSCENE_LOOP, which would end the cutscene shot
    if (set_mario_npc_dialog(state) == MARIO_DIALOG_STATUS_SPEAK) {
        timer = CUTSCENE_LOOP;
    }
    return timer;
}

void cutscene_stop_dialog(UNUSED struct Camera *c) {
    cutscene_common_set_dialog_state(MARIO_DIALOG_STOP);
}

/**
 * Cause Mario to enter the normal dialog state.
 */
void cutscene_mario_dialog(UNUSED struct Camera *c) {
    gCutsceneTimer = cutscene_common_set_dialog_state(MARIO_DIALOG_LOOK_FRONT);
}

/**
 * End the cutscene, used by cutscenes that play when Mario exits a course to castle grounds.
 */
void cutscene_exit_to_castle_grounds_end(struct Camera *c) {
    sStatusFlags |= CAM_FLAG_SMOOTH_MOVEMENT;
    gCutsceneTimer = CUTSCENE_STOP;
    c->cutscene = 0;
    update_camera_yaw(c);
}

/**
 * Start a preset fov shake. Used in cutscenes
 */
void cutscene_set_fov_shake_preset(u8 preset) {
    switch (preset) {
        case 1:
            set_fov_shake(0x100, 0x30, 0x8000);
            break;
        case 2:
            set_fov_shake(0x400, 0x20, 0x4000);
            break;
    }
}

void cutscene_shake_explosion(UNUSED struct Camera *c) {
    set_environmental_camera_shake(SHAKE_ENV_EXPLOSION);
    cutscene_set_fov_shake_preset(1);
}

/**
 * Update the camera's yaw and nextYaw. This is called from cutscenes to ignore the camera mode's yaw.
 * TODO: cutscene_update_camera_yaw
 */
void update_camera_yaw(struct Camera *c) {
    c->nextYaw = calculate_yaw(c->focus, c->pos);
    c->yaw = c->nextYaw;
}

void cutscene_soften_music(UNUSED struct Camera *c) {
    seq_player_lower_volume(SEQ_PLAYER_LEVEL, 60, 40);
}

void cutscene_unsoften_music(UNUSED struct Camera *c) {
    seq_player_unlower_volume(SEQ_PLAYER_LEVEL, 60);
}

void cutscene_reset_spline(void) {
    sCutsceneSplineSegment = 0;
    sCutsceneSplineSegmentProgress = 0;
}

void stop_cutscene_and_retrieve_stored_info(struct Camera *c) {
    gCutsceneTimer = CUTSCENE_STOP;
    c->cutscene = 0;
    vec3f_copy(c->focus, sCameraStoreCutscene.focus);
    vec3f_copy(c->pos, sCameraStoreCutscene.pos);
}

/**
 * Store camera info for the cannon opening cutscene
 */
void store_info_cannon(struct Camera *c) {
    vec3f_copy(sCameraStoreCutscene.pos, c->pos);
    vec3f_copy(sCameraStoreCutscene.focus, c->focus);
    sCameraStoreCutscene.panDist = sPanDistance;
    sCameraStoreCutscene.cannonYOffset = sCannonYOffset;
}

/**
 * Retrieve camera info for the cannon opening cutscene
 */
void retrieve_info_cannon(struct Camera *c) {
    vec3f_copy(c->pos, sCameraStoreCutscene.pos);
    vec3f_copy(c->focus, sCameraStoreCutscene.focus);
    sPanDistance = sCameraStoreCutscene.panDist;
    sCannonYOffset = sCameraStoreCutscene.cannonYOffset;
}

/**
 * Store camera info for the star spawn cutscene
 */
void store_info_star(struct Camera *c) {
    reset_pan_distance(c);
    vec3f_copy(sCameraStoreCutscene.pos, c->pos);
    sCameraStoreCutscene.focus[0] = sMarioCamState->pos[0];
    sCameraStoreCutscene.focus[1] = c->focus[1];
    sCameraStoreCutscene.focus[2] = sMarioCamState->pos[2];
}

/**
 * Retrieve camera info for the star spawn cutscene
 */
void retrieve_info_star(struct Camera *c) {
    vec3f_copy(c->pos, sCameraStoreCutscene.pos);
    vec3f_copy(c->focus, sCameraStoreCutscene.focus);
}

/**
 * Calculates Mario's distance to the floor, or the water level if it is above the floor. Then:
 * `posOff` is set to the distance multiplied by posMul and bounded to [-posBound, posBound]
 * `focOff` is set to the distance multiplied by focMul and bounded to [-focBound, focBound]
 *
 * Notes:
 *      posMul is always 1.0f, focMul is always 0.9f
 *      both ranges are always 200.0f
 *          Since focMul is 0.9, `focOff` is closer to the floor than `posOff`
 *      posOff and focOff are sometimes the same address, which just ignores the pos calculation
 */
void calc_y_to_curr_floor(f32 *posOff, f32 posMul, f32 posBound, f32 *focOff, f32 focMul, f32 focBound) {
    f32 floorHeight = sMarioGeometry.currFloorHeight;
    f32 waterHeight;

    if (!(sMarioCamState->action & ACT_FLAG_METAL_WATER)) {
        //! @bug this should use sMarioGeometry.waterHeight
        if (floorHeight < (waterHeight = find_water_level(sMarioCamState->pos[0], sMarioCamState->pos[2]))) {
            floorHeight = waterHeight;
        }
    }

    if (sMarioCamState->action & ACT_FLAG_ON_POLE) {
        if (sMarioGeometry.currFloorHeight >= gMarioStates[0].usedObj->oPosY && sMarioCamState->pos[1]
                   < 0.7f * gMarioStates[0].usedObj->hitboxHeight + gMarioStates[0].usedObj->oPosY) {
            posBound = 1200;
        }
    }

    *posOff = (floorHeight - sMarioCamState->pos[1]) * posMul;

    if (*posOff > posBound) {
        *posOff = posBound;
    }

    if (*posOff < -posBound) {
        *posOff = -posBound;
    }

    *focOff = (floorHeight - sMarioCamState->pos[1]) * focMul;

    if (*focOff > focBound) {
        *focOff = focBound;
    }

    if (*focOff < -focBound) {
        *focOff = -focBound;
    }
}

/**
 * Set the camera's focus to Mario's position, and add several relative offsets.
 *
 * @param leftRight offset to Mario's left/right, relative to his faceAngle
 * @param yOff y offset
 * @param forwBack offset to Mario's front/back, relative to his faceAngle
 * @param yawOff offset to Mario's faceAngle, changes the direction of `leftRight` and `forwBack`
 */
void set_focus_rel_mario(struct Camera *c, f32 leftRight, f32 yOff, f32 forwBack, s16 yawOff) {
    s16 yaw;
    f32 focFloorYOff;

    calc_y_to_curr_floor(&focFloorYOff, 1.f, 200.f, &focFloorYOff, 0.9f, 200.f);
    yaw = sMarioCamState->faceAngle[1] + yawOff;
    c->focus[2] = sMarioCamState->pos[2] + forwBack * coss(yaw) - leftRight * sins(yaw);
    c->focus[0] = sMarioCamState->pos[0] + forwBack * sins(yaw) + leftRight * coss(yaw);
    c->focus[1] = sMarioCamState->pos[1] + yOff + focFloorYOff;
}

/**
 * Call the event while `start` <= gCutsceneTimer <= `end`
 * If `end` is -1, call for the rest of the shot.
 */
void cutscene_event(CameraEvent event, struct Camera *c, s16 start, s16 end) {
    if (start <= gCutsceneTimer) {
        if (end == -1 || end >= gCutsceneTimer) {
            event(c);
        }
    }
}

/**
 * Set gCutsceneObjSpawn when gCutsceneTimer == `frame`.
 *
 * @see intro_scene.inc.c for details on which objects are spawned.
 */
void cutscene_spawn_obj(u32 obj, s16 frame) {
    if (frame == gCutsceneTimer) {
        gCutsceneObjSpawn = obj;
    }
}

/**
 * Moves the camera towards the cutscene's focus, stored in sCutsceneVars[3].point
 *
 * sCutsceneVars[3].point is used as the target point
 * sCutsceneVars[0].point is used as the current camera focus during the transition
 *
 * @param rotPitch constant pitch offset to add to the camera's focus
 * @param rotYaw constant yaw offset to add to the camera's focus
 */
void cutscene_goto_cvar_pos(struct Camera *c, f32 goalDist, s16 goalPitch, s16 rotPitch, s16 rotYaw) {
    f32 nextDist;
    s16 nextPitch, nextYaw;
    // The next 2 polar coord points are only used in CUTSCENE_PREPARE_CANNON
    f32 cannonDist;
    s16 cannonPitch, cannonYaw;
    f32 curDist;
    s16 curPitch, curYaw;

    vec3f_get_dist_and_angle(sCutsceneVars[3].point, c->pos, &nextDist, &nextPitch, &nextYaw);

    // If over 8000 units away from the cannon, just teleport there
    if ((nextDist > 8000.f) && (c->cutscene == CUTSCENE_PREPARE_CANNON)) {
        nextDist = goalDist * 4.f;
        nextPitch = goalPitch;
        vec3f_copy(sCutsceneVars[0].point, sCutsceneVars[3].point);
        sStatusFlags &= ~CAM_FLAG_SMOOTH_MOVEMENT;
#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
        if (gCurrLevelNum == LEVEL_TTM) {
            nextYaw = atan2s(sCutsceneVars[3].point[2] - c->areaCenZ,
                             sCutsceneVars[3].point[0] - c->areaCenX);
        }
#endif
    } else {
        if (c->cutscene == CUTSCENE_PREPARE_CANNON) {
            vec3f_get_dist_and_angle(c->pos, sCutsceneVars[0].point, &curDist, &curPitch, &curYaw);
            vec3f_get_dist_and_angle(c->pos, sCutsceneVars[3].point, &cannonDist, &cannonPitch, &cannonYaw);
            approach_f32_asymptotic_bool(&curDist, cannonDist, 0.1f);
            approach_s16_asymptotic_bool(&curPitch, cannonPitch, 15);
            approach_s16_asymptotic_bool(&curYaw, cannonYaw, 15);
            // Move the current focus, sCutsceneVars[0].point, in the direction towards the cannon
            vec3f_set_dist_and_angle(c->pos, sCutsceneVars[0].point, curDist, curPitch, curYaw);
        } else {
            approach_vec3f_asymptotic(sCutsceneVars[0].point, sCutsceneVars[3].point, 0.1f, 0.1f, 0.1f);
        }
    }

    approach_f32_asymptotic_bool(&nextDist, goalDist, 0.05f);
    approach_s16_asymptotic_bool(&nextPitch, goalPitch, 0x20);

    vec3f_set_dist_and_angle(sCutsceneVars[3].point, c->pos, nextDist, nextPitch, nextYaw);
    vec3f_copy(c->focus, sCutsceneVars[0].point);

    // Apply the constant rotation given
    pan_camera(c, rotPitch, rotYaw);
    vec3f_get_dist_and_angle(c->pos, c->focus, &nextDist, &nextPitch, &nextYaw);

    if (nextPitch < -0x3000) {
        nextPitch = -0x3000;
    }
    if (nextPitch > 0x3000) {
        nextPitch = 0x3000;
    }

    vec3f_set_dist_and_angle(c->pos, c->focus, nextDist, nextPitch, nextYaw);
}


/**
 * Play the current cutscene until either gCutsceneTimer reaches the max time, or c->cutscene is set to 0
 *
 * Note that CAM_FLAG_SMOOTH_MOVEMENT is cleared while a cutscene is playing, so cutscenes set it for
 * the duration they want the flag to be active.
 */
void play_cutscene(struct Camera *c) {
    s16 cutsceneDuration;
    u8 oldCutscene = c->cutscene;

    sStatusFlags &= ~CAM_FLAG_SMOOTH_MOVEMENT;
    gCameraMovementFlags &= ~CAM_MOVING_INTO_MODE;

#define CUTSCENE(id, cutscene)                                                                            \
    case id:                                                                                              \
        cutsceneDuration = cutscene[sCutsceneShot].duration;                                              \
        cutscene[sCutsceneShot].shot(c);                                                                  \
        break;

    switch (c->cutscene) {
        CUTSCENE(CUTSCENE_STAR_SPAWN,           sCutsceneStarSpawn)
        CUTSCENE(CUTSCENE_RED_COIN_STAR_SPAWN,  sCutsceneRedCoinStarSpawn)
        CUTSCENE(CUTSCENE_ENDING,               sCutsceneEnding)
        CUTSCENE(CUTSCENE_GRAND_STAR,           sCutsceneGrandStar)
        CUTSCENE(CUTSCENE_DOOR_WARP,            sCutsceneDoorWarp)
        CUTSCENE(CUTSCENE_DOOR_PULL,            sCutsceneDoorPull)
        CUTSCENE(CUTSCENE_DOOR_PUSH,            sCutsceneDoorPush)
        CUTSCENE(CUTSCENE_DOOR_PULL_MODE,       sCutsceneDoorPullMode)
        CUTSCENE(CUTSCENE_DOOR_PUSH_MODE,       sCutsceneDoorPushMode)
        CUTSCENE(CUTSCENE_ENTER_CANNON,         sCutsceneEnterCannon)
        CUTSCENE(CUTSCENE_ENTER_PAINTING,       sCutsceneEnterPainting)
        CUTSCENE(CUTSCENE_DEATH_EXIT,           sCutsceneDeathExit)
        CUTSCENE(CUTSCENE_EXIT_PAINTING_SUCC,   sCutsceneExitPaintingSuccess)
        CUTSCENE(CUTSCENE_UNUSED_EXIT,          sCutsceneUnusedExit)
        CUTSCENE(CUTSCENE_INTRO_PEACH,          sCutsceneIntroPeach)
        CUTSCENE(CUTSCENE_ENTER_BOWSER_ARENA,   sCutsceneEnterBowserArena)
        CUTSCENE(CUTSCENE_DANCE_ROTATE,         sCutsceneDanceDefaultRotate)
        CUTSCENE(CUTSCENE_DANCE_DEFAULT,        sCutsceneDanceDefaultRotate)
        CUTSCENE(CUTSCENE_DANCE_FLY_AWAY,       sCutsceneDanceFlyAway)
        CUTSCENE(CUTSCENE_DANCE_CLOSEUP,        sCutsceneDanceCloseup)
        CUTSCENE(CUTSCENE_KEY_DANCE,            sCutsceneKeyDance)
        CUTSCENE(CUTSCENE_0F_UNUSED,            sCutsceneUnused)
        CUTSCENE(CUTSCENE_END_WAVING,           sCutsceneEndWaving)
        CUTSCENE(CUTSCENE_CREDITS,              sCutsceneCredits)
        CUTSCENE(CUTSCENE_CAP_SWITCH_PRESS,     sCutsceneCapSwitchPress)
        CUTSCENE(CUTSCENE_SLIDING_DOORS_OPEN,   sCutsceneSlidingDoorsOpen)
        CUTSCENE(CUTSCENE_PREPARE_CANNON,       sCutscenePrepareCannon)
        CUTSCENE(CUTSCENE_UNLOCK_KEY_DOOR,      sCutsceneUnlockKeyDoor)
        CUTSCENE(CUTSCENE_STANDING_DEATH,       sCutsceneStandingDeath)
        CUTSCENE(CUTSCENE_ENTER_POOL,           sCutsceneEnterPool)
        CUTSCENE(CUTSCENE_DEATH_ON_STOMACH,     sCutsceneDeathStomach)
        CUTSCENE(CUTSCENE_DEATH_ON_BACK,        sCutsceneDeathOnBack)
        CUTSCENE(CUTSCENE_QUICKSAND_DEATH,      sCutsceneQuicksandDeath)
        CUTSCENE(CUTSCENE_SUFFOCATION_DEATH,    sCutsceneSuffocation)
        CUTSCENE(CUTSCENE_EXIT_BOWSER_SUCC,     sCutsceneExitBowserSuccess)
        CUTSCENE(CUTSCENE_EXIT_BOWSER_DEATH,    sCutsceneExitBowserDeath)
        CUTSCENE(CUTSCENE_EXIT_SPECIAL_SUCC,    sCutsceneExitSpecialSuccess)
        CUTSCENE(CUTSCENE_EXIT_WATERFALL,       sCutsceneExitWaterfall)
        CUTSCENE(CUTSCENE_EXIT_FALL_WMOTR,      sCutsceneFallToCastleGrounds)
        CUTSCENE(CUTSCENE_NONPAINTING_DEATH,    sCutsceneNonPaintingDeath)
        CUTSCENE(CUTSCENE_DIALOG,               sCutsceneDialog)
        CUTSCENE(CUTSCENE_READ_MESSAGE,         sCutsceneReadMessage)
        CUTSCENE(CUTSCENE_RACE_DIALOG,          sCutsceneDialog)
        CUTSCENE(CUTSCENE_ENTER_PYRAMID_TOP,    sCutsceneEnterPyramidTop)
        CUTSCENE(CUTSCENE_SSL_PYRAMID_EXPLODE,  sCutscenePyramidTopExplode)
    }

#undef CUTSCENE

    if ((cutsceneDuration != 0) && !(gCutsceneTimer & CUTSCENE_STOP)) {
        if (gCutsceneTimer < CUTSCENE_LOOP) {
            gCutsceneTimer++;
        }
        //! Because gCutsceneTimer is often set to 0x7FFF (CUTSCENE_LOOP), this conditional can only
        //! check for == due to overflow
        if (gCutsceneTimer == cutsceneDuration) {
            sCutsceneShot++;
            gCutsceneTimer = 0;
        }
    } else {
        sMarioCamState->cameraEvent = CAM_EVENT_NONE;
        sCutsceneShot = 0;
        gCutsceneTimer = 0;
    }

    sAreaYawChange = 0;

    // The cutscene just ended
    if ((c->cutscene == CUTSCENE_NONE) && (oldCutscene != 0)) {
        gRecentCutscene = oldCutscene;
    }
}

