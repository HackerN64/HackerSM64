
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

