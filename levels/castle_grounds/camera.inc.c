struct CutsceneSplinePoint sIntroStartToPipePosition[] = {
    { 0, 0, { 2122, 8762, 9114 } },  { 0, 0, { 2122, 8762, 9114 } },  { 1, 0, { 2122, 7916, 9114 } },
    { 1, 0, { 2122, 7916, 9114 } },  { 2, 0, { 957, 5166, 8613 } },   { 3, 0, { 589, 4338, 7727 } },
    { 4, 0, { 690, 3366, 6267 } },   { 5, 0, { -1600, 2151, 4955 } }, { 6, 0, { -1557, 232, 1283 } },
    { 7, 0, { -6962, -295, 2729 } }, { 8, 0, { -6979, 131, 3246 } },  { 9, 0, { -6360, -283, 4044 } },
    { 0, 0, { -5695, -334, 5264 } }, { 1, 0, { -5568, -319, 7933 } }, { 2, 0, { -3848, -200, 6278 } },
    { 3, 0, { -965, -263, 6092 } },  { 4, 0, { 1607, 2465, 6329 } },  { 5, 0, { 2824, 180, 3548 } },
    { 6, 0, { 1236, 136, 945 } },    { 0, 0, { 448, 136, 564 } },     { 0, 0, { 448, 136, 564 } },
    { 0, 0, { 448, 136, 564 } },     { -1, 0, { 448, 136, 564 } }
};

struct CutsceneSplinePoint sIntroStartToPipeFocus[] = {
    { 0, 50, { 1753, 29800, 8999 } }, { 0, 50, { 1753, 29800, 8999 } },
    { 1, 50, { 1753, 8580, 8999 } },  { 1, 100, { 1753, 8580, 8999 } },
    { 2, 50, { 520, 5400, 8674 } },   { 3, 50, { 122, 4437, 7875 } },
    { 4, 50, { 316, 3333, 6538 } },   { 5, 36, { -1526, 2189, 5448 } },
    { 6, 50, { -1517, 452, 1731 } },  { 7, 50, { -6659, -181, 3109 } },
    { 8, 17, { -6649, 183, 3618 } },  { 9, 20, { -6009, -214, 4395 } },
    { 0, 50, { -5258, -175, 5449 } }, { 1, 36, { -5158, -266, 7651 } },
    { 2, 26, { -3351, -192, 6222 } }, { 3, 25, { -483, -137, 6060 } },
    { 4, 100, { 1833, 2211, 5962 } }, { 5, 26, { 3022, 207, 3090 } },
    { 6, 20, { 1250, 197, 449 } },    { 7, 50, { 248, 191, 227 } },
    { 7, 0, { 48, 191, 227 } },       { 7, 0, { 48, 191, 227 } },
    { -1, 0, { 48, 191, 227 } }
};

/**
 * Describes the spline the camera follows, starting when the camera jumps to Lakitu and ending after
 * Mario jumps out of the pipe when the first dialog opens.  This table specifically updates the
 * camera's position.
 */
struct CutsceneSplinePoint sIntroPipeToDialogPosition[] = {
    { 0, 0, { -785, 625, 4527 } },  { 1, 0, { -785, 625, 4527 } },  { 2, 0, { -1286, 644, 4376 } },
    { 3, 0, { -1286, 623, 4387 } }, { 4, 0, { -1286, 388, 3963 } }, { 5, 0, { -1286, 358, 4093 } },
    { 6, 0, { -1386, 354, 4159 } }, { 7, 0, { -1477, 306, 4223 } }, { 8, 0, { -1540, 299, 4378 } },
    { 9, 0, { -1473, 316, 4574 } }, { 0, 0, { -1328, 485, 5017 } }, { 0, 0, { -1328, 485, 5017 } },
    { 0, 0, { -1328, 485, 5017 } }, { -1, 0, { -1328, 485, 5017 } }
};

/**
 * Describes the spline that the camera's focus follows, during the same part of the intro as the above.
 */
#ifdef VERSION_EU
struct CutsceneSplinePoint sIntroPipeToDialogFocus[] = {
    { 0, 25, { -1248, 450, 4596 } }, { 1, 71, { -1258, 485, 4606 } }, { 2, 71, { -1379, 344, 4769 } },
    { 3, 22, { -1335, 366, 4815 } }, { 4, 23, { -1315, 370, 4450 } }, { 5, 40, { -1322, 333, 4591 } },
    { 6, 25, { -1185, 329, 4616 } }, { 7, 21, { -1059, 380, 4487 } }, { 8, 14, { -1086, 421, 4206 } },
    { 9, 21, { -1321, 346, 4098 } }, { 0, 0, { -1328, 385, 4354 } },  { 0, 0, { -1328, 385, 4354 } },
    { 0, 0, { -1328, 385, 4354 } },  { -1, 0, { -1328, 385, 4354 } }
};
#else
struct CutsceneSplinePoint sIntroPipeToDialogFocus[] = {
    { 0, 20, { -1248, 450, 4596 } }, { 1, 59, { -1258, 485, 4606 } }, { 2, 59, { -1379, 344, 4769 } },
    { 3, 20, { -1335, 366, 4815 } }, { 4, 23, { -1315, 370, 4450 } }, { 5, 40, { -1322, 333, 4591 } },
    { 6, 25, { -1185, 329, 4616 } }, { 7, 21, { -1059, 380, 4487 } }, { 8, 14, { -1086, 421, 4206 } },
    { 9, 21, { -1321, 346, 4098 } }, { 0, 0, { -1328, 385, 4354 } },  { 0, 0, { -1328, 385, 4354 } },
    { 0, 0, { -1328, 385, 4354 } },  { -1, 0, { -1328, 385, 4354 } }
};
#endif

struct CutsceneSplinePoint sEndingFlyToWindowPos[] = {
    { 0, 0, { -86, 876, 640 } },   { 1, 0, { -86, 876, 610 } },   { 2, 0, { -66, 945, 393 } },
    { 3, 0, { -80, 976, 272 } },   { 4, 0, { -66, 1306, -36 } },  { 5, 0, { -70, 1869, -149 } },
    { 6, 0, { -10, 2093, -146 } }, { 7, 0, { -10, 2530, -248 } }, { 8, 0, { -10, 2530, -263 } },
    { 9, 0, { -10, 2530, -273 } }
};

struct CutsceneSplinePoint sEndingFlyToWindowFocus[] = {
    { 0, 50, { -33, 889, -7 } },    { 1, 35, { -33, 889, -7 } },    { 2, 31, { -17, 1070, -193 } },
    { 3, 25, { -65, 1182, -272 } }, { 4, 20, { -64, 1559, -542 } }, { 5, 25, { -68, 2029, -677 } },
    { 6, 25, { -9, 2204, -673 } },  { 7, 25, { -8, 2529, -772 } },  { 8, 0, { -8, 2529, -772 } },
    { 9, 0, { -8, 2529, -772 } },   { -1, 0, { -8, 2529, -772 } }
};

struct CutsceneSplinePoint sEndingPeachDescentCamPos[] = {
    { 0, 50, { 1, 120, -1150 } },    { 1, 50, { 1, 120, -1150 } },    { 2, 40, { 118, 121, -1199 } },
    { 3, 40, { 147, 74, -1306 } },   { 4, 40, { 162, 95, -1416 } },   { 5, 40, { 25, 111, -1555 } },
    { 6, 40, { -188, 154, -1439 } }, { 7, 40, { -203, 181, -1242 } }, { 8, 40, { 7, 191, -1057 } },
    { 9, 40, { 262, 273, -1326 } },  { 0, 40, { -4, 272, -1627 } },   { 1, 35, { -331, 206, -1287 } },
    { 2, 30, { -65, 219, -877 } },   { 3, 25, { 6, 216, -569 } },     { 4, 25, { -8, 157, 40 } },
    { 5, 25, { -4, 106, 200 } },     { 6, 25, { -6, 72, 574 } },      { 7, 0, { -6, 72, 574 } },
    { 8, 0, { -6, 72, 574 } },       { -1, 0, { -6, 72, 574 } }
};

struct CutsceneSplinePoint sEndingMarioToPeachPos[] = {
    { 0, 0, { -130, 1111, -1815 } }, { 1, 0, { -131, 1052, -1820 } }, { 2, 0, { -271, 1008, -1651 } },
    { 3, 0, { -439, 1043, -1398 } }, { 4, 0, { -433, 1040, -1120 } }, { 5, 0, { -417, 1040, -1076 } },
    { 6, 0, { -417, 1040, -1076 } }, { 7, 0, { -417, 1040, -1076 } }, { -1, 0, { -417, 1040, -1076 } }
};

struct CutsceneSplinePoint sEndingMarioToPeachFocus[] = {
    { 0, 50, { -37, 1020, -1332 } }, { 1, 20, { -36, 1012, -1330 } }, { 2, 20, { -24, 1006, -1215 } },
    { 3, 20, { 28, 1002, -1224 } },  { 4, 24, { 45, 1013, -1262 } },  { 5, 35, { 34, 1000, -1287 } },
    { 6, 0, { 34, 1000, -1287 } },   { 7, 0, { 34, 1000, -1287 } },   { -1, 0, { 34, 1000, -1287 } }
};

struct CutsceneSplinePoint sEndingLookUpAtCastle[] = {
    { 0, 50, { 200, 1066, -1414 } }, { 0, 50, { 200, 1066, -1414 } }, { 0, 30, { 198, 1078, -1412 } },
    { 0, 33, { 15, 1231, -1474 } },  { 0, 39, { -94, 1381, -1368 } }, { 0, 0, { -92, 1374, -1379 } },
    { 0, 0, { -92, 1374, -1379 } },  { -1, 0, { -92, 1374, -1379 } }
};

struct CutsceneSplinePoint sEndingLookAtSkyFocus[] = {
#ifdef VERSION_EU
    { 0, 50, { 484, 1368, -868 } }, { 0, 72, { 479, 1372, -872 } }, { 0, 50, { 351, 1817, -918 } },
#else
    { 0, 50, { 484, 1368, -888 } }, { 0, 72, { 479, 1372, -892 } }, { 0, 50, { 351, 1817, -918 } },
#endif
    { 0, 50, { 351, 1922, -598 } }, { 0, 0, { 636, 2027, -415 } },  { 0, 0, { 636, 2027, -415 } },
    { -1, 0, { 636, 2027, -415 } }
};

/**
 * Plays the music that starts after peach fades and Lakitu appears.
 */
void cutscene_intro_peach_play_lakitu_flying_music(void) {
    play_music(SEQ_PLAYER_LEVEL, SEQUENCE_ARGS(15, SEQ_EVENT_CUTSCENE_INTRO), 0);
}

/**
 * Lower the volume (US only) and start the peach letter background music
 */
void cutscene_intro_peach_start_letter_music(UNUSED struct Camera *c) {
#if defined(VERSION_US) || defined(VERSION_SH)
    seq_player_lower_volume(SEQ_PLAYER_LEVEL, 60, 40);
#endif
    cutscene_intro_peach_play_message_music();
}

/**
 * Raise the volume (not in JP) and start the flying music.
 */
void cutscene_intro_peach_start_flying_music(UNUSED struct Camera *c) {
#ifndef VERSION_JP
    seq_player_unlower_volume(SEQ_PLAYER_LEVEL, 60);
#endif
    cutscene_intro_peach_play_lakitu_flying_music();
}

#ifdef VERSION_EU
/**
 * Lower the volume for the letter background music. In US, this happens on the same frame as the music
 * starts.
 */
void cutscene_intro_peach_eu_lower_volume(UNUSED struct Camera *c) {
    seq_player_lower_volume(SEQ_PLAYER_LEVEL, 60, 40);
}
#endif

/**
 * Move the camera along `positionSpline` and point its focus at the corresponding point along
 * `focusSpline`. sCutsceneSplineSegmentProgress is updated after pos and focus are calculated.
 */
s32 intro_peach_move_camera_start_to_pipe(struct Camera *c, struct CutsceneSplinePoint positionSpline[],
                  struct CutsceneSplinePoint focusSpline[]) {
    Vec3f offset;
    s32 posReturn = 0;
    s32 focusReturn = 0;

    /**
     * The position spline's speed parameters are all 0, so sCutsceneSplineSegmentProgress doesn't get
     * updated. Otherwise position would move two frames ahead, and c->focus would always be one frame
     * further along the spline than c->pos.
     */
    posReturn = move_point_along_spline(c->pos, positionSpline, &sCutsceneSplineSegment, &sCutsceneSplineSegmentProgress);
    focusReturn = move_point_along_spline(c->focus, focusSpline, &sCutsceneSplineSegment, &sCutsceneSplineSegmentProgress);

    // The two splines used by this function are reflected in the horizontal plane for some reason,
    // so they are rotated every frame. Why do this, Nintendo?
    rotate_in_xz(c->focus, c->focus, DEGREES(-180));
    rotate_in_xz(c->pos, c->pos, DEGREES(-180));

    vec3f_set(offset, -1328.f, 260.f, 4664.f);
    vec3f_add(c->focus, offset);
    vec3f_add(c->pos, offset);

    posReturn += focusReturn; // Unused
    return focusReturn;
}

/**
 * Create a dialog box with the letter text
 */
void peach_letter_text(UNUSED struct Camera *c) {
    create_dialog_box(DIALOG_020);
}

#ifndef VERSION_JP
void play_sound_peach_reading_letter(UNUSED struct Camera *c) {
    play_sound(SOUND_PEACH_DEAR_MARIO, gGlobalSoundSource);
}
#endif

/**
 * Move the camera from peach reading the letter all the way to Mario's warp pipe. Follow the
 * sIntroStartToPipe splines.
 */
void cutscene_intro_peach_start_to_pipe_spline(struct Camera *c) {
    if (intro_peach_move_camera_start_to_pipe(c, sIntroStartToPipePosition, sIntroStartToPipeFocus) != 0) {
        gCameraMovementFlags &= ~CAM_MOVE_C_UP_MODE;
        gCutsceneTimer = CUTSCENE_LOOP;
    }
}

/**
 * Loop the cutscene until Mario exits the dialog.
 */
void cutscene_intro_peach_dialog(struct Camera *c) {
    if (get_dialog_id() == DIALOG_NONE) {
        vec3f_copy(gLakituState.goalPos, c->pos);
        vec3f_copy(gLakituState.goalFocus, c->focus);
        sStatusFlags |= (CAM_FLAG_SMOOTH_MOVEMENT | CAM_FLAG_UNUSED_CUTSCENE_ACTIVE);
        gCutsceneTimer = CUTSCENE_STOP;
        c->cutscene = 0;
    }
}

void cutscene_intro_peach_follow_pipe_spline(struct Camera *c) {
    move_point_along_spline(c->pos, sIntroPipeToDialogPosition, &sCutsceneSplineSegment, &sCutsceneSplineSegmentProgress);
    move_point_along_spline(c->focus, sIntroPipeToDialogFocus, &sCutsceneSplineSegment, &sCutsceneSplineSegmentProgress);
}

void cutscene_intro_peach_clear_cutscene_status(UNUSED struct Camera *c) {
    sMarioCamState->cameraEvent = 0;
}

/**
 * Set fov to 8 degrees, then zoom out to 30.
 */
void cutscene_intro_peach_zoom_fov(UNUSED struct Camera *c) {
    sFOVState.fov = 8.f;
    set_fov_function(CAM_FOV_ZOOM_30);
}

/**
 * Reset the spline progress, turn on handheld shake.
 */
void cutscene_intro_peach_reset_spline(UNUSED struct Camera *c) {
    sCutsceneSplineSegment = 0;
    sCutsceneSplineSegmentProgress = 0.1f;
    //! @bug since this event is only called for one frame, this handheld shake is turned off on the
    //! next frame.
    set_handheld_shake(HAND_CAM_SHAKE_HIGH);
}

/**
 * Turn off handheld shake. This was likely written before handheld shake was changed to turn off every
 * frame, as it's the only instance of HAND_CAM_SHAKE_OFF.
 */
void cutscene_intro_peach_handheld_shake_off(UNUSED struct Camera *c) {
    set_handheld_shake(HAND_CAM_SHAKE_OFF);
}

void intro_pipe_exit_text(UNUSED struct Camera *c) {
    create_dialog_box(DIALOG_033);
}

#ifndef VERSION_JP
void play_sound_intro_turn_on_hud(UNUSED struct Camera *c) {
    play_sound_rbutton_changed();
}
#endif

/**
 * Fly to the pipe. Near the end, the camera jumps to Lakitu's position and the hud turns on.
 */
void cutscene_intro_peach_fly_to_pipe(struct Camera *c) {
#if defined(VERSION_US) || defined(VERSION_SH)
    cutscene_event(play_sound_intro_turn_on_hud, c, 818, 818);
#elif defined(VERSION_EU)
    cutscene_event(play_sound_intro_turn_on_hud, c, 673, 673);
#endif
    cutscene_spawn_obj(6, 1);
    cutscene_event(cutscene_intro_peach_start_flying_music, c, 0, 0);
    cutscene_event(cutscene_intro_peach_start_to_pipe_spline, c, 0, -1);
#ifdef VERSION_EU
    cutscene_event(cutscene_intro_peach_clear_cutscene_status, c, 572, 572);
#else
    cutscene_event(cutscene_intro_peach_clear_cutscene_status, c, 717, 717);
#endif
    clamp_pitch(c->pos, c->focus, 0x3B00, -0x3B00);
    sCutsceneVars[1].point[1] = 400.f;
}

/**
 * Lakitu flies around the warp pipe, then Mario jumps out.
 */
void cutscene_intro_peach_mario_appears(struct Camera *c) {
    UNUSED u32 pad[2];

    sMarioCamState->cameraEvent = 0;
    cutscene_event(cutscene_intro_peach_reset_spline, c, 0, 0);
    cutscene_event(cutscene_intro_peach_follow_pipe_spline, c, 0, -1);
    cutscene_event(cutscene_intro_peach_handheld_shake_off, c, 70, 70);
    cutscene_event(intro_pipe_exit_text, c, 250, 250);

    approach_f32_asymptotic_bool(&sCutsceneVars[1].point[1], 80.f + sMarioGeometry.currFloorHeight +
                                 (sMarioCamState->pos[1] - sMarioGeometry.currFloorHeight) * 1.1f, 0.4f);

    // Make the camera look up as Mario jumps out of the pipe
    if (c->focus[1] < sCutsceneVars[1].point[1]) {
        c->focus[1] = sCutsceneVars[1].point[1];
    }

    sStatusFlags |= CAM_FLAG_UNUSED_CUTSCENE_ACTIVE;
}

/**
 * Reset the fov. This gives the effect of peach zooming out as she fades.
 */
void cutscene_intro_peach_reset_fov(UNUSED struct Camera *c) {
    set_fov_function(CAM_FOV_DEFAULT);
}

/**
 * Peach reads the letter to Mario.
 */
void cutscene_intro_peach_letter(struct Camera *c) {
    cutscene_spawn_obj(5, 0);
    cutscene_event(cutscene_intro_peach_zoom_fov, c, 0, 0);
    cutscene_event(cutscene_intro_peach_start_letter_music, c, 65, 65);
#ifdef VERSION_EU
    cutscene_event(cutscene_intro_peach_eu_lower_volume, c, 68, 68);
#endif
    cutscene_event(cutscene_intro_peach_start_to_pipe_spline, c, 0, 0);
    cutscene_event(peach_letter_text, c, 65, 65);
#ifndef VERSION_JP
    cutscene_event(play_sound_peach_reading_letter, c, 83, 83);
#endif

    if ((gCutsceneTimer > 120) && (get_dialog_id() == DIALOG_NONE)) {
        // Start the next scene
        gCutsceneTimer = CUTSCENE_LOOP;
    }

    clamp_pitch(c->pos, c->focus, 0x3B00, -0x3B00);
}

/**
 * Reset the spline progress.
 */
void cutscene_end_waving_start(UNUSED struct Camera *c) {
    cutscene_reset_spline();
}

// 3rd part of data
struct CutsceneSplinePoint gIntroLakituStartToPipeFocus[] = {
    { 0, 32, { 58, -250, 346 } },    { 1, 50, { -159, -382, 224 } }, { 2, 37, { 0, -277, 237 } },
    { 3, 15, { 1, -44, 245 } },      { 4, 35, { 0, -89, 228 } },     { 5, 15, { 28, 3, 259 } },
    { 6, 25, { -38, -201, 371 } },   { 7, 20, { -642, 118, 652 } },  { 8, 25, { 103, -90, 861 } },
    { 9, 25, { 294, 145, 579 } },    { 10, 30, { 220, -42, 500 } },  { 11, 20, { 10, -134, 200 } },
    { 12, 20, { -143, -145, 351 } }, { 13, 14, { -256, -65, 528 } }, { 14, 20, { -251, -52, 459 } },
    { 15, 25, { -382, 520, 395 } },  { 16, 25, { -341, 240, 653 } }, { 17, 5, { -262, 700, 143 } },
    { 18, 15, { -760, 32, 27 } },    { 19, 20, { -756, -6, -26 } },  { 20, 20, { -613, 5, 424 } },
    { 21, 20, { -22, -100, 312 } },  { 22, 25, { 212, 80, 61 } },    { 23, 20, { 230, -28, 230 } },
    { 24, 35, { -83, -51, 303 } },   { 25, 17, { 126, 90, 640 } },   { 26, 9, { 158, 95, 763 } },
    { 27, 8, { 113, -25, 1033 } },   { 28, 20, { 57, -53, 1291 } },  { 29, 15, { 73, -34, 1350 } },
    { 30, 7, { 0, 96, 1400 } },      { 31, 8, { -59, 269, 1450 } },  { 32, 15, { 57, 1705, 1500 } },
    { 0, 15, { -227, 511, 1550 } },  { -1, 15, { -227, 511, 1600 } }
};

struct CutsceneSplinePoint gIntroLakituStartToPipeOffsetFromCamera[] = {
    { 0, 0, { -46, 87, -15 } },   { 1, 0, { -38, 91, -11 } },  { 2, 0, { -31, 93, -13 } },
    { 3, 0, { -50, 84, -16 } },   { 4, 0, { -52, 83, -17 } },  { 5, 0, { -10, 99, 3 } },
    { 6, 0, { -54, 83, -10 } },   { 7, 0, { -31, 85, -40 } },  { 8, 0, { -34, 91, 19 } },
    { 9, 0, { -9, 95, 28 } },     { 10, 0, { 17, 72, 66 } },   { 11, 0, { 88, -7, 45 } },
    { 12, 0, { 96, -6, -26 } },   { 13, 0, { 56, -1, -82 } },  { 14, 0, { 40, 65, -63 } },
    { 15, 0, { -26, -3, -96 } },  { 16, 0, { 92, 82, 19 } },   { 17, 0, { 92, 32, 19 } },
    { 18, 0, { 92, 32, 19 } },    { 19, 0, { 92, 102, 19 } },  { 20, 0, { -69, 59, -70 } },
    { 21, 0, { -77, 109, -61 } }, { 22, 0, { -87, 59, -46 } }, { 23, 0, { -99, -3, 11 } },
    { 24, 0, { -99, -11, 5 } },   { 25, 0, { -97, -6, 19 } },  { 26, 0, { -97, 22, -7 } },
    { 27, 0, { -98, -11, -13 } }, { 28, 0, { -97, -11, 19 } }, { 29, 0, { -91, -11, 38 } },
    { 30, 0, { -76, -11, 63 } },  { 31, 0, { -13, 33, 93 } },  { 32, 0, { 51, -11, 84 } },
    { 33, 0, { 51, -11, 84 } },   { -1, 0, { 51, -11, 84 } }
};

struct CutsceneSplinePoint gEndWavingPos[] = {
    { 0, 0, { -5, 975, -917 } },    { 0, 0, { -5, 975, -917 } },    { 0, 0, { -5, 975, -917 } },
    { 0, 0, { -76, 1067, 742 } },   { 0, 0, { -105, 1576, 3240 } }, { 0, 0, { -177, 1709, 5586 } },
    { 0, 0, { -177, 1709, 5586 } }, { 0, 0, { -177, 1709, 5586 } }, { 0, 0, { -177, 1709, 5586 } }
};

struct CutsceneSplinePoint gEndWavingFocus[] = {
    { 0, 50, { 18, 1013, -1415 } }, { 0, 100, { 17, 1037, -1412 } }, { 0, 100, { 16, 1061, -1408 } },
    { 0, 100, { -54, 1053, 243 } }, { 0, 100, { -84, 1575, 2740 } }, { 0, 50, { -156, 1718, 5086 } },
    { 0, 0, { -156, 1718, 5086 } }, { 0, 0, { -156, 1718, 5086 } },  { 0, 0, { -156, 1718, 5086 } }
};

void cutscene_end_waving(struct Camera *c) {
    cutscene_event(cutscene_end_waving_start, c, 0, 0);
    move_point_along_spline(c->pos, gEndWavingPos, &sCutsceneSplineSegment, &sCutsceneSplineSegmentProgress);
    move_point_along_spline(c->focus, gEndWavingFocus, &sCutsceneSplineSegment, &sCutsceneSplineSegmentProgress);
    cutscene_spawn_obj(6, 120);
}

void cutscene_exit_fall_to_castle_grounds_warp(struct Camera *c) {
    //! hardcoded position
    vec3f_set(c->pos, 5830.f, 32.f, 3985.f);
}

/**
 * Cutscene that plays when Mario falls from WMOTR.
 */
void cutscene_exit_fall_to_castle_grounds(struct Camera *c) {
    cutscene_event(cutscene_exit_fall_to_castle_grounds_warp, c, 0, 0);
    cutscene_event(cutscene_exit_to_castle_grounds_focus_mario, c, 0, -1);
    update_camera_yaw(c);
}

/**
 * Set the camera position and focus for when Mario falls from the sky.
 */
void cutscene_ending_mario_fall_start(struct Camera *c) {
    vec3f_set(c->focus, -26.f, 0.f, -137.f);
    vec3f_set(c->pos, 165.f, 4725.f, 324.f);
}

/**
 * Focus on Mario when he's falling from the sky.
 */
void cutscene_ending_mario_fall_focus_mario(struct Camera *c) {
    Vec3f offset;
    vec3f_set(offset, 0.f, 80.f, 0.f);

    offset[2] = ABS(sMarioCamState->pos[1] - c->pos[1]) * -0.1f;
    if (offset[2] > -100.f) {
        offset[2] = -100.f;
    }

    offset_rotated(c->focus, sMarioCamState->pos, offset, sMarioCamState->faceAngle);
}

/**
 * Mario falls from the sky after the grand star cutscene.
 */
void cutscene_ending_mario_fall(struct Camera *c) {
    cutscene_event(cutscene_ending_mario_fall_start, c, 0, 0);
    cutscene_event(cutscene_ending_mario_fall_focus_mario, c, 0, -1);
    player2_rotate_cam(c, -0x2000, 0x2000, -0x2000, 0x2000);
}

/**
 * Closeup of Mario as the wing cap fades and Mario looks up.
 */
void cutscene_ending_mario_land_closeup(struct Camera *c) {
    vec3f_set(c->focus, 85.f, 826.f, 250.f);
    vec3f_set(c->pos, -51.f, 988.f, -202.f);
    player2_rotate_cam(c, -0x2000, 0x2000, -0x2000, 0x2000);
}

/**
 * Reset the spline progress and cvar9.
 */
void cutscene_ending_reset_spline(UNUSED struct Camera *c) {
    sCutsceneVars[9].point[0] = 0.f;
    cutscene_reset_spline();
}

/**
 * Follow sEndingFlyToWindowPos/Focus up to the window.
 */
void cutscene_ending_fly_up_to_window(struct Camera *c) {
    move_point_along_spline(c->pos, sEndingFlyToWindowPos, &sCutsceneSplineSegment, &sCutsceneSplineSegmentProgress);
    move_point_along_spline(c->focus, sEndingFlyToWindowFocus, &sCutsceneSplineSegment, &sCutsceneSplineSegmentProgress);
}

/**
 * Move the camera up to the window as the star power frees peach.
 */
void cutscene_ending_stars_free_peach(struct Camera *c) {
    cutscene_event(cutscene_ending_reset_spline, c, 0, 0);
    cutscene_event(cutscene_ending_fly_up_to_window, c, 0, -1);
    player2_rotate_cam(c, -0x2000, 0x2000, -0x2000, 0x2000);
}

/**
 * Move the camera to the ground as Mario lands.
 */
void cutscene_ending_mario_land(struct Camera *c) {
    vec3f_set(c->focus, sEndingFlyToWindowFocus[0].point[0], sEndingFlyToWindowFocus[0].point[1] + 80.f, sEndingFlyToWindowFocus[0].point[2]);
    vec3f_set(c->pos, sEndingFlyToWindowPos[0].point[0], sEndingFlyToWindowPos[0].point[1], sEndingFlyToWindowPos[0].point[2] + 150.f);
    player2_rotate_cam(c, -0x800, 0x2000, -0x2000, 0x2000);
}

/**
 * Move the camera closer to peach appearing.
 */
void cutscene_ending_peach_appear_closeup(struct Camera *c) {
    vec3f_set(c->pos, 179.f, 2463.f, -1216.f);
    c->pos[1] = gCutsceneFocus->oPosY + 35.f;
    vec3f_set(c->focus, gCutsceneFocus->oPosX, gCutsceneFocus->oPosY + 125.f, gCutsceneFocus->oPosZ);
}

/**
 * Peach fades in, the camera focuses on her.
 */
void cutscene_ending_peach_appears(struct Camera *c) {
    cutscene_event(cutscene_ending_peach_appear_closeup, c, 0, 0);
    approach_f32_asymptotic_bool(&c->pos[1], gCutsceneFocus->oPosY + 35.f, 0.02f);
    approach_f32_asymptotic_bool(&c->focus[1], gCutsceneFocus->oPosY + 125.f, 0.15f);
    player2_rotate_cam(c, -0x2000, 0x2000, -0x2000, 0x2000);
}

/**
 * Reset spline progress, set cvar2 y offset.
 */
void cutscene_ending_peach_descends_start(UNUSED struct Camera *c) {
    cutscene_reset_spline();
    sCutsceneVars[2].point[1] = 150.f;
}

/**
 * Follow the sEndingPeachDescentCamPos spline, which rotates around peach.
 */
void cutscene_ending_follow_peach_descent(struct Camera *c) {
    move_point_along_spline(c->pos, sEndingPeachDescentCamPos, &sCutsceneSplineSegment, &sCutsceneSplineSegmentProgress);
    c->pos[1] += gCutsceneFocus->oPosY + sCutsceneVars[3].point[1];
}

/**
 * Decrease cvar2's y offset while the camera flies backwards to Mario.
 */
void cutscene_ending_peach_descent_lower_focus(UNUSED struct Camera *c) {
    camera_approach_f32_symmetric_bool(&(sCutsceneVars[2].point[1]), 90.f, 0.5f);
}

/**
 * Keep following the sEndingPeachDescentCamPos spline, which leads back to Mario.
 */
void cutscene_ending_peach_descent_back_to_mario(struct Camera *c) {
    Vec3f pos;

    move_point_along_spline(pos, sEndingPeachDescentCamPos, &sCutsceneSplineSegment, &sCutsceneSplineSegmentProgress);
    c->pos[0] = pos[0];
    c->pos[2] = pos[2];
    approach_f32_asymptotic_bool(&c->pos[1], (pos[1] += gCutsceneFocus->oPosY), 0.07f);
}

/**
 * Peach starts floating to the ground. Rotate the camera around her, then fly backwards to Mario when
 * she lands.
 */
void cutscene_ending_peach_descends(struct Camera *c) {
    cutscene_event(cutscene_ending_peach_descends_start, c, 0, 0);
    cutscene_event(cutscene_ending_follow_peach_descent, c, 0, 299);
    cutscene_event(cutscene_ending_peach_descent_back_to_mario, c, 300, -1);
    cutscene_event(cutscene_ending_peach_descent_lower_focus, c, 300, -1);
    vec3f_set(c->focus, gCutsceneFocus->oPosX, sCutsceneVars[2].point[1] + gCutsceneFocus->oPosY,
              gCutsceneFocus->oPosZ);
    player2_rotate_cam(c, -0x2000, 0x2000, -0x2000, 0x2000);
}

/**
 * Mario runs across the bridge to peach, and takes off his cap.
 * Follow the sEndingMarioToPeach* splines while Mario runs across.
 */
void cutscene_ending_mario_to_peach(struct Camera *c) {
    cutscene_event(cutscene_ending_reset_spline, c, 0, 0);
    move_point_along_spline(c->pos, sEndingMarioToPeachPos, &sCutsceneSplineSegment, &sCutsceneSplineSegmentProgress);
    move_point_along_spline(c->focus, sEndingMarioToPeachFocus, &sCutsceneSplineSegment, &sCutsceneSplineSegmentProgress);
    player2_rotate_cam(c, -0x2000, 0x2000, -0x2000, 0x2000);
}

void cutscene_exit_waterfall_warp(struct Camera *c) {
    //! hardcoded position
    vec3f_set(c->pos, -3899.f, 39.f, -5671.f);
}

/**
 * Look at Mario, used by cutscenes that play when Mario exits a course to castle grounds.
 */
void cutscene_exit_to_castle_grounds_focus_mario(struct Camera *c) {
    vec3f_copy(c->focus, sMarioCamState->pos);
    c->focus[1] = c->pos[1] + (sMarioCamState->pos[1] + 125.f - c->pos[1]) * 0.5f;
    approach_vec3f_asymptotic(c->focus, sMarioCamState->pos, 0.05f, 0.4f, 0.05f);
}

/**
 * Cutscene that plays when Mario leaves CotMC through the waterfall.
 */
void cutscene_exit_waterfall(struct Camera *c) {
    cutscene_event(cutscene_exit_waterfall_warp, c, 0, 0);
    cutscene_event(cutscene_exit_to_castle_grounds_focus_mario, c, 0, -1);
    update_camera_yaw(c);
}

/**
 * Make the focus follow the sEndingLookUpAtCastle spline.
 */
void cutscene_ending_look_up_at_castle(UNUSED struct Camera *c) {
    move_point_along_spline(c->focus, sEndingLookUpAtCastle, &sCutsceneSplineSegment, &sCutsceneSplineSegmentProgress);
}


/**
 * Peach opens her eyes and the camera looks at the castle window again.
 */
void cutscene_ending_peach_wakeup(struct Camera *c) {
    cutscene_event(cutscene_ending_reset_spline, c, 0, 0);
    cutscene_event(cutscene_ending_look_up_at_castle, c, 0, 0);
#ifdef VERSION_EU
    cutscene_event(cutscene_ending_look_up_at_castle, c, 265, -1);
    cutscene_spawn_obj(7, 315);
    cutscene_spawn_obj(9, 355);
#else
    cutscene_event(cutscene_ending_look_up_at_castle, c, 250, -1);
    cutscene_spawn_obj(7, 300);
    cutscene_spawn_obj(9, 340);
#endif
    vec3f_set(c->pos, -163.f, 978.f, -1082.f);
    player2_rotate_cam(c, -0x800, 0x2000, -0x2000, 0x2000);
}

/**
 * Side view of peach and Mario. Peach thanks Mario for saving her.
 */
void cutscene_ending_dialog(struct Camera *c) {
    vec3f_set(c->focus, 11.f, 983.f, -1273.f);
    vec3f_set(c->pos, -473.f, 970.f, -1152.f);
    player2_rotate_cam(c, -0x800, 0x2000, -0x2000, 0x2000);
}

/**
 * Zoom in and move the camera close to Mario and peach.
 */
void cutscene_ending_kiss_closeup(struct Camera *c) {
    set_fov_function(CAM_FOV_SET_29);
    vec3f_set(c->focus, 350.f, 1034.f, -1216.f);
    vec3f_set(c->pos, -149.f, 1021.f, -1216.f);
}

/**
 * Fly back and zoom out for Mario's spin after the kiss.
 */
void cutscene_ending_kiss_here_we_go(struct Camera *c) {
    Vec3f pos, foc;

    set_fov_function(CAM_FOV_DEFAULT);
    vec3f_set(foc, 233.f, 1068.f, -1298.f);
    vec3f_set(pos, -250.f, 966.f, -1111.f);
    //! another double typo
    approach_vec3f_asymptotic(c->pos, pos, 0.2, 0.1f, 0.2f);
    approach_vec3f_asymptotic(c->focus, foc, 0.2, 0.1f, 0.2f);
}

/**
 * Peach kisses Mario on the nose.
 */
void cutscene_ending_kiss(struct Camera *c) {
    cutscene_event(cutscene_ending_kiss_closeup, c, 0, 0);
#ifdef VERSION_EU
    cutscene_event(cutscene_ending_kiss_here_we_go, c, 185, -1);
#else
    cutscene_event(cutscene_ending_kiss_here_we_go, c, 155, -1);
#endif
    player2_rotate_cam(c, -0x800, 0x2000, -0x2000, 0x2000);
}

/**
 * Make the focus follow sEndingLookAtSkyFocus.
 */
void cutscene_ending_look_at_sky(struct Camera *c) {
    move_point_along_spline(c->focus, sEndingLookAtSkyFocus, &sCutsceneSplineSegment, &sCutsceneSplineSegmentProgress);
    vec3f_set(c->pos, 699.f, 1680.f, -703.f);
}

/**
 * Zoom in the fov. The fovFunc was just set to default, so it wants to approach 45. But while this is
 * called, it will stay at about 37.26f
 */
void cutscene_ending_zoom_fov(UNUSED struct Camera *c) {
    sFOVState.fov = 37.f;
}

/**
 * Peach suggests baking a cake for Mario. Mario looks back at the camera before going inside the castle.
 */
void cutscene_ending_cake_for_mario(struct Camera *c) {
    cutscene_event(cutscene_ending_reset_spline, c, 0, 0);
    cutscene_event(cutscene_ending_look_at_sky, c, 0, 0);
    cutscene_event(cutscene_ending_zoom_fov, c, 0, 499);
    cutscene_event(cutscene_ending_look_at_sky, c, 500, -1);
    cutscene_spawn_obj(8, 600);
    cutscene_spawn_obj(8, 608);
    cutscene_spawn_obj(8, 624);
    cutscene_spawn_obj(8, 710);
}

/**
 * Stop the ending cutscene, reset the fov.
 */
void cutscene_ending_stop(struct Camera *c) {
    set_fov_function(CAM_FOV_SET_45);
    c->cutscene = 0;
    gCutsceneTimer = CUTSCENE_STOP;
}


/**
 * Cutscene that plays when Mario beats the game.
 */
struct Cutscene sCutsceneEnding[] = {
    { cutscene_ending_mario_fall, 170 },
    { cutscene_ending_mario_land, 70 },
#ifdef VERSION_EU
    { cutscene_ending_mario_land_closeup, 0x44 },
    { cutscene_ending_stars_free_peach,  0x15c },
    { cutscene_ending_peach_appears, 0x6d  },
    { cutscene_ending_peach_descends, 0x212 },
    { cutscene_ending_mario_to_peach, 0x69 },
    { cutscene_ending_peach_wakeup, 0x1a4 },
    { cutscene_ending_dialog, 0x114 },
    { cutscene_ending_kiss, 0x10b },
#else
    { cutscene_ending_mario_land_closeup, 75 },
#ifdef VERSION_SH
    { cutscene_ending_stars_free_peach, 431 },
#else
    { cutscene_ending_stars_free_peach, 386 },
#endif
    { cutscene_ending_peach_appears, 139 },
    { cutscene_ending_peach_descends, 590 },
    { cutscene_ending_mario_to_peach, 95 },
#ifdef VERSION_SH
    { cutscene_ending_peach_wakeup, 455 },
    { cutscene_ending_dialog, 286 },
#else
    { cutscene_ending_peach_wakeup, 425 },
    { cutscene_ending_dialog, 236 },
#endif
    { cutscene_ending_kiss, 245 },
#endif
    { cutscene_ending_cake_for_mario, CUTSCENE_LOOP },
    { cutscene_ending_stop, 0 }
};

/**
 * Cutscene that plays after the credits, when Lakitu is flying away from the castle.
 */
struct Cutscene sCutsceneEndWaving[] = {
    { cutscene_end_waving, CUTSCENE_LOOP }
};


/**
 * The intro of the game. Peach reads her letter and Lakitu flies down to Mario's warp pipe.
 */
struct Cutscene sCutsceneIntroPeach[] = {
    { cutscene_intro_peach_letter, CUTSCENE_LOOP },
    { cutscene_intro_peach_reset_fov, 35 },
#ifdef VERSION_EU
    { cutscene_intro_peach_fly_to_pipe, 675 },
#else
    { cutscene_intro_peach_fly_to_pipe, 820 },
#endif
    { cutscene_intro_peach_mario_appears, 270 },
    { cutscene_intro_peach_dialog, CUTSCENE_LOOP }
};

/**
 * Cutscene that plays when Mario enters the castle grounds after leaving CotMC through the waterfall.
 */
struct Cutscene sCutsceneExitWaterfall[] = {
    { cutscene_exit_waterfall, 52 },
    { cutscene_exit_to_castle_grounds_end, 0 }
};

/**
 * Cutscene that plays when Mario falls from WMOTR.
 */
struct Cutscene sCutsceneFallToCastleGrounds[] = {
    { cutscene_exit_fall_to_castle_grounds, 73 },
    { cutscene_exit_to_castle_grounds_end, 0 }
};
