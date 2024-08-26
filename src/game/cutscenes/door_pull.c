
/**
 * Store the camera's pos and focus for the door cutscene
 */
void cutscene_door_start(struct Camera *c) {
    vec3f_copy(sCutsceneVars[0].point, c->pos);
    vec3f_copy(sCutsceneVars[1].point, c->focus);
}

/**
 * Fix the camera in place while the door opens.
 */
void cutscene_door_fix_cam(struct Camera *c) {
    vec3f_copy(c->pos, sCutsceneVars[0].point);
    vec3f_copy(c->focus, sCutsceneVars[1].point);
}

/**
 * Loop until Mario is no longer using the door.
 */
void cutscene_door_loop(struct Camera *c) {
    //! bitwise AND instead of boolean
    if ((sMarioCamState->action != ACT_PULLING_DOOR) & (sMarioCamState->action != ACT_PUSHING_DOOR)) {
        gCutsceneTimer = CUTSCENE_STOP;
        c->cutscene = 0;
    }
}

/**
 * Warp the camera behind Mario.
 */
void cutscene_door_move_behind_mario(struct Camera *c) {
    Vec3f camOffset;
    s16 doorRotation;

    reset_pan_distance(c);
    determine_pushing_or_pulling_door(&doorRotation);
    set_focus_rel_mario(c, 0.f, 125.f, 0.f, 0);
    vec3s_set(sCutsceneVars[0].angle, 0, sMarioCamState->faceAngle[1] + doorRotation, 0);
    vec3f_set(camOffset, 0.f, 125.f, 250.f);

    offset_rotated(c->pos, sMarioCamState->pos, camOffset, sCutsceneVars[0].angle);
}

/**
 * Follow Mario through the door.
 */
void cutscene_door_follow_mario(struct Camera *c) {
    s16 pitch, yaw;
    f32 dist;

    set_focus_rel_mario(c, 0.f, 125.f, 0.f, 0);
    vec3f_get_dist_and_angle(c->focus, c->pos, &dist, &pitch, &yaw);
    camera_approach_f32_symmetric_bool(&dist, 150.f, 7.f);
    vec3f_set_dist_and_angle(c->focus, c->pos, dist, pitch, yaw);
    update_camera_yaw(c);
}

/**
 * Ends the door cutscene. Sets the camera mode to close mode unless the default is free roam.
 */
void cutscene_door_end(struct Camera *c) {
#ifdef USE_COURSE_DEFAULT_MODE
    c->mode = c->defMode;
#else
    c->mode = c->defMode == CAMERA_MODE_FREE_ROAM ? CAMERA_MODE_FREE_ROAM : CAMERA_MODE_CLOSE;
#endif

    c->cutscene = CUTSCENE_NONE;
    gCutsceneTimer = CUTSCENE_STOP;
    sStatusFlags |= CAM_FLAG_SMOOTH_MOVEMENT;
    sStatusFlags &= ~CAM_FLAG_BLOCK_SMOOTH_MOVEMENT;
    set_flag_post_door(c);
    update_camera_yaw(c);
}

/**
 * Cutscene that plays when Mario pulls open a door.
 */
struct Cutscene sCutsceneDoorPull[] = {
// HackerSM64 TODO: Properly transition when moving through doors
#ifndef FORCED_CAMERA_MODE
    { cutscene_door_start, 1 },
    { cutscene_door_fix_cam, 30 },
    { cutscene_door_move_behind_mario, 1 },
    { cutscene_door_follow_mario, 50 },
#endif
    { cutscene_door_end, 0 }
};

/**
 * Cutscene that plays when Mario pulls open a door that has some special mode requirement on the other
 * side.
 */
struct Cutscene sCutsceneDoorPullMode[] = {
// HackerSM64 TODO: Properly transition when moving through doors
#ifndef FORCED_CAMERA_MODE
    { cutscene_door_start, 1 },
    { cutscene_door_fix_cam, 30 },
#endif
    { cutscene_door_mode, CUTSCENE_LOOP }
};
