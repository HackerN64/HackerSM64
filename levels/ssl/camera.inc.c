
/**
 * Starts the "Enter Pyramid Top" cutscene.
 */
BAD_RETURN(s32) cam_ssl_enter_pyramid_top(UNUSED struct Camera *c) {
    start_object_cutscene_without_focus(CUTSCENE_ENTER_PYRAMID_TOP);
}

/**
 * Change to close mode in the center of the pyramid. Outside this trigger, the default mode is outwards
 * radial.
 */
BAD_RETURN(s32) cam_ssl_pyramid_center(struct Camera *c) {
    sStatusFlags |= CAM_FLAG_BLOCK_AREA_PROCESSING;
    transition_to_camera_mode(c, CAMERA_MODE_CLOSE, 90);
}

/**
 * Changes the mode back to outward radial in the boss room inside the pyramid.
 */
BAD_RETURN(s32) cam_ssl_boss_room(struct Camera *c) {
    sStatusFlags |= CAM_FLAG_BLOCK_AREA_PROCESSING;
    transition_to_camera_mode(c, CAMERA_MODE_OUTWARD_RADIAL, 90);
}

/**
 * The SSL triggers are for starting the enter pyramid top cutscene,
 * setting close mode in the middle of the pyramid, and setting the boss fight camera mode to outward
 * radial.
 */
struct CameraTrigger sCamSSL[] = {
    { 1, cam_ssl_enter_pyramid_top, -2048, 1080, -1024, 150, 150, 150, 0 },
    { 2, cam_ssl_pyramid_center, 0, -104, -104, 1248, 1536, 2950, 0 },
    { 2, cam_ssl_pyramid_center, 0, 2500, 256, 515, 5000, 515, 0 },
    { 3, cam_ssl_boss_room, 0, -1534, -2040, 1000, 800, 1000, 0 },
    NULL_TRIGGER
};

/**
 * Cause Mario to enter the normal dialog state.
 */
static BAD_RETURN(s32) cutscene_mario_dialog(UNUSED struct Camera *c) {
    gCutsceneTimer = cutscene_common_set_dialog_state(MARIO_DIALOG_LOOK_FRONT);
}

/// Unused SSL cutscene?
static UNUSED void unused_cutscene_mario_dialog_looking_down(UNUSED struct Camera *c) {
    gCutsceneTimer = cutscene_common_set_dialog_state(MARIO_DIALOG_LOOK_DOWN);
}

/// Unused SSL cutscene?
static UNUSED void unused_cutscene_mario_dialog_looking_up(UNUSED struct Camera *c) {
    gCutsceneTimer = cutscene_common_set_dialog_state(MARIO_DIALOG_LOOK_UP);
}

/**
 * Store the camera focus in cvar1.
 * Store the area's center position (which happens to be the pyramid, in SSL) in cvar3.
 */
BAD_RETURN(s32) cutscene_pyramid_top_explode_start(struct Camera *c) {
    reset_pan_distance(c);
    store_info_cannon(c);

    vec3f_copy(sCutsceneVars[1].point, c->focus);
    vec3f_set(sCutsceneVars[3].point, c->areaCenX, 1280.f, c->areaCenZ);
}

/**
 * Zoom in on the pyramid.
 */
BAD_RETURN(s32) cutscene_pyramid_top_explode_zoom_in(UNUSED struct Camera *c) {
    set_fov_function(CAM_FOV_APP_30);
}

/**
 * Look at the pyramid top.
 */
BAD_RETURN(s32) cutscene_pyramid_top_explode_focus(struct Camera *c) {
    approach_vec3f_asymptotic(c->focus, sCutsceneVars[3].point, 0.02f, 0.02f, 0.02f);
    sStatusFlags |= CAM_FLAG_SMOOTH_MOVEMENT;
}

/**
 * Store the old pos and focus, then warp to the pyramid top.
 */
BAD_RETURN(s32) cutscene_pyramid_top_explode_warp(struct Camera *c) {
    s16 pitch, yaw;
    f32 dist;

    set_fov_function(CAM_FOV_DEFAULT);
    sFOVState.fov = 45.f;

    vec3f_copy(sCutsceneVars[4].point, c->pos);
    vec3f_copy(sCutsceneVars[5].point, c->focus);
    vec3f_copy(c->focus, sCutsceneVars[3].point);

    vec3f_get_dist_and_angle(sCutsceneVars[3].point, sMarioCamState[0].pos, &dist, &pitch, &yaw);
    vec3f_set_dist_and_angle(sCutsceneVars[3].point, c->pos, 2000.f, 0, yaw);
    c->pos[1] += 500.f;
}

/**
 * Close up view of the spinning pyramid top as it rises.
 */
BAD_RETURN(s32) cutscene_pyramid_top_explode_closeup(struct Camera *c) {
    s16 pitch, yaw;
    f32 dist;

    vec3f_get_dist_and_angle(sCutsceneVars[3].point, c->pos, &dist, &pitch, &yaw);
    approach_f32_asymptotic_bool(&dist, 2000.f, 0.1f);
    vec3f_set_dist_and_angle(sCutsceneVars[3].point, c->pos, dist, pitch, yaw);

    c->focus[1] += 4.f;
    c->pos[1] -= 5.f;
    sFOVState.fov = 45.f;
    set_handheld_shake(HAND_CAM_SHAKE_CUTSCENE);
}

/**
 * Shake the camera during the closeup.
 */
BAD_RETURN(s32) cutscene_pyramid_top_explode_cam_shake(UNUSED struct Camera *c) {
    set_environmental_camera_shake(SHAKE_ENV_PYRAMID_EXPLODE);
}

/**
 * Warp back to the old position, and start a heavy camera shake.
 */
BAD_RETURN(s32) cutscene_pyramid_top_explode_warp_back(struct Camera *c) {
    UNUSED u32 pad[2];

    vec3f_copy(c->pos, sCutsceneVars[4].point);
    vec3f_copy(c->focus, sCutsceneVars[5].point);
    set_environmental_camera_shake(SHAKE_ENV_BOWSER_JUMP);
}

/**
 * An unused cutscene for when the pyramid explodes.
 */
BAD_RETURN(s32) cutscene_pyramid_top_explode(struct Camera *c) {
    cutscene_event(cutscene_pyramid_top_explode_start, c, 0, 0);
    cutscene_event(cutscene_pyramid_top_explode_focus, c, 0, 30);
    cutscene_event(cutscene_pyramid_top_explode_warp, c, 31, 31);
    cutscene_event(cutscene_pyramid_top_explode_closeup, c, 31, 139);
    cutscene_event(cutscene_pyramid_top_explode_zoom_in, c, 23, 23);
    cutscene_event(cutscene_pyramid_top_explode_warp_back, c, 140, 140);
    cutscene_event(cutscene_pyramid_top_explode_cam_shake, c, 31, 139);
}

/**
 * End the pyramid top explosion cutscene.
 */
BAD_RETURN(s32) cutscene_pyramid_top_explode_end(struct Camera *c) {
    cutscene_stop_dialog(c);
    stop_cutscene_and_retrieve_stored_info(c);
    // Move the camera back to Mario
    transition_next_state(c, 30);
}

/**
 * Store the camera focus in cvar0, and store the top of the pyramid in cvar3.
 */
BAD_RETURN(s32) cutscene_enter_pyramid_top_start(struct Camera *c) {
    vec3f_copy(sCutsceneVars[0].point, c->focus);
    vec3f_set(sCutsceneVars[3].point, c->areaCenX, 1280.f, c->areaCenZ);
}

/**
 * Cutscene that plays when Mario enters the top of the pyramid.
 */
BAD_RETURN(s32) cutscene_enter_pyramid_top(struct Camera *c) {
    cutscene_event(cutscene_enter_pyramid_top_start, c, 0, 0);
    // Move to cvar3
    cutscene_goto_cvar_pos(c, 200.f, 0x3000, 0, 0);
    sStatusFlags |= CAM_FLAG_SMOOTH_MOVEMENT;
    set_handheld_shake(HAND_CAM_SHAKE_CUTSCENE);

    if (sMarioCamState->pos[1] > 1250.f) {
        // End the cutscene early if Mario ledge-grabbed.
        // This only works because of the janky way that ledge-grabbing is implemented.
        cutscene_exit_to_castle_grounds_end(c);
    }
}

struct CutsceneSplinePoint sSslCreditsSplinePositions[] = {
    { 0, 0, { -4262, 4658, -5015 } },
    { 0, 0, { -3274, 2963, -4661 } },
    { 0, 0, { -2568, 812, -6528 } },
    { 0, 0, { -414, 660, -7232 } },
    { 0, 0, { 1466, 660, -6898 } },
    { -1, 0, { 2724, 660, -6298 } }
};

struct CutsceneSplinePoint sSslCreditsSplineFocus[] = {
    { 0, 50, { -4083, 4277, -4745 } },
    { 0, 50, { -2975, 2574, -4759 } },
    { 0, 50, { -2343, 736, -6088 } },
    { 0, 50, { -535, 572, -6755 } },
    { 0, 50, { 1311, 597, -6427 } },
    { -1, 50, { 2448, 612, -5884 } }
};

/**
 * Cutscene that plays when Mario enters the pyramid through the hole at the top.
 */
struct Cutscene sCutsceneEnterPyramidTop[] = {
    { cutscene_enter_pyramid_top, 90 },
    { cutscene_exit_to_castle_grounds_end, 0 }
};

/**
 * Unused cutscene for when the pyramid explodes.
 */
struct Cutscene sCutscenePyramidTopExplode[] = {
    { cutscene_mario_dialog, CUTSCENE_LOOP },
    { cutscene_pyramid_top_explode, 150 },
    { cutscene_pyramid_top_explode_end, 0 }
};
