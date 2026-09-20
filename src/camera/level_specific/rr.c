#include "engine/math_util.h"
#include "game/camera.h"

void cam_rr_exit_building_side(struct Camera *c) {
    set_camera_mode_8_directions(c);
    s8DirModeBaseYaw = DEGREES(90);
}

void cam_rr_exit_building_top(struct Camera *c) {
    set_camera_mode_8_directions(c);
    if (c->pos[1] < 6343.f) {
        c->pos[1] = 7543.f;
        gLakituState.goalPos[1] = c->pos[1];
        gLakituState.curPos[1] = c->pos[1];
        sStatusFlags &= ~CAM_FLAG_SMOOTH_MOVEMENT;
    }
}

void cam_rr_enter_building_window(struct Camera *c) {
    if (c->mode != CAMERA_MODE_FIXED) {
        set_camera_mode_fixed(c, -2974, 478, -3975);
    }
}

void cam_rr_enter_building(struct Camera *c) {
    if (c->mode != CAMERA_MODE_FIXED) {
        set_camera_mode_fixed(c, -2953, 798, -3943);
    }
    // Prevent the camera from being above the roof
    if (c->pos[1] > 6043.f) {
        c->pos[1] = 6043.f;
    }
}

void cam_rr_enter_building_side(struct Camera *c) {
    if (c->mode != CAMERA_MODE_FIXED) {
        sStatusFlags &= ~CAM_FLAG_SMOOTH_MOVEMENT;
        c->mode = CAMERA_MODE_FIXED;
    }
}

/**
 * The RR triggers are for changing between fixed and 8 direction mode when entering / leaving the building at
 * the end of the ride.
 */
struct CameraTrigger sCamRR[] = {
#ifdef ENABLE_VANILLA_CAM_PROCESSING
    { 1, cam_rr_exit_building_side, -4197, 3819, -3087, 1769, 1490, 342, 0 },
    { 1, cam_rr_enter_building_side, -4197, 3819, -3771, 769, 490, 342, 0 },
    { 1, cam_rr_enter_building_window, -5603, 4834, -5209, 300, 600, 591, 0 },
    { 1, cam_rr_enter_building, -2609, 3730, -5463, 300, 650, 577, 0 },
    { 1, cam_rr_exit_building_top, -4196, 7343, -5155, 4500, 1000, 4500, 0 },
    { 1, cam_rr_enter_building, -4196, 6043, -5155, 500, 300, 500, 0 },
#endif // ENABLE_VANILLA_CAM_PROCESSING
    NULL_TRIGGER,
};
