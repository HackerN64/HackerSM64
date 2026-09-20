#include "engine/math_util.h"
#include "game/camera.h"

/**
 * Moves the camera to through the tunnel by forcing sModeOffsetYaw
 */
void cam_thi_move_cam_through_tunnel(UNUSED struct Camera *c) {
    if (sModeOffsetYaw < DEGREES(60)) {
        sModeOffsetYaw = DEGREES(60);
    }
}

/**
 * Aligns the camera to look through the tunnel
 */
void cam_thi_look_through_tunnel(UNUSED struct Camera *c) {
    // ~82.5 degrees
    if (sModeOffsetYaw > 0x3AAA) {
        sModeOffsetYaw = 0x3AAA;
    }
}

/**
 * The THI triggers are specifically for the tunnel near the start of the Huge Island.
 * The first helps the camera from getting stuck on the starting side, the latter aligns with the
 * tunnel. Both sides achieve their effect by editing the camera yaw.
 */
struct CameraTrigger sCamTHI[] = {
#ifdef ENABLE_VANILLA_CAM_PROCESSING
    { 1, cam_thi_move_cam_through_tunnel, -4609, -2969, 6448, 100, 300, 300, 0 },
    { 1, cam_thi_look_through_tunnel,     -4809, -2969, 6448, 100, 300, 300, 0 },
#endif // ENABLE_VANILLA_CAM_PROCESSING
    NULL_TRIGGER
};
