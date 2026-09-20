#include "game/camera.h"


/**
 * Unused. Changes the camera to radial mode when Mario is on the tower.
 *
 * @see sCamBOB for bounds.
 */
void cam_bob_tower(struct Camera *c) {
    sStatusFlags |= CAM_FLAG_BLOCK_AREA_PROCESSING;
    transition_to_camera_mode(c, CAMERA_MODE_RADIAL, 90);
}

/**
 * Unused. Changes the camera to free roam mode when Mario is not climbing the tower.
 *
 * This is the only CameraTrigger event that uses the area == -1 feature:
 * If this was used, it would be called by default in BoB.
 *
 * @see sCamBOB
 */
void cam_bob_default_free_roam(struct Camera *c) {
    transition_to_camera_mode(c, CAMERA_MODE_FREE_ROAM, 90);
}

/**
 * These triggers are unused, but because the first trigger surrounds the BoB tower and activates radial
 * mode (which is called "tower mode" in the patent), it's speculated they belonged to BoB.
 *
 * This table contains the only instance of a CameraTrigger with an area set to -1, and it sets the mode
 * to free_roam when Mario is not walking up the tower.
 */
struct CameraTrigger sCamBOB[] = {
#ifdef ENABLE_VANILLA_CAM_PROCESSING
    {  1, cam_bob_tower, 2468, 2720, -4608, 3263, 1696, 3072, 0 },
    { -1, cam_bob_default_free_roam, 0, 0, 0, 0, 0, 0, 0 },
#endif // ENABLE_VANILLA_CAM_PROCESSING
    NULL_TRIGGER
};
