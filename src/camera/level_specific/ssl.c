#include "game/camera.h"
#include "camera/cutscene_helpers.h"

/**
 * Starts the "Enter Pyramid Top" cutscene.
 */
void cam_ssl_enter_pyramid_top(UNUSED struct Camera *c) {
    start_object_cutscene_without_focus(CUTSCENE_ENTER_PYRAMID_TOP);
}

/**
 * Change to close mode in the center of the pyramid. Outside this trigger, the default mode is outwards
 * radial.
 */
void cam_ssl_pyramid_center(struct Camera *c) {
    sStatusFlags |= CAM_FLAG_BLOCK_AREA_PROCESSING;
    transition_to_camera_mode(c, CAMERA_MODE_CLOSE, 90);
}

/**
 * Changes the mode back to outward radial in the boss room inside the pyramid.
 */
void cam_ssl_boss_room(struct Camera *c) {
    sStatusFlags |= CAM_FLAG_BLOCK_AREA_PROCESSING;
    transition_to_camera_mode(c, CAMERA_MODE_OUTWARD_RADIAL, 90);
}

/**
 * The SSL triggers are for starting the enter pyramid top cutscene,
 * setting close mode in the middle of the pyramid, and setting the boss fight camera mode to outward
 * radial.
 */
struct CameraTrigger sCamSSL[] = {
#ifdef ENABLE_VANILLA_CAM_PROCESSING
    { 1, cam_ssl_enter_pyramid_top, -2048, 1080, -1024, 150, 150, 150, 0 },
    { 2, cam_ssl_pyramid_center, 0, -104, -104, 1248, 1536, 2950, 0 },
    { 2, cam_ssl_pyramid_center, 0, 2500, 256, 515, 5000, 515, 0 },
    { 3, cam_ssl_boss_room, 0, -1534, -2040, 1000, 800, 1000, 0 },
#endif // ENABLE_VANILLA_CAM_PROCESSING
    NULL_TRIGGER
};
