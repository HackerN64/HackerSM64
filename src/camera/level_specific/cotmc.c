#include "game/camera.h"

/**
 * Fix the camera in place as Mario gets exits out the MC cave into the waterfall.
 */
void cam_cotmc_exit_waterfall(UNUSED struct Camera *c) {
    gCameraMovementFlags |= CAM_MOVE_FIX_IN_PLACE;
}

/**
 * The CotMC trigger is only used to prevent fix Lakitu in place when Mario exits through the waterfall.
 */
struct CameraTrigger sCamCotMC[] = {
#ifdef ENABLE_VANILLA_CAM_PROCESSING
    { 1, cam_cotmc_exit_waterfall, 0, 1500, 3500, 550, 10000, 1500, 0 },
#endif // ENABLE_VANILLA_CAM_PROCESSING
    NULL_TRIGGER
};
