#include "game/camera.h"

void cam_ccm_enter_slide_shortcut(UNUSED struct Camera *c) {
    sStatusFlags |= CAM_FLAG_CCM_SLIDE_SHORTCUT;
}

void cam_ccm_leave_slide_shortcut(UNUSED struct Camera *c) {
    sStatusFlags &= ~CAM_FLAG_CCM_SLIDE_SHORTCUT;
}

/**
 * The CCM triggers are used to set the flag that says when Mario is in the slide shortcut.
 */
struct CameraTrigger sCamCCM[] = {
#ifdef ENABLE_VANILLA_CAM_PROCESSING
    { 2, cam_ccm_enter_slide_shortcut, -4846, 2061, 27, 1229, 1342, 396, 0 },
    { 2, cam_ccm_leave_slide_shortcut, -6412, -3917, -6246, 307, 185, 132, 0 },
#endif // ENABLE_VANILLA_CAM_PROCESSING
    NULL_TRIGGER
};
