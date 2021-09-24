
/**
 * Sets 8 directional mode and blocks the next trigger from processing.
 * Activated when Mario is walking in front of the snowman's head.
 */
void cam_sl_snowman_head_8dir(struct Camera *c) {
    sStatusFlags |= CAM_FLAG_BLOCK_AREA_PROCESSING;
    transition_to_camera_mode(c, CAMERA_MODE_8_DIRECTIONS, 60);
    s8DirModeBaseYaw = 0x1D27;
}

/**
 * Sets free roam mode in SL, called by a trigger that covers a large area and surrounds the 8 direction
 * trigger.
 */
void cam_sl_free_roam(struct Camera *c) {
    transition_to_camera_mode(c, CAMERA_MODE_FREE_ROAM, 60);
}

/**
 * The SL triggers operate camera behavior in front of the snowman who blows air.
 * The first sets a 8 direction mode, while the latter (which encompasses the former)
 * sets free roam mode.
 *
 * This behavior is exploitable, since the ranges assume that Mario must pass through the latter on
 * exit. Using hyperspeed, the earlier area can be directly exited from, keeping the changes it applies.
 */
struct CameraTrigger sCamSL[] = {
    { 1, cam_sl_snowman_head_8dir, 1119, 3584, 1125, 1177, 358, 358, -0x1D27 },
    // This trigger surrounds the previous one
    { 1, cam_sl_free_roam, 1119, 3584, 1125, 4096, 4096, 4096, -0x1D27 },
    NULL_TRIGGER
};

struct CutsceneSplinePoint sSlCreditsSplinePositions[] = {
    { 0, 0, { 939, 6654, 6196 } },
    { 0, 0, { 1873, 5160, 3714 } },
    { 0, 0, { 3120, 3564, 1314 } },
    { -1, 0, { 2881, 4231, 573 } }
};

struct CutsceneSplinePoint sSlCreditsSplineFocus[] = {
    { 0, 50, { 875, 6411, 5763 } },
    { 0, 50, { 1659, 4951, 3313 } },
    { 0, 50, { 2630, 3565, 1215 } },
    { -1, 50, { 2417, 4056, 639 } }
};
