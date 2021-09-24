
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
    {  1, cam_bob_tower, 2468, 2720, -4608, 3263, 1696, 3072, 0 },
    { -1, cam_bob_default_free_roam, 0, 0, 0, 0, 0, 0, 0 },
    NULL_TRIGGER
};

/*
 * credits spline paths.
 * TODO: Separate these into their own file(s)
 */

struct CutsceneSplinePoint sBobCreditsSplinePositions[] = {
    { 1, 0, { 5984, 3255, 4975 } },
    { 2, 0, { 4423, 3315, 1888 } },
    { 3, 0, { 776, 2740, -1825 } },
    { 4, 0, { -146, 3894, -3167 } },
    { -1, 0, { 741, 4387, -5474 } }
};

struct CutsceneSplinePoint sBobCreditsSplineFocus[] = {
    { 0, 30, { 5817, 3306, 4507 } },
    { 0, 40, { 4025, 3378, 1593 } },
    { 0, 50, { 1088, 2652, -2205 } },
    { 0, 60, { 205, 3959, -3517 } },
    { -1, 60, { 1231, 4400, -5649 } }
};
