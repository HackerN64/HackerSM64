
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
    { 1, cam_cotmc_exit_waterfall, 0, 1500, 3500, 550, 10000, 1500, 0 },
    NULL_TRIGGER
};

struct CutsceneSplinePoint sCotmcCreditsSplinePositions[] = {
    { 0, 0, { -296, 495, 1607 } },
    { 0, 0, { -430, 541, 654 } },
    { 0, 0, { -466, 601, -359 } },
    { 0, 0, { -217, 433, -1549 } },
    { -1, 0, { -95, 366, -2922 } }
};

struct CutsceneSplinePoint sCotmcCreditsSplineFocus[] = {
    { 0, 50, { -176, 483, 2092 } },
    { 0, 50, { -122, 392, 1019 } },
    { 0, 50, { -268, 450, -792 } },
    { 0, 50, { -172, 399, -2046 } },
    { -1, 50, { -51, 355, -3420 } }
};
