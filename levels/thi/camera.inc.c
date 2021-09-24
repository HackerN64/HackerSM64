
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
    { 1, cam_thi_move_cam_through_tunnel, -4609, -2969, 6448, 100, 300, 300, 0 },
    { 1, cam_thi_look_through_tunnel,     -4809, -2969, 6448, 100, 300, 300, 0 },
    NULL_TRIGGER
};

struct CutsceneSplinePoint sThiWigglerCreditsSplinePositions[] = {
    { 1, 0, { -1411, 2474, -1276 } },
    { 2, 0, { -1606, 2479, -434 } },
    { -1, 0, { -1170, 2122, 1337 } }
};

struct CutsceneSplinePoint sThiWigglerCreditsSplineFocus[] = {
    { 1, 50, { -1053, 2512, -928 } },
    { 2, 50, { -1234, 2377, -114 } },
    { -1, 50, { -758, 2147, 1054 } }
};

struct CutsceneSplinePoint sThiHugeCreditsSplinePositions[] = {
    { 0, 0, { 6990, -1000, -4858 } },
    { 0, 0, { 7886, -1055, 2878 } },
    { 0, 0, { 1952, -1481, 10920 } },
    { 0, 0, { -1684, -219, 2819 } },
    { 0, 0, { -2427, -131, 2755 } },
    { 0, 0, { -3246, 416, 3286 } },
    { -1, 0, { -3246, 416, 3286 } }
};

struct CutsceneSplinePoint sThiHugeCreditsSplineFocus[] = {
    { 1, 70, { 7022, -965, -5356 } },
    { 2, 40, { 7799, -915, 2405 } },
    { 3, 60, { 1878, -1137, 10568 } },
    { 4, 50, { -1931, -308, 2394 } },
    { 5, 50, { -2066, -386, 2521 } },
    { 6, 50, { -2875, 182, 3045 } },
    { -1, 50, { -2875, 182, 3045 } }
};
