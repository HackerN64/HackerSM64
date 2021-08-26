
BAD_RETURN(s32) cam_ccm_enter_slide_shortcut(UNUSED struct Camera *c) {
    sStatusFlags |= CAM_FLAG_CCM_SLIDE_SHORTCUT;
}

BAD_RETURN(s32) cam_ccm_leave_slide_shortcut(UNUSED struct Camera *c) {
    sStatusFlags &= ~CAM_FLAG_CCM_SLIDE_SHORTCUT;
}

/**
 * The CCM triggers are used to set the flag that says when Mario is in the slide shortcut.
 */
struct CameraTrigger sCamCCM[] = {
    { 2, cam_ccm_enter_slide_shortcut, -4846, 2061, 27, 1229, 1342, 396, 0 },
    { 2, cam_ccm_leave_slide_shortcut, -6412, -3917, -6246, 307, 185, 132, 0 },
    NULL_TRIGGER
};

struct CutsceneSplinePoint sCcmSlideCreditsSplinePositions[] = {
    { 0, 0, { -6324, 6745, -5626 } },
    { 1, 0, { -6324, 6745, -5626 } },
    { 2, 0, { -6108, 6762, -5770 } },
    { 3, 0, { -5771, 6787, -5962 } },
    { -1, 0, { -5672, 6790, -5979 } }
};

struct CutsceneSplinePoint sCcmSlideCreditsSplineFocus[] = {
    { 0, 50, { -5911, 6758, -5908 } },
    { 1, 50, { -5911, 6758, -5908 } },
    { 2, 50, { -5652, 6814, -5968 } },
    { 3, 50, { -5277, 6801, -6043 } },
    { -1, 50, { -5179, 6804, -6060 } }
};

struct CutsceneSplinePoint sCcmOutsideCreditsSplinePositions[] = {
    { 1, 0, { 1427, -1387, 5409 } },
    { 2, 0, { -1646, -1536, 4526 } },
    { 3, 0, { -3852, -1448, 3913 } },
    { -1, 0, { -5199, -1366, 1886 } }
};

struct CutsceneSplinePoint sCcmOutsideCreditsSplineFocus[] = {
    { 1, 50, { 958, -1481, 5262 } },
    { 2, 50, { -2123, -1600, 4391 } },
    { 3, 50, { -3957, -1401, 3426 } },
    { -1, 50, { -4730, -1215, 1795 } }
};
