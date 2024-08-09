/**
 * Cutscene that plays when Mario beats the game.
 */
struct Cutscene sCutsceneEnding[] = {
    { cutscene_ending_mario_fall, 170 },
    { cutscene_ending_mario_land, 70 },
    { cutscene_ending_mario_land_closeup, 75 },
#ifdef VERSION_SH
    { cutscene_ending_stars_free_peach, 431 },
#else
    { cutscene_ending_stars_free_peach, 386 },
#endif
    { cutscene_ending_peach_appears, 139 },
    { cutscene_ending_peach_descends, 590 },
    { cutscene_ending_mario_to_peach, 95 },
#ifdef VERSION_SH
    { cutscene_ending_peach_wakeup, 455 },
    { cutscene_ending_dialog, 286 },
#else
    { cutscene_ending_peach_wakeup, 425 },
    { cutscene_ending_dialog, 236 },
#endif
    { cutscene_ending_kiss, 245 },
    { cutscene_ending_cake_for_mario, CUTSCENE_LOOP },
    { cutscene_ending_stop, 0 }
};
