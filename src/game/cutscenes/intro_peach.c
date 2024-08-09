/**
 * The intro of the game. Peach reads her letter and Lakitu flies down to Mario's warp pipe.
 */
struct Cutscene sCutsceneIntroPeach[] = {
    { cutscene_intro_peach_letter, CUTSCENE_LOOP },
    { cutscene_intro_peach_reset_fov, 35 },
#ifdef VERSION_EU
    { cutscene_intro_peach_fly_to_pipe, 675 },
#else
    { cutscene_intro_peach_fly_to_pipe, 820 },
#endif
    { cutscene_intro_peach_mario_appears, 270 },
    { cutscene_intro_peach_dialog, CUTSCENE_LOOP }
};
