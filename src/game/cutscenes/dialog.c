/**
 * Cutscene that plays when Mario talks to a creature.
 */
struct Cutscene sCutsceneDialog[] = {
    { cutscene_dialog, CUTSCENE_LOOP },
    { cutscene_dialog_set_flag, 12 },
    { cutscene_dialog_end, 0 }
};
