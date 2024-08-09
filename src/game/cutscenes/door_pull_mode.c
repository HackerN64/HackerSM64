/**
 * Cutscene that plays when Mario pulls open a door that has some special mode requirement on the other
 * side.
 */
struct Cutscene sCutsceneDoorPullMode[] = {
// HackerSM64 TODO: Properly transition when moving through doors
#ifndef FORCED_CAMERA_MODE
    { cutscene_door_start, 1 },
    { cutscene_door_fix_cam, 30 },
#endif
    { cutscene_door_mode, CUTSCENE_LOOP }
};
