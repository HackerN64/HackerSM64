/**
 * Cutscene that plays when Mario pulls open a door.
 */
struct Cutscene sCutsceneDoorPull[] = {
// HackerSM64 TODO: Properly transition when moving through doors
#ifndef FORCED_CAMERA_MODE
    { cutscene_door_start, 1 },
    { cutscene_door_fix_cam, 30 },
    { cutscene_door_move_behind_mario, 1 },
    { cutscene_door_follow_mario, 50 },
#endif
    { cutscene_door_end, 0 }
};
