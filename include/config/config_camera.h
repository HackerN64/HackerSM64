#pragma once

/*******************
 * CAMERA SETTINGS *
 *******************/

// Forces the camera mode to your choice (except when swimming or flying).
// It does this by setting the area's camera->defMode to this mode, and also
// changes hardcoded modes to use c->defMode.
//      Note: removes door cutscenes due to the way they're designed to work with specific modes.
//      Search for FORCED_CAMERA_MODE in camera.c for more information.
#define FORCED_CAMERA_MODE        CAMERA_MODE_8_DIRECTIONS

// Changes hardcoded camera mode reverting to instead use the area's default mode (defMode).
// If you're using a FORCED_CAMERA_MODE, this must be on for it to work.
#define USE_COURSE_DEFAULT_MODE

/***** Movement specific camera modes: *****/
// Camera mode when Mario is < 400 units away from the water surface (default is CAMERA_MODE_WATER_SURFACE).
#define WATER_SURFACE_CAMERA_MODE CAMERA_MODE_WATER_SURFACE
// Camera mode when Mario is > 800 units away from the water surface (default is CAMERA_MODE_BEHIND_MARIO).
#define DEEP_WATER_CAMERA_MODE    CAMERA_MODE_BEHIND_MARIO
// Camera mode when Mario is flying (default is CAMERA_MODE_BEHIND_MARIO).
#define FLYING_CAMERA_MODE        CAMERA_MODE_BEHIND_MARIO
/*******************************************/

// Makes the camera approach Mario's height much more quickly.
#define FAST_VERTICAL_CAMERA_MOVEMENT

// Enables "parallel lakitu camera" or "aglab cam" which lets you move the camera smoothly with the D-pad.
#define PARALLEL_LAKITU_CAM

// Enables Puppy Camera 2, a rewritten camera that can be freely configured and modified.
// #define PUPPYCAM

// Note: Reonucam is available, but because we had no time to test it properly, it's included as a patch rather than being in the code by default.
// Run this command to apply the patch if you want to use it:
// tools/apply_patch.sh enhancements/reonucam.patch
// Consider it a beta, but it should work fine. Please report any bugs with it. Applying the patch will simply add a define here, so you can still turn it off even after patching.

/**********************************/
/***** Vanilla config options *****/
/**********************************/

// Included for ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS define.
#include "config_game.h"

// Allow course specific camera processing.
// You will likely want this disabled in non-vanilla hacks.
// This is automatically enabled when ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS is enabled,
// but feel free to override it if you really want to for some reason/
#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
    #define ENABLE_VANILLA_CAM_PROCESSING
#endif
