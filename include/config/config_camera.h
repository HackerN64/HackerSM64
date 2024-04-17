#pragma once

/*******************
 * CAMERA SETTINGS *
 *******************/

/**
 * Forces the camera mode to your choice (except when swimming or flying).
 * It does this by setting the area's camera->defMode to this mode, and also
 * changes hardcoded modes to use c->defMode.
 *      Note: removes door cutscenes due to the way they're designed to work with specific modes.
 *      Search for FORCED_CAMERA_MODE in camera.c for more information.
 */
#define FORCED_CAMERA_MODE        CAMERA_MODE_8_DIRECTIONS

/**
 * Changes hardcoded camera mode reverting to instead use the area's default mode (defMode).
 * If you're using a FORCED_CAMERA_MODE, this must be on for it to work.
 */
#define USE_COURSE_DEFAULT_MODE

/***** Movement specific camera modes: *****/
/**
 * Camera mode when Mario is < 400 units away from the water surface (default is CAMERA_MODE_WATER_SURFACE).
 */
#define WATER_SURFACE_CAMERA_MODE CAMERA_MODE_WATER_SURFACE
/**
 * Camera mode when Mario is > 800 units away from the water surface (default is CAMERA_MODE_BEHIND_MARIO).
 */
#define DEEP_WATER_CAMERA_MODE    CAMERA_MODE_BEHIND_MARIO
/**
 * Camera mode when Mario is flying (default is CAMERA_MODE_BEHIND_MARIO).
 */
#define FLYING_CAMERA_MODE        CAMERA_MODE_BEHIND_MARIO
/*******************************************/

/** 
 * Makes the camera approach Mario's height much more quickly.
 */
#define FAST_VERTICAL_CAMERA_MOVEMENT

/**
 * Enables camera collision for 8 direction camera and, by extension, to Parallel Lakitu cam or Reonucam if enabled.
 * If you enable it, please consider using surface types with the SURFACE_FLAG_NO_CAM_COLLISION flag for small obstacles,
 * such as fences, pillars, signs, etc, in order to make your game more enjoyable and not let the camera get in the way of gameplay.
 */
#define EIGHT_DIR_CAMERA_COLLISION

/**
 * Enables "parallel lakitu camera" or "aglab cam" which lets you move the camera smoothly with the D-pad. Will be disabled if Reonucam is enabled.
 */
#define PARALLEL_LAKITU_CAM

// Enables Reonucam, a custom camera that aims to be a more feature-rich "aglabcam" that doesn't use a single button more than the vanilla camera. 
// An explanation the features can be seen here: https://www.youtube.com/watch?v=TQNkznX9Z3k (please note that the analog feature shown at the end is no longer present)
#define REONUCAM

/**
 * Enables Puppy Camera 2, a rewritten camera that can be freely configured and modified.
 */
// #define PUPPYCAM

/**********************************/
/***** Vanilla config options *****/
/**********************************/

/**
 * Included for ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS define.
 */
#include "config_game.h"

/**
 * Allow course specific camera processing.
 * You will likely want this disabled in non-vanilla hacks.
 * This is automatically enabled when ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS is enabled,
 * but feel free to override it if you really want to for some reason.
 */
#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
    #define ENABLE_VANILLA_CAM_PROCESSING
#endif

// Reonucam overrides
#ifdef REONUCAM
    // Use course default mode
    #ifndef USE_COURSE_DEFAULT_MODE
    #define USE_COURSE_DEFAULT_MODE
    #endif

    // Force camera mode to 8 Dir
    #ifdef FORCED_CAMERA_MODE
    #undef FORCED_CAMERA_MODE
    #endif
    #define FORCED_CAMERA_MODE CAMERA_MODE_8_DIRECTIONS

    // Disable vanilla cam processing
    #ifdef ENABLE_VANILLA_CAM_PROCESSING
    #undef ENABLE_VANILLA_CAM_PROCESSING
    #endif
#endif
