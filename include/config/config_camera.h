#pragma once

/*******************
 * CAMERA SETTINGS *
 *******************/

// Force the camera mode to your choice
//      Note: this is very aggressive (e.g. removes door cutscenes)
//      Search for FORCED_CAMERA_MODE in camera.c for more information
#define FORCED_CAMERA_MODE        CAMERA_MODE_8_DIRECTIONS

/** Movement specific camera modes: **/
// Camera mode when Mario is < 400 units away from the water surface (default is CAMERA_MODE_WATER_SURFACE)
#define WATER_SURFACE_CAMERA_MODE CAMERA_MODE_WATER_SURFACE
// Camera mode when Mario is > 800 units away from the water surface (default is CAMERA_MODE_BEHIND_MARIO)
#define DEEP_WATER_CAMERA_MODE    CAMERA_MODE_BEHIND_MARIO
// Camera mode when Mario is flying (default is CAMERA_MODE_BEHIND_MARIO)
#define FLYING_CAMERA_MODE        CAMERA_MODE_BEHIND_MARIO
/*************************************/

// Makes the camera approach Mario's height much more quickly
#define FAST_VERTICAL_CAMERA_MOVEMENT

// Enables "parallel lakitu camera" or "aglab cam" which lets you move the camera smoothly with the dpad
#define PARALLEL_LAKITU_CAM

// Enables Puppy Camera 2, a rewritten camera that can be freely configured and modified.
// #define PUPPYCAM


/**********************************/
/***** Vanilla config options *****/
/**********************************/

// Included for ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS define
#include "config_game.h"

// Allow course specific camera processing
// You will likely want this disabled in non-vanilla hacks
// This is automatically enabled when ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS is enabled,
// but feel free to override it if you really want to for some reason
#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
#define ENABLE_VANILLA_CAM_PROCESSING
#endif


/**************************************/
/****** Compatibility safeguards ******/
/**************************************/
// Don't change these unless you know what you're doing
#ifndef WATER_SURFACE_CAMERA_MODE
#define WATER_SURFACE_CAMERA_MODE CAMERA_MODE_WATER_SURFACE
#endif
#ifndef DEEP_WATER_CAMERA_MODE
#define DEEP_WATER_CAMERA_MODE CAMERA_MODE_BEHIND_MARIO
#endif
