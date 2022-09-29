#pragma once

/******************
 * DEBUG SETTINGS *
 ******************/

/**
 * Enables most debug options, including PUPPYPRINT_DEBUG.
 */
// #define DEBUG_ALL

/**
 * Disables all debug options (except PUPPYPRINT).
 */
#define DISABLE_ALL

/**
 * Enables a comprehensive standalone profiler. Automatically enabled by PUPPYPRINT_DEBUG.
 * If not using PUPPYPRINT_DEBUG, press L to toggle the profiler.
 */
#define USE_PROFILER

/**
 * -- DEBUG STARTUP SPAWN PARAMETERS --
 * Sets a level in order to boot straight into said level, skipping the title screen and file select.
 * This allows you to quickly test the level or location you're working on.
 * NOTE: STARTUP_SPAWN_LEVEL is required for the other parameters to have an effect.
 * NOTE: It is recommended to set STARTUP_SPAWN_POS in addition to STARTUP_SPAWN_AREA, otherwise the default position may be out of bounds.
 * NOTE: STARTUP_SPAWN_ANGLE is an s16 angle.
 */
// #define STARTUP_SPAWN_LEVEL LEVEL_BOB
// #define STARTUP_SPAWN_AREA  1
// #define STARTUP_SPAWN_POS   0, 0, 0
// #define STARTUP_SPAWN_ANGLE 0x0

/**
 * Enables debug level select. Hold L while the game boots to turn it on.
 */
#define DEBUG_LEVEL_SELECT

/**
 * Enables debug free move (D-pad up to enter, A to exit).
 */
#define ENABLE_DEBUG_FREE_MOVE

/**
 * Enables a custom, enhanced performance profiler. (Enables PUPPYPRINT by default in config_safeguards).
 */
// #define PUPPYPRINT_DEBUG 1

/**
 * Uses cycles instead of microseconds in Puppyprint debug output.
 */
// #define PUPPYPRINT_DEBUG_CYCLES

/**
 * A vanilla style debug mode. It doesn't rely on a text engine, but it's much less powerful that PUPPYPRINT_DEBUG.
 * Press D-pad left to show the debug UI.
 */
// #define VANILLA_STYLE_CUSTOM_DEBUG

/**
 * Visual debug enables some collision visuals. Tapping Right on the D-pad will cycle between visual hitboxes, visual surfaces, both, and neither.
 * If puppyprint is enabled, then this can be cycled only while the screen is active.
 */
// #define VISUAL_DEBUG

/**
 * Opens all courses and doors. Used for debugging purposes to unlock all content.
 */
#define UNLOCK_ALL

/**
 * Same as above, but also reads all save file flags as complete.
 * This will not overwrite existing save file data unless you save over it.
 */
// #define COMPLETE_SAVE_FILE

/**
 * Removes the limit on FPS.
 */
// #define UNLOCK_FPS

/**
 * Includes vanilla debug functionality.
 */
// #define VANILLA_DEBUG

/**
 * Forces a crash when the game starts. Useful for debugging the crash screen.
 */
// #define DEBUG_FORCE_CRASH_ON_BOOT
