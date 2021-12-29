#pragma once

/*****************************
 * COMPATIBILITY SAFE GUARDS *
 *****************************/

/**
 * NOTE: Only mess with these if you know what you're doing!
 * These are put in place to insure that connected functionality works as intended.
 */


/*****************
 * config_graphics
 */

#ifndef F3DZEX_GBI_2
    #undef OBJECTS_REJ // OBJECTS_REJ requires f3dzex.
#endif // !F3DZEX_GBI_2

#ifndef F3DEX_GBI_SHARED
    #undef OBJECTS_REJ // Non F3DEX-based ucodes do NOT support ucode switching.
#endif // !F3DEX_GBI_SHARED

#ifdef OBJECTS_REJ
// Enable required ucodes.
    #define F3DEX2_REJ_GBI
    #define F3DLX2_REJ_GBI
#endif // OBJECTS_REJ


/*****************
 * config_debug
 */

#ifdef PUPPYPRINT_DEBUG
    #ifndef PUPPYPRINT
        #define PUPPYPRINT
    #endif
#endif

#ifdef COMPLETE_SAVE_FILE
    #ifndef UNLOCK_ALL
        #define UNLOCK_ALL
    #endif
#endif // COMPLETE_SAVE_FILE

#ifdef DEBUG_ALL
    #ifndef DEBUG_LEVEL_SELECT
        #define DEBUG_LEVEL_SELECT
    #endif
    #ifndef ENABLE_DEBUG_FREE_MOVE
        #define ENABLE_DEBUG_FREE_MOVE
    #endif
    #ifndef PUPPYPRINT
        #define PUPPYPRINT
    #endif
    #ifndef PUPPYPRINT_DEBUG
        #define PUPPYPRINT_DEBUG 1
    #endif
    #ifndef VISUAL_DEBUG
        #define VISUAL_DEBUG
    #endif
    #ifndef UNLOCK_ALL
        #define UNLOCK_ALL
    #endif
    #ifndef COMPLETE_SAVE_FILE
        #define COMPLETE_SAVE_FILE
    #endif
#endif // DEBUG_ALL

#ifdef DISABLE_ALL
    #undef DEBUG_ALL
    #undef TEST_LEVEL
    #undef DEBUG_LEVEL_SELECT
    #undef ENABLE_DEBUG_FREE_MOVE
    #undef VANILLA_DEBUG
    #undef CUSTOM_DEBUG
    #undef PUPPYPRINT_DEBUG
    #undef PUPPYPRINT_DEBUG_CYCLES
    #undef VISUAL_DEBUG
    #undef UNLOCK_ALL
    #undef COMPLETE_SAVE_FILE
    #undef DEBUG_FORCE_CRASH_ON_BOOT
#endif // DISABLE_ALL


/*****************
 * config_camera
 */

#ifdef FORCED_CAMERA_MODE
    #ifndef USE_COURSE_DEFAULT_MODE
        #define USE_COURSE_DEFAULT_MODE // Forced camera mode overwrites the default mode
    #endif
#endif

#ifndef WATER_SURFACE_CAMERA_MODE
    #define WATER_SURFACE_CAMERA_MODE CAMERA_MODE_WATER_SURFACE
#endif

#ifndef DEEP_WATER_CAMERA_MODE
    #define DEEP_WATER_CAMERA_MODE CAMERA_MODE_BEHIND_MARIO
#endif

#ifndef FLYING_CAMERA_MODE
    #define FLYING_CAMERA_MODE CAMERA_MODE_BEHIND_MARIO
#endif


/*****************
 * config_game
 */

#ifdef DISABLE_LIVES
    #undef SAVE_NUM_LIVES
#endif // DISABLE_LIVES

#ifndef START_LEVEL
    #define START_LEVEL LEVEL_CASTLE_GROUNDS
#endif


/*****************
 * config_goddard
 */

#ifndef KEEP_MARIO_HEAD
    #undef GODDARD_EASTER_EGG

    #ifndef DISABLE_DEMO
        #define DISABLE_DEMO
    #endif
#endif // !KEEP_MARIO_HEAD

/*****************
 * config_menu
 */

#ifdef DISABLE_EXIT_COURSE
    #undef EXIT_COURSE_WHILE_MOVING
    #undef EXIT_COURSE_LEVEL
    #undef EXIT_COURSE_AREA
    #undef EXIT_COURSE_NODE
#endif // DISABLE_EXIT_COURSE


/*****************
 * config_objects
 */

// Enable floombas if the intro floombas are enabled
#ifdef INTRO_FLOOMBAS
    #ifndef FLOOMBAS
        #define FLOOMBAS
    #endif
#endif


/*****************
 * config_rom
 */

#ifndef TARGET_N64
    #undef BORDER_HEIGHT_CONSOLE
    #undef BORDER_HEIGHT_EMULATOR
    #define BORDER_HEIGHT_CONSOLE  0
    #define BORDER_HEIGHT_EMULATOR 0
#endif // !TARGET_N64
