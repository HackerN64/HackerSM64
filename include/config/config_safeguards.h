#pragma once

/*****************************
 * COMPATIBILITY SAFE GUARDS *
 *****************************/

/**
 * NOTE: Only mess with these if you know what you're doing!
 * These are put in place to insure that connected functionality works as intended.
 */


/*****************
 * config_audio
 */

#ifndef MAX_SIMULTANEOUS_NOTES_EMULATOR
    #ifdef EXPAND_AUDIO_HEAP
        #define MAX_SIMULTANEOUS_NOTES_EMULATOR 40
    #else
        #define MAX_SIMULTANEOUS_NOTES_EMULATOR 20
    #endif
#endif // MAX_SIMULTANEOUS_NOTES_EMULATOR

#ifndef MAX_SIMULTANEOUS_NOTES_CONSOLE
    #ifdef EXPAND_AUDIO_HEAP
        #define MAX_SIMULTANEOUS_NOTES_CONSOLE 24
    #else
        #define MAX_SIMULTANEOUS_NOTES_CONSOLE 16
    #endif
#endif // MAX_SIMULTANEOUS_NOTES_CONSOLE

#if (MAX_SIMULTANEOUS_NOTES_EMULATOR >= MAX_SIMULTANEOUS_NOTES_CONSOLE)
    #define MAX_SIMULTANEOUS_NOTES MAX_SIMULTANEOUS_NOTES_EMULATOR
#else
    #define MAX_SIMULTANEOUS_NOTES MAX_SIMULTANEOUS_NOTES_CONSOLE
#endif

// Anything higher than 64 will most likely crash on boot. Even if it doesn't, it's still dangerous.
#if (MAX_SIMULTANEOUS_NOTES > 64)
    #undef MAX_SIMULTANEOUS_NOTES
    #define MAX_SIMULTANEOUS_NOTES 64
#elif (MAX_SIMULTANEOUS_NOTES < 0)
    #undef MAX_SIMULTANEOUS_NOTES
    #define MAX_SIMULTANEOUS_NOTES 0
#endif

#if defined(BETTER_REVERB) && !(defined(VERSION_US) || defined(VERSION_JP))
    #undef BETTER_REVERB
#endif


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
    #undef PUPPYPRINT
    #define PUPPYPRINT
#endif

#ifdef COMPLETE_SAVE_FILE
    #undef UNLOCK_ALL
    #define UNLOCK_ALL
#endif // COMPLETE_SAVE_FILE

#ifdef DEBUG_ALL
    #undef DEBUG_LEVEL_SELECT
    #define DEBUG_LEVEL_SELECT

    #undef ENABLE_DEBUG_FREE_MOVE
    #define ENABLE_DEBUG_FREE_MOVE

    #undef PUPPYPRINT
    #define PUPPYPRINT

    #undef PUPPYPRINT_DEBUG
    #define PUPPYPRINT_DEBUG 1

    #undef VISUAL_DEBUG
    #define VISUAL_DEBUG

    #undef UNLOCK_ALL
    #define UNLOCK_ALL

    #undef COMPLETE_SAVE_FILE
    #define COMPLETE_SAVE_FILE
#endif // DEBUG_ALL

#ifdef DISABLE_ALL
    #undef DEBUG_ALL
    #undef TEST_LEVEL
    #undef DEBUG_LEVEL_SELECT
    #undef ENABLE_DEBUG_FREE_MOVE
    #undef VANILLA_DEBUG
    #undef VANILLA_STYLE_CUSTOM_DEBUG
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
    #undef USE_COURSE_DEFAULT_MODE
    #define USE_COURSE_DEFAULT_MODE // Forced camera mode overwrites the default mode
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

    #undef DISABLE_DEMO
    #define DISABLE_DEMO
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
    #undef FLOOMBAS
    #define FLOOMBAS
#endif


/*****************
 * config_rom
 */

#ifndef TARGET_N64
    #undef BORDER_HEIGHT_CONSOLE
    #define BORDER_HEIGHT_CONSOLE  0

    #undef BORDER_HEIGHT_EMULATOR
    #define BORDER_HEIGHT_EMULATOR 0
#endif // !TARGET_N64
