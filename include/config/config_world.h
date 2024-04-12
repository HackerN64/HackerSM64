#pragma once

// For the static assert macro
#include "macros.h"

/**
 * World scale value. This allows you to scale down geometry by the given amount, which allows for larger levels
 * without the distortion you would otherwise get. Larger world scale comes at a cost of precision, which
 * can increase Z-fighting. Values above 4 should not be necessary.
 *
 * Uncomment this out to specifically set it, otherwise world scale will be based off of your extended bounds mode.
 */
// #define WORLD_SCALE 1

/**
 *  0: Regular bounds
 *      Same as vanilla sm64, boundaries are (-8192 to 8191)
 *      16x16 collision cells.
 *  1: 2x extended bounds
 *      level boundaries are twice as big (-16384 to 16383)
 *      Collision calculations remain as fast as vanilla, at the cost of using more RAM.
 *      32x32 collision cells.
 *  2: Regular bounds (performance)
 *      Same boundaries as vanilla (-8192 to 8191), but with twice the amount of collision cells
 *      Trades more RAM usage for faster collision calculations.
 *      32x32 collision cells.
 *  3: 4x extended bounds
 *      level boundaries are 4 times as big (-32768 to 32767)
 *      Collision calculations remain as fast as vanilla, at the cost of using far more RAM (16 times vanilla).
 *      64x64 collision cells.
 */

/**
 * Set this to the extended bounds mode you want, then do "make clean".
 */
#define EXTENDED_BOUNDS_MODE 1

/**************************************************************************
 * Don't touch the stuff past this point unless you know what you're doing!
 **************************************************************************/

#if EXTENDED_BOUNDS_MODE == 0 // 1x, normal cell size
    #define LEVEL_BOUNDARY_MAX 0x2000L //  8192
    #define CELL_SIZE          0x400   //  1024, NUM_CELLS = 16
    #ifndef WORLD_SCALE
        #define WORLD_SCALE 1
    #endif
#elif EXTENDED_BOUNDS_MODE == 1 // 2x, normal cell size
    #define LEVEL_BOUNDARY_MAX 0x4000L // 16384
    #define CELL_SIZE          0x400   //  1024, NUM_CELLS = 32
    #ifndef WORLD_SCALE
        #define WORLD_SCALE 2
    #endif
#elif EXTENDED_BOUNDS_MODE == 2 // 1x, smaller cell size
    #define LEVEL_BOUNDARY_MAX 0x2000L //  8192
    #define CELL_SIZE          0x200   //   512, NUM_CELLS = 32
    #ifndef WORLD_SCALE
        #define WORLD_SCALE 1
    #endif
#elif EXTENDED_BOUNDS_MODE == 3 // 4x, normal cell size
    #define LEVEL_BOUNDARY_MAX 0x8000L // 32768
    #define CELL_SIZE          0x400   //  1024, NUM_CELLS = 64
    #ifndef WORLD_SCALE
        #define WORLD_SCALE 4
    #endif
#endif

STATIC_ASSERT(((EXTENDED_BOUNDS_MODE >= 0) && (EXTENDED_BOUNDS_MODE <= 3)), "You must set a valid extended bounds mode!");

/**
 * The amount of cells in each axis in an area.
 */
#define NUM_CELLS                   (2 * LEVEL_BOUNDARY_MAX / CELL_SIZE)

/**
 * Use this to convert game units to cell coordinates.
 */
#define GET_CELL_COORD(p)   ((((s32)(p) + LEVEL_BOUNDARY_MAX) / CELL_SIZE) & (NUM_CELLS - 1))
