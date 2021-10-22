#ifndef __EXTENDED_BOUNDS_H__
#define __EXTENDED_BOUNDS_H__

/*
    Better Extended Bounds by anonymous_moose
        Thanks to someone2639 for the shiftable segments patch
        Thanks to Wiseguy for the Surface Pool Full error code and 4x bounds fix

    0: Regular bounds
        Same as vanilla sm64, boundaries are (-8192 to 8191)
        16x16 collision cells.
    1: 2x extended bounds
        level boundaries are twice as big (-16384 to 16383)
        Collision calculations remain as fast as vanilla, at the cost of using more RAM.
        32x32 collision cells.
    2: Regular bounds (performance)
        Same boundaries as vanilla (-8192 to 8191), but with twice the amount of collision cells
        Trades more RAM usage for faster collision calculations.
        32x32 collision cells.
    3: 4x extended bounds
        level boundaries are 4 times as big (-32768 to 32767)
        Collision calculations remain as fast as vanilla, at the cost of using far more RAM (16 times vanilla).
        64x64 collision cells.

    If you see "SURFACE POOL FULL" or "SURFACE NODE POOL FULL" in game, you should increase
    SURFACE_POOL_SIZE or SURFACE_NODE_POOL_SIZE, respectively, or reduce the amount of
    collision surfaces in your level.
*/

// For the static assert macro
#include "macros.h"
#include "config.h"

// Set EXTENDED_BOUNDS_MODE in /include/config/config_collision.h, then do "make clean".

// Don't touch the stuff past this point unless you know what you're doing!

#if EXTENDED_BOUNDS_MODE == 0 // 1x, normal cell size
    #define LEVEL_BOUNDARY_MAX 0x2000L //  8192
    #define CELL_SIZE          0x400   //  1024, NUM_CELLS = 16
#elif EXTENDED_BOUNDS_MODE == 1 // 2x, normal cell size
    #define LEVEL_BOUNDARY_MAX 0x4000L // 16384
    #define CELL_SIZE          0x400   //  1024, NUM_CELLS = 32
#elif EXTENDED_BOUNDS_MODE == 2 // 1x, smaller cell size
    #define LEVEL_BOUNDARY_MAX 0x2000L //  8192
    #define CELL_SIZE          0x200   //   512, NUM_CELLS = 32
#elif EXTENDED_BOUNDS_MODE == 3 // 4x, normal cell size
    #define LEVEL_BOUNDARY_MAX 0x8000L // 32768
    #define CELL_SIZE          0x400   //  1024, NUM_CELLS = 64
#endif

STATIC_ASSERT(((EXTENDED_BOUNDS_MODE >= 0) && (EXTENDED_BOUNDS_MODE <= 3)), "You must set a valid extended bounds mode!");

// The amount of cells in each axis in an area.
#define NUM_CELLS                   (2 * LEVEL_BOUNDARY_MAX / CELL_SIZE)
// The maximum amount of collision surfaces (static and dynamic combined)
#define SURFACE_POOL_SIZE           (LEVEL_BOUNDARY_MAX / 2) // Vanilla: 2300
// The maximum amount of SurfaceNodes (static and dynamic combined).
// Each cell has a SurfaceNode for every surface which intersects it,
// so each cell a Surface intersects with gets its own SurfaceNode,
// so larger surfaces means more SurfaceNodes.
// Multiply SURFACE_POOL_SIZE by the average amount of cells the surfaces intersect.
#define SURFACE_NODE_POOL_SIZE      (SURFACE_POOL_SIZE * 4) // Vanilla: 7000

// Flags for error messages
#define NOT_ENOUGH_ROOM_FOR_SURFACES (1 << 0)
#define NOT_ENOUGH_ROOM_FOR_NODES    (1 << 1)

// Use this to convert game units to cell coordinates
#define GET_CELL_COORD(p)   ((((s32)(p) + LEVEL_BOUNDARY_MAX) / CELL_SIZE) & (NUM_CELLS - 1));

#endif // __EXTENDED_BOUNDS_H__
