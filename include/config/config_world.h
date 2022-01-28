#pragma once


/**************************
 * EXTENDED BOUNDS CONFIG *
 **************************/

// -- General Config --

/**
 * The distance the level boundary is from the origin.
 * For best performance results, this should be a power of 2.
 * Values lower than vanilla are not recommended.
 * Vanilla: 0x2000 (8192)
 */
#define LEVEL_BOUNDARY_MAX 8192

/**
 * The number of cells along each axis in an area.
 * Higher numbers = smaller cells = higher performance + higher RAM usage.
 * Lower numbers = larger cells = lower perfocmance + lower RAM usage.
 * For best performance results, this should be a power of 2,
 * since that allows for GET_CELL_COORD to be optimized.
 * Vanilla: 16
 */
#define NUM_CELLS 32


// -- Don't touch the stuff past this point unless you know what you're doing! --


// -- Precomputed constants --

/**
 * World scale value. This allows you to scale down geometry by the given amount,
 * which allows for larger levels without the distortion you would otherwise get.
 * Larger world scale comes at a cost of precision, which can increase Z-fighting.
 * Values above 4 should not be necessary.
 * Vanilla: 1
 */
#define WORLD_SCALE (LEVEL_BOUNDARY_MAX / 8192.0f)

/**
 * The size of one collision cell.
 * Vanilla: 0x400 (1024)
 */
#define CELL_SIZE ((LEVEL_BOUNDARY_MAX * 2) / NUM_CELLS)


// -- Memory pool sizes --

/**
 *  If you see "SURFACE POOL FULL" or "SURFACE NODE POOL FULL" in game, you should increase
 *  SURFACE_POOL_SIZE or SURFACE_NODE_POOL_SIZE, respectively, or reduce the amount of
 *  collision surfaces in your level.
 */

/**
 * The maximum amount of collision surfaces (static and dynamic combined).
 * Vanilla: 2300
 */
#define SURFACE_POOL_SIZE (LEVEL_BOUNDARY_MAX / 2)

/**
 * The maximum amount of SurfaceNodes (static and dynamic combined).
 * Each cell creates a SurfaceNode for each Surface in it.
 * This means one Surface can have multiple SurfaceNodes if it is more than one collision cell.
 * This should always be larger than SURFACE_POOL_SIZE.
 * Vanilla: 7000
 */
#define SURFACE_NODE_POOL_SIZE (SURFACE_POOL_SIZE * 4)


// -- Macros --

/**
 * Use this to convert game units to cell coordinates:
 */
#define GET_CELL_COORD(p) ((((s32)(p) + LEVEL_BOUNDARY_MAX) / CELL_SIZE) % NUM_CELLS)
