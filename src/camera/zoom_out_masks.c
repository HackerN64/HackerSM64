#include <PR/ultratypes.h>
#include "level_table.h"
#include "macros.h"

/**
 * These masks set whether or not the camera zooms out when game is paused.
 *
 * Each entry is used by two levels. Even levels use the low 4 bits, odd levels use the high 4 bits
 * Because areas are 1-indexed, a mask of 0x1 will make area 1 (not area 0) zoom out.
 *
 * In zoom_out_if_paused_and_outside(), the current area is converted to a shift.
 * Then the value of (1 << shift) is &'d with the level's mask,
 * and if the result is non-zero, the camera will zoom out.
 */


/**
 * Helper macro for defining which areas of a level should zoom out the camera when the game is paused.
 * Because a mask is used by two levels, the pattern will repeat when more than 4 areas are used by a level.
 */
#define ZOOMOUT_AREA_MASK(level1Area1, level1Area2, level1Area3, level1Area4, \
                          level2Area1, level2Area2, level2Area3, level2Area4) \
    ((level2Area4) << 7 |                                                     \
     (level2Area3) << 6 |                                                     \
     (level2Area2) << 5 |                                                     \
     (level2Area1) << 4 |                                                     \
     (level1Area4) << 3 |                                                     \
     (level1Area3) << 2 |                                                     \
     (level1Area2) << 1 |                                                     \
     (level1Area1) << 0)

u8 sZoomOutAreaMasks[] = {
    ZOOMOUT_AREA_MASK(0,0,0,0, 0,0,0,0), // Unused         | Unused
    ZOOMOUT_AREA_MASK(0,0,0,0, 0,0,0,0), // Unused         | Unused
    ZOOMOUT_AREA_MASK(0,0,0,0, 1,0,0,0), // BBH            | CCM
    ZOOMOUT_AREA_MASK(0,0,0,0, 0,0,0,0), // CASTLE_INSIDE  | HMC
    ZOOMOUT_AREA_MASK(1,0,0,0, 1,0,0,0), // SSL            | BOB
    ZOOMOUT_AREA_MASK(1,0,0,0, 1,0,0,0), // SL             | WDW
    ZOOMOUT_AREA_MASK(0,0,0,0, 1,1,0,0), // JRB            | THI
    ZOOMOUT_AREA_MASK(0,0,0,0, 1,0,0,0), // TTC            | RR
    ZOOMOUT_AREA_MASK(1,0,0,0, 1,0,0,0), // CASTLE_GROUNDS | BITDW
    ZOOMOUT_AREA_MASK(0,0,0,0, 1,0,0,0), // VCUTM          | BITFS
    ZOOMOUT_AREA_MASK(0,0,0,0, 1,0,0,0), // SA             | BITS
    ZOOMOUT_AREA_MASK(1,0,0,0, 0,0,0,0), // LLL            | DDD
    ZOOMOUT_AREA_MASK(1,0,0,0, 0,0,0,0), // WF             | ENDING
    ZOOMOUT_AREA_MASK(0,0,0,0, 0,0,0,0), // COURTYARD      | PSS
    ZOOMOUT_AREA_MASK(0,0,0,0, 1,0,0,0), // COTMC          | TOTWC
    ZOOMOUT_AREA_MASK(1,0,0,0, 1,0,0,0), // BOWSER_1       | WMOTR
    ZOOMOUT_AREA_MASK(0,0,0,0, 1,0,0,0), // Unused         | BOWSER_2
    ZOOMOUT_AREA_MASK(1,0,0,0, 0,0,0,0), // BOWSER_3       | Unused
    ZOOMOUT_AREA_MASK(1,0,0,0, 0,0,0,0), // TTM            | Unused
    ZOOMOUT_AREA_MASK(0,0,0,0, 0,0,0,0), // Unused         | Unused
};

STATIC_ASSERT(ARRAY_COUNT(sZoomOutAreaMasks) - 1 == LEVEL_MAX / 2, "Make sure you edit sZoomOutAreaMasks when adding / removing courses.");
