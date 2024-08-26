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
