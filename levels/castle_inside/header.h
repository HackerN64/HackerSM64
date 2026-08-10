#ifndef CASTLE_INSIDE_HEADER_H
#define CASTLE_INSIDE_HEADER_H

#include "types.h"
#include "game/paintings.h"
#include "game/moving_texture.h"

extern struct Painting bob_painting;
extern struct Painting ccm_painting;
extern struct Painting wf_painting;
extern struct Painting jrb_painting;
extern struct Painting lll_painting;
extern struct Painting ssl_painting;
extern struct Painting hmc_painting;
extern struct Painting ddd_painting;
extern struct Painting wdw_painting;
extern struct Painting thi_tiny_painting;
extern struct Painting ttm_painting;
extern struct Painting ttc_painting;
extern struct Painting sl_painting;
extern struct Painting thi_huge_painting;
extern const Trajectory *const inside_castle_seg7_trajectory_mips[];
extern const struct MovtexQuadCollection inside_castle_movtex_green_room_water[];
extern const struct MovtexQuadCollection inside_castle_movtex_moat_water[];

// script
extern const LevelScript level_castle_inside_entry[];

#include "levels/castle_inside/header.inc.h"

#endif
