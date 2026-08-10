#ifndef TTM_HEADER_H
#define TTM_HEADER_H

#include "types.h"
#include "game/moving_texture.h"

extern struct Painting ttm_slide_painting;
extern const Trajectory ttm_seg7_trajectory_070170A0[];
extern const struct MovtexQuadCollection ttm_movtex_puddle[];
extern Movtex ttm_movtex_tris_begin_waterfall[];
extern Movtex ttm_movtex_tris_begin_puddle_waterfall[];
extern Movtex ttm_movtex_tris_end_waterfall[];
extern Movtex ttm_movtex_tris_end_puddle_waterfall[];
extern Movtex ttm_movtex_tris_puddle_waterfall[];

// script
extern const LevelScript level_ttm_entry[];

#include "levels/ttm/header.inc.h"

#endif
