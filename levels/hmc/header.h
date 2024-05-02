#ifndef HMC_HEADER_H
#define HMC_HEADER_H

#include "types.h"
#include "game/moving_texture.h"

extern const Texture *const hmc_seg7_painting_textures_07025518[];
extern struct Painting cotmc_painting;
extern const Trajectory hmc_seg7_trajectory_checkerboard_platform[];
extern const struct MovtexQuadCollection hmc_movtex_dorrie_pool_water[];
extern const struct MovtexQuadCollection hmc_movtex_toxic_maze_mist[];

// script
extern const LevelScript level_hmc_entry[];

#include "levels/hmc/header.inc.h"

#endif
