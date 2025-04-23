#ifndef SSL_HEADER_H
#define SSL_HEADER_H

#include "types.h"
#include "game/moving_texture.h"

extern Movtex ssl_movtex_tris_quicksand_pit[];
extern Movtex ssl_movtex_tris_pyramid_quicksand_pit[];
extern const struct MovtexQuadCollection ssl_movtex_puddle_water[];
extern const struct MovtexQuadCollection ssl_movtex_toxbox_quicksand_mist[];
extern Movtex ssl_movtex_tris_pyramid_quicksand[];
extern Movtex ssl_movtex_tris_pyramid_corners_quicksand[];
extern Movtex ssl_movtex_tris_sides_quicksand[];
extern Movtex ssl_movtex_tris_pyramid_sand_pathway_front[];
extern Movtex ssl_movtex_tris_pyramid_sand_pathway_floor[];
extern Movtex ssl_movtex_tris_pyramid_sand_pathway_side[];

// script
extern const LevelScript level_ssl_entry[];

#include "levels/ssl/header.inc.h"

#endif
