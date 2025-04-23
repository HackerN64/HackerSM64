#include <ultra64.h>
#include "sm64.h"
#include "geo_commands.h"

#include "game/level_geo.h"
#include "game/geo_misc.h"
#include "game/camera.h"
#include "game/moving_texture.h"
#include "game/screen_transition.h"
#include "game/paintings.h"

#include "make_const_nonconst.h"

#include "actors/vanilla_actors/lll/header.h"

#include "actors/vanilla_actors/lll/10/geo.inc.c"
#include "actors/vanilla_actors/lll/moving_octagonal_mesh_platform/geo.inc.c"
#include "actors/vanilla_actors/lll/drawbridge_part/geo.inc.c"
#include "actors/vanilla_actors/lll/rotating_block_fire_bars/geo.inc.c"
#include "actors/vanilla_actors/lll/wooden_float_small/geo.inc.c"
#include "actors/vanilla_actors/lll/wooden_float_large/geo.inc.c"
#include "actors/vanilla_actors/lll/collapsing_wooden_platform/geo.inc.c"
#include "actors/vanilla_actors/lll/long_wooden_bridge/geo.inc.c"
#include "actors/vanilla_actors/lll/rotating_hexagonal_ring/geo.inc.c"
#include "actors/vanilla_actors/lll/sinking_rectangular_platform/geo.inc.c"
#include "actors/vanilla_actors/lll/sinking_square_platform/geo.inc.c"
#include "actors/vanilla_actors/lll/tilting_square_platform/geo.inc.c"
#include "actors/vanilla_actors/lll/puzzle_piece/geo.inc.c"
#include "actors/vanilla_actors/lll/sinking_rock_block/geo.inc.c"
#include "actors/vanilla_actors/lll/rolling_log/geo.inc.c"
#include "actors/vanilla_actors/lll/volcano_falling_trap/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_lll[] = {
    LOAD_MODEL_FROM_GEO(MODEL_LLL_DRAWBRIDGE_PART,                lll_geo_000B20),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_ROTATING_BLOCK_FIRE_BARS,       lll_geo_000B38),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_ROTATING_HEXAGONAL_RING,        lll_geo_000BB0),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_SINKING_RECTANGULAR_PLATFORM,   lll_geo_000BC8),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_SINKING_SQUARE_PLATFORMS,       lll_geo_000BE0),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_TILTING_SQUARE_PLATFORM,        lll_geo_000BF8),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_BOWSER_PIECE_1,                 lll_geo_bowser_puzzle_piece_1),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_BOWSER_PIECE_2,                 lll_geo_bowser_puzzle_piece_2),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_BOWSER_PIECE_3,                 lll_geo_bowser_puzzle_piece_3),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_BOWSER_PIECE_4,                 lll_geo_bowser_puzzle_piece_4),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_BOWSER_PIECE_5,                 lll_geo_bowser_puzzle_piece_5),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_BOWSER_PIECE_6,                 lll_geo_bowser_puzzle_piece_6),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_BOWSER_PIECE_7,                 lll_geo_bowser_puzzle_piece_7),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_BOWSER_PIECE_8,                 lll_geo_bowser_puzzle_piece_8),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_BOWSER_PIECE_9,                 lll_geo_bowser_puzzle_piece_9),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_BOWSER_PIECE_10,                lll_geo_bowser_puzzle_piece_10),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_BOWSER_PIECE_11,                lll_geo_bowser_puzzle_piece_11),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_BOWSER_PIECE_12,                lll_geo_bowser_puzzle_piece_12),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_BOWSER_PIECE_13,                lll_geo_bowser_puzzle_piece_13),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_BOWSER_PIECE_14,                lll_geo_bowser_puzzle_piece_14),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_MOVING_OCTAGONAL_MESH_PLATFORM, lll_geo_000B08),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_SINKING_ROCK_BLOCK,             lll_geo_000DD0),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_ROLLING_LOG,                    lll_geo_000DE8),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_ROTATING_HEXAGONAL_PLATFORM,    lll_geo_000A78),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_WOOD_BRIDGE,                    lll_geo_000B50),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_LARGE_WOOD_BRIDGE,              lll_geo_000B68),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_FALLING_PLATFORM,               lll_geo_000B80),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_LARGE_FALLING_PLATFORM,         lll_geo_000B98),
    LOAD_MODEL_FROM_GEO(MODEL_LLL_VOLCANO_FALLING_TRAP,           lll_geo_000EA8),
    RETURN(),
};
