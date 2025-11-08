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

#include "actors/vanilla_actors/bitdw/header.h"

#include "actors/vanilla_actors/bitdw/sliding_platform/geo.inc.c"
#include "actors/vanilla_actors/bitdw/seesaw_platform/geo.inc.c"
#include "actors/vanilla_actors/bitdw/square_platform/geo.inc.c"
#include "actors/vanilla_actors/bitdw/ferris_wheel_axle/geo.inc.c"
#include "actors/vanilla_actors/bitdw/ferris_platform/geo.inc.c"
#include "actors/vanilla_actors/bitdw/collapsing_stairs_1/geo.inc.c"
#include "actors/vanilla_actors/bitdw/collapsing_stairs_2/geo.inc.c"
#include "actors/vanilla_actors/bitdw/collapsing_stairs_3/geo.inc.c"
#include "actors/vanilla_actors/bitdw/collapsing_stairs_4/geo.inc.c"
#include "actors/vanilla_actors/bitdw/collapsing_stairs_5/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_bitdw[] = {
    LOAD_MODEL_FROM_GEO(MODEL_BITDW_SQUARE_PLATFORM,   geo_bitdw_000558),
    LOAD_MODEL_FROM_GEO(MODEL_BITDW_SEESAW_PLATFORM,   geo_bitdw_000540),
    LOAD_MODEL_FROM_GEO(MODEL_BITDW_SLIDING_PLATFORM,  geo_bitdw_000528),
    LOAD_MODEL_FROM_GEO(MODEL_BITDW_FERRIS_WHEEL_AXLE, geo_bitdw_000570),
    LOAD_MODEL_FROM_GEO(MODEL_BITDW_BLUE_PLATFORM,     geo_bitdw_000588),
    LOAD_MODEL_FROM_GEO(MODEL_BITDW_STAIRCASE_FRAME4,  geo_bitdw_0005A0),
    LOAD_MODEL_FROM_GEO(MODEL_BITDW_STAIRCASE_FRAME3,  geo_bitdw_0005B8),
    LOAD_MODEL_FROM_GEO(MODEL_BITDW_STAIRCASE_FRAME2,  geo_bitdw_0005D0),
    LOAD_MODEL_FROM_GEO(MODEL_BITDW_STAIRCASE_FRAME1,  geo_bitdw_0005E8),
    LOAD_MODEL_FROM_GEO(MODEL_BITDW_STAIRCASE,         geo_bitdw_000600),
    RETURN(),
};
