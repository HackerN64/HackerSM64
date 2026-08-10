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

#include "actors/vanilla_actors/bits/header.h"

#include "actors/vanilla_actors/bits/20/geo.inc.c"
#include "actors/vanilla_actors/bits/21/geo.inc.c"
#include "actors/vanilla_actors/bits/22/geo.inc.c"
#include "actors/vanilla_actors/bits/23/geo.inc.c"
#include "actors/vanilla_actors/bits/24/geo.inc.c"
#include "actors/vanilla_actors/bits/25/geo.inc.c"
#include "actors/vanilla_actors/bits/26/geo.inc.c"
#include "actors/vanilla_actors/bits/27/geo.inc.c"
#include "actors/vanilla_actors/bits/28/geo.inc.c"
#include "actors/vanilla_actors/bits/29/geo.inc.c"
#include "actors/vanilla_actors/bits/30/geo.inc.c"
#include "actors/vanilla_actors/bits/31/geo.inc.c"
#include "actors/vanilla_actors/bits/32/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_bits[] = {
    LOAD_MODEL_FROM_GEO(MODEL_BITS_SLIDING_PLATFORM,       bits_geo_0005E0),
    LOAD_MODEL_FROM_GEO(MODEL_BITS_TWIN_SLIDING_PLATFORMS, bits_geo_0005F8),
    LOAD_MODEL_FROM_GEO(MODEL_BITS_OCTAGONAL_PLATFORM,     bits_geo_000610),
    LOAD_MODEL_FROM_GEO(MODEL_BITS_BLUE_PLATFORM,          bits_geo_000628),
    LOAD_MODEL_FROM_GEO(MODEL_BITS_FERRIS_WHEEL_AXLE,      bits_geo_000640),
    LOAD_MODEL_FROM_GEO(MODEL_BITS_ARROW_PLATFORM,         bits_geo_000658),
    LOAD_MODEL_FROM_GEO(MODEL_BITS_SEESAW_PLATFORM,        bits_geo_000670),
    LOAD_MODEL_FROM_GEO(MODEL_BITS_TILTING_W_PLATFORM,     bits_geo_000688),
    LOAD_MODEL_FROM_GEO(MODEL_BITS_STAIRCASE,              bits_geo_0006A0),
    LOAD_MODEL_FROM_GEO(MODEL_BITS_STAIRCASE_FRAME1,       bits_geo_0006B8),
    LOAD_MODEL_FROM_GEO(MODEL_BITS_STAIRCASE_FRAME2,       bits_geo_0006D0),
    LOAD_MODEL_FROM_GEO(MODEL_BITS_STAIRCASE_FRAME3,       bits_geo_0006E8),
    LOAD_MODEL_FROM_GEO(MODEL_BITS_STAIRCASE_FRAME4,       bits_geo_000700),
    RETURN(),
};
