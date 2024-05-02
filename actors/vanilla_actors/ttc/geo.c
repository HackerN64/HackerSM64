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

#include "actors/vanilla_actors/ttc/header.h"

#include "actors/vanilla_actors/ttc/rotating_cube/geo.inc.c"
#include "actors/vanilla_actors/ttc/rotating_prism/geo.inc.c"
#include "actors/vanilla_actors/ttc/pendulum/geo.inc.c"
#include "actors/vanilla_actors/ttc/large_treadmill/geo.inc.c"
#include "actors/vanilla_actors/ttc/small_treadmill/geo.inc.c"
#include "actors/vanilla_actors/ttc/push_block/geo.inc.c"
#include "actors/vanilla_actors/ttc/rotating_hexagon/geo.inc.c"
#include "actors/vanilla_actors/ttc/rotating_triangle/geo.inc.c"
#include "actors/vanilla_actors/ttc/pit_block/geo.inc.c"
#include "actors/vanilla_actors/ttc/pit_block_2/geo.inc.c"
#include "actors/vanilla_actors/ttc/elevator_platform/geo.inc.c"
#include "actors/vanilla_actors/ttc/clock_hand/geo.inc.c"
#include "actors/vanilla_actors/ttc/spinner/geo.inc.c"
#include "actors/vanilla_actors/ttc/small_gear/geo.inc.c"
#include "actors/vanilla_actors/ttc/large_gear/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_ttc[] = {
    LOAD_MODEL_FROM_GEO(MODEL_TTC_ROTATING_CUBE,     ttc_geo_000240),
    LOAD_MODEL_FROM_GEO(MODEL_TTC_ROTATING_PRISM,    ttc_geo_000258),
    LOAD_MODEL_FROM_GEO(MODEL_TTC_PENDULUM,          ttc_geo_000270),
    LOAD_MODEL_FROM_GEO(MODEL_TTC_LARGE_TREADMILL,   ttc_geo_000288),
    LOAD_MODEL_FROM_GEO(MODEL_TTC_SMALL_TREADMILL,   ttc_geo_0002A8),
    LOAD_MODEL_FROM_GEO(MODEL_TTC_PUSH_BLOCK,        ttc_geo_0002C8),
    LOAD_MODEL_FROM_GEO(MODEL_TTC_ROTATING_HEXAGON,  ttc_geo_0002E0),
    LOAD_MODEL_FROM_GEO(MODEL_TTC_ROTATING_TRIANGLE, ttc_geo_0002F8),
    LOAD_MODEL_FROM_GEO(MODEL_TTC_PIT_BLOCK,         ttc_geo_000310),
    LOAD_MODEL_FROM_GEO(MODEL_TTC_PIT_BLOCK_UNUSED,  ttc_geo_000328),
    LOAD_MODEL_FROM_GEO(MODEL_TTC_ELEVATOR_PLATFORM, ttc_geo_000340),
    LOAD_MODEL_FROM_GEO(MODEL_TTC_CLOCK_HAND,        ttc_geo_000358),
    LOAD_MODEL_FROM_GEO(MODEL_TTC_SPINNER,           ttc_geo_000370),
    LOAD_MODEL_FROM_GEO(MODEL_TTC_SMALL_GEAR,        ttc_geo_000388),
    LOAD_MODEL_FROM_GEO(MODEL_TTC_LARGE_GEAR,        ttc_geo_0003A0),
    RETURN(),
};
