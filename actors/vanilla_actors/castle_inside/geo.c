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

#include "actors/vanilla_actors/castle_inside/header.h"

#include "actors/vanilla_actors/castle_inside/star_door/geo.inc.c"
#include "actors/vanilla_actors/castle_inside/trap_door/geo.inc.c"
#include "actors/vanilla_actors/castle_inside/clock_minute_hand/geo.inc.c"
#include "actors/vanilla_actors/castle_inside/clock_hour_hand/geo.inc.c"
#include "actors/vanilla_actors/castle_inside/water_level_pillar/geo.inc.c"
#include "actors/vanilla_actors/castle_inside/pendulum/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_castle_inside[] = {
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_BOWSER_TRAP,        castle_geo_000F18),
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_WATER_LEVEL_PILLAR, castle_geo_001940),
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_CLOCK_MINUTE_HAND,  castle_geo_001530),
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_CLOCK_HOUR_HAND,    castle_geo_001548),
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_CLOCK_PENDULUM,     castle_geo_001518),
    // The different sets of star doors all use different model IDs, despite them all loading the same geo layout.
    // It is possible that star doors were originally going to have numbers on them, similar to the other locked doors.
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_STAR_DOOR_30_STARS, castle_geo_000F00),
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_STAR_DOOR_8_STARS,  castle_geo_000F00),
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_STAR_DOOR_50_STARS, castle_geo_000F00),
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_STAR_DOOR_70_STARS, castle_geo_000F00),
    RETURN(),
};
