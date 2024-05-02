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

#include "actors/vanilla_actors/rr/header.h"

#include "actors/vanilla_actors/rr/flying_carpet/geo.inc.c"
#include "actors/vanilla_actors/rr/swinging_platform/geo.inc.c"
#include "actors/vanilla_actors/rr/rotating_bridge_platform/geo.inc.c"
#include "actors/vanilla_actors/rr/cruiser_wing/geo.inc.c"
#include "actors/vanilla_actors/rr/octagonal_platform/geo.inc.c"
#include "actors/vanilla_actors/rr/sliding_platform/geo.inc.c"
#include "actors/vanilla_actors/rr/pyramid_platform/geo.inc.c" // unused
#include "actors/vanilla_actors/rr/elevator_platform/geo.inc.c"
#include "actors/vanilla_actors/rr/seesaw_platform/geo.inc.c"
#include "actors/vanilla_actors/rr/donut_block/geo.inc.c"
#include "actors/vanilla_actors/rr/l_platform/geo.inc.c" // also unused
#include "actors/vanilla_actors/rr/tricky_triangles_1/geo.inc.c"
#include "actors/vanilla_actors/rr/tricky_triangles_2/geo.inc.c"
#include "actors/vanilla_actors/rr/tricky_triangles_3/geo.inc.c"
#include "actors/vanilla_actors/rr/tricky_triangles_4/geo.inc.c"
#include "actors/vanilla_actors/rr/tricky_triangles_5/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_rr[] = {
    LOAD_MODEL_FROM_GEO(MODEL_RR_SLIDING_PLATFORM,         rr_geo_0008C0),
    LOAD_MODEL_FROM_GEO(MODEL_RR_FLYING_CARPET,            rr_geo_000848),
    LOAD_MODEL_FROM_GEO(MODEL_RR_OCTAGONAL_PLATFORM,       rr_geo_0008A8),
    LOAD_MODEL_FROM_GEO(MODEL_RR_ROTATING_BRIDGE_PLATFORM, rr_geo_000878),
    LOAD_MODEL_FROM_GEO(MODEL_RR_TRIANGLE_PLATFORM,        rr_geo_0008D8),
    LOAD_MODEL_FROM_GEO(MODEL_RR_CRUISER_WING,             rr_geo_000890),
    LOAD_MODEL_FROM_GEO(MODEL_RR_SEESAW_PLATFORM,          rr_geo_000908),
    LOAD_MODEL_FROM_GEO(MODEL_RR_L_SHAPED_PLATFORM,        rr_geo_000940),
    LOAD_MODEL_FROM_GEO(MODEL_RR_SWINGING_PLATFORM,        rr_geo_000860),
    LOAD_MODEL_FROM_GEO(MODEL_RR_DONUT_PLATFORM,           rr_geo_000920),
    LOAD_MODEL_FROM_GEO(MODEL_RR_ELEVATOR_PLATFORM,        rr_geo_0008F0),
    LOAD_MODEL_FROM_GEO(MODEL_RR_TRICKY_TRIANGLES,         rr_geo_000958),
    LOAD_MODEL_FROM_GEO(MODEL_RR_TRICKY_TRIANGLES_FRAME1,  rr_geo_000970),
    LOAD_MODEL_FROM_GEO(MODEL_RR_TRICKY_TRIANGLES_FRAME2,  rr_geo_000988),
    LOAD_MODEL_FROM_GEO(MODEL_RR_TRICKY_TRIANGLES_FRAME3,  rr_geo_0009A0),
    LOAD_MODEL_FROM_GEO(MODEL_RR_TRICKY_TRIANGLES_FRAME4,  rr_geo_0009B8),
    RETURN(),
};
