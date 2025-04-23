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

#include "actors/vanilla_actors/wf/header.h"

#include "actors/vanilla_actors/wf/small_bomp/geo.inc.c"
#include "actors/vanilla_actors/wf/large_bomp/geo.inc.c"
#include "actors/vanilla_actors/wf/rotating_wooden_platform/geo.inc.c"
#include "actors/vanilla_actors/wf/sliding_platform/geo.inc.c"
#include "actors/vanilla_actors/wf/tumbling_bridge_near/geo.inc.c"
#include "actors/vanilla_actors/wf/tumbling_bridge_far/geo.inc.c"
#include "actors/vanilla_actors/wf/beta_extending_platform/geo.inc.c"
#include "actors/vanilla_actors/wf/extending_platform/geo.inc.c"
#include "actors/vanilla_actors/wf/breakable_wall_right/geo.inc.c"
#include "actors/vanilla_actors/wf/breakable_wall_left/geo.inc.c"
#include "actors/vanilla_actors/wf/kickable_board/geo.inc.c"
#include "actors/vanilla_actors/wf/tower_door/geo.inc.c"
#include "actors/vanilla_actors/wf/giant_pole/geo.inc.c"
#include "actors/vanilla_actors/wf/rotating_platform/geo.inc.c"
#include "actors/vanilla_actors/wf/10/geo.inc.c"
#include "actors/vanilla_actors/wf/11/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_wf[] = {
    LOAD_MODEL_FROM_GEO(MODEL_WF_SMALL_BOMP,                    wf_geo_000A00),
    LOAD_MODEL_FROM_GEO(MODEL_WF_LARGE_BOMP,                    wf_geo_000A40),
    LOAD_MODEL_FROM_GEO(MODEL_WF_ROTATING_WOODEN_PLATFORM,      wf_geo_000A58),
    LOAD_MODEL_FROM_GEO(MODEL_WF_SLIDING_PLATFORM,              wf_geo_000A98),
    LOAD_MODEL_FROM_GEO(MODEL_WF_TUMBLING_BRIDGE_PART,          wf_geo_000AB0),
    LOAD_MODEL_FROM_GEO(MODEL_WF_TUMBLING_BRIDGE,               wf_geo_000AC8),
    LOAD_MODEL_FROM_GEO(MODEL_WF_TOWER_TRAPEZOID_PLATORM,       wf_geo_000AF8),
    LOAD_MODEL_FROM_GEO(MODEL_WF_TOWER_SQUARE_PLATORM,          wf_geo_000B10),
    LOAD_MODEL_FROM_GEO(MODEL_WF_TOWER_SQUARE_PLATORM_UNUSED,   wf_geo_000B38),
    LOAD_MODEL_FROM_GEO(MODEL_WF_TOWER_SQUARE_PLATORM_ELEVATOR, wf_geo_000B60),
    LOAD_MODEL_FROM_GEO(MODEL_WF_BREAKABLE_WALL_RIGHT,          wf_geo_000B78),
    LOAD_MODEL_FROM_GEO(MODEL_WF_BREAKABLE_WALL_LEFT,           wf_geo_000B90),
    LOAD_MODEL_FROM_GEO(MODEL_WF_KICKABLE_BOARD,                wf_geo_000BA8),
    LOAD_MODEL_FROM_GEO(MODEL_WF_TOWER_DOOR,                    wf_geo_000BE0),
    LOAD_MODEL_FROM_GEO(MODEL_WF_KICKABLE_BOARD_FELLED,         wf_geo_000BC8),
    LOAD_MODEL_FROM_GEO(MODEL_WF_GIANT_POLE,                    wf_geo_000AE0),
    LOAD_MODEL_FROM_GEO(MODEL_WF_ROTATING_PLATFORM,             wf_geo_0009B8),
    LOAD_MODEL_FROM_GEO(MODEL_LEVEL_GEOMETRY_08,                wf_geo_0008A8),
    LOAD_MODEL_FROM_GEO(MODEL_LEVEL_GEOMETRY_09,                wf_geo_0008E8),
    RETURN(),
};
