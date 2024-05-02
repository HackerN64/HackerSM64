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

#include "actors_vanilla/bbh/header.h"

#include "actors_vanilla/bbh/staircase_step/geo.inc.c"
#include "actors_vanilla/bbh/tilting_trap_platform/geo.inc.c"
#include "actors_vanilla/bbh/tumbling_platform_far/geo.inc.c"
#include "actors_vanilla/bbh/tumbling_platform_near/geo.inc.c"
#include "actors_vanilla/bbh/moving_bookshelf/geo.inc.c"
#include "actors_vanilla/bbh/mesh_elevator/geo.inc.c"
#include "actors_vanilla/bbh/merry_go_round/geo.inc.c"
#include "actors_vanilla/bbh/coffin/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_bbh[] = {
    LOAD_MODEL_FROM_GEO(MODEL_BBH_STAIRCASE_STEP,         geo_bbh_0005B0),
    LOAD_MODEL_FROM_GEO(MODEL_BBH_TILTING_FLOOR_PLATFORM, geo_bbh_0005C8),
    LOAD_MODEL_FROM_GEO(MODEL_BBH_TUMBLING_PLATFORM,      geo_bbh_0005E0),
    LOAD_MODEL_FROM_GEO(MODEL_BBH_TUMBLING_PLATFORM_PART, geo_bbh_0005F8),
    LOAD_MODEL_FROM_GEO(MODEL_BBH_MOVING_BOOKSHELF,       geo_bbh_000610),
    LOAD_MODEL_FROM_GEO(MODEL_BBH_MESH_ELEVATOR,          geo_bbh_000628),
    LOAD_MODEL_FROM_GEO(MODEL_BBH_MERRY_GO_ROUND,         geo_bbh_000640),
    LOAD_MODEL_FROM_GEO(MODEL_BBH_WOODEN_TOMB,            geo_bbh_000658),
    RETURN(),
};
