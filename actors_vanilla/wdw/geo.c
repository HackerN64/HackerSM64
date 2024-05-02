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

#include "actors_vanilla/wdw/header.h"

#include "actors_vanilla/wdw/square_floating_platform/geo.inc.c"
#include "actors_vanilla/wdw/arrow_lift/geo.inc.c"
#include "actors_vanilla/wdw/water_level_diamond/geo.inc.c"
#include "actors_vanilla/wdw/hidden_platform/geo.inc.c"
#include "actors_vanilla/wdw/express_elevator/geo.inc.c"
#include "actors_vanilla/wdw/rectangular_floating_platform/geo.inc.c"
#include "actors_vanilla/wdw/rotating_platform/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_wdw[] = {
    LOAD_MODEL_FROM_GEO(MODEL_WDW_SQUARE_FLOATING_PLATFORM,      wdw_geo_000580),
    LOAD_MODEL_FROM_GEO(MODEL_WDW_ARROW_LIFT,                    wdw_geo_000598),
    LOAD_MODEL_FROM_GEO(MODEL_WDW_WATER_LEVEL_DIAMOND,           wdw_geo_0005C0),
    LOAD_MODEL_FROM_GEO(MODEL_WDW_HIDDEN_PLATFORM,               wdw_geo_0005E8),
    LOAD_MODEL_FROM_GEO(MODEL_WDW_EXPRESS_ELEVATOR,              wdw_geo_000610),
    LOAD_MODEL_FROM_GEO(MODEL_WDW_RECTANGULAR_FLOATING_PLATFORM, wdw_geo_000628),
    LOAD_MODEL_FROM_GEO(MODEL_WDW_ROTATING_PLATFORM,             wdw_geo_000640),
    RETURN(),
};
