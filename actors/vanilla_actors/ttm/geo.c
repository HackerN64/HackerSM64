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

#include "actors/vanilla_actors/ttm/header.h"

#include "actors/vanilla_actors/ttm/star_cage/geo.inc.c"
#include "actors/vanilla_actors/ttm/rolling_log/geo.inc.c"
#include "actors/vanilla_actors/ttm/blue_smiley/geo.inc.c"
#include "actors/vanilla_actors/ttm/yellow_smiley/geo.inc.c"
#include "actors/vanilla_actors/ttm/star_smiley/geo.inc.c"
#include "actors/vanilla_actors/ttm/moon_smiley/geo.inc.c"
#include "actors/vanilla_actors/ttm/slide_exit_podium/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_ttm[] = {
    LOAD_MODEL_FROM_GEO(MODEL_TTM_SLIDE_EXIT_PODIUM, ttm_geo_000DF4),
    LOAD_MODEL_FROM_GEO(MODEL_TTM_ROLLING_LOG,       ttm_geo_000730),
    LOAD_MODEL_FROM_GEO(MODEL_TTM_STAR_CAGE,        ttm_geo_000710),
    LOAD_MODEL_FROM_GEO(MODEL_TTM_BLUE_SMILEY,       ttm_geo_000D14),
    LOAD_MODEL_FROM_GEO(MODEL_TTM_YELLOW_SMILEY,     ttm_geo_000D4C),
    LOAD_MODEL_FROM_GEO(MODEL_TTM_STAR_SMILEY,       ttm_geo_000D84),
    LOAD_MODEL_FROM_GEO(MODEL_TTM_MOON_SMILEY,       ttm_geo_000DBC),
    RETURN(),
};
