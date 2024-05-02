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

#include "actors/vanilla_actors/castle_grounds/header.h"

#include "actors/vanilla_actors/castle_grounds/11/geo.inc.c"
#include "actors/vanilla_actors/castle_grounds/7/geo.inc.c"
#include "actors/vanilla_actors/castle_grounds/8/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_castle_grounds[] = {
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_GROUNDS_VCUTM_GRILL,  castle_grounds_geo_00070C),
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_GROUNDS_FLAG,         castle_grounds_geo_000660),
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_GROUNDS_CANNON_GRILL, castle_grounds_geo_000724),
    RETURN(),
};
