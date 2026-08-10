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

#include "actors/vanilla_actors/ddd/header.h"

#include "actors/vanilla_actors/ddd/pole/geo.inc.c"
#include "actors/vanilla_actors/ddd/sub_door/geo.inc.c"
#include "actors/vanilla_actors/ddd/submarine/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_ddd[] = {
    LOAD_MODEL_FROM_GEO(MODEL_DDD_BOWSER_SUB_DOOR, ddd_geo_000478),
    LOAD_MODEL_FROM_GEO(MODEL_DDD_BOWSER_SUB,      ddd_geo_0004A0),
    LOAD_MODEL_FROM_GEO(MODEL_DDD_POLE,            ddd_geo_000450),
    RETURN(),
};
