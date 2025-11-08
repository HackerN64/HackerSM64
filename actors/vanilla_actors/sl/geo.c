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

#include "actors/vanilla_actors/sl/header.h"

#include "actors/vanilla_actors/sl/unused_cracked_ice/geo.inc.c"
#include "actors/vanilla_actors/sl/unused_ice_shard/geo.inc.c"
#include "actors/vanilla_actors/sl/snow_mound/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_sl[] = {
    LOAD_MODEL_FROM_GEO(MODEL_SL_SNOW_TRIANGLE,      sl_geo_000390),
    LOAD_MODEL_FROM_GEO(MODEL_SL_CRACKED_ICE,        sl_geo_000360),
    LOAD_MODEL_FROM_GEO(MODEL_SL_CRACKED_ICE_CHUNK,  sl_geo_000378),
    RETURN(),
};
