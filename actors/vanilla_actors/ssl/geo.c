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

#include "actors/vanilla_actors/ssl/header.h"

#include "actors/vanilla_actors/ssl/pyramid_top/geo.inc.c"
#include "actors/vanilla_actors/ssl/tox_box/geo.inc.c"
#include "actors/vanilla_actors/ssl/grindel/geo.inc.c"
#include "actors/vanilla_actors/ssl/spindel/geo.inc.c"
#include "actors/vanilla_actors/ssl/moving_pyramid_wall/geo.inc.c"
#include "actors/vanilla_actors/ssl/pyramid_elevator/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_ssl[] = {
    LOAD_MODEL_FROM_GEO(MODEL_SSL_PYRAMID_TOP,         ssl_geo_000618),
    LOAD_MODEL_FROM_GEO(MODEL_SSL_GRINDEL,             ssl_geo_000734),
    LOAD_MODEL_FROM_GEO(MODEL_SSL_SPINDEL,             ssl_geo_000764),
    LOAD_MODEL_FROM_GEO(MODEL_SSL_MOVING_PYRAMID_WALL, ssl_geo_000794),
    LOAD_MODEL_FROM_GEO(MODEL_SSL_PYRAMID_ELEVATOR,    ssl_geo_0007AC),
    LOAD_MODEL_FROM_GEO(MODEL_SSL_TOX_BOX,             ssl_geo_000630),
    RETURN(),
};
