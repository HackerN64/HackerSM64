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

#include "actors/vanilla_actors/vcutm/header.h"

#include "actors/vanilla_actors/vcutm/seesaw/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_vcutm[] = {
    LOAD_MODEL_FROM_GEO(MODEL_VCUTM_SEESAW_PLATFORM, vcutm_geo_0001F0),
    RETURN(),
};
