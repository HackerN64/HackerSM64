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

#include "actors/vanilla_actors/ccm/header.h"

#include "actors/vanilla_actors/ccm/ropeway_lift/geo.inc.c"
#include "actors/vanilla_actors/ccm/snowman_base/geo.inc.c"
#include "actors/vanilla_actors/ccm/snowman_head/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_ccm[] = {
    LOAD_MODEL_FROM_GEO(MODEL_CCM_ROPEWAY_LIFT,  ccm_geo_0003D0),
    LOAD_MODEL_FROM_GEO(MODEL_CCM_SNOWMAN_BASE,  ccm_geo_0003F0),
    LOAD_MODEL_FROM_GEO(MODEL_CCM_SNOWMAN_HEAD,  ccm_geo_00040C),
    RETURN(),
};
