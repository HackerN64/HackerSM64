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

#include "actors/vanilla_actors/hmc/header.h"

#include "actors/vanilla_actors/hmc/grill_door/geo.inc.c"
#include "actors/vanilla_actors/hmc/rolling_rock/geo.inc.c"
#include "actors/vanilla_actors/hmc/rolling_rock_fragment_1/geo.inc.c"
#include "actors/vanilla_actors/hmc/rolling_rock_fragment_2/geo.inc.c"
#include "actors/vanilla_actors/hmc/arrow_platform/geo.inc.c"
#include "actors/vanilla_actors/hmc/arrow_platform_button/geo.inc.c"
#include "actors/vanilla_actors/hmc/elevator_platform/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_hmc[] = {
    LOAD_MODEL_FROM_GEO(MODEL_HMC_METAL_PLATFORM,       hmc_geo_0005A0),
    LOAD_MODEL_FROM_GEO(MODEL_HMC_METAL_ARROW_PLATFORM, hmc_geo_0005B8),
    LOAD_MODEL_FROM_GEO(MODEL_HMC_ELEVATOR_PLATFORM,    hmc_geo_0005D0),
    LOAD_MODEL_FROM_GEO(MODEL_HMC_ROLLING_ROCK,         hmc_geo_000548),
    LOAD_MODEL_FROM_GEO(MODEL_HMC_ROCK_PIECE,           hmc_geo_000570),
    LOAD_MODEL_FROM_GEO(MODEL_HMC_ROCK_SMALL_PIECE,     hmc_geo_000588),
    LOAD_MODEL_FROM_GEO(MODEL_HMC_RED_GRILLS,           hmc_geo_000530),
    RETURN(),
};
