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

#include "actors/vanilla_actors/bob/header.h"

#include "actors/vanilla_actors/bob/chain_chomp_gate/geo.inc.c"
#include "actors/vanilla_actors/bob/seesaw_platform/geo.inc.c"
#include "actors/vanilla_actors/bob/grate_door/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_bob[] = {
    LOAD_MODEL_FROM_GEO(MODEL_BOB_CHAIN_CHOMP_GATE, bob_geo_000440),
    LOAD_MODEL_FROM_GEO(MODEL_BOB_SEESAW_PLATFORM,  bob_geo_000458),
    LOAD_MODEL_FROM_GEO(MODEL_BOB_BARS_GRILLS,      bob_geo_000470),
    RETURN(),
};
