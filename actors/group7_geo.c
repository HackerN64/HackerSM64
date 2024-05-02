#include <ultra64.h>
#include "sm64.h"
#include "geo_commands.h"

#include "make_const_nonconst.h"

#include "common1.h"
#include "group7.h"

#include "spindrift/geo.inc.c"
#include "penguin/geo.inc.c"
#include "snowman/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_global_8[] = {
    LOAD_MODEL_FROM_GEO(MODEL_SPINDRIFT,               spindrift_geo),
    LOAD_MODEL_FROM_GEO(MODEL_MR_BLIZZARD_HIDDEN,      mr_blizzard_hidden_geo),
    LOAD_MODEL_FROM_GEO(MODEL_MR_BLIZZARD,             mr_blizzard_geo),
    LOAD_MODEL_FROM_GEO(MODEL_PENGUIN,                 penguin_geo),
    RETURN(),
};
