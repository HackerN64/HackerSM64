#include <ultra64.h>
#include "sm64.h"
#include "geo_commands.h"

#include "make_const_nonconst.h"

#include "common1.h"
#include "group16.h"

#include "chillychief/geo.inc.c"
#include "moneybag/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_global_17[] = {
    LOAD_MODEL_FROM_GEO(MODEL_CHILL_BULLY,             chilly_chief_geo),
    LOAD_MODEL_FROM_GEO(MODEL_BIG_CHILL_BULLY,         chilly_chief_big_geo),
    LOAD_MODEL_FROM_GEO(MODEL_MONEYBAG,                moneybag_geo),
    RETURN(),
};
