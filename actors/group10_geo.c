#include <ultra64.h>
#include "sm64.h"
#include "geo_commands.h"

#include "make_const_nonconst.h"

#include "common1.h"
#include "group10.h"

#include "bird/geo.inc.c"
#include "peach/geo.inc.c"
#include "yoshi/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_global_11[] = {
    LOAD_MODEL_FROM_GEO(MODEL_BIRDS,                   birds_geo),
    LOAD_MODEL_FROM_GEO(MODEL_PEACH,                   peach_geo),
    LOAD_MODEL_FROM_GEO(MODEL_YOSHI,                   yoshi_geo),
    RETURN(),
};
