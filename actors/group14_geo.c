#include <ultra64.h>
#include "sm64.h"
#include "geo_commands.h"

#include "make_const_nonconst.h"

#include "common1.h"
#include "group14.h"

#include "koopa_flag/geo.inc.c"
#include "poundable_pole/geo.inc.c"
#include "koopa/geo.inc.c"
#include "piranha_plant/geo.inc.c"
#include "whomp/geo.inc.c"
#include "chain_ball/geo.inc.c"
#include "chain_chomp/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_global_15[] = {
    LOAD_MODEL_FROM_GEO(MODEL_PIRANHA_PLANT,           piranha_plant_geo),
    LOAD_MODEL_FROM_GEO(MODEL_WHOMP,                   whomp_geo),
    LOAD_MODEL_FROM_GEO(MODEL_KOOPA_WITH_SHELL,        koopa_with_shell_geo),
    LOAD_MODEL_FROM_GEO(MODEL_KOOPA_WITHOUT_SHELL,     koopa_without_shell_geo),
    LOAD_MODEL_FROM_GEO(MODEL_METALLIC_BALL,           metallic_ball_geo),
    LOAD_MODEL_FROM_GEO(MODEL_CHAIN_CHOMP,             chain_chomp_geo),
    LOAD_MODEL_FROM_GEO(MODEL_KOOPA_FLAG,              koopa_flag_geo),
    LOAD_MODEL_FROM_GEO(MODEL_WOODEN_POST,             wooden_post_geo),
    RETURN(),
};
