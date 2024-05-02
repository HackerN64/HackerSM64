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

#include "actors/vanilla_actors/bitfs/header.h"

#include "actors/vanilla_actors/bitfs/elevator/geo.inc.c"
#include "actors/vanilla_actors/bitfs/sinking_cage_platform/geo.inc.c"
#include "actors/vanilla_actors/bitfs/sinking_cage_pole/geo.inc.c"
#include "actors/vanilla_actors/bitfs/tilting_square_platform/geo.inc.c"
#include "actors/vanilla_actors/bitfs/tumbling_platform_near/geo.inc.c"
#include "actors/vanilla_actors/bitfs/tumbling_platform_far/geo.inc.c"
#include "actors/vanilla_actors/bitfs/stretching_platform/geo.inc.c"
#include "actors/vanilla_actors/bitfs/moving_square_platform/geo.inc.c"
#include "actors/vanilla_actors/bitfs/sliding_platform/geo.inc.c"
#include "actors/vanilla_actors/bitfs/platform_on_track/geo.inc.c"
#include "actors/vanilla_actors/bitfs/sinking_platforms/geo.inc.c"
#include "actors/vanilla_actors/bitfs/seesaw_platform/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_vo_bitfs[] = {
    LOAD_MODEL_FROM_GEO(MODEL_BITFS_PLATFORM_ON_TRACK,       bitfs_geo_000758),
    LOAD_MODEL_FROM_GEO(MODEL_BITFS_TILTING_SQUARE_PLATFORM, bitfs_geo_0006C0),
    LOAD_MODEL_FROM_GEO(MODEL_BITFS_SINKING_PLATFORMS,       bitfs_geo_000770),
    LOAD_MODEL_FROM_GEO(MODEL_BITFS_BLUE_POLE,               bitfs_geo_0006A8),
    LOAD_MODEL_FROM_GEO(MODEL_BITFS_SINKING_CAGE_PLATFORM,   bitfs_geo_000690),
    LOAD_MODEL_FROM_GEO(MODEL_BITFS_ELEVATOR,                bitfs_geo_000678),
    LOAD_MODEL_FROM_GEO(MODEL_BITFS_STRETCHING_PLATFORMS,    bitfs_geo_000708),
    LOAD_MODEL_FROM_GEO(MODEL_BITFS_SEESAW_PLATFORM,         bitfs_geo_000788),
    LOAD_MODEL_FROM_GEO(MODEL_BITFS_MOVING_SQUARE_PLATFORM,  bitfs_geo_000728),
    LOAD_MODEL_FROM_GEO(MODEL_BITFS_SLIDING_PLATFORM,        bitfs_geo_000740),
    LOAD_MODEL_FROM_GEO(MODEL_BITFS_TUMBLING_PLATFORM_PART,  bitfs_geo_0006D8),
    LOAD_MODEL_FROM_GEO(MODEL_BITFS_TUMBLING_PLATFORM,       bitfs_geo_0006F0),
    RETURN(),
};
