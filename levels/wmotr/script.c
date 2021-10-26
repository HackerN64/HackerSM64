#include <ultra64.h>
#include "sm64.h"
#include "behavior_data.h"
#include "model_ids.h"
#include "seq_ids.h"
#include "segment_symbols.h"
#include "level_commands.h"

#include "game/level_update.h"

#include "levels/scripts.h"

#include "actors/common1.h"

#include "make_const_nonconst.h"
#include "levels/wmotr/header.h"

static const LevelScript script_func_local_1[] = {
    OBJECT(/*model*/ MODEL_NONE, /*pos*/  3996, -2739,  5477, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x52, 0x00, 0x00), /*beh*/ bhvPoleGrabbing),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -2911,  3564, -3967, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x54, 0x00, 0x00), /*beh*/ bhvPoleGrabbing),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -3258,  3359, -3946, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x69, 0x00, 0x00), /*beh*/ bhvPoleGrabbing),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -2639,  3154, -4369, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x7D, 0x00, 0x00), /*beh*/ bhvPoleGrabbing),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -2980,  4048, -4248, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x24, 0x00, 0x00), /*beh*/ bhvPoleGrabbing),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -3290,  3636, -4477, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x4D, 0x00, 0x00), /*beh*/ bhvPoleGrabbing),
    RETURN(),
};

static const LevelScript script_func_local_2[] = {
    OBJECT(/*model*/ MODEL_NONE, /*pos*/  -160,  1950,  -470, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvHiddenRedCoinStar),
    RETURN(),
};

const LevelScript level_wmotr_entry[] = {
    INIT_LEVEL(),
    LOAD_LEVEL_DATA(wmotr),
    LOAD_SKYBOX(cloud_floor),
    LOAD_TEXTURE_BIN(sky),
    LOAD_GROUPA(group2),
    LOAD_GROUPB(group17),
    LOAD_COMMON0(),
    ALLOC_LEVEL_POOL(),
    MARIO(/*model*/ MODEL_MARIO, /*behParam*/ BP(0x00, 0x00, 0x00, 0x01), /*beh*/ bhvMario),
    LOAD_ACTOR_MODELS(common0),
    LOAD_ACTOR_MODELS(group2),
    LOAD_ACTOR_MODELS(group17),

    AREA(/*index*/ 1, wmotr_geo_0001F0),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ -67, 2669, -16, /*angle*/ 0, 270, 0, /*behParam*/ BP(0x00, 0x0A, 0x00, 0x00), /*beh*/ bhvAirborneWarp),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_WMOTR,          /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF0, /*destLevel*/ LEVEL_CASTLE,         /*destArea*/ 0x02, /*destNode*/ 0x38, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE,         /*destArea*/ 0x02, /*destNode*/ 0x6D, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF3, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_func_local_1),
        JUMP_LINK(script_func_local_2),
        TERRAIN(/*terrainData*/ wmotr_seg7_collision),
        MACRO_OBJECTS(/*objList*/ wmotr_seg7_macro_objs),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0000, /*seq*/ SEQ_LEVEL_SLIDE),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_SNOW),
    END_AREA(),

    FREE_LEVEL_POOL(),
    MARIO_POS(/*area*/ 1, /*yaw*/ 270, /*pos*/ -67, 1669, -16),
    CALL(     /*arg*/ 0, /*func*/ lvl_init_or_update),
    CALL_LOOP(/*arg*/ 1, /*func*/ lvl_init_or_update),
    CLEAR_LEVEL(),
    SLEEP_BEFORE_EXIT(/*frames*/ 1),
    EXIT(),
};
