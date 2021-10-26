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
#include "levels/sl/header.h"


static const LevelScript script_func_local_1[] = {
    OBJECT_WITH_ACTS(/*model*/ MODEL_STAR, /*pos*/  700, 4500,  690, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvStar,              /*acts*/ ALL_ACTS),
    OBJECT_WITH_ACTS(/*model*/ MODEL_STAR, /*pos*/ 4350, 1350, 4350, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x02, 0x00, 0x00, 0x00), /*beh*/ bhvStar,              /*acts*/ ALL_ACTS),
    OBJECT_WITH_ACTS(/*model*/ MODEL_NONE, /*pos*/ 5000, 1200,    0, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x04, 0x00, 0x00, 0x00), /*beh*/ bhvHiddenRedCoinStar, /*acts*/ ALL_ACTS),
    RETURN(),
};

static const LevelScript script_func_local_2[] = {
    OBJECT(/*model*/ MODEL_NONE, /*pos*/  977, 1024, 2075, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvSnowMoundSpawn),
    RETURN(),
};

static const LevelScript script_func_local_3[] = {
    OBJECT(/*model*/ MODEL_PENGUIN,            /*pos*/ 1715, 3328,   518, /*angle*/ 0, -51, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvSLWalkingPenguin),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/  700, 3428,   700, /*angle*/ 0,  30, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvSLSnowmanWind),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/  480, 2300,  1370, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvIgloo),
    OBJECT(/*model*/ MODEL_BIG_CHILL_BULLY,    /*pos*/  315, 1331, -4852, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x01, 0x00, 0x00, 0x00), /*beh*/ bhvBigChillBully),
    OBJECT(/*model*/ MODEL_MR_BLIZZARD_HIDDEN, /*pos*/ 2954,  970,   750, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x02, 0x00, 0x00), /*beh*/ bhvMrBlizzard),
    RETURN(),
};

static const LevelScript script_func_local_4[] = {
    OBJECT_WITH_ACTS(/*model*/ MODEL_STAR, /*pos*/ 0, 500, 1000, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x05, 0x00, 0x00, 0x00), /*beh*/ bhvStar, /*acts*/ ALL_ACTS),
    RETURN(),
};

const LevelScript level_sl_entry[] = {
    INIT_LEVEL(),
    LOAD_LEVEL_DATA(sl),
    LOAD_TEXTURE_BIN(snow),
    LOAD_EFFECTS(),
    LOAD_SKYBOX(ccm),
    LOAD_GROUPA(group7),
    LOAD_GROUPB(group16),
    LOAD_COMMON0(),
    ALLOC_LEVEL_POOL(),
    MARIO(/*model*/ MODEL_MARIO, /*behParam*/ BP(0x00, 0x00, 0x00, 0x01), /*beh*/ bhvMario),
    LOAD_ACTOR_MODELS(common0),
    LOAD_ACTOR_MODELS(group7),
    LOAD_ACTOR_MODELS(group16),
    LOAD_MODEL_FROM_GEO(MODEL_SL_SNOW_TRIANGLE,      sl_geo_000390),
    LOAD_MODEL_FROM_GEO(MODEL_SL_CRACKED_ICE,        sl_geo_000360),
    LOAD_MODEL_FROM_GEO(MODEL_SL_CRACKED_ICE_CHUNK,  sl_geo_000378),
    LOAD_MODEL_FROM_GEO(MODEL_SL_SNOW_TREE,          snow_tree_geo),

    AREA(/*index*/ 1, sl_geo_0003A8),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/  5541, 2024,   443, /*angle*/ 0, 270, 0, /*behParam*/ BP(0x00, 0x0A, 0x00, 0x00), /*beh*/ bhvSpinAirborneWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/   257, 2150,  1399, /*angle*/ 0, 290, 0, /*behParam*/ BP(0x00, 0x0B, 0x00, 0x00), /*beh*/ bhvInstantActiveWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/   569, 2150,  1336, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x06, 0x0C, 0x00, 0x00), /*beh*/ bhvWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/  5468, 1056, -5400, /*angle*/ 0, -20, 0, /*behParam*/ BP(0x00, 0x0D, 0x00, 0x00), /*beh*/ bhvFadingWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ -3698, 1024, -1237, /*angle*/ 0,   6, 0, /*behParam*/ BP(0x00, 0x0E, 0x00, 0x00), /*beh*/ bhvFadingWarp),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_SL, /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0B, /*destLevel*/ LEVEL_SL, /*destArea*/ 0x01, /*destNode*/ 0x0B, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0C, /*destLevel*/ LEVEL_SL, /*destArea*/ 0x02, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0D, /*destLevel*/ LEVEL_SL, /*destArea*/ 0x01, /*destNode*/ 0x0E, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0E, /*destLevel*/ LEVEL_SL, /*destArea*/ 0x01, /*destNode*/ 0x0D, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_func_local_1),
        JUMP_LINK(script_func_local_2),
        JUMP_LINK(script_func_local_3),
        WARP_NODE(/*id*/ 0xF0, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x02, /*destNode*/ 0x36, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x02, /*destNode*/ 0x68, /*flags*/ WARP_NO_CHECKPOINT),
        TERRAIN(/*terrainData*/ sl_seg7_area_1_collision),
        MACRO_OBJECTS(/*objList*/ sl_seg7_area_1_macro_objs),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0000, /*seq*/ SEQ_LEVEL_SNOW),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_SNOW),
    END_AREA(),

    AREA(/*index*/ 2, sl_geo_000484),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ 0, 0, 2867, /*angle*/ 0, 180, 0, /*behParam*/ BP(0x00, 0x0A, 0x00, 0x00), /*beh*/ bhvInstantActiveWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ 0, 0, 3277, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x14, 0x0B, 0x00, 0x00), /*beh*/ bhvWarp),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_SL, /*destArea*/ 0x02, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0B, /*destLevel*/ LEVEL_SL, /*destArea*/ 0x01, /*destNode*/ 0x0B, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_func_local_4),
        WARP_NODE(/*id*/ 0xF0, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x02, /*destNode*/ 0x36, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x02, /*destNode*/ 0x68, /*flags*/ WARP_NO_CHECKPOINT),
        TERRAIN(/*terrainData*/ sl_seg7_area_2_collision),
        MACRO_OBJECTS(/*objList*/ sl_seg7_area_2_macro_objs),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0004, /*seq*/ SEQ_LEVEL_UNDERGROUND),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_SNOW),
    END_AREA(),

    FREE_LEVEL_POOL(),
    MARIO_POS(/*area*/ 1, /*yaw*/ 270, /*pos*/ 5541, 1024, 443),
    CALL(     /*arg*/ 0, /*func*/ lvl_init_or_update),
    CALL_LOOP(/*arg*/ 1, /*func*/ lvl_init_or_update),
    CLEAR_LEVEL(),
    SLEEP_BEFORE_EXIT(/*frames*/ 1),
    EXIT(),
};
