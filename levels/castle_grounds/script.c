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
#include "levels/castle_grounds/header.h"

static const LevelScript script_func_local_1[] = {
    WARP_NODE(/*id*/ 0x00, /*destLevel*/ LEVEL_CASTLE,         /*destArea*/ 0x01, /*destNode*/ 0x00, /*flags*/ WARP_NO_CHECKPOINT),
    WARP_NODE(/*id*/ 0x01, /*destLevel*/ LEVEL_CASTLE,         /*destArea*/ 0x01, /*destNode*/ 0x01, /*flags*/ WARP_NO_CHECKPOINT),
    WARP_NODE(/*id*/ 0x02, /*destLevel*/ LEVEL_CASTLE,         /*destArea*/ 0x03, /*destNode*/ 0x02, /*flags*/ WARP_NO_CHECKPOINT),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/     0,   900, -1710, /*angle*/ 0, 180, 0, /*behParam*/ BP(0x00, 0x03, 0x00, 0x00), /*beh*/ bhvDeathWarp),
    WARP_NODE(/*id*/ 0x03, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x03, /*flags*/ WARP_NO_CHECKPOINT),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -1328,   260,  4664, /*angle*/ 0, 180, 0, /*behParam*/ BP(0x00, 0x04, 0x00, 0x00), /*beh*/ bhvSpinAirborneCircleWarp),
    WARP_NODE(/*id*/ 0x04, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x04, /*flags*/ WARP_NO_CHECKPOINT),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -3379,  -815, -2025, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x3C, 0x05, 0x00, 0x00), /*beh*/ bhvWarp),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -3379,  -500, -2025, /*angle*/ 0, 180, 0, /*behParam*/ BP(0x00, 0x06, 0x00, 0x00), /*beh*/ bhvLaunchDeathWarp),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -3799, -1199, -5816, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x07, 0x00, 0x00), /*beh*/ bhvSwimmingWarp),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -3379,  -500, -2025, /*angle*/ 0, 180, 0, /*behParam*/ BP(0x00, 0x08, 0x00, 0x00), /*beh*/ bhvLaunchStarCollectWarp),
    WARP_NODE(/*id*/ 0x05, /*destLevel*/ LEVEL_VCUTM,          /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
    WARP_NODE(/*id*/ 0x06, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x06, /*flags*/ WARP_NO_CHECKPOINT),
    WARP_NODE(/*id*/ 0x07, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x07, /*flags*/ WARP_NO_CHECKPOINT),
    WARP_NODE(/*id*/ 0x08, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x08, /*flags*/ WARP_NO_CHECKPOINT),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/  5408,  4500,  3637, /*angle*/ 0, 225, 0, /*behParam*/ BP(0x00, 0x0A, 0x00, 0x00), /*beh*/ bhvAirborneWarp),
    WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -6901,  2376, -6509, /*angle*/ 0, 230, 0, /*behParam*/ BP(0x00, 0x14, 0x00, 0x00), /*beh*/ bhvAirborneWarp),
    WARP_NODE(/*id*/ 0x14, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x14, /*flags*/ WARP_NO_CHECKPOINT),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/  4997, -1250,  2258, /*angle*/ 0, 210, 0, /*behParam*/ BP(0x00, 0x1E, 0x00, 0x00), /*beh*/ bhvSwimmingWarp),
    WARP_NODE(/*id*/ 0x1E, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x1E, /*flags*/ WARP_NO_CHECKPOINT),
    RETURN(),
};

static const LevelScript script_func_local_2[] = {
    OBJECT(/*model*/ MODEL_NONE,                        /*pos*/ -5812,  100, -5937, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvWaterfallSoundLoop),
    OBJECT(/*model*/ MODEL_NONE,                        /*pos*/ -7430, 1500,   873, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvBirdsSoundLoop),
    OBJECT(/*model*/ MODEL_NONE,                        /*pos*/   -80, 1500,  5004, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x01, 0x00, 0x00), /*beh*/ bhvBirdsSoundLoop),
    OBJECT(/*model*/ MODEL_NONE,                        /*pos*/  7131, 1500, -2989, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x02, 0x00, 0x00), /*beh*/ bhvBirdsSoundLoop),
    OBJECT(/*model*/ MODEL_NONE,                        /*pos*/ -7430, 1500, -5937, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvAmbientSounds),
    OBJECT(/*model*/ MODEL_CASTLE_GROUNDS_VCUTM_GRILL,  /*pos*/     0,    0,     0, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvMoatGrills),
    OBJECT(/*model*/ MODEL_NONE,                        /*pos*/     0,    0,     0, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvInvisibleObjectsUnderBridge),
    OBJECT(/*model*/ MODEL_MIST,                        /*pos*/ -4878, -787, -5690, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvWaterMist2),
    OBJECT(/*model*/ MODEL_MIST,                        /*pos*/ -4996, -787, -5548, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x01, 0x00, 0x00), /*beh*/ bhvWaterMist2),
    OBJECT(/*model*/ MODEL_MIST,                        /*pos*/ -5114, -787, -5406, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x02, 0x00, 0x00), /*beh*/ bhvWaterMist2),
    OBJECT(/*model*/ MODEL_MIST,                        /*pos*/ -5212, -787, -5219, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x03, 0x00, 0x00), /*beh*/ bhvWaterMist2),
    OBJECT(/*model*/ MODEL_MIST,                        /*pos*/ -5311, -787, -5033, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x04, 0x00, 0x00), /*beh*/ bhvWaterMist2),
    OBJECT(/*model*/ MODEL_MIST,                        /*pos*/ -5419, -787, -4895, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x05, 0x00, 0x00), /*beh*/ bhvWaterMist2),
    OBJECT(/*model*/ MODEL_MIST,                        /*pos*/ -5527, -787, -4757, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x06, 0x00, 0x00), /*beh*/ bhvWaterMist2),
    OBJECT(/*model*/ MODEL_MIST,                        /*pos*/ -5686, -787, -4733, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x07, 0x00, 0x00), /*beh*/ bhvWaterMist2),
    OBJECT(/*model*/ MODEL_MIST,                        /*pos*/ -5845, -787, -4710, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x08, 0x00, 0x00), /*beh*/ bhvWaterMist2),
    OBJECT(/*model*/ MODEL_NONE,                        /*pos*/  5223, -975,  1667, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvManyBlueFishSpawner),
    OBJECT(/*model*/ MODEL_BIRDS,                       /*pos*/ -5069,  850,  3221, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x01, 0x00, 0x00), /*beh*/ bhvBird),
    OBJECT(/*model*/ MODEL_BIRDS,                       /*pos*/ -4711,  742,   433, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x01, 0x00, 0x00), /*beh*/ bhvBird),
    OBJECT(/*model*/ MODEL_BIRDS,                       /*pos*/  5774,  913, -1114, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x01, 0x00, 0x00), /*beh*/ bhvBird),
    OBJECT(/*model*/ MODEL_NONE,                        /*pos*/ -1328,  260,  4664, /*angle*/ 0, 180, 0, /*behParam*/ BP(0x00, 0x28, 0x00, 0x00), /*beh*/ bhvIntroScene),
    OBJECT(/*model*/ MODEL_CASTLE_GROUNDS_CANNON_GRILL, /*pos*/     0,    0,     0, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvHiddenAt120Stars),
    OBJECT(/*model*/ MODEL_LAKITU,                      /*pos*/    11,  803, -3015, /*angle*/ 0,   0, 0, /*behParam*/ BP(0x00, 0x01, 0x00, 0x00), /*beh*/ bhvCameraLakitu),
    RETURN(),
};

static const LevelScript script_func_local_3[] = {
    OBJECT(/*model*/ MODEL_CASTLE_GROUNDS_FLAG, /*pos*/ -3213, 3348, -3011, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvCastleFlagWaving),
    OBJECT(/*model*/ MODEL_CASTLE_GROUNDS_FLAG, /*pos*/  3213, 3348, -3011, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvCastleFlagWaving),
    OBJECT(/*model*/ MODEL_CASTLE_GROUNDS_FLAG, /*pos*/ -3835, 3348, -6647, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvCastleFlagWaving),
    OBJECT(/*model*/ MODEL_CASTLE_GROUNDS_FLAG, /*pos*/  3835, 3348, -6647, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvCastleFlagWaving),
    RETURN(),
};

static const LevelScript script_func_local_4[] = {
    OBJECT(/*model*/ MODEL_BUTTERFLY, /*pos*/ -4508,  406,  4400, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvButterfly),
    OBJECT(/*model*/ MODEL_BUTTERFLY, /*pos*/ -4408,  406,  4500, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvButterfly),
    OBJECT(/*model*/ MODEL_BUTTERFLY, /*pos*/ -4708,  406,  4100, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvButterfly),
    OBJECT(/*model*/ MODEL_BUTTERFLY, /*pos*/ -6003,  473, -2621, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvButterfly),
    OBJECT(/*model*/ MODEL_BUTTERFLY, /*pos*/ -6003,  473, -2321, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvButterfly),
    OBJECT(/*model*/ MODEL_BUTTERFLY, /*pos*/  6543,  461,  -617, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvButterfly),
    OBJECT(/*model*/ MODEL_BUTTERFLY, /*pos*/  6143,  461,  -617, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvButterfly),
    OBJECT(/*model*/ MODEL_BUTTERFLY, /*pos*/  5773,  775, -5722, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvButterfly),
    OBJECT(/*model*/ MODEL_BUTTERFLY, /*pos*/  5873,  775, -5622, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvButterfly),
    OBJECT(/*model*/ MODEL_BUTTERFLY, /*pos*/  5473,  775, -5322, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvButterfly),
    OBJECT(/*model*/ MODEL_BUTTERFLY, /*pos*/ -1504,  326,  3196, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvButterfly),
    OBJECT(/*model*/ MODEL_BUTTERFLY, /*pos*/ -1204,  326,  3296, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvButterfly),
    OBJECT(/*model*/ MODEL_YOSHI,     /*pos*/     0, 3174, -5625, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvYoshi),
    RETURN(),
};

const LevelScript level_castle_grounds_entry[] = {
    INIT_LEVEL(),
    LOAD_LEVEL_DATA(castle_grounds),
    LOAD_SKYBOX(water),
    LOAD_TEXTURE_BIN(outside),
    LOAD_GROUPA(group10),
    LOAD_GROUPB(group15),
    LOAD_COMMON0(),
    ALLOC_LEVEL_POOL(),
    MARIO(/*model*/ MODEL_MARIO, /*behParam*/ BP(0x00, 0x00, 0x00, 0x01), /*beh*/ bhvMario),
    LOAD_ACTOR_MODELS(common0),
    LOAD_ACTOR_MODELS(group10),
    LOAD_ACTOR_MODELS(group15),
    LOAD_MODEL_FROM_GEO(MODEL_LEVEL_GEOMETRY_03,           castle_grounds_geo_0006F4),
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_GROUNDS_BUBBLY_TREE,  bubbly_tree_geo),
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_GROUNDS_WARP_PIPE,    warp_pipe_geo),
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_GROUNDS_CASTLE_DOOR,  castle_door_geo),
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_GROUNDS_METAL_DOOR,   metal_door_geo),
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_GROUNDS_VCUTM_GRILL,  castle_grounds_geo_00070C),
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_GROUNDS_FLAG,         castle_grounds_geo_000660),
    LOAD_MODEL_FROM_GEO(MODEL_CASTLE_GROUNDS_CANNON_GRILL, castle_grounds_geo_000724),

    AREA(/*index*/ 1, castle_grounds_geo_00073C),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x03, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_func_local_1),
        JUMP_LINK(script_func_local_2),
        JUMP_LINK(script_func_local_3),
        JUMP_LINK(script_func_local_4),
        TERRAIN(/*terrainData*/ castle_grounds_seg7_collision_level),
        MACRO_OBJECTS(/*objList*/ castle_grounds_seg7_macro_objs),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0000, /*seq*/ SEQ_SOUND_PLAYER),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_GRASS),
    END_AREA(),

    FREE_LEVEL_POOL(),
    MARIO_POS(/*area*/ 1, /*yaw*/ 180, /*pos*/ -1328, 260, 4664),
    CALL(     /*arg*/ 0, /*func*/ lvl_init_or_update),
    CALL_LOOP(/*arg*/ 1, /*func*/ lvl_init_or_update),
    CLEAR_LEVEL(),
    SLEEP_BEFORE_EXIT(/*frames*/ 1),
    EXIT(),
};
