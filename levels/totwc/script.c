#include <ultra64.h>
#include "sm64.h"
#include "behavior_data.h"
#include "model_ids.h"
#include "seq_ids.h"
#include "dialog_ids.h"
#include "segment_symbols.h"
#include "level_commands.h"

#include "game/level_update.h"

#include "levels/scripts.h"

#include "actors/common1.h"

#include "make_const_nonconst.h"
#include "levels/totwc/header.h"


static const LevelScript script_totwc_macro_objects[] = {
    // Macro objects
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX,   /*pos*/     0, -1760,  -600, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_WING_CAP << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/     0, -1000,  3000, /*angle*/ 0,  90, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING | COIN_FORMATION_FLAG_VERTICAL) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/ -3500,  -200,     0, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING | COIN_FORMATION_FLAG_VERTICAL) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/  4500,  1400,     0, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING | COIN_FORMATION_FLAG_VERTICAL) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/     0,   600, -4000, /*angle*/ 0,  90, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING | COIN_FORMATION_FLAG_VERTICAL) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_RED_COIN,          /*pos*/  -100,   800, -4000, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,          /*pos*/ -3500,     0,   100, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,          /*pos*/   100,  -800,  3000, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,          /*pos*/  4500,  1600,  -100, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,          /*pos*/   100,   800, -4000, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,          /*pos*/ -3500,     0,  -100, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,          /*pos*/  -100,  -800,  3000, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,          /*pos*/  4500,  1600,   100, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/  2913,  1200, -2941, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/  3954,  1400, -1817, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/  1554,  1000, -3680, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/ -2657,   400, -2742, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/ -1485,   600, -3628, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/ -3231,   200, -1514, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/ -2197,  -400,  2142, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/ -3056,  -200,  1310, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/ -1144,  -600,  2742, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/  3680,  1800,  1439, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/  2080,  2000,  2080, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/   920,  2200,  1920, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/   -40,  2400,  1520, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/  -960,  2600,  1040, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/ -1760,  2800,   600, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    // Special objects
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/  4608,  -357,  -511, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/ -4095,   666,   512, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/ -5631, -1893,  6144, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/ -2047,  -869,  2048, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/  1024,  1178,  1536, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/ -2047,  -357, -2559, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/  2560, -2917,  2560, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/  1024,  1178,  4096, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/ -1023,  -869,  5632, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/ -5119, -2405,  1024, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/   512, -2917,  3584, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/  1024, -1893, -3071, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/  -511,  -869, -4095, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/  4608, -3429,  1536, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/  6144, -3429, -5119, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/ -2559, -2405, -6655, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/  3072,  -869,  5632, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/  6144, -1381,  -511, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/  4096,   666, -1535, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/ -4607, -1381, -2559, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/ -1535, -2917, -6143, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/  2560, -3429, -3583, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    RETURN(),
};

static const LevelScript script_totwc_objects_1[] = {
    OBJECT(/*model*/ MODEL_CAP_SWITCH, /*pos*/   0, -2047, 10, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvCapSwitch),
    RETURN(),
};

static const LevelScript script_totwc_objects_2[] = {
    OBJECT(/*model*/ MODEL_NONE,       /*pos*/ 800, -1700,  0, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenRedCoinStar),
    RETURN(),
};

const LevelScript level_totwc_entry[] = {
    INIT_LEVEL(),
    LOAD_YAY0(        /*seg*/ 0x07, _totwc_segment_7SegmentRomStart, _totwc_segment_7SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x0A, _cloud_floor_skybox_yay0SegmentRomStart, _cloud_floor_skybox_yay0SegmentRomEnd),
    LOAD_YAY0_TEXTURE(/*seg*/ 0x09, _sky_yay0SegmentRomStart, _sky_yay0SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x05, _group8_yay0SegmentRomStart, _group8_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0C, _group8_geoSegmentRomStart,  _group8_geoSegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x08, _common0_yay0SegmentRomStart, _common0_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0F, _common0_geoSegmentRomStart,  _common0_geoSegmentRomEnd),
    ALLOC_LEVEL_POOL(),
    MARIO(/*model*/ MODEL_MARIO, /*behParam*/ 0x00000001, /*beh*/ bhvMario),
    JUMP_LINK(script_func_global_1),
    JUMP_LINK(script_func_global_9),
    LOAD_MODEL_FROM_GEO(MODEL_LEVEL_GEOMETRY_03, totwc_geo_000160),

    AREA(/*index*/ 1, totwc_geo_000188),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ -4095, 2935, 0, /*angle*/ 0, 90, 0, /*behParam*/ 0x000A0000, /*beh*/ bhvFlyingWarp),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_TOTWC, /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF3, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x01, /*destNode*/ 0x20, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF0, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x01, /*destNode*/ 0x26, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x01, /*destNode*/ 0x23, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_totwc_objects_1),
        JUMP_LINK(script_totwc_objects_2),
        TERRAIN(/*terrainData*/ totwc_seg7_collision),
        JUMP_LINK(script_totwc_macro_objects),
        SHOW_DIALOG(/*index*/ 0x00, DIALOG_131),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0000, /*seq*/ SEQ_LEVEL_SLIDE),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_STONE),
    END_AREA(),

    FREE_LEVEL_POOL(),
    MARIO_POS(/*area*/ 1, /*yaw*/ 90, /*pos*/ -4095, 2935, 0),
    CALL(/*arg*/ 0, /*func*/ lvl_init_or_update),
    CALL_LOOP(/*arg*/ 1, /*func*/ lvl_init_or_update),
    CLEAR_LEVEL(),
    SLEEP_BEFORE_EXIT(/*frames*/ 1),
    EXIT(),
};
