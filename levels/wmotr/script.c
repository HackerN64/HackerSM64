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


static const LevelScript script_wmotr_macro_objects[] = {
    // Macro objects
    OBJECT(/*model*/ MODEL_DL_CANNON_LID,   /*pos*/ -4456,   827,   191, /*angle*/ 0,   0, 0, /*behParam*/ (0x00 << 16), /*beh*/ bhvCannonClosed),
    OBJECT(/*model*/ MODEL_DL_CANNON_LID,   /*pos*/  3712, -2740,  5200, /*angle*/ 0,   0, 0, /*behParam*/ (0x80 << 16), /*beh*/ bhvCannonClosed),
    OBJECT(/*model*/ MODEL_BOBOMB_BUDDY,    /*pos*/  3684, -2712,  4660, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvBobombBuddyOpensCannon),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/ -3050,  2100, -4200, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX, /*pos*/ -2744,  4899, -4439, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_1UP_WALKING << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/  3995, -1850,  5478, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHidden1upInPoleSpawner),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/ -2980,  3990, -4248, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/  2735,  3140, -3085, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/ -3640,  4600, -4200, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/  4400,   240,    80, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/  3440, -2680,  5240, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/  -600, -1360,  5040, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX, /*pos*/  -400,  1960,  -120, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_WING_CAP << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX, /*pos*/  -240, -1080,  4520, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_WING_CAP << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX, /*pos*/  3600, -2480,  5440, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_WING_CAP << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX, /*pos*/  3960,   520,   440, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_WING_CAP << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX, /*pos*/ -3200,  4880, -4040, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_WING_CAP << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX, /*pos*/ -2760,  2320, -4080, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_WING_CAP << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_1UP,             /*pos*/ -3630,  -430,  3180, /*angle*/ 0,   0, 0, /*behParam*/ (MUSHROOM_BP_REQUIRES_NONE << 16), /*beh*/ bhv1Up),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/   320,  1725,    40, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/ -2560,  4600, -4800, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/   -50,  2180,  1900, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING | COIN_FORMATION_FLAG_VERTICAL) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/ -2850,  1550,  2280, /*angle*/ 0,  40, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING | COIN_FORMATION_FLAG_VERTICAL) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/  1710,  -650,  4850, /*angle*/ 0,  85, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING | COIN_FORMATION_FLAG_VERTICAL) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/  2350,  2300,   240, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING | COIN_FORMATION_FLAG_VERTICAL) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_1UP,             /*pos*/ -3260,  3370, -3945, /*angle*/ 0,   0, 0, /*behParam*/ (MUSHROOM_BP_REQUIRES_NONE << 16), /*beh*/ bhv1Up),
    RETURN(),
};

static const LevelScript script_wmotr_objects_1[] = {
    OBJECT(/*model*/ MODEL_NONE, /*pos*/  3996, -2739,  5477, /*angle*/ 0, 0, 0, /*behParam*/ 0x00520000, /*beh*/ bhvPoleGrabbing),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -2911,  3564, -3967, /*angle*/ 0, 0, 0, /*behParam*/ 0x00540000, /*beh*/ bhvPoleGrabbing),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -3258,  3359, -3946, /*angle*/ 0, 0, 0, /*behParam*/ 0x00690000, /*beh*/ bhvPoleGrabbing),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -2639,  3154, -4369, /*angle*/ 0, 0, 0, /*behParam*/ 0x007D0000, /*beh*/ bhvPoleGrabbing),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -2980,  4048, -4248, /*angle*/ 0, 0, 0, /*behParam*/ 0x00240000, /*beh*/ bhvPoleGrabbing),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -3290,  3636, -4477, /*angle*/ 0, 0, 0, /*behParam*/ 0x004D0000, /*beh*/ bhvPoleGrabbing),
    RETURN(),
};

static const LevelScript script_wmotr_objects_2[] = {
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -160, 1950, -470, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenRedCoinStar),
    RETURN(),
};

const LevelScript level_wmotr_entry[] = {
    INIT_LEVEL(),
    LOAD_YAY0(        /*seg*/ 0x07, _wmotr_segment_7SegmentRomStart, _wmotr_segment_7SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x0A, _cloud_floor_skybox_yay0SegmentRomStart, _cloud_floor_skybox_yay0SegmentRomEnd),
    LOAD_YAY0_TEXTURE(/*seg*/ 0x09, _sky_yay0SegmentRomStart, _sky_yay0SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x05, _group2_yay0SegmentRomStart, _group2_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0C, _group2_geoSegmentRomStart,  _group2_geoSegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x06, _group17_yay0SegmentRomStart, _group17_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0D, _group17_geoSegmentRomStart,  _group17_geoSegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x08, _common0_yay0SegmentRomStart, _common0_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0F, _common0_geoSegmentRomStart,  _common0_geoSegmentRomEnd),
    ALLOC_LEVEL_POOL(),
    MARIO(/*model*/ MODEL_MARIO, /*behParam*/ 0x00000001, /*beh*/ bhvMario),
    JUMP_LINK(script_func_global_1),
    JUMP_LINK(script_func_global_3),
    JUMP_LINK(script_func_global_18),

    AREA(/*index*/ 1, wmotr_geo_0001F0),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ -67, 2669, -16, /*angle*/ 0, 270, 0, /*behParam*/ 0x000A0000, /*beh*/ bhvAirborneWarp),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_WMOTR, /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF0, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x02, /*destNode*/ 0x38, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x02, /*destNode*/ 0x6D, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF3, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_wmotr_objects_1),
        JUMP_LINK(script_wmotr_objects_2),
        TERRAIN(/*terrainData*/ wmotr_seg7_collision),
        JUMP_LINK(script_wmotr_macro_objects),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0000, /*seq*/ SEQ_LEVEL_SLIDE),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_SNOW),
    END_AREA(),

    FREE_LEVEL_POOL(),
    MARIO_POS(/*area*/ 1, /*yaw*/ 270, /*pos*/ -67, 1669, -16),
    CALL(/*arg*/ 0, /*func*/ lvl_init_or_update),
    CALL_LOOP(/*arg*/ 1, /*func*/ lvl_init_or_update),
    CLEAR_LEVEL(),
    SLEEP_BEFORE_EXIT(/*frames*/ 1),
    EXIT(),
};
