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
#include "levels/sl/header.h"


static const LevelScript script_sl_area_1_macro_objects[] = {
    // Macro objects
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,    /*pos*/  4086,  1024,   400, /*angle*/ 0,  90, 0, /*behParam*/ (DIALOG_086 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  1285,  2210,   385, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  1728,  2560,  -671, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  1371,  2188,  -500, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  1814,  3174,   114, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/    28,  3328,  1885, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  -228,  3482,  1742, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_DL_CANNON_LID,      /*pos*/  4483,   821,  1168, /*angle*/ 0,   0, 0, /*behParam*/ (0x80 << 16), /*beh*/ bhvCannonClosed),
    OBJECT(/*model*/ MODEL_MR_BLIZZARD_HIDDEN, /*pos*/ -3452,  1110, -3364, /*angle*/ 0,   0, 0, /*behParam*/ (MR_BLIZZARD_STYPE_NO_CAP << 16), /*beh*/ bhvMrBlizzard),
    OBJECT(/*model*/ MODEL_MR_BLIZZARD_HIDDEN, /*pos*/  5422,  1065, -1288, /*angle*/ 0,   0, 0, /*behParam*/ (MR_BLIZZARD_STYPE_NO_CAP << 16), /*beh*/ bhvMrBlizzard),
    OBJECT(/*model*/ MODEL_MR_BLIZZARD_HIDDEN, /*pos*/ -6533,  2048, -2444, /*angle*/ 0,   0, 0, /*behParam*/ (MR_BLIZZARD_STYPE_NO_CAP << 16), /*beh*/ bhvMrBlizzard),
    OBJECT(/*model*/ MODEL_RED_COIN,           /*pos*/  4750,  1061, -5230, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,           /*pos*/   416,  1050, -4522, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,           /*pos*/ -6560,  2040, -5080, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,           /*pos*/ -6760,  2040, -1360, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,           /*pos*/ -6880,  1857,  1000, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,           /*pos*/ -4211,  1092, -4723, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,           /*pos*/ -6271,  1390,  4764, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,           /*pos*/  -529,  1050, -5329, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_SPINDRIFT,          /*pos*/ -3760,  1120,  1240, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_SPINDRIFT,          /*pos*/  3840,  1240, -5280, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_SPINDRIFT,          /*pos*/ -3440,  1400,   -40, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_SPINDRIFT,          /*pos*/   400,  1060,  5860, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_SPINDRIFT,          /*pos*/   880,  1080,  4860, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_SPINDRIFT,          /*pos*/  1400,  1080,  3860, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_SPINDRIFT,          /*pos*/  3400,  1660, -2920, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_SPINDRIFT,          /*pos*/ -3171,  1075, -4765, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_SPINDRIFT,          /*pos*/ -4241,  1009, -3834, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX,    /*pos*/ -5450,  1300,  5900, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_KOOPA_SHELL << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_AMP,                /*pos*/  4060,   900, -2940, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvCirclingAmp),
    OBJECT(/*model*/ MODEL_SPINDRIFT,          /*pos*/ -4096,  1125,  3062, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_SPINDRIFT,          /*pos*/ -4536,  1125,  3782, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/  -660,  2120,  1340, /*angle*/ 0,  65, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/ -1520,  1040,   940, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/ -1340,  1280,  1020, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/ -1180,  1520,  1120, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,    /*pos*/  -835,  1125, -3856, /*angle*/ 0, 295, 0, /*behParam*/ (DIALOG_061 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,    /*pos*/ -5050,  1020,  6026, /*angle*/ 0, 180, 0, /*behParam*/ (DIALOG_016 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_FLYGUY,             /*pos*/  2766,  1522, -3633, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFlyGuy),
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,    /*pos*/ -3600,  1024,  -800, /*angle*/ 0, 135, 0, /*behParam*/ (DIALOG_148 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  2440,  1024,  4840, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvMoneybagHidden),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/ -2400,  1177, -4200, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvMoneybagHidden),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX,    /*pos*/ -3380,  1360, -4140, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_1UP_RUNNING_AWAY << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX,    /*pos*/ -4700,  1300,  5850, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_STAR_4           << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  2909,  1024,  4245, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  3418,  1024,  3554, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/     0,  5420,     0, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHidden1upInPoleSpawner),
    // Special objects
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/  5395,  1054, -5443, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/     0,  4864,     0, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/  5666,  1024, -3341, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/  1919,  1024, -4759, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/  3645,  1024, -5889, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/  1658,  1536, -3605, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/ -3769,  1024, -1197, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/ -2745,  1024,  -582, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/  1766,  2816,  -942, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    RETURN(),
};

static const LevelScript script_sl_area_2_macro_objects[] = {
    // Macro objects
    OBJECT(/*model*/ MODEL_YELLOW_COIN,     /*pos*/  1740,     0,   900, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,     /*pos*/  1440,     0,   900, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,     /*pos*/  1140,     0,   900, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_GOOMBA,          /*pos*/  1760,     0,   200, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_SPINDRIFT,       /*pos*/  1600,    80,  -800, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX, /*pos*/  1660,   300, -1720, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_VANISH_CAP << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX, /*pos*/  -720,   300, -1740, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_COINS_3    << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_BOBOMB_BUDDY,    /*pos*/ -1400,     0, -1740, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvBobombBuddyOpensCannon),
    OBJECT(/*model*/ MODEL_GOOMBA,          /*pos*/   -20,     0,   960, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_GOOMBA,          /*pos*/   320,     0, -1220, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_SPINDRIFT,       /*pos*/  -860,    40,  2040, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_SPINDRIFT,       /*pos*/   800,    60,  2000, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/  1500,     0,   500, /*angle*/ 0,  90, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE  ) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/  1500,   100,   500, /*angle*/ 0,  90, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/  1500,   200,   500, /*angle*/ 0,  90, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/  1500,   300,   500, /*angle*/ 0,  90, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_1UP,             /*pos*/  1700,    20,  -100, /*angle*/ 0,   0, 0, /*behParam*/ (MUSHROOM_BP_REQUIRES_NONE << 16), /*beh*/ bhv1Up),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX, /*pos*/  -120,   300, -1740, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_1UP_RUNNING_AWAY << 16), /*beh*/ bhvExclamationBox),
    RETURN(),
};

static const LevelScript script_sl_area_1_objects_1[] = {
    OBJECT_WITH_ACTS(/*model*/ MODEL_STAR, /*pos*/  700, 4500,  690, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStar,                 /*acts*/ ALL_ACTS),
    OBJECT_WITH_ACTS(/*model*/ MODEL_STAR, /*pos*/ 4350, 1350, 4350, /*angle*/ 0, 0, 0, /*behParam*/ 0x02000000, /*beh*/ bhvStar,                 /*acts*/ ALL_ACTS),
    OBJECT_WITH_ACTS(/*model*/ MODEL_NONE, /*pos*/ 5000, 1200,    0, /*angle*/ 0, 0, 0, /*behParam*/ 0x04000000, /*beh*/ bhvHiddenRedCoinStar, /*acts*/ ALL_ACTS),
    RETURN(),
};

static const LevelScript script_sl_area_1_objects_2[] = {
    OBJECT(/*model*/ MODEL_NONE, /*pos*/  977, 1024, 2075, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSnowMoundSpawn),
    RETURN(),
};

static const LevelScript script_sl_area_1_objects_3[] = {
    OBJECT(/*model*/ MODEL_PENGUIN,            /*pos*/ 1715, 3328,   518, /*angle*/ 0, -51, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSLWalkingPenguin),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/  700, 3428,   700, /*angle*/ 0,  30, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSLSnowmanWind),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/  480, 2300,  1370, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvIgloo),
    OBJECT(/*model*/ MODEL_BIG_CHILL_BULLY,    /*pos*/  315, 1331, -4852, /*angle*/ 0,   0, 0, /*behParam*/ 0x01000000, /*beh*/ bhvBigChillBully),
    OBJECT(/*model*/ MODEL_MR_BLIZZARD_HIDDEN, /*pos*/ 2954,  970,   750, /*angle*/ 0,   0, 0, /*behParam*/ 0x00020000, /*beh*/ bhvMrBlizzard),
    RETURN(),
};

static const LevelScript script_sl_area_2_objects_1[] = {
    OBJECT_WITH_ACTS(/*model*/ MODEL_STAR, /*pos*/ 0, 500, 1000, /*angle*/ 0, 0, 0, /*behParam*/ 0x05000000, /*beh*/ bhvStar, /*acts*/ ALL_ACTS),
    RETURN(),
};

const LevelScript level_sl_entry[] = {
    INIT_LEVEL(),
    LOAD_YAY0(        /*seg*/ 0x07, _sl_segment_7SegmentRomStart, _sl_segment_7SegmentRomEnd),
    LOAD_YAY0_TEXTURE(/*seg*/ 0x09, _snow_yay0SegmentRomStart, _snow_yay0SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x0B, _effect_yay0SegmentRomStart, _effect_yay0SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x0A, _ccm_skybox_yay0SegmentRomStart, _ccm_skybox_yay0SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x05, _group7_yay0SegmentRomStart, _group7_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0C, _group7_geoSegmentRomStart,  _group7_geoSegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x06, _group16_yay0SegmentRomStart, _group16_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0D, _group16_geoSegmentRomStart,  _group16_geoSegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x08, _common0_yay0SegmentRomStart, _common0_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0F, _common0_geoSegmentRomStart,  _common0_geoSegmentRomEnd),
    ALLOC_LEVEL_POOL(),
    MARIO(/*model*/ MODEL_MARIO, /*behParam*/ 0x00000001, /*beh*/ bhvMario),
    JUMP_LINK(script_func_global_1),
    JUMP_LINK(script_func_global_8),
    JUMP_LINK(script_func_global_17),
    LOAD_MODEL_FROM_GEO(MODEL_SL_SNOW_TRIANGLE,      sl_geo_000390),
    LOAD_MODEL_FROM_GEO(MODEL_SL_CRACKED_ICE,        sl_geo_000360),
    LOAD_MODEL_FROM_GEO(MODEL_SL_CRACKED_ICE_CHUNK,  sl_geo_000378),
    LOAD_MODEL_FROM_GEO(MODEL_SL_SNOW_TREE,          snow_tree_geo),

    AREA(/*index*/ 1, sl_geo_0003A8),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/  5541, 2024,   443, /*angle*/ 0, 270, 0, /*behParam*/ 0x000A0000, /*beh*/ bhvSpinAirborneWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/   257, 2150,  1399, /*angle*/ 0, 290, 0, /*behParam*/ 0x000B0000, /*beh*/ bhvInstantActiveWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/   569, 2150,  1336, /*angle*/ 0,   0, 0, /*behParam*/ 0x060C0000, /*beh*/ bhvWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/  5468, 1056, -5400, /*angle*/ 0, -20, 0, /*behParam*/ 0x000D0000, /*beh*/ bhvFadingWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ -3698, 1024, -1237, /*angle*/ 0,   6, 0, /*behParam*/ 0x000E0000, /*beh*/ bhvFadingWarp),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_SL, /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0B, /*destLevel*/ LEVEL_SL, /*destArea*/ 0x01, /*destNode*/ 0x0B, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0C, /*destLevel*/ LEVEL_SL, /*destArea*/ 0x02, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0D, /*destLevel*/ LEVEL_SL, /*destArea*/ 0x01, /*destNode*/ 0x0E, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0E, /*destLevel*/ LEVEL_SL, /*destArea*/ 0x01, /*destNode*/ 0x0D, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_sl_area_1_objects_1),
        JUMP_LINK(script_sl_area_1_objects_2),
        JUMP_LINK(script_sl_area_1_objects_3),
        WARP_NODE(/*id*/ 0xF0, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x02, /*destNode*/ 0x36, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x02, /*destNode*/ 0x68, /*flags*/ WARP_NO_CHECKPOINT),
        TERRAIN(/*terrainData*/ sl_seg7_area_1_collision),
        JUMP_LINK(script_sl_area_1_macro_objects),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0000, /*seq*/ SEQ_LEVEL_SNOW),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_SNOW),
    END_AREA(),

    AREA(/*index*/ 2, sl_geo_000484),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ 0, 0, 2867, /*angle*/ 0, 180, 0, /*behParam*/ 0x000A0000, /*beh*/ bhvInstantActiveWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ 0, 0, 3277, /*angle*/ 0,   0, 0, /*behParam*/ 0x140B0000, /*beh*/ bhvWarp),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_SL, /*destArea*/ 0x02, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0B, /*destLevel*/ LEVEL_SL, /*destArea*/ 0x01, /*destNode*/ 0x0B, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_sl_area_2_objects_1),
        WARP_NODE(/*id*/ 0xF0, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x02, /*destNode*/ 0x36, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x02, /*destNode*/ 0x68, /*flags*/ WARP_NO_CHECKPOINT),
        TERRAIN(/*terrainData*/ sl_seg7_area_2_collision),
        JUMP_LINK(script_sl_area_2_macro_objects),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0004, /*seq*/ SEQ_LEVEL_UNDERGROUND),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_SNOW),
    END_AREA(),

    FREE_LEVEL_POOL(),
    MARIO_POS(/*area*/ 1, /*yaw*/ 270, /*pos*/ 5541, 1024, 443),
    CALL(/*arg*/ 0, /*func*/ lvl_init_or_update),
    CALL_LOOP(/*arg*/ 1, /*func*/ lvl_init_or_update),
    CLEAR_LEVEL(),
    SLEEP_BEFORE_EXIT(/*frames*/ 1),
    EXIT(),
};
