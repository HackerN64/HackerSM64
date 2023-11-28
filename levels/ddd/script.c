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
#include "levels/ddd/header.h"


static const LevelScript script_ddd_area_1_macro_objects[] = {
    // Macro objects
    OBJECT(/*model*/ MODEL_NONE,        /*pos*/ -2880, -5120, -1850, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSeaweedBundle),
    OBJECT(/*model*/ MODEL_CLAM_SHELL,  /*pos*/ -4760, -5120,   580, /*angle*/ 0,  90, 0, /*behParam*/ 0x00000000, /*beh*/ bhvClamShell),
    OBJECT(/*model*/ MODEL_NONE,        /*pos*/ -3510, -5120,  1950, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSeaweedBundle),
    OBJECT(/*model*/ MODEL_NONE,        /*pos*/ -4760, -5100,   580, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHidden1upTrigger),
    OBJECT(/*model*/ MODEL_1UP,         /*pos*/ -4760, -5080,   580, /*angle*/ 0,   0, 0, /*behParam*/ (1 << 16), /*beh*/ bhvHidden1up),
    OBJECT(/*model*/ MODEL_NONE,        /*pos*/ -1000, -4250,     0, /*angle*/ 0,  90, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING | COIN_FORMATION_FLAG_VERTICAL) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,        /*pos*/ -4646, -4615,  -800, /*angle*/ 0,   0, 0, /*behParam*/ (FISH_SPAWNER_BP_FEW_BLUE << 16), /*beh*/ bhvFishSpawner),
    OBJECT(/*model*/ MODEL_NONE,        /*pos*/ -1804, -2560,  1292, /*angle*/ 0,   0, 0, /*behParam*/ (FISH_SPAWNER_BP_FEW_CYAN << 16), /*beh*/ bhvFishSpawner),
    OBJECT(/*model*/ MODEL_NONE,        /*pos*/ -3760, -4560,   660, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_VERTICAL                           ) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,        /*pos*/ -4240, -4786,  1180, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_VERTICAL                           ) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,        /*pos*/ -4840, -1137,  1860, /*angle*/ 0, 315, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE                                                            ) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_YELLOW_COIN, /*pos*/ -2180, -5119,  1720, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN, /*pos*/ -1460, -5119,  1300, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN, /*pos*/ -1320, -5119,   840, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_NONE,        /*pos*/ -3580, -2280, -1120, /*angle*/ 0,   0, 0, /*behParam*/ (FISH_SPAWNER_BP_FEW_BLUE << 16), /*beh*/ bhvFishSpawner),
    OBJECT(/*model*/ MODEL_NONE,        /*pos*/ -2020, -3460,  -120, /*angle*/ 0,   0, 0, /*behParam*/ (FISH_SPAWNER_BP_FEW_BLUE << 16), /*beh*/ bhvFishSpawner),
    OBJECT(/*model*/ MODEL_NONE,        /*pos*/     0, -4250,     0, /*angle*/ 0,  90, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING | COIN_FORMATION_FLAG_VERTICAL) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,        /*pos*/ -2000, -4250,     0, /*angle*/ 0,  90, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING | COIN_FORMATION_FLAG_VERTICAL) << 16), /*beh*/ bhvCoinFormation),
    RETURN(),
};

static const LevelScript script_ddd_area_2_macro_objects[] = {
    // Macro objects
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX,  /*pos*/  6800,   500,  -850, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_METAL_CAP  << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX,  /*pos*/  6800,   500,  -150, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_VANISH_CAP << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_PURPLE_SWITCH,    /*pos*/  6800,   110,  2000, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFloorSwitchHiddenObjects),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,    /*pos*/  6075,  -100,  2000, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,    /*pos*/  5875,   100,  2000, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,    /*pos*/  5675,   100,  2000, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,    /*pos*/  5475,   300,  2000, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,    /*pos*/  5275,   300,  2000, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,    /*pos*/  5075,   500,  2000, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,    /*pos*/  4875,   500,  2000, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,    /*pos*/  6275,  -100,  2000, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,    /*pos*/  6475,  -100,  2000, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_RED_COIN,         /*pos*/  4680,  1155, -2200, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,         /*pos*/  3525,  1155, -2250, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,         /*pos*/  3850,  1111,  3550, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,         /*pos*/  4400,  1288,  3595, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,         /*pos*/  2974,  1015,  3584, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,         /*pos*/  5550,  1015,  3575, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,         /*pos*/  3300,  1111, -1900, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,         /*pos*/  2100,  1025, -1600, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/  2061, -3323,  2246, /*angle*/ 0,   0, 0, /*behParam*/ (FISH_SPAWNER_BP_FEW_BLUE << 16), /*beh*/ bhvFishSpawner),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/  5661, -2923, -1415, /*angle*/ 0,   0, 0, /*behParam*/ (FISH_SPAWNER_BP_FEW_CYAN << 16), /*beh*/ bhvFishSpawner),
    OBJECT(/*model*/ MODEL_CLAM_SHELL,       /*pos*/  1273, -4106,  2320, /*angle*/ 0,  90, 0, /*behParam*/ 0x00000000, /*beh*/ bhvClamShell),
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,  /*pos*/  3086,   110,  6120, /*angle*/ 0, 180, 0, /*behParam*/ (DIALOG_053 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/  6420, -4106, -2026, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSeaweedBundle),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/  6340, -4106, -1413, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSeaweedBundle),
    OBJECT(/*model*/ MODEL_CLAM_SHELL,       /*pos*/  1966, -4106,  3226, /*angle*/ 0, 135, 0, /*behParam*/ 0x00000000, /*beh*/ bhvClamShell),
    OBJECT(/*model*/ MODEL_CLAM_SHELL,       /*pos*/  3886, -4006,  5130, /*angle*/ 0, 180, 0, /*behParam*/ 0x00000000, /*beh*/ bhvClamShell),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,    /*pos*/  5740,   710,  3900, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,    /*pos*/  5740,   510,  4100, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,    /*pos*/  5740,   310,  4300, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,    /*pos*/  5740,   110,  4500, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/  2999, -4092,  4430, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSeaweedBundle),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/  4876, -4087,  4430, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_RING                                 ) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/  3892, -4030, -3830, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_VERTICAL) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/  3923,   110,  5953, /*angle*/ 0,  90, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE                                 ) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,     /*pos*/   938,   153,  4107, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,    /*pos*/  5940,   110,  4500, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BLUE_COIN_SWITCH, /*pos*/  1614,   928,   922, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvBlueCoinSwitch),
    OBJECT(/*model*/ MODEL_BLUE_COIN,        /*pos*/  1414,  1020,   622, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenBlueCoin),
    OBJECT(/*model*/ MODEL_BLUE_COIN,        /*pos*/  1814,  1020,   622, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenBlueCoin),
    OBJECT(/*model*/ MODEL_BLUE_COIN,        /*pos*/  1814,  1020,  1222, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenBlueCoin),
    OBJECT(/*model*/ MODEL_BLUE_COIN,        /*pos*/  1414,  1020,  1222, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenBlueCoin),
    OBJECT(/*model*/ MODEL_BLUE_COIN,        /*pos*/  1414,  1020,   922, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenBlueCoin),
    OBJECT(/*model*/ MODEL_BLUE_COIN,        /*pos*/  1814,  1020,   922, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenBlueCoin),
    OBJECT(/*model*/ MODEL_KOOPA_SHELL,      /*pos*/  3886, -4050,  5100, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvKoopaShellUnderwater),
    RETURN(),
};

static const LevelScript script_ddd_area_1_objects_1[] = {
    OBJECT(/*model*/ MODEL_SUSHI,        /*pos*/ -3071,  -270,   0, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSushiShark),
    OBJECT(/*model*/ MODEL_SUSHI,        /*pos*/ -3071, -4270,   0, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSushiShark),
    OBJECT(/*model*/ MODEL_NONE,         /*pos*/ -3071,  -130,   0, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFewBlueFishSpawner),
    OBJECT(/*model*/ MODEL_NONE,         /*pos*/ -3071, -4270,   0, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvManyBlueFishSpawner),
    OBJECT(/*model*/ MODEL_NONE,         /*pos*/ -3071, -2000,   0, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvChirpChirp),
    OBJECT(/*model*/ MODEL_NONE,         /*pos*/ -3071, -3000,   0, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvChirpChirp),
    OBJECT(/*model*/ MODEL_DL_WHIRLPOOL, /*pos*/ -3174, -4915, 102, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvWhirlpool),
    RETURN(),
};

static const LevelScript script_ddd_area_1_objects_2[] = {
    OBJECT_WITH_ACTS(/*model*/ MODEL_NONE,      /*pos*/ -2400, -4607, 125, /*angle*/ 0, 0, 0, /*behParam*/ 0x01000000, /*beh*/ bhvTreasureChests, /*acts*/ ALL_ACTS),
    OBJECT_WITH_ACTS(/*model*/ MODEL_MANTA_RAY, /*pos*/ -4640, -1380,  40, /*angle*/ 0, 0, 0, /*behParam*/ 0x04000000, /*beh*/ bhvMantaRay,       /*acts*/ ACT_2 | ACT_3 | ACT_4 | ACT_5 | ACT_6),
    RETURN(),
};

static const LevelScript script_ddd_area_2_objects_1[] = {
    OBJECT(/*model*/ MODEL_DDD_BOWSER_SUB_DOOR, /*pos*/    0,    0,     0, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvBowserSubDoor),
    OBJECT(/*model*/ MODEL_DDD_BOWSER_SUB,      /*pos*/    0,    0,     0, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvBowsersSub),
    OBJECT(/*model*/ MODEL_DDD_POLE,            /*pos*/ 5120, 1005,  3584, /*angle*/ 0, 180, 0, /*behParam*/ 0x001E0000, /*beh*/ bhvDDDPole),
    OBJECT(/*model*/ MODEL_DDD_POLE,            /*pos*/ 5605, 1005,  3380, /*angle*/ 0, 270, 0, /*behParam*/ 0x00150000, /*beh*/ bhvDDDPole),
    OBJECT(/*model*/ MODEL_DDD_POLE,            /*pos*/ 1800, 1005,  1275, /*angle*/ 0,   0, 0, /*behParam*/ 0x000B0000, /*beh*/ bhvDDDPole),
    OBJECT(/*model*/ MODEL_DDD_POLE,            /*pos*/ 4000, 1005,  1075, /*angle*/ 0, 180, 0, /*behParam*/ 0x000B0000, /*beh*/ bhvDDDPole),
    OBJECT(/*model*/ MODEL_DDD_POLE,            /*pos*/ 1830, 1005,   520, /*angle*/ 0, 270, 0, /*behParam*/ 0x00140000, /*beh*/ bhvDDDPole),
    OBJECT(/*model*/ MODEL_DDD_POLE,            /*pos*/ 4000, 1005,  1275, /*angle*/ 0,   0, 0, /*behParam*/ 0x000B0000, /*beh*/ bhvDDDPole),
    OBJECT(/*model*/ MODEL_DDD_POLE,            /*pos*/ 5760, 1005,   360, /*angle*/ 0, 270, 0, /*behParam*/ 0x00170000, /*beh*/ bhvDDDPole),
    OBJECT(/*model*/ MODEL_DDD_POLE,            /*pos*/ 3310, 1005, -1945, /*angle*/ 0,   0, 0, /*behParam*/ 0x00170000, /*beh*/ bhvDDDPole),
    OBJECT(/*model*/ MODEL_DDD_POLE,            /*pos*/ 3550, 1005, -2250, /*angle*/ 0,   0, 0, /*behParam*/ 0x000D0000, /*beh*/ bhvDDDPole),
    RETURN(),
};

static const LevelScript script_ddd_area_2_objects_2[] = {
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ 3404, -3319, -489, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvJetStream),
    RETURN(),
};

static const LevelScript script_ddd_area_2_objects_3[] = {
    OBJECT_WITH_ACTS(/*model*/ MODEL_STAR, /*pos*/ 3900,   850,  -600, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStar,                    /*acts*/ ALL_ACTS),
    OBJECT_WITH_ACTS(/*model*/ MODEL_NONE, /*pos*/ 5513,  1200,   900, /*angle*/ 0, 0, 0, /*behParam*/ 0x02000000, /*beh*/ bhvHiddenRedCoinStar,    /*acts*/ ALL_ACTS),
    OBJECT_WITH_ACTS(/*model*/ MODEL_NONE, /*pos*/ 3404, -3319,  -489, /*angle*/ 0, 0, 0, /*behParam*/ 0x03000000, /*beh*/ bhvJetStreamRingSpawner, /*acts*/ ALL_ACTS),
    OBJECT_WITH_ACTS(/*model*/ MODEL_STAR, /*pos*/ 2030, -3700, -2780, /*angle*/ 0, 0, 0, /*behParam*/ 0x05000000, /*beh*/ bhvStar,                    /*acts*/ ALL_ACTS),
    RETURN(),
};

const LevelScript level_ddd_entry[] = {
    INIT_LEVEL(),
    LOAD_YAY0(        /*seg*/ 0x07, _ddd_segment_7SegmentRomStart, _ddd_segment_7SegmentRomEnd),
    LOAD_YAY0_TEXTURE(/*seg*/ 0x09, _water_yay0SegmentRomStart, _water_yay0SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x0B, _effect_yay0SegmentRomStart, _effect_yay0SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x0A, _water_skybox_yay0SegmentRomStart, _water_skybox_yay0SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x05, _group4_yay0SegmentRomStart, _group4_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0C, _group4_geoSegmentRomStart,  _group4_geoSegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x06, _group13_yay0SegmentRomStart, _group13_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0D, _group13_geoSegmentRomStart,  _group13_geoSegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x08, _common0_yay0SegmentRomStart, _common0_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0F, _common0_geoSegmentRomStart,  _common0_geoSegmentRomEnd),
    ALLOC_LEVEL_POOL(),
    MARIO(/*model*/ MODEL_MARIO, /*behParam*/ 0x00000001, /*beh*/ bhvMario),
    JUMP_LINK(script_func_global_1),
    JUMP_LINK(script_func_global_5),
    JUMP_LINK(script_func_global_14),
    LOAD_MODEL_FROM_GEO(MODEL_DDD_BOWSER_SUB_DOOR, ddd_geo_000478),
    LOAD_MODEL_FROM_GEO(MODEL_DDD_BOWSER_SUB,      ddd_geo_0004A0),
    LOAD_MODEL_FROM_GEO(MODEL_DDD_POLE,            ddd_geo_000450),

    AREA(/*index*/ 1, ddd_geo_0004C0),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ -3071, 3000, 0, /*angle*/ 0, 7, 0, /*behParam*/ 0x000A0000, /*beh*/ bhvSpinAirborneWarp),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_DDD, /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF0, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x03, /*destNode*/ 0x35, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x03, /*destNode*/ 0x67, /*flags*/ WARP_NO_CHECKPOINT),
        WHIRLPOOL(/*index*/ 0, /*acts*/ ALL_ACTS, /*pos*/ -3174, -4915, 102, /*strength*/ 20),
        JUMP_LINK(script_ddd_area_1_objects_1),
        JUMP_LINK(script_ddd_area_1_objects_2),
        INSTANT_WARP(/*index*/ 3, /*destArea*/ 2, /*displace*/ -8192, 0, 0),
        TERRAIN(/*terrainData*/ ddd_seg7_area_1_collision),
        JUMP_LINK(script_ddd_area_1_macro_objects),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0003, /*seq*/ SEQ_LEVEL_WATER),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_WATER),
    END_AREA(),

    AREA(/*index*/ 2, ddd_geo_000570),
        WHIRLPOOL(/*index*/ 0, /*acts*/ ALL_ACTS,                              /*pos*/  3355, -3575,  -515, /*strength*/ -30),
        WHIRLPOOL(/*index*/ 1, /*acts*/ ACT_2 | ACT_3 | ACT_4 | ACT_5 | ACT_6, /*pos*/  3917, -2040, -6041, /*strength*/  50),
        WARP_NODE(/*id*/ 0xF0, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x03, /*destNode*/ 0x35, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x03, /*destNode*/ 0x67, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF3, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x1E, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_ddd_area_2_objects_1),
        JUMP_LINK(script_ddd_area_2_objects_2),
        JUMP_LINK(script_ddd_area_2_objects_3),
        INSTANT_WARP(/*index*/ 2, /*destArea*/ 1, /*displace*/ 8192, 0, 0),
        TERRAIN(/*terrainData*/ ddd_seg7_area_2_collision),
        JUMP_LINK(script_ddd_area_2_macro_objects),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0003, /*seq*/ SEQ_LEVEL_WATER),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_WATER),
    END_AREA(),

    FREE_LEVEL_POOL(),
    MARIO_POS(/*area*/ 1, /*yaw*/ 180, /*pos*/ -3071, 3000, 500),
    CALL(/*arg*/ 0, /*func*/ lvl_init_or_update),
    CALL_LOOP(/*arg*/ 1, /*func*/ lvl_init_or_update),
    CLEAR_LEVEL(),
    SLEEP_BEFORE_EXIT(/*frames*/ 1),
    EXIT(),
};
