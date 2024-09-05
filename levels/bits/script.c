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
#include "levels/bits/header.h"


static const LevelScript script_bits_macro_objects[] = {
    // Macro objects
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/ -1400, -4000,     0, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/  -272, -4000,     0, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/   840, -4000,     0, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_AMP,               /*pos*/  4100, -1320, -1800, /*angle*/ 0,   0, 0, /*behParam*/ (1 << 16), /*beh*/ bhvCirclingAmp),
    OBJECT(/*model*/ MODEL_GOOMBA,            /*pos*/  6180, -1791,     0, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_GOOMBA,            /*pos*/  6260, -1453, -1800, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_PIRANHA_PLANT,     /*pos*/  1920, -1187, -1840, /*angle*/ 0,   0, 0, /*behParam*/ (FIRE_PIRANHA_PLANT_BP_NORMAL << 16), /*beh*/ bhvFirePiranhaPlant),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/ -5080, -2457,  -800, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_1UP,               /*pos*/  1380, -1740,  -660, /*angle*/ 0,   0, 0, /*behParam*/ (MUSHROOM_BP_REQUIRES_NONE << 16), /*beh*/ bhv1Up),
    OBJECT(/*model*/ MODEL_WHOMP,             /*pos*/ -4180, -1330,  -800, /*angle*/ 0,  90, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSmallWhomp),
    OBJECT(/*model*/ MODEL_CHUCKYA,           /*pos*/ -3909,  -409, -1199, /*angle*/ 0,  90, 0, /*behParam*/ 0x00000000, /*beh*/ bhvChuckya),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/   600,  -200,  -900, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/   900,    60,  -900, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/  1200,   310,  -900, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/  1500,   560,  -900, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/  1800,   810,  -900, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/  2100,  1060,  -900, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_HEART,             /*pos*/  1279,  2639,  -959, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRecoveryHeart),
    OBJECT(/*model*/ MODEL_1UP,               /*pos*/ -1399,  2750, -1159, /*angle*/ 0,   0, 0, /*behParam*/ (MUSHROOM_BP_REQUIRES_NONE << 16), /*beh*/ bhv1Up),
    OBJECT(/*model*/ MODEL_BLACK_BOBOMB,      /*pos*/ -2839,  2059,  -959, /*angle*/ 0,   0, 0, /*behParam*/ (BOBOMB_BP_STYPE_GENERIC << 16), /*beh*/ bhvBobomb),
    OBJECT(/*model*/ MODEL_BLACK_BOBOMB,      /*pos*/ -3479,  2059,  -799, /*angle*/ 0,   0, 0, /*behParam*/ (BOBOMB_BP_STYPE_GENERIC << 16), /*beh*/ bhvBobomb),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/ -7200,  2080,  -890, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/ -6600,  2080,  -180, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/ -6600,  2080, -1550, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/ -6600,  2080,  -180, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHidden1upTrigger),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/ -7200,  2080,  -890, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHidden1upTrigger),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/ -6600,  2080, -1550, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHidden1upTrigger),
    OBJECT(/*model*/ MODEL_1UP,               /*pos*/ -6640,  2280,  -890, /*angle*/ 0,   0, 0, /*behParam*/ (3 << 16), /*beh*/ bhvHidden1up),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/ -5239,  3839,  -999, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvGoombaTripletSpawner),
    OBJECT(/*model*/ MODEL_BLACK_BOBOMB,      /*pos*/ -1559,  3800, -1079, /*angle*/ 0,   0, 0, /*behParam*/ (BOBOMB_BP_STYPE_GENERIC << 16), /*beh*/ bhvBobomb),
    OBJECT(/*model*/ MODEL_BLACK_BOBOMB,      /*pos*/ -1079,  3800, -1039, /*angle*/ 0,   0, 0, /*behParam*/ (BOBOMB_BP_STYPE_GENERIC << 16), /*beh*/ bhvBobomb),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/  1879,  4639, -1559, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_1UP,               /*pos*/  -259,  6059, -3759, /*angle*/ 0,   0, 0, /*behParam*/ (MUSHROOM_BP_REQUIRES_NONE << 16), /*beh*/ bhv1Up),
    OBJECT(/*model*/ MODEL_HEART,             /*pos*/   359,  6099, -3219, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRecoveryHeart),
    OBJECT(/*model*/ MODEL_PIRANHA_PLANT,     /*pos*/  3680,  2428, -1840, /*angle*/ 0,   0, 0, /*behParam*/ (FIRE_PIRANHA_PLANT_BP_NORMAL << 16), /*beh*/ bhvFirePiranhaPlant),
    OBJECT(/*model*/ MODEL_AMP,               /*pos*/ -6460,  2080,  -900, /*angle*/ 0,   0, 0, /*behParam*/ (2 << 16), /*beh*/ bhvCirclingAmp),
    OBJECT(/*model*/ MODEL_AMP,               /*pos*/ -6460,  2280,  -900, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvCirclingAmp),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/   360,  6040, -2880, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvGoombaTripletSpawner),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/  -302, -2457,  -800, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_RED_COIN,          /*pos*/  1590, -1120, -2055, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,          /*pos*/ -2960, -3360,     0, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,          /*pos*/  -298, -2600, -1600, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,          /*pos*/  7618,  4850, -1909, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,          /*pos*/  -136,  3109,  -909, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,          /*pos*/ -3330,  4050,  -900, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,          /*pos*/ -4830,  2103, -1030, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,          /*pos*/   348,  5921, -4585, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_METAL_BOX,         /*pos*/ -2300, -4100,     0, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvPushableMetalBox),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,      /*pos*/ -4769,  2344,  -904, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,      /*pos*/   377,  4140, -2042, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    OBJECT(/*model*/ MODEL_1UP,               /*pos*/ -1751, -1246,  -805, /*angle*/ 0,   0, 0, /*behParam*/ (MUSHROOM_BP_REQUIRES_NONE << 16), /*beh*/ bhv1Up),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,      /*pos*/ -6712,  -899,  -796, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/  7322,  4566, -1922, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,      /*pos*/ -3600, -2433,  -800, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,      /*pos*/ -2656, -2442,  -800, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX,   /*pos*/  4100, -1050, -1800, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_1UP_RUNNING_AWAY << 16), /*beh*/ bhvExclamationBox),
    // Special objects
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/ -4505, -5119,     0, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_04, /*pos*/ -2291, -2073,  -762, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_05, /*pos*/  3891, -3276,  -705, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_06, /*pos*/  1753, -1986,  -531, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_07, /*pos*/ -5414, -1330,  -787, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_07, /*pos*/ -3182,  2059,  -904, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_08, /*pos*/ -3182,  1854,  -904, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_08, /*pos*/   759,  5628, -1008, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_09, /*pos*/ -4902,  -895, -1171, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_0A, /*pos*/ -3882,  -357, -1241, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_0B, /*pos*/ -1199,  -387, -1308, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_0C, /*pos*/  5584,  2735, -1851, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_0D, /*pos*/ -5248,  3800,  -895, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_0D, /*pos*/ -1356,  3800,  -895, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_0E, /*pos*/ -6459,  3800,  -904, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_0F, /*pos*/  -930,  2884,  -904, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_10, /*pos*/  2486,  -438,  -899, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_11, /*pos*/   563,  2249,  -904, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_12, /*pos*/ -6459,  2039,  -904, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_13, /*pos*/  1143,  4670,  -494, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_14, /*pos*/   349,  6268, -4054, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    RETURN(),
};

static const LevelScript script_bits_objects_1[] = {
    OBJECT(/*model*/ MODEL_BITS_SLIDING_PLATFORM,       /*pos*/ -2370, -4525,     0, /*angle*/ 0, 0, 0,   /*behParam*/ 0x00100000, /*beh*/ bhvSlidingPlatform2),
    OBJECT(/*model*/ MODEL_BITS_TWIN_SLIDING_PLATFORMS, /*pos*/ -2611,  3544,  -904, /*angle*/ 0, 0, 0,   /*behParam*/ 0x014F0000, /*beh*/ bhvSlidingPlatform2),
    OBJECT(/*model*/ MODEL_BITS_TWIN_SLIDING_PLATFORMS, /*pos*/ -4700,  3544,  -904, /*angle*/ 0, 180, 0, /*behParam*/ 0x010F0000, /*beh*/ bhvSlidingPlatform2),
    OBJECT(/*model*/ MODEL_BITS_OCTAGONAL_PLATFORM,     /*pos*/  4139, -1740, -1831, /*angle*/ 0, 0, 0,   /*behParam*/ 0x02000000, /*beh*/ bhvOctagonalPlatformRotating),
    OBJECT(/*model*/ MODEL_BITS_OCTAGONAL_PLATFORM,     /*pos*/ -6459,  1732,  -904, /*angle*/ 0, 0, 0,   /*behParam*/ 0x00000000, /*beh*/ bhvOctagonalPlatformRotating),
    OBJECT(/*model*/ MODEL_BITS_OCTAGONAL_PLATFORM,     /*pos*/ -4770,  1732,  -904, /*angle*/ 0, 0, 0,   /*behParam*/ 0x02000000, /*beh*/ bhvOctagonalPlatformRotating),
    OBJECT(/*model*/ MODEL_BITS_FERRIS_WHEEL_AXLE,      /*pos*/ -1748, -1330, -1094, /*angle*/ 0, 0, 0,   /*behParam*/ 0x00000000, /*beh*/ bhvFerrisWheelAxle),
    OBJECT(/*model*/ MODEL_BITS_FERRIS_WHEEL_AXLE,      /*pos*/  2275,  5628, -1315, /*angle*/ 0, 0, 0,   /*behParam*/ 0x00000000, /*beh*/ bhvFerrisWheelAxle),
    OBJECT(/*model*/ MODEL_BITS_FERRIS_WHEEL_AXLE,      /*pos*/  3114,  4701, -1320, /*angle*/ 0, 0, 0,   /*behParam*/ 0x00000000, /*beh*/ bhvFerrisWheelAxle),
    OBJECT(/*model*/ MODEL_BITS_ARROW_PLATFORM,         /*pos*/  2793,  2325,  -904, /*angle*/ 0, 0, 0,   /*behParam*/ 0x00610000, /*beh*/ bhvActivatedBackAndForthPlatform),
    OBJECT(/*model*/ MODEL_BITS_SEESAW_PLATFORM,        /*pos*/    27, -1555,  -713, /*angle*/ 0, 90, 0,  /*behParam*/ 0x00010000, /*beh*/ bhvSeesawPlatform),
    OBJECT(/*model*/ MODEL_BITS_TILTING_W_PLATFORM,     /*pos*/  -306, -4300,     0, /*angle*/ 0, 0, 0,   /*behParam*/ 0x00020000, /*beh*/ bhvSeesawPlatform),
    OBJECT(/*model*/ MODEL_BITS_STAIRCASE,              /*pos*/  1769,  -234,  -899, /*angle*/ 0, 0, 0,   /*behParam*/ 0x00000000, /*beh*/ bhvAnimatesOnFloorSwitchPress),
    OBJECT(/*model*/ MODEL_PURPLE_SWITCH,               /*pos*/  -279,  -234,  -900, /*angle*/ 0, 0, 0,   /*behParam*/ 0x00000000, /*beh*/ bhvFloorSwitchAnimatesObject),
    OBJECT(/*model*/ MODEL_NONE,                        /*pos*/ -6460,  2039,  -905, /*angle*/ 0, 0, 0,   /*behParam*/ 0x00CF0000, /*beh*/ bhvPoleGrabbing),
    OBJECT(/*model*/ MODEL_NONE,                        /*pos*/ -3326,  3227,  -905, /*angle*/ 0, 0, 0,   /*behParam*/ 0x004D0000, /*beh*/ bhvPoleGrabbing),
    OBJECT(/*model*/ MODEL_NONE,                        /*pos*/  5518,  3184, -4019, /*angle*/ 0, 0, 0,   /*behParam*/ 0x00000000, /*beh*/ bhvFlamethrower),
    OBJECT(/*model*/ MODEL_NONE,                        /*pos*/  6465,  3731, -1915, /*angle*/ 0, 0, 0,   /*behParam*/ 0x00000000, /*beh*/ bhvFlamethrower),
    OBJECT(/*model*/ MODEL_NONE,                        /*pos*/  5915,  3718, -4019, /*angle*/ 0, 0, 0,   /*behParam*/ 0x00000000, /*beh*/ bhvFlamethrower),
    RETURN(),
};

static const LevelScript script_bits_objects_2[] = {
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ 350, 6800, -6800, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvBowserCourseRedCoinStar),
    RETURN(),
};

const LevelScript level_bits_entry[] = {
    INIT_LEVEL(),
#include "levels/bits/areas/script_vanilla_load.inc.c"
    ALLOC_LEVEL_POOL(),
    MARIO(/*model*/ MODEL_MARIO, /*behParam*/ 0x00000001, /*beh*/ bhvMario),
#include "levels/bits/areas/script_vanilla.inc.c"

    AREA(/*index*/ 1, bits_geo_000718),
        OBJECT(/*model*/ MODEL_NONE,           /*pos*/ -7039, -3812,     4, /*angle*/ 0, 90, 0,  /*behParam*/ 0x000A0000, /*beh*/ bhvAirborneWarp),
        OBJECT(/*model*/ MODEL_BITS_WARP_PIPE, /*pos*/   351,  6652, -6030, /*angle*/ 0, 0, 0,   /*behParam*/ 0x000B0000, /*beh*/ bhvWarpPipe),
        OBJECT(/*model*/ MODEL_NONE,           /*pos*/   351,  6800, -3900, /*angle*/ 0, 180, 0, /*behParam*/ 0x000C0000, /*beh*/ bhvDeathWarp),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_BITS, /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0B, /*destLevel*/ LEVEL_BOWSER_3, /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0C, /*destLevel*/ LEVEL_BITS, /*destArea*/ 0x01, /*destNode*/ 0x0C, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x02, /*destNode*/ 0x6B, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_bits_objects_1),
        JUMP_LINK(script_bits_objects_2),
        TERRAIN(/*terrainData*/ bits_seg7_collision_level),
        JUMP_LINK(script_bits_macro_objects),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0000, /*seq*/ SEQ_LEVEL_KOOPA_ROAD),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_STONE),
    END_AREA(),

    FREE_LEVEL_POOL(),
    MARIO_POS(/*area*/ 1, /*yaw*/ 90, /*pos*/ -7039, -4812, 4),
    CALL(/*arg*/ 0, /*func*/ lvl_init_or_update),
    CALL_LOOP(/*arg*/ 1, /*func*/ lvl_init_or_update),
    CLEAR_LEVEL(),
    SLEEP_BEFORE_EXIT(/*frames*/ 1),
    EXIT(),
};
