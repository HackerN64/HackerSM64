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
#include "levels/ccm/header.h"


static const LevelScript script_ccm_area_1_macro_objects[] = {
    // Macro objects
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,    /*pos*/ -2412,  2912,  -878, /*angle*/ 0, 180, 0, /*behParam*/ (DIALOG_040 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_MR_BLIZZARD_HIDDEN, /*pos*/  3054, -2674,  2072, /*angle*/ 0,   0, 0, /*behParam*/ (MR_BLIZZARD_STYPE_NO_CAP << 16), /*beh*/ bhvMrBlizzard),
    OBJECT(/*model*/ MODEL_SPINDRIFT,          /*pos*/  2542,  2622, -1714, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_SPINDRIFT,          /*pos*/ -6090, -1740,  1936, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_SPINDRIFT,          /*pos*/  4346, -3068,   400, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_SPINDRIFT,          /*pos*/ -5054, -4659, -1054, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_SPINDRIFT,          /*pos*/ -5033, -4740, -2666, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSpindrift),
    OBJECT(/*model*/ MODEL_BOBOMB_BUDDY,       /*pos*/ -1220, -3583,  5969, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvBobombBuddyOpensCannon),
    OBJECT(/*model*/ MODEL_DL_CANNON_LID,      /*pos*/ -3615, -4607,  4790, /*angle*/ 0,   0, 0, /*behParam*/ (0x00 << 16), /*beh*/ bhvCannonClosed),
    OBJECT(/*model*/ MODEL_DL_CANNON_LID,      /*pos*/ -5045, -1740,  4615, /*angle*/ 0,   0, 0, /*behParam*/ (0x00 << 16), /*beh*/ bhvCannonClosed),
    OBJECT(/*model*/ MODEL_DL_CANNON_LID,      /*pos*/  1090, -4607,  5729, /*angle*/ 0,   0, 0, /*behParam*/ (0x80 << 16), /*beh*/ bhvCannonClosed),
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,    /*pos*/ -1060,  2560, -1840, /*angle*/ 0, 270, 0, /*behParam*/ (DIALOG_087 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/  -180,  3100, -1480, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_VERTICAL) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/  3560,  2100,  -280, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE                                 ) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/ -1942,  1334,  1678, /*angle*/ 0,  77, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE                                 ) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/ -1020,   646, -2706, /*angle*/ 0,  90, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE                                 ) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/ -1460,  -888, -3933, /*angle*/ 0,  93, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE                                 ) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/ -2586, -4889, -4093, /*angle*/ 0,  75, 0, /*behParam*/ ((COIN_FORMATION_FLAG_ARROW                                ) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,    /*pos*/ -4350, -4864, -4813, /*angle*/ 0, 270, 0, /*behParam*/ (DIALOG_094 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,    /*pos*/ -1037, -3583,  5872, /*angle*/ 0,   0, 0, /*behParam*/ (DIALOG_091 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,    /*pos*/  -309, -4889, -3690, /*angle*/ 0, 180, 0, /*behParam*/ (DIALOG_049 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,    /*pos*/  1900, -1535,  3500, /*angle*/ 0,   0, 0, /*behParam*/ (DIALOG_040 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_RED_COIN,           /*pos*/  -488,  3100, -2305, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,           /*pos*/ -3120, -1200, -2080, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/ -5200, -1345,  2995, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHidden1upInPoleSpawner),
    OBJECT(/*model*/ MODEL_RED_COIN,           /*pos*/  2940, -1400,  2630, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,           /*pos*/  -600,  -505,  1940, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,           /*pos*/ -3814, -2112,  6427, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,           /*pos*/  2700, -4600,  1230, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,           /*pos*/  1990, -4040,  4950, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_BLUE_COIN_SWITCH,   /*pos*/  3400,  -820,     0, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvBlueCoinSwitch),
    OBJECT(/*model*/ MODEL_BLUE_COIN,          /*pos*/  3400,  -640,  -740, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenBlueCoin),
    OBJECT(/*model*/ MODEL_BLUE_COIN,          /*pos*/  3400,  -320, -1740, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenBlueCoin),
    OBJECT(/*model*/ MODEL_HEART,              /*pos*/  -270, -4650, -3100, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRecoveryHeart),
    OBJECT(/*model*/ MODEL_RED_COIN,           /*pos*/  4408, -4550, -1534, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX,    /*pos*/ -4887, -1300, -4003, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_1UP_RUNNING_AWAY << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX,    /*pos*/ -1557,  -205,  1794, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_1UP_RUNNING_AWAY << 16), /*beh*/ bhvExclamationBox),
    // Special objects
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03,  /*pos*/  -409,  2765, -1637, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_04,  /*pos*/  2355, -4095,  4096, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_05,  /*pos*/ -4210, -1446,  1874, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_05,  /*pos*/  2560,  2560, -1023, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_06,  /*pos*/  -818,   102,  2048, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_07,  /*pos*/  -511, -3071, -3583, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/ -5201, -1740,  2994, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/  1989, -4607,  4949, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/  1248, -4607,  5474, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/ -5508, -1740,  4148, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/ -4576, -1740,  4814, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/  -488,  2560, -2305, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/ -5892, -1740,   811, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/ -3748, -4607,  4464, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/  2237,  2560, -1630, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/  2885,  2560, -1638, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/ -1146, -3583,  5919, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/ -1768,  2560, -1793, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CCM_SNOW_TREE,      /*pos*/ -3443,   807, -2713, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CASTLE_WOODEN_DOOR, /*pos*/  2464, -4607,  4302, /*angle*/ 0, 222, 0, /*behParam*/ (20 << 16), /*beh*/ bhvDoorWarp),
    RETURN(),
};

static const LevelScript script_ccm_area_2_macro_objects[] = {
    // Macro objects
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  2255,  4091,  -536, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  2055,  4352,  -497, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  1843,  4486,  -433, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  1603,  4567,  -347, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  1397,  4530,  -287, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,    /*pos*/ -5320,  6656, -6540, /*angle*/ 0,   0, 0, /*behParam*/ (DIALOG_054 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/ -4980,  2180, -1980, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/  6640,   -60,  -800, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/  6640, -1020,   440, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/  6640, -1820,  1760, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/  6640, -2260,  2620, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/ -6460, -5000, -3200, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/ -6460, -5540, -4100, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/ -6460, -5200, -5000, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX,    /*pos*/ -5600, -4500, -6644, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_1UP_WALKING << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_BLUE_COIN,          /*pos*/ -3160,  6267, -6120, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvMovingBlueCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  -700,  5923, -6240, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  2260,  5342, -6440, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_NONE,               /*pos*/  2960,  4280,  -880, /*angle*/ 0, 120, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  1900,   659, -2340, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  1720,   690, -2220, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  3400,   302, -5240, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  3600,   297, -5420, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  6120,  4435, -5020, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  6140,  -152, -5540, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  6320,  -200, -5320, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  3960, -3297,  6440, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  1400, -3560,  5840, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  -220, -3780,  4960, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  -520, -3890,  4360, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  -740, -4008,  3540, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  -920, -4094,  2760, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  -960, -4101,  2080, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/  -860,  3391,  -760, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/ -1120,  3293, -1140, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/ -1560,  2989, -3280, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/ -1740,  2926, -3780, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/ -2849,  2677, -4608, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,        /*pos*/ -4620,  2353, -4120, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_1UP,                /*pos*/ -4890,  1452,   552, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhv1upSliding),
    OBJECT(/*model*/ MODEL_1UP,                /*pos*/ -6369, -1538,  3726, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhv1upSliding),
    // Special objects
    OBJECT(/*model*/ MODEL_CASTLE_WOODEN_DOOR, /*pos*/ -7628, -5836, -6988, /*angle*/ 0, 90, 0,  /*behParam*/ (20 << 16), /*beh*/ bhvDoorWarp),
    RETURN(),
};

static const LevelScript script_ccm_area_1_objects_1[] = {
    OBJECT(/*model*/ MODEL_CCM_ROPEWAY_LIFT, /*pos*/ 531, -4430, 6426,    /*angle*/ 0,   0, 0, /*behParam*/ 0x07120000, /*beh*/ bhvPlatformOnTrack),
    RETURN(),
};

static const LevelScript script_ccm_area_1_objects_2[] = {
    OBJECT(/*model*/ MODEL_PENGUIN,          /*pos*/  2650, -3735,  3970, /*angle*/ 0,   0, 0, /*behParam*/ 0x00010000, /*beh*/ bhvSmallPenguin),
    OBJECT(/*model*/ MODEL_PENGUIN,          /*pos*/  -555,  3470, -1000, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSmallPenguin),
    OBJECT(/*model*/ MODEL_MR_BLIZZARD,      /*pos*/ -2376, -1589,  4256, /*angle*/ 0, 252, 0, /*behParam*/ 0x00010000, /*beh*/ bhvMrBlizzard),
    OBJECT(/*model*/ MODEL_MR_BLIZZARD,      /*pos*/  -394, -1589,  4878, /*angle*/ 0,  74, 0, /*behParam*/ 0x00010000, /*beh*/ bhvMrBlizzard),
    OBJECT_WITH_ACTS(/*model*/ MODEL_CCM_SNOWMAN_BASE, /*pos*/  2560,  2662, -1122, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSnowmansBottom, /*acts*/ ACT_5),
    RETURN(),
};

static const LevelScript script_ccm_area_1_objects_3[] = {
    OBJECT_WITH_ACTS(/*model*/ MODEL_NONE,             /*pos*/  2665, -4607,  4525, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvCcmTouchedStarSpawn, /*acts*/ ALL_ACTS),
    OBJECT_WITH_ACTS(/*model*/ MODEL_PENGUIN,          /*pos*/  3450, -4700,  4550, /*angle*/ 0,   0, 0, /*behParam*/ 0x01000000, /*beh*/ bhvTuxiesMother,          /*acts*/ ALL_ACTS),
    OBJECT_WITH_ACTS(/*model*/ MODEL_NONE,             /*pos*/  4200,  -927,   400, /*angle*/ 0,   0, 0, /*behParam*/ 0x03000000, /*beh*/ bhvHiddenRedCoinStar,   /*acts*/ ALL_ACTS),
    OBJECT_WITH_ACTS(/*model*/ MODEL_CCM_SNOWMAN_HEAD, /*pos*/ -4230, -1169,  1813, /*angle*/ 0, 270, 0, /*behParam*/ 0x04000000, /*beh*/ bhvSnowmansHead,          /*acts*/ ALL_ACTS),
    OBJECT_WITH_ACTS(/*model*/ MODEL_STAR,             /*pos*/ -2000, -2200, -3000, /*angle*/ 0,   0, 0, /*behParam*/ 0x05000000, /*beh*/ bhvStar,                   /*acts*/ ALL_ACTS),
    RETURN(),
};

static const LevelScript script_ccm_area_2_objects_1[] = {
    OBJECT_WITH_ACTS(/*model*/ MODEL_PENGUIN, /*pos*/ -4952,  6656, -6075, /*angle*/ 0, 270, 0, /*behParam*/ 0x02000000, /*beh*/ bhvRacingPenguin,   /*acts*/ ACT_2 | ACT_3 | ACT_4 | ACT_5 | ACT_6),
    OBJECT(/*model*/ MODEL_NONE,    /*pos*/ -6500, -5836, -6400, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvPenguinRaceFinishLine),
    OBJECT(/*model*/ MODEL_NONE,    /*pos*/ -6393,  -716,  7503, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvPenguinRaceShortcutCheck),
    OBJECT(/*model*/ MODEL_NONE,    /*pos*/ -4943,  1321,   667, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvPlaysMusicTrackWhenTouched),
    RETURN(),
};

const LevelScript level_ccm_entry[] = {
    INIT_LEVEL(),
    LOAD_YAY0(        /*seg*/ 0x07, _ccm_segment_7SegmentRomStart, _ccm_segment_7SegmentRomEnd),
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
    LOAD_MODEL_FROM_GEO(MODEL_LEVEL_GEOMETRY_03, ccm_geo_00042C),
    LOAD_MODEL_FROM_GEO(MODEL_LEVEL_GEOMETRY_04, ccm_geo_00045C),
    LOAD_MODEL_FROM_GEO(MODEL_LEVEL_GEOMETRY_05, ccm_geo_000494),
    LOAD_MODEL_FROM_GEO(MODEL_LEVEL_GEOMETRY_06, ccm_geo_0004BC),
    LOAD_MODEL_FROM_GEO(MODEL_LEVEL_GEOMETRY_07, ccm_geo_0004E4),
    LOAD_MODEL_FROM_GEO(MODEL_CCM_CABIN_DOOR,    cabin_door_geo),
    LOAD_MODEL_FROM_GEO(MODEL_CCM_SNOW_TREE,     snow_tree_geo),
    LOAD_MODEL_FROM_GEO(MODEL_CCM_ROPEWAY_LIFT,  ccm_geo_0003D0),
    LOAD_MODEL_FROM_GEO(MODEL_CCM_SNOWMAN_BASE,  ccm_geo_0003F0),
    LOAD_MODEL_FROM_GEO(MODEL_CCM_SNOWMAN_HEAD,  ccm_geo_00040C),

    AREA(/*index*/ 1, ccm_geo_00051C),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ -1512,  3560, -2305, /*angle*/ 0,  140, 0, /*behParam*/ 0x000A0000, /*beh*/ bhvSpinAirborneWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/  -181,  2918, -1486, /*angle*/ 0,    0, 0, /*behParam*/ 0x0F1E0000, /*beh*/ bhvWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ -1847,  2815,  -321, /*angle*/ 0, -158, 0, /*behParam*/ 0x001F0000, /*beh*/ bhvFadingWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/  3349, -4694,  -183, /*angle*/ 0,  -34, 0, /*behParam*/ 0x00200000, /*beh*/ bhvFadingWarp),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_CCM, /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x14, /*destLevel*/ LEVEL_CCM, /*destArea*/ 0x02, /*destNode*/ 0x14, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x1E, /*destLevel*/ LEVEL_CCM, /*destArea*/ 0x02, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x1F, /*destLevel*/ LEVEL_CCM, /*destArea*/ 0x01, /*destNode*/ 0x20, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x20, /*destLevel*/ LEVEL_CCM, /*destArea*/ 0x01, /*destNode*/ 0x1F, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF0, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x01, /*destNode*/ 0x33, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x01, /*destNode*/ 0x65, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_ccm_area_1_objects_1),
        JUMP_LINK(script_ccm_area_1_objects_2),
        JUMP_LINK(script_ccm_area_1_objects_3),
        TERRAIN(/*terrainData*/ ccm_seg7_area_1_collision),
        JUMP_LINK(script_ccm_area_1_macro_objects),
        SHOW_DIALOG(/*index*/ 0x00, DIALOG_048),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0000, /*seq*/ SEQ_LEVEL_SNOW),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_SNOW),
    END_AREA(),

    AREA(/*index*/ 2, ccm_geo_0005E8),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ -5836, 7465, -6143, /*angle*/ 0, 90, 0, /*behParam*/ 0x000A0000, /*beh*/ bhvAirborneWarp),
        WARP_NODE(/*id*/ 0x14, /*destLevel*/ LEVEL_CCM, /*destArea*/ 0x01, /*destNode*/ 0x14, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_CCM, /*destArea*/ 0x02, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF0, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x01, /*destNode*/ 0x33, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x01, /*destNode*/ 0x65, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_ccm_area_2_objects_1),
        TERRAIN(/*terrainData*/ ccm_seg7_area_2_collision),
        JUMP_LINK(script_ccm_area_2_macro_objects),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0001, /*seq*/ SEQ_LEVEL_SLIDE),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_SLIDE),
    END_AREA(),

    FREE_LEVEL_POOL(),
    MARIO_POS(/*area*/ 1, /*yaw*/ 140, /*pos*/ -1512, 2560, -2305),
    CALL(/*arg*/ 0, /*func*/ lvl_init_or_update),
    CALL_LOOP(/*arg*/ 1, /*func*/ lvl_init_or_update),
    CLEAR_LEVEL(),
    SLEEP_BEFORE_EXIT(/*frames*/ 1),
    EXIT(),
};
