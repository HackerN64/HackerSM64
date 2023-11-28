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
#include "levels/thi/header.h"


static const LevelScript script_thi_area_1_macro_objects[] = {
    // Macro objects
    OBJECT(/*model*/ MODEL_DL_CANNON_LID,    /*pos*/  6656, -2832,  6964, /*angle*/ 0,   0, 0, /*behParam*/ (0x80 << 16), /*beh*/ bhvCannonClosed),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/   870,  -502,  2828, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/  4800,  -110,  2250, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHidden1upInPoleSpawner),
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,  /*pos*/  6728, -2559,  1561, /*angle*/ 0,  90, 0, /*behParam*/ (DIALOG_091 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/   -66, -1637, -4944, /*angle*/ 0,  90, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_GOOMBA,           /*pos*/  7069, -1535, -4758, /*angle*/ 0,   0, 0, /*behParam*/ (GOOMBA_SIZE_HUGE << 16), /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_GOOMBA,           /*pos*/  7177, -1535, -3522, /*angle*/ 0,   0, 0, /*behParam*/ (GOOMBA_SIZE_HUGE << 16), /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/  5711, -1677, -2944, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,      /*pos*/ -1200, -1540,  1150, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,      /*pos*/  -250, -1540,  1150, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/ -1200, -1540,  1150, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHidden1upTrigger),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/  -250, -1550,  1150, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHidden1upTrigger),
    OBJECT(/*model*/ MODEL_1UP,              /*pos*/  -777, -1544,  1233, /*angle*/ 0,   0, 0, /*behParam*/ (2 << 16), /*beh*/ bhvHidden1up),
    OBJECT(/*model*/ MODEL_GOOMBA,           /*pos*/  4600, -1544,  3455, /*angle*/ 0,   0, 0, /*behParam*/ (GOOMBA_SIZE_HUGE << 16), /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_GOOMBA,           /*pos*/  3444,  -522,  3011, /*angle*/ 0,   0, 0, /*behParam*/ (GOOMBA_SIZE_HUGE << 16), /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_GOOMBA,           /*pos*/ -3622,  -511,  3100, /*angle*/ 0,   0, 0, /*behParam*/ (GOOMBA_SIZE_HUGE << 16), /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/ -4911,  -395, -1433, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/   199,  2233,   433, /*angle*/ 0,  90, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_GOOMBA,           /*pos*/ -3177,  1255, -2366, /*angle*/ 0,   0, 0, /*behParam*/ (GOOMBA_SIZE_HUGE << 16), /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_CHUCKYA,          /*pos*/ -1800,  2233,  -322, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvChuckya),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/ -6222, -3400, -1455, /*angle*/ 0,   0, 0, /*behParam*/ (FISH_SPAWNER_BP_FEW_BLUE << 16), /*beh*/ bhvFishSpawner),
    OBJECT(/*model*/ MODEL_ENEMY_LAKITU,     /*pos*/ -1905, -2223,  6541, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvEnemyLakitu),
    OBJECT(/*model*/ MODEL_FLYGUY,           /*pos*/ -1911,     0, -5822, /*angle*/ 0,   0, 0, /*behParam*/ (FLY_GUY_BP_SHOOTS_FIRE << 16), /*beh*/ bhvFlyGuy),
    OBJECT(/*model*/ MODEL_FLYGUY,           /*pos*/  6493, -2186,  5189, /*angle*/ 0,   0, 0, /*behParam*/ (FLY_GUY_BP_SHOOTS_FIRE << 16), /*beh*/ bhvFlyGuy),
    OBJECT(/*model*/ MODEL_1UP,              /*pos*/ -6000, -3566, -1320, /*angle*/ 0,   0, 0, /*behParam*/ (MUSHROOM_BP_REQUIRES_NONE << 16), /*beh*/ bhv1Up),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/ -6550, -3450,  4100, /*angle*/ 0,   0, 0, /*behParam*/ (FISH_SPAWNER_BP_FEW_BLUE << 16), /*beh*/ bhvFishSpawner),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/ -1750, -3450,  7800, /*angle*/ 0,   0, 0, /*behParam*/ (FISH_SPAWNER_BP_FEW_BLUE << 16), /*beh*/ bhvFishSpawner),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/  1950, -3500,  7600, /*angle*/ 0,   0, 0, /*behParam*/ (FISH_SPAWNER_BP_FEW_BLUE << 16), /*beh*/ bhvFishSpawner),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX,  /*pos*/ -5712, -2190,  1100, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_1UP_RUNNING_AWAY << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX,  /*pos*/  6022, -1722,  -633, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_1UP_RUNNING_AWAY << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_WOODEN_POST,      /*pos*/ -5822, -2969,  5822, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvWoodenPost),
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,  /*pos*/  -886, -2559,  6655, /*angle*/ 0,  90, 0, /*behParam*/ (DIALOG_165 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,  /*pos*/ -2370,  -511,  2320, /*angle*/ 0,   0, 0, /*behParam*/ (DIALOG_166 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX,  /*pos*/  2600,  3500, -2400, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_STAR_2           << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_GOOMBA,           /*pos*/ -3180,  -511,  2080, /*angle*/ 0,   0, 0, /*behParam*/ (GOOMBA_SIZE_HUGE << 16), /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_FLYGUY,           /*pos*/  -300, -2340,  3940, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFlyGuy),
    OBJECT(/*model*/ MODEL_WOODEN_POST,      /*pos*/  -520, -2560,  6660, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvWoodenPost),
    OBJECT(/*model*/ MODEL_KOOPA_WITH_SHELL, /*pos*/  -800, -2236,  3080, /*angle*/ 0,   0, 0, /*behParam*/ (KOOPA_BP_NORMAL << 16), /*beh*/ bhvKoopa),
    OBJECT(/*model*/ MODEL_BUTTERFLY,        /*pos*/ -3111,  -511,  2400, /*angle*/ 0,   0, 0, /*behParam*/ (TRIPLET_BUTTERFLY_BP_0 << 16), /*beh*/ bhvTripletButterfly),
    OBJECT(/*model*/ MODEL_BUTTERFLY,        /*pos*/  4844,  -533,  2266, /*angle*/ 0,   0, 0, /*behParam*/ (TRIPLET_BUTTERFLY_BP_0 << 16), /*beh*/ bhvTripletButterfly),
    // Special objects
    OBJECT(/*model*/ MODEL_BOB_BUBBLY_TREE,  /*pos*/  4813,  -511,  2254, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    RETURN(),
};

static const LevelScript script_thi_area_2_macro_objects[] = {
    // Macro objects
    OBJECT(/*model*/ MODEL_BOBOMB_BUDDY,      /*pos*/  1902,  -767,  1318, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvBobombBuddyOpensCannon),
    OBJECT(/*model*/ MODEL_PURPLE_SWITCH,     /*pos*/ -1380,  -766, -1770, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFloorSwitchHiddenObjects),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/ -1140,  -970, -1920, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/ -1140,  -970, -1720, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -940,  -970, -1920, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -940,  -970, -1720, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -340,  -970, -2120, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -340,  -970, -1920, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -540,  -970, -1920, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -740,  -970, -1920, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -540,  -970, -1720, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -140,  -970, -2120, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/    40,  -970, -2520, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -740,  -970, -1720, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -140,  -970, -2320, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/    40,  -970, -2320, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/    40,  -970, -3120, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -140,  -970, -2720, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -140,  -970, -3320, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/    40,  -970, -3320, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -140,  -970, -2520, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/    40,  -970, -2720, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -140,  -970, -3120, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/    40,  -970, -2920, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -140,  -970, -2920, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/   -40,  -970, -3720, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/    40,  -970, -3520, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -140,  -970, -3520, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -140,  -970, -1920, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/    40,  -970, -2120, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/  -340,  -970, -1720, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_PIRANHA_PLANT,     /*pos*/ -1837,  -613, -1214, /*angle*/ 0,   0, 0, /*behParam*/ (FIRE_PIRANHA_PLANT_BP_NORMAL << 16), /*beh*/ bhvFirePiranhaPlant),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/    29,   666,   148, /*angle*/ 0,  90, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/   266,  -162,   829, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_GOOMBA,            /*pos*/  1881,  -778,  1614, /*angle*/ 0,   0, 0, /*behParam*/ (GOOMBA_SIZE_TINY << 16), /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_GOOMBA,            /*pos*/  1822,  -460, -1511, /*angle*/ 0,   0, 0, /*behParam*/ (GOOMBA_SIZE_TINY << 16), /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_GOOMBA,            /*pos*/  2148,  -460,  -918, /*angle*/ 0,   0, 0, /*behParam*/ (GOOMBA_SIZE_TINY << 16), /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/  -133,  -491, -1481, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/ -1466,   -70,  -814, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/ -1466,  -107,  -518, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/ -1466,  -151,  -162, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/   133,  -491, -1496, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX,   /*pos*/ -1866,  -400,   311, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_1UP_WALKING << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/  -380,  -480,   370, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/   -60,  -480,   370, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/ -1171,  -153,  1023, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_GOOMBA,            /*pos*/  -923,   295,  -614, /*angle*/ 0,   0, 0, /*behParam*/ (GOOMBA_SIZE_TINY << 16), /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_GOOMBA,            /*pos*/  1466,  -460,   999, /*angle*/ 0,   0, 0, /*behParam*/ (GOOMBA_SIZE_TINY << 16), /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX,   /*pos*/  1849,  -325,  -183, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_COINS_3     << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_GOOMBA,            /*pos*/  1033,  -162,   916, /*angle*/ 0,   0, 0, /*behParam*/ (GOOMBA_SIZE_TINY << 16), /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_GOOMBA,            /*pos*/  -550,   666,  -150, /*angle*/ 0,   0, 0, /*behParam*/ (GOOMBA_SIZE_TINY << 16), /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/   383, -1022,  2133, /*angle*/ 0,   0, 0, /*behParam*/ (FISH_SPAWNER_BP_FEW_BLUE << 16), /*beh*/ bhvFishSpawner),
    OBJECT(/*model*/ MODEL_GOOMBA,            /*pos*/  2133,  -767,   466, /*angle*/ 0,   0, 0, /*behParam*/ (GOOMBA_SIZE_TINY << 16), /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_GOOMBA,            /*pos*/ -1033,  -153,  1050, /*angle*/ 0,   0, 0, /*behParam*/ (GOOMBA_SIZE_TINY << 16), /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/  1980,  -880,  2100, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenStarTrigger),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/   140,  -153,   360, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenStarTrigger),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/ -1330,  -900,  1945, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenStarTrigger),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/     0,  1150,  -450, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenStarTrigger),
    OBJECT(/*model*/ MODEL_NONE,              /*pos*/ -1392,    92,  -633, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenStarTrigger),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,      /*pos*/  1348,  -148,   666, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,      /*pos*/  1733,  -770,  1718, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,      /*pos*/ -2133,  -770,   296, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,      /*pos*/  -160,  -608,   666, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    OBJECT(/*model*/ MODEL_BUTTERFLY,         /*pos*/ -1693,  -890,  1746, /*angle*/ 0,   0, 0, /*behParam*/ (TRIPLET_BUTTERFLY_BP_0 << 16), /*beh*/ bhvTripletButterfly),
    OBJECT(/*model*/ MODEL_KOOPA_WITH_SHELL,  /*pos*/  -600,  -153,   800, /*angle*/ 0,   0, 0, /*behParam*/ (KOOPA_BP_TINY << 16), /*beh*/ bhvKoopa),
    OBJECT(/*model*/ MODEL_BREAKABLE_BOX,     /*pos*/   -40,  -970, -3920, /*angle*/ 0,   0, 0, /*behParam*/ (BREAKABLE_BOX_BP_NO_COINS << 16), /*beh*/ bhvHiddenObject),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,       /*pos*/   280,  -640, -4140, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvYellowCoin),
    OBJECT(/*model*/ MODEL_GOOMBA,            /*pos*/ -2020,  -890,  1720, /*angle*/ 0,   0, 0, /*behParam*/ (GOOMBA_SIZE_TINY << 16), /*beh*/ bhvGoomba),
    // Special objects
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03, /*pos*/   -40,  -767, -4494, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_BOB_BUBBLY_TREE,   /*pos*/  1444,  -153,   676, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    RETURN(),
};

static const LevelScript script_thi_area_3_macro_objects[] = {
    // Macro objects
    OBJECT(/*model*/ MODEL_RED_COIN,         /*pos*/  1038,  1122,   998, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,         /*pos*/  1590,  1078,   160, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,         /*pos*/  1038,  1112,  -823, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,         /*pos*/  -238,  1122, -1319, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,         /*pos*/  -523,   824,  -500, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,         /*pos*/ -1838,  1068,   338, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,         /*pos*/  -240,  1500,  1040, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,         /*pos*/ -1914,  1360, -1909, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_1UP,              /*pos*/ -1920,  1540, -1040, /*angle*/ 0,   0, 0, /*behParam*/ (MUSHROOM_BP_REQUIRES_NONE << 16), /*beh*/ bhv1Up),
    OBJECT(/*model*/ MODEL_BLUE_COIN_SWITCH, /*pos*/ -1495,  1434,  1595, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvBlueCoinSwitch),
    OBJECT(/*model*/ MODEL_BLUE_COIN,        /*pos*/  -540,  1500,  1240, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenBlueCoin),
    OBJECT(/*model*/ MODEL_BLUE_COIN,        /*pos*/  -840,  1500,  1440, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenBlueCoin),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/ -1200,  2200, -1200, /*angle*/ 0,  45, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/  1200,  2200,  1200, /*angle*/ 0,  45, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,     /*pos*/  -224,  1456,   672, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    RETURN(),
};

static const LevelScript script_thi_area_2_objects_1[] = {
    OBJECT_WITH_ACTS(/*model*/ MODEL_NONE,    /*pos*/     0, -700, -4500, /*angle*/ 0, 0, 0, /*behParam*/ 0x03000000, /*beh*/ bhvHiddenStar,          /*acts*/ ALL_ACTS),
    RETURN(),
};

static const LevelScript script_thi_area_3_objects_1[] = {
    OBJECT_WITH_ACTS(/*model*/ MODEL_NONE,             /*pos*/ -1800,   800, -1500, /*angle*/ 0,   0, 0, /*behParam*/ 0x04000000, /*beh*/ bhvHiddenRedCoinStar, /*acts*/ ALL_ACTS),
    OBJECT(/*model*/ MODEL_WIGGLER_HEAD,     /*pos*/    17,  1843,   -62, /*angle*/ 0,   0, 0, /*behParam*/ 0x05000000, /*beh*/ bhvWigglerHead),
    RETURN(),
};

static const LevelScript script_thi_area_1_objects_1[] = {
    OBJECT_WITH_ACTS(/*model*/ MODEL_KOOPA_WITH_SHELL, /*pos*/ -1900,  -511,  2400, /*angle*/ 0, -30, 0, /*behParam*/ 0x02030000, /*beh*/ bhvKoopa,             /*acts*/ ACT_3),
    OBJECT_WITH_ACTS(/*model*/ MODEL_NONE,             /*pos*/  7400, -1537, -6300, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvKoopaRaceEndpoint, /*acts*/ ACT_3),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/ -6556, -2969,  6565, /*angle*/ 0,   0, 0, /*behParam*/ 0x00010000, /*beh*/ bhvGoombaTripletSpawner),
    OBJECT(/*model*/ MODEL_GOOMBA,           /*pos*/  6517, -2559,  4327, /*angle*/ 0,   0, 0, /*behParam*/ 0x00010000, /*beh*/ bhvGoomba),
    OBJECT(/*model*/ MODEL_PIRANHA_PLANT,    /*pos*/ -6336, -2047, -3861, /*angle*/ 0,   0, 0, /*behParam*/ 0x00010000, /*beh*/ bhvFirePiranhaPlant),
    OBJECT(/*model*/ MODEL_PIRANHA_PLANT,    /*pos*/ -5740, -2047, -6578, /*angle*/ 0,   0, 0, /*behParam*/ 0x00010000, /*beh*/ bhvFirePiranhaPlant),
    OBJECT(/*model*/ MODEL_PIRANHA_PLANT,    /*pos*/ -6481, -2047, -5998, /*angle*/ 0,   0, 0, /*behParam*/ 0x00010000, /*beh*/ bhvFirePiranhaPlant),
    OBJECT(/*model*/ MODEL_PIRANHA_PLANT,    /*pos*/ -5577, -2047, -4961, /*angle*/ 0,   0, 0, /*behParam*/ 0x00010000, /*beh*/ bhvFirePiranhaPlant),
    OBJECT(/*model*/ MODEL_PIRANHA_PLANT,    /*pos*/ -6865, -2047, -4568, /*angle*/ 0,   0, 0, /*behParam*/ 0x00010000, /*beh*/ bhvFirePiranhaPlant),
    OBJECT(/*model*/ MODEL_NONE,             /*pos*/ -4413,   204, -2140, /*angle*/ 0,   0, 0, /*behParam*/ 0x00030000, /*beh*/ bhvThiBowlingBallSpawner),
    OBJECT(/*model*/ MODEL_BUBBA,            /*pos*/ -6241, -3300,  -716, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvBubba),
    OBJECT(/*model*/ MODEL_BUBBA,            /*pos*/  1624, -3300,  8142, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvBubba),
    RETURN(),
};

static const LevelScript script_thi_area_1_objects_2[] = {
    OBJECT(/*model*/ MODEL_THI_HUGE_ISLAND_TOP, /*pos*/     0, 3891, -1533, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvThiHugeIslandTop),
    RETURN(),
};

static const LevelScript script_thi_area_2_objects_2[] = {
    OBJECT(/*model*/ MODEL_THI_TINY_ISLAND_TOP, /*pos*/     0, 1167,  -460, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvThiTinyIslandTop),
    OBJECT(/*model*/ MODEL_NONE,                /*pos*/ -1382,   80,  -649, /*angle*/ 0, 0, 0, /*behParam*/ 0x00040000, /*beh*/ bhvThiBowlingBallSpawner),
    RETURN(),
};

static const LevelScript script_thi_area_1_objects_3[] = {
    OBJECT(/*model*/ MODEL_THI_WARP_PIPE, /*pos*/  6656, -1536, -5632, /*angle*/ 0, 0, 0, /*behParam*/ 0x00320000, /*beh*/ bhvWarpPipe),
    OBJECT(/*model*/ MODEL_THI_WARP_PIPE, /*pos*/ -5888, -2048, -5888, /*angle*/ 0, 0, 0, /*behParam*/ 0x00330000, /*beh*/ bhvWarpPipe),
    OBJECT(/*model*/ MODEL_THI_WARP_PIPE, /*pos*/ -3072,   512, -3840, /*angle*/ 0, 0, 0, /*behParam*/ 0x00340000, /*beh*/ bhvWarpPipe),
    WARP_NODE(/*id*/ 0x32, /*destLevel*/ LEVEL_THI, /*destArea*/ 0x02, /*destNode*/ 0x32, /*flags*/ WARP_NO_CHECKPOINT),
    WARP_NODE(/*id*/ 0x33, /*destLevel*/ LEVEL_THI, /*destArea*/ 0x02, /*destNode*/ 0x33, /*flags*/ WARP_NO_CHECKPOINT),
    WARP_NODE(/*id*/ 0x34, /*destLevel*/ LEVEL_THI, /*destArea*/ 0x02, /*destNode*/ 0x34, /*flags*/ WARP_NO_CHECKPOINT),
    RETURN(),
};

static const LevelScript script_thi_area_2_objects_3[] = {
    OBJECT(/*model*/ MODEL_THI_WARP_PIPE, /*pos*/  1997, -461, -1690, /*angle*/ 0, 0, 0, /*behParam*/ 0x00320000, /*beh*/ bhvWarpPipe),
    OBJECT(/*model*/ MODEL_THI_WARP_PIPE, /*pos*/ -1766, -614, -1766, /*angle*/ 0, 0, 0, /*behParam*/ 0x00330000, /*beh*/ bhvWarpPipe),
    OBJECT(/*model*/ MODEL_THI_WARP_PIPE, /*pos*/  -922,  154, -1152, /*angle*/ 0, 0, 0, /*behParam*/ 0x00340000, /*beh*/ bhvWarpPipe),
    WARP_NODE(/*id*/ 0x32, /*destLevel*/ LEVEL_THI, /*destArea*/ 0x01, /*destNode*/ 0x32, /*flags*/ WARP_NO_CHECKPOINT),
    WARP_NODE(/*id*/ 0x33, /*destLevel*/ LEVEL_THI, /*destArea*/ 0x01, /*destNode*/ 0x33, /*flags*/ WARP_NO_CHECKPOINT),
    WARP_NODE(/*id*/ 0x34, /*destLevel*/ LEVEL_THI, /*destArea*/ 0x01, /*destNode*/ 0x34, /*flags*/ WARP_NO_CHECKPOINT),
    RETURN(),
};

const LevelScript level_thi_entry[] = {
    INIT_LEVEL(),
    LOAD_YAY0(        /*seg*/ 0x07, _thi_segment_7SegmentRomStart, _thi_segment_7SegmentRomEnd),
    LOAD_YAY0_TEXTURE(/*seg*/ 0x09, _grass_yay0SegmentRomStart, _grass_yay0SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x0A, _water_skybox_yay0SegmentRomStart, _water_skybox_yay0SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x05, _group11_yay0SegmentRomStart, _group11_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0C, _group11_geoSegmentRomStart,  _group11_geoSegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x06, _group14_yay0SegmentRomStart, _group14_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0D, _group14_geoSegmentRomStart,  _group14_geoSegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x08, _common0_yay0SegmentRomStart, _common0_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0F, _common0_geoSegmentRomStart,  _common0_geoSegmentRomEnd),
    ALLOC_LEVEL_POOL(),
    MARIO(/*model*/ MODEL_MARIO, /*behParam*/ 0x00000001, /*beh*/ bhvMario),
    JUMP_LINK(script_func_global_1),
    JUMP_LINK(script_func_global_12),
    JUMP_LINK(script_func_global_15),
    LOAD_MODEL_FROM_GEO(MODEL_THI_BUBBLY_TREE,     bubbly_tree_geo),
    LOAD_MODEL_FROM_GEO(MODEL_LEVEL_GEOMETRY_03,   thi_geo_0005F0),
    LOAD_MODEL_FROM_GEO(MODEL_THI_WARP_PIPE,       warp_pipe_geo),
    LOAD_MODEL_FROM_GEO(MODEL_THI_HUGE_ISLAND_TOP, thi_geo_0005B0),
    LOAD_MODEL_FROM_GEO(MODEL_THI_TINY_ISLAND_TOP, thi_geo_0005C8),

    AREA(/*index*/ 1, thi_geo_000608),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ -7372, -1969,  7373, /*angle*/ 0, 149, 0, /*behParam*/ 0x000A0000, /*beh*/ bhvSpinAirborneWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/   410,  -512,   922, /*angle*/ 0,   0, 0, /*behParam*/ 0x000B0000, /*beh*/ bhvInstantActiveWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/   410,  -512,   717, /*angle*/ 0,   0, 0, /*behParam*/ 0x050C0000, /*beh*/ bhvWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/     0,  3170, -1570, /*angle*/ 0,   0, 0, /*behParam*/ 0x0A0D0000, /*beh*/ bhvWarp),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_THI, /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0B, /*destLevel*/ LEVEL_THI, /*destArea*/ 0x01, /*destNode*/ 0x0B, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0C, /*destLevel*/ LEVEL_THI, /*destArea*/ 0x03, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0D, /*destLevel*/ LEVEL_THI, /*destArea*/ 0x03, /*destNode*/ 0x0B, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF0, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x02, /*destNode*/ 0x37, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x02, /*destNode*/ 0x69, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_thi_area_1_objects_1),
        JUMP_LINK(script_thi_area_1_objects_2),
        JUMP_LINK(script_thi_area_1_objects_3),
        TERRAIN(/*terrainData*/ thi_seg7_area_1_collision),
        JUMP_LINK(script_thi_area_1_macro_objects),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0000, /*seq*/ SEQ_LEVEL_GRASS),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_GRASS),
    END_AREA(),

    AREA(/*index*/ 2, thi_geo_0006D4),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ -2211,  110,  2212, /*angle*/ 0,  149, 0, /*behParam*/ 0x000A0000, /*beh*/ bhvSpinAirborneWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/   280, -767, -4180, /*angle*/ 0,    0, 0, /*behParam*/ 0x000B0000, /*beh*/ bhvFadingWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ -1638,    0, -1988, /*angle*/ 0, -126, 0, /*behParam*/ 0x000C0000, /*beh*/ bhvFadingWarp),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_THI, /*destArea*/ 0x02, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0B, /*destLevel*/ LEVEL_THI, /*destArea*/ 0x02, /*destNode*/ 0x0C, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0C, /*destLevel*/ LEVEL_THI, /*destArea*/ 0x02, /*destNode*/ 0x0B, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF0, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x02, /*destNode*/ 0x33, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x02, /*destNode*/ 0x65, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_thi_area_2_objects_1),
        JUMP_LINK(script_thi_area_2_objects_2),
        JUMP_LINK(script_thi_area_2_objects_3),
        TERRAIN(/*terrainData*/ thi_seg7_area_2_collision),
        JUMP_LINK(script_thi_area_2_macro_objects),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0000, /*seq*/ SEQ_LEVEL_GRASS),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_GRASS),
    END_AREA(),

    AREA(/*index*/ 3, thi_geo_00079C),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ 512, 1024, 2150, /*angle*/ 0, 180, 0, /*behParam*/ 0x000A0000, /*beh*/ bhvInstantActiveWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/   0, 3277,    0, /*angle*/ 0,   0, 0, /*behParam*/ 0x000B0000, /*beh*/ bhvAirborneWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ 512, 1024, 2355, /*angle*/ 0,   0, 0, /*behParam*/ 0x050C0000, /*beh*/ bhvWarp),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_THI, /*destArea*/ 0x03, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0B, /*destLevel*/ LEVEL_THI, /*destArea*/ 0x03, /*destNode*/ 0x0B, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0C, /*destLevel*/ LEVEL_THI, /*destArea*/ 0x01, /*destNode*/ 0x0B, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF0, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x02, /*destNode*/ 0x37, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x02, /*destNode*/ 0x69, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_thi_area_3_objects_1),
        TERRAIN(/*terrainData*/ thi_seg7_area_3_collision),
        JUMP_LINK(script_thi_area_3_macro_objects),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0004, /*seq*/ SEQ_LEVEL_UNDERGROUND),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_GRASS),
    END_AREA(),

    FREE_LEVEL_POOL(),
    MARIO_POS(/*area*/ 1, /*yaw*/ 149, /*pos*/ -7372, -2969, 7373),
    CALL(/*arg*/ 0, /*func*/ lvl_init_or_update),
    CALL_LOOP(/*arg*/ 1, /*func*/ lvl_init_or_update),
    CLEAR_LEVEL(),
    SLEEP_BEFORE_EXIT(/*frames*/ 1),
    EXIT(),
};
