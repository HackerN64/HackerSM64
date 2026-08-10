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
#include "levels/cotmc/header.h"


static const LevelScript script_cotmc_macro_objects[] = {
    // Macro objects
    OBJECT(/*model*/ MODEL_SNUFIT,          /*pos*/ -2920,   220,   -20, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSnufit),
    OBJECT(/*model*/ MODEL_SNUFIT,          /*pos*/ -1380,   240,   740, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSnufit),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX, /*pos*/  -360,   300,  -200, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_METAL_CAP        << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_SNUFIT,          /*pos*/   360,   200, -1120, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSnufit),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/   400,   256, -4300, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX, /*pos*/   300,   620, -5280, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_METAL_CAP        << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_SNUFIT,          /*pos*/  -340,   260, -2620, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvSnufit),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/     0,  -450, -7000, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_RING) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_1UP,             /*pos*/   900,   260, -3620, /*angle*/ 0,   0, 0, /*behParam*/ (MUSHROOM_BP_REQUIRES_NONE << 16), /*beh*/ bhv1Up),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/     0,  -170, -1660, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/   -20,  -211, -3940, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST, /*pos*/   -71,    20,   720, /*angle*/ 0, 270, 0, /*behParam*/ (DIALOG_123 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/   200,  -291, -5600, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/   980,   260, -3430, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/  -540,  -352, -5940, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/  -300,   450, -6240, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/  -200,  -400, -6680, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/   250,   450, -6400, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/   540,  -361, -6340, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/   980,   260, -3810, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX, /*pos*/   -20,   180,  2060, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_1UP_RUNNING_AWAY << 16), /*beh*/ bhvExclamationBox),
    RETURN(),
};

static const LevelScript script_cotmc_objects_1[] = {
    OBJECT(/*model*/ MODEL_CAP_SWITCH, /*pos*/ 0,  363, -6144, /*angle*/ 0, 0, 0, /*behParam*/ 0x00010000, /*beh*/ bhvCapSwitch),
    OBJECT(/*model*/ MODEL_NONE,       /*pos*/ 0,  500, -7373, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvWaterfallSoundLoop),
    OBJECT(/*model*/ MODEL_NONE,       /*pos*/ 0,  500,  3584, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvWaterfallSoundLoop),
    RETURN(),
};

static const LevelScript script_cotmc_objects_2[] = {
    OBJECT(/*model*/ MODEL_NONE,       /*pos*/ 0, -200, -7000, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenRedCoinStar),
    RETURN(),
};

const LevelScript level_cotmc_entry[] = {
    INIT_LEVEL(),
#include "levels/cotmc/areas/script_vanilla_load.inc.c"
    ALLOC_LEVEL_POOL(),
    MARIO(/*model*/ MODEL_MARIO, /*behParam*/ 0x00000001, /*beh*/ bhvMario),
#include "levels/cotmc/areas/script_vanilla.inc.c"

    AREA(/*index*/ 1, cotmc_geo_0001A0),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ -4185, 1020, -47, /*angle*/ 0, 90, 0, /*behParam*/ 0x000A0000, /*beh*/ bhvAirborneWarp),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_COTMC, /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF0, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x03, /*destNode*/ 0x34, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x03, /*destNode*/ 0x66, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF3, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x14, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_cotmc_objects_1),
        JUMP_LINK(script_cotmc_objects_2),
        TERRAIN(/*terrainData*/ cotmc_seg7_collision_level),
        JUMP_LINK(script_cotmc_macro_objects),
        SHOW_DIALOG(/*index*/ 0x00, DIALOG_130),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0004, /*seq*/ SEQ_LEVEL_UNDERGROUND),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_STONE),
    END_AREA(),

    FREE_LEVEL_POOL(),
    MARIO_POS(/*area*/ 1, /*yaw*/ 90, /*pos*/ -4185, 20, -47),
    CALL(/*arg*/ 0, /*func*/ lvl_init_or_update),
    CALL_LOOP(/*arg*/ 1, /*func*/ lvl_init_or_update),
    CLEAR_LEVEL(),
    SLEEP_BEFORE_EXIT(/*frames*/ 1),
    EXIT(),
};
