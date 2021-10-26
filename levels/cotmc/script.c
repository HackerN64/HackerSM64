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

static const LevelScript script_func_local_1[] = {
    OBJECT(/*model*/ MODEL_CAP_SWITCH, /*pos*/ 0,  363, -6144, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x01, 0x00, 0x00), /*beh*/ bhvCapSwitch),
    OBJECT(/*model*/ MODEL_NONE,       /*pos*/ 0,  500, -7373, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvWaterfallSoundLoop),
    OBJECT(/*model*/ MODEL_NONE,       /*pos*/ 0,  500,  3584, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvWaterfallSoundLoop),
    RETURN(),
};

static const LevelScript script_func_local_2[] = {
    OBJECT(/*model*/ MODEL_NONE,       /*pos*/ 0, -200, -7000, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvHiddenRedCoinStar),
    RETURN(),
};

const LevelScript level_cotmc_entry[] = {
    INIT_LEVEL(),
    LOAD_LEVEL_DATA(cotmc),
    LOAD_TEXTURE_BIN(cave),
    LOAD_GROUPA(group8),
    LOAD_GROUPB(group17),
    LOAD_COMMON0(),
    ALLOC_LEVEL_POOL(),
    MARIO(/*model*/ MODEL_MARIO, /*behParam*/ BP(0x00, 0x00, 0x00, 0x01), /*beh*/ bhvMario),
    LOAD_ACTOR_MODELS(group8),
    LOAD_ACTOR_MODELS(group17),
    LOAD_ACTOR_MODELS(common0),

    AREA(/*index*/ 1, cotmc_geo_0001A0),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ -4185, 1020, -47, /*angle*/ 0, 90, 0, /*behParam*/ BP(0x00, 0x0A, 0x00, 0x00), /*beh*/ bhvAirborneWarp),
        WARP_NODE(/*id*/ 0x0A,                 /*destLevel*/ LEVEL_COTMC,          /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ WARP_NODE_DEFAULT,    /*destLevel*/ LEVEL_CASTLE,         /*destArea*/ 0x03, /*destNode*/ 0x34, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ WARP_NODE_DEATH,      /*destLevel*/ LEVEL_CASTLE,         /*destArea*/ 0x03, /*destNode*/ 0x66, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ WARP_NODE_WARP_FLOOR, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x14, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_func_local_2),
        JUMP_LINK(script_func_local_1),
        TERRAIN(/*terrainData*/ cotmc_seg7_collision_level),
        MACRO_OBJECTS(/*objList*/ cotmc_seg7_macro_objs),
        SHOW_DIALOG(/*index*/ 0x00, DIALOG_130),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0004, /*seq*/ SEQ_LEVEL_UNDERGROUND),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_STONE),
    END_AREA(),

    FREE_LEVEL_POOL(),
    MARIO_POS(/*area*/ 1, /*yaw*/ 90, /*pos*/ -4185, 20, -47),
    CALL(     /*arg*/ 0, /*func*/ lvl_init_or_update),
    CALL_LOOP(/*arg*/ 1, /*func*/ lvl_init_or_update),
    CLEAR_LEVEL(),
    SLEEP_BEFORE_EXIT(/*frames*/ 1),
    EXIT(),
};
