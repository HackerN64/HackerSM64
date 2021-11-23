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
#include "levels/sa/header.h"

static const LevelScript script_func_local_1[] = {
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ 0, -1000, 0, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x10, 0x00, 0x00), /*beh*/ bhvFishSpawner),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ 0, -1000, 0, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x12, 0x00, 0x00), /*beh*/ bhvFishSpawner),
    RETURN(),
};

static const LevelScript script_func_local_2[] = {
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ 0, -4250, 0, /*angle*/ 0, 0, 0, /*behParam*/ BP(0x00, 0x00, 0x00, 0x00), /*beh*/ bhvHiddenRedCoinStar),
    RETURN(),
};

const LevelScript level_sa_entry[] = {
    INIT_LEVEL(),
    LOAD_LEVEL_DATA(sa),
    LOAD_TEXTURE_BIN(inside),
    LOAD_SKYBOX(cloud_floor),
    LOAD_EFFECTS(),
    LOAD_GROUPA(group4),
    LOAD_GROUPB(group13),
    ALLOC_LEVEL_POOL(),
    MARIO(/*model*/ MODEL_MARIO, /*behParam*/ BP(0x00, 0x00, 0x00, 0x01), /*beh*/ bhvMario),
    LOAD_ACTOR_MODELS(group4),
    LOAD_ACTOR_MODELS(group13),

    AREA(/*index*/ 1, sa_geo_000170),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ 0, -1535, 0, /*angle*/ 0, 90, 0, /*behParam*/ BP(0x00, 0x0A, 0x00, 0x00), /*beh*/ bhvSwimmingWarp),
        WARP_NODE(/*id*/ 0x0A,              /*destLevel*/ LEVEL_SA,     /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ WARP_NODE_DEFAULT, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x01, /*destNode*/ 0x27, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ WARP_NODE_DEATH,   /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x01, /*destNode*/ 0x28, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_func_local_1),
        JUMP_LINK(script_func_local_2),
        TERRAIN(/*terrainData*/ sa_seg7_collision),
        MACRO_OBJECTS(/*objList*/ sa_seg7_macro_objs),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0003, /*seq*/ (SEQ_LEVEL_WATER | SEQ_VARIATION)),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_WATER),
    END_AREA(),

    FREE_LEVEL_POOL(),
    MARIO_POS(/*area*/ 1, /*yaw*/ 90, /*pos*/ 0, -1535, 0),
    CALL(     /*arg*/ 0, /*func*/ lvl_init_or_update),
    CALL_LOOP(/*arg*/ 1, /*func*/ lvl_init_or_update),
    CLEAR_LEVEL(),
    SLEEP_BEFORE_EXIT(/*frames*/ 1),
    EXIT(),
};
