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


static const LevelScript script_sa_macro_objects[] = {
    // Macro objects
    OBJECT(/*model*/ MODEL_RED_COIN, /*pos*/ -2400, -4607, -2400, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN, /*pos*/ -2400, -4607,  2400, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN, /*pos*/  2400, -4607,  2400, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN, /*pos*/  2400, -4607, -2400, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN, /*pos*/     0, -2200, -1500, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN, /*pos*/ -1500, -2200,     0, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN, /*pos*/     0, -2200,  1500, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN, /*pos*/  1500, -2200,     0, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_NONE,     /*pos*/  1927, -2909,     0, /*angle*/ 0,   0, 0, /*behParam*/ (FISH_SPAWNER_BP_FEW_BLUE << 16), /*beh*/ bhvFishSpawner),
    OBJECT(/*model*/ MODEL_NONE,     /*pos*/     0, -2400, -1500, /*angle*/ 0,  90, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING | COIN_FORMATION_FLAG_VERTICAL) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,     /*pos*/ -1500, -2400,     0, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING | COIN_FORMATION_FLAG_VERTICAL) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,     /*pos*/     0, -2400,  1500, /*angle*/ 0,  90, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING | COIN_FORMATION_FLAG_VERTICAL) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,     /*pos*/  1500, -2400,     0, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING | COIN_FORMATION_FLAG_VERTICAL) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,     /*pos*/     0, -3500,     0, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHidden1upTrigger),
    OBJECT(/*model*/ MODEL_1UP,      /*pos*/     0, -3800,     0, /*angle*/ 0,   0, 0, /*behParam*/ (1 << 16),  /*beh*/ bhvHidden1up),
    OBJECT(/*model*/ MODEL_NONE,     /*pos*/     0, -3500,     0, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_NONE,     /*pos*/ -1000, -4080, -1740, /*angle*/ 0,   0, 0, /*behParam*/ (FISH_SPAWNER_BP_FEW_CYAN << 16), /*beh*/ bhvFishSpawner),
    RETURN(),
};

static const LevelScript script_sa_objects_1[] = {
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ 0, -1000, 0, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFishSpawner),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ 0, -1000, 0, /*angle*/ 0, 0, 0, /*behParam*/ 0x00020000, /*beh*/ bhvFishSpawner),
    RETURN(),
};

static const LevelScript script_sa_objects_2[] = {
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ 0, -4250, 0, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenRedCoinStar),
    RETURN(),
};

const LevelScript level_sa_entry[] = {
    INIT_LEVEL(),
    LOAD_YAY0(        /*seg*/ 0x07, _sa_segment_7SegmentRomStart, _sa_segment_7SegmentRomEnd),
    LOAD_YAY0_TEXTURE(/*seg*/ 0x09, _inside_yay0SegmentRomStart, _inside_yay0SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x0A, _cloud_floor_skybox_yay0SegmentRomStart, _cloud_floor_skybox_yay0SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x0B, _effect_yay0SegmentRomStart, _effect_yay0SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x05, _group4_yay0SegmentRomStart, _group4_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0C, _group4_geoSegmentRomStart,  _group4_geoSegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x06, _group13_yay0SegmentRomStart, _group13_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0D, _group13_geoSegmentRomStart,  _group13_geoSegmentRomEnd),
    ALLOC_LEVEL_POOL(),
    MARIO(/*model*/ MODEL_MARIO, /*behParam*/ 0x00000001, /*beh*/ bhvMario),
    JUMP_LINK(script_func_global_5),
    JUMP_LINK(script_func_global_14),

    AREA(/*index*/ 1, sa_geo_000170),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ 0, -1535, 0, /*angle*/ 0, 90, 0, /*behParam*/ 0x000A0000, /*beh*/ bhvSwimmingWarp),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_SA, /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF0, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x01, /*destNode*/ 0x27, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x01, /*destNode*/ 0x28, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_sa_objects_1),
        JUMP_LINK(script_sa_objects_2),
        TERRAIN(/*terrainData*/ sa_seg7_collision),
        JUMP_LINK(script_sa_macro_objects),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0003, /*seq*/ (SEQ_LEVEL_WATER | SEQ_VARIATION)),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_WATER),
    END_AREA(),

    FREE_LEVEL_POOL(),
    MARIO_POS(/*area*/ 1, /*yaw*/ 90, /*pos*/ 0, -1535, 0),
    CALL(/*arg*/ 0, /*func*/ lvl_init_or_update),
    CALL_LOOP(/*arg*/ 1, /*func*/ lvl_init_or_update),
    CLEAR_LEVEL(),
    SLEEP_BEFORE_EXIT(/*frames*/ 1),
    EXIT(),
};
