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
#include "levels/castle_courtyard/header.h"


static const LevelScript script_castle_courtyard_macro_objects[] = {
    // Macro objects
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,      /*pos*/  3180,    20,   330, /*angle*/ 0, 225, 0, /*behParam*/ (DIALOG_158 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,      /*pos*/ -3180,    20,   330, /*angle*/ 0, 135, 0, /*behParam*/ (DIALOG_159 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,      /*pos*/   300,     0, -3600, /*angle*/ 0,   0, 0, /*behParam*/ (DIALOG_102 << 16), /*beh*/ bhvMessagePanel),
    OBJECT(/*model*/ MODEL_WOODEN_SIGNPOST,      /*pos*/  -300,     0, -3600, /*angle*/ 0,   0, 0, /*behParam*/ (DIALOG_160 << 16), /*beh*/ bhvMessagePanel),
    // Special objects
    OBJECT(/*model*/ MODEL_LEVEL_GEOMETRY_03,    /*pos*/     0,  2048,  2662, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvStaticObject),
    OBJECT(/*model*/ MODEL_COURTYARD_SPIKY_TREE, /*pos*/  2272,  -214, -1432, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_COURTYARD_SPIKY_TREE, /*pos*/   818,    10,   203, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_COURTYARD_SPIKY_TREE, /*pos*/  -820,    10,   201, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_COURTYARD_SPIKY_TREE, /*pos*/  1681,  -214,  -132, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_COURTYARD_SPIKY_TREE, /*pos*/  2382,  -214,  -843, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_COURTYARD_SPIKY_TREE, /*pos*/  -817,    10, -3630, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_COURTYARD_SPIKY_TREE, /*pos*/  2769,  -214, -1523, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_COURTYARD_SPIKY_TREE, /*pos*/  2444,  -214, -2330, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_COURTYARD_SPIKY_TREE, /*pos*/  2042,  -214, -3032, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_COURTYARD_SPIKY_TREE, /*pos*/   824,    10, -3633, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_COURTYARD_SPIKY_TREE, /*pos*/ -2537,  -214,  -759, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_COURTYARD_SPIKY_TREE, /*pos*/ -1640,  -214, -3228, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_COURTYARD_SPIKY_TREE, /*pos*/ -2732,  -214, -2166, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_COURTYARD_SPIKY_TREE, /*pos*/ -2446,  -214, -1786, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_COURTYARD_SPIKY_TREE, /*pos*/ -2820,  -214, -1317, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_COURTYARD_SPIKY_TREE, /*pos*/ -1868,  -214,   -45, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvTree),
    OBJECT(/*model*/ MODEL_CASTLE_WOODEN_DOOR,   /*pos*/     0,     0,   461, /*angle*/ 0,   0, 0, /*behParam*/  (1 << 16), /*beh*/ bhvDoorWarp),
    RETURN(),
};

static const LevelScript script_castle_courtyard_objects_1[] = {
    OBJECT(/*model*/ MODEL_NONE, /*pos*/     0, 200, -1652, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvAmbientSounds),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/ -2700,   0, -1652, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvBirdsSoundLoop),
    OBJECT(/*model*/ MODEL_NONE, /*pos*/  2700,   0, -1652, /*angle*/ 0, 0, 0, /*behParam*/ 0x00010000, /*beh*/ bhvBirdsSoundLoop),
    RETURN(),
};

static const LevelScript script_castle_courtyard_objects_2[] = {
    OBJECT(/*model*/ MODEL_BOO, /*pos*/ -3217, 100,  -101, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvCourtyardBooTriplet),
    OBJECT(/*model*/ MODEL_BOO, /*pos*/  3317, 100, -1701, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvCourtyardBooTriplet),
    OBJECT(/*model*/ MODEL_BOO, /*pos*/   -71,   1, -1387, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvCourtyardBooTriplet),
    RETURN(),
};

const LevelScript level_castle_courtyard_entry[] = {
    INIT_LEVEL(),
    LOAD_YAY0(        /*seg*/ 0x07, _castle_courtyard_segment_7SegmentRomStart, _castle_courtyard_segment_7SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x0A, _water_skybox_yay0SegmentRomStart, _water_skybox_yay0SegmentRomEnd),
    LOAD_YAY0_TEXTURE(/*seg*/ 0x09, _outside_yay0SegmentRomStart, _outside_yay0SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x05, _group9_yay0SegmentRomStart, _group9_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0C, _group9_geoSegmentRomStart,  _group9_geoSegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x08, _common0_yay0SegmentRomStart, _common0_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0F, _common0_geoSegmentRomStart,  _common0_geoSegmentRomEnd),
    ALLOC_LEVEL_POOL(),
    MARIO(/*model*/ MODEL_MARIO, /*behParam*/ 0x00000001, /*beh*/ bhvMario),
    JUMP_LINK(script_func_global_1),
    JUMP_LINK(script_func_global_10),
    LOAD_MODEL_FROM_GEO(MODEL_COURTYARD_SPIKY_TREE,  spiky_tree_geo),
    LOAD_MODEL_FROM_GEO(MODEL_COURTYARD_WOODEN_DOOR, wooden_door_geo),
    LOAD_MODEL_FROM_GEO(MODEL_LEVEL_GEOMETRY_03,     castle_courtyard_geo_000200),

    AREA(/*index*/ 1, castle_courtyard_geo_000218),
        OBJECT(/*model*/ MODEL_BOO,  /*pos*/ -2360, -100, -2712, /*angle*/ 0,   0, 0, /*behParam*/ 0x01050000, /*beh*/ bhvBooWithCage),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/     0,   51, -1000, /*angle*/ 0, 180, 0, /*behParam*/ 0x000A0000, /*beh*/ bhvLaunchStarCollectWarp),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/     0,   51, -1000, /*angle*/ 0, 180, 0, /*behParam*/ 0x000B0000, /*beh*/ bhvLaunchDeathWarp),
        WARP_NODE(/*id*/ 0x05, /*destLevel*/ LEVEL_BBH, /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_CASTLE_COURTYARD, /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x0B, /*destLevel*/ LEVEL_CASTLE_COURTYARD, /*destArea*/ 0x01, /*destNode*/ 0x0B, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0x01, /*destLevel*/ LEVEL_CASTLE, /*destArea*/ 0x01, /*destNode*/ 0x02, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x03, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_castle_courtyard_objects_1),
        JUMP_LINK(script_castle_courtyard_objects_2),
        TERRAIN(/*terrainData*/ castle_courtyard_seg7_collision),
        JUMP_LINK(script_castle_courtyard_macro_objects),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0000, /*seq*/ SEQ_SOUND_PLAYER),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_STONE),
    END_AREA(),

    FREE_LEVEL_POOL(),
    MARIO_POS(/*area*/ 1, /*yaw*/ 0, /*pos*/ -14, 0, -201),
    CALL(/*arg*/ 0, /*func*/ lvl_init_or_update),
    CALL_LOOP(/*arg*/ 1, /*func*/ lvl_init_or_update),
    CLEAR_LEVEL(),
    SLEEP_BEFORE_EXIT(/*frames*/ 1),
    EXIT(),
};
