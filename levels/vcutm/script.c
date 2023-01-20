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
#include "levels/vcutm/header.h"


static const LevelScript script_vcutm_macro_objects[] = {
    // Macro objects
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX, /*pos*/ -3434,  2951, -3076, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_1UP_WALKING << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_1UP,             /*pos*/ -4711,  1594, -2532, /*angle*/ 0,   0, 0, /*behParam*/ (MUSHROOM_BP_REQUIRES_NONE << 16), /*beh*/ bhv1Up),
    OBJECT(/*model*/ MODEL_1UP,             /*pos*/ -5952,  -393, -1141, /*angle*/ 0,   0, 0, /*behParam*/ (MUSHROOM_BP_REQUIRES_NONE << 16), /*beh*/ bhv1Up),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX, /*pos*/  3980,   300, -6220, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_VANISH_CAP  << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/  4200,     0, -4940, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHidden1upTrigger),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/  4740,     0, -4940, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHidden1upTrigger),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/  4740,     0, -4400, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHidden1upTrigger),
    OBJECT(/*model*/ MODEL_1UP,             /*pos*/  4460,     0, -4700, /*angle*/ 0,   0, 0, /*behParam*/ (3 << 16), /*beh*/ bhvHidden1up),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,     /*pos*/  4200,     0, -4940, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,     /*pos*/  4740,     0, -4940, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_YELLOW_COIN,     /*pos*/  4740,     0, -4400, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvOneCoin),
    OBJECT(/*model*/ MODEL_NONE,            /*pos*/ -2100, -3276,    20, /*angle*/ 0,   0, 0, /*behParam*/ ((COIN_FORMATION_FLAG_NONE) << 16), /*beh*/ bhvCoinFormation),
    OBJECT(/*model*/ MODEL_AMP,             /*pos*/ -2100, -2800, -3866, /*angle*/ 0,   0, 0, /*behParam*/ (1 << 16), /*beh*/ bhvCirclingAmp),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX, /*pos*/ -2145, -2160, -5963, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_COINS_3     << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/ -5920,  4575, -4618, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/ -4740,  3581, -3854, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/ -4727,  -227, -1161, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/ -3418,  -559,  -836, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/  -600, -1400, -6300, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/   900, -1400, -6300, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/  2750,  -800, -6300, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_RED_COIN,        /*pos*/  4509,     0, -5527, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvRedCoin),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,    /*pos*/ -1740, -3240,  1580, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,    /*pos*/ -2400, -3240,   600, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,    /*pos*/ -1740, -3240,     0, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,    /*pos*/ -2400, -3240, -1079, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,    /*pos*/ -2920, -3240,   880, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,    /*pos*/ -4000, -3240,  1600, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    OBJECT(/*model*/ MODEL_EXCLAMATION_BOX, /*pos*/ -6020, -2976,  1240, /*angle*/ 0,   0, 0, /*behParam*/ (EXCLAMATION_BOX_BP_VANISH_CAP  << 16), /*beh*/ bhvExclamationBox),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,    /*pos*/ -2100, -2840, -3866, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    OBJECT(/*model*/ MODEL_BOWLING_BALL,    /*pos*/ -4600,  5760, -6140, /*angle*/ 0,   0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvFireSpitter),
    RETURN(),
};

static const LevelScript script_vcutm_objects_1[] = {
    OBJECT(/*model*/ MODEL_VCUTM_SEESAW_PLATFORM, /*pos*/   154, -1919, -6256, /*angle*/ 0, 270, 0, /*behParam*/ 0x00070000, /*beh*/ bhvSeesawPlatform),
    OBJECT(/*model*/ MODEL_VCUTM_SEESAW_PLATFORM, /*pos*/ -2047, -3378, -2047, /*angle*/ 0,   0, 0, /*behParam*/ 0x00070000, /*beh*/ bhvSeesawPlatform),
    OBJECT(/*model*/ MODEL_NONE,           /*pos*/  3251, -1082, -6256, /*angle*/ 0, 270, 0, /*behParam*/ 0x014B0000, /*beh*/ bhvCheckerboardElevatorGroup),
    OBJECT(/*model*/ MODEL_NONE,           /*pos*/  2355, -1901, -6256, /*angle*/ 0, 270, 0, /*behParam*/ 0x014B0000, /*beh*/ bhvCheckerboardElevatorGroup),
    OBJECT(/*model*/ MODEL_NONE,           /*pos*/  1459, -1594, -6256, /*angle*/ 0,  90, 0, /*behParam*/ 0x014B0000, /*beh*/ bhvCheckerboardElevatorGroup),
    OBJECT(/*model*/ MODEL_NONE,           /*pos*/ -1151, -2413, -6256, /*angle*/ 0, 270, 0, /*behParam*/ 0x014B0000, /*beh*/ bhvCheckerboardElevatorGroup),
    RETURN(),
};

static const LevelScript script_vcutm_objects_2[] = {
    OBJECT(/*model*/ MODEL_CAP_SWITCH, /*pos*/ 4506,  26, -6246, /*angle*/ 0, 0, 0, /*behParam*/ 0x00020000, /*beh*/ bhvCapSwitch),
    RETURN(),
};

static const LevelScript script_vcutm_objects_3[] = {
    OBJECT(/*model*/ MODEL_NONE,       /*pos*/ 4600, 250, -4500, /*angle*/ 0, 0, 0, /*behParam*/ 0x00000000, /*beh*/ bhvHiddenRedCoinStar),
    RETURN(),
};

const LevelScript level_vcutm_entry[] = {
    INIT_LEVEL(),
    LOAD_YAY0_TEXTURE(/*seg*/ 0x09, _outside_yay0SegmentRomStart, _outside_yay0SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x07, _vcutm_segment_7SegmentRomStart, _vcutm_segment_7SegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x05, _group8_yay0SegmentRomStart, _group8_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0C, _group8_geoSegmentRomStart,  _group8_geoSegmentRomEnd),
    LOAD_YAY0(        /*seg*/ 0x08, _common0_yay0SegmentRomStart, _common0_yay0SegmentRomEnd),
    LOAD_RAW(         /*seg*/ 0x0F, _common0_geoSegmentRomStart,  _common0_geoSegmentRomEnd),
    ALLOC_LEVEL_POOL(),
    MARIO(/*model*/ MODEL_MARIO, /*behParam*/ 0x00000001, /*beh*/ bhvMario),
    JUMP_LINK(script_func_global_1),
    JUMP_LINK(script_func_global_9),
    LOAD_MODEL_FROM_GEO(MODEL_VCUTM_SEESAW_PLATFORM, vcutm_geo_0001F0),
    LOAD_MODEL_FROM_GEO(MODEL_VCUTM_WARP_PIPE,       warp_pipe_geo),

    AREA(/*index*/ 1, vcutm_geo_000208),
        OBJECT(/*model*/ MODEL_NONE, /*pos*/ -6143, 6734, -6143, /*angle*/ 0, 0, 0, /*behParam*/ 0x000A0000, /*beh*/ bhvAirborneWarp),
        WARP_NODE(/*id*/ 0x0A, /*destLevel*/ LEVEL_VCUTM, /*destArea*/ 0x01, /*destNode*/ 0x0A, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF3, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x07, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF0, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x08, /*flags*/ WARP_NO_CHECKPOINT),
        WARP_NODE(/*id*/ 0xF1, /*destLevel*/ LEVEL_CASTLE_GROUNDS, /*destArea*/ 0x01, /*destNode*/ 0x06, /*flags*/ WARP_NO_CHECKPOINT),
        JUMP_LINK(script_vcutm_objects_1),
        JUMP_LINK(script_vcutm_objects_2),
        JUMP_LINK(script_vcutm_objects_3),
        TERRAIN(/*terrainData*/ vcutm_seg7_collision),
        JUMP_LINK(script_vcutm_macro_objects),
        SHOW_DIALOG(/*index*/ 0x00, DIALOG_129),
        SET_BACKGROUND_MUSIC(/*settingsPreset*/ 0x0000, /*seq*/ SEQ_LEVEL_SLIDE),
        TERRAIN_TYPE(/*terrainType*/ TERRAIN_STONE),
    END_AREA(),

    FREE_LEVEL_POOL(),
    MARIO_POS(/*area*/ 1, /*yaw*/ 0, /*pos*/ -6143, 5734, -6143),
    CALL(/*arg*/ 0, /*func*/ lvl_init_or_update),
    CALL_LOOP(/*arg*/ 1, /*func*/ lvl_init_or_update),
    CLEAR_LEVEL(),
    SLEEP_BEFORE_EXIT(/*frames*/ 1),
    EXIT(),
};
