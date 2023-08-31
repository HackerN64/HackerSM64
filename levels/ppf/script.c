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
#include "levels/ppf/header.h"


const LevelScript level_ppf_entry[] = {
	INIT_LEVEL(),
	LOAD_MIO0(0x7, _ppf_segment_7SegmentRomStart, _ppf_segment_7SegmentRomEnd), 
	LOAD_MIO0(0xb, _effect_mio0SegmentRomStart, _effect_mio0SegmentRomEnd), 
	LOAD_MIO0(0xa, _water_skybox_mio0SegmentRomStart, _water_skybox_mio0SegmentRomEnd), 
	LOAD_MIO0(0x5, _group1_mio0SegmentRomStart, _group1_mio0SegmentRomEnd), 
	LOAD_RAW(0xc, _group1_geoSegmentRomStart, _group1_geoSegmentRomEnd), 
	LOAD_MIO0(0x6, _group14_mio0SegmentRomStart, _group14_mio0SegmentRomEnd), 
	LOAD_RAW(0xd, _group14_geoSegmentRomStart, _group14_geoSegmentRomEnd), 
    LOAD_YAY0(/*seg*/ 0x08, _common0_yay0SegmentRomStart, _common0_yay0SegmentRomEnd),
    LOAD_RAW(/*seg*/ 0x0F, _common0_geoSegmentRomStart,  _common0_geoSegmentRomEnd),
	ALLOC_LEVEL_POOL(),
	MARIO(MODEL_MARIO, 0x00000001, bhvMario), 
    JUMP_LINK(script_func_global_1),
    JUMP_LINK(script_func_global_2),
    JUMP_LINK(script_func_global_15),
	/* Fast64 begin persistent block [level commands] */
	/* Fast64 end persistent block [level commands] */

	AREA(1, ppf_area_1),
		MARIO_POS(0x01, 0, 2351, 135, -4381),
		OBJECT(MODEL_YELLOW_COIN, 1805, 323, -976, 0, 0, 0, 0x00000000, bhvOneCoin),
		OBJECT(MODEL_YELLOW_COIN, 1807, 251, -813, 0, 0, 0, 0x00000000, bhvOneCoin),
		OBJECT(MODEL_YELLOW_COIN, 1809, 259, -659, 0, 0, 0, 0x00000000, bhvOneCoin),
		OBJECT(MODEL_YELLOW_COIN, 1812, 310, -439, 0, 0, 0, 0x00000000, bhvOneCoin),
		OBJECT(MODEL_YELLOW_COIN, 1814, 358, -301, 0, 0, 0, 0x00000000, bhvOneCoin),
        OBJECT(/*model*/ MODEL_LUIGI,       /*pos*/ -1789, 6747, -3414, /*angle*/ 0, 0, 0, /*behParam*/ DIALOG_133 << 24, /*beh*/ bhvLuigiNPC),
		TERRAIN(ppf_area_1_collision),
		MACRO_OBJECTS(ppf_area_1_macro_objs),
		SET_BACKGROUND_MUSIC(0x00, SEQ_LEVEL_GRASS),
        SHOW_DIALOG(/*index*/ 0x00, DIALOG_048),
		TERRAIN_TYPE(TERRAIN_GRASS),
		/* Fast64 begin persistent block [area commands] */
		/* Fast64 end persistent block [area commands] */
	END_AREA(),

	FREE_LEVEL_POOL(),
	MARIO_POS(0x01, 0, 2351, 135, -4381),
	CALL(0, lvl_init_or_update),
	CALL_LOOP(1, lvl_init_or_update),
	CLEAR_LEVEL(),
	SLEEP_BEFORE_EXIT(1),
	EXIT(),
};
