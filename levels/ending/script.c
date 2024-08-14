#include <ultra64.h>
#include "sm64.h"
#include "behavior_data.h"
#include "model_ids.h"
#include "seq_ids.h"
#include "segment_symbols.h"
#include "level_commands.h"

#include "game/area.h"
#include "game/level_update.h"

#include "levels/scripts.h"

#include "actors/common1.h"

#include "make_const_nonconst.h"
#include "levels/ending/header.h"

const LevelScript level_ending_entry_loop[] = {
    SLEEP(/*frames*/ 1),
    JUMP(level_ending_entry_loop), // (loop sleep 1 forever)
};

#ifdef RESET_AFTER_CREDITS
const LevelScript level_ending_wait_loop[] = {
    CALL(0, credits_end),
    CALL_LOOP(1, credits_end_wait_for_reset),
    CLEAR_LEVEL(),
    SLEEP_BEFORE_EXIT(/*frames*/ 1),
    EXIT(),
};
#endif // RESET_AFTER_CREDITS

const LevelScript level_ending_entry[] = {
    INIT_LEVEL(),
    LOAD_LEVEL_DATA(ending),
    ALLOC_LEVEL_POOL(),

    AREA(/*index*/ 1, ending_geo_area_1),
    END_AREA(),

    FREE_LEVEL_POOL(),
    SLEEP(/*frames*/ 60),
    BLACKOUT(/*active*/ FALSE),
    LOAD_AREA(/*area*/ 1),
    TRANSITION(/*transType*/ WARP_TRANSITION_FADE_FROM_COLOR, /*time*/ 75, /*color*/ 0x00, 0x00, 0x00),
    SLEEP(/*frames*/ 120),
    CALL(/*arg*/ 0, /*func*/ lvl_play_the_end_screen_sound),

#ifdef RESET_AFTER_CREDITS
    JUMP(level_ending_wait_loop),
#else
    JUMP(level_ending_entry_loop), // (loop sleep 1 forever)
#endif // RESET_AFTER_CREDITS
};
