#include <ultra64.h>
#include "global_object_fields.h"
#include "object_helpers.h"

// sound_waterfall.inc.c

void bhv_waterfall_sound_loop(void) {
    cur_obj_play_sound_1(SOUND_ENV_WATERFALL2);
}
