#include <ultra64.h>
#include "global_object_fields.h"
#include "game/object_helpers.h"

// sound_volcano.inc.c

void bhv_volcano_sound_loop(void) {
    cur_obj_play_sound_1(SOUND_ENV_DRONING1);
}
