#include <ultra64.h>
#include "global_object_fields.h"
#include "game/object_helpers.h"
#include "game/spawn_sound.h"

void bhv_waterfall_sound_loop(void) {
    cur_obj_play_sound_1(SOUND_ENV_WATERFALL2);
}
