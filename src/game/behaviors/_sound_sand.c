#include <ultra64.h>
#include "global_object_fields.h"
#include "game/object_helpers.h"

// sound_sand.inc.c

void bhv_sand_sound_loop(void) {
    if (gCamera->mode == CAMERA_MODE_BEHIND_MARIO) {
        return;
    }

    cur_obj_play_sound_1(SOUND_ENV_MOVINGSAND);
}
