#include <ultra64.h>
#include "global_object_fields.h"
#include "audio/external.h"
#include "game/object_helpers.h"

void bhv_ambient_sounds_init(void) {
    if (gCamera->mode == CAMERA_MODE_BEHIND_MARIO) {
        return;
    }

    play_sound(SOUND_AIR_CASTLE_OUTDOORS_AMBIENT, gGlobalSoundSource);
}
