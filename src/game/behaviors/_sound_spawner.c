#include <ultra64.h>
#include "global_object_fields.h"
#include "object_helpers.h"

// sound_spawner.inc.c

void bhv_sound_spawner_init(void) {
    play_sound(o->oSoundEffectBits, o->header.gfx.cameraToObject);
}
