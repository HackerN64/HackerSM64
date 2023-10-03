#include <ultra64.h>
#include "global_object_fields.h"
#include "audio/external.h"
#include "game/object_helpers.h"

void bhv_sound_spawner_init(void) {
    play_sound(o->oSoundEffectBits, o->header.gfx.cameraToObject);
}
