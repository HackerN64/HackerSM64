#include <ultra64.h>
#include "global_object_fields.h"
#include "engine/math_util.h"
#include "game/object_helpers.h"

void bhv_castle_flag_init(void) {
    o->header.gfx.animInfo.animFrame = random_float() * 28.0f;
}
