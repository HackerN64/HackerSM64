#include <ultra64.h>
#include "global_object_fields.h"
#include "engine/math_util.h"
#include "game/object_helpers.h"

void bhv_jrb_floating_box_loop(void) {
    o->oPosY = o->oHomeY + sins(o->oTimer * 0x400) * 10.0f;
}
