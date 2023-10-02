#include <ultra64.h>
#include "global_object_fields.h"
#include "object_helpers.h"

// reds_star_marker.inc.c
// Filename is abbreviated to prevent compiler seg fault

void bhv_red_coin_star_marker_init(void) {
    o->header.gfx.scale[2] = 0.75f;
}
