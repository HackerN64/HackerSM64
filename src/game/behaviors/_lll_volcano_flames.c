#include <ultra64.h>
#include "global_object_fields.h"
#include "object_helpers.h"

// lll_volcano_flames.inc.c

void bhv_volcano_flames_loop(void) {
    cur_obj_update_floor_height();
    cur_obj_compute_vel_xz();

    o->oPosX += o->oVelX;
    o->oPosZ += o->oVelZ;

    cur_obj_move_y(-4.0f, -0.7f, 2.0f);

    if (o->oMoveFlags & (OBJ_MOVE_MASK_ON_GROUND | OBJ_MOVE_AT_WATER_SURFACE | OBJ_MOVE_UNDERWATER_OFF_GROUND)) {
        obj_mark_for_deletion(o);
    }
}
