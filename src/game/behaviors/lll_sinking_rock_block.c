#include <ultra64.h>
#include "global_object_fields.h"
#include "game/object_helpers.h"

/* Sink When Stepped On */
#define /*0x104*/ oSinkWhenSteppedOnTimer           OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oSinkWhenSteppedOnYOffsetFromHome OBJECT_FIELD_F32(0x20)

// from lll_octagonal_rotating_mesh.c
extern s32 lll_octagonal_mesh_find_y_offset(s32 *standTimer, f32 *posOffset, s32 standTimerInc, s32 moveDownAmount);

void bhv_lll_sinking_rock_block_loop(void) {
    lll_octagonal_mesh_find_y_offset(&o->oSinkWhenSteppedOnTimer, &o->oSinkWhenSteppedOnYOffsetFromHome, 124, -110);
    o->oGraphYOffset = 0.0f;
    o->oPosY = o->oHomeY + o->oSinkWhenSteppedOnYOffsetFromHome;
}
