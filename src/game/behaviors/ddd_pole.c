#include <ultra64.h>
#include "global_object_fields.h"
#include "object_helpers.h"

#define /*0x0F4*/ oDDDPoleVel       OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oDDDPoleMaxOffset OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oDDDPoleOffset    OBJECT_FIELD_F32(0x1D)
// ddd_pole.inc.c

void bhv_ddd_pole_init(void) {
    if (gCurrActNum == 1) {
        obj_mark_for_deletion(o);
    } else {
        o->hitboxDownOffset = 100.0f;
        o->oDDDPoleMaxOffset = 100.0f * o->oBehParams2ndByte;
    }
}

void bhv_ddd_pole_update(void) {
    if (o->oTimer > 20) {
        o->oDDDPoleOffset += o->oDDDPoleVel;

        if (clamp_f32(&o->oDDDPoleOffset, 0.0f, o->oDDDPoleMaxOffset)) {
            o->oDDDPoleVel = -o->oDDDPoleVel;
            o->oTimer = 0;
        }
    }

    obj_set_dist_from_home(o->oDDDPoleOffset);
}
