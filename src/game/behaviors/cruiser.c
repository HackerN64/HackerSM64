#include <ultra64.h>
#include "global_object_fields.h"
#include "game/object_helpers.h"

/* RR Cruiser Wing */
#define /*0x0F4*/ oRRCruiserWingInitYaw   OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oRRCruiserWingInitPitch OBJECT_FIELD_S32(0x1C)

void bhv_rr_cruiser_wing_init(void) {
    o->oRRCruiserWingInitYaw = o->oFaceAngleYaw;
    o->oRRCruiserWingInitPitch = o->oFaceAnglePitch;
}

void bhv_rr_cruiser_wing_loop(void) {
    if (o->oBehParams2ndByte == 0) {
        o->oFaceAngleYaw = o->oRRCruiserWingInitYaw + sins(o->oTimer * 0x400) * 8192.0f;
        o->oFaceAnglePitch = o->oRRCruiserWingInitPitch + coss(o->oTimer * 0x400) * 2048.0f;
    } else {
        o->oFaceAngleYaw = o->oRRCruiserWingInitYaw - sins(o->oTimer * 0x400) * 8192.0f;
        o->oFaceAnglePitch = o->oRRCruiserWingInitPitch + coss(o->oTimer * 0x400) * 2048.0f;
    }
    if (o->oTimer == 64) {
        cur_obj_play_sound_2(SOUND_GENERAL_BOAT_ROCK);
        o->oTimer = 0;
    }
}
