#include <ultra64.h>
#include "global_object_fields.h"
#include "game/object_helpers.h"

#define /*0x0F4*/ oSwingPlatformAngle OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oSwingPlatformSpeed OBJECT_FIELD_F32(0x1C)
// swing_platform.inc.c

void bhv_swing_platform_init(void) {
    o->oSwingPlatformAngle = 0x2000;
}

void bhv_swing_platform_update(void) {
    s32 startRoll = o->oFaceAngleRoll;

    if (o->oFaceAngleRoll < 0) {
        o->oSwingPlatformSpeed += 4.0f;
    } else {
        o->oSwingPlatformSpeed -= 4.0f;
    }

    o->oSwingPlatformAngle += o->oSwingPlatformSpeed;
    o->oFaceAngleRoll = o->oSwingPlatformAngle;
    o->oAngleVelRoll = o->oFaceAngleRoll - startRoll;
}
