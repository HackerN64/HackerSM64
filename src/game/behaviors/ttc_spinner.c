#include <ultra64.h>
#include "global_object_fields.h"
#include "engine/math_util.h"
#include "game/object_helpers.h"

/**
 * Behavior for TTC spinner.
 */

/* TTC Spinner */
#define /*0x0F4*/ oTTCSpinnerDir     OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oTTCChangeDirTimer OBJECT_FIELD_S32(0x1C)

/**
 * Spinner speeds on each setting.
 */
static s16 sTTCSpinnerSpeeds[] = {
    /* TTC_SPEED_SLOW    */ 200,
    /* TTC_SPEED_FAST    */ 600,
    /* TTC_SPEED_RANDOM  */ 200,
    /* TTC_SPEED_STOPPED */ 0,
};

/**
 * Update function for bhvTTCSpinner.
 */
void bhv_ttc_spinner_update(void) {
    o->oAngleVelPitch = sTTCSpinnerSpeeds[gTTCSpeedSetting];

    if (gTTCSpeedSetting == TTC_SPEED_RANDOM) {
        if (o->oTimer > o->oTTCChangeDirTimer) {
            o->oTTCSpinnerDir = random_sign();
            o->oTTCChangeDirTimer = random_mod_offset(30, 30, 4);
            o->oTimer = 0;
        } else if (o->oTimer > 5) {
            o->oAngleVelPitch *= o->oTTCSpinnerDir;
        } else {
            // Stop for 5 frames after changing direction
            o->oAngleVelPitch = 0;
        }
    }

    o->oFaceAnglePitch += o->oAngleVelPitch;
}
