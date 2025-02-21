#include <ultra64.h>
#include "global_object_fields.h"
#include "game/level_update.h"
#include "game/object_helpers.h"
#include "game/spawn_sound.h"

/* Spinning Recovery Heart */
#define /*0x0F4*/ oSpinningHeartTotalSpin   OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oSpinningHeartPlayedSound OBJECT_FIELD_S32(0x1C)

struct ObjectHitbox sRecoveryHeartHitbox = {
    /* interactType:      */ 0,
    /* downOffset:        */ 0,
    /* damageOrCoinValue: */ 0,
    /* health:            */ 0,
    /* numLootCoins:      */ 0,
    /* radius:            */ 50,
    /* height:            */ 50,
    /* hurtboxRadius:     */ 50,
    /* hurtboxHeight:     */ 50,
};

void bhv_recovery_heart_loop(void) {
    obj_set_hitbox(o, &sRecoveryHeartHitbox);
    if (obj_check_if_collided_with_object(o, gMarioObject)) {
        if (o->oSpinningHeartPlayedSound == 0) {
            cur_obj_play_sound_2(SOUND_GENERAL_HEART_SPIN);
            o->oSpinningHeartPlayedSound++;
        }

        o->oAngleVelYaw = (s32)(200.0f * gMarioState->forwardVel) + 1000;
    } else {
        o->oSpinningHeartPlayedSound = 0;

        if ((o->oAngleVelYaw -= 50) < 400) {
            o->oAngleVelYaw = 400;
            o->oSpinningHeartTotalSpin = 0;
        }
    }

    if ((o->oSpinningHeartTotalSpin += o->oAngleVelYaw) >= 0x10000) {
        gMarioState->healCounter += 4;
#ifdef BREATH_METER
        gMarioState->breathCounter += 4;
#endif
        o->oSpinningHeartTotalSpin -= 0x10000;
    }

    o->oFaceAngleYaw += o->oAngleVelYaw;
}
