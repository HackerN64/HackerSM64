#include <ultra64.h>
#include "behavior_data.h"
#include "global_object_fields.h"
#include "audio/external.h"
#include "engine/math_util.h"
#include "game/level_update.h"
#include "game/object_helpers.h"

enum UnlockDoorStarStates {
    UNLOCK_DOOR_STAR_RISING,
    UNLOCK_DOOR_STAR_WAITING,
    UNLOCK_DOOR_STAR_SPAWNING_PARTICLES,
    UNLOCK_DOOR_STAR_DONE
};

/* Sealed Door Star */
#define /*0x108*/ oUnlockDoorStarState  OBJECT_FIELD_U32(0x20)
#define /*0x10C*/ oUnlockDoorStarTimer  OBJECT_FIELD_S32(0x21)
#define /*0x110*/ oUnlockDoorStarYawVel OBJECT_FIELD_S32(0x22)

static void star_door_unlock_spawn_particles(s16 angleOffset) {
    struct Object *sparkleParticle = spawn_object(o, MODEL_NONE, bhvSparkleSpawn);

    sparkleParticle->oPosX += 100.0f * sins((o->oUnlockDoorStarTimer * 0x2800) + angleOffset);
    sparkleParticle->oPosZ += 100.0f * coss((o->oUnlockDoorStarTimer * 0x2800) + angleOffset);
    // Particles are spawned lower each frame
    sparkleParticle->oPosY -= o->oUnlockDoorStarTimer * 10.0f;
}

void bhv_unlock_door_star_init(void) {
    o->oUnlockDoorStarState = UNLOCK_DOOR_STAR_RISING;
    o->oUnlockDoorStarTimer = 0;
    o->oUnlockDoorStarYawVel = 0x1000;
    o->oPosX += 30.0f * sins(gMarioState->faceAngle[1] - 0x4000);
    o->oPosY += 160.0f;
    o->oPosZ += 30.0f * coss(gMarioState->faceAngle[1] - 0x4000);
    o->oMoveAngleYaw = 0x7800;
    obj_scale(o, 0.5f);
}

void bhv_unlock_door_star_loop(void) {
    s16 prevYaw = o->oMoveAngleYaw;

    // Speed up the star every frame
    if (o->oUnlockDoorStarYawVel < 0x2400) {
        o->oUnlockDoorStarYawVel += 0x60;
    }
    switch (o->oUnlockDoorStarState) {
        case UNLOCK_DOOR_STAR_RISING:
            o->oPosY += 3.4f; // Raise the star up in the air
            o->oMoveAngleYaw +=
                o->oUnlockDoorStarYawVel; // Apply yaw velocity
            cur_obj_scale(o->oUnlockDoorStarTimer / 50.0f + 0.5f); // Scale the star to be bigger
            if (++o->oUnlockDoorStarTimer == 30) {
                o->oUnlockDoorStarTimer = 0;
                o->oUnlockDoorStarState++; // Sets state to UNLOCK_DOOR_STAR_WAITING
            }
            break;
        case UNLOCK_DOOR_STAR_WAITING:
            o->oMoveAngleYaw +=
                o->oUnlockDoorStarYawVel; // Apply yaw velocity
            if (++o->oUnlockDoorStarTimer == 30) {
                play_sound(SOUND_MENU_STAR_SOUND, o->header.gfx.cameraToObject); // Play final sound
                cur_obj_hide(); // Hide the object
                o->oUnlockDoorStarTimer = 0;
                o->oUnlockDoorStarState++; // Sets state to UNLOCK_DOOR_STAR_SPAWNING_PARTICLES
            }
            break;
        case UNLOCK_DOOR_STAR_SPAWNING_PARTICLES:
            // Spawn two particles, opposite sides of the star.
            star_door_unlock_spawn_particles(0);
            star_door_unlock_spawn_particles(0x8000);
            if (o->oUnlockDoorStarTimer++ == 20) {
                o->oUnlockDoorStarTimer = 0;
                o->oUnlockDoorStarState++; // Sets state to UNLOCK_DOOR_STAR_DONE
            }
            break;
        case UNLOCK_DOOR_STAR_DONE: // The object stays loaded for an additional 50 frames so that the
                                    // sound doesn't immediately stop.
            if (o->oUnlockDoorStarTimer++ == 50) {
                obj_mark_for_deletion(o);
            }
            break;
    }
    // Checks if the angle has cycled back to 0.
    // This means that the code will execute when the star completes a full revolution.
    if (prevYaw > (s16) o->oMoveAngleYaw) {
        play_sound(
            SOUND_GENERAL_SHORT_STAR,
            o->header.gfx.cameraToObject); // Play a sound every time the star spins once
    }
}
