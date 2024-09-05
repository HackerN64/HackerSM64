#include <ultra64.h>
#include "behavior_data.h"
#include "global_object_fields.h"
#include "game/object_helpers.h"

/* Object Respawner */
#define /*0x0F4*/ oRespawnerModelToRespawn    OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oRespawnerMinSpawnDist      OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oRespawnerBehaviorToRespawn OBJECT_FIELD_CVPTR(0x1D)

void bhv_respawner_loop(void) {
    struct Object *spawnedObject;
    if (!is_point_within_radius_of_mario(o->oPosX, o->oPosY, o->oPosZ, o->oRespawnerMinSpawnDist)) {
        spawnedObject = spawn_object(o, o->oRespawnerModelToRespawn, o->oRespawnerBehaviorToRespawn);
        spawnedObject->oBehParams = o->oBehParams;
        spawnedObject->respawnInfo = o->respawnInfo;
        spawnedObject->respawnInfoPointer = o->respawnInfoPointer;
        o->activeFlags = ACTIVE_FLAG_DEACTIVATED;
    }
}

void create_respawner(ModelID32 model, const BehaviorScript *behToSpawn, s32 minSpawnDist) {
    struct Object *respawner = spawn_object_abs_with_rot(o, 0, MODEL_NONE, bhvRespawner, o->oHomeX, o->oHomeY, o->oHomeZ, 0, 0, 0);
    respawner->oBehParams = o->oBehParams;
    respawner->oRespawnerModelToRespawn = model;
    respawner->oRespawnerMinSpawnDist = minSpawnDist;
    respawner->oRespawnerBehaviorToRespawn = behToSpawn;
    respawner->respawnInfo = o->respawnInfo;
    respawner->respawnInfoPointer = o->respawnInfoPointer;
}
