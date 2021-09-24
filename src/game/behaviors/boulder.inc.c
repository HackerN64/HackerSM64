// boulder.c.inc

static struct ObjectHitbox sRollingSphereHitbox2 = {
    /* interactType:      */ INTERACT_DAMAGE,
    /* downOffset:        */ 0,
    /* damageOrCoinValue: */ 3,
    /* health:            */ 0,
    /* numLootCoins:      */ 0,
    /* radius:            */ 210,
    /* height:            */ 350,
    /* hurtboxRadius:     */ 0,
    /* hurtboxHeight:     */ 0,
};

void bhv_big_boulder_init(void) {
    o->oHomeX = o->oPosX;
    o->oHomeY = o->oPosY;
    o->oHomeZ = o->oPosZ;

    o->oGravity = 8.0f;
    o->oFriction = 0.999f;
    o->oBuoyancy = 2.0f;
}

void set_rolling_sphere_hitbox2(void) {
    obj_set_hitbox(o, &sRollingSphereHitbox2);

    if ((o->oInteractStatus & INT_STATUS_INTERACTED) != 0) {
        o->oInteractStatus = 0;
    }
}

void adjust_rolling_face_pitch2(f32 f12) {
    o->oFaceAnglePitch += (s16)(o->oForwardVel * (100.0f / f12));
    o->oSnowmansBottomRollingFacePitch += o->oForwardVel * 1e-4;

    if (o->oSnowmansBottomRollingFacePitch > 1.0)
        o->oSnowmansBottomRollingFacePitch = 1.0f;
}

void boulder_act_1(void) {
    s16 collisionFlags = object_step_without_floor_orient();
    if ((collisionFlags & OBJ_COL_FLAGS_LANDED) == OBJ_COL_FLAG_GROUNDED && o->oVelY > 10.0f) {
        cur_obj_play_sound_2(SOUND_GENERAL_GRINDEL_ROLL);
        spawn_mist_particles();
    }

    if (o->oForwardVel > 70.0)
        o->oForwardVel = 70.0f;

    if (o->oPosY < -1000.0f)
        o->activeFlags = ACTIVE_FLAG_DEACTIVATED;
}

void bhv_big_boulder_loop(void) {
    cur_obj_scale(1.5f);
    o->oGraphYOffset = 270.0f;
    switch (o->oAction) {
        case 0:
            o->oForwardVel = 40.0f;
            o->oAction = 1;
            break;

        case 1:
            boulder_act_1();
            adjust_rolling_face_pitch2(1.5f);
            cur_obj_play_sound_1(SOUND_ENV_UNKNOWN2);
            break;
    }

    set_rolling_sphere_hitbox2();
}

void bhv_big_boulder_generator_loop(void) {
    struct Object *boulderObj;
    if (o->oTimer >= 256) {
        o->oTimer = 0;
    }

    if (!current_mario_room_check(4) || is_point_within_radius_of_mario(o->oPosX, o->oPosY, o->oPosZ, 1500))
        return;

    if (is_point_within_radius_of_mario(o->oPosX, o->oPosY, o->oPosZ, 6000)) {
        if ((o->oTimer & 0x3F) == 0) {
            boulderObj = spawn_object(o, MODEL_HMC_ROLLING_ROCK, bhvBigBoulder);
            boulderObj->oMoveAngleYaw = random_float() * 4096.0f;
        }
    } else {
        if ((o->oTimer & 0x7F) == 0) {
            boulderObj = spawn_object(o, MODEL_HMC_ROLLING_ROCK, bhvBigBoulder);
            boulderObj->oMoveAngleYaw = random_float() * 4096.0f;
        }
    }
}
