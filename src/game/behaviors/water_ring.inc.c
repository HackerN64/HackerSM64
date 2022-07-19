// water_ring.inc.c

#ifndef FIX_WATER_RINGS
f32 water_ring_calc_mario_dist(void) {
    Vec3f marioDist;
    vec3_diff(marioDist, &o->oPosVec, gMarioObject->header.gfx.pos);
    marioDist[1] += 80.0f;
    return vec3_dot(marioDist, &o->oWaterRingNormalVec);
}
#endif

void water_ring_init(void) {
    cur_obj_init_animation(WATER_RING_ANIM_WOBBLE);
    o->oWaterRingScalePhaseX = (s32)(random_float() * 4096.0f) + 0x1000;
    o->oWaterRingScalePhaseY = (s32)(random_float() * 4096.0f) + 0x1000;
    o->oWaterRingScalePhaseZ = (s32)(random_float() * 4096.0f) + 0x1000;

#ifndef FIX_WATER_RINGS
    //! This normal calculation assumes a facing yaw of 0, which is not the case
    //  for the manta ray rings. It also errs by multiplying the normal X by -1.
    //  This cause the ring's orientation for the purposes of collision to be
    //  different than the graphical orientation, which means that Mario won't
    //  necessarily collect a ring even if he appears to swim through it.
    o->oWaterRingNormalX = coss(o->oFaceAnglePitch) * sins(o->oFaceAngleRoll) * -1.0f;
    o->oWaterRingNormalY = coss(o->oFaceAnglePitch) * coss(o->oFaceAngleRoll);
    o->oWaterRingNormalZ = sins(o->oFaceAnglePitch);

    o->oWaterRingMarioDistInFront = water_ring_calc_mario_dist();

    // Adding this code will alter the ring's graphical orientation to align with the faulty
    // collision orientation:
    //
    // o->oFaceAngleYaw = 0;
    // o->oFaceAngleRoll *= -1;
#endif
}

void bhv_jet_stream_water_ring_init(void) {
    water_ring_init();
    o->oOpacity = 70;
    cur_obj_init_animation(WATER_RING_ANIM_WOBBLE);
    o->oFaceAnglePitch = 0x8000;
}

void water_ring_check_collection(UNUSED f32 avgScale, struct Object *ringManager) {
#ifdef FIX_WATER_RINGS
    if (o->oInteractStatus & INT_STATUS_INTERACTED) {
#else
    f32 marioDistInFront = water_ring_calc_mario_dist();

    if (!is_point_close_to_object(o, gMarioObject->header.gfx.pos[0],
                                     gMarioObject->header.gfx.pos[1] + 80.0f,
                                     gMarioObject->header.gfx.pos[2],
                                     (avgScale + 0.2f) * 120.0f)) {
        o->oWaterRingMarioDistInFront = marioDistInFront;
        return;
    }

    if (o->oWaterRingMarioDistInFront * marioDistInFront < 0.0f) {
#endif
        struct Object *ringSpawner = o->parentObj;

        if (ringSpawner) {
            if ((o->oWaterRingIndex == ringManager->oWaterRingMgrLastRingCollected + 1)
                || (ringSpawner->oWaterRingSpawnerRingsCollected == 0)) {
                ringSpawner->oWaterRingSpawnerRingsCollected++;
                if (ringSpawner->oWaterRingSpawnerRingsCollected < 6) {
                    spawn_orange_number(ringSpawner->oWaterRingSpawnerRingsCollected, 0, -40, 0);
                    play_sound(SOUND_MENU_COLLECT_SECRET
                                + (((u8) ringSpawner->oWaterRingSpawnerRingsCollected - 1) << 16),
                                gGlobalSoundSource);
                }
                ringManager->oWaterRingMgrLastRingCollected = o->oWaterRingIndex;
            } else {
                ringSpawner->oWaterRingSpawnerRingsCollected = 0;
            }
        }

        o->oAction = WATER_RING_ACT_COLLECTED;
    }

#ifndef FIX_WATER_RINGS
    o->oWaterRingMarioDistInFront = marioDistInFront;
#endif
}

void water_ring_set_scale(f32 avgScale) {
    o->header.gfx.scale[0] = sins(o->oWaterRingScalePhaseX) * 0.1f + avgScale;
    o->header.gfx.scale[1] = sins(o->oWaterRingScalePhaseY) * 0.5f + avgScale;
    o->header.gfx.scale[2] = sins(o->oWaterRingScalePhaseZ) * 0.1f + avgScale;
    o->oWaterRingScalePhaseX += 0x1700;
    o->oWaterRingScalePhaseY += 0x1700;
    o->oWaterRingScalePhaseZ += 0x1700;
}

void water_ring_act_collected(void) {
    f32 avgScale = (f32) o->oTimer * 0.2f + o->oWaterRingAvgScale;

    if (o->oTimer > 20) {
        o->activeFlags = ACTIVE_FLAG_DEACTIVATED;
    }

    o->oOpacity -= 10;
    if (o->oOpacity < 0) {
        o->oOpacity = 0;
    }

    water_ring_set_scale(avgScale);
}

void water_ring_act_not_collected(void) {
    f32 avgScale = (f32) o->oTimer / 225.0f * 3.0f + 0.5f;

    //! In this case ringSpawner and ringManager are the same object,
    //  because the Jet Stream Ring Spawner is its own parent object.
    struct Object *ringSpawner = o->parentObj;

    if (o->oTimer > 225) {
        o->oOpacity -= 2;
        if (o->oOpacity < 3) {
            o->activeFlags = ACTIVE_FLAG_DEACTIVATED;
        }
    }

    water_ring_check_collection(avgScale, ringSpawner);
    water_ring_set_scale(avgScale);

    o->oPosY += 10.0f;
    o->oFaceAngleYaw += 0x100;
    set_object_visibility(o, 5000);

    if (ringSpawner->oWaterRingSpawnerRingsCollected == 4
        && o->oWaterRingIndex == ringSpawner->oWaterRingMgrLastRingCollected + 1) {
        o->oOpacity = sins(o->oTimer * 0x1000) * 200.0f + 50.0f;
    }

    o->oWaterRingAvgScale = avgScale;
}

void bhv_jet_stream_water_ring_loop(void) {
    switch (o->oAction) {
        case WATER_RING_ACT_NOT_COLLECTED:
            water_ring_act_not_collected();
            break;

        case WATER_RING_ACT_COLLECTED:
            water_ring_act_collected();
            break;
    }
}

void spawn_manta_ray_ring_manager(void) {
    struct Object *ringManager = spawn_object(o, MODEL_NONE, bhvMantaRayRingManager);
    o->parentObj = ringManager;
}

void water_ring_spawner_act_inactive(void) {
    //! Because the index counter overflows at 10000, it's possible to wait
    //  for about 4 hours and 38 minutes if you miss a ring, and the index will
    //  come around again.
    if (o->oTimer == 300) {
        o->oTimer = 0;
    }

    if ((o->oTimer ==   0)
     || (o->oTimer ==  50)
     || (o->oTimer == 150)
     || (o->oTimer == 200)
     || (o->oTimer == 250)) {
        struct Object *waterRing = spawn_object(o, MODEL_WATER_RING, bhvJetStreamWaterRing);
        waterRing->oWaterRingIndex = o->oWaterRingMgrNextRingIndex;
        o->oWaterRingMgrNextRingIndex++;
        if (o->oWaterRingMgrNextRingIndex > 10000) {
            o->oWaterRingMgrNextRingIndex = 0;
        }
    }
}

void bhv_jet_stream_ring_spawner_loop(void) {
    switch (o->oAction) {
        case JS_RING_SPAWNER_ACT_ACTIVE:
            water_ring_spawner_act_inactive();

            if (o->oWaterRingSpawnerRingsCollected == 5) {
                spawn_mist_particles();
                spawn_default_star(3400.0f, -3200.0f, -500.0f);
                o->oAction = JS_RING_SPAWNER_ACT_INACTIVE;
            }
            break;

        case JS_RING_SPAWNER_ACT_INACTIVE:
            break;
    }
}

void bhv_manta_ray_water_ring_init(void) {
    water_ring_init();
    o->oOpacity = 150;
}

void manta_water_ring_act_not_collected(void) {
    f32 avgScale = (f32) o->oTimer / 50.0f * 1.3f + 0.1f;
    struct Object *ringSpawner = o->parentObj;
    struct Object *ringManager = ringSpawner->parentObj;

    if (avgScale > 1.3f) {
        avgScale = 1.3f;
    }

    if (o->oTimer > 150) {
        o->oOpacity -= 2;
        if (o->oOpacity < 3) {
            obj_mark_for_deletion(o);
        }
    }

    water_ring_check_collection(avgScale, ringManager);
    water_ring_set_scale(avgScale);
    set_object_visibility(o, 5000);

    if (ringSpawner->oWaterRingSpawnerRingsCollected == 4
        && o->oWaterRingIndex == ringManager->oWaterRingMgrLastRingCollected + 1) {
        o->oOpacity = sins(o->oTimer * 0x1000) * 200.0f + 50.0f;
    }

    o->oWaterRingAvgScale = avgScale;
}

void bhv_manta_ray_water_ring_loop(void) {
    switch (o->oAction) {
        case WATER_RING_ACT_NOT_COLLECTED:
            manta_water_ring_act_not_collected();
            break;

        case WATER_RING_ACT_COLLECTED:
            water_ring_act_collected();
            break;
    }
}
