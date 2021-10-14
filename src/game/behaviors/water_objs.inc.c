// water_objs.inc.c
// TODO: Better name, please

void bhv_water_air_bubble_init(void) {
    cur_obj_scale(4.0f);
}

// Fields 0xF4 & 0xF8 seem to be angles for bubble and cannon

void bhv_water_air_bubble_loop(void) {
    s32 i;
    o->header.gfx.scale[0] = sins(o->oWaterObjScaleXAngle) * 0.5f + 4.0f;
    o->header.gfx.scale[1] = -sins(o->oWaterObjScaleXAngle) * 0.5f + 4.0f;
    o->oWaterObjScaleXAngle += 0x400;

    if (o->oTimer < 30) {
        cur_obj_become_intangible();
        o->oPosY += 3.0f;
    } else {
        cur_obj_become_tangible();
        cur_obj_forward_vel_approach_upward(2.0f, 10.0f);
        o->oMoveAngleYaw = obj_angle_to_object(o, gMarioObject);
        cur_obj_move_using_fvel_and_gravity();
    }

    o->oPosX += random_float() * 4.0f - 2.0f;
    o->oPosZ += random_float() * 4.0f - 2.0f;

    if (o->oInteractStatus & INT_STATUS_INTERACTED || o->oTimer > 200) {
        cur_obj_play_sound_2(SOUND_GENERAL_QUIET_BUBBLE);
        obj_mark_for_deletion(o);
        for (i = 0; i < 30; i++) {
            spawn_object(o, MODEL_BUBBLE, bhvBubbleMaybe);
        }
    }

    if (find_water_level(o->oPosX, o->oPosZ) < o->oPosY) {
        obj_mark_for_deletion(o);
    }

    o->oInteractStatus = INT_STATUS_NONE;
}

void bhv_bubble_wave_init(void) {
    o->oWaterObjScaleXAngleVel = 0x800 + (s32)(random_float() * 2048.0f);
    o->oWaterObjScaleYAngleVel = 0x800 + (s32)(random_float() * 2048.0f);
    cur_obj_play_sound_2(SOUND_GENERAL_QUIET_BUBBLE);
}

void scale_bubble_random(void) {
    cur_obj_scale(random_float() + 1.0f);
}

void bhv_bubble_maybe_loop(void) {
    o->oPosY += random_float() * 3.0f + 6.0f;
    o->oPosX += random_float() * 10.0f - 5.0f;
    o->oPosZ += random_float() * 10.0f - 5.0f;

    o->header.gfx.scale[0] = sins(o->oWaterObjScaleXAngle) * 0.2f + 1.0f;
    o->oWaterObjScaleXAngle += o->oWaterObjScaleXAngleVel;
    o->header.gfx.scale[1] = sins(o->oWaterObjScaleYAngle) * 0.2f + 1.0f;
    o->oWaterObjScaleYAngle += o->oWaterObjScaleYAngleVel;
}

void bhv_small_water_wave_loop(void) {
    o->header.gfx.scale[0] = sins(o->oWaterObjScaleXAngle) * 0.2f + 1.0f;
    o->oWaterObjScaleXAngle += o->oWaterObjScaleXAngleVel;
    o->header.gfx.scale[1] = sins(o->oWaterObjScaleYAngle) * 0.2f + 1.0f;
    o->oWaterObjScaleYAngle += o->oWaterObjScaleYAngleVel;

    if (o->oPosY > find_water_level(o->oPosX, o->oPosZ)) {
        o->activeFlags = ACTIVE_FLAG_DEACTIVATED;
        o->oPosY += 5.0f;
        if (gFreeObjectList.next != NULL) {
            spawn_object(o, MODEL_SMALL_WATER_SPLASH, bhvObjectWaterSplash);
        }
    }

    if (o->oInteractStatus & INT_STATUS_INTERACTED) {
        obj_mark_for_deletion(o);
    }
}

void scale_bubble_sin(void) {
    o->header.gfx.scale[0] = sins(o->oWaterObjScaleXAngle) * 0.5f + 2.0f;
    o->oWaterObjScaleXAngle += o->oWaterObjScaleXAngleVel;
    o->header.gfx.scale[1] = sins(o->oWaterObjScaleYAngle) * 0.5f + 2.0f;
    o->oWaterObjScaleYAngle += o->oWaterObjScaleYAngleVel;
}

void bhv_particle_init(void) {
    obj_scale_xyz(o, 2.0f, 2.0f, 1.0f);
    o->oWaterObjScaleXAngleVel = 0x800 + (s32)(random_float() * 2048.0f);
    o->oWaterObjScaleYAngleVel = 0x800 + (s32)(random_float() * 2048.0f);
    obj_translate_xyz_random(o, 100.0f);
}

void bhv_particle_loop() {
    o->oPosY += 5.0f;
    obj_translate_xz_random(o, 4.0f);
    scale_bubble_sin();
    if (o->oTimer != 0 && o->oPosY > find_water_level(o->oPosX, o->oPosZ)) {
        obj_mark_for_deletion(o);
        try_to_spawn_object(5, 0, o, MODEL_SMALL_WATER_SPLASH, bhvObjectWaterSplash);
    }
}

void bhv_small_bubbles_loop(void) {
    o->oPosY += 5.0f;
    obj_translate_xz_random(o, 4.0f);
    scale_bubble_sin();
}

void bhv_fish_group_loop(void) {
    if ((gMarioCurrentRoom == 15 || gMarioCurrentRoom == 7) && (gGlobalTimer & 1)) {
        spawn_object(o, MODEL_WHITE_PARTICLE_SMALL, bhvSmallParticleBubbles);
    }
}

void bhv_water_waves_init(void) {
    s32 i;
    for (i = 0; i < 3; i++) {
        spawn_object(o, MODEL_WHITE_PARTICLE_SMALL, bhvSmallParticle);
    }
}
