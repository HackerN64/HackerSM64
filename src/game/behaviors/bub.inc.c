// bub.c.inc

// NOTE: These first set of functions spawn a school of bub depending on objF4's
// value. The later action functions seem to check Y distance to Mario and proceed
// to do nothing, which indicates this behavior set is incomplete.

void bub_spawner_act_0(void) {
    s32 i;
    s32 amt = o->oCheepCheepSpawnerSpawnAmount;
    if (o->oDistanceToMario < 1500.0f) {
        for (i = 0; i < amt; i++) {
            spawn_object(o, MODEL_BUB, bhvBub);
        }
        o->oAction = 1;
    }
}

void bub_spawner_act_1(void) {
    if (gMarioObject->oPosY - o->oPosY > 2000.0f)
        o->oAction = 2;
}

void bub_spawner_act_2(void) {
    o->oAction = 3;
}

void bub_spawner_act_3(void) {
    o->oAction = 0;
}

void (*sBirdChirpChirpActions[])(void) = { bub_spawner_act_0, bub_spawner_act_1,
                                           bub_spawner_act_2, bub_spawner_act_3 };

void bhv_bub_spawner_loop(void) {
    cur_obj_call_action_function(sBirdChirpChirpActions);
}

void bub_move_vertically(s32 ySpeed) {
    f32 parentY = o->parentObj->oPosY;
    if (parentY - 100.0f - o->oCheepCheepMaxYOffset < o->oPosY && o->oPosY < parentY + 1000.0f + o->oCheepCheepMaxYOffset) {
        o->oPosY = approach_f32_symmetric(o->oPosY, o->oCheepCheepTargetY, ySpeed);
    }
}

void bub_act_0(void) {
    o->oCheepCheepTargetYOffset = random_float() * 100.0f;
    o->oCheepCheepMaxYOffset = random_float() * 300.0f;
    o->oAction = 1;
}

void bub_act_1(void) {
    f32 dy;
    if (o->oTimer == 0) {
        o->oForwardVel = random_float() * 2 + 2;
        o->oCheepCheepRandomSwimAway = random_float();
    }
    dy = o->oPosY - gMarioObject->oPosY;
    if (o->oPosY < o->oCheepCheepWaterLevel - 50.0f) {
        if (dy < 0.0f)
            dy = 0.0f - dy;
        if (dy < 500.0f)
            bub_move_vertically(1);
        else
            bub_move_vertically(4);
    } else {
        o->oPosY = o->oCheepCheepWaterLevel - 50.0f;
        if (dy > 300.0f)
            o->oPosY = o->oPosY - 1.0f;
    }
    if (800.0f < cur_obj_lateral_dist_from_mario_to_home())
        o->oAngleToMario = cur_obj_angle_to_home();
    cur_obj_rotate_yaw_toward(o->oAngleToMario, 0x100);
    if (o->oDistanceToMario < 200.0f)
        if (o->oCheepCheepRandomSwimAway < 0.5f)
            o->oAction = 2;
    if (o->oInteractStatus & INT_STATUS_INTERACTED)
        o->oAction = 2;
}

void bub_act_2(void) {
    f32 dy;
    if (o->oTimer < 20) {
        if (o->oInteractStatus & INT_STATUS_INTERACTED)
            spawn_object(o, MODEL_WHITE_PARTICLE_SMALL, bhvSmallParticleSnow);
    } else
        o->oInteractStatus = 0;
    if (o->oTimer == 0)
        cur_obj_play_sound_2(SOUND_GENERAL_MOVING_WATER);
    if (o->oForwardVel == 0.0f)
        o->oForwardVel = 6.0f;
    dy = o->oPosY - gMarioObject->oPosY;
    if (o->oPosY < o->oCheepCheepWaterLevel - 50.0f) {
        if (dy < 0.0f)
            dy = 0.0f - dy;
        if (dy < 500.0f)
            bub_move_vertically(2);
        else
            bub_move_vertically(4);
    } else {
        o->oPosY = o->oCheepCheepWaterLevel - 50.0f;
        if (dy > 300.0f)
            o->oPosY -= 1.0f;
    }
    if (cur_obj_lateral_dist_from_mario_to_home() > 800.0f)
        o->oAngleToMario = cur_obj_angle_to_home();
    cur_obj_rotate_yaw_toward(o->oAngleToMario + 0x8000, 0x400);
    if (o->oTimer > 200 && o->oDistanceToMario > 600.0f)
        o->oAction = 1;
}

void (*sCheepCheepActions[])(void) = { bub_act_0, bub_act_1, bub_act_2 };

void bhv_bub_loop(void) {
    o->oCheepCheepWaterLevel = find_water_level(o->oPosX, o->oPosZ);
    o->oCheepCheepTargetY = gMarioObject->oPosY + o->oCheepCheepTargetYOffset;
    o->oWallHitboxRadius = 30.0f;
    cur_obj_update_floor_and_walls();
    cur_obj_call_action_function(sCheepCheepActions);
    cur_obj_move_using_fvel_and_gravity();
    if (o->parentObj->oAction == 2)
        obj_mark_for_deletion(o);
}
