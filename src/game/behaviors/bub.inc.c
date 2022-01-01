// bub.inc.c

// NOTE: These first set of functions spawn a school of bub depending on oCheepCheepSpawnerSpawnAmount's
// value. The later action functions seem to check Y distance to Mario and proceed
// to do nothing, which indicates this behavior set is incomplete.

void bub_spawner_act_spawn_bubs(void) {
    s32 i;
    s32 amt = o->oCheepCheepSpawnerSpawnAmount;
    if (o->oDistanceToMario < 1500.0f) {
        for (i = 0; i < amt; i++) {
            spawn_object(o, MODEL_BUB, bhvBub);
        }
        o->oAction = BUB_SPAWNER_ACT_IDLE;
    }
}

void bub_spawner_act_idle(void) {
    if ((gMarioObject->oPosY - o->oPosY) > 2000.0f) {
        o->oAction = BUB_SPAWNER_ACT_REMOVE_BUBS;
    }
}

void bub_spawner_act_remove_bubs(void) {
    o->oAction = BUB_SPAWNER_ACT_RESET;
}

void bub_spawner_act_reset(void) {
    o->oAction = BUB_SPAWNER_ACT_SPAWN_BUBS;
}

ObjActionFunc sBirdChirpChirpActions[] = {
    bub_spawner_act_spawn_bubs,
    bub_spawner_act_idle,
    bub_spawner_act_remove_bubs,
    bub_spawner_act_reset,
};

void bhv_bub_spawner_loop(void) {
    cur_obj_call_action_function(sBirdChirpChirpActions);
}

void bub_move_vertically(s32 ySpeed) {
    f32 parentY = o->parentObj->oPosY;
    if (
        parentY - 100.0f - o->oCheepCheepMaxYOffset < o->oPosY
        && o->oPosY < parentY + 1000.0f + o->oCheepCheepMaxYOffset
    ) {
        o->oPosY = approach_f32_symmetric(o->oPosY, o->oCheepCheepTargetY, ySpeed);
    }
}

void bub_act_init(void) {
    o->oCheepCheepTargetYOffset = random_float() * 100.0f;
    o->oCheepCheepMaxYOffset    = random_float() * 300.0f;
    o->oAction = BUB_ACT_SWIMMING_TOWARDS_MARIO;
}

void bub_act_swimming_towards_mario(void) {
    if (o->oTimer == 0) {
        o->oForwardVel = random_float() * 2 + 2;
        o->oCheepCheepRandomSwimAway = random_float();
    }

    f32 dy = o->oPosY - gMarioObject->oPosY;

    if (o->oPosY < o->oCheepCheepWaterLevel - 50.0f) {
        if (absf(dy) < 500.0f) {
            bub_move_vertically(1);
        } else {
            bub_move_vertically(4);
        }
    } else {
        o->oPosY = o->oCheepCheepWaterLevel - 50.0f;

        if (dy > 300.0f) {
            o->oPosY -= 1.0f;
        }
    }

    if (800.0f < cur_obj_lateral_dist_from_mario_to_home()) {
        o->oAngleToMario = cur_obj_angle_to_home();
    }

    cur_obj_rotate_yaw_toward(o->oAngleToMario, 0x100);
    if (o->oDistanceToMario < 200.0f && o->oCheepCheepRandomSwimAway < 0.5f) {
        o->oAction = BUB_ACT_SWIMMING_AWAY_FROM_MARIO;
    }

    if (o->oInteractStatus & INT_STATUS_INTERACTED) {
        o->oAction = BUB_ACT_SWIMMING_AWAY_FROM_MARIO;
    }
}

void bub_act_swimming_away_from_mario(void) {
    if (o->oTimer < 20) {
        if (o->oInteractStatus & INT_STATUS_INTERACTED) {
            spawn_object(o, MODEL_WHITE_PARTICLE_SMALL, bhvSmallParticleSnow);
        }
    } else {
        o->oInteractStatus = INT_STATUS_NONE;
    }

    if (o->oTimer == 0) {
        cur_obj_play_sound_2(SOUND_GENERAL_MOVING_WATER);
    }

    if (o->oForwardVel == 0.0f) {
        o->oForwardVel = 6.0f;
    }

    f32 dy = o->oPosY - gMarioObject->oPosY;

    if (o->oPosY < o->oCheepCheepWaterLevel - 50.0f) {
        if (absf(dy) < 500.0f) {
            bub_move_vertically(2);
        } else {
            bub_move_vertically(4);
        }
    } else {
        o->oPosY = o->oCheepCheepWaterLevel - 50.0f;

        if (dy > 300.0f) {
            o->oPosY -= 1.0f;
        }
    }

    if (cur_obj_lateral_dist_from_mario_to_home() > 800.0f) {
        o->oAngleToMario = cur_obj_angle_to_home();
    }

    cur_obj_rotate_yaw_toward(o->oAngleToMario + 0x8000, 0x400);

    if (o->oTimer > 200 && o->oDistanceToMario > 600.0f) {
        o->oAction = BUB_ACT_SWIMMING_TOWARDS_MARIO;
    }
}

ObjActionFunc sCheepCheepActions[] = {
    bub_act_init,
    bub_act_swimming_towards_mario,
    bub_act_swimming_away_from_mario,
};

void bhv_bub_loop(void) {
    o->oCheepCheepWaterLevel = find_water_level(o->oPosX, o->oPosZ);
    o->oCheepCheepTargetY = gMarioObject->oPosY + o->oCheepCheepTargetYOffset;
    o->oWallHitboxRadius = 30.0f;

    cur_obj_update_floor_and_walls();
    cur_obj_call_action_function(sCheepCheepActions);
    cur_obj_move_using_fvel_and_gravity();

    if (o->parentObj->oAction == BUB_SPAWNER_ACT_REMOVE_BUBS) {
        obj_mark_for_deletion(o);
    }
}
