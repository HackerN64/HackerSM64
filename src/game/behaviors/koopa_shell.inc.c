// koopa_shell.inc.c

struct ObjectHitbox sKoopaShellHitbox = {
    /* interactType:      */ INTERACT_KOOPA_SHELL,
    /* downOffset:        */ 0,
    /* damageOrCoinValue: */ 4,
    /* health:            */ 1,
    /* numLootCoins:      */ 1,
    /* radius:            */ 50,
    /* height:            */ 50,
    /* hurtboxRadius:     */ 50,
    /* hurtboxHeight:     */ 50,
};

void shell_despawn(void) {
    if (o->oTimer > 300) obj_flicker_and_disappear(o, 300);
}

void koopa_shell_spawn_water_drop(void) {
    spawn_object(o, MODEL_WAVE_TRAIL, bhvObjectWaveTrail);

    if (gMarioStates[0].forwardVel > 10.0f) {
        struct Object *drop = spawn_object_with_scale(o, MODEL_WHITE_PARTICLE_SMALL, bhvWaterDroplet, 1.5f);
        drop->oVelY = random_float() * 30.0f;
        obj_translate_xz_random(drop, 110.0f);
    }
}

void bhv_koopa_shell_flame_loop(void) {
    if (o->oTimer == 0) {
        o->oMoveAngleYaw = random_u16();
        o->oVelY = random_float() * 30.0f;
        o->oGravity = -4.0f;
        o->oAnimState = random_float() * 10.0f;
        obj_translate_xz_random(o, 110.0f);
        o->oKoopaShellFlameScale = 4.0f;
    }

    cur_obj_update_floor_height();
    cur_obj_move_using_fvel_and_gravity();

    if (o->oFloorHeight > o->oPosY || o->oTimer > 10) {
        obj_mark_for_deletion(o);
    }

    o->oKoopaShellFlameScale -= 0.3f;
    cur_obj_scale(o->oKoopaShellFlameScale);
}

void bhv_koopa_shell_flame_spawn(void) {
    s32 i;
    for (i = 0; i < 2; i++) {
        spawn_object(o, MODEL_RED_FLAME, bhvKoopaShellFlame);
    }
}

void koopa_shell_spawn_sparkles(f32 a) {
    struct Object *sparkleObj = spawn_object(o, MODEL_NONE, bhvSparkleSpawn);
    sparkleObj->oPosY += a;
}

void bhv_koopa_shell_loop(void) {
    struct Surface *floor;

    obj_set_hitbox(o, &sKoopaShellHitbox);
    cur_obj_scale(1.0f);

    switch (o->oAction) {
        case KOOPA_SHELL_ACT_MARIO_NOT_RIDING:
            cur_obj_update_floor_and_walls();
            cur_obj_if_hit_wall_bounce_away();

            if (o->oInteractStatus & INT_STATUS_INTERACTED) {
                o->oAction = KOOPA_SHELL_ACT_MARIO_RIDING;
            }

            o->oFaceAngleYaw += 0x1000;
            cur_obj_move_standard(-20);
            koopa_shell_spawn_sparkles(10.0f);
            shell_despawn();
            break;

        case KOOPA_SHELL_ACT_MARIO_RIDING:
            obj_copy_pos(o, gMarioObject);
            floor = cur_obj_update_floor_height_and_get_floor();

            if (absf(find_water_level(o->oPosX, o->oPosZ) - o->oPosY) < 10.0f) {
                koopa_shell_spawn_water_drop();
            } else if (absf(o->oPosY - o->oFloorHeight) < 5.0f) {
                if (floor != NULL && floor->type == SURFACE_BURNING) {
                    bhv_koopa_shell_flame_spawn();
                } else {
                    koopa_shell_spawn_sparkles(10.0f);
                }
            } else {
                koopa_shell_spawn_sparkles(10.0f);
            }

            o->oFaceAngleYaw = gMarioObject->oMoveAngleYaw;

            if (o->oInteractStatus & INT_STATUS_STOP_RIDING) {
                obj_mark_for_deletion(o);
                spawn_mist_particles();
                o->oAction = KOOPA_SHELL_ACT_MARIO_NOT_RIDING;
            }
            break;
    }

    o->oInteractStatus = INT_STATUS_NONE;
}
