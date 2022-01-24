// donut_platform.inc.c

void bhv_donut_row_init(void) {
    s32 i;
    for (i = 0; i < o->oBehParams2ndByte; i++) {
        spawn_object_relative(OBJ_BP_NONE,
            (i * 204.8f), 0, 0,
            o, MODEL_NONE, bhvDonutPlatformSpawner);
    }
}

void bhv_donut_platform_spawner_update(void) {
    if (o->oDonutPlatformObject == NULL) {
        if (o->oDistanceToMario > 1000.0f && o->oDistanceToMario < 2000.0f) {
            o->oDonutPlatformObject = spawn_object_relative(OBJ_BP_NONE, 0, 0, 0,
                                                            o, MODEL_RR_DONUT_PLATFORM, bhvDonutPlatform);
        }
    }
}

void bhv_donut_platform_update(void) {
    if (o->oTimer != 0 && ((o->oMoveFlags & OBJ_MOVE_MASK_ON_GROUND) || o->oDistanceToMario > 2500.0f)) {
        o->parentObj->oDonutPlatformObject = NULL;

        if (o->oDistanceToMario > 2500.0f) {
            obj_mark_for_deletion(o);
        } else {
            obj_explode_and_spawn_coins(150.0f, COIN_TYPE_YELLOW);
            create_sound_spawner(SOUND_GENERAL_DONUT_PLATFORM_EXPLOSION);
        }
    } else {
        if (o->oGravity == 0.0f) {
            if (gMarioObject->platform == o) {
                cur_obj_shake_y(4.0f);
                if (o->oTimer > 15) {
                    o->oGravity = -0.1f;
                }
            } else {
                cur_obj_set_pos_to_home();
                o->oTimer = 0;
            }
        } else {
            cur_obj_update_floor_and_walls();
            cur_obj_move_standard(78);
        }

        load_object_collision_model();
    }
}
