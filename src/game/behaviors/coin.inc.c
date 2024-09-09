// coin.inc.c

struct ObjectHitbox sYellowCoinHitbox = {
    /* interactType:      */ INTERACT_COIN,
    /* downOffset:        */ 0,
    /* damageOrCoinValue: */ 1,
    /* health:            */ 0,
    /* numLootCoins:      */ 0,
    /* radius:            */ 100,
    /* height:            */ 64,
    /* hurtboxRadius:     */ 0,
    /* hurtboxHeight:     */ 0,
};

/**
 *     3
 *   4   6
 * 5   2   7
 * 
 *     1
 * 
 *     0
 **/
s16 sCoinArrowPositions[][2] = {
    { 0, -150 },
    { 0, -50 },
    { 0, 50 },
    { 0, 150 },
    { -50, 100 },
    { -100, 50 },
    { 50, 100 },
    { 100, 50 },
};

s32 bhv_coin_sparkles_init(void) {
    if (o->oInteractStatus & INT_STATUS_INTERACTED
        && !(o->oInteractStatus & INT_STATUS_TOUCHED_BOB_OMB)) {
        spawn_object(o, MODEL_SPARKLES, bhvCoinSparklesSpawner);
        obj_mark_for_deletion(o);
        return TRUE;
    }

    o->oInteractStatus = INT_STATUS_NONE;

    return FALSE;
}

void bhv_yellow_coin_init(void) {
    cur_obj_set_behavior(bhvYellowCoin);
    obj_set_hitbox(o, &sYellowCoinHitbox);
    cur_obj_update_floor_height();

    if (500.0f < absf(o->oPosY - o->oFloorHeight)) {
        if (cur_obj_has_model(MODEL_YELLOW_COIN)) {
            cur_obj_set_model(MODEL_YELLOW_COIN_NO_SHADOW);
        } else if (cur_obj_has_model(MODEL_BLUE_COIN)) {
            cur_obj_set_model(MODEL_BLUE_COIN_NO_SHADOW);
        } else if (cur_obj_has_model(MODEL_RED_COIN)) {
            cur_obj_set_model(MODEL_RED_COIN_NO_SHADOW);
#ifdef IA8_30FPS_COINS
        } else if (cur_obj_has_model(MODEL_SILVER_COIN)) {
            cur_obj_set_model(MODEL_SILVER_COIN_NO_SHADOW);
#endif
        }
    }

    if (o->oFloorHeight < FLOOR_LOWER_LIMIT_MISC) {
        obj_mark_for_deletion(o);
    }
}

void bhv_yellow_coin_loop(void) {
    bhv_coin_sparkles_init();
    o->oAnimState++;
}

void bhv_temp_coin_loop(void) {
    o->oAnimState++;

    if (cur_obj_wait_then_blink(200, 20)) {
        obj_mark_for_deletion(o);
    }

    bhv_coin_sparkles_init();
}

void bhv_coin_init(void) {
    o->oVelY = random_float() * 10.0f + 30 + o->oCoinBaseYVel;
    o->oForwardVel = random_float() * 10.0f;
    o->oMoveAngleYaw = random_u16();

    cur_obj_set_behavior(bhvYellowCoin);
    obj_set_hitbox(o, &sYellowCoinHitbox);
    cur_obj_become_intangible();
}

void bhv_coin_loop(void) {
    cur_obj_update_floor_and_walls();
    cur_obj_if_hit_wall_bounce_away();
    cur_obj_move_standard(-62);

    struct Surface *floor = o->oFloor;

    if (floor != NULL) {
        switch (o->oAction) {
            case BOUNCING_COIN_ACT_FALLING:
                if (o->oTimer == 0) {
                    cur_obj_play_sound_2(SOUND_GENERAL_COIN_SPURT);
                }
                if (o->oMoveFlags & OBJ_MOVE_LANDED) {
                    o->oAction = BOUNCING_COIN_ACT_BOUNCING;
                }
                
                break;

            case BOUNCING_COIN_ACT_BOUNCING:
                o->oBounciness = 0;
                if (floor->normal.y < 0.9f) {
                    s16 targetYaw = SURFACE_YAW(floor);
                    cur_obj_rotate_yaw_toward(targetYaw, 0x400);
                }
                break;

            case OBJ_ACT_LAVA_DEATH:
#ifdef COIN_LAVA_FLICKER
                obj_flicker_and_disappear(o, 0);
#else
                obj_mark_for_deletion(o);
#endif
                break;
        }
    }

    if (o->oVelY <= 0) {
        cur_obj_become_tangible();
    }

    if (o->oMoveFlags & OBJ_MOVE_LANDED) {
        if (o->oMoveFlags & OBJ_MOVE_ABOVE_DEATH_BARRIER) {
            obj_mark_for_deletion(o);
        } else if (o->oMoveFlags & OBJ_MOVE_ABOVE_LAVA) {
            o->oAction = OBJ_ACT_LAVA_DEATH;
        }
    }

    if (o->oMoveFlags & OBJ_MOVE_BOUNCE) {
        if (o->oCoinBounceTimer < 5) {
            cur_obj_play_sound_2(SOUND_GENERAL_COIN_DROP);
        }
        o->oCoinBounceTimer++;
    }

    if (cur_obj_wait_then_blink(400, 20)) {
        obj_mark_for_deletion(o);
    }

    bhv_coin_sparkles_init();
}

void bhv_coin_formation_spawned_coin_loop(void) {
    if (o->oTimer == 0) {
        cur_obj_set_behavior(bhvYellowCoin);
        obj_set_hitbox(o, &sYellowCoinHitbox);
        if (o->oCoinSnapToGround) {
            o->oPosY += 300.0f;
            cur_obj_update_floor_height();

            if (o->oPosY + FIND_FLOOR_BUFFER < o->oFloorHeight || o->oFloorHeight < FLOOR_LOWER_LIMIT_MISC) {
                obj_mark_for_deletion(o);
            } else {
                o->oPosY = o->oFloorHeight;
            }
        } else {
            cur_obj_update_floor_height();

            if (absf(o->oPosY - o->oFloorHeight) > 250.0f) {
                cur_obj_set_model(MODEL_YELLOW_COIN_NO_SHADOW);
            }
        }
    } else {
        if (bhv_coin_sparkles_init()) {
            o->parentObj->oCoinRespawnBits |= (1 << o->oBehParams2ndByte);
        }
        o->oAnimState++;
    }
    if (o->parentObj->oAction == COIN_FORMATION_ACT_DEACTIVATE) {
        obj_mark_for_deletion(o);
    }
}

void spawn_coin_in_formation(s32 index, s32 shape) {
    Vec3i pos = { 0, 0, 0 };
    s32 spawnCoin    = TRUE;
    s32 snapToGround = TRUE;

    switch (shape & COIN_FORMATION_BP_SHAPE_MASK) {
        case COIN_FORMATION_BP_SHAPE_HORIZONTAL_LINE:
            pos[2] = 160 * (index - 2);
            if (index > 4) spawnCoin = FALSE;
            break;
        case COIN_FORMATION_BP_SHAPE_VERTICAL_LINE:
            snapToGround = FALSE;
            pos[1] = index << 7;
            if (index > 4) spawnCoin = FALSE;
            break;
        case COIN_FORMATION_BP_SHAPE_HORIZONTAL_RING:
            pos[0] = sins(index << 13) * 300.0f;
            pos[2] = coss(index << 13) * 300.0f;
            break;
        case COIN_FORMATION_BP_SHAPE_VERTICAL_RING:
            snapToGround = FALSE;
            pos[0] = coss(index << 13) * 200.0f;
            pos[1] = sins(index << 13) * 200.0f + 200.0f;
            break;
        case COIN_FORMATION_BP_SHAPE_ARROW:
            pos[0] = sCoinArrowPositions[index][0];
            pos[2] = sCoinArrowPositions[index][1];
            break;
    }

    if (shape & COIN_FORMATION_BP_FLYING) {
        snapToGround = FALSE;
    }

    if (spawnCoin) {
        struct Object *newCoin =
            spawn_object_relative(index, pos[0], pos[1], pos[2], o,
                                  MODEL_YELLOW_COIN, bhvCoinFormationSpawnedCoin);
        newCoin->oCoinSnapToGround = snapToGround;
    }
}

void bhv_coin_formation_init(void) {
    o->oCoinRespawnBits = o->respawnInfo;
}

void bhv_coin_formation_loop(void) {
    s32 bitIndex;

    switch (o->oAction) {
        case COIN_FORMATION_ACT_INACTIVE:
            if (o->oDistanceToMario < COIN_FORMATION_DISTANCE) {
                for (bitIndex = 0; bitIndex < 8; bitIndex++) {
                    if (!(o->oCoinRespawnBits & (1 << bitIndex))) {
                        spawn_coin_in_formation(bitIndex, o->oBehParams2ndByte);
                    }
                }
                o->oAction = COIN_FORMATION_ACT_ACTIVE;
            }
            break;
        case COIN_FORMATION_ACT_ACTIVE:
            if (o->oDistanceToMario > (COIN_FORMATION_DISTANCE + 100.0f)) {
                o->oAction = COIN_FORMATION_ACT_DEACTIVATE;
            }
            break;
        case COIN_FORMATION_ACT_DEACTIVATE:
            o->oAction = COIN_FORMATION_ACT_INACTIVE;
            break;
    }

    set_object_respawn_info_bits(o, o->oCoinRespawnBits);
}

void coin_inside_boo_act_dropped(void) {
    cur_obj_update_floor_and_walls();
    cur_obj_if_hit_wall_bounce_away();

    if (o->oMoveFlags & OBJ_MOVE_BOUNCE) {
        cur_obj_play_sound_2(SOUND_GENERAL_COIN_DROP);
    }

    if (o->oTimer > 90 || o->oMoveFlags & OBJ_MOVE_LANDED) {
        obj_set_hitbox(o, &sYellowCoinHitbox);
        cur_obj_become_tangible();
        cur_obj_set_behavior(bhvYellowCoin);
    }

    cur_obj_move_standard(-30);
    bhv_coin_sparkles_init();

    if (cur_obj_has_model(MODEL_BLUE_COIN)) {
        o->oDamageOrCoinValue = 5;
    }

    if (cur_obj_wait_then_blink(400, 20)) {
        obj_mark_for_deletion(o);
    }
}

void coin_inside_boo_act_carried(void) {
    struct Object *parent = o->parentObj;

    cur_obj_become_intangible();
    if (o->oTimer == 0 && GET_BPARAM1(parent->oBehParams) != COIN_INSIDE_BOO_BP_YELLOW_COIN) {
        cur_obj_set_model(MODEL_BLUE_COIN);
        cur_obj_scale(0.7f);
    }

    obj_copy_pos(o, parent);

    if (parent->oBooDeathStatus == BOO_DEATH_STATUS_DYING) {
        o->oAction = COIN_INSIDE_BOO_ACT_DROPPED;
        s16 marioMoveYaw = gMarioObject->oMoveAngleYaw;
        o->oVelX = sins(marioMoveYaw) * 3.0f;
        o->oVelZ = coss(marioMoveYaw) * 3.0f;
        o->oVelY = 35.0f;
    }
}

ObjActionFunc sCoinInsideBooActions[] = {
    coin_inside_boo_act_carried,
    coin_inside_boo_act_dropped
};

void bhv_coin_inside_boo_loop(void) {
    cur_obj_call_action_function(sCoinInsideBooActions);
}

void bhv_coin_sparkles_loop(void) {
    cur_obj_scale(0.6f);
}

void bhv_golden_coin_sparkles_loop(void) {
    struct Object *sparkleObj = spawn_object(o, MODEL_SPARKLES, bhvCoinSparkles);
    sparkleObj->oPosX += random_float() * 30.0f - 15.0f;
    sparkleObj->oPosZ += random_float() * 30.0f - 15.0f;
}
