// exclamation_box.inc.c

struct ExclamationBoxContents {
    u8 id;
    u8 unk1;
    u8 behParams;
    ModelID16 model;
    const BehaviorScript *behavior;
};

struct ObjectHitbox sExclamationBoxHitbox = {
    /* interactType:      */ INTERACT_BREAKABLE,
    /* downOffset:        */ 5,
    /* damageOrCoinValue: */ 0,
    /* health:            */ 1,
    /* numLootCoins:      */ 0,
    /* radius:            */ 40,
    /* height:            */ 30,
    /* hurtboxRadius:     */ 40,
    /* hurtboxHeight:     */ 30,
};

struct ExclamationBoxContents sExclamationBoxContents[] = {
    { EXCLAMATION_BOX_BP_WING_CAP,         0, 0, MODEL_MARIOS_WING_CAP,  bhvWingCap               },
    { EXCLAMATION_BOX_BP_METAL_CAP,        0, 0, MODEL_MARIOS_METAL_CAP, bhvMetalCap              },
    { EXCLAMATION_BOX_BP_VANISH_CAP,       0, 0, MODEL_MARIOS_CAP,       bhvVanishCap             },
    { EXCLAMATION_BOX_BP_KOOPA_SHELL,      0, 0, MODEL_KOOPA_SHELL,      bhvKoopaShell            },
    { EXCLAMATION_BOX_BP_COINS_1,          0, 0, MODEL_YELLOW_COIN,      bhvSingleCoinGetsSpawned },
    { EXCLAMATION_BOX_BP_COINS_3,          0, 0, MODEL_NONE,             bhvThreeCoinsSpawn       },
    { EXCLAMATION_BOX_BP_COINS_10,         0, 0, MODEL_NONE,             bhvTenCoinsSpawn         },
    { EXCLAMATION_BOX_BP_1UP_WALKING,      0, 0, MODEL_1UP,              bhv1upWalking            },
    { EXCLAMATION_BOX_BP_STAR_1,           0, 0, MODEL_STAR,             bhvSpawnedStar           },
    { EXCLAMATION_BOX_BP_1UP_RUNNING_AWAY, 0, 0, MODEL_1UP,              bhv1upRunningAway        },
    { EXCLAMATION_BOX_BP_STAR_2,           0, 1, MODEL_STAR,             bhvSpawnedStar           },
    { EXCLAMATION_BOX_BP_STAR_3,           0, 2, MODEL_STAR,             bhvSpawnedStar           },
    { EXCLAMATION_BOX_BP_STAR_4,           0, 3, MODEL_STAR,             bhvSpawnedStar           },
    { EXCLAMATION_BOX_BP_STAR_5,           0, 4, MODEL_STAR,             bhvSpawnedStar           },
    { EXCLAMATION_BOX_BP_STAR_6,           0, 5, MODEL_STAR,             bhvSpawnedStar           },
    { EXCLAMATION_BOX_BP_NULL,             0, 0, MODEL_NONE,             NULL                     }
};

void bhv_rotating_exclamation_mark_loop(void) {
    if (o->parentObj->oAction != EXCLAMATION_BOX_ACT_OUTLINE) {
        obj_mark_for_deletion(o);
    }
}

void exclamation_box_act_init(void) {
    if (o->oBehParams2ndByte < EXCLAMATION_BOX_BP_KOOPA_SHELL) {
        o->oAnimState = o->oBehParams2ndByte;
#ifdef UNLOCK_ALL
        u8 tangible = TRUE;
#else
        u8 tangible = ((save_file_get_flags() & sCapSaveFlags[o->oBehParams2ndByte])
                    || (GET_BPARAM1(o->oBehParams) != EXCLAMATION_BOX_BP1_NEEDS_SWITCH));
#endif
        if (tangible) {
            o->oAction = EXCLAMATION_BOX_ACT_ACTIVE;
        } else {
            o->oAction = EXCLAMATION_BOX_ACT_OUTLINE;
        }
    } else {
        o->oAnimState = EXCLAMATION_BOX_ANIM_STATE_YELLOW;
        o->oAction = EXCLAMATION_BOX_ACT_ACTIVE;
    }
}

void exclamation_box_act_outline(void) {
    cur_obj_become_intangible();
    if (o->oTimer == 0) {
        spawn_object(o, MODEL_EXCLAMATION_POINT, bhvRotatingExclamationMark);
        cur_obj_set_model(MODEL_EXCLAMATION_BOX_OUTLINE);
    }
    if ((save_file_get_flags() & sCapSaveFlags[o->oBehParams2ndByte])
     || (GET_BPARAM1(o->oBehParams) != EXCLAMATION_BOX_BP1_NEEDS_SWITCH)) {
        o->oAction = EXCLAMATION_BOX_ACT_ACTIVE;
        cur_obj_set_model(MODEL_EXCLAMATION_BOX);
    }
}

void exclamation_box_act_active(void) {
    obj_set_hitbox(o, &sExclamationBoxHitbox);
    if (o->oTimer == 0) {
        cur_obj_unhide();
        cur_obj_become_tangible();
        o->oInteractStatus = INT_STATUS_NONE;
        o->oPosY = o->oHomeY;
        o->oGraphYOffset = 0.0f;
    }
    if (cur_obj_was_attacked_or_ground_pounded()) {
        cur_obj_become_intangible();
        o->oExclamationBoxScaleAngle = 0x4000;
        o->oVelY = 30.0f;
        o->oGravity = -8.0f;
        o->oFloorHeight = o->oPosY;
        o->oAction = EXCLAMATION_BOX_ACT_SCALING;
#if ENABLE_RUMBLE
        queue_rumble_data(5, 80);
#endif
    }
    load_object_collision_model();
}

void exclamation_box_act_scaling(void) {
    cur_obj_move_using_fvel_and_gravity();
    if (o->oVelY < 0.0f) {
        o->oVelY = 0.0f;
        o->oGravity = 0.0f;
    }
    o->oExclamationBoxVerticalScale = (sins(o->oExclamationBoxScaleAngle) + 1.0f) * 0.3f + 0.0f;
    o->oExclamationBoxHorizontalScale = (-sins(o->oExclamationBoxScaleAngle) + 1.0f) * 0.5f + 1.0f;
    o->oGraphYOffset = (-sins(o->oExclamationBoxScaleAngle) + 1.0f) * 26.0f;
    o->oExclamationBoxScaleAngle += 0x1000;
    o->header.gfx.scale[0] = o->oExclamationBoxHorizontalScale * 2.0f;
    o->header.gfx.scale[1] = o->oExclamationBoxVerticalScale   * 2.0f;
    o->header.gfx.scale[2] = o->oExclamationBoxHorizontalScale * 2.0f;
    if (o->oTimer == 7) {
        o->oAction = EXCLAMATION_BOX_ACT_EXPLODE;
    }
}

void exclamation_box_spawn_contents(struct ExclamationBoxContents *contentsList, u8 boxType) {
    struct Object *contentsObj = NULL;

    while (contentsList->id != EXCLAMATION_BOX_BP_NULL) {
        if (boxType == contentsList->id) {
            contentsObj = spawn_object(o, contentsList->model, contentsList->behavior);
            contentsObj->oVelY = 20.0f;
            contentsObj->oForwardVel = 3.0f;
            contentsObj->oMoveAngleYaw = gMarioObject->oMoveAngleYaw;
            OR_BPARAM1(o->oBehParams, contentsList->behParams);
            if (contentsList->model == MODEL_STAR) {
                o->oFlags |= OBJ_FLAG_PERSISTENT_RESPAWN;
            }
            break;
        }
        contentsList++;
    }
}

void exclamation_box_act_explode(void) {
    exclamation_box_spawn_contents(sExclamationBoxContents, o->oBehParams2ndByte);
    spawn_mist_particles_variable(0, 0, 46.0f);
    spawn_triangle_break_particles(20, MODEL_CARTOON_STAR, 0.3f, o->oAnimState);
    create_sound_spawner(SOUND_GENERAL_BREAK_BOX);
    if (o->oBehParams2ndByte < EXCLAMATION_BOX_BP_COINS_1) {
        o->oAction = EXCLAMATION_BOX_ACT_WAIT_FOR_RESPAWN;
        cur_obj_hide();
    } else {
        obj_mark_for_deletion(o);
    }
}

void exclamation_box_act_wait_for_respawn(void) {
    if (o->oTimer > 300) {
        o->oAction = EXCLAMATION_BOX_ACT_ACTIVE;
    }
}

ObjActionFunc sExclamationBoxActions[] = {
    exclamation_box_act_init,
    exclamation_box_act_outline,
    exclamation_box_act_active,
    exclamation_box_act_scaling,
    exclamation_box_act_explode,
    exclamation_box_act_wait_for_respawn,
};

void bhv_exclamation_box_loop(void) {
    cur_obj_scale(2.0f);
    cur_obj_call_action_function(sExclamationBoxActions);
}
