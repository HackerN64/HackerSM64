// exclamation_box.inc.c

struct ObjectHitbox sExclamationBoxHitbox = {
    .interactType      = INTERACT_BREAKABLE,
    .downOffset        = 5,
    .damageOrCoinValue = 0,
    .health            = 1,
    .numLootCoins      = 0,
    .radius            = 40,
    .height            = 30,
    .hurtboxRadius     = 40,
    .hurtboxHeight     = 30,
};

struct ExclamationBoxContents {
    ModelID32 model;
    const BehaviorScript *behavior;
    u8 behParam;
};

struct ExclamationBoxContents sExclamationBoxContents[] = {
    [EXCLAMATION_BOX_BP_WING_CAP        ] = { MODEL_MARIOS_WING_CAP,  bhvWingCap,               0 },
    [EXCLAMATION_BOX_BP_METAL_CAP       ] = { MODEL_MARIOS_METAL_CAP, bhvMetalCap,              0 },
    [EXCLAMATION_BOX_BP_VANISH_CAP      ] = { MODEL_MARIOS_CAP,       bhvVanishCap,             0 },
    [EXCLAMATION_BOX_BP_KOOPA_SHELL     ] = { MODEL_KOOPA_SHELL,      bhvKoopaShell,            0 },
    [EXCLAMATION_BOX_BP_COINS_1         ] = { MODEL_YELLOW_COIN,      bhvSingleCoinGetsSpawned, 0 },
    [EXCLAMATION_BOX_BP_COINS_3         ] = { MODEL_NONE,             bhvThreeCoinsSpawn,       0 },
    [EXCLAMATION_BOX_BP_COINS_10        ] = { MODEL_NONE,             bhvTenCoinsSpawn,         0 },
    [EXCLAMATION_BOX_BP_1UP_WALKING     ] = { MODEL_1UP,              bhv1upWalking,            0 },
    [EXCLAMATION_BOX_BP_STAR_1          ] = { MODEL_STAR,             bhvSpawnedStar,           0 },
    [EXCLAMATION_BOX_BP_1UP_RUNNING_AWAY] = { MODEL_1UP,              bhv1upRunningAway,        0 },
    [EXCLAMATION_BOX_BP_STAR_2          ] = { MODEL_STAR,             bhvSpawnedStar,           1 },
    [EXCLAMATION_BOX_BP_STAR_3          ] = { MODEL_STAR,             bhvSpawnedStar,           2 },
    [EXCLAMATION_BOX_BP_STAR_4          ] = { MODEL_STAR,             bhvSpawnedStar,           3 },
    [EXCLAMATION_BOX_BP_STAR_5          ] = { MODEL_STAR,             bhvSpawnedStar,           4 },
    [EXCLAMATION_BOX_BP_STAR_6          ] = { MODEL_STAR,             bhvSpawnedStar,           5 },
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
        const u8 tangible = TRUE;
#else
        const u8 tangible = ((save_file_get_flags() & sCapSaveFlags[o->oBehParams2ndByte])
                          || (GET_BPARAM1(o->oBehParams) != EXCLAMATION_BOX_BP1_NEEDS_SWITCH));
#endif
        o->oAction = tangible ? EXCLAMATION_BOX_ACT_ACTIVE : EXCLAMATION_BOX_ACT_OUTLINE;
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
    o->oGraphYOffset                  = ((-sins(o->oExclamationBoxScaleAngle) + 1.0f) * 26.0f);
    o->oExclamationBoxHorizontalScale = ((-sins(o->oExclamationBoxScaleAngle) + 1.0f) *  0.5f) + 1.0f;
    o->oExclamationBoxVerticalScale   = (( sins(o->oExclamationBoxScaleAngle) + 1.0f) *  0.3f) + 0.0f;
    o->oExclamationBoxScaleAngle += 0x1000;
    obj_scale_xyz(o,
        (o->oExclamationBoxHorizontalScale * 2.0f),
        (o->oExclamationBoxVerticalScale   * 2.0f),
        (o->oExclamationBoxHorizontalScale * 2.0f)
    );
    if (o->oTimer == 7) {
        o->oAction = EXCLAMATION_BOX_ACT_EXPLODE;
    }
}

void exclamation_box_spawn_contents(struct ExclamationBoxContents *contentsList, u8 boxType) {
    struct Object *contentsObj = NULL;

    if (boxType < ARRAY_COUNT(sExclamationBoxContents)) {
        struct ExclamationBoxContents *contents = &contentsList[boxType];

        contentsObj = spawn_object(o, contents->model, contents->behavior);
        contentsObj->oVelY = 20.0f;
        contentsObj->oForwardVel = 3.0f;
        contentsObj->oMoveAngleYaw = gMarioObject->oMoveAngleYaw;
        OR_BPARAM1(o->oBehParams, contents->behParam);
        if (contents->model == MODEL_STAR) {
            o->oFlags |= OBJ_FLAG_PERSISTENT_RESPAWN;
        }
    }
}

void exclamation_box_act_explode(void) {
    exclamation_box_spawn_contents(sExclamationBoxContents, o->oBehParams2ndByte);
    spawn_mist_particles_variable(0, 0, 46.0f);
    spawn_triangle_break_particles(20, MODEL_CARTOON_STAR, 0.3f, o->oAnimState);
    create_sound_spawner(SOUND_GENERAL_BREAK_BOX);
    if (o->oBehParams2ndByte <= EXCLAMATION_BOX_BP_KOOPA_SHELL) {
        // Cap boxes + Koopa shell boxes.
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
