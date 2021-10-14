// flying_bookend_switch.inc.c

struct BookSwitchPosition {
    s16 relPosX;
    s16 relPosY;
};

struct ObjectHitbox sFlyingBookendHitbox = {
    /* interactType:      */ INTERACT_HIT_FROM_BELOW,
    /* downOffset:        */ 0,
    /* damageOrCoinValue: */ 2,
    /* health:            */ 0,
    /* numLootCoins:      */ -1,
    /* radius:            */ 60,
    /* height:            */ 30,
    /* hurtboxRadius:     */ 40,
    /* hurtboxHeight:     */ 30,
};

struct BookSwitchPosition sBookSwitchPositions[] = {
    { 52, 150 },
    { 135, 3 },
    { -75, 78 },
};

struct ObjectHitbox sBookSwitchHitbox = {
    /* interactType:      */ INTERACT_BREAKABLE,
    /* downOffset:        */ 0,
    /* damageOrCoinValue: */ 0,
    /* health:            */ 99,
    /* numLootCoins:      */ 0,
    /* radius:            */ 20,
    /* height:            */ 30,
    /* hurtboxRadius:     */ 20,
    /* hurtboxHeight:     */ 30,
};

void flying_bookend_act_0(void) {
    if (obj_is_near_to_and_facing_mario(400.0f, 0x3000)) {
        cur_obj_play_sound_2(SOUND_OBJ_DEFAULT_DEATH);

        o->oAction = 1;

        o->oBookendTargetPitch = o->oFaceAnglePitch + 0x7FFF;
        o->oBookendTargetRoll = o->oFaceAngleRoll - 0x7FFF;

        cur_obj_set_model(MODEL_BOOKEND_PART);
    }
}

void flying_bookend_act_1(void) {
    if (obj_forward_vel_approach(3.0f, 1.0f)) {
        if (cur_obj_init_anim_and_check_if_end(2)) {
            o->oAction = 2;
            o->oForwardVel = 0.0f;
        } else {
            o->oForwardVel = 3.0f;

            if (o->oTimer > 5) {
                obj_face_pitch_approach(o->oBookendTargetPitch, 2000);
                if (o->oTimer >= 10) {
                    obj_face_roll_approach(o->oBookendTargetRoll, 2000);
                    if (o->oTimer >= 20) {
                        approach_f32_ptr(&o->header.gfx.scale[0], 3.0f, 0.2f);
                    }
                }
            }
        }
    }

    cur_obj_move_using_fvel_and_gravity();
}

void flying_bookend_act_2(void) {
    cur_obj_init_animation_with_sound(1);
    cur_obj_update_floor_and_walls();

    if (o->oForwardVel == 0.0f) {
        obj_turn_pitch_toward_mario(120.0f, 1000);
        o->oFaceAnglePitch = o->oMoveAnglePitch + 0x7FFF;
        cur_obj_rotate_yaw_toward(o->oAngleToMario, 1000);

        if (o->oTimer > 30) {
            obj_compute_vel_from_move_pitch(50.0f);
        }
    }

    cur_obj_move_standard(78);
}

void flying_bookend_act_3(void) {
    o->oDamageOrCoinValue = 1;
    o->oNumLootCoins = 0;

    if (o->oTimer > 3) {
        o->oAction = 2;
        o->oForwardVel = 50.0f;
    }

    obj_forward_vel_approach(50.0f, 2.0f);
    cur_obj_move_using_fvel_and_gravity();
}

void bhv_flying_bookend_loop(void) {
    if (!(o->activeFlags & ACTIVE_FLAG_IN_DIFFERENT_ROOM)) {
        o->oDeathSound = SOUND_OBJ_POUNDING1;
        cur_obj_scale(o->header.gfx.scale[0]);

        switch (o->oAction) {
            case 0:
                flying_bookend_act_0();
                break;
            case 1:
                flying_bookend_act_1();
                break;
            case 2:
                flying_bookend_act_2();
                break;
            case 3:
                flying_bookend_act_3();
                break;
        }

        obj_check_attacks(&sFlyingBookendHitbox, -1);

        if (o->oAction == -1 || (o->oMoveFlags & (OBJ_MOVE_MASK_ON_GROUND | OBJ_MOVE_HIT_WALL))) {
            o->oNumLootCoins = 0;
            obj_die_if_health_non_positive();
        }

        o->oGraphYOffset = 30.0f * o->header.gfx.scale[0];
    }
}

void bhv_bookend_spawn_loop(void) {
    if (!(o->activeFlags & ACTIVE_FLAG_IN_DIFFERENT_ROOM)) {
        if (o->oTimer > 40 && obj_is_near_to_and_facing_mario(600.0f, 0x2000)) {
            struct Object *bookendObj = spawn_object(o, MODEL_BOOKEND, bhvFlyingBookend);

            if (bookendObj != NULL) {
                bookendObj->oAction = 3;
                cur_obj_play_sound_2(SOUND_OBJ_DEFAULT_DEATH);
            }

            o->oTimer = 0;
        }
    }
}

void bookshelf_manager_act_0(void) {
    s32 i;

    if (!(o->activeFlags & ACTIVE_FLAG_IN_DIFFERENT_ROOM)) {
        for (i = 0; i < 3; i++) {
            spawn_object_relative(i, sBookSwitchPositions[i].relPosX, sBookSwitchPositions[i].relPosY, 0, o, 
                                  MODEL_BOOKEND, bhvBookSwitch);
        }

        o->oAction = 1;
    }
}

void bookshelf_manager_act_1(void) {
    if (o->oBookSwitchManagerIsActive == 0) {
        if (obj_is_near_to_and_facing_mario(500.0f, 0x3000)) {
            o->oBookSwitchManagerIsActive = 1;
        }
    } else if (o->oTimer > 60) {
        o->oAction = 2;
        o->oBookSwitchManagerIsActive = 0;
    }
}

void bookshelf_manager_act_2(void) {
    if (!(o->activeFlags & ACTIVE_FLAG_IN_DIFFERENT_ROOM)) {
        if (o->oBookSwitchManagerNumCorrectChoices < 0) {
            if (o->oTimer > 30) {
                o->oBookSwitchManagerNumCorrectChoices = o->oBookSwitchManagerIsActive = 0;
            } else if (o->oTimer > 10) {
                o->oBookSwitchManagerIsActive = 1;
            }
        } else {
            if (o->oBookSwitchManagerNumCorrectChoices >= 3) {
                if (o->oTimer > 100) {
                    o->parentObj = cur_obj_nearest_object_with_behavior(bhvHauntedBookshelf);
                    o->parentObj->oAction = 1;
                    o->oPosX = o->parentObj->oPosX;
                    o->oAction = 3;
                } else if (o->oTimer == 30) {
                    play_puzzle_jingle();
                }
            } else {
                o->oTimer = 0;
            }
        }
    } else {
        o->oAction = 4;
    }
}

void bookshelf_manager_act_3(void) {
    if (o->oTimer > 85) {
        o->oAction = 4;
    } else {
        o->oForwardVel = o->parentObj->oPosX - o->oPosX;
        o->oPosX = o->parentObj->oPosX;
    }
}

void bookshelf_manager_act_4(void) {
    if (o->oBookSwitchManagerNumCorrectChoices >= 3) {
        obj_mark_for_deletion(o);
    } else {
        o->oAction = 0;
    }
}

void bhv_haunted_bookshelf_manager_loop(void) {
    switch (o->oAction) {
        case 0:
            bookshelf_manager_act_0();
            break;
        case 2:
            bookshelf_manager_act_2();
            break;
        case 1:
            bookshelf_manager_act_1();
            break;
        case 3:
            bookshelf_manager_act_3();
            break;
        case 4:
            bookshelf_manager_act_4();
            break;
    }
}

void bhv_book_switch_loop(void) {
    o->header.gfx.scale[0] = 2.0f;
    o->header.gfx.scale[1] = 0.9f;

    if (o->parentObj->oAction == 4) {
        obj_mark_for_deletion(o);
    } else {
        s32 attackType = obj_check_attacks(&sBookSwitchHitbox, o->oAction);

        if (o->parentObj->oBookSwitchManagerIsActive != 0 || o->oAction == 1) {
            if (o->oDistanceToMario < 100.0f) {
                cur_obj_become_tangible();
            } else {
                cur_obj_become_intangible();
            }

            o->oAction = 1;

            if (o->oBookSwitchDistFromHome == 0.0f) {
                cur_obj_play_sound_2(SOUND_OBJ_DEFAULT_DEATH);
            }

            if (approach_f32_ptr(&o->oBookSwitchDistFromHome, 50.0f, 20.0f)) {
                if (o->parentObj->oBookSwitchManagerNumCorrectChoices >= 0 && o->oTimer > 60
                    && (attackType == ATTACK_PUNCH || attackType == ATTACK_KICK_OR_TRIP
                        || attackType == ATTACK_FROM_BELOW)) {
                    o->oAction = 2;
                }
            } else {
                o->oTimer = 0;
            }
        } else {
            cur_obj_become_intangible();

            if (approach_f32_ptr(&o->oBookSwitchDistFromHome, 0.0f, 20.0f) && (o->oAction != 0)) {
                if (o->parentObj->oBookSwitchManagerNumCorrectChoices == o->oBehParams2ndByte) {
                    play_sound(SOUND_GENERAL2_RIGHT_ANSWER, gGlobalSoundSource);
                    o->parentObj->oBookSwitchManagerNumCorrectChoices++;
                } else {
                    s16 rand01 = random_u16() & 0x1;
                    s16 z = gMarioObject->oPosZ + 1.5f * gMarioStates[0].vel[2];

                    play_sound(SOUND_MENU_CAMERA_BUZZ, gGlobalSoundSource);

                    if (z > 0) z = 0;

                    struct Object *bookendObj = spawn_object_abs_with_rot(o, 0, MODEL_BOOKEND, bhvFlyingBookend,
                                                     0x1FC * rand01 - 0x8CA, 890, z, 0,
                                                     0x8000 * rand01 + 0x4000, 0);

                    if (bookendObj != NULL) {
                        bookendObj->oAction = 3;
                    }

                    o->parentObj->oBookSwitchManagerNumCorrectChoices = -1;
                }

                o->oAction = 0;
            }
        }

        o->oPosX += o->parentObj->oForwardVel;
        o->oPosZ = o->oHomeZ - o->oBookSwitchDistFromHome;
        cur_obj_push_mario_away_from_cylinder(70.0f, 70.0f);
    }
}
