// treasure_chest.inc.c

/**
 * Hitbox for treasure chest bottom.
 */
static struct ObjectHitbox sTreasureChestBottomHitbox = {
    /* interactType:      */ INTERACT_SHOCK,
    /* downOffset:        */ 0,
    /* damageOrCoinValue: */ 1,
    /* health:            */ 0,
    /* numLootCoins:      */ 0,
    /* radius:            */ 300,
    /* height:            */ 300,
    /* hurtboxRadius:     */ 310,
    /* hurtboxHeight:     */ 310,
};

void bhv_treasure_chest_top_loop(void) {
    struct Object *parent = o->parentObj->parentObj;

    switch (o->oAction) {
        case TREASURE_CHEST_TOP_ACT_CLOSED:
            if (o->parentObj->oAction == TREASURE_CHEST_BOTTOM_ACT_OPENING) {
                o->oAction = TREASURE_CHEST_TOP_ACT_OPENING;
            }
            break;

        case TREASURE_CHEST_TOP_ACT_OPENING:
            if (o->oTimer == 0) {
                if (parent->oTreasureChestAboveWater) {
                    play_sound(SOUND_GENERAL_OPEN_CHEST, o->header.gfx.cameraToObject);
                } else {
                    spawn_object_relative(0, 0, -80, 120, o, MODEL_BUBBLE, bhvWaterAirBubble);
                    play_sound(SOUND_GENERAL_OPEN_CHEST_WATER, o->header.gfx.cameraToObject);
                }
            }

            o->oFaceAnglePitch -= 0x200;
            if (o->oFaceAnglePitch < -0x4000) {
                o->oFaceAnglePitch = -0x4000;
                o->oAction = TREASURE_CHEST_TOP_ACT_OPENED;
                if (o->parentObj->oBehParams2ndByte != TREASURE_CHEST_BP_4) {
                    spawn_orange_number(o->parentObj->oBehParams2ndByte, 0, -40, 0);
                }
            }
            break;

        case TREASURE_CHEST_TOP_ACT_OPENED:
            if (o->parentObj->oAction == TREASURE_CHEST_BOTTOM_ACT_CLOSE) {
                o->oAction = TREASURE_CHEST_TOP_ACT_CLOSING;
            }
            break;

        case TREASURE_CHEST_TOP_ACT_CLOSING:
            o->oFaceAnglePitch += 0x800;
            if (o->oFaceAnglePitch > 0x0) {
                o->oFaceAnglePitch = 0x0;
                o->oAction = TREASURE_CHEST_TOP_ACT_CLOSED;
            }
    }
}

void bhv_treasure_chest_bottom_init(void) {
    spawn_object_relative(0, 0, 102, -77, o, MODEL_TREASURE_CHEST_LID, bhvTreasureChestTop);
    obj_set_hitbox(o, &sTreasureChestBottomHitbox);
}

void bhv_treasure_chest_bottom_loop(void) {
    switch (o->oAction) {
        case TREASURE_CHEST_BOTTOM_ACT_CLOSE:
            if ((!o->parentObj->oTreasureChestDoCloseChests)
                && is_point_within_radius_of_mario(o->oPosX, o->oPosY, o->oPosZ, 150)
                && obj_check_if_facing_toward_angle(o->oMoveAngleYaw, gMarioObject->header.gfx.angle[1] + 0x8000, 0x3000)) {
                if (o->parentObj->oTreasureChestNumOpenedChests == o->oBehParams2ndByte) {
                    play_sound(SOUND_GENERAL2_RIGHT_ANSWER, gGlobalSoundSource);
                    o->parentObj->oTreasureChestNumOpenedChests++;
                    o->oAction = TREASURE_CHEST_BOTTOM_ACT_OPENING;
                } else {
                    o->parentObj->oTreasureChestNumOpenedChests = 1;
                    o->parentObj->oTreasureChestDoCloseChests = TRUE;
                    o->oAction = TREASURE_CHEST_BOTTOM_ACT_OPENED;
                    cur_obj_become_tangible();
                    play_sound(SOUND_MENU_CAMERA_BUZZ, gGlobalSoundSource);
                }
            }
            break;

        case TREASURE_CHEST_BOTTOM_ACT_OPENING:
            if (o->parentObj->oTreasureChestDoCloseChests) {
                o->oAction = TREASURE_CHEST_BOTTOM_ACT_CLOSE;
            }
            break;

        case TREASURE_CHEST_BOTTOM_ACT_OPENED:
            cur_obj_become_intangible();
            if (!is_point_within_radius_of_mario(o->oPosX, o->oPosY, o->oPosZ, 500)) {
                o->parentObj->oTreasureChestDoCloseChests = FALSE;
                o->oAction = TREASURE_CHEST_BOTTOM_ACT_CLOSE;
            }
    }

    cur_obj_push_mario_away_from_cylinder(150.0f, 150.0f);
    o->oInteractStatus = INT_STATUS_NONE;
}

void spawn_treasure_chest(s8 param, s32 x, s32 y, s32 z, s16 yaw) {
    struct Object *chestBaseObj = spawn_object_abs_with_rot(o, 0, MODEL_TREASURE_CHEST_BASE,
                                                            bhvTreasureChestBottom, x, y, z, 0, yaw, 0);
    chestBaseObj->oBehParams2ndByte = param;
}

void bhv_treasure_chest_ship_init(void) {
    spawn_treasure_chest(1, 400, -350, -2700, 0);
    spawn_treasure_chest(2, 650, -350, -940, -0x6001);
    spawn_treasure_chest(3, -550, -350, -770, 0x5FFF);
    spawn_treasure_chest(4, 100, -350, -1700, 0);
    o->oTreasureChestNumOpenedChests = 1;
    o->oTreasureChestAboveWater = FALSE;
}

void bhv_treasure_chest_ship_loop(void) {
    switch (o->oAction) {
        case TREASURE_CHEST_ACT_SUCCESS_SOUND:
            if (o->oTreasureChestNumOpenedChests == 5) {
                play_puzzle_jingle();
                fade_volume_scale(0, 127, 1000);
                o->oAction = TREASURE_CHEST_ACT_REWARD;
            }
            break;

        case TREASURE_CHEST_ACT_REWARD:
            if (gEnvironmentRegions != NULL) {
                gEnvironmentRegions[6] -= 5;
                play_sound(SOUND_ENV_WATER_DRAIN, gGlobalSoundSource);
                set_environmental_camera_shake(SHAKE_ENV_JRB_SHIP_DRAIN);
                if (gEnvironmentRegions[6] < -335) {
                    gEnvironmentRegions[6] = -335;
                    o->activeFlags = ACTIVE_FLAG_DEACTIVATED;
                }
#if ENABLE_RUMBLE
                reset_rumble_timers_vibrate(2);
#endif
            }
            break;
    }
}

void bhv_treasure_chest_jrb_init(void) {
    spawn_treasure_chest(TREASURE_CHEST_BP_1, -1700, -2812, -1150, 0x7FFF);
    spawn_treasure_chest(TREASURE_CHEST_BP_2, -1150, -2812, -1550, 0x7FFF);
    spawn_treasure_chest(TREASURE_CHEST_BP_3, -2400, -2812, -1800, 0x7FFF);
    spawn_treasure_chest(TREASURE_CHEST_BP_4, -1800, -2812, -2100, 0x7FFF);
    o->oTreasureChestNumOpenedChests = 1;
    o->oTreasureChestAboveWater = TRUE;
}

void bhv_treasure_chest_jrb_loop(void) {
    switch (o->oAction) {
        case TREASURE_CHEST_ACT_SUCCESS_SOUND:
            if (o->oTreasureChestNumOpenedChests == 5) {
                play_puzzle_jingle();
                o->oAction = TREASURE_CHEST_ACT_REWARD;
            }
            break;

        case TREASURE_CHEST_ACT_REWARD:
            if (o->oTimer == 60) {
                spawn_mist_particles();
                spawn_default_star(-1800.0f, -2500.0f, -1700.0f);
                o->oAction = TREASURE_CHEST_ACT_END;
            }
            break;

        case TREASURE_CHEST_ACT_END:
            break;
    }
}

void bhv_treasure_chest_init(void) {
    spawn_treasure_chest(1, -4500, -5119, 1300, -0x6001);
    spawn_treasure_chest(2, -1800, -5119, 1050, 0x1FFF);
    spawn_treasure_chest(3, -4500, -5119, -1100, 0x238E);
    spawn_treasure_chest(4, -2400, -4607, 125, 0x3E93);

    o->oTreasureChestNumOpenedChests = 1;
    o->oTreasureChestAboveWater = FALSE;
}

void bhv_treasure_chest_loop(void) {
    switch (o->oAction) {
        case TREASURE_CHEST_ACT_SUCCESS_SOUND:
            if (o->oTreasureChestNumOpenedChests == 5) {
                play_puzzle_jingle();
                o->oAction = TREASURE_CHEST_ACT_REWARD;
            }
            break;

        case TREASURE_CHEST_ACT_REWARD:
            if (o->oTimer == 60) {
                spawn_mist_particles();
                spawn_default_star(-1900.0f, -4000.0f, -1400.0f);
                o->oAction = TREASURE_CHEST_ACT_END;
            }
            break;

        case TREASURE_CHEST_ACT_END:
            break;
    }
}
