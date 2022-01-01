// flamethrower.inc.c

void bhv_flamethrower_flame_loop(void) {
    f32 scale;
    s32 remainingTime;

    if (o->oTimer == 0) {
        o->oAnimState = (s32)(random_float() * 10.0f);
        obj_translate_xyz_random(o, 10.0f);
    }

    if (o->oBehParams2ndByte == FLAMETHROWER_BP_SLOW) {
        scale = o->oTimer * (o->oForwardVel - 6.0f) / 100.0f + 2.0f;
    } else {
        scale = o->oTimer * (o->oForwardVel - 20.0f) / 100.0f + 1.0f;
    }

    if (o->oBehParams2ndByte == FLAMETHROWER_BP_TALL_HITBOX) {
        o->hitboxHeight = 200.0f;
        o->hitboxDownOffset = 150.0f;
        o->oVelY = -28.0f;

        cur_obj_update_floor_height();

        if (o->oPosY - 25.0f * scale < o->oFloorHeight) {
            o->oVelY = 0;
            o->oPosY = o->oFloorHeight + 25.0f * scale;
        }

        remainingTime = o->parentObj->oFlameThowerTimeRemaining / 1.2f;
    } else {
        remainingTime = o->parentObj->oFlameThowerTimeRemaining;
    }

    cur_obj_scale(scale);

    if (o->oBehParams2ndByte == FLAMETHROWER_BP_UPWARDS) {
        o->oPosY += o->oForwardVel; // weird?
    } else {
        cur_obj_move_using_fvel_and_gravity();
    }

    if (o->oTimer > remainingTime) {
        obj_mark_for_deletion(o);
    }

    o->oInteractStatus = INT_STATUS_NONE;
}

void bhv_flamethrower_loop(void) {
    if (o->oAction == FLAMETHROWER_ACT_IDLE) {
#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
        if (gCurrLevelNum != LEVEL_BBH || gMarioOnMerryGoRound)
#endif
        {
            if (o->oDistanceToMario < 2000.0f) {
                o->oAction = FLAMETHROWER_ACT_BLOW_FIRE;
            }
        }
    } else if (o->oAction == FLAMETHROWER_ACT_BLOW_FIRE) {
        ModelID32 model = MODEL_RED_FLAME;
        f32 flameVel = 95.0f;

        if (o->oBehParams2ndByte == FLAMETHROWER_BP_BLUE) {
            model = MODEL_BLUE_FLAME;
        }

        if (o->oBehParams2ndByte == FLAMETHROWER_BP_SLOW) {
            flameVel = 50.0f;
        }

        f32 flameTimeRemaining = 1;

        if (o->oTimer < 60) {
            flameTimeRemaining = 15;
        } else if (o->oTimer < 74) {
            flameTimeRemaining = 75 - o->oTimer; // Range: [15..2]
        } else {
            o->oAction = FLAMETHROWER_ACT_COOLDOWN;
        }

        o->oFlameThowerTimeRemaining = flameTimeRemaining;

        struct Object *flame = spawn_object_relative(o->oBehParams2ndByte, 0, 0, 0, o, model, bhvFlamethrowerFlame);
        flame->oForwardVel = flameVel;

        cur_obj_play_sound_1(SOUND_AIR_BLOW_FIRE);
    } else if (o->oTimer > 60) {
        o->oAction = FLAMETHROWER_ACT_IDLE;
    }
}

void bhv_rr_rotating_bridge_platform_loop(void) {
    o->oMoveAngleYaw -= 0x80;
    o->oAngleVelYaw = -0x80;

    bhv_flamethrower_loop();
}
