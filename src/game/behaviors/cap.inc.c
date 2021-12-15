// cap.inc.c

static struct ObjectHitbox sCapHitbox = {
    /* interactType:      */ INTERACT_CAP,
    /* downOffset:        */ 0,
    /* damageOrCoinValue: */ 0,
    /* health:            */ 0,
    /* numLootCoins:      */ 0,
    /* radius:            */ 80,
    /* height:            */ 80,
    /* hurtboxRadius:     */ 90,
    /* hurtboxHeight:     */ 90,
};

s32 cap_set_hitbox(void) {
    obj_set_hitbox(o, &sCapHitbox);

    if (o->oInteractStatus & INT_STATUS_INTERACTED) {
        o->activeFlags = ACTIVE_FLAG_DEACTIVATED;
        o->oInteractStatus = INT_STATUS_NONE;
        return TRUE;
    }

    return FALSE;
}

void cap_despawn(void) {
    if (o->oTimer > 300) {
        obj_flicker_and_disappear(o, 300);
    }
}

void cap_check_quicksand(void) {
    if (sObjFloor == NULL) {
        return;
    }

    switch (sObjFloor->type) {
        case SURFACE_DEATH_PLANE:
            o->activeFlags = ACTIVE_FLAG_DEACTIVATED;
            break;

        case SURFACE_SHALLOW_QUICKSAND:
        case SURFACE_DEEP_QUICKSAND:
        case SURFACE_QUICKSAND:
            o->oAction = CAP_ACT_QUICKSAND;
            o->oForwardVel = 0.0f;
            break;

        case SURFACE_DEEP_MOVING_QUICKSAND:
        case SURFACE_SHALLOW_MOVING_QUICKSAND:
        case SURFACE_MOVING_QUICKSAND:
            o->oAction = CAP_ACT_MOVING_QUICKSAND;
            o->oMoveAngleYaw = (sObjFloor->force & 0xFF) << 8;
            o->oForwardVel = 8 + 2 * (0 - ((sObjFloor->force & 0xFF00) >> 8));
            break;

        case SURFACE_INSTANT_QUICKSAND:
            o->oAction = CAP_ACT_INSTANT_QUICKSAND;
            o->oForwardVel = 0.0f;
            break;

        case SURFACE_INSTANT_MOVING_QUICKSAND:
            o->oAction = CAP_ACT_INSTANT_MOVING_QUICKSAND;
            o->oMoveAngleYaw = (sObjFloor->force & 0xFF) << 8;
            o->oForwardVel = 8 + 2 * (0 - ((sObjFloor->force & 0xFF00) >> 8));
            break;
    }
}

void cap_sink_quicksand(void) {
    switch (o->oAction) {
        case CAP_ACT_QUICKSAND:
            if (o->oTimer < 10) {
                o->oGraphYOffset += -1.0f;
                o->oFaceAnglePitch = 0x2000;
            }
            break;

        case CAP_ACT_MOVING_QUICKSAND:
            if (o->oTimer < 10) {
                o->oGraphYOffset += -3.0f;
            }
            o->oFaceAnglePitch = 0x2000;
            break;

        case CAP_ACT_INSTANT_QUICKSAND:
            o->oGraphYOffset += -1.0f;
            if (o->oTimer > 20) {
                o->activeFlags = ACTIVE_FLAG_DEACTIVATED;
            }
            break;

        case CAP_ACT_INSTANT_MOVING_QUICKSAND:
            o->oGraphYOffset += -6.0f;
            if (o->oTimer > 20) {
                o->activeFlags = ACTIVE_FLAG_DEACTIVATED;
            }
            o->oFaceAnglePitch = 0x2000;
            break;
    }

    cap_check_quicksand();
}

void bhv_wing_cap_init(void) {
    o->oGravity = 1.2f;
    o->oFriction = 0.999f;
    o->oBuoyancy = 0.9f;
    o->oOpacity = 255;
}

void cap_scale_vertically(void) {
    o->oCapScaleAngle += 0x2000;
    o->header.gfx.scale[1] = coss(o->oCapScaleAngle) * 0.3f + 0.7f;
    if (o->oCapScaleAngle == 0x10000) {
        o->oCapScaleAngle = 0;
        o->oCapDoScaleVertically = FALSE;
    }
}

void wing_vanish_cap_act_0(void) {
    o->oFaceAngleYaw += o->oForwardVel * 128.0f;
    s16 collisionFlags = object_step();
    if (collisionFlags & OBJ_COL_FLAG_GROUNDED) {
        cap_check_quicksand();
        if (o->oVelY != 0.0f) {
            o->oCapDoScaleVertically = TRUE;
            o->oVelY = 0.0f;
        }
    }

    if (o->oCapDoScaleVertically) {
        cap_scale_vertically();
    }
}

void bhv_wing_vanish_cap_loop(void) {
    switch (o->oAction) {
        case CAP_ACT_MOVE:
            wing_vanish_cap_act_0();
            break;

        default:
            object_step();
            cap_sink_quicksand();
            break;
    }

    if (o->oTimer > 20) {
        cur_obj_become_tangible();
    }

    cap_despawn();
    cap_set_hitbox();
}

void bhv_metal_cap_init(void) {
    o->oGravity = 2.4f;
    o->oFriction = 0.999f;
    o->oBuoyancy = 1.5f;
    o->oOpacity = 255;
}

void metal_cap_act_0(void) {
    o->oFaceAngleYaw += (o->oForwardVel * 128.0f);

    s16 collisionFlags = object_step();

    if (collisionFlags & OBJ_COL_FLAG_GROUNDED) {
        cap_check_quicksand();
    }
}

void bhv_metal_cap_loop(void) {
    switch (o->oAction) {
        case CAP_ACT_MOVE:
            metal_cap_act_0();
            break;

        default:
            object_step();
            cap_sink_quicksand();
            break;
    }

    if (o->oTimer > 20) {
        cur_obj_become_tangible();
    }

    cap_set_hitbox();
    cap_despawn();
}

void bhv_normal_cap_init(void) {
    o->oGravity = 0.7f;
    o->oFriction = 0.89f;
    o->oBuoyancy = 0.9f;
    o->oOpacity = 255;

    save_file_set_cap_pos(o->oPosX, o->oPosY, o->oPosZ);
}

void normal_cap_set_save_flags(void) {
    save_file_clear_flags(SAVE_FLAG_CAP_ON_GROUND);

    switch (gCurrCourseNum) {
        case COURSE_SSL:
            save_file_set_flags(SAVE_FLAG_CAP_ON_KLEPTO);
            break;

        case COURSE_SL:
            save_file_set_flags(SAVE_FLAG_CAP_ON_MR_BLIZZARD);
            break;

        case COURSE_TTM:
            save_file_set_flags(SAVE_FLAG_CAP_ON_UKIKI);
            break;

        default:
            save_file_set_flags(SAVE_FLAG_CAP_ON_KLEPTO);
            break;
    }
}

void normal_cap_act_0(void) {
    o->oFaceAngleYaw += o->oForwardVel * 128.0f;
    o->oFaceAnglePitch += o->oForwardVel * 80.0f;
    s16 collisionFlags = object_step();

    if (collisionFlags & OBJ_COL_FLAG_GROUNDED) {
        cap_check_quicksand();

        if (o->oVelY != 0.0f) {
            o->oCapDoScaleVertically = TRUE;
            o->oVelY = 0.0f;
            o->oFaceAnglePitch = 0;
        }
    }

    if (o->oCapDoScaleVertically) {
        cap_scale_vertically();
    }
}

void bhv_normal_cap_loop(void) {
    switch (o->oAction) {
        case CAP_ACT_MOVE:
            normal_cap_act_0();
            break;

        default:
            object_step();
            cap_sink_quicksand();
            break;
    }

    if ((s32) o->oForwardVel != 0) {
        save_file_set_cap_pos(o->oPosX, o->oPosY, o->oPosZ);
    }

    if (o->activeFlags == ACTIVE_FLAG_DEACTIVATED) {
        normal_cap_set_save_flags();
    }

    if (cap_set_hitbox() == TRUE) {
        save_file_clear_flags(SAVE_FLAG_CAP_ON_GROUND);
    }
}

void bhv_vanish_cap_init(void) {
    o->oGravity = 1.2f;
    o->oFriction = 0.999f;
    o->oBuoyancy = 0.9f;
    o->oOpacity = 150;
}
