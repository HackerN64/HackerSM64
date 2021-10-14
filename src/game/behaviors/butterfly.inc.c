// butterfly.inc.c

void bhv_butterfly_init(void) {
    cur_obj_init_animation(BUTTERFLY_ANIM_RESTING);
    o->oButterflyYPhase = random_float() * 100.0f;
    o->header.gfx.animInfo.animFrame = random_float() * 7.0f;
    vec3f_copy(&o->oHomeVec, &o->oPosVec);
}

void butterfly_step(s32 speed) {
    s16 yaw = o->oMoveAngleYaw;
    s16 pitch = o->oMoveAnglePitch;
    s16 yPhase = o->oButterflyYPhase;
    f32 floorY;

    o->oVelX = sins(yaw) * (f32) speed;
    o->oVelY = sins(pitch) * (f32) speed;
    o->oVelZ = coss(yaw) * (f32) speed;

    o->oPosX += o->oVelX;
    o->oPosZ += o->oVelZ;

    if (o->oAction == BUTTERFLY_ACT_FOLLOW_MARIO) {
        o->oPosY -= o->oVelY + coss((s32)(yPhase * 655.36f)) * 5.0f; // * 20.0f / 4;
    } else {
        o->oPosY -= o->oVelY;
    }

    floorY = find_floor_height(o->oPosX, o->oPosY, o->oPosZ);

    if (o->oPosY < floorY + 2.0f) {
        o->oPosY = floorY + 2.0f;
    }

    o->oButterflyYPhase++;
    if (o->oButterflyYPhase > 100) {
        o->oButterflyYPhase = 0;
    }
}

void butterfly_calculate_angle(void) {
    s32 yPhase = 5 * o->oButterflyYPhase / 4;
    gMarioObject->oPosX += yPhase;
    gMarioObject->oPosZ += yPhase;
    obj_turn_toward_object(o, gMarioObject, O_MOVE_ANGLE_YAW_INDEX, 0x300);
    gMarioObject->oPosX -= yPhase;
    gMarioObject->oPosZ -= yPhase;
    yPhase = (5 * o->oButterflyYPhase + 0x100) / 4;
    gMarioObject->oPosY += yPhase;
    obj_turn_toward_object(o, gMarioObject, O_MOVE_ANGLE_PITCH_INDEX, 0x500);
    gMarioObject->oPosY -= yPhase;
}

void butterfly_act_rest(void) {
    if (is_point_within_radius_of_mario(o->oPosX, o->oPosY, o->oPosZ, 1000)) {
        cur_obj_init_animation(BUTTERFLY_ANIM_FLYING);

        o->oAction = BUTTERFLY_ACT_FOLLOW_MARIO;
        o->oMoveAngleYaw = gMarioObject->header.gfx.angle[1];
    }
}

void butterfly_act_follow_mario(void) {
    butterfly_calculate_angle();

    butterfly_step(7);

    if (!is_point_within_radius_of_mario(o->oHomeX, o->oHomeY, o->oHomeZ, 1200)) {
        o->oAction = BUTTERFLY_ACT_RETURN_HOME;
    }
}

void butterfly_act_return_home(void) {
    Vec3f homeDist;
    vec3f_diff(homeDist, &o->oHomeVec, &o->oPosVec);
    f32 homeDistXSq  = sqr(homeDist[0]);
    f32 homeDistZSq  = sqr(homeDist[2]);
    s16 hAngleToHome = atan2s(homeDist[2], homeDist[0]);
    s16 vAngleToHome = atan2s(sqrtf(homeDistXSq + homeDistZSq), -homeDist[1]);
    o->oMoveAngleYaw = approach_s16_symmetric(o->oMoveAngleYaw, hAngleToHome, 0x800);
    o->oMoveAnglePitch = approach_s16_symmetric(o->oMoveAnglePitch, vAngleToHome, 0x50);

    butterfly_step(7);

    if ((homeDistXSq + sqr(homeDist[1]) + homeDistZSq) < 144.0f) {
        cur_obj_init_animation(BUTTERFLY_ANIM_RESTING);

        o->oAction = BUTTERFLY_ACT_RESTING;
        vec3f_copy(&o->oPosVec, &o->oHomeVec);
    }
}

void bhv_butterfly_loop(void) {
    switch (o->oAction) {
        case BUTTERFLY_ACT_RESTING:
            butterfly_act_rest();
            break;

        case BUTTERFLY_ACT_FOLLOW_MARIO:
            butterfly_act_follow_mario();
            break;

        case BUTTERFLY_ACT_RETURN_HOME:
            butterfly_act_return_home();
            break;
    }

    set_object_visibility(o, 3000);
}
