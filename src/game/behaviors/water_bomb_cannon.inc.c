// water_bomb_cannon.inc.c

void bhv_bubble_cannon_barrel_loop(void) {
    if (o->parentObj->oAction == 2) {
        obj_mark_for_deletion(o);
    } else {
        o->oMoveAngleYaw = o->parentObj->oFaceAngleYaw;
        o->oMoveAnglePitch = o->parentObj->oMoveAnglePitch + 0x4000;
        o->oFaceAnglePitch = o->parentObj->oMoveAnglePitch;

        if ((o->oCannonBarrelBubblesForwardVelCheck += o->oForwardVel) > 0.0f) {
            cur_obj_set_pos_via_transform();
            obj_forward_vel_approach(-5.0f, 18.0f);
        } else {
            o->oCannonBarrelBubblesForwardVelCheck = 0.0f;
            obj_copy_pos(o, o->parentObj);

            // check this
            if (o->parentObj->oWaterCannonIdleTimer != 0) {
                if (o->oForwardVel == 0.0f) {
                    o->oForwardVel = 35.0f;

                    struct Object *waterBomb = spawn_object(o, MODEL_WATER_BOMB, bhvWaterBomb);
                    if (waterBomb != NULL) {
                        waterBomb->oForwardVel = -100.0f;
                        waterBomb->header.gfx.scale[1] = 1.7f;
                    }

                    set_camera_shake_from_point(SHAKE_POS_MEDIUM, o->oPosX, o->oPosY, o->oPosZ);
                }
            } else {
                o->oForwardVel = 0.0f;
            }
        }
    }
}

void water_bomb_cannon_act_0(void) {
    if (o->oDistanceToMario < 2000.0f) {
        spawn_object(o, MODEL_CANNON_BARREL, bhvCannonBarrelBubbles);
        cur_obj_unhide();

        o->oAction = 1;
        o->oMoveAnglePitch = o->oWaterCannonTargetMovePitch = 0x1C00;
    }
}

void water_bomb_cannon_act_1(void) {
    if (o->oDistanceToMario > 2500.0f) {
        o->oAction = 2;
    } else if (o->oBehParams2ndByte == 0) {
        if (o->oWaterCannonIdleTimer != 0) {
            o->oWaterCannonIdleTimer--;
        } else {
            obj_move_pitch_approach(o->oWaterCannonTargetMovePitch, 0x80);
            obj_face_yaw_approach(o->oWaterCannonTargetFaceYaw, 0x100);

            if ((s16) o->oFaceAngleYaw == (s16) o->oWaterCannonTargetFaceYaw) {
                if (o->oWaterCannonRotationTimer != 0) {
                    o->oWaterCannonRotationTimer--;
                } else {
                    cur_obj_play_sound_2(SOUND_OBJ_WATER_BOMB_CANNON);
                    o->oWaterCannonIdleTimer = 70;
                    o->oWaterCannonTargetMovePitch = 0x1000 + 0x400 * (random_u16() & 0x3);
                    o->oWaterCannonTargetFaceYaw = -0x2000 + o->oMoveAngleYaw + 0x1000 * (random_u16() % 5);
                    o->oWaterCannonRotationTimer = 60;
                }
            }
        }
    }
}

void water_bomb_cannon_act_2(void) {
    cur_obj_hide();
    o->oAction = 0;
}

void bhv_water_bomb_cannon_loop(void) {
    cur_obj_push_mario_away_from_cylinder(220.0f, 300.0f);

    switch (o->oAction) {
        case 0:
            water_bomb_cannon_act_0();
            break;
        case 1:
            water_bomb_cannon_act_1();
            break;
        case 2:
            water_bomb_cannon_act_2();
            break;
    }
}
