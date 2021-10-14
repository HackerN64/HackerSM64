// spindel.inc.c

void bhv_spindel_init(void) {
    o->oHomeY = o->oPosY;
    o->oSpindelMoveTimer = 0;
    o->oSpindelMoveDirection = 0;
}

void bhv_spindel_loop(void) {
    if (o->oSpindelMoveTimer == -1) {
        if (o->oTimer == 32) {
            o->oSpindelMoveTimer = 0;
            o->oTimer = 0;
        } else {
            o->oVelZ = 0.0f;
            o->oAngleVelPitch = 0;
            return;
        }
    }

    s32 shake = 10 - o->oSpindelMoveTimer;

    if (shake < 0) {
        shake *= -1;
    }

    shake -= 6;
    if (shake < 0) {
        shake = 0;
    }

    if (o->oTimer == shake + 8) {
        o->oTimer = 0;
        o->oSpindelMoveTimer++;
        if (o->oSpindelMoveTimer == 20) {
            if (o->oSpindelMoveDirection == 0) {
                o->oSpindelMoveDirection = 1;
            } else {
                o->oSpindelMoveDirection = 0;
            }

            o->oSpindelMoveTimer = -1;
        }
    }

    if (shake == 4 || shake == 3) {
        shake = 4;
    } else if (shake == 2 || shake == 1) {
        shake = 2;
    } else if (shake == 0) {
        shake = 1;
    }

    if (o->oTimer < shake * 8) {
        if (o->oSpindelMoveDirection == 0) {
            o->oVelZ = 20 / shake;
            o->oAngleVelPitch = 1024 / shake;
        } else {
            o->oVelZ = -20 / shake;
            o->oAngleVelPitch = -1024 / shake;
        }

        o->oPosZ += o->oVelZ;
        o->oMoveAnglePitch += o->oAngleVelPitch;

        if (absf(o->oMoveAnglePitch & 0x1fff) < 800.0f && o->oAngleVelPitch != 0) {
            cur_obj_play_sound_2(SOUND_GENERAL2_SPINDEL_ROLL);
        }

        o->oPosY = o->oHomeY + absf(sins(o->oMoveAnglePitch * 4) * 23.0f);
        if (o->oTimer + 1 == shake * 8) {
            set_camera_shake_from_point(SHAKE_POS_SMALL, o->oPosX, o->oPosY, o->oPosZ);
        }
    }
}
