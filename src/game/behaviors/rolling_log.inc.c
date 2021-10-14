// rolling_log.inc.c

// why are the falling platforms and rolling logs grouped
// together? seems strange, but it also cooresponds to the
// behavior script grouping if the same file is assumed.
// hypothesis is that the object in the middle here used to be
// a rolling log of another variation.

void bhv_ttm_rolling_log_init(void) {
    o->oRollingLogX = 3970.0f;
    o->oRollingLogZ = 3654.0f;
    o->oRollingLogMaxDist = 271037.0f;

    o->oMoveAngleYaw = 8810;
    o->oForwardVel = 0.0f;
    o->oVelX = 0.0f;
    o->oVelZ = 0.0f;
    o->oFaceAnglePitch = 0;
    o->oAngleVelPitch = 0;
}

void rolling_log_roll_log(void) {
    if (gMarioObject->platform == o) {
        f32 rollAmount = (gMarioObject->header.gfx.pos[2] - o->oPosZ) * coss(-o->oMoveAngleYaw)
                       - (gMarioObject->header.gfx.pos[0] - o->oPosX) * sins(-o->oMoveAngleYaw);
        if (rollAmount > 0) {
            o->oAngleVelPitch += 0x10;
        } else {
            o->oAngleVelPitch -= 0x10;
        }

        o->oAngleVelPitch = CLAMP(o->oAngleVelPitch, -0x200, 0x200);
    } else {
        if (is_point_close_to_object(o, o->oHomeX, o->oHomeY, o->oHomeZ, 100)) {
            if (o->oAngleVelPitch != 0) {
                if (o->oAngleVelPitch > 0) {
                    o->oAngleVelPitch -= 0x10;
                } else {
                    o->oAngleVelPitch += 0x10;
                }

                if (o->oAngleVelPitch < 0x10 && o->oAngleVelPitch > -0x10) {
                    o->oAngleVelPitch = 0;
                }
            }
        } else {
            if (o->oAngleVelPitch != 0x100) {
                if (o->oAngleVelPitch > 0x100) {
                    o->oAngleVelPitch -= 0x10;
                } else {
                    o->oAngleVelPitch += 0x10;
                }

                if (o->oAngleVelPitch < 0x110 && o->oAngleVelPitch > 0xF0) {
                    o->oAngleVelPitch = 0x100;
                }
            }
        }
    }
}

void bhv_rolling_log_loop(void) {
    f32 prevX = o->oPosX;
    f32 prevZ = o->oPosZ;

    rolling_log_roll_log();

    o->oForwardVel = o->oAngleVelPitch / 0x40;
    o->oVelX = o->oForwardVel * sins(o->oMoveAngleYaw);
    o->oVelZ = o->oForwardVel * coss(o->oMoveAngleYaw);

    o->oPosX += o->oVelX;
    o->oPosZ += o->oVelZ;

    if (o->oRollingLogMaxDist < sqr(o->oPosX - o->oRollingLogX) + sqr(o->oPosZ - o->oRollingLogZ)) {
        o->oForwardVel = 0.0f;
        o->oPosX = prevX;
        o->oPosZ = prevZ;
        o->oVelX = 0.0f;
        o->oVelZ = 0.0f;
    }

    o->oFaceAnglePitch += o->oAngleVelPitch;
    if (absf(o->oFaceAnglePitch & 0x1FFF) < 528.0f && o->oAngleVelPitch != 0) {
        cur_obj_play_sound_2(SOUND_GENERAL_ROLLING_LOG);
    }
}

void volcano_act_1(void) {
    o->oVolcanoTrapPitchVel += 4.0f;
    o->oAngleVelPitch += o->oVolcanoTrapPitchVel;
    o->oFaceAnglePitch -= o->oAngleVelPitch;

    if (o->oFaceAnglePitch < -0x4000) {
        o->oFaceAnglePitch = -0x4000;
        o->oAngleVelPitch = 0;
        o->oVolcanoTrapPitchVel = 0;
        o->oAction = 2;
        cur_obj_play_sound_2(SOUND_GENERAL_BIG_POUND);
        set_camera_shake_from_point(SHAKE_POS_LARGE, o->oPosX, o->oPosY, o->oPosZ);
    }
}

void volcano_act_3(void) {
    o->oAngleVelPitch = 0x90;
    o->oFaceAnglePitch += o->oAngleVelPitch;

    if (o->oFaceAnglePitch > 0) {
        o->oFaceAnglePitch = 0;
    }

    if (o->oTimer == 200) {
        o->oAction = 0;
    }
}

void bhv_volcano_trap_loop(void) {
    switch (o->oAction) {
        case 0:
            if (is_point_within_radius_of_mario(o->oPosX, o->oPosY, o->oPosZ, 1000)) {
                o->oAction = 1;
                cur_obj_play_sound_2(SOUND_GENERAL_VOLCANO_TRAP_FALL);
            }
            break;

        case 1:
            volcano_act_1();
            break;

        case 2:
            if (o->oTimer < 8) {
                o->oPosY = o->oHomeY + sins(o->oTimer * 0x1000) * 10.0f;
            }
            if (o->oTimer == 50) {
                cur_obj_play_sound_2(SOUND_GENERAL_VOLCANO_TRAP_RISE);
                o->oAction = 3;
            }
            break;

        case 3:
            volcano_act_3();
            break;
    }
}

void bhv_lll_rolling_log_init(void) {
    o->oRollingLogX = 5120.0f;
    o->oRollingLogZ = 6016.0f;
    o->oRollingLogMaxDist = 1048576.0f;

    o->oMoveAngleYaw = 0x3FFF;
    o->oForwardVel = 0.0f;
    o->oVelX = 0.0f;
    o->oVelZ = 0.0f;
    o->oFaceAnglePitch = 0;
    o->oAngleVelPitch = 0;
}
