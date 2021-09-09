
static void boo_oscillate(s32 ignoreOpacity) {
    o->oFaceAnglePitch = sins(o->oBooOscillationTimer) * 0x400;

    if (o->oOpacity == 0xFF || ignoreOpacity == TRUE) {
        o->header.gfx.scale[0] = sins(o->oBooOscillationTimer) * 0.08 + o->oBooBaseScale;
        o->header.gfx.scale[1] = -sins(o->oBooOscillationTimer) * 0.08 + o->oBooBaseScale;
        o->header.gfx.scale[2] = o->header.gfx.scale[0];
        o->oGravity = sins(o->oBooOscillationTimer) * o->oBooBaseScale;
        o->oBooOscillationTimer += 0x400;
    }
}

void bhv_boo_in_castle_loop(void) {
    s16 targetAngle;

    o->oBooBaseScale = 2.0f;

    if (o->oAction == 0) {
        cur_obj_hide();

#ifndef UNLOCK_ALL
        if (gHudDisplay.stars < 12) {
            obj_mark_for_deletion(o);
        }
#endif

        if (gMarioCurrentRoom == 1) {
            o->oAction++;
        }
    } else if (o->oAction == 1) {
        cur_obj_unhide();

        o->oOpacity = 180;

        if (o->oTimer == 0) {
            cur_obj_scale(o->oBooBaseScale);
        }

        if (o->oDistanceToMario < 1000.0f) {
            o->oAction++;
            cur_obj_play_sound_2(SOUND_OBJ_BOO_LAUGH_LONG);
        }

        o->oForwardVel = 0.0f;
        targetAngle = o->oAngleToMario;
    } else {
        cur_obj_forward_vel_approach_upward(32.0f, 1.0f);

        o->oHomeX = -1000.0f;
        o->oHomeZ = -9000.0f;

        targetAngle = cur_obj_angle_to_home();

        if (o->oPosZ < -5000.0f) {
            if (o->oOpacity > 0) {
                o->oOpacity -= 20;
            } else {
                o->oOpacity = 0;
            }
        }

        if (o->activeFlags & ACTIVE_FLAG_IN_DIFFERENT_ROOM) {
            o->oAction = 1;
        }
    }

    o->oVelY = 0.0f;

    targetAngle = cur_obj_angle_to_home();

    cur_obj_rotate_yaw_toward(targetAngle, 0x5A8);
    boo_oscillate(TRUE);
    cur_obj_move_using_fvel_and_gravity();
}
