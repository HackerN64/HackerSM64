// chuckya.inc.c

void common_anchor_mario_behavior(f32 forwardVel, f32 yVel, s32 flag) {
    switch (o->parentObj->oCommonAnchorAction) {
        case 0:
            break;

        case 1:
            obj_set_gfx_pos_at_obj_pos(gMarioObject, o);
            break;

        case 2:
            gMarioObject->oInteractStatus |= flag + INT_STATUS_MARIO_THROWN_BY_OBJ;
            gMarioStates[0].forwardVel = forwardVel;
            gMarioStates[0].vel[1] = yVel;
            o->parentObj->oCommonAnchorAction = 0;
            break;

        case 3:
            gMarioObject->oInteractStatus |= INT_STATUS_MARIO_THROWN_BY_OBJ | INT_STATUS_MARIO_DROPPED_BY_OBJ; // loads 2 interactions at once?
            gMarioStates[0].forwardVel = 10.0f;
            gMarioStates[0].vel[1] = 10.0f;
            o->parentObj->oCommonAnchorAction = 0;
            break;
    }

    o->oMoveAngleYaw = o->parentObj->oMoveAngleYaw;
    if (o->parentObj->activeFlags == ACTIVE_FLAG_DEACTIVATED) {
        obj_mark_for_deletion(o);
    }
}

void bhv_chuckya_anchor_mario_loop(void) {
    common_anchor_mario_behavior(40.0f, 40.0f, INT_STATUS_MARIO_DROPPED_BY_OBJ);
}

void chuckya_act_0(void) {
    s32 initialSubAction;

    if (o->oTimer == 0) {
        o->oChuckyaSubActionTimer = 0;
    }

    o->oAngleToMario = obj_angle_to_object(o, gMarioObject);

    switch (initialSubAction = o->oSubAction) {
        case 0:
            o->oForwardVel = 0.0f;
            if (cur_obj_lateral_dist_from_mario_to_home() < 2000.0f) {
                cur_obj_rotate_yaw_toward(o->oAngleToMario, 0x400);
                if (o->oChuckyaSubActionTimer > 40
                    || abs_angle_diff(o->oMoveAngleYaw, o->oAngleToMario) < 0x1000) {
                    o->oSubAction = 1;
                }
            } else {
                o->oSubAction = 3;
            }
            break;

        case 1:
            approach_f32_symmetric_bool(&o->oForwardVel, 30.0f, 4.0f);
            if (abs_angle_diff(o->oMoveAngleYaw, o->oAngleToMario) > 0x4000) {
                o->oSubAction = 2;
            }
            if (cur_obj_lateral_dist_from_mario_to_home() > 2000.0f) {
                o->oSubAction = 3;
            }
            break;

        case 2:
            approach_f32_symmetric_bool(&o->oForwardVel, 0, 4.0f);
            if (o->oChuckyaSubActionTimer > 48) {
                o->oSubAction = 0;
            }
            break;

        case 3:
            if (cur_obj_lateral_dist_to_home() < 500.0f) {
                o->oForwardVel = 0.0f;
            } else {
                approach_f32_symmetric_bool(&o->oForwardVel, 10.0f, 4.0f);
                o->oAngleToMario = cur_obj_angle_to_home();
                cur_obj_rotate_yaw_toward(o->oAngleToMario, 0x800);
            }
            if (cur_obj_lateral_dist_from_mario_to_home() < 1900.0f) {
                o->oSubAction = 0;
            }
            break;
    }

    if (o->oSubAction != initialSubAction) {
        o->oChuckyaSubActionTimer = 0;
    } else {
        o->oChuckyaSubActionTimer++;
    }

    cur_obj_init_animation_with_sound(4);

    if (o->oForwardVel > 1.0f) {
        cur_obj_play_sound_1(SOUND_AIR_CHUCKYA_MOVE);
    }

    print_debug_bottom_up("sp %d", o->oForwardVel);
}

void chuckya_act_1(void) {
    if (o->oSubAction == 0) {
        if (cur_obj_init_animation_and_check_if_near_end(0)) {
            o->oSubAction++;
        }
        o->oChuckyaSubActionTimer = random_float() * 30.0f + 10.0f;
        o->oChuckyaNumPlayerEscapeActions = 0;
        o->oForwardVel = 0.0f;
    } else {
        if (o->oSubAction == 1) {
            o->oChuckyaNumPlayerEscapeActions += player_performed_grab_escape_action();
            print_debug_bottom_up("%d", o->oChuckyaNumPlayerEscapeActions);
            if (o->oChuckyaNumPlayerEscapeActions > 10) {
                o->oCommonAnchorAction = 3;
                o->oAction = 3;
                o->oInteractStatus &= ~INT_STATUS_GRABBED_MARIO;
            } else {
                cur_obj_init_animation_with_sound(1);
                o->oMoveAngleYaw += INT_STATUS_GRABBED_MARIO;
                if (o->oChuckyaSubActionTimer-- < 0
                 && (check_if_moving_over_floor(50.0f, 150.0f) || o->oChuckyaSubActionTimer < -16)) {
                    o->oSubAction++;
                }
            }
        } else {
            cur_obj_init_animation_with_sound(3);
            if (cur_obj_check_anim_frame(18)) {
                cur_obj_play_sound_2(SOUND_OBJ_RELEASE_MARIO);
                o->oCommonAnchorAction = 2;
                o->oAction = 3;
                o->oInteractStatus &= ~INT_STATUS_GRABBED_MARIO;
            }
        }
    }
}

void chuckya_act_3(void) {
    o->oForwardVel = 0.0f;
    o->oVelY = 0.0f;
    cur_obj_init_animation_with_sound(4);
    if (o->oTimer > 100) {
        o->oAction = 0;
    }
}

void chuckya_act_2(void) {
    if (o->oMoveFlags & (OBJ_MOVE_HIT_WALL | OBJ_MOVE_MASK_IN_WATER | OBJ_MOVE_LANDED)) {
        obj_mark_for_deletion(o);
        obj_spawn_loot_yellow_coins(o, 5, 20.0f);
        spawn_mist_particles_with_sound(SOUND_OBJ_CHUCKYA_DEATH);
    }
}

ObjActionFunc sChuckyaActions[] = {
    chuckya_act_0,
    chuckya_act_1,
    chuckya_act_2,
    chuckya_act_3,
};

void chuckya_move(void) {
    cur_obj_update_floor_and_walls();
    cur_obj_call_action_function(sChuckyaActions);
    cur_obj_move_standard(-30);
    if (o->oInteractStatus & INT_STATUS_GRABBED_MARIO) {
        o->oAction = 1;
        o->oCommonAnchorAction = 1;
        cur_obj_play_sound_2(SOUND_OBJ_GRAB_MARIO);
    }
}

void bhv_chuckya_loop(void) {
    cur_obj_scale(2.0f);
    o->oInteractionSubtype |= INT_SUBTYPE_GRABS_MARIO;

    switch (o->oHeldState) {
        case HELD_FREE:
            chuckya_move();
            break;
        case HELD_HELD:
            cur_obj_unrender_set_action_and_anim(2, 0);
            break;
        case HELD_THROWN:
        case HELD_DROPPED:
            cur_obj_get_thrown_or_placed(20.0f, 50.0f, 2);
            break;
    }

    o->oInteractStatus = INT_STATUS_NONE;

    print_debug_bottom_up("md %d", o->oAction);
}
