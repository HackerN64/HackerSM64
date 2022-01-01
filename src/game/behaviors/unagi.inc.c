// unagi.inc.c

struct ObjectHitbox sUnagiHitbox = {
    /* interactType:      */ INTERACT_CLAM_OR_BUBBA,
    /* downOffset:        */ 50,
    /* damageOrCoinValue: */ 3,
    /* health:            */ 99,
    /* numLootCoins:      */ 0,
    /* radius:            */ 150,
    /* height:            */ 150,
    /* hurtboxRadius:     */ 150,
    /* hurtboxHeight:     */ 150,
};

void bhv_unagi_init(void) {
    if (o->oBehParams2ndByte != UNAGI_BP_IN_CAVE) {
        o->oPathedStartWaypoint = segmented_to_virtual(jrb_seg7_trajectory_unagi_1);
        if (o->oBehParams2ndByte == UNAGI_BP_IN_SHIP) {
            o->oFaceAnglePitch = -0x1DB0;
        } else {
            o->oAction = UNAGI_ACT_SHIP_PATH;
        }
    } else {
        o->oPathedStartWaypoint = segmented_to_virtual(jrb_seg7_trajectory_unagi_2);
        o->oAction = UNAGI_ACT_IN_CAVE;
#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS 
        // !HackerSM64: Show transparent star if collected
        if (save_file_get_star_flags(gCurrSaveFileNum - 1, COURSE_NUM_TO_INDEX(COURSE_JRB)) & STAR_FLAG_ACT_2) {
            o->oAnimState = UNAGI_ANIM_STATE_HAS_TRANSPARENT_STAR;
        } else {
            o->oAnimState = UNAGI_ANIM_STATE_HAS_STAR;
        }
#else
        o->oAnimState = UNAGI_ANIM_STATE_HAS_STAR;
#endif

        o->oUnagiInitMoveYaw = o->oMoveAngleYaw;
    }

    o->oPathedPrevWaypoint = o->oPathedStartWaypoint;
}

void unagi_act_0(void) {
    if (o->oDistanceToMario > 4500.0f && o->oSubAction != UNAGI_SUB_ACT_SHIP_RESET_PATH_WAIT_FOR_MARIO) {
        o->oAction = UNAGI_ACT_SHIP_PATH;
        vec3s_to_vec3f(&o->oPosVec, o->oPathedStartWaypoint->pos);
    } else if (o->oUnagiDistanceToMario < 700.0f) {
        o->oSubAction = UNAGI_SUB_ACT_SHIP_RESET_PATH_DO_RESET;
    }
}

void unagi_act_1_4(s32 nextAction) {
    if (o->oSoundStateID == UNAGI_ANIM_STATIC_STRAIGHT) {
        if (cur_obj_check_anim_frame(30)) {
            o->oForwardVel = 40.0f;
        }
    } else if (cur_obj_check_if_at_animation_end()) {
        if ((o->oAction != nextAction) && (o->oPathedPrevWaypointFlags & 0xFF) >= 7) {
            cur_obj_init_animation_with_sound(UNAGI_ANIM_STATIC_STRAIGHT);
        } else {
            cur_obj_init_animation_with_sound(UNAGI_ANIM_SWIM);
        }
    }

    if (cur_obj_check_anim_frame(6)) {
        cur_obj_play_sound_2(SOUND_GENERAL_MOVING_WATER);
    }

    if (cur_obj_follow_path() == PATH_REACHED_END) {
        o->oAction = nextAction;
    }

    o->oMoveAnglePitch = o->oFaceAnglePitch =
        approach_s16_symmetric(o->oMoveAnglePitch, o->oPathedTargetPitch, 50);

    cur_obj_rotate_yaw_toward(o->oPathedTargetYaw, 120);
    obj_roll_to_match_yaw_turn(o->oPathedTargetYaw, 0x2000, 100);

    obj_forward_vel_approach(10.0f, 0.2f);
    cur_obj_set_pos_via_transform();
}

void unagi_act_2(void) {
    o->oPathedPrevWaypoint = o->oPathedStartWaypoint;
    o->oPathedPrevWaypointFlags = 0;

    cur_obj_set_pos_to_home();

    o->oMoveAnglePitch = o->oFaceAnglePitch = 0;
    o->oMoveAngleYaw = o->oFaceAngleYaw = o->oUnagiInitMoveYaw;
    o->oFaceAngleRoll = 0;

    o->oForwardVel = o->oVelX = o->oVelZ = o->oUnagiNextForwardVel = 0.0f;

    o->oUnagiDistFromHome = -800.0f;

    o->oAction = UNAGI_ACT_IN_CAVE;
}

void unagi_act_3(void) {
    if (o->oUnagiDistFromHome < 0.0f) {
        cur_obj_init_animation_with_sound(UNAGI_ANIM_IDLE_2);

        if ((o->oUnagiDistFromHome += 10.0f) > 0.0f) {
            o->oUnagiDistFromHome = 0.0f;
        }
    } else {
        if (o->oUnagiDistFromHome == 0.0f) {
            cur_obj_init_animation_with_sound(UNAGI_ANIM_IDLE_2);

            if (o->oTimer > 60 && o->oUnagiDistanceToMario < 1000.0f) {
                cur_obj_play_sound_2(SOUND_OBJ_EEL_EXIT_CAVE);
                o->oUnagiNextForwardVel = o->oUnagiTargetNextForwardVel = 30.0f;
            } else {
                o->oUnagiTargetNextForwardVel = 0.0f;
            }
        } else if (o->oUnagiTargetNextForwardVel > 0.0f) {
            if (cur_obj_init_anim_and_check_if_end(UNAGI_ANIM_OPEN_MOUTH)) {
                o->oUnagiTargetNextForwardVel = 0.0f;
            }
        } else if (o->oUnagiTargetNextForwardVel == 0.0f) {
            cur_obj_init_animation_with_sound(UNAGI_ANIM_YAWN);
            if (cur_obj_check_if_at_animation_end()) {
                if (o->oUnagiDistanceToMario < 1000.0f) {
                    o->oAction = UNAGI_ACT_CAVE_PATH;
                    o->oForwardVel = o->oUnagiNextForwardVel;
                    cur_obj_init_animation_with_sound(UNAGI_ANIM_BITE);
                } else {
                    o->oUnagiTargetNextForwardVel = -50.0f;
                    cur_obj_init_animation_with_sound(UNAGI_ANIM_IDLE_2_1);
                }
            }
        }

        approach_f32_ptr(&o->oUnagiNextForwardVel, o->oUnagiTargetNextForwardVel, 4.0f);

        if ((o->oUnagiDistFromHome += o->oUnagiNextForwardVel) < 0.0f) {
            o->oUnagiDistFromHome = o->oUnagiNextForwardVel = 0.0f;
            o->oTimer = 0;
        }
    }

    o->oPosX = o->oHomeX + o->oUnagiDistFromHome * sins(o->oMoveAngleYaw);
    o->oPosZ = o->oHomeZ + o->oUnagiDistFromHome * coss(o->oMoveAngleYaw);
}

void bhv_unagi_loop(void) {
    s32 i;

    if (!o->oUnagiHasStar) {
        o->oUnagiDistanceToMario = 99999.0f;
        if (o->oDistanceToMario < 3000.0f) {
            for (i = UNAGI_PART_BP_BACK; i < UNAGI_PART_BP_FRONT; i++) {
                spawn_object_relative(i, 0, 0, 0, o, MODEL_NONE, bhvUnagiSubobject);
            }
            o->oUnagiHasStar = TRUE;
        }
    } else if (o->oDistanceToMario > 4000.0f) {
        o->oUnagiHasStar = FALSE;
    }

    switch (o->oAction) {
        case UNAGI_ACT_SHIP_RESET_PATH:
            unagi_act_0();
            break;
        case UNAGI_ACT_SHIP_PATH:
            unagi_act_1_4(o->oAction);
            break;
        case UNAGI_ACT_RETURN_TO_CAVE:
            unagi_act_2();
            // fallthrough
        case UNAGI_ACT_IN_CAVE:
            unagi_act_3();
            break;
        case UNAGI_ACT_CAVE_PATH:
            unagi_act_1_4(UNAGI_ACT_RETURN_TO_CAVE);
            break;
    }
}

void bhv_unagi_subobject_loop(void) {
    if (!o->parentObj->oUnagiHasStar) {
        obj_mark_for_deletion(o);
    } else {
        f32 offset = 300.0f * o->oBehParams2ndByte;

        o->oPosY = o->parentObj->oPosY - offset * sins(o->parentObj->oFaceAnglePitch) * 1.13f;

        offset = coss(o->parentObj->oFaceAnglePitch / 2) * offset;

        o->oPosX = o->parentObj->oPosX + offset * sins(o->parentObj->oFaceAngleYaw);
        o->oPosZ = o->parentObj->oPosZ + offset * coss(o->parentObj->oFaceAngleYaw);

        if (o->oBehParams2ndByte == UNAGI_PART_BP_BACK) {
            if (o->parentObj->oAnimState != UNAGI_ANIM_STATE_NO_STAR && o->oDistanceToMario < 150.0f) {
                o->oBehParams = o->parentObj->oBehParams;
                spawn_default_star(6833.0f, -3654.0f, 2230.0f);
                o->parentObj->oAnimState = UNAGI_ANIM_STATE_NO_STAR;
            }
        } else {
            obj_check_attacks(&sUnagiHitbox, o->oAction);
            if (o->oBehParams2ndByte == UNAGI_PART_BP_CONTROL_DISTANCE) {
                o->parentObj->oUnagiDistanceToMario = o->oDistanceToMario;
            }
        }
    }
}
