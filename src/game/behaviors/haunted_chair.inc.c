// haunted_chair.inc.c

struct ObjectHitbox sHauntedChairHitbox = {
    /* interactType:      */ INTERACT_MR_BLIZZARD,
    /* downOffset:        */ 0,
    /* damageOrCoinValue: */ 2,
    /* health:            */ 0,
    /* numLootCoins:      */ 0,
    /* radius:            */ 50,
    /* height:            */ 50,
    /* hurtboxRadius:     */ 50,
    /* hurtboxHeight:     */ 50,
};

void bhv_haunted_chair_init(void) {
    struct Object *pianoObj;
    f32 dist;

    pianoObj = cur_obj_find_nearest_object_with_behavior(bhvMadPiano, &dist);
    if (pianoObj != NULL && dist < 300.0f) {
        o->parentObj = pianoObj;
    } else {
        o->oHauntedChairSpinTimer = 1;
    }
}

void haunted_chair_act_0(void) {
    s16 dAngleToPiano;

    if (o->parentObj != o) {
        if (o->oHauntedChairFallTargetAngle == 0) {
            if (lateral_dist_between_objects(o, o->parentObj) < 250.0f) {
                dAngleToPiano = obj_angle_to_object(o, o->parentObj) - o->oFaceAngleYaw + 0x2000;
                if (dAngleToPiano & 0x4000) {
                    o->oHauntedChairFallFromPianoAngle = &o->oFaceAngleRoll;
                    if (dAngleToPiano > 0) {
                        o->oHauntedChairFallTargetAngle = 0x4000;
                    } else {
                        o->oHauntedChairFallTargetAngle = -0x4000;
                    }
                } else {
                    o->oHauntedChairFallFromPianoAngle = &o->oFaceAnglePitch;
                    if (dAngleToPiano < 0) {
                        o->oHauntedChairFallTargetAngle = 0x5000;
                    } else {
                        o->oHauntedChairFallTargetAngle = -0x4000;
                    }
                }

                if (o->oHauntedChairFallTargetAngle < 0) {
                    o->oHauntedChairPitchVel = -1500.0f;
                } else {
                    o->oHauntedChairPitchVel = 1500.0f;
                }
            }
        } else {
            oscillate_toward(o->oHauntedChairFallFromPianoAngle, &o->oHauntedChairPitchVel, o->oHauntedChairFallTargetAngle,
                             4000.0f, 20.0f, 2.0f);
        }
    } else if (o->oHauntedChairSpinTimer != 0) {
        if (o->oDistanceToMario < 500.0f) {
            o->oHauntedChairSpinTimer -= 1;
        }
        o->oTimer = 0.0f;
    } else {
        if ((o->oTimer & 0x8) != 0) {
            f32 offset;

            if (o->oFaceAnglePitch < 0) {
                cur_obj_play_sound_2(SOUND_GENERAL_HAUNTED_CHAIR_MOVE);
                offset = 4.0f;
            } else {
                offset = -4.0f;
            }

            o->oHomeX -= offset;
            o->oHomeZ -= offset;

            o->oFaceAnglePitch = o->oFaceAngleRoll = (s32)(50.0f * offset);
        } else {
            o->oFaceAnglePitch = o->oFaceAngleRoll = 0;
        }

        if (o->oTimer > 30) {
            o->oAction = 1;
            o->oHauntedChairPitchVel = 0.0f;
            o->oHauntedChairRollVel = 200.0f;
            o->oHauntedChairSpinTimer = 40;
        }
    }

    cur_obj_push_mario_away_from_cylinder(80.0f, 120.0f);
}

void haunted_chair_act_1(void) {
    cur_obj_update_floor_and_walls();

    if (o->oTimer < 70) {
        if (o->oTimer < 50) {
            o->oVelY = 6.0f;
        } else {
            o->oVelY = 0.0f;
        }

        o->oGravity = 0.0f;
        oscillate_toward(&o->oFaceAnglePitch, &o->oHauntedChairPitchVel, -4000, 200.0f, 20.0f, 2.0f);
        oscillate_toward(&o->oFaceAngleRoll, &o->oHauntedChairRollVel, 0, 0.0f, 20.0f, 1.0f);
    } else {
        if (o->oHauntedChairSpinTimer != 0) {
            if (--o->oHauntedChairSpinTimer == 0) {
                cur_obj_play_sound_2(SOUND_GENERAL_HAUNTED_CHAIR);
                o->oMoveAnglePitch = obj_turn_pitch_toward_mario(120.0f, 0);
                o->oMoveAngleYaw = o->oAngleToMario;
                obj_compute_vel_from_move_pitch(50.0f);
            } else if (o->oHauntedChairSpinTimer > 20) {
                if (gGlobalTimer % 4 == 0) {
                    cur_obj_play_sound_2(SOUND_GENERAL_SWISH_AIR_2);
                }
                o->oFaceAngleYaw += 0x2710;
            }
        } else if (o->oMoveFlags & (OBJ_MOVE_MASK_ON_GROUND | OBJ_MOVE_HIT_WALL)) {
            obj_die_if_health_non_positive();
        }
    }

    obj_check_attacks(&sHauntedChairHitbox, o->oAction);
    cur_obj_move_standard(78);
}

void bhv_haunted_chair_loop(void) {
    if (!(o->activeFlags & ACTIVE_FLAG_IN_DIFFERENT_ROOM)) {
        switch (o->oAction) {
            case 0:
                haunted_chair_act_0();
                break;
            case 1:
                haunted_chair_act_1();
                break;
        }
        cur_obj_spin_all_dimensions(30.0f, 30.0f);
    }
}
