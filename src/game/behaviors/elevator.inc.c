// elevator.inc.c

static s16 sElevatorHeights[] = {
      -51,    0, 0,
     -461,    0, 0,
     -512,    0, 0,
    -2611,    0, 0,
    -2360,    0, 0,
      214,    0, 0,
      -50, 1945, 1,
};

void elevator_starting_shake(void) {
    cur_obj_play_sound_2(SOUND_GENERAL_QUIET_POUND1);
    cur_obj_shake_screen(SHAKE_POS_SMALL);
}

void elevator_act_0(void) {
    o->oVelY = 0;
    if (o->oElevatorType == 2) {
        if (gMarioObject->platform == o) {
            if (o->oPosY > o->oElevatorMidY) {
                o->oAction = 2;
            } else {
                o->oAction = 1;
            }
        }
    } else if (gMarioObject->oPosY > o->oElevatorMidY || o->oElevatorType == 1) {
        o->oPosY = o->oElevatorMaxY;
        if (gMarioObject->platform == o) {
            o->oAction = 2;
        }
    } else {
        o->oPosY = o->oElevatorMinY;
        if (gMarioObject->platform == o) {
            o->oAction = 1;
        }
    }
}

void elevator_act_1(void) {
    cur_obj_play_sound_1(SOUND_ENV_ELEVATOR1);
    if (o->oTimer == 0 && cur_obj_is_mario_on_platform()) {
        elevator_starting_shake();
    }
    approach_f32_signed(&o->oVelY, 10.0f, 2.0f);
    o->oPosY += o->oVelY;
    if (o->oPosY > o->oElevatorMaxY) {
        o->oPosY = o->oElevatorMaxY;
        if (o->oElevatorType == 2 || o->oElevatorType == 1) {
            o->oAction = 3;
        } else if (gMarioObject->oPosY < o->oElevatorMidY) {
            o->oAction = 2;
        } else {
            o->oAction = 3;
        }
    }
}

void elevator_act_2(void) {
    cur_obj_play_sound_1(SOUND_ENV_ELEVATOR1);
    if (o->oTimer == 0 && cur_obj_is_mario_on_platform()) {
        elevator_starting_shake();
    }
    approach_f32_signed(&o->oVelY, -10.0f, -2.0f);
    o->oPosY += o->oVelY;
    if (o->oPosY < o->oElevatorMinY) {
        o->oPosY = o->oElevatorMinY;
        if (o->oElevatorType == 1) {
            o->oAction = 4;
        } else if (o->oElevatorType == 2) {
            o->oAction = 3;
        } else if (gMarioObject->oPosY > o->oElevatorMidY) {
            o->oAction = 1;
        } else {
            o->oAction = 3;
        }
    }
}

void elevator_act_4(void) {
    o->oVelY = 0;
    if (o->oTimer == 0) {
        cur_obj_shake_screen(SHAKE_POS_SMALL);
        cur_obj_play_sound_2(SOUND_GENERAL_ELEVATOR_LAND);
    }
    if (!mario_is_in_air_action() && !cur_obj_is_mario_on_platform()) {
        o->oAction = 1;
    }
}

void elevator_act_3(void) {
    o->oVelY = 0;
    if (o->oTimer == 0) {
        cur_obj_shake_screen(SHAKE_POS_SMALL);
        cur_obj_play_sound_2(SOUND_GENERAL_ELEVATOR_LAND);
    }
    if (!mario_is_in_air_action() && !cur_obj_is_mario_on_platform()) {
        o->oAction = 0;
    }
}

void bhv_elevator_init(void) {
    s32 index = sElevatorHeights[o->oBehParams2ndByte * 3 + 2];
    if (index == 0) {
        o->oElevatorMinY = sElevatorHeights[o->oBehParams2ndByte * 3];
        o->oElevatorMaxY = o->oHomeY;
        o->oElevatorMidY = (o->oElevatorMinY + o->oElevatorMaxY) / 2;
        o->oElevatorType = cur_obj_has_behavior(bhvRrElevatorPlatform);
    } else {
        o->oElevatorMinY = sElevatorHeights[o->oBehParams2ndByte * 3];
        o->oElevatorMaxY = sElevatorHeights[o->oBehParams2ndByte * 3 + 1];
        o->oElevatorMidY = (o->oElevatorMinY + o->oElevatorMaxY) / 2;
        o->oElevatorType = 2;
    }
}

ObjActionFunc sElevatorActions[] = {
    elevator_act_0,
    elevator_act_1,
    elevator_act_2,
    elevator_act_3,
    elevator_act_4,
};

void bhv_elevator_loop(void) {
    cur_obj_call_action_function(sElevatorActions);
}
