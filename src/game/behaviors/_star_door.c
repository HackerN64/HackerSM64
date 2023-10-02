// star_door.inc.c

void star_door_update_pos(void) {
    o->oVelX = (o->oLeftVel) * coss(o->oMoveAngleYaw);
    o->oVelZ = (o->oLeftVel) * -sins(o->oMoveAngleYaw);
    o->oPosX += o->oVelX;
    o->oPosZ += o->oVelZ;
}

void bhv_star_door_loop(void) {
    struct Object *doorObj = cur_obj_nearest_object_with_behavior(bhvStarDoor);

    switch (o->oAction) {
        case STAR_DOOR_ACT_CLOSED:
            cur_obj_become_tangible();
            if (o->oInteractStatus & (INT_STATUS_DOOR_PULLED | INT_STATUS_DOOR_PUSHED)) {
                o->oAction = STAR_DOOR_ACT_OPENING;
            }
            if (doorObj != NULL && doorObj->oAction != STAR_DOOR_ACT_CLOSED) {
                o->oAction = STAR_DOOR_ACT_OPENING;
            }
            break;

        case STAR_DOOR_ACT_OPENING:
            if (o->oTimer == 0 && (s16) o->oMoveAngleYaw >= 0) {
                cur_obj_play_sound_2(SOUND_GENERAL_STAR_DOOR_OPEN);
#if ENABLE_RUMBLE
                queue_rumble_data(35, 30);
#endif
            }
            cur_obj_become_intangible();
            o->oLeftVel = -8.0f;
            star_door_update_pos();
            if (o->oTimer > 15) {
                o->oAction++; // STAR_DOOR_ACT_OPEN
            }
            break;

        case STAR_DOOR_ACT_OPEN:
            if (o->oTimer > 30) {
                o->oAction++; // STAR_DOOR_ACT_CLOSING
            }
            break;

        case STAR_DOOR_ACT_CLOSING:
            if (o->oTimer == 0 && (s16) o->oMoveAngleYaw >= 0) {
                cur_obj_play_sound_2(SOUND_GENERAL_STAR_DOOR_CLOSE);
#if ENABLE_RUMBLE
                queue_rumble_data(35, 30);
#endif
            }
            o->oLeftVel = 8.0f;
            star_door_update_pos();
            if (o->oTimer > 15) {
                o->oAction++; // STAR_DOOR_ACT_RESET
            }
            break;

        case STAR_DOOR_ACT_RESET:
            o->oInteractStatus = INT_STATUS_NONE;
            o->oAction = STAR_DOOR_ACT_CLOSED;
            break;
    }
}
