// door.inc.c

struct DoorAction {
    u32 flag;
    ObjAction32 action;
};

static struct DoorAction sDoorActions[] = {
    { INT_STATUS_WARP_DOOR_PULLED, DOOR_ACT_WARP_PULLED },
    { INT_STATUS_WARP_DOOR_PUSHED, DOOR_ACT_WARP_PUSHED },
    { INT_STATUS_DOOR_PULLED,      DOOR_ACT_PULLED      },
    { INT_STATUS_DOOR_PUSHED,      DOOR_ACT_PUSHED      },
    { -1,                          DOOR_ACT_CLOSED      },
};

static s32 sDoorOpenSounds[] = { SOUND_GENERAL_OPEN_WOOD_DOOR, SOUND_GENERAL_OPEN_IRON_DOOR };

static s32 sDoorCloseSounds[] = { SOUND_GENERAL_CLOSE_WOOD_DOOR, SOUND_GENERAL_CLOSE_IRON_DOOR };

void door_animation_and_reset(s32 animIndex) {
    cur_obj_init_animation_with_sound(animIndex);
    if (cur_obj_check_if_near_animation_end()) {
        o->oAction = DOOR_ACT_CLOSED;
    }
}

void set_door_camera_event(void) {
    if (segmented_to_virtual(bhvDoor) == o->behavior) {
        gPlayerCameraState->cameraEvent = CAM_EVENT_DOOR;
    } else {
        gPlayerCameraState->cameraEvent = CAM_EVENT_DOOR_WARP;
    }
    gPlayerCameraState->usedObj = o;
}

void play_door_open_noise(void) {
    s32 isMetalDoor = cur_obj_has_model(MODEL_HMC_METAL_DOOR);
    if (o->oTimer == 0) {
        cur_obj_play_sound_2(sDoorOpenSounds[isMetalDoor]);
        gTimeStopState |= TIME_STOP_MARIO_OPENED_DOOR;
    }
    if (o->oTimer == 70) {
        cur_obj_play_sound_2(sDoorCloseSounds[isMetalDoor]);
    }
}

void play_warp_door_open_noise(void) {
    s32 isMetalDoor = cur_obj_has_model(MODEL_HMC_METAL_DOOR);
    if (o->oTimer == 30) {
        cur_obj_play_sound_2(sDoorCloseSounds[isMetalDoor]);
    }
}

void bhv_door_loop(void) {
    s32 index = 0;

    while (sDoorActions[index].flag != 0xFFFFFFFF) {
        if (cur_obj_clear_interact_status_flag(sDoorActions[index].flag)) {
            set_door_camera_event();
            cur_obj_change_action(sDoorActions[index].action);
        }
        index++;
    }

    switch (o->oAction) {
        case DOOR_ACT_CLOSED:
            cur_obj_init_animation_with_sound(DOOR_ANIM_CLOSED);
            break;
        case DOOR_ACT_PULLED:
            door_animation_and_reset(DOOR_ANIM_PULLED);
            play_door_open_noise();
            break;
        case DOOR_ACT_PUSHED:
            door_animation_and_reset(DOOR_ANIM_PUSHED);
            play_door_open_noise();
            break;
        case DOOR_ACT_WARP_PULLED:
            door_animation_and_reset(DOOR_ANIM_WARP_PULLED);
            play_warp_door_open_noise();
            break;
        case DOOR_ACT_WARP_PUSHED:
            door_animation_and_reset(DOOR_ANIM_WARP_PUSHED);
            play_warp_door_open_noise();
            break;
    }
    if (o->oAction == DOOR_ACT_CLOSED) {
        load_object_collision_model();
    }
    bhv_door_rendering_loop();
}

void bhv_door_init(void) {
    f32 x = o->oPosX;
    f32 y = o->oPosY;
    f32 z = o->oPosZ;
    struct Surface *floor;

    find_room_floor(x, y, z, &floor);
    if (floor != NULL) o->oDoorSelfRoom = floor->room;

    x = o->oPosX + sins(o->oMoveAngleYaw) *  200.0f;
    z = o->oPosZ + coss(o->oMoveAngleYaw) *  200.0f;

    find_room_floor(x, y, z, &floor);
    if (floor != NULL) o->oDoorForwardRoom = floor->room;

    x = o->oPosX + sins(o->oMoveAngleYaw) * -200.0f;
    z = o->oPosZ + coss(o->oMoveAngleYaw) * -200.0f;

    find_room_floor(x, y, z, &floor);
    if (floor != NULL) o->oDoorBackwardRoom = floor->room;

    if (o->oDoorSelfRoom > 0 && o->oDoorSelfRoom < 60) {
        gDoorAdjacentRooms[o->oDoorSelfRoom][0] = o->oDoorForwardRoom;
        gDoorAdjacentRooms[o->oDoorSelfRoom][1] = o->oDoorBackwardRoom;
    }
}

void bhv_door_rendering_loop(void) {
    o->oDoorIsRendering = (
        gMarioCurrentRoom == 0
        || gMarioCurrentRoom == o->oDoorSelfRoom
        || gMarioCurrentRoom == o->oDoorForwardRoom
        || gMarioCurrentRoom == o->oDoorBackwardRoom
        || gDoorAdjacentRooms[gMarioCurrentRoom][0] == o->oDoorForwardRoom
        || gDoorAdjacentRooms[gMarioCurrentRoom][0] == o->oDoorBackwardRoom
        || gDoorAdjacentRooms[gMarioCurrentRoom][1] == o->oDoorForwardRoom
        || gDoorAdjacentRooms[gMarioCurrentRoom][1] == o->oDoorBackwardRoom
    );
    COND_BIT(o->oDoorIsRendering, o->header.gfx.node.flags, GRAPH_RENDER_ACTIVE);
}
