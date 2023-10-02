// tower_platform.inc.c

void bhv_wf_solid_tower_platform_loop(void) {
    if (o->parentObj->oAction == 3) {
        obj_mark_for_deletion(o);
    }
}

void bhv_wf_elevator_tower_platform_loop(void) {
    switch (o->oAction) {
        case 0:
            if (gMarioObject->platform == o) {
                o->oAction++;
            }
            break;

        case 1:
            cur_obj_play_sound_1(SOUND_ENV_ELEVATOR1);
            if (o->oTimer > 140) {
                o->oAction++;
            } else {
                o->oPosY += 5.0f;
            }
            break;

        case 2:
            if (o->oTimer > 60) {
                o->oAction++;
            }
            break;

        case 3:
            cur_obj_play_sound_1(SOUND_ENV_ELEVATOR1);
            if (o->oTimer > 140) {
                o->oAction = 0;
            } else {
                o->oPosY -= 5.0f;
            }
            break;
    }

    if (o->parentObj->oAction == 3) {
        obj_mark_for_deletion(o);
    }
}

void bhv_wf_sliding_tower_platform_loop(void) {
    s32 moveTimer = o->oPlatformWFTowerMoveDistance / o->oPlatformWFTowerForwardVel;

    switch (o->oAction) {
        case 0:
            if (o->oTimer > moveTimer) {
                o->oAction++;
            }
            o->oForwardVel = -o->oPlatformWFTowerForwardVel;
            break;

        case 1:
            if (o->oTimer > moveTimer) {
                o->oAction = 0;
            }
            o->oForwardVel = o->oPlatformWFTowerForwardVel;
            break;
    }

    cur_obj_compute_vel_xz();

    o->oPosX += o->oVelX;
    o->oPosZ += o->oVelZ;

    if (o->parentObj->oAction == 3) {
        obj_mark_for_deletion(o);
    }
}

void spawn_and_init_wf_platforms(s16 a, const BehaviorScript *bhv) {
    struct Object *platform = spawn_object(o, a, bhv);

    s16 yaw = ((o->oPlatformSpawnerWFTowerPlatformNum * o->oPlatformSpawnerWFTowerDYaw) + o->oPlatformSpawnerWFTowerYawOffset);

    platform->oMoveAngleYaw = yaw;
    platform->oPosX += o->oPlatformSpawnerWFTowerRadius * sins(yaw);
    platform->oPosY += 100 * o->oPlatformSpawnerWFTowerPlatformNum;
    platform->oPosZ += o->oPlatformSpawnerWFTowerRadius * coss(yaw);
    platform->oPlatformWFTowerMoveDistance = o->oPlatformSpawnerWFTowerMoveDistance;
    platform->oPlatformWFTowerForwardVel = o->oPlatformSpawnerWFTowerForwardVel;

    o->oPlatformSpawnerWFTowerPlatformNum++;
}

void spawn_wf_platform_group(void) {
    o->oPlatformSpawnerWFTowerPlatformNum = 0;
    o->oPlatformSpawnerWFTowerYawOffset = 0;
    o->oPlatformSpawnerWFTowerDYaw = 0x2000;
    o->oPlatformSpawnerWFTowerRadius = 704.0f;
    o->oPlatformSpawnerWFTowerMoveDistance = 380.0f;
    o->oPlatformSpawnerWFTowerForwardVel = 3.0f;

    spawn_and_init_wf_platforms(MODEL_WF_TOWER_SQUARE_PLATORM, bhvWfSolidTowerPlatform);
    spawn_and_init_wf_platforms(MODEL_WF_TOWER_SQUARE_PLATORM, bhvWfSlidingTowerPlatform);
    spawn_and_init_wf_platforms(MODEL_WF_TOWER_SQUARE_PLATORM, bhvWfSolidTowerPlatform);
    spawn_and_init_wf_platforms(MODEL_WF_TOWER_SQUARE_PLATORM, bhvWfSlidingTowerPlatform);
    spawn_and_init_wf_platforms(MODEL_WF_TOWER_SQUARE_PLATORM, bhvWfSolidTowerPlatform);
    spawn_and_init_wf_platforms(MODEL_WF_TOWER_SQUARE_PLATORM, bhvWfSlidingTowerPlatform);
    spawn_and_init_wf_platforms(MODEL_WF_TOWER_SQUARE_PLATORM, bhvWfSolidTowerPlatform);
    spawn_and_init_wf_platforms(MODEL_WF_TOWER_SQUARE_PLATORM_ELEVATOR, bhvWfElevatorTowerPlatform);
}

void bhv_tower_platform_group_loop(void) {
    f32 marioY = gMarioObject->oPosY;

    o->oDistanceToMario = dist_between_objects(o, gMarioObject);

    switch (o->oAction) {
        case 0:
            if (marioY > o->oHomeY - 1000.0f) {
                o->oAction++;
            }
            break;

        case 1:
            spawn_wf_platform_group();
            o->oAction++;
            break;

        case 2:
            if (marioY < o->oHomeY - 1000.0f) {
                o->oAction++;
            }
            break;

        case 3:
            o->oAction = 0;
            break;
    }
}
