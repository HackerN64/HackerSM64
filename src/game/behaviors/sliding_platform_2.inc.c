// sliding_platform_2.inc.c

static Collision const *sSlidingPlatformsCollisionData[] = {
    bits_seg7_collision_0701A9A0,
    bits_seg7_collision_0701AA0C,
    bitfs_seg7_collision_07015714,
    bitfs_seg7_collision_07015768,
    rr_seg7_collision_070295F8,
    rr_seg7_collision_0702967C,
    NULL,
    bitdw_seg7_collision_0700F688,
};

void bhv_sliding_platform_init(void) {
    s32 collisionDataIndex = GET_BPARAM1(o->oBehParams) & SLIDING_PLATFORM_BP1_TYPES_MASK;

    o->collisionData = segmented_to_virtual(sSlidingPlatformsCollisionData[collisionDataIndex]);
    o->oBackAndForthPlatformPathLength = 50.0f * (GET_BPARAM2(o->oBehParams) & SLIDING_PLATFORM_BP2_LENGTH_MASK);

    if (collisionDataIndex < SLIDING_PLATFORM_BP1_RR_PYRAMID || collisionDataIndex > SLIDING_PLATFORM_BP1_NULL) {
        o->oBackAndForthPlatformVel = 15.0f;
        if (GET_BPARAM2(o->oBehParams) & SLIDING_PLATFORM_BP2_FLAG_INVERTED) {
            o->oMoveAngleYaw += 0x8000;
        }
    } else { // Applies to both unused types:
        o->oBackAndForthPlatformVel = 10.0f;
        if (GET_BPARAM2(o->oBehParams) & SLIDING_PLATFORM_BP2_FLAG_INVERTED) {
            o->oBackAndForthPlatformDirection = -1.0f;
        } else {
            o->oBackAndForthPlatformDirection = 1.0f;
        }
    }
}

void bhv_sliding_platform_loop(void) {
    if (o->oTimer > 10) {
        o->oBackAndForthPlatformDistance += o->oBackAndForthPlatformVel;
        if (clamp_f32(&o->oBackAndForthPlatformDistance, -o->oBackAndForthPlatformPathLength, 0.0f)) {
            o->oBackAndForthPlatformVel = -o->oBackAndForthPlatformVel;
            o->oTimer = 0;
        }
    }

    obj_perform_position_op(POS_OP_SAVE_POSITION);

    if (o->oBackAndForthPlatformDirection != 0.0f) {
        o->oPosY = o->oHomeY + o->oBackAndForthPlatformDistance * o->oBackAndForthPlatformDirection;
    } else {
        obj_set_dist_from_home(o->oBackAndForthPlatformDistance);
    }

    obj_perform_position_op(POS_OP_COMPUTE_VELOCITY);
}
