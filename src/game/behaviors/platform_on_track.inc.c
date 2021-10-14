
/**
 * Behavior for bhvPlatformOnTrack and bhvTrackBall.
 * The platform spawns up to 5 track balls at a time, which then despawn
 * themselves as the platform moves past them.
 */

/**
 * Collision models for the different types of platforms.
 */
static Collision const *sPlatformOnTrackCollisionModels[] = {
    /* PLATFORM_ON_TRACK_TYPE_CARPET    */ rr_seg7_collision_flying_carpet,
    /* PLATFORM_ON_TRACK_TYPE_SKI_LIFT  */ ccm_seg7_collision_ropeway_lift,
    /* PLATFORM_ON_TRACK_TYPE_CHECKERED */ checkerboard_platform_seg8_collision_platform,
    /* PLATFORM_ON_TRACK_TYPE_GRATE     */ bitfs_seg7_collision_platform_on_track,
};

/**
 * Paths for the different instances of these platforms.
 */
static Trajectory const *sPlatformOnTrackPaths[] = {
    rr_seg7_trajectory_0702EC3C,
    rr_seg7_trajectory_0702ECC0,
    ccm_seg7_trajectory_ropeway_lift,
    bitfs_seg7_trajectory_070159AC,
    hmc_seg7_trajectory_checkerboard_platform,
    lll_seg7_trajectory_0702856C,
    lll_seg7_trajectory_07028660,
    rr_seg7_trajectory_0702ED9C,
    rr_seg7_trajectory_0702EEE0,
};

static void platform_on_track_update_pos_or_spawn_ball(s32 ballIndex, Vec3f pos) {
    if ((ballIndex == 0) || (GET_BPARAM2(o->oBehParams) & PLATFORM_ON_TRACK_BP_SPAWN_BALLS)) {
        struct Waypoint *initialPrevWaypoint = o->oPlatformOnTrackPrevWaypoint;
        struct Waypoint *nextWaypoint = initialPrevWaypoint;
        struct Waypoint *prevWaypoint;
        f32 amountToMove;
        Vec3f d;
        f32 distToNextWaypoint;

        if (ballIndex != 0) {
            amountToMove = (300.0f * ballIndex);
        } else {
            obj_perform_position_op(POS_OP_SAVE_POSITION);
            o->oPlatformOnTrackPrevWaypointFlags = WAYPOINT_FLAGS_NONE;
            amountToMove = o->oForwardVel;
        }

        do {
            prevWaypoint = nextWaypoint;

            nextWaypoint++;
            if (nextWaypoint->flags == WAYPOINT_FLAGS_END) {
                if (ballIndex == 0) {
                    o->oPlatformOnTrackPrevWaypointFlags = WAYPOINT_FLAGS_END;
                }

                if ((GET_BPARAM1(o->oBehParams) & PLATFORM_ON_TRACK_BP_RETURN_TO_START)) {
                    nextWaypoint = o->oPlatformOnTrackStartWaypoint;
                } else {
                    return;
                }
            }

            vec3_diff(d, nextWaypoint->pos, pos);

            distToNextWaypoint = vec3_mag(d);

            // Move directly to the next waypoint, even if it's farther away
            // than amountToMove
            amountToMove -= distToNextWaypoint;
            vec3f_add(pos, d);
        } while (amountToMove > 0.0f);

        // If we moved farther than amountToMove, move in the opposite direction
        // No risk of near-zero division: If distToNextWaypoint is close to
        // zero, then that means we didn't cross a waypoint this frame (since
        // otherwise distToNextWaypoint would equal the distance between two
        // waypoints, which should never be that small). But this implies that
        // amountToMove - distToNextWaypoint <= 0, and amountToMove is at least
        // 0.1 (from platform on track behavior).
        distToNextWaypoint = amountToMove / distToNextWaypoint;
        pos[0] += d[0] * distToNextWaypoint;
        pos[1] += d[1] * distToNextWaypoint;
        pos[2] += d[2] * distToNextWaypoint;

        if (ballIndex != 0) {
            struct Object *trackBall = spawn_object_relative((o->oPlatformOnTrackBaseBallIndex + ballIndex), 0, 0, 0, o,
                                              MODEL_TRAJECTORY_MARKER_BALL, bhvTrackBall);
            if (trackBall != NULL) {
                vec3f_copy(&trackBall->oPosVec, pos);
            }
        } else {
            if (prevWaypoint != initialPrevWaypoint) {
                if (o->oPlatformOnTrackPrevWaypointFlags == WAYPOINT_FLAGS_NONE) {
                    o->oPlatformOnTrackPrevWaypointFlags = initialPrevWaypoint->flags;
                }
                o->oPlatformOnTrackPrevWaypoint = prevWaypoint;
            }

            vec3f_copy(&o->oPosVec, pos);

            obj_perform_position_op(POS_OP_COMPUTE_VELOCITY);

            o->oPlatformOnTrackPitch = atan2s(sqrtf(sqr(o->oVelX) + sqr(o->oVelZ)), -o->oVelY);
            o->oPlatformOnTrackYaw = atan2s(o->oVelZ, o->oVelX);
        }
    }
}

/**
 * Despawn all track balls and enter the init action.
 */
static void platform_on_track_reset(void) {
    o->oAction = PLATFORM_ON_TRACK_ACT_INIT;
    // This will cause the track balls to all despawn
    o->oPlatformOnTrackBaseBallIndex += 99;
}

/**
 * If this platform is the kind that disappears, pause for a while, then
 * begin blinking, and finally reset.
 */
static void platform_on_track_mario_not_on_platform(void) {
    if (!(GET_BPARAM1(o->oBehParams) & PLATFORM_ON_TRACK_BP_DONT_DISAPPEAR)) {
        // Once oTimer reaches 150, blink 40 times
        if (cur_obj_wait_then_blink(150, 40)) {
            platform_on_track_reset();
            o->header.gfx.node.flags &= ~GRAPH_RENDER_INVISIBLE;
        }
    }
}

/**
 * Init function for bhvPlatformOnTrack.
 */
void bhv_platform_on_track_init(void) {
    if (!(o->activeFlags & ACTIVE_FLAG_IN_DIFFERENT_ROOM)) {
        s16 pathIndex = (u16)(o->oBehParams >> 16) & PLATFORM_ON_TRACK_BP_MASK_PATH;
        o->oPlatformOnTrackType = ((u16)(o->oBehParams >> 16) & PLATFORM_ON_TRACK_BP_MASK_TYPE) >> 4;

        o->oPlatformOnTrackIsNotSkiLift = o->oPlatformOnTrackType - PLATFORM_ON_TRACK_TYPE_SKI_LIFT;

        o->collisionData =
            segmented_to_virtual(sPlatformOnTrackCollisionModels[o->oPlatformOnTrackType]);

        o->oPlatformOnTrackStartWaypoint = segmented_to_virtual(sPlatformOnTrackPaths[pathIndex]);

        o->oPlatformOnTrackIsNotHMC = pathIndex - 4;

        o->oBehParams2ndByte = o->oMoveAngleYaw; // TODO: Weird?

        if (o->oPlatformOnTrackType == PLATFORM_ON_TRACK_TYPE_CHECKERED) {
            o->header.gfx.scale[1] = 2.0f;
        }
    }
}

/**
 * Move to the start waypoint, spawn the first track balls, and enter the
 * wait for mario action.
 */
static void platform_on_track_act_init(void) {
    s32 i;

    o->oPlatformOnTrackPrevWaypoint = o->oPlatformOnTrackStartWaypoint;
    o->oPlatformOnTrackPrevWaypointFlags = WAYPOINT_FLAGS_NONE;
    o->oPlatformOnTrackBaseBallIndex = 0;

    vec3s_to_vec3f(&o->oHomeVec, o->oPlatformOnTrackStartWaypoint->pos);
    vec3f_copy(&o->oPosVec, &o->oHomeVec);

    o->oFaceAngleYaw = o->oBehParams2ndByte;
    o->oForwardVel = o->oVelX = o->oVelY = o->oVelZ = o->oPlatformOnTrackDistMovedSinceLastBall = 0.0f;

    o->oPlatformOnTrackWasStoodOn = FALSE;

    if (o->oPlatformOnTrackIsNotSkiLift) {
        o->oFaceAngleRoll = 0;
    }

    if (!(o->activeFlags & ACTIVE_FLAG_IN_DIFFERENT_ROOM)) {
        // Spawn track balls
        for (i = 1; i < 6; i++) {
            platform_on_track_update_pos_or_spawn_ball(i, &o->oHomeVec);
        }
    }

    o->oAction = PLATFORM_ON_TRACK_ACT_WAIT_FOR_MARIO;
}

/**
 * Wait for mario to stand on the platform for 20 frames, then begin moving.
 */
static void platform_on_track_act_wait_for_mario(void) {
    if (gMarioObject->platform == o) {
        if (o->oTimer > 20) {
            o->oAction = PLATFORM_ON_TRACK_ACT_MOVE_ALONG_TRACK;
        }
    } else {
        if (o->activeFlags & ACTIVE_FLAG_IN_DIFFERENT_ROOM) {
            platform_on_track_reset();
        }

        o->oTimer = 0;
    }
}

/**
 * Move along the track. After reaching the end, either start falling,
 * return to the init action, or continue moving back to the start waypoint.
 */
static void platform_on_track_act_move_along_track(void) {
    s16 initialAngle;

    if (!o->oPlatformOnTrackIsNotSkiLift) {
        cur_obj_play_sound_1(SOUND_ENV_ELEVATOR3);
    } else if (!o->oPlatformOnTrackIsNotHMC) {
        cur_obj_play_sound_1(SOUND_ENV_ELEVATOR1);
    }

    // Fall after reaching the last waypoint if desired
    if (o->oPlatformOnTrackPrevWaypointFlags == WAYPOINT_FLAGS_END
     && !(GET_BPARAM1(o->oBehParams) & PLATFORM_ON_TRACK_BP_RETURN_TO_START)) {
        o->oAction = PLATFORM_ON_TRACK_ACT_FALL;
    } else {
        // The ski lift should pause or stop after reaching a special waypoint
        if (o->oPlatformOnTrackPrevWaypointFlags != WAYPOINT_FLAGS_NONE && !o->oPlatformOnTrackIsNotSkiLift) {
            if (o->oPlatformOnTrackPrevWaypointFlags == WAYPOINT_FLAGS_END
             || o->oPlatformOnTrackPrevWaypointFlags == WAYPOINT_FLAGS_PLATFORM_ON_TRACK_PAUSE) {
                cur_obj_play_sound_2(SOUND_GENERAL_ELEVATOR_WOBBLE_LOWPRIO);

                o->oForwardVel = 0.0f;
                if (o->oPlatformOnTrackPrevWaypointFlags == WAYPOINT_FLAGS_END) {
                    o->oAction = PLATFORM_ON_TRACK_ACT_INIT;
                } else {
                    o->oAction = PLATFORM_ON_TRACK_ACT_PAUSE_BRIEFLY;
                }
            }
        } else {
            // The ski lift accelerates, while the others instantly start
            if (!o->oPlatformOnTrackIsNotSkiLift) {
                obj_forward_vel_approach(10.0, 0.1f);
            } else {
#ifdef CONTROLLABLE_PLATFORM_SPEED
                f32 targetVel = gMarioObject->platform == o
                    ? (o->oDistanceToMario * coss(o->oAngleToMario - o->oMoveAngleYaw)) - 10.0f
                    : 10.0f;
                if (targetVel < 10.0f) {
                    targetVel = 10.0f;
                } else if (targetVel > 16.0f) {
                    targetVel = 16.0f;
                }
                obj_forward_vel_approach(targetVel, 0.1f);
#else
                o->oForwardVel = 10.0f;
#endif
            }

            // Spawn a new track ball if necessary
            if (approach_f32_ptr(&o->oPlatformOnTrackDistMovedSinceLastBall, 300.0f, o->oForwardVel)) {
                o->oPlatformOnTrackDistMovedSinceLastBall -= 300.0f;

                vec3f_copy(&o->oHomeVec, &o->oPosVec);
                o->oPlatformOnTrackBaseBallIndex = (u16)(o->oPlatformOnTrackBaseBallIndex + 1);

                platform_on_track_update_pos_or_spawn_ball(5, &o->oHomeVec);
            }
        }

        platform_on_track_update_pos_or_spawn_ball(0, &o->oPosVec);

        o->oMoveAnglePitch = o->oPlatformOnTrackPitch;
        o->oMoveAngleYaw = o->oPlatformOnTrackYaw;

        //! Both oAngleVelYaw and oAngleVelRoll aren't reset until the platform
        //  starts moving again, resulting in unexpected platform displacement
        //  after reappearing

        // Turn face yaw and compute yaw vel
        if (!(GET_BPARAM1(o->oBehParams) & PLATFORM_ON_TRACK_BP_DONT_TURN_YAW)) {
            s16 targetFaceYaw = o->oMoveAngleYaw + 0x4000;
            s16 yawSpeed = abs_angle_diff(targetFaceYaw, o->oFaceAngleYaw) / 20;

            initialAngle = o->oFaceAngleYaw;
            clamp_s16(&yawSpeed, 100, 500);
            obj_face_yaw_approach(targetFaceYaw, yawSpeed);
            o->oAngleVelYaw = (s16) o->oFaceAngleYaw - initialAngle;
        }

        // Turn face roll and compute roll vel
        if ((GET_BPARAM1(o->oBehParams) & PLATFORM_ON_TRACK_BP_DONT_TURN_ROLL)) {
            s16 rollSpeed = abs_angle_diff(o->oMoveAnglePitch, o->oFaceAngleRoll) / 20;

            initialAngle = o->oFaceAngleRoll;
            clamp_s16(&rollSpeed, 100, 500);
            //! If the platform is moving counterclockwise upward or
            //  clockwise downward, this will be backward
            obj_face_roll_approach(o->oMoveAnglePitch, rollSpeed);
            o->oAngleVelRoll = (s16) o->oFaceAngleRoll - initialAngle;
        }
    }

    if (gMarioObject->platform != o) {
        platform_on_track_mario_not_on_platform();
    } else {
        o->oTimer = 0;
        o->header.gfx.node.flags &= ~GRAPH_RENDER_INVISIBLE;
    }
}

/**
 * Wait 20 frames then continue moving.
 */
static void platform_on_track_act_pause_briefly(void) {
    if (o->oTimer > 20) {
        o->oAction = PLATFORM_ON_TRACK_ACT_MOVE_ALONG_TRACK;
    }
}

/**
 * Fall downward with no terminal velocity, stopping after reaching y = -12k
 * and eventually blinking and disappearing.
 */
static void platform_on_track_act_fall(void) {
    cur_obj_move_using_vel_and_gravity();

    if (gMarioObject->platform != o) {
        platform_on_track_mario_not_on_platform();
    } else {
        o->oTimer = 0;
        o->header.gfx.node.flags &= ~GRAPH_RENDER_INVISIBLE;
    }
}

/**
 * Control the rocking of the ski lift.
 */
static void platform_on_track_rock_ski_lift(void) {
    s32 targetRoll = 0;

    o->oFaceAngleRoll += (s32) o->oPlatformOnTrackSkiLiftRollVel;

    // Tilt away from the moving direction and toward mario
    if (gMarioObject->platform == o) {
        targetRoll = o->oForwardVel * sins(o->oMoveAngleYaw) * -50.0f
                     + (s32)(o->oDistanceToMario * sins(o->oAngleToMario - o->oFaceAngleYaw) * -4.0f);
    }

    oscillate_toward(
        /* value          */ &o->oFaceAngleRoll,
        /* vel            */ &o->oPlatformOnTrackSkiLiftRollVel,
        /* target         */ targetRoll,
        /* velCloseToZero */ 5.0f,
        /* accel          */ 6.0f,
        /* slowdown       */ 1.5f);
    clamp_f32(&o->oPlatformOnTrackSkiLiftRollVel, -100.0f, 100.0f);
}

/**
 * Update function for bhvPlatformOnTrack.
 */
void bhv_platform_on_track_update(void) {
    switch (o->oAction) {
        case PLATFORM_ON_TRACK_ACT_INIT:
            platform_on_track_act_init();
            break;
        case PLATFORM_ON_TRACK_ACT_WAIT_FOR_MARIO:
            platform_on_track_act_wait_for_mario();
            break;
        case PLATFORM_ON_TRACK_ACT_MOVE_ALONG_TRACK:
            platform_on_track_act_move_along_track();
            break;
        case PLATFORM_ON_TRACK_ACT_PAUSE_BRIEFLY:
            platform_on_track_act_pause_briefly();
            break;
        case PLATFORM_ON_TRACK_ACT_FALL:
            platform_on_track_act_fall();
            break;
    }

    if (!o->oPlatformOnTrackIsNotSkiLift) {
        platform_on_track_rock_ski_lift();
    } else if (o->oPlatformOnTrackType == PLATFORM_ON_TRACK_TYPE_CARPET) {
#ifdef CONTROLLABLE_PLATFORM_SPEED
        Angle targetRoll; // Visually, this is the platform's pitch, since these platforms technically move sideways
        if (gMarioObject->platform == o) {
            if (!o->oPlatformOnTrackWasStoodOn) {
                o->oPlatformOnTrackOffsetY    = -8.0f;
                o->oPlatformOnTrackWasStoodOn = TRUE;
            }
            targetRoll = o->oDistanceToMario * coss(o->oAngleToMario - o->oMoveAngleYaw) * 0x4;
        } else {
            targetRoll = 0x0;
        }
        o->oFaceAngleRoll = approach_s32_symmetric(o->oFaceAngleRoll, targetRoll, 0x100);
#else

        if (!o->oPlatformOnTrackWasStoodOn && gMarioObject->platform == o) {
            o->oPlatformOnTrackOffsetY = -8.0f;
            o->oPlatformOnTrackWasStoodOn = TRUE;
        }

        approach_f32_ptr(&o->oPlatformOnTrackOffsetY, 0.0f, 0.5f);
        o->oPosY += o->oPlatformOnTrackOffsetY;
#endif
    }
}

/**
 * Update function for bhvTrackBall.
 */
void bhv_track_ball_update(void) {
    // Despawn after the elevator passes this ball
    s16 relativeIndex =
        (s16) o->oBehParams2ndByte - (s16) o->parentObj->oPlatformOnTrackBaseBallIndex - 1;
    if (relativeIndex < 1 || relativeIndex > 5) {
        obj_mark_for_deletion(o);
    }
}
