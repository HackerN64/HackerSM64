// hoot.inc.c

void bhv_hoot_init(void) {
    cur_obj_init_animation(HOOT_ANIM_DEFAULT);
    Vec3f homeOffset = { 800.0f, -150.0f, 300.0f };
    vec3f_sum(&o->oHomeVec, &o->oPosVec, homeOffset);
    o->header.gfx.node.flags |= GRAPH_RENDER_INVISIBLE;

    cur_obj_become_intangible();
}

f32 hoot_find_next_floor(f32 dist) {
    f32 nextX = (dist * sins(o->oMoveAngleYaw)) + o->oPosX;
    f32 nextZ = (dist * coss(o->oMoveAngleYaw)) + o->oPosZ;

    return find_floor_height(nextX, CELL_HEIGHT_LIMIT, nextZ);
}

void hoot_floor_bounce(void) {
    f32 floorY = hoot_find_next_floor(375.0f);

    if (floorY + 75.0f > o->oPosY) {
        o->oMoveAnglePitch -= DEGREES(20);
    }

    floorY = hoot_find_next_floor(200.0f);
    if (floorY + 125.0f > o->oPosY) {
        o->oMoveAnglePitch -= DEGREES(40);
    }

    floorY = hoot_find_next_floor(0.0f);
    if (floorY + 125.0f > o->oPosY) {
        o->oPosY = (floorY + 125.0f);
    }

    if (o->oMoveAnglePitch < -DEGREES(120)) {
        o->oMoveAnglePitch = -DEGREES(120);
    }
}

void hoot_free_step(s16 fastOscY, s32 speed) {
    struct Surface *floor;
    s16 yaw = o->oMoveAngleYaw;
    s16 pitch = o->oMoveAnglePitch;
    s16 animFrame = o->header.gfx.animInfo.animFrame;
    f32 xPrev = o->oPosX;
    f32 zPrev = o->oPosZ;

    o->oVelY = sins(pitch) * speed;
    f32 hSpeed = coss(pitch) * speed;
    o->oVelX = sins(yaw) * hSpeed;
    o->oVelZ = coss(yaw) * hSpeed;

    o->oPosX += o->oVelX;
    o->oPosY -= o->oVelY + (coss((s32)(animFrame * (fastOscY == 0 ? 3276.8f : 6553.6f))) * 12.5f); // 50.0f / 4;
    o->oPosZ += o->oVelZ;

    find_floor(o->oPosX, o->oPosY, o->oPosZ, &floor);
    if (floor == NULL) {
        o->oPosX = xPrev;
        o->oPosZ = zPrev;
    }

    if (animFrame == 0) {
        cur_obj_play_sound_2(SOUND_GENERAL_WING_FLAP);
    }
}

void hoot_player_set_yaw(void) {
#ifdef HOOT_YAW_FIX
    Angle turnSpeed  = gMarioState->intendedMag * 0x20;
    o->oMoveAngleYaw = approach_s16_symmetric(o->oMoveAngleYaw, gMarioState->intendedYaw, turnSpeed);
#else
    s16 stickX = gPlayer3Controller->rawStickX;
    s16 stickY = gPlayer3Controller->rawStickY;
    if (stickX < 10 && stickX > -10) stickX = 0;
    if (stickY < 10 && stickY > -10) stickY = 0;
    o->oMoveAngleYaw -= 5 * stickX;
#endif
}

void hoot_carry_step(s32 speed, UNUSED f32 xPrev, UNUSED f32 zPrev) {
    s16 yaw = o->oMoveAngleYaw;
    s16 pitch = o->oMoveAnglePitch;
    s16 animFrame = o->header.gfx.animInfo.animFrame;

    o->oVelY = sins(pitch) * speed;
    f32 hSpeed = coss(pitch) * speed;
    o->oVelX = sins(yaw) * hSpeed;
    o->oVelZ = coss(yaw) * hSpeed;

    o->oPosX += o->oVelX;
    o->oPosY -= o->oVelY + (coss((s32)(animFrame * 6553.6f)) * 12.5f); // 50.0f / 4;
    o->oPosZ += o->oVelZ;

    if (animFrame == 0) {
        cur_obj_play_sound_2(SOUND_GENERAL_WING_FLAP);
    }
}

void hoot_surface_collision(f32 xPrev, UNUSED f32 yPrev, f32 zPrev) {
    struct Surface *floor;
    struct WallCollisionData hitbox;

    hitbox.x = o->oPosX;
    hitbox.y = o->oPosY;
    hitbox.z = o->oPosZ;
    hitbox.offsetY = 10.0f;
    hitbox.radius = 50.0f;

    if (find_wall_collisions(&hitbox)) {
        vec3f_set(&o->oPosVec, hitbox.x, hitbox.y, hitbox.z);
        gMarioObject->oInteractStatus |= INT_STATUS_MARIO_DROP_FROM_HOOT;
    }

    f32 floorY = find_floor(o->oPosX, o->oPosY, o->oPosZ, &floor);
    if (floor == NULL) {
        o->oPosX = xPrev;
        o->oPosZ = zPrev;
        return;
    }

    if (absf(o->oPosX) > 8000.0f) o->oPosX = xPrev;

    if (absf(o->oPosZ) > 8000.0f) o->oPosZ = zPrev;

    if (floorY + 125.0f > o->oPosY) {
        o->oPosY = floorY + 125.0f;
    }
}

void hoot_act_ascent(f32 xPrev, f32 zPrev) {
    f32 negX = 0 - o->oPosX;
    f32 negZ = 0 - o->oPosZ;
    s16 angleToOrigin = atan2s(negZ, negX);

    o->oMoveAngleYaw = approach_s16_symmetric(o->oMoveAngleYaw, angleToOrigin, 0x500);
    o->oMoveAnglePitch = DEGREES(290);

    if (o->oTimer >= 29) {
        cur_obj_play_sound_1(SOUND_ENV_WIND2);
        o->header.gfx.animInfo.animFrame = 1;
    }

    if (o->oPosY > 6500.0f) {
        o->oAction = HOOT_ACT_CARRY;
    }

    hoot_carry_step(60, xPrev, zPrev);
}

void hoot_action_loop(void) {
    f32 xPrev = o->oPosX;
    f32 yPrev = o->oPosY;
    f32 zPrev = o->oPosZ;

    switch (o->oAction) {
        case HOOT_ACT_ASCENT:
            hoot_act_ascent(xPrev, zPrev);
            break;

        case HOOT_ACT_CARRY:
            hoot_player_set_yaw();

            o->oMoveAnglePitch = 0x71C;

            if (o->oPosY < 2700.0f) {
                set_time_stop_flags(TIME_STOP_ENABLED | TIME_STOP_MARIO_AND_DOORS);

                if (cutscene_object_with_dialog(CUTSCENE_DIALOG, o, DIALOG_045)) {
                    clear_time_stop_flags(TIME_STOP_ENABLED | TIME_STOP_MARIO_AND_DOORS);
                    o->oAction = HOOT_ACT_TIRED;
                }
            }

            hoot_carry_step(20, xPrev, zPrev);
            break;

        case HOOT_ACT_TIRED:
            hoot_player_set_yaw();

            o->oMoveAnglePitch = 0;

            hoot_carry_step(20, xPrev, zPrev);

            if (o->oTimer > 60) {
                gMarioObject->oInteractStatus |= INT_STATUS_MARIO_DROP_FROM_HOOT;
            }
            break;
    }

    hoot_surface_collision(xPrev, yPrev, zPrev);
}

void hoot_turn_to_home(void) {
    Angle pitchToHome, yawToHome;
    vec3f_get_angle(&o->oPosVec, &o->oHomeVec, &pitchToHome, &yawToHome);

    o->oMoveAngleYaw = approach_s16_symmetric(o->oMoveAngleYaw, yawToHome, 0x140);
    o->oMoveAnglePitch = approach_s16_symmetric(o->oMoveAnglePitch, -pitchToHome, 0x140);
}

void hoot_awake_loop(void) {
    if (o->oInteractStatus == TRUE) { //! Note: Not a flag, treated as a TRUE/FALSE statement
        hoot_action_loop();
        cur_obj_init_animation(HOOT_ANIM_HOLDING_MARIO);
    } else {
        cur_obj_init_animation(HOOT_ANIM_DEFAULT);

        hoot_turn_to_home();
        hoot_floor_bounce();
        hoot_free_step(0, 10);

        o->oAction = 0;
        o->oTimer = 0;
    }

    set_object_visibility(o, 2000);
}

void bhv_hoot_loop(void) {
    switch (o->oHootAvailability) {
        case HOOT_AVAIL_ASLEEP_IN_TREE:
            if (is_point_within_radius_of_mario(o->oPosX, o->oPosY, o->oPosZ, 50)) {
                o->header.gfx.node.flags &= ~GRAPH_RENDER_INVISIBLE;
                o->oHootAvailability = HOOT_AVAIL_WANTS_TO_TALK;
            }
#ifdef HOOT_TREE_PARTICLES
            if (random_float() < 0.05f) {
                struct Object *obj   = spawn_object(o, MODEL_LEAVES, bhvTreeLeaf);
                f32 scale            = (random_float() * 3.0f);
                obj_scale(obj, scale);
                obj->oMoveAngleYaw   = random_u16();
                obj->oForwardVel     = ((random_float() *  5.0f) + 5.0f);
                obj->oVelY           =  (random_float() * 15.0f);
                obj->oFaceAnglePitch = random_u16();
                obj->oFaceAngleRoll  = random_u16();
                obj->oFaceAngleYaw   = random_u16();
            }
#endif
            break;

        case HOOT_AVAIL_WANTS_TO_TALK:
            hoot_awake_loop();

            if (set_mario_npc_dialog(MARIO_DIALOG_LOOK_UP) == MARIO_DIALOG_STATUS_SPEAK 
                && cutscene_object_with_dialog(CUTSCENE_DIALOG, o, DIALOG_044)) {
                set_mario_npc_dialog(MARIO_DIALOG_STOP);

                cur_obj_become_tangible();

                o->oHootAvailability = HOOT_AVAIL_READY_TO_FLY;
            }
            break;

        case HOOT_AVAIL_READY_TO_FLY:
            hoot_awake_loop();
            break;
    }
}
