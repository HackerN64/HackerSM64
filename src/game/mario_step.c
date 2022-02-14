#include <ultra64.h>

#include "sm64.h"
#include "engine/math_util.h"
#include "engine/surface_collision.h"
#include "engine/surface_load.h"
#include "mario.h"
#include "audio/external.h"
#include "game_init.h"
#include "interaction.h"
#include "mario_step.h"

#include "config.h"

static s16 sMovingSandSpeeds[] = { 12, 8, 4, 0 };

struct Surface gWaterSurfacePseudoFloor = {
    SURFACE_VERY_SLIPPERY,      // type
    0x0,                        // force
    0x0,                        // flags
     0,                         // room
    -SURFACE_VERTICAL_BUFFER,   // lowerY
     SURFACE_VERTICAL_BUFFER,   // upperY
    { 0, 0, 0 },                // vertex1
    { 0, 0, 0 },                // vertex2
    { 0, 0, 0 },                // vertex3
    { 0.0f, 1.0f, 0.0f },       // normal
    0.0f,                       // originOffset
    NULL,                       // object
};

/**
 * Always returns zero. This may have been intended
 * to be used for the beta trampoline. Its return value
 * is used by set_mario_y_vel_based_on_fspeed as a constant
 * addition to Mario's Y velocity. Given the closeness of
 * this function to stub_mario_step_2, it is probable that this
 * was intended to check whether a trampoline had made itself
 * known through stub_mario_step_2 and whether Mario was on it,
 * and if so return a higher value than 0.
 */
f32 get_additive_y_vel_for_jumps(void) {
    return 0.0f;
}

/**
 * Does nothing, but takes in a MarioState. This is only ever
 * called by update_mario_inputs, which is called as part of Mario's
 * update routine. Due to its proximity to stub_mario_step_2, an
 * incomplete trampoline function, and get_additive_y_vel_for_jumps,
 * a potentially trampoline-related function, it is plausible that
 * this could be used for checking if Mario was on the trampoline.
 * It could, for example, make him bounce.
 */
void stub_mario_step_1(UNUSED struct MarioState *x) {
}

/**
 * Does nothing. This is only called by the beta trampoline.
 * Due to its proximity to get_additive_y_vel_for_jumps, another
 * currently-pointless function, it is probable that this was used
 * by the trampoline to make itself known to get_additive_y_vel_for_jumps,
 * or to set a variable with its intended additive Y vel.
 */
void stub_mario_step_2(void) {
}

void transfer_bully_speed(struct BullyCollisionData *obj1, struct BullyCollisionData *obj2) {
    f32 rx = obj2->posX - obj1->posX;
    f32 rz = obj2->posZ - obj1->posZ;

    //! Bully NaN crash
    f32 rzx = sqr(rx) + sqr(rz);
    f32 projectedV1 = (rx * obj1->velX + rz * obj1->velZ) / rzx;
    f32 projectedV2 = (-rx * obj2->velX - rz * obj2->velZ) / rzx;

    // Kill speed along r. Convert one object's speed along r and transfer it to
    // the other object.
    f32 p1x = projectedV1 * rx;
    f32 p1z = projectedV1 * rz;
    f32 p2x = projectedV2 * -rx;
    f32 p2z = projectedV2 * -rz;

    obj2->velX += obj2->conversionRatio * p1x - p2x;
    obj2->velZ += obj2->conversionRatio * p1z - p2z;

    obj1->velX += -p1x + obj1->conversionRatio * p2x;
    obj1->velZ += -p1z + obj1->conversionRatio * p2z;

    //! Bully battery
}

void init_bully_collision_data(struct BullyCollisionData *data, f32 posX, f32 posZ,
                               f32 forwardVel, s16 yaw, f32 conversionRatio, f32 radius) {
    if (forwardVel < 0.0f) {
        forwardVel *= -1.0f;
        yaw += 0x8000;
    }

    data->radius = radius;
    data->conversionRatio = conversionRatio;
    data->posX = posX;
    data->posZ = posZ;
    data->velX = forwardVel * sins(yaw);
    data->velZ = forwardVel * coss(yaw);
}

void mario_bonk_reflection(struct MarioState *m, u32 negateSpeed) {
    if (m->wall != NULL) {
        s16 wallAngle = m->wallYaw;
        m->faceAngle[1] = wallAngle - (s16)(m->faceAngle[1] - wallAngle);

        play_sound((m->flags & MARIO_METAL_CAP) ? SOUND_ACTION_METAL_BONK : SOUND_ACTION_BONK,
                   m->marioObj->header.gfx.cameraToObject);
    } else {
        play_sound(SOUND_ACTION_HIT, m->marioObj->header.gfx.cameraToObject);
    }

    if (negateSpeed) {
        mario_set_forward_vel(m, -m->forwardVel);
    } else {
        m->faceAngle[1] += 0x8000;
    }
}

u32 mario_update_quicksand(struct MarioState *m, f32 sinkingSpeed) {
    if (m->action & ACT_FLAG_RIDING_SHELL) {
        m->quicksandDepth = 0.0f;
    } else {
        if (m->quicksandDepth < 1.1f) {
            m->quicksandDepth = 1.1f;
        }

        switch (m->floor->type) {
            case SURFACE_SHALLOW_QUICKSAND:
                if ((m->quicksandDepth += sinkingSpeed) >= 10.0f) {
                    m->quicksandDepth = 10.0f;
                }
                break;

            case SURFACE_SHALLOW_MOVING_QUICKSAND:
                if ((m->quicksandDepth += sinkingSpeed) >= 25.0f) {
                    m->quicksandDepth = 25.0f;
                }
                break;

            case SURFACE_QUICKSAND:
            case SURFACE_MOVING_QUICKSAND:
                if ((m->quicksandDepth += sinkingSpeed) >= 60.0f) {
                    m->quicksandDepth = 60.0f;
                }
                break;

            case SURFACE_DEEP_QUICKSAND:
            case SURFACE_DEEP_MOVING_QUICKSAND:
                if ((m->quicksandDepth += sinkingSpeed) >= 160.0f) {
                    update_mario_sound_and_camera(m);
                    return drop_and_set_mario_action(m, ACT_QUICKSAND_DEATH, 0);
                }
                break;

            case SURFACE_INSTANT_QUICKSAND:
            case SURFACE_INSTANT_MOVING_QUICKSAND:
                update_mario_sound_and_camera(m);
                return drop_and_set_mario_action(m, ACT_QUICKSAND_DEATH, 0);
                break;

            default:
                m->quicksandDepth = 0.0f;
                break;
        }
    }

    return FALSE;
}

u32 mario_push_off_steep_floor(struct MarioState *m, u32 action, u32 actionArg) {
    s16 floorDYaw = m->floorYaw - m->faceAngle[1];

    if (floorDYaw > -0x4000 && floorDYaw < 0x4000) {
        m->forwardVel = 16.0f;
        m->faceAngle[1] = m->floorYaw;
    } else {
        m->forwardVel = -16.0f;
        m->faceAngle[1] = m->floorYaw + 0x8000;
    }

    return set_mario_action(m, action, actionArg);
}

u32 mario_update_moving_sand(struct MarioState *m) {
    struct Surface *floor = m->floor;
    s32 floorType = floor->type;

    if (floorType == SURFACE_DEEP_MOVING_QUICKSAND || floorType == SURFACE_SHALLOW_MOVING_QUICKSAND
        || floorType == SURFACE_MOVING_QUICKSAND || floorType == SURFACE_INSTANT_MOVING_QUICKSAND) {
        s16 pushAngle = floor->force << 8;
        f32 pushSpeed = sMovingSandSpeeds[floor->force >> 8];

        m->vel[0] += pushSpeed * sins(pushAngle);
        m->vel[2] += pushSpeed * coss(pushAngle);

        return TRUE;
    }

    return FALSE;
}

u32 mario_update_windy_ground(struct MarioState *m) {
    struct Surface *floor = m->floor;

    if (floor->type == SURFACE_HORIZONTAL_WIND) {
        f32 pushSpeed;
        s16 pushAngle = floor->force << 8;

        if (m->action & ACT_FLAG_MOVING) {
            s16 pushDYaw = m->faceAngle[1] - pushAngle;

            pushSpeed = m->forwardVel > 0.0f ? -m->forwardVel * 0.5f : -8.0f;

            if (pushDYaw > -0x4000 && pushDYaw < 0x4000) {
                pushSpeed *= -1.0f;
            }

            pushSpeed *= coss(pushDYaw);
        } else {
            pushSpeed = 3.2f + (gGlobalTimer % 4);
        }

        m->vel[0] += pushSpeed * sins(pushAngle);
        m->vel[2] += pushSpeed * coss(pushAngle);

        return TRUE;
    }

    return FALSE;
}

void stop_and_set_height_to_floor(struct MarioState *m) {
    struct Object *marioObj = m->marioObj;

    mario_set_forward_vel(m, 0.0f);
    m->vel[1] = 0.0f;

    //! This is responsible for some downwarps.
    m->pos[1] = m->floorHeight;

    vec3f_copy(marioObj->header.gfx.pos, m->pos);
    vec3s_set(marioObj->header.gfx.angle, 0, m->faceAngle[1], 0);
}

s32 stationary_ground_step(struct MarioState *m) {
    struct Object *marioObj = m->marioObj;
    u32 stepResult = GROUND_STEP_NONE;

    mario_set_forward_vel(m, 0.0f);

    u32 takeStep = (mario_update_moving_sand(m) | mario_update_windy_ground(m));
    if (takeStep) {
        stepResult = perform_ground_step(m);
    } else {
        // Hackersm64: this condition fixes potential downwarps
        if (m->pos[1] <= m->floorHeight + 160.0f) {
            m->pos[1] = m->floorHeight;
        }

        vec3f_copy(marioObj->header.gfx.pos, m->pos);
        vec3s_set(marioObj->header.gfx.angle, 0, m->faceAngle[1], 0);
    }

    return stepResult;
}

static s32 perform_ground_quarter_step(struct MarioState *m, Vec3f nextPos) {
    struct WallCollisionData lowerWall, upperWall;
    struct Surface *ceil, *floor;

    s16 i;
    s16 wallDYaw;
    s32 oldWallDYaw;

    resolve_and_return_wall_collisions(nextPos, 30.0f, 24.0f, &lowerWall);
    resolve_and_return_wall_collisions(nextPos, 60.0f, 50.0f, &upperWall);

    f32 floorHeight = find_floor(nextPos[0], nextPos[1], nextPos[2], &floor);
    f32 ceilHeight = find_mario_ceil(nextPos, floorHeight, &ceil);

    f32 waterLevel = find_water_level(nextPos[0], nextPos[2]);

    if (floor == NULL) {
        return GROUND_STEP_HIT_WALL_STOP_QSTEPS;
    }

    if ((m->action & ACT_FLAG_RIDING_SHELL) && floorHeight < waterLevel) {
        floorHeight = waterLevel;
        floor = &gWaterSurfacePseudoFloor;
        floor->originOffset = -floorHeight;
    }

    if (nextPos[1] > floorHeight + 100.0f) {
        if (nextPos[1] + 160.0f >= ceilHeight) {
            return GROUND_STEP_HIT_WALL_STOP_QSTEPS;
        }

        vec3f_copy(m->pos, nextPos);
        set_mario_floor(m, floor, floorHeight);
        return GROUND_STEP_LEFT_GROUND;
    }

    if (floorHeight + 160.0f >= ceilHeight) {
        return GROUND_STEP_HIT_WALL_STOP_QSTEPS;
    }

    vec3f_set(m->pos, nextPos[0], floorHeight, nextPos[2]);

    // H64 TODO: Add config opt & check if floor is slippery
    if (!SURFACE_IS_UNSAFE(floor->type)) {
        vec3f_copy(m->lastSafePos, m->pos);
    }

    set_mario_floor(m, floor, floorHeight);

    if (m->wall != NULL) {
        oldWallDYaw = abs_angle_diff(m->wallYaw, m->faceAngle[1]);
    } else {
        oldWallDYaw = 0x0;
    }
    for (i = 0; i < upperWall.numWalls; i++) {
        wallDYaw = abs_angle_diff(SURFACE_YAW(upperWall.walls[i]), m->faceAngle[1]);
        if (wallDYaw > oldWallDYaw) {
            oldWallDYaw = wallDYaw;
            set_mario_wall(m, upperWall.walls[i]);
        }

        if (wallDYaw >= DEGREES(60) && wallDYaw <= DEGREES(120)) {
            continue;
        }

        return GROUND_STEP_HIT_WALL_CONTINUE_QSTEPS;
    }

    return GROUND_STEP_NONE;
}

s32 perform_ground_step(struct MarioState *m) {
    s32 i;
    u32 stepResult;
    Vec3f intendedPos;
    const f32 numSteps = 4.0f;

    set_mario_wall(m, NULL);

    for (i = 0; i < 4; i++) {
        intendedPos[0] = m->pos[0] + m->floor->normal.y * (m->vel[0] / numSteps);
        intendedPos[2] = m->pos[2] + m->floor->normal.y * (m->vel[2] / numSteps);
        intendedPos[1] = m->pos[1];

        stepResult = perform_ground_quarter_step(m, intendedPos);
        if (stepResult == GROUND_STEP_LEFT_GROUND || stepResult == GROUND_STEP_HIT_WALL_STOP_QSTEPS) {
            break;
        }
    }

    m->terrainSoundAddend = mario_get_terrain_sound_addend(m);
    vec3f_copy(m->marioObj->header.gfx.pos, m->pos);
    vec3s_set(m->marioObj->header.gfx.angle, 0, m->faceAngle[1], 0);

    if (stepResult == GROUND_STEP_HIT_WALL_CONTINUE_QSTEPS) {
        stepResult = GROUND_STEP_HIT_WALL;
    }
    return stepResult;
}

// Horizontal dot product of surface normal
#define hdot_surf(surf, vec) (((surf)->normal.x * (vec)[0]) + ((surf)->normal.z * (vec)[2]))

struct Surface *check_ledge_grab(struct MarioState *m, struct Surface *prevWall, struct Surface *wall, Vec3f intendedPos, Vec3f nextPos, Vec3f ledgePos, struct Surface **ledgeFloor) {
    struct Surface *returnedWall = wall;
    if (m->vel[1] > 0.0f || wall == NULL) {
        return NULL;
    }

    if (prevWall == NULL) {
        prevWall = wall;
    }

    // Return the already grabbed wall if Mario is moving into it more than the newly tested wall.
    if (hdot_surf(prevWall, m->vel) < hdot_surf(wall, m->vel)) {
        returnedWall = prevWall;
    }

    // Only ledge grab if the wall displaced Mario in the opposite direction of his velocity.
    // hdot(displacement, vel).
    if (
        ((nextPos[0] - intendedPos[0]) * m->vel[0]) + ((nextPos[2] - intendedPos[2]) * m->vel[2]) > 0.0f
    ) {
        returnedWall = prevWall;
    }

    ledgePos[0] = nextPos[0] - (wall->normal.x * 60.0f);
    ledgePos[2] = nextPos[2] - (wall->normal.z * 60.0f);
    ledgePos[1] = find_floor(ledgePos[0], nextPos[1] + 160.0f, ledgePos[2], ledgeFloor);

    if (ledgeFloor == NULL
        || (*ledgeFloor) == NULL
        || ledgePos[1] < nextPos[1] + 100.0f
        || (*ledgeFloor)->normal.y < COS25 // H64 TODO: check if floor is actually slippery
        || SURFACE_IS_UNSAFE((*ledgeFloor)->type)) {
        return NULL;
    }

    return returnedWall;
}

#undef hdot_surf

s32 bonk_or_hit_lava_wall(struct MarioState *m, struct WallCollisionData *wallData) {
    s16 i;
    s16 wallDYaw;
    s32 oldWallDYaw;
    s32 result = AIR_STEP_NONE;

    if (m->wall != NULL) {
        oldWallDYaw = abs_angle_diff(m->wallYaw, m->faceAngle[1]);
    } else {
        oldWallDYaw = 0x0;
    }

    for (i = 0; i < wallData->numWalls; i++) {
        if (wallData->walls[i] != NULL) {
            if (wallData->walls[i]->type == SURFACE_BURNING) {
                set_mario_wall(m, wallData->walls[i]);
                return AIR_STEP_HIT_LAVA_WALL;
            }

            // Update wall reference (bonked wall) only if the new wall has a better facing angle
            wallDYaw = abs_angle_diff(SURFACE_YAW(wallData->walls[i]), m->faceAngle[1]);
            if (wallDYaw > oldWallDYaw) {
                oldWallDYaw = wallDYaw;
                set_mario_wall(m, wallData->walls[i]);

                if (wallDYaw > DEGREES(180 - WALL_KICK_DEGREES)) {
                    m->flags |= MARIO_AIR_HIT_WALL;
                    result = AIR_STEP_HIT_WALL;
                }
            }
        }
    }

    return result;
}

s32 perform_air_quarter_step(struct MarioState *m, Vec3f intendedPos, u32 stepArg) {
    s16 i;
    s32 stepResult = AIR_STEP_NONE;

    Vec3f nextPos, ledgePos;
    struct WallCollisionData upperWall, lowerWall;
    struct Surface *ceil, *floor, *ledgeFloor;
    struct Surface *grabbedWall = NULL;

    vec3f_copy(nextPos, intendedPos);

    resolve_and_return_wall_collisions(nextPos, 150.0f, 50.0f, &upperWall);
    resolve_and_return_wall_collisions(nextPos, 30.0f, 50.0f, &lowerWall);

    f32 floorHeight = find_floor(nextPos[0], nextPos[1], nextPos[2], &floor);
    f32 ceilHeight = find_mario_ceil(nextPos, floorHeight, &ceil);

    f32 waterLevel = find_water_level(nextPos[0], nextPos[2]);

    //! The water pseudo floor is not referenced when your intended qstep is
    // out of bounds, so it won't detect you as landing.

    if (floor == NULL) {
        if (nextPos[1] <= m->floorHeight) {
            m->pos[1] = m->floorHeight;
            return AIR_STEP_LANDED;
        }

        m->pos[1] = nextPos[1];
        return AIR_STEP_HIT_WALL;
    }

    if ((m->action & ACT_FLAG_RIDING_SHELL) && floorHeight < waterLevel) {
        floorHeight = waterLevel;
        floor = &gWaterSurfacePseudoFloor;
        floor->originOffset = -floorHeight;
    }

    //! This check uses f32, but findFloor uses short (overflow jumps)
    if (nextPos[1] <= floorHeight) {
        if (ceilHeight - floorHeight > 160.0f) {
            m->pos[0] = nextPos[0];
            m->pos[2] = nextPos[2];
            set_mario_floor(m, floor, floorHeight);
        }

        //! When ceilHeight - floorHeight <= 160, the step result says that
        // Mario landed, but his movement is cancelled and his referenced floor
        // isn't updated (pedro spots)
        m->pos[1] = floorHeight;
        return AIR_STEP_LANDED;
    }

    if (nextPos[1] + 160.0f > ceilHeight) {
        if (m->vel[1] >= 0.0f) {
            m->vel[1] = 0.0f;

#ifdef HANGING_FIX
            // Grab ceiling unless they just were grabbing a ceiling
            if (!(m->prevAction & ACT_FLAG_HANGING) && ceil != NULL && ceil->type == SURFACE_HANGABLE) {
#else
            if ((stepArg & AIR_STEP_CHECK_HANG) && ceil != NULL && ceil->type == SURFACE_HANGABLE) {
#endif
                return AIR_STEP_GRABBED_CEILING;
            }

            return AIR_STEP_NONE;
        }

        //! Potential subframe downwarp->upwarp?
        if (nextPos[1] <= m->floorHeight) {
            m->pos[1] = m->floorHeight;
            return AIR_STEP_LANDED;
        }

        m->pos[1] = nextPos[1];
        return AIR_STEP_HIT_CEILING;
    }

    //! When the wall is not completely vertical or there is a slight wall
    // misalignment, you can activate these conditions in unexpected situations

    if ((stepArg & AIR_STEP_CHECK_LEDGE_GRAB) && upperWall.numWalls == 0 && lowerWall.numWalls != 0) {
        for (i = 0; i < lowerWall.numWalls; i++) {
            grabbedWall = check_ledge_grab(m, grabbedWall, lowerWall.walls[i], intendedPos, nextPos, ledgePos, &ledgeFloor);
            if (grabbedWall != NULL) {
                stepResult = AIR_STEP_GRABBED_LEDGE;
            }
        }

        if (stepResult == AIR_STEP_GRABBED_LEDGE && grabbedWall != NULL && ledgeFloor != NULL && ledgePos != NULL) {
            vec3f_copy(m->pos, ledgePos);
            set_mario_floor(m, floor, ledgePos[1]);
            m->faceAngle[0] = 0x0;
            m->faceAngle[1] = SURFACE_YAW(grabbedWall) + 0x8000;
        } else {
            vec3f_copy(m->pos, nextPos);
            set_mario_floor(m, floor, floorHeight);
        }
        return stepResult;
    }

    vec3f_copy(m->pos, nextPos);
    set_mario_floor(m, floor, floorHeight);

    if (upperWall.numWalls > 0) {
        stepResult  = bonk_or_hit_lava_wall(m, &upperWall);
        if (stepResult != AIR_STEP_NONE) {
            return stepResult;
        }
    }

    return (lowerWall.numWalls > 0) ? bonk_or_hit_lava_wall(m, &lowerWall) : AIR_STEP_NONE;
}

void apply_twirl_gravity(struct MarioState *m) {
#ifdef Z_TWIRL
    f32 Zmodifier = m->input & INPUT_Z_DOWN ? 4.0f : 1.0f;
#endif
    f32 heaviness = 1.0f;

    if (m->angleVel[1] > 1024) {
        heaviness = 1024.0f / m->angleVel[1];
    }

#ifdef Z_TWIRL
    f32 terminalVelocity = -75.0f * heaviness * Zmodifier;
    m->vel[1] -= 4.0f * heaviness * Zmodifier;
#else
    f32 terminalVelocity = -75.0f * heaviness;

    m->vel[1] -= 4.0f * heaviness;
#endif
    if (m->vel[1] < terminalVelocity) {
        m->vel[1] = terminalVelocity;
    }
}

u32 should_strengthen_gravity_for_jump_ascent(struct MarioState *m) {
    if (!(m->flags & MARIO_JUMPING)) {
        return FALSE;
    }

    if (m->action & (ACT_FLAG_INTANGIBLE | ACT_FLAG_INVULNERABLE)) {
        return FALSE;
    }

    if (!(m->input & INPUT_A_DOWN) && m->vel[1] > 20.0f) {
        return (m->action & ACT_FLAG_CONTROL_JUMP_HEIGHT) != 0;
    }

    return FALSE;
}

void apply_gravity(struct MarioState *m) {
    if (m->action == ACT_TWIRLING && m->vel[1] < 0.0f) {
        apply_twirl_gravity(m);
    } else if (m->action == ACT_SHOT_FROM_CANNON) {
        m->vel[1] -= 1.0f;
        if (m->vel[1] < -75.0f) {
            m->vel[1] = -75.0f;
        }
    } else if (m->action == ACT_LONG_JUMP || m->action == ACT_SLIDE_KICK
               || m->action == ACT_BBH_ENTER_SPIN) {
        m->vel[1] -= 2.0f;
        if (m->vel[1] < -75.0f) {
            m->vel[1] = -75.0f;
        }
    } else if (m->action == ACT_LAVA_BOOST || m->action == ACT_FALL_AFTER_STAR_GRAB) {
        m->vel[1] -= 3.2f;
        if (m->vel[1] < -65.0f) {
            m->vel[1] = -65.0f;
        }
    } else if (m->action == ACT_GETTING_BLOWN) {
        m->vel[1] -= m->windGravity;
        if (m->vel[1] < -75.0f) {
            m->vel[1] = -75.0f;
        }
    } else if (should_strengthen_gravity_for_jump_ascent(m)) {
        m->vel[1] /= 4.0f;
    } else if (m->action & ACT_FLAG_METAL_WATER) {
        m->vel[1] -= 1.6f;
        if (m->vel[1] < -16.0f) {
            m->vel[1] = -16.0f;
        }
    } else if ((m->flags & MARIO_WING_CAP) && m->vel[1] < 0.0f && (m->input & INPUT_A_DOWN)) {
        m->marioBodyState->wingFlutter = TRUE;

        m->vel[1] -= 2.0f;
        if (m->vel[1] < -37.5f) {
            if ((m->vel[1] += 4.0f) > -37.5f) {
                m->vel[1] = -37.5f;
            }
        }
    } else {
        m->vel[1] -= 4.0f;
        if (m->vel[1] < -75.0f) {
            m->vel[1] = -75.0f;
        }
    }
}

void apply_vertical_wind(struct MarioState *m) {
    f32 maxVelY;

    if (m->action != ACT_GROUND_POUND) {
        f32 offsetY = m->pos[1] - -1500.0f;

        if (m->floor->type == SURFACE_VERTICAL_WIND && -3000.0f < offsetY && offsetY < 2000.0f) {
            if (offsetY >= 0.0f) {
                maxVelY = 10000.0f / (offsetY + 200.0f);
            } else {
                maxVelY = 50.0f;
            }

            if (m->vel[1] < maxVelY) {
                if ((m->vel[1] += maxVelY / 8.0f) > maxVelY) {
                    m->vel[1] = maxVelY;
                }
            }
        }
    }
}

s32 perform_air_step(struct MarioState *m, u32 stepArg) {
    Vec3f intendedPos;
    const f32 numSteps = 4.0f;
    s32 i;
    s32 quarterStepResult;
    s32 stepResult = AIR_STEP_NONE;

    set_mario_wall(m, NULL);

    for (i = 0; i < 4; i++) {
        intendedPos[0] = m->pos[0] + m->vel[0] / numSteps;
        intendedPos[1] = m->pos[1] + m->vel[1] / numSteps;
        intendedPos[2] = m->pos[2] + m->vel[2] / numSteps;

        quarterStepResult = perform_air_quarter_step(m, intendedPos, stepArg);

        if (quarterStepResult != AIR_STEP_NONE) {
            stepResult = quarterStepResult;
        }

        if (quarterStepResult == AIR_STEP_LANDED || quarterStepResult == AIR_STEP_GRABBED_LEDGE
            || quarterStepResult == AIR_STEP_GRABBED_CEILING
            || quarterStepResult == AIR_STEP_HIT_LAVA_WALL) {
            break;
        }
    }

    if (m->vel[1] >= 0.0f) {
        m->peakHeight = m->pos[1];
    }

    m->terrainSoundAddend = mario_get_terrain_sound_addend(m);

    if (m->action != ACT_FLYING) {
        apply_gravity(m);
    }
    apply_vertical_wind(m);

    vec3f_copy(m->marioObj->header.gfx.pos, m->pos);
    vec3s_set(m->marioObj->header.gfx.angle, 0, m->faceAngle[1], 0);

    return stepResult;
}

// They had these functions the whole time and never used them? Lol

void set_vel_from_pitch_and_yaw(struct MarioState *m) {
    m->vel[0] = m->forwardVel * coss(m->faceAngle[0]) * sins(m->faceAngle[1]);
    m->vel[1] = m->forwardVel * sins(m->faceAngle[0]);
    m->vel[2] = m->forwardVel * coss(m->faceAngle[0]) * coss(m->faceAngle[1]);
}

void set_vel_from_yaw(struct MarioState *m) {
    m->vel[0] = m->slideVelX = m->forwardVel * sins(m->faceAngle[1]);
    m->vel[1] = 0.0f;
    m->vel[2] = m->slideVelZ = m->forwardVel * coss(m->faceAngle[1]);
}
