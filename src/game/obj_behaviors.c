#include <PR/ultratypes.h>

#include "sm64.h"
#include "area.h"
#include "audio/external.h"
#include "behavior_actions.h"
#include "behavior_data.h"
#include "camera.h"
#include "course_table.h"
#include "dialog_ids.h"
#include "engine/behavior_script.h"
#include "engine/math_util.h"
#include "engine/surface_collision.h"
#include "envfx_bubbles.h"
#include "game_init.h"
#include "ingame_menu.h"
#include "interaction.h"
#include "level_misc_macros.h"
#include "level_table.h"
#include "level_update.h"
#include "levels/bob/header.h"
#include "levels/ttm/header.h"
#include "mario.h"
#include "mario_actions_cutscene.h"
#include "mario_misc.h"
#include "memory.h"
#include "obj_behaviors.h"
#include "object_helpers.h"
#include "object_list_processor.h"
#include "rendering_graph_node.h"
#include "save_file.h"
#include "spawn_object.h"
#include "spawn_sound.h"
#include "rumble_init.h"
#include "puppylights.h"

/**
 * @file obj_behaviors.c
 * This file contains a portion of the obj behaviors and many helper functions for those
 * specific behaviors. Few functions besides the bhv_ functions are used elsewhere in the repo.
 */

/**
 * Current object floor as defined in object_step.
 */
struct Surface *sObjFloor;

/**
 * Set to false when an object close to the floor should not be oriented in reference
 * to it. Happens with boulder, falling pillar, and the rolling snowman body.
 */
static s8 sOrientObjWithFloor = TRUE;

/**
 * Keeps track of Mario's previous non-zero room.
 * Helps keep track of room when Mario is over an object.
 */
s16 sPrevCheckMarioRoom = 0;

extern void *ccm_seg7_trajectory_snowman;
extern void *inside_castle_seg7_trajectory_mips;

Vec3f sObjSavedPos;

void wiggler_jumped_on_attack_handler(void);
void huge_goomba_weakly_attacked(void);

/**
 * An unused geo function. Bears strong similarity to geo_bits_bowser_coloring, and relates something
 * of the opacity of an object to something else. Perhaps like, giving a parent object the same
 * opacity?
 */
Gfx UNUSED *geo_obj_transparency_something(s32 callContext, struct GraphNode *node, UNUSED Mat4 *mtx) {
    Gfx *gfxHead = NULL;
    Gfx *gfx;

    if (callContext == GEO_CONTEXT_RENDER) {
        struct Object *heldObject = (struct Object *) gCurGraphNodeObject;
        struct Object *obj = (struct Object *) node;


        if (gCurGraphNodeHeldObject != NULL) {
            heldObject = gCurGraphNodeHeldObject->objNode;
        }

        gfxHead = alloc_display_list(3 * sizeof(Gfx));
        gfx = gfxHead;
        SET_GRAPH_NODE_LAYER(obj->header.gfx.node.flags, LAYER_TRANSPARENT);

        gDPSetEnvColor(gfx++, 255, 255, 255, heldObject->oOpacity);

        gSPEndDisplayList(gfx);
    }

    return gfxHead;
}

/**
 * Backwards compatibility, used to be a duplicate function
 */
#define absf_2 absf

/**
 * Turns an object away from floors/walls that it runs into.
 */
void turn_obj_away_from_surface(f32 velX, f32 velZ, f32 nX, UNUSED f32 nY, f32 nZ, f32 *objYawX,
                            f32 *objYawZ) {
    *objYawX = (nZ * nZ - nX * nX) * velX / (nX * nX + nZ * nZ)
               - 2 * velZ * (nX * nZ) / (nX * nX + nZ * nZ);

    *objYawZ = (nX * nX - nZ * nZ) * velZ / (nX * nX + nZ * nZ)
               - 2 * velX * (nX * nZ) / (nX * nX + nZ * nZ);
}

/**
 * Finds any wall collisions, applies them, and turns away from the surface.
 */
s8 obj_find_wall(f32 objNewX, f32 objY, f32 objNewZ, f32 objVelX, f32 objVelZ) {
    struct WallCollisionData hitbox;
    f32 wall_nX, wall_nY, wall_nZ, objVelXCopy, objVelZCopy, objYawX, objYawZ;

    hitbox.x = objNewX;
    hitbox.y = objY;
    hitbox.z = objNewZ;
    hitbox.offsetY = o->hitboxHeight / 2;
    hitbox.radius = o->hitboxRadius;

    if (find_wall_collisions(&hitbox) != 0) {
        o->oPosX = hitbox.x;
        o->oPosY = hitbox.y;
        o->oPosZ = hitbox.z;

        wall_nX = hitbox.walls[0]->normal.x;
        wall_nY = hitbox.walls[0]->normal.y;
        wall_nZ = hitbox.walls[0]->normal.z;

        objVelXCopy = objVelX;
        objVelZCopy = objVelZ;

        // Turns away from the first wall only.
        turn_obj_away_from_surface(objVelXCopy, objVelZCopy, wall_nX, wall_nY, wall_nZ, &objYawX, &objYawZ);

        o->oMoveAngleYaw = atan2s(objYawZ, objYawX);
        return FALSE;
    }

    return TRUE;
}

/**
 * Turns an object away from steep floors, similarly to walls.
 */
s8 turn_obj_away_from_steep_floor(struct Surface *objFloor, f32 floorY, f32 objVelX, f32 objVelZ) {
    f32 floor_nX, floor_nY, floor_nZ, objVelXCopy, objVelZCopy, objYawX, objYawZ;

    if (objFloor == NULL) {
        o->oMoveAngleYaw += 0x8000;
        return FALSE;
    }

    floor_nX = objFloor->normal.x;
    floor_nY = objFloor->normal.y;
    floor_nZ = objFloor->normal.z;

    // If the floor is steep and we are below it (i.e. walking into it), turn away from the floor.
    if (floor_nY < 0.5f && floorY > o->oPosY) {
        objVelXCopy = objVelX;
        objVelZCopy = objVelZ;
        turn_obj_away_from_surface(objVelXCopy, objVelZCopy, floor_nX, floor_nY, floor_nZ, &objYawX, &objYawZ);
        o->oMoveAngleYaw = atan2s(objYawZ, objYawX);
        return FALSE;
    }

    return TRUE;
}

/**
 * Orients an object with the given normals, typically the surface under the object.
 */
void obj_orient_graph(struct Object *obj, f32 normalX, f32 normalY, f32 normalZ) {
    Vec3f objVisualPosition, surfaceNormals;

    Mat4 *throwMatrix;

    // Passes on orienting certain objects that shouldn't be oriented, like boulders.
    if (!sOrientObjWithFloor) {
        return;
    }

    // Passes on orienting billboard objects, i.e. coins, trees, etc.
    if (obj->header.gfx.node.flags & GRAPH_RENDER_BILLBOARD) {
        return;
    }

    throwMatrix = alloc_display_list(sizeof(*throwMatrix));
    // If out of memory, fail to try orienting the object.
    if (throwMatrix == NULL) {
        return;
    }

    vec3f_copy_y_off(objVisualPosition, &obj->oPosVec, obj->oGraphYOffset);
    vec3f_set(surfaceNormals, normalX, normalY, normalZ);

    mtxf_align_terrain_normal(*throwMatrix, surfaceNormals, objVisualPosition, obj->oFaceAngleYaw);
    obj->header.gfx.throwMatrix = throwMatrix;
}

/**
 * Determines an object's forward speed multiplier.
 */
void calc_obj_friction(f32 *objFriction, f32 floor_nY) {
    if (floor_nY < 0.2f && o->oFriction < 0.9999f) {
        *objFriction = 0;
    } else {
        *objFriction = o->oFriction;
    }
}

/**
 * Updates an objects speed for gravity and updates Y position.
 */
void calc_new_obj_vel_and_pos_y(struct Surface *objFloor, f32 objFloorY, f32 objVelX, f32 objVelZ) {
    f32 floor_nX = objFloor->normal.x;
    f32 floor_nY = objFloor->normal.y;
    f32 floor_nZ = objFloor->normal.z;
    f32 objFriction;

    // Caps vertical speed with a "terminal velocity".
    o->oVelY -= o->oGravity;
    if (o->oVelY > 75.0) {
        o->oVelY = 75.0;
    }
    if (o->oVelY < -75.0) {
        o->oVelY = -75.0;
    }

    o->oPosY += o->oVelY;

    // Snap the object up to the floor.
    if (o->oPosY < objFloorY) {
        o->oPosY = objFloorY;

        // Bounces an object if the ground is hit fast enough.
        if (o->oVelY < -17.5f) {
            o->oVelY = -(o->oVelY / 2);
        } else {
            o->oVelY = 0;
        }
    }

    //! (Obj Position Crash) If you got an object with height past 2^31, the game would crash.
    if ((s32) o->oPosY >= (s32) objFloorY && (s32) o->oPosY < (s32) objFloorY + 37) {
        obj_orient_graph(o, floor_nX, floor_nY, floor_nZ);

        // Adds horizontal component of gravity for horizontal speed.
        f32 nxz = sqr(floor_nX) + sqr(floor_nZ);
        f32 vel = ((nxz) / (nxz + sqr(floor_nY))) * o->oGravity * 2;
        objVelX += floor_nX * vel;
        objVelZ += floor_nZ * vel;

        if (objVelX < NEAR_ZERO && objVelX > -NEAR_ZERO) objVelX = 0;
        if (objVelZ < NEAR_ZERO && objVelZ > -NEAR_ZERO) objVelZ = 0;

        if (objVelX != 0 || objVelZ != 0) {
            o->oMoveAngleYaw = atan2s(objVelZ, objVelX);
        }

        calc_obj_friction(&objFriction, floor_nY);
        o->oForwardVel = sqrtf(sqr(objVelX) + sqr(objVelZ)) * objFriction;
    }
}

void calc_new_obj_vel_and_pos_y_underwater(struct Surface *objFloor, f32 floorY, f32 objVelX, f32 objVelZ, f32 waterY) {
    f32 floor_nX = objFloor->normal.x;
    f32 floor_nY = objFloor->normal.y;
    f32 floor_nZ = objFloor->normal.z;

    f32 netYAccel = (1.0f - o->oBuoyancy) * (-1.0f * o->oGravity);
    o->oVelY -= netYAccel;

    // Caps vertical speed with a "terminal velocity".
    if (o->oVelY > 75.0f) {
        o->oVelY = 75.0f;
    }
    if (o->oVelY < -75.0f) {
        o->oVelY = -75.0f;
    }

    o->oPosY += o->oVelY;

    // Snap the object up to the floor.
    if (o->oPosY < floorY) {
        o->oPosY = floorY;

        // Bounces an object if the ground is hit fast enough.
        if (o->oVelY < -17.5f) {
            o->oVelY = -(o->oVelY / 2);
        } else {
            o->oVelY = 0;
        }
    }

    // If moving fast near the surface of the water, flip vertical speed? To emulate skipping?
    if (o->oForwardVel > 12.5f && (waterY + 30.0f) > o->oPosY && (waterY - 30.0f) < o->oPosY) {
        o->oVelY = -o->oVelY;
    }

    if ((s32) o->oPosY >= (s32) floorY && (s32) o->oPosY < (s32) floorY + 37) {
        obj_orient_graph(o, floor_nX, floor_nY, floor_nZ);

        // Adds horizontal component of gravity for horizontal speed.
        f32 nxz = sqr(floor_nX) + sqr(floor_nZ);
        f32 velm = (nxz / (nxz + sqr(floor_nY))) * netYAccel * 2;
        objVelX += floor_nX * velm;
        objVelZ += floor_nZ * velm;
    }

    if (objVelX < NEAR_ZERO && objVelX > -NEAR_ZERO) objVelX = 0;
    if (objVelZ < NEAR_ZERO && objVelZ > -NEAR_ZERO) objVelZ = 0;

    if (o->oVelY < NEAR_ZERO && o->oVelY > -NEAR_ZERO) {
        o->oVelY = 0;
    }

    if (objVelX != 0 || objVelZ != 0) {
        o->oMoveAngleYaw = atan2s(objVelZ, objVelX);
    }

    // Decreases both vertical velocity and forward velocity. Likely so that skips above
    // don't loop infinitely.
    o->oForwardVel = sqrtf(sqr(objVelX) + sqr(objVelZ)) * 0.8f;
    o->oVelY *= 0.8f;
}

/**
 * Updates an objects position from oForwardVel and oMoveAngleYaw.
 */
void obj_update_pos_vel_xz(void) {
    o->oPosX += o->oForwardVel * sins(o->oMoveAngleYaw);
    o->oPosZ += o->oForwardVel * coss(o->oMoveAngleYaw);
}

/**
 * Generates splashes if at surface of water, entering water, or bubbles
 * if underwater.
 */
void obj_splash(s32 waterY, s32 objY) {
    u32 globalTimer = gGlobalTimer;

    // Spawns waves if near surface of water and plays a noise if entering.
    if ((f32)(waterY + 30) > o->oPosY && o->oPosY > (f32)(waterY - 30)) {
        spawn_object(o, MODEL_IDLE_WATER_WAVE, bhvObjectWaterWave);

        if (o->oVelY < -20.0f) {
            cur_obj_play_sound_2(SOUND_OBJ_DIVING_INTO_WATER);
        }
    }

    // Spawns bubbles if underwater.
    if ((objY + 50) < waterY && !(globalTimer & 31)) {
        spawn_object(o, MODEL_WHITE_PARTICLE_SMALL, bhvObjectBubble);
    }
}

/**
 * Generic object move function. Handles walls, water, floors, and gravity.
 * Returns flags for certain interactions.
 */
s16 object_step(void) {
    f32 objX = o->oPosX;
    f32 objY = o->oPosY;
    f32 objZ = o->oPosZ;

    f32 floorY;
    f32 waterY = FLOOR_LOWER_LIMIT_MISC;

    f32 objVelX = o->oForwardVel * sins(o->oMoveAngleYaw);
    f32 objVelZ = o->oForwardVel * coss(o->oMoveAngleYaw);

    s16 collisionFlags = 0;

    // Find any wall collisions, receive the push, and set the flag.
    if (obj_find_wall(objX + objVelX, objY, objZ + objVelZ, objVelX, objVelZ) == 0) {
        collisionFlags += OBJ_COL_FLAG_HIT_WALL;
    }

    floorY = find_floor(objX + objVelX, objY, objZ + objVelZ, &sObjFloor);

    o->oFloor       = sObjFloor;
    o->oFloorHeight = floorY;

    if (turn_obj_away_from_steep_floor(sObjFloor, floorY, objVelX, objVelZ) == 1) {
        waterY = find_water_level(objX + objVelX, objZ + objVelZ);
        if (waterY > objY) {
            calc_new_obj_vel_and_pos_y_underwater(sObjFloor, floorY, objVelX, objVelZ, waterY);
            collisionFlags += OBJ_COL_FLAG_UNDERWATER;
        } else {
            calc_new_obj_vel_and_pos_y(sObjFloor, floorY, objVelX, objVelZ);
        }
    } else {
        // Treat any awkward floors similar to a wall.
        collisionFlags +=
            ((collisionFlags & OBJ_COL_FLAG_HIT_WALL) ^ OBJ_COL_FLAG_HIT_WALL);
    }

    obj_update_pos_vel_xz();
    if ((s32) o->oPosY == (s32) floorY) {
        collisionFlags += OBJ_COL_FLAG_GROUNDED;
    }

    if ((s32) o->oVelY == 0) {
        collisionFlags += OBJ_COL_FLAG_NO_Y_VEL;
    }

    // Generate a splash if in water.
    obj_splash((s32) waterY, (s32) o->oPosY);
    return collisionFlags;
}

/**
 * Takes an object step but does not orient with the object's floor.
 * Used for boulders, falling pillars, and the rolling snowman body.
 */
s16 object_step_without_floor_orient(void) {
    sOrientObjWithFloor = FALSE;
    s16 collisionFlags = object_step();
    sOrientObjWithFloor = TRUE;

    return collisionFlags;
}

/**
 * Uses an object's forward velocity and yaw to move its X, Y, and Z positions.
 * This does accept an object as an argument, though it is always called with `o`.
 */
void obj_move_xyz_using_fvel_and_yaw(struct Object *obj) {
    obj->oVelX = obj->oForwardVel * sins(obj->oMoveAngleYaw);
    obj->oVelZ = obj->oForwardVel * coss(obj->oMoveAngleYaw);

    vec3f_add(&obj->oPosVec, &obj->oVelVec);
}

/**
 * Checks if a point is within distance from Mario's graphical position. Test is exclusive.
 */
s32 is_point_within_radius_of_mario(f32 x, f32 y, f32 z, s32 dist) {
    f32 dx = x - gMarioObject->header.gfx.pos[0];
    f32 dy = y - gMarioObject->header.gfx.pos[1];
    f32 dz = z - gMarioObject->header.gfx.pos[2];

    return sqr(dx) + sqr(dy) + sqr(dz) < (f32)sqr(dist);
}

/**
 * Checks whether a point is within distance of a given point. Test is exclusive.
 */
s32 is_point_close_to_object(struct Object *obj, f32 x, f32 y, f32 z, s32 dist) {
    f32 dx = x - obj->oPosX;
    f32 dy = y - obj->oPosY;
    f32 dz = z - obj->oPosZ;

    return sqr(dx) + sqr(dy) + sqr(dz) < (f32)sqr(dist);
}

/**
 * Sets an object as visible if within a certain distance of Mario's graphical position.
 */
void set_object_visibility(struct Object *obj, s32 dist) {
    COND_BIT(
        !is_point_within_radius_of_mario(obj->oPosX, obj->oPosY, obj->oPosZ, dist),
        obj->header.gfx.node.flags,
        GRAPH_RENDER_INVISIBLE
    );
}

/**
 * Turns an object towards home if Mario is not near to it.
 */
s32 obj_return_home_if_safe(struct Object *obj, f32 homeX, f32 y, f32 homeZ, s32 dist) {
    f32 homeDistX = homeX - obj->oPosX;
    f32 homeDistZ = homeZ - obj->oPosZ;
    s16 angleTowardsHome = atan2s(homeDistZ, homeDistX);

    if (is_point_within_radius_of_mario(homeX, y, homeZ, dist)) {
        return TRUE;
    } else {
        obj->oMoveAngleYaw = approach_s16_symmetric(obj->oMoveAngleYaw, angleTowardsHome, 320);
    }

    return FALSE;
}

/**
 * Randomly displaces an objects home if RNG says to, and turns the object towards its home.
 */
void obj_return_and_displace_home(struct Object *obj, f32 homeX, UNUSED f32 homeY, f32 homeZ, s32 baseDisp) {
    s16 angleToNewHome;
    f32 homeDistX, homeDistZ;

    if ((s32)(random_float() * 50.0f) == 0) {
        obj->oHomeX = (f32)(baseDisp * 2) * random_float() - (f32) baseDisp + homeX;
        obj->oHomeZ = (f32)(baseDisp * 2) * random_float() - (f32) baseDisp + homeZ;
    }

    homeDistX = obj->oHomeX - obj->oPosX;
    homeDistZ = obj->oHomeZ - obj->oPosZ;
    angleToNewHome = atan2s(homeDistZ, homeDistX);
    obj->oMoveAngleYaw = approach_s16_symmetric(obj->oMoveAngleYaw, angleToNewHome, 320);
}

/**
 * A series of checks using sin and cos to see if a given angle is facing in the same direction
 * of a given angle, within a certain range.
 */
s32 obj_check_if_facing_toward_angle(u32 base, u32 goal, s16 range) {
    s16 dAngle = (u16) goal - (u16) base;

    if (((f32) sins(-range) < (f32) sins(dAngle)) && ((f32) sins(dAngle) < (f32) sins(range))
        && (coss(dAngle) > 0)) {
        return TRUE;
    }

    return FALSE;
}

/**
 * Finds any wall collisions and returns what the displacement vector would be.
 */
s32 obj_find_wall_displacement(Vec3f dist, f32 x, f32 y, f32 z, f32 radius) {
    struct WallCollisionData hitbox;
    hitbox.x = x;
    hitbox.y = y;
    hitbox.z = z;
    hitbox.offsetY = 10.0f;
    hitbox.radius = radius;

    if (find_wall_collisions(&hitbox) != 0) {
        dist[0] = hitbox.x - x;
        dist[1] = hitbox.y - y;
        dist[2] = hitbox.z - z;
        return TRUE;
    } else {
        return FALSE;
    }
}

/**
 * Spawns a number of coins at the location of an object
 * with a random forward velocity, y velocity, and direction.
 */
void obj_spawn_yellow_coins(struct Object *obj, s8 nCoins) {
    struct Object *coin;
    s8 count;

    for (count = 0; count < nCoins; count++) {
        coin = spawn_object(obj, MODEL_YELLOW_COIN, bhvMovingYellowCoin);
        coin->oForwardVel = random_float() * 20;
        coin->oVelY = random_float() * 40 + 20;
        coin->oMoveAngleYaw = random_u16();
    }
}

/**
 * Controls whether certain objects should flicker/when to despawn.
 */
s32 obj_flicker_and_disappear(struct Object *obj, s16 lifeSpan) {
    if (obj->oTimer < lifeSpan) {
        return FALSE;
    }

    if (obj->oTimer < lifeSpan + 40) {
        COND_BIT((obj->oTimer & 0x1), obj->header.gfx.node.flags, GRAPH_RENDER_INVISIBLE);
    } else {
        obj->activeFlags = ACTIVE_FLAG_DEACTIVATED;
        return TRUE;
    }

    return FALSE;
}

/**
 * Checks if a given room is Mario's current room, even if on an object.
 */
s32 current_mario_room_check(RoomData room) {
    s32 result;

    // Since object surfaces have room 0, this tests if the surface is an
    // object first and uses the last room if so.
    if (gMarioCurrentRoom == 0) {
        return room == sPrevCheckMarioRoom;
    } else {
        result = room == gMarioCurrentRoom;

        sPrevCheckMarioRoom = gMarioCurrentRoom;
    }

    return result;
}

/**
 * Triggers dialog when Mario is facing an object and controls it while in the dialog.
 */
s32 trigger_obj_dialog_when_facing(s32 *inDialog, s16 dialogID, f32 dist, s32 actionArg) {
    if ((is_point_within_radius_of_mario(o->oPosX, o->oPosY, o->oPosZ, (s32) dist)
         && obj_check_if_facing_toward_angle(o->oFaceAngleYaw, gMarioObject->header.gfx.angle[1] + 0x8000, 0x1000)
         && obj_check_if_facing_toward_angle(o->oMoveAngleYaw, o->oAngleToMario, 0x1000))
        || (*inDialog == TRUE)) {
        *inDialog = TRUE;

        if (set_mario_npc_dialog(actionArg) == MARIO_DIALOG_STATUS_SPEAK) { // If Mario is speaking.
            s16 dialogResponse = cutscene_object_with_dialog(CUTSCENE_DIALOG, o, dialogID);
            if (dialogResponse != DIALOG_RESPONSE_NONE) {
                set_mario_npc_dialog(MARIO_DIALOG_STOP);
                *inDialog = FALSE;
                return dialogResponse;
            }
            return DIALOG_RESPONSE_NONE;
        }
    }

    return DIALOG_RESPONSE_NONE;
}

/**
 *Checks if a floor is one that should cause an object to "die".
 */
void obj_check_floor_death(s16 collisionFlags, struct Surface *floor) {
    if (floor == NULL) {
        return;
    }

    if ((collisionFlags & OBJ_COL_FLAG_GROUNDED) == OBJ_COL_FLAG_GROUNDED) {
        switch (floor->type) {
            case SURFACE_BURNING:
                o->oAction = OBJ_ACT_LAVA_DEATH;
                break;
            case SURFACE_VERTICAL_WIND:
            case SURFACE_DEATH_PLANE:
                o->oAction = OBJ_ACT_DEATH_PLANE_DEATH;
                break;
            default:
                break;
        }
    }
}

/**
 * Controls an object dying in lava by creating smoke, sinking the object, playing
 * audio, and eventually despawning it. Returns TRUE when the obj is dead.
 */
s32 obj_lava_death(void) {
    struct Object *deathSmoke;

    if (o->oTimer > 30) {
        o->activeFlags = ACTIVE_FLAG_DEACTIVATED;
        return TRUE;
    } else {
        // Sinking effect
        o->oPosY -= 10.0f;
    }

    if ((o->oTimer % 8) == 0) {
        cur_obj_play_sound_2(SOUND_OBJ_BULLY_EXPLODE_LAVA);
        deathSmoke = spawn_object(o, MODEL_SMOKE, bhvBobombBullyDeathSmoke);
        deathSmoke->oPosX += random_float() * 20.0f;
        deathSmoke->oPosY += random_float() * 20.0f;
        deathSmoke->oPosZ += random_float() * 20.0f;
        deathSmoke->oForwardVel = random_float() * 10.0f;
    }

    return FALSE;
}

/**
 * Spawns an orange number object relatively, such as those that count up for secrets.
 */
void spawn_orange_number(s8 behParam, s16 relX, s16 relY, s16 relZ) {
#ifdef DIALOG_INDICATOR
    if (behParam > ORANGE_NUMBER_F) return;
#else
    if (behParam > ORANGE_NUMBER_9) return;
#endif

    struct Object *orangeNumber = spawn_object_relative(behParam, relX, relY, relZ, o, MODEL_NUMBER, bhvOrangeNumber);
    orangeNumber->oPosY += 25.0f;
    orangeNumber->oOrangeNumberOffset = relX;
    orangeNumber->oHomeX = o->oPosX;
    orangeNumber->oHomeZ = o->oPosZ;
}

/**
 * Unused variables for debug_sequence_tracker.
 */
s8 sDebugSequenceTracker = 0;
s8 sDebugTimer = 0;

/**
 * Unused presumably debug function that tracks for a sequence of inputs.
 */
UNUSED s32 debug_sequence_tracker(s16 debugInputSequence[]) {
    // If end of sequence reached, return true.
    if (debugInputSequence[sDebugSequenceTracker] == 0) {
        sDebugSequenceTracker = 0;
        return TRUE;
    }

    // If the button pressed is next in sequence, reset timer and progress to next value.
    if (debugInputSequence[sDebugSequenceTracker] & gPlayer1Controller->buttonPressed) {
        sDebugSequenceTracker++;
        sDebugTimer = 0;
    // If wrong input or timer reaches 10, reset sequence progress.
    } else if (sDebugTimer == 10 || gPlayer1Controller->buttonPressed != 0) {
        sDebugSequenceTracker = 0;
        sDebugTimer = 0;
        return FALSE;
    }
    sDebugTimer++;

    return FALSE;
}

s32 obj_is_rendering_enabled(void) {
    if (o->header.gfx.node.flags & GRAPH_RENDER_ACTIVE) {
        return TRUE;
    } else {
        return FALSE;
    }
}

s16 obj_get_pitch_from_vel(void) {
    return -atan2s(o->oForwardVel, o->oVelY);
}

/**
 * Show dialog proposing a race.
 * If the player accepts the race, then leave time stop enabled and Mario in the
 * text action so that the racing object can wait before starting the race.
 * If the player declines the race, then disable time stop and allow Mario to
 * move again.
 */
s32 obj_update_race_proposition_dialog(s16 dialogID) {
    s32 dialogResponse = cur_obj_update_dialog_with_cutscene(MARIO_DIALOG_LOOK_UP,
        (DIALOG_FLAG_TURN_TO_MARIO | DIALOG_FLAG_TIME_STOP_ENABLED), CUTSCENE_RACE_DIALOG, dialogID);

    if (dialogResponse == DIALOG_RESPONSE_NO) {
        set_mario_npc_dialog(MARIO_DIALOG_STOP);
        disable_time_stop_including_mario();
    }

    return dialogResponse;
}

void obj_set_dist_from_home(f32 distFromHome) {
    o->oPosX = o->oHomeX + distFromHome * coss(o->oMoveAngleYaw);
    o->oPosZ = o->oHomeZ + distFromHome * sins(o->oMoveAngleYaw);
}

s32 obj_is_near_to_and_facing_mario(f32 maxDist, s16 maxAngleDiff) {
    if (o->oDistanceToMario < maxDist
        && abs_angle_diff(o->oMoveAngleYaw, o->oAngleToMario) < maxAngleDiff) {
        return TRUE;
    }
    return FALSE;
}

void obj_perform_position_op(s32 op) {
    switch (op) {
        case POS_OP_SAVE_POSITION:    vec3f_copy(sObjSavedPos, &o->oPosVec); break;
        case POS_OP_COMPUTE_VELOCITY: vec3f_diff(&o->oVelVec, &o->oPosVec, sObjSavedPos); break;
        case POS_OP_RESTORE_POSITION: vec3f_copy(&o->oPosVec, sObjSavedPos); break;
    }
}

void cur_obj_spin_all_dimensions(f32 pitchSpeed, f32 rollSpeed) {
    f32 pitch, yaw, roll;
    f32 c, s;
    f32 px, pz, ny, nz, nx;

    if (o->oForwardVel == 0.0f) {
        roll = yaw = pitch = 0.0f;

        if (o->oMoveFlags & OBJ_MOVE_IN_AIR) {
            yaw = 50.0f;
        } else {
            if (o->oFaceAnglePitch < 0x0) {
                pitch = -pitchSpeed;
            } else if (o->oFaceAnglePitch > 0x0) {
                pitch =  pitchSpeed;
            }

            if (o->oFaceAngleRoll < 0x0) {
                roll = -rollSpeed;
            } else if (o->oFaceAngleRoll > 0x0) {
                roll = rollSpeed;
            }
        }

        c = coss(o->oFaceAnglePitch);
        s = sins(o->oFaceAnglePitch);
        nz = pitch * c + yaw * s;
        ny = yaw * c - pitch * s;

        c = coss(o->oFaceAngleRoll);
        s = sins(o->oFaceAngleRoll);
        nx = roll * c + ny * s;
        ny = ny * c - roll * s;

        c = coss(o->oFaceAngleYaw);
        s = sins(o->oFaceAngleYaw);
        px = nx * c - nz * s;
        nz = nz * c + nx * s;

        nx = roll * c - pitch * s;
        pz = pitch * c + roll * s;

        o->oPosX = o->oHomeX - nx + px;
        o->oGraphYOffset = yaw - ny;
        o->oPosZ = o->oHomeZ + pz - nz;
    }
}

void obj_rotate_yaw_and_bounce_off_walls(s16 targetYaw, s16 turnAmount) {
    if (o->oMoveFlags & OBJ_MOVE_HIT_WALL) {
        targetYaw = cur_obj_reflect_move_angle_off_wall();
    }
    cur_obj_rotate_yaw_toward(targetYaw, turnAmount);
}

s16 obj_get_pitch_to_home(f32 latDistToHome) {
    return atan2s(latDistToHome, o->oPosY - o->oHomeY);
}

void obj_compute_vel_from_move_pitch(f32 speed) {
    o->oForwardVel = speed * coss(o->oMoveAnglePitch);
    o->oVelY = speed * -sins(o->oMoveAnglePitch);
}

s32 clamp_s16(s16 *value, s16 minimum, s16 maximum) {
    if (*value <= minimum) {
        *value = minimum;
    } else if (*value >= maximum) {
        *value = maximum;
    } else {
        return FALSE;
    }

    return TRUE;
}

s32 clamp_f32(f32 *value, f32 minimum, f32 maximum) {
    if (*value <= minimum) {
        *value = minimum;
    } else if (*value >= maximum) {
        *value = maximum;
    } else {
        return FALSE;
    }

    return TRUE;
}

void cur_obj_init_anim_extend(s32 animIndex) {
    cur_obj_init_animation_with_sound(animIndex);
    cur_obj_extend_animation_if_at_end();
}

s32 cur_obj_init_anim_and_check_if_end(s32 animIndex) {
    cur_obj_init_animation_with_sound(animIndex);
    return cur_obj_check_if_near_animation_end();
}

s32 cur_obj_init_anim_check_frame(s32 animIndex, s32 frame) {
    cur_obj_init_animation_with_sound(animIndex);
    return cur_obj_check_anim_frame(frame);
}

s32 cur_obj_set_anim_if_at_end(s32 animIndex) {
    if (cur_obj_check_if_at_animation_end()) {
        cur_obj_init_animation_with_sound(animIndex);
        return TRUE;
    }
    return FALSE;
}

s32 cur_obj_play_sound_at_anim_range(s8 startFrame1, s8 startFrame2, u32 sound) {
    s32 rangeLength = o->header.gfx.animInfo.animAccel / 0x10000;

    if (rangeLength <= 0) {
        rangeLength = 1;
    }

    if (cur_obj_check_anim_frame_in_range(startFrame1, rangeLength) || cur_obj_check_anim_frame_in_range(startFrame2, rangeLength)) {
        cur_obj_play_sound_2(sound);
        return TRUE;
    }

    return FALSE;
}

s16 obj_turn_pitch_toward_mario(f32 targetOffsetY, s16 turnAmount) {
    s16 targetPitch;

    o->oPosY -= targetOffsetY;
    targetPitch = obj_turn_toward_object(o, gMarioObject, O_MOVE_ANGLE_PITCH_INDEX, turnAmount);
    o->oPosY += targetOffsetY;

    return targetPitch;
}

s32 approach_f32_ptr(f32 *px, f32 target, f32 delta) {
    if (*px > target) {
        delta = -delta;
    }

    *px += delta;

    if ((*px - target) * delta >= 0) {
        *px = target;
        return TRUE;
    }
    return FALSE;
}

s32 obj_forward_vel_approach(f32 target, f32 delta) {
    return approach_f32_ptr(&o->oForwardVel, target, delta);
}

s32 obj_y_vel_approach(f32 target, f32 delta) {
    return approach_f32_ptr(&o->oVelY, target, delta);
}

s32 obj_move_pitch_approach(s16 target, s16 delta) {
    o->oMoveAnglePitch = approach_s16_symmetric(o->oMoveAnglePitch, target, delta);

    if ((s16) o->oMoveAnglePitch == target) {
        return TRUE;
    }

    return FALSE;
}

s32 obj_face_pitch_approach(s16 targetPitch, s16 deltaPitch) {
    o->oFaceAnglePitch = approach_s16_symmetric(o->oFaceAnglePitch, targetPitch, deltaPitch);

    if ((s16) o->oFaceAnglePitch == targetPitch) {
        return TRUE;
    }

    return FALSE;
}

s32 obj_face_yaw_approach(s16 targetYaw, s16 deltaYaw) {
    o->oFaceAngleYaw = approach_s16_symmetric(o->oFaceAngleYaw, targetYaw, deltaYaw);

    if ((s16) o->oFaceAngleYaw == targetYaw) {
        return TRUE;
    }

    return FALSE;
}

s32 obj_face_roll_approach(s16 targetRoll, s16 deltaRoll) {
    o->oFaceAngleRoll = approach_s16_symmetric(o->oFaceAngleRoll, targetRoll, deltaRoll);

    if ((s16) o->oFaceAngleRoll == targetRoll) {
        return TRUE;
    }

    return FALSE;
}

s32 obj_smooth_turn(s16 *angleVel, s32 *angle, s16 targetAngle, f32 targetSpeedProportion,
                           s16 accel, s16 minSpeed, s16 maxSpeed) {
    s16 currentSpeed;
    s16 currentAngle = (s16)(*angle);

    *angleVel = approach_s16_symmetric(*angleVel, (targetAngle - currentAngle) * targetSpeedProportion, accel);

    currentSpeed = abss(*angleVel);
    clamp_s16(&currentSpeed, minSpeed, maxSpeed);

    *angle = approach_angle(*angle, targetAngle, currentSpeed);
    return (s16)(*angle) == targetAngle;
}

void obj_roll_to_match_yaw_turn(s16 targetYaw, s16 maxRoll, s16 rollSpeed) {
    s16 targetRoll = o->oMoveAngleYaw - targetYaw;
    clamp_s16(&targetRoll, -maxRoll, maxRoll);
    obj_face_roll_approach(targetRoll, rollSpeed);
}

s16 random_linear_offset(s16 base, s16 range) {
    return base + (s16)(range * random_float());
}

s16 random_mod_offset(s16 base, s16 step, s16 mod) {
    return base + step * (random_u16() % mod);
}

s16 obj_random_fixed_turn(s16 delta) {
    return o->oMoveAngleYaw + (s16) random_sign() * delta;
}

/**
 * Begin by increasing the object's scale by *scaleVel, and slowly decreasing
 * scaleVel. Once the object starts to shrink, wait a bit, and then begin to
 * scale the object toward endScale. The first time it reaches below
 * shootFireScale during this time, return 1.
 * Return -1 once it's reached endScale.
 */
s32 obj_grow_then_shrink(f32 *scaleVel, f32 shootFireScale, f32 endScale) {
    if (o->oTimer < 2) {
        o->header.gfx.scale[0] += *scaleVel;

        if ((*scaleVel -= 0.01f) > -0.03f) {
            o->oTimer = 0;
        }
    } else if (o->oTimer > 10) {
        if (approach_f32_ptr(&o->header.gfx.scale[0], endScale, 0.05f)) {
            return -1;
        } else if (*scaleVel != 0.0f && o->header.gfx.scale[0] < shootFireScale) {
            *scaleVel = 0.0f;
            return 1;
        }
    }

    return 0;
}

s32 oscillate_toward(s32 *value, f32 *vel, s32 target, f32 velCloseToZero, f32 accel, f32 slowdown) {
    s32 startValue = *value;
    *value += (s32) *vel;

    if (*value == target
        || ((*value - target) * (startValue - target) < 0 && *vel > -velCloseToZero
            && *vel < velCloseToZero)) {
        *value = target;
        *vel = 0.0f;
        return TRUE;
    } else {
        if (*value >= target) {
            accel = -accel;
        }
        if (*vel * accel < 0.0f) {
            accel *= slowdown;
        }

        *vel += accel;
    }

    return FALSE;
}

void obj_update_blinking(s32 *blinkTimer, s16 baseCycleLength, s16 cycleLengthRange,
                                s16 blinkLength) {
    if (*blinkTimer != 0) {
        (*blinkTimer)--;
    } else {
        *blinkTimer = random_linear_offset(baseCycleLength, cycleLengthRange);
    }

    if (*blinkTimer > blinkLength) {
        o->oAnimState = OBJ_BLINKING_ANIM_STATE_EYES_OPEN;
    } else {
        o->oAnimState = OBJ_BLINKING_ANIM_STATE_EYES_CLOSED;
    }
}

s32 obj_resolve_object_collisions(s32 *targetYaw) {
    struct Object *otherObject;
    f32 dx, dz;
    s16 angle;
    f32 radius, otherRadius, relativeRadius;

    if (o->numCollidedObjs != 0) {
        s32 i;
        for (i = 0; i < o->numCollidedObjs; i++) {
            otherObject = o->collidedObjs[i];
            if (otherObject == gMarioObject) continue;
            if (otherObject->oInteractType & INTERACT_MASK_NO_OBJ_COLLISIONS) continue;

            dx = o->oPosX - otherObject->oPosX;
            dz = o->oPosZ - otherObject->oPosZ;

            radius = o->hurtboxRadius > 0 ? o->hurtboxRadius : o->hitboxRadius;
            otherRadius = otherObject->hurtboxRadius > 0 ? otherObject->hurtboxRadius : otherObject->hitboxRadius;
            relativeRadius = radius + otherRadius;

            if ((sqr(dx) + sqr(dz)) > sqr(relativeRadius)) continue;
            angle    = atan2s(dz, dx);
            o->oPosX = otherObject->oPosX + (relativeRadius * sins(angle));
            o->oPosZ = otherObject->oPosZ + (relativeRadius * coss(angle));

            if (targetYaw != NULL && abs_angle_diff(o->oMoveAngleYaw, angle) < 0x4000) {
                *targetYaw = (s16)(angle - o->oMoveAngleYaw + angle + 0x8000);
                return TRUE;
            }
        }
    }

    return FALSE;
}

s32 obj_bounce_off_walls_edges_objects(s32 *targetYaw) {
    if (o->oMoveFlags & OBJ_MOVE_HIT_WALL) {
        *targetYaw = cur_obj_reflect_move_angle_off_wall();
    } else if (o->oMoveFlags & OBJ_MOVE_HIT_EDGE) {
        *targetYaw = (s16)(o->oMoveAngleYaw + 0x8000);
    } else if (!obj_resolve_object_collisions(targetYaw)) {
        return FALSE;
    }

    return TRUE;
}

s32 obj_resolve_collisions_and_turn(s16 targetYaw, s16 turnSpeed) {
    obj_resolve_object_collisions(NULL);

    if (cur_obj_rotate_yaw_toward(targetYaw, turnSpeed)) {
        return FALSE;
    } else {
        return TRUE;
    }
}

void obj_die_if_health_non_positive(void) {
    if (o->oHealth <= 0) {
        if (o->oDeathSound == 0) {
            spawn_mist_particles_with_sound(SOUND_OBJ_DEFAULT_DEATH);
        } else if (o->oDeathSound > 0) {
            spawn_mist_particles_with_sound(o->oDeathSound);
        } else {
            spawn_mist_particles();
        }

        if ((s32)o->oNumLootCoins < 0) {
            spawn_object(o, MODEL_BLUE_COIN, bhvMrIBlueCoin);
        } else {
            obj_spawn_loot_yellow_coins(o, o->oNumLootCoins, 20.0f);
        }
        // This doesn't do anything
        obj_spawn_loot_yellow_coins(o, o->oNumLootCoins, 20.0f);

        if (o->oHealth < 0) {
            cur_obj_hide();
            cur_obj_become_intangible();
        } else {
            obj_mark_for_deletion(o);
        }
    }
}
UNUSED  void obj_unused_die(void) {
    o->oHealth = 0;
    obj_die_if_health_non_positive();
}

void obj_set_knockback_action(s32 attackType) {
    switch (attackType) {
        case ATTACK_KICK_OR_TRIP:
        case ATTACK_FAST_ATTACK:
            o->oAction = OBJ_ACT_VERTICAL_KNOCKBACK;
            o->oForwardVel = 20.0f;
            o->oVelY = 50.0f;
            break;

        default:
            o->oAction = OBJ_ACT_HORIZONTAL_KNOCKBACK;
            o->oForwardVel = 50.0f;
            o->oVelY = 30.0f;
            break;
    }

    o->oFlags &= ~OBJ_FLAG_SET_FACE_YAW_TO_MOVE_YAW;
    o->oMoveAngleYaw = obj_angle_to_object(gMarioObject, o);
}

void obj_set_squished_action(void) {
    cur_obj_play_sound_2(SOUND_OBJ_STOMPED);
    o->oAction = OBJ_ACT_SQUISHED;
}

s32 obj_die_if_above_lava_and_health_non_positive(void) {
    if (o->oMoveFlags & OBJ_MOVE_UNDERWATER_ON_GROUND) {
        if (o->oGravity + o->oBuoyancy > 0.0f
            || find_water_level(o->oPosX, o->oPosZ) - o->oPosY < 150.0f) {
            return FALSE;
        }
    } else if (!(o->oMoveFlags & OBJ_MOVE_ABOVE_LAVA)) {
        if (o->oMoveFlags & OBJ_MOVE_ENTERED_WATER) {
            if (o->oWallHitboxRadius < 200.0f) {
                cur_obj_play_sound_2(SOUND_OBJ_DIVING_INTO_WATER);
            } else {
                cur_obj_play_sound_2(SOUND_OBJ_DIVING_IN_WATER);
            }
        }
        return FALSE;
    }

    obj_die_if_health_non_positive();
    return TRUE;
}

s32 obj_handle_attacks(struct ObjectHitbox *hitbox, s32 attackedMarioAction,
                              u8 *attackHandlers) {
    s32 attackType;

    obj_set_hitbox(o, hitbox);

    //! Die immediately if above lava
    if (obj_die_if_above_lava_and_health_non_positive()) {
        return ATTACK_HANDLER_DIE_IF_HEALTH_NON_POSITIVE;
    } else if (o->oInteractStatus & INT_STATUS_INTERACTED) {
        if (o->oInteractStatus & INT_STATUS_ATTACKED_MARIO) {
            if (o->oAction != attackedMarioAction) {
                o->oAction = attackedMarioAction;
                o->oTimer = 0;
            }
        } else {
            attackType = o->oInteractStatus & INT_STATUS_ATTACK_MASK;

            switch (attackHandlers[attackType - 1]) {
                case ATTACK_HANDLER_NOP:
                    break;

                case ATTACK_HANDLER_DIE_IF_HEALTH_NON_POSITIVE:
                    obj_die_if_health_non_positive();
                    break;

                case ATTACK_HANDLER_KNOCKBACK:
                    obj_set_knockback_action(attackType);
                    break;

                case ATTACK_HANDLER_SQUISHED:
                    obj_set_squished_action();
                    break;

                case ATTACK_HANDLER_SPECIAL_KOOPA_LOSE_SHELL:
                    shelled_koopa_attack_handler(attackType);
                    break;

                case ATTACK_HANDLER_SET_SPEED_TO_ZERO:
                    obj_set_speed_to_zero();
                    break;

                case ATTACK_HANDLER_SPECIAL_WIGGLER_JUMPED_ON:
                    wiggler_jumped_on_attack_handler();
                    break;

                case ATTACK_HANDLER_SPECIAL_HUGE_GOOMBA_WEAKLY_ATTACKED:
                    huge_goomba_weakly_attacked();
                    break;

                case ATTACK_HANDLER_SQUISHED_WITH_BLUE_COIN:
                    o->oNumLootCoins = -1;
                    obj_set_squished_action();
                    break;
            }

            o->oInteractStatus = INT_STATUS_NONE;
            return attackType;
        }
    }

    o->oInteractStatus = INT_STATUS_NONE;
    return ATTACK_HANDLER_NOP;
}

void obj_act_knockback(UNUSED f32 baseScale) {
    cur_obj_update_floor_and_walls();

    if (o->header.gfx.animInfo.curAnim != NULL) {
        cur_obj_extend_animation_if_at_end();
    }

    //! Dies immediately if above lava
    if ((o->oMoveFlags
         & (OBJ_MOVE_MASK_ON_GROUND | OBJ_MOVE_MASK_IN_WATER | OBJ_MOVE_HIT_WALL | OBJ_MOVE_ABOVE_LAVA))
        || (o->oAction == OBJ_ACT_VERTICAL_KNOCKBACK && o->oTimer >= 9)) {
        obj_die_if_health_non_positive();
    }

    cur_obj_move_standard(-78);
}

void obj_act_squished(f32 baseScale) {
    f32 targetScaleY = baseScale * 0.3f;

    cur_obj_update_floor_and_walls();

    if (o->header.gfx.animInfo.curAnim != NULL) {
        cur_obj_extend_animation_if_at_end();
    }

    if (approach_f32_ptr(&o->header.gfx.scale[1], targetScaleY, baseScale * 0.14f)) {
        o->header.gfx.scale[0] = o->header.gfx.scale[2] = baseScale * 2.0f - o->header.gfx.scale[1];

        if (o->oTimer >= 16) {
            obj_die_if_health_non_positive();
        }
    }

    o->oForwardVel = 0.0f;
    cur_obj_move_standard(-78);
}

s32 obj_update_standard_actions(f32 scale) {
    if (o->oAction < 100) {
        return TRUE;
    } else {
        cur_obj_become_intangible();

        switch (o->oAction) {
            case OBJ_ACT_HORIZONTAL_KNOCKBACK:
            case OBJ_ACT_VERTICAL_KNOCKBACK:
                obj_act_knockback(scale);
                break;

            case OBJ_ACT_SQUISHED:
                obj_act_squished(scale);
                break;
        }

        return FALSE;
    }
}

s32 obj_check_attacks(struct ObjectHitbox *hitbox, s32 attackedMarioAction) {
    s32 attackType;

    obj_set_hitbox(o, hitbox);

    //! Dies immediately if above lava
    if (obj_die_if_above_lava_and_health_non_positive()) {
        return ATTACK_HANDLER_DIE_IF_HEALTH_NON_POSITIVE;
    } else if (o->oInteractStatus & INT_STATUS_INTERACTED) {
        if (o->oInteractStatus & INT_STATUS_ATTACKED_MARIO) {
            if (o->oAction != attackedMarioAction) {
                o->oAction = attackedMarioAction;
                o->oTimer = 0;
            }
        } else {
            attackType = o->oInteractStatus & INT_STATUS_ATTACK_MASK;
            obj_die_if_health_non_positive();
            o->oInteractStatus = INT_STATUS_NONE;
            return attackType;
        }
    }

    o->oInteractStatus = INT_STATUS_NONE;
    return ATTACK_HANDLER_NOP;
}

s32 obj_move_for_one_second(s32 endAction) {
    cur_obj_update_floor_and_walls();
    cur_obj_extend_animation_if_at_end();

    if (o->oTimer > 30) {
        o->oAction = endAction;
        return TRUE;
    }

    cur_obj_move_standard(-78);
    return FALSE;
}

/**
 * If we are far from home (> threshold away), then set oAngleToMario to the
 * angle to home and oDistanceToMario to 25000.
 * If we are close to home, but Mario is far from us (> threshold away), then
 * keep oAngleToMario the same and set oDistanceToMario to 20000.
 * If we are close to both home and Mario, then keep both oAngleToMario and
 * oDistanceToMario the same.
 *
 * The point of this function is to avoid having to write extra code to get
 * the object to return to home. When Mario is far away and the object is far
 * from home, it could theoretically re-use the "approach Mario" logic to approach
 * its home instead.
 * However, most objects that use this function handle the far-from-home case
 * separately anyway.
 * This function causes seemingly erroneous behavior in some objects that try to
 * attack Mario (e.g. fly guy shooting fire or lunging), especially when combined
 * with partial updates.
 */
void treat_far_home_as_mario(f32 threshold) {
    Vec3f d;
    vec3f_diff(d, &o->oHomeVec, &o->oPosVec);

    if (vec3_sumsq(d) > sqr(threshold)) {
        o->oAngleToMario = atan2s(d[2], d[0]);
        o->oDistanceToMario = 25000.0f;
    } else {
        if (!gMarioObject) {
            o->oDistanceToMario = 20000.0f;
            return;
        }

        vec3f_diff(d, &o->oHomeVec, &gMarioObject->oPosVec);
        if (vec3_sumsq(d) > sqr(threshold)) {
            o->oDistanceToMario = 20000.0f;
        }
    }
}

// TODO koopa : Text arg field name
// TODO chain_chomp : chain_chomp_sub_act_lunge documentation
// TODO wiggler
// TODO enemy_lakitu
// TODO camera_lakitu : 104 label, follow cam documentation
// TODO monty_mole
// TODO water_bomb : Shadow position
// TODO ttc_treadmill

/**
 * Used by bowser, fly guy, piranha plant, and fire spitters.
 */
void obj_spit_fire(s16 relativePosX, s16 relativePosY, s16 relativePosZ, f32 scale, ModelID32 model,
                   f32 startSpeed, f32 endSpeed, s16 movePitch) {
    struct Object *obj = spawn_object_relative_with_scale(MOVING_FLAME_BP_MOVE, relativePosX, relativePosY, relativePosZ,
                                                           scale, o, model, bhvMovingFlame);

    if (obj != NULL) {
        obj->oSmallPiranhaFlameStartSpeed = startSpeed;
        obj->oSmallPiranhaFlameEndSpeed = endSpeed;
        obj->oSmallPiranhaFlameModel = model;
        obj->oMoveAnglePitch = movePitch;
    }
}


