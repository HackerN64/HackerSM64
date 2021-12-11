#include <PR/ultratypes.h>

#include "engine/math_util.h"
#include "engine/surface_collision.h"
#include "level_update.h"
#include "object_fields.h"
#include "object_helpers.h"
#include "object_list_processor.h"
#include "platform_displacement.h"
#include "types.h"
#include "sm64.h"
#include "behavior_data.h"

#include "config.h"

struct Object *gMarioPlatform = NULL;

/**
 * Determine if Mario is standing on a platform object, meaning that he is
 * within 4 units of the floor. Set his referenced platform object accordingly.
 */
void update_mario_platform(void) {
    struct Surface *floor;
    f32 marioX, marioY, marioZ;
    f32 floorHeight;
    u32 awayFromFloor;

    if (gMarioObject == NULL) {
        return;
    }

    //! If Mario moves onto a rotating platform in a PU, the find_floor call
    //  will detect the platform and he will end up receiving a large amount
    //  of displacement since he is considered to be far from the platform's
    //  axis of rotation.

    marioX = gMarioObject->oPosX;
    marioY = gMarioObject->oPosY;
    marioZ = gMarioObject->oPosZ;
    floorHeight = find_floor(marioX, marioY, marioZ, &floor);

    awayFromFloor =  absf(marioY - floorHeight) >= 4.0f;

    if (awayFromFloor) {
        gMarioPlatform = NULL;
        gMarioObject->platform = NULL;
    } else {
        if (floor != NULL && floor->object != NULL) {
            gMarioPlatform = floor->object;
            gMarioObject->platform = floor->object;
        } else {
            gMarioPlatform = NULL;
            gMarioObject->platform = NULL;
        }
    }
}

/**
 * Get Mario's position and store it in x, y, and z.
 */
void get_mario_pos(f32 *x, f32 *y, f32 *z) {
    *x = gMarioStates[0].pos[0];
    *y = gMarioStates[0].pos[1];
    *z = gMarioStates[0].pos[2];
}

/**
 * Set Mario's position.
 */
void set_mario_pos(f32 x, f32 y, f32 z) {
    gMarioStates[0].pos[0] = x;
    gMarioStates[0].pos[1] = y;
    gMarioStates[0].pos[2] = z;
}

#ifdef PLATFORM_DISPLACEMENT_2
static struct PlatformDisplacementInfo sMarioDisplacementInfo;
static Vec3f sMarioAmountDisplaced;

extern s32 gGlobalTimer;

/**
 * Upscale or downscale a vector by another vector.
 */
static void scale_vec3f(Vec3f dst, Vec3f src, Vec3f scale, u32 doInverted) {
    if (doInverted) {
        dst[0] = src[0] / scale[0];
        dst[1] = src[1] / scale[1];
        dst[2] = src[2] / scale[2];
    } else {
        dst[0] = src[0] * scale[0];
        dst[1] = src[1] * scale[1];
        dst[2] = src[2] * scale[2];
    }
}

/**
 * Apply one frame of platform displacement to Mario or an object using the given
 * platform.
 */
void apply_platform_displacement(struct PlatformDisplacementInfo *displaceInfo, Vec3f pos, s16 *yaw, struct Object *platform) {
    Vec3f platformPos;
    Vec3f posDifference;
    Vec3f yawVec;
    Vec3f scaledPos;
    // Determine how much Mario turned on his own since last frame
    s16 yawDifference = *yaw - displaceInfo->prevYaw;

    // Avoid a crash if the platform unloaded its collision while stood on
    if (platform->header.gfx.throwMatrix == NULL) return;

    vec3f_copy(platformPos, (*platform->header.gfx.throwMatrix)[3]);

    // Determine how far Mario moved on his own since last frame
    vec3f_copy(posDifference, pos);
    vec3f_sub(posDifference, displaceInfo->prevPos);

    if ((platform == displaceInfo->prevPlatform) && (gGlobalTimer == displaceInfo->prevTimer + 1)) {
        // Transform from relative positions to world positions
        scale_vec3f(scaledPos, displaceInfo->prevTransformedPos, platform->header.gfx.scale, FALSE);
        linear_mtxf_mul_vec3f(*platform->header.gfx.throwMatrix, pos, scaledPos);

        // Add on how much Mario moved in the previous frame
        vec3f_add(pos, posDifference);

        // Calculate new yaw
        linear_mtxf_mul_vec3f(*platform->header.gfx.throwMatrix, yawVec, displaceInfo->prevTransformedYawVec);
        *yaw = atan2s(yawVec[2], yawVec[0]) + yawDifference;
    } else {
        // First frame of standing on the platform, don't calculate a new position
        vec3f_sub(pos, platformPos);
    }

    // Apply velocity-based displacement for certain objects (like the TTC Treadmills)
    if (platform->oFlags & OBJ_FLAG_VELOCITY_PLATFORM) {
        pos[0] += platform->oVelX;
        pos[1] += platform->oVelY;
        pos[2] += platform->oVelZ;
    }

    // Transform from world positions to relative positions for use next frame
    linear_mtxf_transpose_mul_vec3f(*platform->header.gfx.throwMatrix, scaledPos, pos);
    scale_vec3f(displaceInfo->prevTransformedPos, scaledPos, platform->header.gfx.scale, TRUE);
    vec3f_add(pos, platformPos);

    // If the object is Mario, set inertia
    if (pos == gMarioState->pos) {
        vec3f_copy(sMarioAmountDisplaced, pos);
        vec3f_sub(sMarioAmountDisplaced, displaceInfo->prevPos);
        vec3f_sub(sMarioAmountDisplaced, posDifference);

        // Make sure inertia isn't set on the first frame otherwise the previous value isn't cleared
        if ((platform != displaceInfo->prevPlatform) || (gGlobalTimer != displaceInfo->prevTimer + 1)) {
            vec3_zero(sMarioAmountDisplaced);
        }
    }

    // Update info for next frame
    // Update position
    vec3f_copy(displaceInfo->prevPos, pos);

    // Set yaw info
    vec3f_set(yawVec, sins(*yaw), 0, coss(*yaw));
    linear_mtxf_transpose_mul_vec3f(*platform->header.gfx.throwMatrix, displaceInfo->prevTransformedYawVec, yawVec);
    displaceInfo->prevYaw = *yaw;

    // Update platform and timer
    displaceInfo->prevPlatform = platform;
    displaceInfo->prevTimer = gGlobalTimer;
}

// Doesn't change in the code, set this to FALSE if you don't want inertia
u8 gDoInertia = TRUE;

static u8 sShouldApplyInertia = FALSE;
static u8 sInertiaFirstFrame = FALSE;

/**
 * Apply inertia based on Mario's last platform.
 */
static void apply_mario_inertia(void) {
    // On the first frame of leaving the ground, boost Mario's y velocity
    if (sInertiaFirstFrame) {
        gMarioState->vel[1] += sMarioAmountDisplaced[1];
    }

    // Apply sideways inertia
    gMarioState->pos[0] += sMarioAmountDisplaced[0];
    gMarioState->pos[2] += sMarioAmountDisplaced[2];

    // Drag
    sMarioAmountDisplaced[0] *= 0.97f;
    sMarioAmountDisplaced[2] *= 0.97f;

    // Stop applying inertia once Mario has landed, or when ground pounding
    if (!(gMarioState->action & ACT_FLAG_AIR) || (gMarioState->action == ACT_GROUND_POUND)) {
        sShouldApplyInertia = FALSE;
    }
}

/**
 * Apply platform displacement or inertia if required.
 */
void apply_mario_platform_displacement(void) {
    struct Object *platform;

    platform = gMarioPlatform;
    if (!(gTimeStopState & TIME_STOP_ACTIVE) && gMarioObject != NULL) {
        if (platform != NULL) {
            apply_platform_displacement(&sMarioDisplacementInfo, gMarioState->pos, &gMarioState->faceAngle[1], platform);
            sShouldApplyInertia = TRUE;
            sInertiaFirstFrame = TRUE;
        } else if (sShouldApplyInertia && gDoInertia) {
            apply_mario_inertia();
            sInertiaFirstFrame = FALSE;
        }
    }
}

#else

/**
 * Apply one frame of platform rotation to Mario or an object using the given
 * platform. If isMario is false, use gCurrentObject.
 */
void apply_platform_displacement(u32 isMario, struct Object *platform) {
    f32 x, y, z;
    f32 platformPosX, platformPosY, platformPosZ;
    Vec3f currentObjectOffset;
    Vec3f relativeOffset;
    Vec3f newObjectOffset;
    Vec3s rotation;
    Mat4 displaceMatrix;

    rotation[0] = platform->oAngleVelPitch;
    rotation[1] = platform->oAngleVelYaw;
    rotation[2] = platform->oAngleVelRoll;

    if (isMario) {
        get_mario_pos(&x, &y, &z);
    } else {
        x = gCurrentObject->oPosX;
        y = gCurrentObject->oPosY;
        z = gCurrentObject->oPosZ;
    }

    x += platform->oVelX;
    z += platform->oVelZ;

    if (rotation[0] != 0 || rotation[1] != 0 || rotation[2] != 0) {
        // unusedPitch = rotation[0];
        // unusedRoll  = rotation[2];
        // unusedYaw   = platform->oFaceAngleYaw;

        if (isMario) {
            gMarioStates[0].faceAngle[1] += rotation[1];
        }

        platformPosX = platform->oPosX;
        platformPosY = platform->oPosY;
        platformPosZ = platform->oPosZ;

        currentObjectOffset[0] = x - platformPosX;
        currentObjectOffset[1] = y - platformPosY;
        currentObjectOffset[2] = z - platformPosZ;

        rotation[0] = platform->oFaceAnglePitch - platform->oAngleVelPitch;
        rotation[1] = platform->oFaceAngleYaw - platform->oAngleVelYaw;
        rotation[2] = platform->oFaceAngleRoll - platform->oAngleVelRoll;

        mtxf_rotate_zxy_and_translate(displaceMatrix, currentObjectOffset, rotation);
        linear_mtxf_transpose_mul_vec3f(displaceMatrix, relativeOffset, currentObjectOffset);

        rotation[0] = platform->oFaceAnglePitch;
        rotation[1] = platform->oFaceAngleYaw;
        rotation[2] = platform->oFaceAngleRoll;

        mtxf_rotate_zxy_and_translate(displaceMatrix, currentObjectOffset, rotation);
        linear_mtxf_mul_vec3f(displaceMatrix, newObjectOffset, relativeOffset);

        x = platformPosX + newObjectOffset[0];
        y = platformPosY + newObjectOffset[1];
        z = platformPosZ + newObjectOffset[2];
    }

    if (isMario) {
        set_mario_pos(x, y, z);
    } else {
        gCurrentObject->oPosX = x;
        gCurrentObject->oPosY = y;
        gCurrentObject->oPosZ = z;
    }
}

/**
 * If Mario's platform is not null, apply platform displacement.
 */
void apply_mario_platform_displacement(void) {
    struct Object *platform = gMarioPlatform;

    if (!(gTimeStopState & TIME_STOP_ACTIVE) && gMarioObject != NULL && platform != NULL) {
        apply_platform_displacement(TRUE, platform);
    }
}
#endif

/**
 * Set Mario's platform to NULL.
 */
void clear_mario_platform(void) {
    gMarioPlatform = NULL;
}
