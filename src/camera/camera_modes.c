#include "types.h"
#include "camera/camera_math.h"
#include "engine/math_util.h"
#include "engine/surface_collision.h"
#include "game/camera.h"
#include "camera_modes.h"

/**
 * Set the camera's y coordinate to goalHeight, respecting floors and ceilings in the way
 */
void set_camera_height(struct Camera *c, f32 goalHeight) {
    struct Surface *surface;
    f32 marioFloorHeight, marioCeilHeight, camFloorHeight;
    f32 baseOff = 125.f;
    f32 camCeilHeight = find_ceil(c->pos[0], gLakituState.goalPos[1] - 50.f, c->pos[2], &surface);
#ifdef FAST_VERTICAL_CAMERA_MOVEMENT
    f32 approachRate = 20.0f;
#endif

    if (sMarioCamState->action & ACT_FLAG_HANGING) {
        marioCeilHeight = sMarioGeometry.currCeilHeight;
        marioFloorHeight = sMarioGeometry.currFloorHeight;

        if (marioFloorHeight < marioCeilHeight - 400.f) {
            marioFloorHeight = marioCeilHeight - 400.f;
        }

        goalHeight = marioFloorHeight + (marioCeilHeight - marioFloorHeight) * 0.4f;

        if (sMarioCamState->pos[1] - 400 > goalHeight) {
            goalHeight = sMarioCamState->pos[1] - 400;
        }

        approach_camera_height(c, goalHeight, 5.f);
    } else {
        camFloorHeight = find_floor(c->pos[0], c->pos[1] + 100.f, c->pos[2], &surface) + baseOff;
        marioFloorHeight = baseOff + sMarioGeometry.currFloorHeight;

        if (camFloorHeight < marioFloorHeight) {
            camFloorHeight = marioFloorHeight;
        }
        if (goalHeight < camFloorHeight) {
            goalHeight = camFloorHeight;
            c->pos[1] = goalHeight;
        }
        // Warp camera to goalHeight if further than 1000 and Mario is stuck in the ground
        if (sMarioCamState->action == ACT_BUTT_STUCK_IN_GROUND ||
            sMarioCamState->action == ACT_HEAD_STUCK_IN_GROUND ||
            sMarioCamState->action == ACT_FEET_STUCK_IN_GROUND) {
            if (absf(c->pos[1] - goalHeight) > 1000.0f) {
                c->pos[1] = goalHeight;
            }
        }

#ifdef FAST_VERTICAL_CAMERA_MOVEMENT
        approachRate += ABS(c->pos[1] - goalHeight) / 20;
        approach_camera_height(c, goalHeight, approachRate);
#else
        approach_camera_height(c, goalHeight, 20.f);
#endif

        if (camCeilHeight != CELL_HEIGHT_LIMIT) {
            camCeilHeight -= baseOff;
            if ((c->pos[1] > camCeilHeight && sMarioGeometry.currFloorHeight + baseOff < camCeilHeight)
                || (sMarioGeometry.currCeilHeight != CELL_HEIGHT_LIMIT
                    && sMarioGeometry.currCeilHeight > camCeilHeight && c->pos[1] > camCeilHeight)) {
                c->pos[1] = camCeilHeight;
            }
        }
    }
}

/**
 * Pitch the camera down when the camera is facing down a slope
 */
s16 look_down_slopes(s16 camYaw) {
    struct Surface *floor;
    // Default pitch
    s16 pitch = 0x05B0;
    // x and z offsets towards the camera
    f32 xOff = sMarioCamState->pos[0] + sins(camYaw) * 40.f;
    f32 zOff = sMarioCamState->pos[2] + coss(camYaw) * 40.f;

    f32 floorDY = find_floor(xOff, sMarioCamState->pos[1], zOff, &floor) - sMarioCamState->pos[1];

    if (floor != NULL) {
        if (floor->type != SURFACE_WALL_MISC && floorDY > 0) {
            if (floor->normal.z == 0.f && floorDY < 100.f) {
                pitch = 0x05B0;
            } else {
                // Add the slope's angle of declination to the pitch
                pitch += atan2s(40.f, floorDY);
            }
        }
    }

    return pitch;
}

/**
 * Look ahead to the left or right in the direction the player is facing
 * The calculation for pan[0] could be simplified to:
 *      yaw = -yaw;
 *      pan[0] = sins(sMarioCamState->faceAngle[1] + yaw) * sins(0xC00) * dist;
 * Perhaps, early in development, the pan used to be calculated for both the x and z directions
 *
 * Since this function only affects the camera's focus, Mario's movement direction isn't affected.
 */
void pan_ahead_of_player(struct Camera *c) {
    f32 dist;
    s16 pitch, yaw;
    Vec3f pan = { 0, 0, 0 };

    // Get distance and angle from camera to Mario.
    vec3f_get_dist_and_angle(c->pos, sMarioCamState->pos, &dist, &pitch, &yaw);

    // The camera will pan ahead up to about 30% of the camera's distance to Mario.
    pan[2] = sins(0xC00) * dist;

    rotate_in_xz(pan, pan, sMarioCamState->faceAngle[1]);
    // rotate in the opposite direction
    yaw = -yaw;
    rotate_in_xz(pan, pan, yaw);
    // Only pan left or right
    pan[2] = 0.f;

    // If Mario is long jumping, or on a flag pole (but not at the top), then pan in the opposite direction
    if (sMarioCamState->action == ACT_LONG_JUMP ||
       (sMarioCamState->action != ACT_TOP_OF_POLE && (sMarioCamState->action & ACT_FLAG_ON_POLE))) {
        pan[0] = -pan[0];
    }

    // Slowly make the actual pan, sPanDistance, approach the calculated pan
    // If Mario is sleeping, then don't pan
    if (sStatusFlags & CAM_FLAG_SLEEPING) {
        approach_f32_asymptotic_bool(&sPanDistance, 0.f, 0.025f);
    } else {
        approach_f32_asymptotic_bool(&sPanDistance, pan[0], 0.025f);
    }

    // Now apply the pan. It's a dir vector to the left or right, rotated by the camera's yaw to Mario
    pan[0] = sPanDistance;
    yaw = -yaw;
    rotate_in_xz(pan, pan, yaw);
    vec3f_add(c->focus, pan);
}
