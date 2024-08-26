#include <ultra64.h>

#include "sm64.h"
#include "camera.h"
#include "seq_ids.h"
#include "dialog_ids.h"
#include "audio/external.h"
#include "mario_misc.h"
#include "game_init.h"
#include "hud.h"
#include "engine/math_util.h"
#include "area.h"
#include "engine/surface_collision.h"
#include "engine/behavior_script.h"
#include "level_update.h"
#include "ingame_menu.h"
#include "mario_actions_cutscene.h"
#include "save_file.h"
#include "object_helpers.h"
#include "print.h"
#include "spawn_sound.h"
#include "behavior_actions.h"
#include "behavior_data.h"
#include "object_list_processor.h"
#include "paintings.h"
#include "engine/graph_node.h"
#include "level_table.h"
#include "config.h"
#include "puppyprint.h"
#include "profiling.h"

#define CBUTTON_MASK (U_CBUTTONS | D_CBUTTONS | L_CBUTTONS | R_CBUTTONS)

/**
 * @file camera.c
 * Implements the camera system, including C-button input, camera modes, camera triggers, and cutscenes.
 *
 * When working with the camera, you should be familiar with sm64's coordinate system.
 * Relative to the camera, the coordinate system follows the right hand rule:
 *          +X points right.
 *          +Y points up.
 *          +Z points out of the screen.
 *
 * You should also be familiar with Euler angles: 'pitch', 'yaw', and 'roll'.
 *      pitch: rotation about the X-axis, measured from +Y.
 *          Unlike yaw and roll, pitch is bounded in +-0x4000 (90 degrees).
 *          Pitch is 0 when the camera points parallel to the xz-plane (+Y points straight up).
 *
 *      yaw: rotation about the Y-axis, measured from (absolute) +Z.
 *          Positive yaw rotates clockwise, towards +X.
 *
 *      roll: rotation about the Z-axis, measured from the camera's right direction.
 *          Unfortunately, it's weird: For some reason, roll is flipped. Positive roll makes the camera
 *          rotate counterclockwise, which means the WORLD rotates clockwise. Luckily roll is rarely
 *          used.
 *
 *      Remember the right hand rule: make a thumbs-up with your right hand, stick your thumb in the
 *      +direction (except for roll), and the angle follows the rotation of your curled fingers.
 *
 * Illustrations:
 * Following the right hand rule, each hidden axis's positive direction points out of the screen.
 *
 *       YZ-Plane (pitch)        XZ-Plane (yaw)          XY-Plane (roll -- Note flipped)
 *          +Y                      -Z                      +Y
 *           ^                       ^ (into the             ^
 *         --|--                     |   screen)             |<-
 * +pitch /  |  \ -pitch             |                       |  \ -roll
 *       v   |   v                   |                       |   |
 * +Z <------O------> -Z   -X <------O------> +X   -X <------O------> +X
 *           |                   ^   |   ^                   |   |
 *           |                    \  |  /                    |  / +roll
 *           |               -yaw  --|--  +yaw               |<-
 *           v                       v                       v
 *          -Y                      +Z                      -Y
 *
 */

// BSS
/**
 * Stores Lakitu's position from the last frame, used for transitioning in next_lakitu_state()
 */
Vec3f sOldPosition;
/**
 * Stores Lakitu's focus from the last frame, used for transitioning in next_lakitu_state()
 */
Vec3f sOldFocus;
/**
 * Global array of PlayerCameraState.
 * L is real.
 */
struct PlayerCameraState gPlayerCameraState[2];
/**
 * Direction controlled by player 2, moves the focus during the credits.
 */
Vec3f sPlayer2FocusOffset;
/**
 * The pitch used for the credits easter egg.
 */
s16 sCreditsPlayer2Pitch;
/**
 * The yaw used for the credits easter egg.
 */
s16 sCreditsPlayer2Yaw;
/**
 * Used to decide when to zoom out in the pause menu.
 */
u8 sFramesPaused;

extern struct CameraFOVStatus sFOVState;
extern struct TransitionInfo sModeTransition;
extern struct PlayerGeometry sMarioGeometry;
extern s16 sAvoidYawVel;
extern s16 sCameraYawAfterDoorCutscene;
extern struct HandheldShakePoint sHandheldShakeSpline[4];
extern s16 sHandheldShakeMag;
extern f32 sHandheldShakeTimer;
extern f32 sHandheldShakeInc;
extern s16 sHandheldShakePitch;
extern s16 sHandheldShakeYaw;
extern s16 sHandheldShakeRoll;
extern s16 sSelectionFlags;
extern s16 s2ndRotateFlags;
extern s16 sCameraSoundFlags;
extern u16 sCButtonsPressed;
extern s16 sCutsceneDialogID;
extern struct LakituState gLakituState;
extern s16 sAreaYaw;
extern s16 sAreaYawChange;
extern s16 sLakituDist;
extern s16 sLakituPitch;
extern f32 sZoomAmount;
extern s16 sCSideButtonYaw;
extern s16 sBehindMarioSoundTimer;
extern f32 sZeroZoomDist;
extern s16 sCUpCameraPitch;
extern s16 sModeOffsetYaw;
extern s16 sSpiralStairsYawOffset;
extern s16 s8DirModeBaseYaw;
extern s16 s8DirModeYawOffset;
extern f32 sPanDistance;
extern f32 sCannonYOffset;
extern struct ModeTransitionInfo sModeInfo;
extern Vec3f sCastleEntranceOffset;
extern u32 sParTrackIndex;
extern struct ParallelTrackingPoint *sParTrackPath;
extern struct CameraStoredInfo sParTrackTransOff;
extern struct CameraStoredInfo sCameraStoreCUp;
extern struct CameraStoredInfo sCameraStoreCutscene;
extern s16 gCameraMovementFlags;
extern s16 sStatusFlags;
extern struct CutsceneSplinePoint sCurCreditsSplinePos[32];
extern struct CutsceneSplinePoint sCurCreditsSplineFocus[32];
extern s16 sCutsceneSplineSegment;
extern f32 sCutsceneSplineSegmentProgress;
extern s16 sCutsceneShot;
extern s16 gCutsceneTimer;
extern struct CutsceneVariable sCutsceneVars[10];
extern s32 gObjCutsceneDone;
extern u32 gCutsceneObjSpawn;
extern struct Camera *gCamera;

/**
 * Lakitu's position and focus.
 * @see LakituState
 */
struct LakituState gLakituState;
struct CameraFOVStatus sFOVState;
struct TransitionInfo sModeTransition;
struct PlayerGeometry sMarioGeometry;
struct Camera *gCamera;
s16 sAvoidYawVel;
s16 sCameraYawAfterDoorCutscene;
/**
 * The current spline that controls the camera's position during the credits.
 */
struct CutsceneSplinePoint sCurCreditsSplinePos[32];

/**
 * The current spline that controls the camera's focus during the credits.
 */
struct CutsceneSplinePoint sCurCreditsSplineFocus[32];

/**
 * The progress (from 0 to 1) through the current spline segment.
 * When it becomes >= 1, 1.0 is subtracted from it and sCutsceneSplineSegment is increased.
 */
f32 sCutsceneSplineSegmentProgress;

/**
 * The current segment of the CutsceneSplinePoint[] being used.
 */
s16 sCutsceneSplineSegment;

// Shaky Hand-held Camera effect variables
struct HandheldShakePoint sHandheldShakeSpline[4];
s16 sHandheldShakeMag;
f32 sHandheldShakeTimer;
f32 sHandheldShakeInc;
s16 sHandheldShakePitch;
s16 sHandheldShakeYaw;
s16 sHandheldShakeRoll;

/**
 * Controls which object to spawn in the intro and ending cutscenes.
 */
u32 gCutsceneObjSpawn;
/**
 * Controls when an object-based cutscene should end. It's only used in the star spawn cutscenes, but
 * Yoshi also toggles this.
 */
s32 gObjCutsceneDone;

/**
 * Determines which R-Trigger mode is selected in the pause menu.
 */
s16 sSelectionFlags;

/**
 * Flags that determine what movements the camera should start / do this frame.
 */
s16 gCameraMovementFlags;

/**
 * Flags that change how modes operate and how Lakitu moves.
 * The most commonly used flag is CAM_FLAG_SMOOTH_MOVEMENT, which makes Lakitu fly to the next position,
 * instead of warping.
 */
s16 sStatusFlags;
/**
 * Flags that determine whether the player has already rotated left or right. Used in radial mode to
 * determine whether to rotate all the way, or just to 60 degrees.
 */
s16 s2ndRotateFlags;
/**
 * Flags that control buzzes and sounds that play, mostly for C-button input.
 */
s16 sCameraSoundFlags;
/**
 * Stores what C-Buttons are pressed this frame.
 */
u16 sCButtonsPressed;
/**
 * A copy of gDialogID, the dialog displayed during the cutscene.
 */
s16 sCutsceneDialogID;
/**
 * The currently playing shot in the cutscene.
 */
s16 sCutsceneShot;
/**
 * The current frame of the cutscene shot.
 */
s16 gCutsceneTimer;

/**
 * The angle of the direction vector from the area's center to Mario's position.
 */
s16 sAreaYaw;

/**
 * How much sAreaYaw changed when Mario moved.
 */
s16 sAreaYawChange;

/**
 * Lakitu's distance from Mario in C-Down mode
 */
s16 sLakituDist;

/**
 * How much Lakitu looks down in C-Down mode
 */
s16 sLakituPitch;

/**
 * The amount of distance left to zoom out
 */
f32 sZoomAmount;

s16 sCSideButtonYaw;

/**
 * Sound timer used to space out sounds in behind Mario mode
 */
s16 sBehindMarioSoundTimer;

/**
 * Virtually unused aside being set to 0 and compared with gCameraZoomDist (which is never < 0)
 */
f32 sZeroZoomDist;

/**
 * The camera's pitch in C-Up mode. Mainly controls Mario's head rotation.
 */
s16 sCUpCameraPitch;
/**
 * The current mode's yaw, which gets added to the camera's yaw.
 */
s16 sModeOffsetYaw;

/**
 * Stores Mario's yaw around the stairs, relative to the camera's position.
 *
 * Used in update_spiral_stairs_camera()
 */
s16 sSpiralStairsYawOffset;

/**
 * The constant offset to 8-direction mode's yaw.
 */
s16 s8DirModeBaseYaw;
/**
 * Player-controlled yaw offset in 8-direction mode, a multiple of 45 degrees.
 */
s16 s8DirModeYawOffset;

/**
 * The distance that the camera will look ahead of Mario in the direction Mario is facing.
 */
f32 sPanDistance;

/**
 * When Mario gets in the cannon, it is pointing straight up and rotates down.
 * This is used to make the camera start up and rotate down, like the cannon.
 */
f32 sCannonYOffset;
/**
 * These structs are used by the cutscenes. Most of the fields are unused, and some (all?) of the used
 * ones have multiple uses.
 * Check the cutscene_start functions for documentation on the cvars used by a specific cutscene.
 */
struct CutsceneVariable sCutsceneVars[10];
struct ModeTransitionInfo sModeInfo;
/**
 * Offset added to sFixedModeBasePosition when Mario is inside, near the castle lobby entrance
 */
Vec3f sCastleEntranceOffset;

/**
 * The index into the current parallel tracking path
 */
u32 sParTrackIndex;

/**
 * The current list of ParallelTrackingPoints used in update_parallel_tracking_camera()
 */
struct ParallelTrackingPoint *sParTrackPath;

/**
 * On the first frame after the camera changes to a different parallel tracking path, this stores the
 * displacement between the camera's calculated new position and its previous positions
 *
 * This transition offset is then used to smoothly interpolate the camera's position between the two
 * paths
 */
struct CameraStoredInfo sParTrackTransOff;

/**
 * The information stored when C-Up is active, used to update Lakitu's rotation when exiting C-Up
 */
struct CameraStoredInfo sCameraStoreCUp;

/**
 * The information stored during cutscenes
 */
struct CameraStoredInfo sCameraStoreCutscene;

// first iteration of data
struct Object *gCutsceneFocus = NULL;

/**
 * The information of a second focus camera used by some objects
 */
struct Object *gSecondCameraFocus = NULL;

/**
 * How fast the camera's yaw should approach the next yaw.
 */
s16 sYawSpeed = 0x400;
s32 gCurrLevelArea = 0;
u32 gPrevLevel = 0;

f32 gCameraZoomDist = 800.0f;

/**
 * A cutscene that plays when the player interacts with an object
 */
u8 sObjectCutscene = CUTSCENE_NONE;

/**
 * The ID of the cutscene that ended. It's set to 0 if no cutscene ended less than 8 frames ago.
 *
 * It is only used to prevent the same cutscene from playing twice before 8 frames have passed.
 */
u8 gRecentCutscene = CUTSCENE_NONE;

/**
 * A timer that increments for 8 frames when a cutscene ends.
 * When it reaches 8, it sets gRecentCutscene to 0.
 */
u8 sFramesSinceCutsceneEnded = 0;
/**
 * Mario's response to a dialog.
 * 0 = No response yet
 * 1 = Yes
 * 2 = No
 * 3 = Dialog doesn't have a response
 */
u8 sCutsceneDialogResponse = DIALOG_RESPONSE_NONE;
struct PlayerCameraState *sMarioCamState = &gPlayerCameraState[0];
// struct PlayerCameraState *sLuigiCamState = &gPlayerCameraState[1];
Vec3f sFixedModeBasePosition    = { 646.0f, 143.0f, -1513.0f };

s32 update_radial_camera(struct Camera *c, Vec3f focus, Vec3f pos);
s32 update_outward_radial_camera(struct Camera *c, Vec3f focus, Vec3f pos);
s32 update_behind_mario_camera(struct Camera *c, Vec3f focus, Vec3f pos);
s32 update_mario_camera(struct Camera *c, Vec3f focus, Vec3f pos);
s32 unused_update_mode_5_camera(struct Camera *c, Vec3f focus, Vec3f pos);
s32 update_c_up(struct Camera *c, Vec3f focus, Vec3f pos);
s32 nop_update_water_camera(struct Camera *c, Vec3f focus, Vec3f pos);
s32 update_slide_or_0f_camera(struct Camera *c, Vec3f focus, Vec3f pos);
s32 update_in_cannon(struct Camera *c, Vec3f focus, Vec3f pos);
s32 update_boss_fight_camera(struct Camera *c, Vec3f focus, Vec3f pos);
s32 update_parallel_tracking_camera(struct Camera *c, Vec3f focus, Vec3f pos);
s32 update_fixed_camera(struct Camera *c, Vec3f focus, Vec3f pos);
s32 update_8_directions_camera(struct Camera *c, Vec3f focus, Vec3f pos);
s32 update_slide_or_0f_camera(struct Camera *c, Vec3f focus, Vec3f pos);
s32 update_spiral_stairs_camera(struct Camera *c, Vec3f focus, Vec3f pos);

typedef s32 (*CameraTransition)(struct Camera *c, Vec3f focus, Vec3f pos);
CameraTransition sModeTransitions[] = {
    NULL,
    update_radial_camera,
    update_outward_radial_camera,
    update_behind_mario_camera,
    update_mario_camera,
    unused_update_mode_5_camera,
    update_c_up,
    update_mario_camera,
    nop_update_water_camera,
    update_slide_or_0f_camera,
    update_in_cannon,
    update_boss_fight_camera,
    update_parallel_tracking_camera,
    update_fixed_camera,
    update_8_directions_camera,
    update_slide_or_0f_camera,
    update_mario_camera,
    update_spiral_stairs_camera
};

// Move these two tables to another include file?
extern u8 sDanceCutsceneIndexTable[][4];
extern u8 sZoomOutAreaMasks[];

/**
 * Starts a camera shake triggered by an interaction
 */
void set_camera_shake_from_hit(s16 shake) {
    switch (shake) {
        // Makes the camera stop for a bit
        case SHAKE_ATTACK:
            gLakituState.focHSpeed = 0;
            gLakituState.posHSpeed = 0;
            break;

        case SHAKE_FALL_DAMAGE:
            set_camera_pitch_shake(0x60, 0x3, 0x8000);
            set_camera_roll_shake(0x60, 0x3, 0x8000);
            break;

        case SHAKE_GROUND_POUND:
            set_camera_pitch_shake(0x60, 0xC, 0x8000);
            break;

        case SHAKE_SMALL_DAMAGE:
            if (sMarioCamState->action & (ACT_FLAG_SWIMMING | ACT_FLAG_METAL_WATER)) {
                set_camera_yaw_shake(0x200, 0x10, 0x1000);
                set_camera_roll_shake(0x400, 0x20, 0x1000);
                set_fov_shake(0x100, 0x30, 0x8000);
            } else {
                set_camera_yaw_shake(0x80, 0x8, 0x4000);
                set_camera_roll_shake(0x80, 0x8, 0x4000);
                set_fov_shake(0x100, 0x30, 0x8000);
            }

            gLakituState.focHSpeed = 0;
            gLakituState.posHSpeed = 0;
            break;

        case SHAKE_MED_DAMAGE:
            if (sMarioCamState->action & (ACT_FLAG_SWIMMING | ACT_FLAG_METAL_WATER)) {
                set_camera_yaw_shake(0x400, 0x20, 0x1000);
                set_camera_roll_shake(0x600, 0x30, 0x1000);
                set_fov_shake(0x180, 0x40, 0x8000);
            } else {
                set_camera_yaw_shake(0x100, 0x10, 0x4000);
                set_camera_roll_shake(0x100, 0x10, 0x4000);
                set_fov_shake(0x180, 0x40, 0x8000);
            }

            gLakituState.focHSpeed = 0;
            gLakituState.posHSpeed = 0;
            break;

        case SHAKE_LARGE_DAMAGE:
            if (sMarioCamState->action & (ACT_FLAG_SWIMMING | ACT_FLAG_METAL_WATER)) {
                set_camera_yaw_shake(0x600, 0x30, 0x1000);
                set_camera_roll_shake(0x800, 0x40, 0x1000);
                set_fov_shake(0x200, 0x50, 0x8000);
            } else {
                set_camera_yaw_shake(0x180, 0x20, 0x4000);
                set_camera_roll_shake(0x200, 0x20, 0x4000);
                set_fov_shake(0x200, 0x50, 0x8000);
            }

            gLakituState.focHSpeed = 0;
            gLakituState.posHSpeed = 0;
            break;

        case SHAKE_HIT_FROM_BELOW:
            gLakituState.focHSpeed = 0.07f;
            gLakituState.posHSpeed = 0.07f;
            break;

        case SHAKE_SHOCK:
            set_camera_pitch_shake(random_float() * 64.0f, 0x8, 0x8000);
            set_camera_yaw_shake(random_float() * 64.0f, 0x8, 0x8000);
            break;
    }
}

/**
 * Start a shake from the environment
 */
void set_environmental_camera_shake(s16 shake) {
    switch (shake) {
        case SHAKE_ENV_EXPLOSION:
            set_camera_pitch_shake(0x60, 0x8, 0x4000);
            break;

        case SHAKE_ENV_BOWSER_THROW_BOUNCE:
            set_camera_pitch_shake(0xC0, 0x8, 0x4000);
            break;

        case SHAKE_ENV_BOWSER_JUMP:
            set_camera_pitch_shake(0x100, 0x8, 0x3000);
            break;

        case SHAKE_ENV_UNUSED_6:
            set_camera_roll_shake(0x80, 0x10, 0x3000);
            break;

        case SHAKE_ENV_UNUSED_7:
            set_camera_pitch_shake(0x20, 0x8, 0x8000);
            break;

        case SHAKE_ENV_PYRAMID_EXPLODE:
            set_camera_pitch_shake(0x40, 0x8, 0x8000);
            break;

        case SHAKE_ENV_JRB_SHIP_DRAIN:
            set_camera_pitch_shake(0x20, 0x8, 0x8000);
            set_camera_roll_shake(0x400, 0x10, 0x100);
            break;

        case SHAKE_ENV_FALLING_BITS_PLAT:
            set_camera_pitch_shake(0x40, 0x2, 0x8000);
            break;

        case SHAKE_ENV_UNUSED_5:
            set_camera_yaw_shake(-0x200, 0x80, 0x200);
            break;
    }
}

/**
 * Starts a camera shake, but scales the amplitude by the point's distance from the camera
 */
void set_camera_shake_from_point(s16 shake, f32 posX, f32 posY, f32 posZ) {
    switch (shake) {
        case SHAKE_POS_BOWLING_BALL:
            set_pitch_shake_from_point(0x28, 0x8, 0x4000, 2000.0f, posX, posY, posZ);
            break;

        case SHAKE_POS_SMALL:
            set_pitch_shake_from_point(0x80, 0x8, 0x4000, 4000.0f, posX, posY, posZ);
            set_fov_shake_from_point_preset(SHAKE_FOV_SMALL, posX, posY, posZ);
            break;

        case SHAKE_POS_MEDIUM:
            set_pitch_shake_from_point(0xC0, 0x8, 0x4000, 6000.0f, posX, posY, posZ);
            set_fov_shake_from_point_preset(SHAKE_FOV_MEDIUM, posX, posY, posZ);
            break;

        case SHAKE_POS_LARGE:
            set_pitch_shake_from_point(0x100, 0x8, 0x3000, 8000.0f, posX, posY, posZ);
            set_fov_shake_from_point_preset(SHAKE_FOV_LARGE, posX, posY, posZ);
            break;
    }
}

/**
 * Start a camera shake from an environmental source, but only shake the camera's pitch.
 */
void unused_set_camera_pitch_shake_env(s16 shake) {
    switch (shake) {
        case SHAKE_ENV_EXPLOSION:
            set_camera_pitch_shake(0x60, 0x8, 0x4000);
            break;

        case SHAKE_ENV_BOWSER_THROW_BOUNCE:
            set_camera_pitch_shake(0xC0, 0x8, 0x4000);
            break;

        case SHAKE_ENV_BOWSER_JUMP:
            set_camera_pitch_shake(0x100, 0x8, 0x3000);
            break;
    }
}

/**
 * Calculates Mario's distance to the floor, or the water level if it is above the floor. Then:
 * `posOff` is set to the distance multiplied by posMul and bounded to [-posBound, posBound]
 * `focOff` is set to the distance multiplied by focMul and bounded to [-focBound, focBound]
 *
 * Notes:
 *      posMul is always 1.0f, focMul is always 0.9f
 *      both ranges are always 200.0f
 *          Since focMul is 0.9, `focOff` is closer to the floor than `posOff`
 *      posOff and focOff are sometimes the same address, which just ignores the pos calculation
 */
void calc_y_to_curr_floor(f32 *posOff, f32 posMul, f32 posBound, f32 *focOff, f32 focMul, f32 focBound) {
    f32 floorHeight = sMarioGeometry.currFloorHeight;
    f32 waterHeight;

    if (!(sMarioCamState->action & ACT_FLAG_METAL_WATER)) {
        //! @bug this should use sMarioGeometry.waterHeight
        if (floorHeight < (waterHeight = find_water_level(sMarioCamState->pos[0], sMarioCamState->pos[2]))) {
            floorHeight = waterHeight;
        }
    }

    if (sMarioCamState->action & ACT_FLAG_ON_POLE) {
        if (sMarioGeometry.currFloorHeight >= gMarioStates[0].usedObj->oPosY && sMarioCamState->pos[1]
                   < 0.7f * gMarioStates[0].usedObj->hitboxHeight + gMarioStates[0].usedObj->oPosY) {
            posBound = 1200;
        }
    }

    *posOff = (floorHeight - sMarioCamState->pos[1]) * posMul;

    if (*posOff > posBound) {
        *posOff = posBound;
    }

    if (*posOff < -posBound) {
        *posOff = -posBound;
    }

    *focOff = (floorHeight - sMarioCamState->pos[1]) * focMul;

    if (*focOff > focBound) {
        *focOff = focBound;
    }

    if (*focOff < -focBound) {
        *focOff = -focBound;
    }
}

void focus_on_mario(Vec3f focus, Vec3f pos, f32 posYOff, f32 focYOff, f32 dist, s16 pitch, s16 yaw) {
    Vec3f marioPos;

    marioPos[0] = sMarioCamState->pos[0];
    marioPos[1] = sMarioCamState->pos[1] + posYOff;
    marioPos[2] = sMarioCamState->pos[2];

    vec3f_set_dist_and_angle(marioPos, pos, dist, pitch + sLakituPitch, yaw);

    focus[0] = sMarioCamState->pos[0];
    focus[1] = sMarioCamState->pos[1] + focYOff;
    focus[2] = sMarioCamState->pos[2];
}

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

#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
s16 find_in_bounds_yaw_wdw_bob_thi(Vec3f pos, Vec3f origin, s16 yaw) {
    switch (gCurrLevelArea) {
        case AREA_WDW_MAIN:
            yaw = clamp_positions_and_find_yaw(pos, origin, 4508.f, -3739.f, 4508.f, -3739.f);
            break;
        case AREA_BOB:
            yaw = clamp_positions_and_find_yaw(pos, origin, 8000.f, -8000.f, 7050.f, -8000.f);
            break;
        case AREA_THI_HUGE:
            yaw = clamp_positions_and_find_yaw(pos, origin, 8192.f, -8192.f, 8192.f, -8192.f);
            break;
        case AREA_THI_TINY:
            yaw = clamp_positions_and_find_yaw(pos, origin, 2458.f, -2458.f, 2458.f, -2458.f);
            break;
    }
    return yaw;
}
#endif

/**
 * Rotates the camera around the area's center point.
 */
s32 update_radial_camera(struct Camera *c, Vec3f focus, Vec3f pos) {
    f32 cenDistX = sMarioCamState->pos[0] - c->areaCenX;
    f32 cenDistZ = sMarioCamState->pos[2] - c->areaCenZ;
    s16 camYaw = atan2s(cenDistZ, cenDistX) + sModeOffsetYaw;
    s16 pitch = look_down_slopes(camYaw);
    f32 posY, focusY;
    f32 yOff = 125.f;
    f32 baseDist = 1000.f;

    sAreaYaw = camYaw - sModeOffsetYaw;
    calc_y_to_curr_floor(&posY, 1.f, 200.f, &focusY, 0.9f, 200.f);
    focus_on_mario(focus, pos, posY + yOff, focusY + yOff, sLakituDist + baseDist, pitch, camYaw);
#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
    camYaw = find_in_bounds_yaw_wdw_bob_thi(pos, focus, camYaw);
#endif
    return camYaw;
}

/**
 * Update the camera during 8 directional mode
 */
s32 update_8_directions_camera(struct Camera *c, Vec3f focus, Vec3f pos) {
    s16 camYaw = s8DirModeBaseYaw + s8DirModeYawOffset;
    s16 pitch = look_down_slopes(camYaw);
    f32 posY;
    f32 focusY;
    f32 yOff = 125.f;
    f32 baseDist = 1000.f;

    sAreaYaw = camYaw;
    calc_y_to_curr_floor(&posY, 1.f, 200.f, &focusY, 0.9f, 200.f);
    focus_on_mario(focus, pos, posY + yOff, focusY + yOff, sLakituDist + baseDist, pitch, camYaw);
    pan_ahead_of_player(c);
#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
    if (gCurrLevelArea == AREA_DDD_SUB) {
        camYaw = clamp_positions_and_find_yaw(pos, focus, 6839.f, 995.f, 5994.f, -3945.f);
    }
#endif
    return camYaw;
}

/**
 * Moves the camera for the radial and outward radial camera modes.
 *
 * If sModeOffsetYaw is 0, the camera points directly at the area center point.
 */
void radial_camera_move(struct Camera *c) {
    s16 maxAreaYaw = DEGREES(60);
    s16 minAreaYaw = DEGREES(-60);
    s16 rotateSpeed = 0x1000;
    s16 avoidYaw;
    f32 areaDistX = sMarioCamState->pos[0] - c->areaCenX;
    f32 areaDistZ = sMarioCamState->pos[2] - c->areaCenZ;

    // How much the camera's yaw changed
    s16 yawOffset = calculate_yaw(sMarioCamState->pos, c->pos) - atan2s(areaDistZ, areaDistX);

    if (yawOffset > maxAreaYaw) {
        yawOffset = maxAreaYaw;
    }
    if (yawOffset < minAreaYaw) {
        yawOffset = minAreaYaw;
    }

    // Check if Mario stepped on a surface that rotates the camera. For example, when Mario enters the
    // gate in BoB, the camera turns right to face up the hill path
    if (!(gCameraMovementFlags & CAM_MOVE_ROTATE)) {
        if (sMarioGeometry.currFloorType == SURFACE_CAMERA_MIDDLE
            && sMarioGeometry.prevFloorType != SURFACE_CAMERA_MIDDLE) {
            gCameraMovementFlags |= (CAM_MOVE_RETURN_TO_MIDDLE | CAM_MOVE_ENTERED_ROTATE_SURFACE);
        }
        if (sMarioGeometry.currFloorType == SURFACE_CAMERA_ROTATE_RIGHT
            && sMarioGeometry.prevFloorType != SURFACE_CAMERA_ROTATE_RIGHT) {
            gCameraMovementFlags |= (CAM_MOVE_ROTATE_RIGHT | CAM_MOVE_ENTERED_ROTATE_SURFACE);
        }
        if (sMarioGeometry.currFloorType == SURFACE_CAMERA_ROTATE_LEFT
            && sMarioGeometry.prevFloorType != SURFACE_CAMERA_ROTATE_LEFT) {
            gCameraMovementFlags |= (CAM_MOVE_ROTATE_LEFT | CAM_MOVE_ENTERED_ROTATE_SURFACE);
        }
    }

    if (gCameraMovementFlags & CAM_MOVE_ENTERED_ROTATE_SURFACE) {
        rotateSpeed = 0x200;
    }

    if (c->mode == CAMERA_MODE_OUTWARD_RADIAL) {
        areaDistX = -areaDistX;
        areaDistZ = -areaDistZ;
    }

    // Avoid obstructing walls
    s32 avoidStatus = rotate_camera_around_walls(c, c->pos, &avoidYaw, 0x400);
    if (avoidStatus == AVOID_STATUS_WALL_COVERING_MARIO) {
        if (avoidYaw - atan2s(areaDistZ, areaDistX) + DEGREES(90) < 0) {
            avoidYaw += DEGREES(180);
        }

        // We want to change sModeOffsetYaw so that the player is no longer obstructed by the wall.
        // So, we make avoidYaw relative to the yaw around the area center
        avoidYaw -= atan2s(areaDistZ, areaDistX);

        // Bound avoid yaw to radial mode constraints
        if (avoidYaw > DEGREES(105)) {
            avoidYaw = DEGREES(105);
        }
        if (avoidYaw < DEGREES(-105)) {
            avoidYaw = DEGREES(-105);
        }
    }

    if (gCameraMovementFlags & CAM_MOVE_RETURN_TO_MIDDLE) {
        if (camera_approach_s16_symmetric_bool(&sModeOffsetYaw, 0, rotateSpeed) == 0) {
            gCameraMovementFlags &= ~CAM_MOVE_RETURN_TO_MIDDLE;
        }
    } else {
        // Prevent the player from rotating into obstructing walls
        if ((gCameraMovementFlags & CAM_MOVE_ROTATE_RIGHT) && avoidStatus == 3
            && avoidYaw + 0x10 < sModeOffsetYaw) {
            sModeOffsetYaw = avoidYaw;
            gCameraMovementFlags &= ~(CAM_MOVE_ROTATE_RIGHT | CAM_MOVE_ENTERED_ROTATE_SURFACE);
        }
        if ((gCameraMovementFlags & CAM_MOVE_ROTATE_LEFT) && avoidStatus == 3
            && avoidYaw - 0x10 > sModeOffsetYaw) {
            sModeOffsetYaw = avoidYaw;
            gCameraMovementFlags &= ~(CAM_MOVE_ROTATE_LEFT | CAM_MOVE_ENTERED_ROTATE_SURFACE);
        }

        // If it's the first time rotating, just rotate to +-60 degrees
        if (!(s2ndRotateFlags & CAM_MOVE_ROTATE_RIGHT) && (gCameraMovementFlags & CAM_MOVE_ROTATE_RIGHT)
            && camera_approach_s16_symmetric_bool(&sModeOffsetYaw, maxAreaYaw, rotateSpeed) == 0) {
            gCameraMovementFlags &= ~(CAM_MOVE_ROTATE_RIGHT | CAM_MOVE_ENTERED_ROTATE_SURFACE);
        }
        if (!(s2ndRotateFlags & CAM_MOVE_ROTATE_LEFT) && (gCameraMovementFlags & CAM_MOVE_ROTATE_LEFT)
            && camera_approach_s16_symmetric_bool(&sModeOffsetYaw, minAreaYaw, rotateSpeed) == 0) {
            gCameraMovementFlags &= ~(CAM_MOVE_ROTATE_LEFT | CAM_MOVE_ENTERED_ROTATE_SURFACE);
        }

        // If it's the second time rotating, rotate all the way to +-105 degrees.
        if ((s2ndRotateFlags & CAM_MOVE_ROTATE_RIGHT) && (gCameraMovementFlags & CAM_MOVE_ROTATE_RIGHT)
            && camera_approach_s16_symmetric_bool(&sModeOffsetYaw, DEGREES(105), rotateSpeed) == 0) {
            gCameraMovementFlags &= ~(CAM_MOVE_ROTATE_RIGHT | CAM_MOVE_ENTERED_ROTATE_SURFACE);
            s2ndRotateFlags &= ~CAM_MOVE_ROTATE_RIGHT;
        }
        if ((s2ndRotateFlags & CAM_MOVE_ROTATE_LEFT) && (gCameraMovementFlags & CAM_MOVE_ROTATE_LEFT)
            && camera_approach_s16_symmetric_bool(&sModeOffsetYaw, DEGREES(-105), rotateSpeed) == 0) {
            gCameraMovementFlags &= ~(CAM_MOVE_ROTATE_LEFT | CAM_MOVE_ENTERED_ROTATE_SURFACE);
            s2ndRotateFlags &= ~CAM_MOVE_ROTATE_LEFT;
        }
    }
    if (!(gCameraMovementFlags & CAM_MOVE_ROTATE)) {
        // If not rotating, rotate away from walls obscuring Mario from view
        if (avoidStatus == 3) {
            approach_s16_asymptotic_bool(&sModeOffsetYaw, avoidYaw, 10);
        } else {
            if (c->mode == CAMERA_MODE_RADIAL) {
                // sModeOffsetYaw only updates when Mario is moving
                rotateSpeed = gMarioStates[0].forwardVel / 32.f * 128.f;
                camera_approach_s16_symmetric_bool(&sModeOffsetYaw, yawOffset, rotateSpeed);
            }
            if (c->mode == CAMERA_MODE_OUTWARD_RADIAL) {
                sModeOffsetYaw = offset_yaw_outward_radial(c, atan2s(areaDistZ, areaDistX));
            }
        }
    }

    // Bound sModeOffsetYaw within (-120, 120) degrees
    if (sModeOffsetYaw > 0x5554) {
        sModeOffsetYaw = 0x5554;
    }
    if (sModeOffsetYaw < -0x5554) {
        sModeOffsetYaw = -0x5554;
    }
}

/**
 * Moves Lakitu from zoomed in to zoomed out and vice versa.
 * When C-Down mode is not active, sLakituDist and sLakituPitch decrease to 0.
 */
void lakitu_zoom(f32 rangeDist, s16 rangePitch) {
    if (sLakituDist < 0) {
        if ((sLakituDist += 30) > 0) {
            sLakituDist = 0;
        }
    } else if (rangeDist < sLakituDist) {
        if ((sLakituDist -= 30) < rangeDist) {
            sLakituDist = rangeDist;
        }
    } else if (gCameraMovementFlags & CAM_MOVE_ZOOMED_OUT) {
        if ((sLakituDist += 30) > rangeDist) {
            sLakituDist = rangeDist;
        }
    } else {
        if ((sLakituDist -= 30) < 0) {
            sLakituDist = 0;
        }
    }

    if (gCurrLevelArea == AREA_SSL_PYRAMID && gCamera->mode == CAMERA_MODE_OUTWARD_RADIAL) {
        rangePitch /= 2;
    }

    if (gCameraMovementFlags & CAM_MOVE_ZOOMED_OUT) {
        if ((sLakituPitch += rangePitch / 13) > rangePitch) {
            sLakituPitch = rangePitch;
        }
    } else {
        if ((sLakituPitch -= rangePitch / 13) < 0) {
            sLakituPitch = 0;
        }
    }
}

void radial_camera_input_default(struct Camera *c) {
    radial_camera_input(c);
}

/**
 * Makes Lakitu cam's yaw match the angle turned towards in C-Up mode, and makes Lakitu slowly fly back
 * to the distance he was at before C-Up
 */
void update_yaw_and_dist_from_c_up(UNUSED struct Camera *c) {
    sModeOffsetYaw = sModeInfo.transitionStart.yaw - sAreaYaw;
    sLakituDist = sModeInfo.transitionStart.dist - 1000.0f;
    // No longer in C-Up
    gCameraMovementFlags &= ~CAM_MOVING_INTO_MODE;
}

/**
 * Handles input and updates for the radial camera mode
 */
void mode_radial_camera(struct Camera *c) {
    Vec3f pos;
    s16 oldAreaYaw = sAreaYaw;

    if (gCameraMovementFlags & CAM_MOVING_INTO_MODE) {
        update_yaw_and_dist_from_c_up(c);
    }

    radial_camera_input_default(c);
    radial_camera_move(c);

    if (c->mode == CAMERA_MODE_RADIAL) {
        lakitu_zoom(400.f, 0x900);
    }
    c->nextYaw = update_radial_camera(c, c->focus, pos);
    c->pos[0] = pos[0];
    c->pos[2] = pos[2];
    sAreaYawChange = sAreaYaw - oldAreaYaw;
    if (sMarioCamState->action == ACT_RIDING_HOOT) {
        pos[1] += 500.f;
    }
    set_camera_height(c, pos[1]);
    pan_ahead_of_player(c);
}

s32 snap_to_45_degrees(s16 angle) {
    if (angle % DEGREES(45)) {
        s16 d1 = ABS(angle) % DEGREES(45);
        s16 d2 = DEGREES(45) - d1;
        if (angle > 0) {
            if (d1 < d2) return angle - d1;
            else return angle + d2;
        } else {
            if (d1 < d2) return angle + d1;
            else return angle - d2;
        }
    }
    return angle;
}

/**
 * A mode that only has 8 camera angles, 45 degrees apart
 */
void mode_8_directions_camera(struct Camera *c) {
    Vec3f pos;
    s16 oldAreaYaw = sAreaYaw;

    radial_camera_input(c);

    if (gPlayer1Controller->buttonPressed & R_CBUTTONS) {
        s8DirModeYawOffset += DEGREES(45);
        play_sound_cbutton_side();
    }
    if (gPlayer1Controller->buttonPressed & L_CBUTTONS) {
        s8DirModeYawOffset -= DEGREES(45);
        play_sound_cbutton_side();
    }
#ifdef PARALLEL_LAKITU_CAM
    // extra functionality
    else if (gPlayer1Controller->buttonPressed & U_JPAD) {
        s8DirModeYawOffset = 0;
        s8DirModeYawOffset = gMarioState->faceAngle[1] - 0x8000;
    }
    else if (gPlayer1Controller->buttonDown & L_JPAD) {
        s8DirModeYawOffset -= DEGREES(2);
    }
    else if (gPlayer1Controller->buttonDown & R_JPAD) {
        s8DirModeYawOffset += DEGREES(2);
    }
    else if (gPlayer1Controller->buttonPressed & D_JPAD) {
        s8DirModeYawOffset = snap_to_45_degrees(s8DirModeYawOffset);
    }
#endif

    lakitu_zoom(400.f, 0x900);
    c->nextYaw = update_8_directions_camera(c, c->focus, pos);
    c->pos[0] = pos[0];
    c->pos[2] = pos[2];
    sAreaYawChange = sAreaYaw - oldAreaYaw;
    set_camera_height(c, pos[1]);
}

/**
 * Updates the camera in outward radial mode.
 * sModeOffsetYaw is calculated in radial_camera_move, which calls offset_yaw_outward_radial
 */
s32 update_outward_radial_camera(struct Camera *c, Vec3f focus, Vec3f pos) {
    f32 xDistFocToMario = sMarioCamState->pos[0] - c->areaCenX;
    f32 zDistFocToMario = sMarioCamState->pos[2] - c->areaCenZ;
    s16 camYaw = atan2s(zDistFocToMario, xDistFocToMario) + sModeOffsetYaw + DEGREES(180);
    s16 pitch = look_down_slopes(camYaw);
    f32 baseDist = 1000.f;
    // A base offset of 125.f is ~= Mario's eye height
    f32 yOff = 125.f;
    f32 posY;
    f32 focusY;

    sAreaYaw = camYaw - sModeOffsetYaw - DEGREES(180);
    calc_y_to_curr_floor(&posY, 1.f, 200.f, &focusY, 0.9f, 200.f);
    focus_on_mario(focus, pos, posY + yOff, focusY + yOff, sLakituDist + baseDist, pitch, camYaw);

    return camYaw;
}

/**
 * Input and updates for the outward radial mode.
 */
void mode_outward_radial_camera(struct Camera *c) {
    Vec3f pos;
    s16 oldAreaYaw = sAreaYaw;

    if (gCameraMovementFlags & CAM_MOVING_INTO_MODE) {
        update_yaw_and_dist_from_c_up(c);
    }
    radial_camera_input_default(c);
    radial_camera_move(c);
    lakitu_zoom(400.f, 0x900);
    c->nextYaw = update_outward_radial_camera(c, c->focus, pos);
    c->pos[0] = pos[0];
    c->pos[2] = pos[2];
    sAreaYawChange = sAreaYaw - oldAreaYaw;
    if (sMarioCamState->action == ACT_RIDING_HOOT) {
        pos[1] += 500.f;
    }
    set_camera_height(c, pos[1]);
    pan_ahead_of_player(c);
}

/**
 * Move the camera in parallel tracking mode
 *
 * Uses the line between the next two points in sParTrackPath
 * The camera can move forward/back and side to side, but it will face perpendicular to that line
 *
 * Although, annoyingly, it's not truly parallel, the function returns the yaw from the camera to Mario,
 * so Mario will run slightly towards the camera.
 */
s32 update_parallel_tracking_camera(struct Camera *c, Vec3f focus, Vec3f pos) {
    Vec3f path[2];
    Vec3f parMidPoint;
    Vec3f marioOffset;
    Vec3f camOffset;
    /// Adjusts the focus to look where Mario is facing. Unused since marioOffset is copied to focus
    Vec3f focOffset = { 0 };
    s16 pathPitch;
    s16 pathYaw;
    f32 camParDist;
    f32 pathLength;
    f32 parScale = 0.5f;
    f32 marioFloorDist;
    Vec3f marioPos;
    Vec3s pathAngle;
    // Variables for changing to the next/prev path in the list
    Vec3f oldPos;
    Vec3f prevPathPos;
    Vec3f nextPathPos;
    f32 distToNext, distToPrev;
    s16 prevPitch, nextPitch;
    s16 prevYaw, nextYaw;

    // Store camera pos, for changing between paths
    vec3f_copy(oldPos, pos);

    vec3f_copy(path[0], sParTrackPath[sParTrackIndex].pos);
    vec3f_copy(path[1], sParTrackPath[sParTrackIndex + 1].pos);

    f32 distThresh = sParTrackPath[sParTrackIndex].distThresh;
    f32 zoom = sParTrackPath[sParTrackIndex].zoom;
    calc_y_to_curr_floor(&marioFloorDist, 1.f, 200.f, &marioFloorDist, 0.9f, 200.f);

    marioPos[0] = sMarioCamState->pos[0];
    // Mario's y pos + ~Mario's height + Mario's height above the floor
    marioPos[1] = sMarioCamState->pos[1] + 150.f + marioFloorDist;
    marioPos[2] = sMarioCamState->pos[2];

    // Calculate middle of the path (parScale is 0.5f)
    parMidPoint[0] = path[0][0] + (path[1][0] - path[0][0]) * parScale;
    parMidPoint[1] = path[0][1] + (path[1][1] - path[0][1]) * parScale;
    parMidPoint[2] = path[0][2] + (path[1][2] - path[0][2]) * parScale;

    // Get direction of path
    vec3f_get_dist_and_angle(path[0], path[1], &pathLength, &pathPitch, &pathYaw);

    marioOffset[0] = marioPos[0] - parMidPoint[0];
    marioOffset[1] = marioPos[1] - parMidPoint[1];
    marioOffset[2] = marioPos[2] - parMidPoint[2];

    // Make marioOffset point from the midpoint -> the start of the path
    // Rotating by -yaw then -pitch moves the hor dist from the midpoint into marioOffset's z coordinate
    // marioOffset[0] = the (perpendicular) horizontal distance from the path
    // marioOffset[1] = the vertical distance from the path
    // marioOffset[2] = the (parallel) horizontal distance from the path's midpoint
    pathYaw = -pathYaw;
    rotate_in_xz(marioOffset, marioOffset, pathYaw);
    pathYaw = -pathYaw;
    pathPitch = -pathPitch;
    rotate_in_yz(marioOffset, marioOffset, pathPitch);
    pathPitch = -pathPitch;
    focOffset[2] = marioOffset[2];

    // Repeat above calcs with camOffset
    camOffset[0] = pos[0] - parMidPoint[0];
    camOffset[1] = pos[1] - parMidPoint[1];
    camOffset[2] = pos[2] - parMidPoint[2];
    pathYaw = -pathYaw;
    rotate_in_xz(camOffset, camOffset, pathYaw);
    pathYaw = -pathYaw;
    pathPitch = -pathPitch;
    rotate_in_yz(camOffset, camOffset, pathPitch);
    pathPitch = -pathPitch;

    // If Mario is distThresh units away from the camera along the path, move the camera
    //! When distThresh != 0, it causes Mario to move slightly towards the camera when running sideways
    //! Set each ParallelTrackingPoint's distThresh to 0 to make Mario truly run parallel to the path
    if (marioOffset[2] > camOffset[2]) {
        if (marioOffset[2] - camOffset[2] > distThresh) {
            camOffset[2] = marioOffset[2] - distThresh;
        }
    } else {
        if (marioOffset[2] - camOffset[2] < -distThresh) {
            camOffset[2] = marioOffset[2] + distThresh;
        }
    }

    // If zoom != 0.0, the camera will move zoom% closer to Mario
    marioOffset[0] = -marioOffset[0] * zoom;
    marioOffset[1] = marioOffset[1] * zoom;
    marioOffset[2] = camOffset[2];

    // make marioOffset[2] == distance from the start of the path
    marioOffset[2] = pathLength / 2 - marioOffset[2];

    pathAngle[0] = pathPitch;
    pathAngle[1] = pathYaw + DEGREES(180);
    pathAngle[2] = 0;

    // Rotate the offset in the direction of the path again
    offset_rotated(pos, path[0], marioOffset, pathAngle);
    vec3f_get_dist_and_angle(path[0], c->pos, &camParDist, &pathPitch, &pathYaw);

    // Adjust the focus. Does nothing, focus is set to Mario at the end
    focOffset[2] = pathLength / 2 - focOffset[2];
    offset_rotated(c->focus, path[0], focOffset, pathAngle);

    // Changing paths, update the stored position offset
    if (sStatusFlags & CAM_FLAG_CHANGED_PARTRACK_INDEX) {
        sStatusFlags &= ~CAM_FLAG_CHANGED_PARTRACK_INDEX;
        sParTrackTransOff.pos[0] = oldPos[0] - c->pos[0];
        sParTrackTransOff.pos[1] = oldPos[1] - c->pos[1];
        sParTrackTransOff.pos[2] = oldPos[2] - c->pos[2];
    }
    // Slowly transition to the next path
    approach_f32_asymptotic_bool(&sParTrackTransOff.pos[0], 0.f, 0.025f);
    approach_f32_asymptotic_bool(&sParTrackTransOff.pos[1], 0.f, 0.025f);
    approach_f32_asymptotic_bool(&sParTrackTransOff.pos[2], 0.f, 0.025f);
    vec3f_add(c->pos, sParTrackTransOff.pos);

    // Check if the camera should go to the next path
    if (sParTrackPath[sParTrackIndex + 1].startOfPath != 0) {
        // get Mario's distance to the next path
        calculate_angles(sParTrackPath[sParTrackIndex + 1].pos, sParTrackPath[sParTrackIndex + 2].pos, &nextPitch, &nextYaw);
        vec3f_set_dist_and_angle(sParTrackPath[sParTrackIndex + 1].pos, nextPathPos, 400.f, nextPitch, nextYaw);
        distToPrev = calc_abs_dist_squared(marioPos, nextPathPos);

        // get Mario's distance to the previous path
        calculate_angles(sParTrackPath[sParTrackIndex + 1].pos, sParTrackPath[sParTrackIndex].pos, &prevPitch, &prevYaw);
        vec3f_set_dist_and_angle(sParTrackPath[sParTrackIndex + 1].pos, prevPathPos, 400.f, prevPitch, prevYaw);
        distToNext = calc_abs_dist_squared(marioPos, prevPathPos);
        if (distToPrev < distToNext) {
            sParTrackIndex++;
            sStatusFlags |= CAM_FLAG_CHANGED_PARTRACK_INDEX;
        }
    }

    // Check if the camera should go to the previous path
    if (sParTrackIndex != 0) {
        // get Mario's distance to the next path
        calculate_angles((*(sParTrackPath + sParTrackIndex)).pos, (*(sParTrackPath + sParTrackIndex + 1)).pos, &nextPitch, &nextYaw);
        vec3f_set_dist_and_angle(sParTrackPath[sParTrackIndex].pos, nextPathPos, 700.f, nextPitch, nextYaw);
        distToPrev = calc_abs_dist_squared(marioPos, nextPathPos);

        // get Mario's distance to the previous path
        calculate_angles((*(sParTrackPath + sParTrackIndex)).pos, (*(sParTrackPath + sParTrackIndex - 1)).pos, &prevPitch, &prevYaw);
        vec3f_set_dist_and_angle(sParTrackPath[sParTrackIndex].pos, prevPathPos, 700.f, prevPitch, prevYaw);
        distToNext = calc_abs_dist_squared(marioPos, prevPathPos);
        if (distToPrev > distToNext) {
            sParTrackIndex--;
            sStatusFlags |= CAM_FLAG_CHANGED_PARTRACK_INDEX;
        }
    }

    // Update the camera focus and return the camera's yaw
    vec3f_copy(focus, marioPos);
    vec3f_get_dist_and_angle(focus, pos, &camParDist, &pathPitch, &pathYaw);
    return pathYaw;
}

/**
 * Updates the camera during fixed mode.
 */
s32 update_fixed_camera(struct Camera *c, Vec3f focus, UNUSED Vec3f pos) {
    f32 focusFloorOff;
    f32 goalHeight;
    f32 ceilHeight;
    f32 heightOffset;
    f32 distCamToFocus;
    f32 scaleToMario = 0.5f;
    s16 pitch;
    s16 yaw;
    Vec3s faceAngle;
    struct Surface *ceiling;
    Vec3f basePos;

    play_camera_buzz_if_c_sideways();

    // Don't move closer to Mario in these areas
    switch (gCurrLevelArea) {
        case AREA_RR:
            scaleToMario = 0.f;
            heightOffset = 0.f;
            break;

        case AREA_CASTLE_LOBBY:
            scaleToMario = 0.3f;
            heightOffset = 0.f;
            break;

        case AREA_BBH:
            scaleToMario = 0.f;
            heightOffset = 0.f;
            break;
    }

    handle_c_button_movement(c);
    play_camera_buzz_if_cdown();

    calc_y_to_curr_floor(&focusFloorOff, 1.f, 200.f, &focusFloorOff, 0.9f, 200.f);
    vec3f_copy(focus, sMarioCamState->pos);
    focus[1] += focusFloorOff + 125.f;
    vec3f_get_dist_and_angle(focus, c->pos, &distCamToFocus, &faceAngle[0], &faceAngle[1]);
    faceAngle[2] = 0;

    vec3f_copy(basePos, sFixedModeBasePosition);
    vec3f_add(basePos, sCastleEntranceOffset);

    if (sMarioGeometry.currFloorType != SURFACE_DEATH_PLANE
        && sMarioGeometry.currFloorHeight != FLOOR_LOWER_LIMIT) {
        goalHeight = sMarioGeometry.currFloorHeight + basePos[1] + heightOffset;
    } else {
        goalHeight = gLakituState.goalPos[1];
    }

    if (300 > distCamToFocus) {
        goalHeight += 300 - distCamToFocus;
    }

    ceilHeight = find_ceil(c->pos[0], goalHeight - 100.f, c->pos[2], &ceiling);
    if (ceilHeight != CELL_HEIGHT_LIMIT) {
        if (goalHeight > (ceilHeight -= 125.f)) {
            goalHeight = ceilHeight;
        }
    }

    if (sStatusFlags & CAM_FLAG_SMOOTH_MOVEMENT) {
        camera_approach_f32_symmetric_bool(&c->pos[1], goalHeight, 15.f);
    } else {
        if (goalHeight < sMarioCamState->pos[1] - 500.f) {
            goalHeight = sMarioCamState->pos[1] - 500.f;
        }
        c->pos[1] = goalHeight;
    }

    c->pos[0] = basePos[0] + (sMarioCamState->pos[0] - basePos[0]) * scaleToMario;
    c->pos[2] = basePos[2] + (sMarioCamState->pos[2] - basePos[2]) * scaleToMario;

    if (scaleToMario != 0.f) {
        vec3f_get_dist_and_angle(c->focus, c->pos, &distCamToFocus, &pitch, &yaw);
        if (distCamToFocus > 1000.f) {
            distCamToFocus = 1000.f;
            vec3f_set_dist_and_angle(c->focus, c->pos, distCamToFocus, pitch, yaw);
        }
    }

    return faceAngle[1];
}

/**
 * Updates the camera during a boss fight
 */
s32 update_boss_fight_camera(struct Camera *c, Vec3f focus, Vec3f pos) {
    struct Object *obj;
    f32 focusDistance;
    // Floor normal values
    f32 nx;
    f32 ny;
    f32 nz;
    /// Floor originOffset
    f32 oo;
    s16 yaw;
    s16 heldState;
    struct Surface *floor;
    Vec3f secondFocus;
    Vec3f holdFocOffset = { 0.f, -150.f, -125.f };

    handle_c_button_movement(c);

    // Start camera shakes if bowser jumps or gets thrown.
    if (sMarioCamState->cameraEvent == CAM_EVENT_BOWSER_JUMP) {
        set_environmental_camera_shake(SHAKE_ENV_BOWSER_JUMP);
        sMarioCamState->cameraEvent = CAM_EVENT_NONE;
    }
    if (sMarioCamState->cameraEvent == CAM_EVENT_BOWSER_THROW_BOUNCE) {
        set_environmental_camera_shake(SHAKE_ENV_BOWSER_THROW_BOUNCE);
        sMarioCamState->cameraEvent = CAM_EVENT_NONE;
    }

    yaw = sModeOffsetYaw + DEGREES(45);
    // Get boss's position and whether Mario is holding it.
    if ((obj = gSecondCameraFocus) != NULL) {
        vec3f_copy(secondFocus, &obj->oPosVec);
        heldState = obj->oHeldState;
    } else {
    // If no boss is there, just rotate around the area's center point.
        secondFocus[0] = c->areaCenX;
        secondFocus[1] = sMarioCamState->pos[1];
        secondFocus[2] = c->areaCenZ;
        heldState = 0;
    }

    focusDistance = calc_abs_dist(sMarioCamState->pos, secondFocus) * 1.6f;
    if (focusDistance < 800.f) {
        focusDistance = 800.f;
    }
    if (focusDistance > 5000.f) {
        focusDistance = 5000.f;
    }

    // If holding the boss, add a slight offset to secondFocus so that the spinning is more pronounced.
    if (heldState == HELD_HELD) {
        offset_rotated(secondFocus, sMarioCamState->pos, holdFocOffset, sMarioCamState->faceAngle);
    }

    // Set the camera focus to the average of Mario and secondFocus
    focus[0] = (sMarioCamState->pos[0] + secondFocus[0]) / 2.f;
    focus[1] = (sMarioCamState->pos[1] + secondFocus[1]) / 2.f + 125.f;
    focus[2] = (sMarioCamState->pos[2] + secondFocus[2]) / 2.f;

    // Calculate the camera's position as an offset from the focus
    // When C-Down is not active, this
    vec3f_set_dist_and_angle(focus, pos, focusDistance, 0x1000, yaw);
    // Find the floor of the arena
    pos[1] = find_floor(c->areaCenX, CELL_HEIGHT_LIMIT, c->areaCenZ, &floor);
    if (floor != NULL) {
        nx = floor->normal.x;
        ny = floor->normal.y;
        nz = floor->normal.z;
        oo = floor->originOffset;
        pos[1] = 300.f - (nx * pos[0] + nz * pos[2] + oo) / ny;
#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
        switch (gCurrLevelArea) {
            case AREA_BOB:
                pos[1] += 125.f;
                //! fall through, makes the BoB boss fight camera move up twice as high as it should
                FALL_THROUGH;
            case AREA_WF:
                pos[1] += 125.f;
        }
#endif
    }
#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
    // Prevent the camera from going to the ground in the outside boss fight
    if (gCurrLevelNum == LEVEL_BBH) {
        pos[1] = 2047.f;
    }
#endif
    // Rotate from C-Button input
    if (sCSideButtonYaw < 0) {
        sModeOffsetYaw += 0x200;
        if ((sCSideButtonYaw += 0x100) > 0) {
            sCSideButtonYaw = 0;
        }
    }
    if (sCSideButtonYaw > 0) {
        sModeOffsetYaw -= 0x200;
        if ((sCSideButtonYaw -= 0x100) < 0) {
            sCSideButtonYaw = 0;
        }
    }

    focus[1] = (sMarioCamState->pos[1] + secondFocus[1]) / 2.f + 100.f;
    if (heldState == 1) {
        focus[1] += 300.f * sins((gMarioStates[0].angleVel[1] > 0.f) ?  gMarioStates[0].angleVel[1]
                                                                     : -gMarioStates[0].angleVel[1]);
    }

    //! Unnecessary conditional, focusDistance is already bounded to 800
    // if (focusDistance < 400.f) {
    //     focusDistance = 400.f;
    // }

    // Set C-Down distance and pitch.
    // C-Down will essentially double the distance from the center.
    // sLakituPitch approaches 33.75 degrees.
    lakitu_zoom(focusDistance, 0x1800);

    // Move the camera position back as sLakituDist and sLakituPitch increase.
    // This doesn't zoom out of bounds because pos is set above each frame.
    // The constant 0x1000 doubles the pitch from the center when sLakituPitch is 0
    // When Lakitu is fully zoomed out, the pitch comes to 0x3800, or 78.75 degrees, up from the focus.
    vec3f_set_dist_and_angle(pos, pos, sLakituDist, sLakituPitch + 0x1000, yaw);

    return yaw;
}

/**
 * Maps cutscene to numbers in [0,4]. Used in determine_dance_cutscene() with sDanceCutsceneIndexTable.
 *
 * Only the first 5 entries are used. Perhaps the last 5 were bools used to indicate whether the star
 * type exits the course or not.
 */
u8 sDanceCutsceneTable[] = {
    CUTSCENE_DANCE_FLY_AWAY, CUTSCENE_DANCE_ROTATE, CUTSCENE_DANCE_CLOSEUP, CUTSCENE_KEY_DANCE, CUTSCENE_DANCE_DEFAULT,
    CUTSCENE_NONE,           CUTSCENE_NONE,         CUTSCENE_NONE,          CUTSCENE_NONE,      CUTSCENE_NONE,
};

/**
 * Table that dictates camera movement in bookend room.
 * Due to only the X being varied in the table, this only moves along the X axis linearly.
 * Third entry is seemingly unused.
 */
struct ParallelTrackingPoint sBBHLibraryParTrackPath[] = {
    { 1, { -929.0f, 1619.0f, -1490.0f }, 50.0f, 0.0f },
    { 0, { -2118.0f, 1619.0f, -1490.0f }, 50.0f, 0.0f },
    { 0, { 0.0f, 0.0f, 0.0f }, 0.0f, 0.0f },
};

s32 unused_update_mode_5_camera(UNUSED struct Camera *c, UNUSED Vec3f focus, UNUSED Vec3f pos) {
   return 0;
}

void mode_boss_fight_camera(struct Camera *c) {
    c->nextYaw = update_boss_fight_camera(c, c->focus, c->pos);
}

/**
 * Parallel tracking mode, the camera faces perpendicular to a line defined by sParTrackPath
 *
 * @see update_parallel_tracking_camera
 */
void mode_parallel_tracking_camera(struct Camera *c) {
    radial_camera_input(c);
    set_fov_function(CAM_FOV_DEFAULT);
    c->nextYaw = update_parallel_tracking_camera(c, c->focus, c->pos);
}

/**
 * Fixed camera mode, the camera rotates around a point and looks and zooms toward Mario.
 */
void mode_fixed_camera(struct Camera *c) {
#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
    if (gCurrLevelNum == LEVEL_BBH) {
        set_fov_function(CAM_FOV_BBH);
    } else {
        set_fov_function(CAM_FOV_APP_45);
    }
#else
    set_fov_function(CAM_FOV_APP_45);
#endif
    c->nextYaw = update_fixed_camera(c, c->focus, c->pos);
    c->yaw = c->nextYaw;
    pan_ahead_of_player(c);
    vec3_zero(sCastleEntranceOffset);
}

/**
 * Updates the camera in BEHIND_MARIO mode.
 *
 * The C-Buttons rotate the camera 90 degrees left/right and 67.5 degrees up/down.
 */
s32 update_behind_mario_camera(struct Camera *c, Vec3f focus, Vec3f pos) {
    f32 dist;
    s16 absPitch;
    s16 pitch;
    s16 yaw;
    s16 goalPitch = -sMarioCamState->faceAngle[0];
    s16 marioYaw = sMarioCamState->faceAngle[1] + DEGREES(180);
    s16 goalYawOff = 0;
    s16 yawSpeed;
    s16 pitchInc = 32;
    f32 maxDist = 800.f;
    f32 focYOff = 125.f;

    // Zoom in when Mario R_TRIG mode is active
    if (sSelectionFlags & CAM_MODE_MARIO_ACTIVE) {
        maxDist = 350.f;
        focYOff = 120.f;
    }
    if (!(sMarioCamState->action & (ACT_FLAG_SWIMMING | ACT_FLAG_METAL_WATER))) {
        pitchInc = 128;
    }

    // Focus on Mario
    vec3f_copy(focus, sMarioCamState->pos);
    c->focus[1] += focYOff;
    vec3f_get_dist_and_angle(focus, pos, &dist, &pitch, &yaw);
    if (dist > maxDist) {
        dist = maxDist;
    }
    if ((absPitch = pitch) < 0) {
        absPitch = -absPitch;
    }

    // Determine the yaw speed based on absPitch. A higher absPitch (further away from looking straight)
    // translates to a slower speed
    // Note: Pitch is always within +- 90 degrees or +-0x4000, and 0x4000 / 0x200 = 32
    yawSpeed = 32 - absPitch / 0x200;
    if (yawSpeed < 1) {
        yawSpeed = 1;
    }
    if (yawSpeed > 32) {
        yawSpeed = 32;
    }

    if (sCSideButtonYaw != 0) {
        camera_approach_s16_symmetric_bool(&sCSideButtonYaw, 0, 1);
        yawSpeed = 8;
    }
    if (sBehindMarioSoundTimer != 0) {
        goalPitch = 0;
        camera_approach_s16_symmetric_bool(&sBehindMarioSoundTimer, 0, 1);
        pitchInc = 0x800;
    }

    if (sBehindMarioSoundTimer == 28) {
        if (sCSideButtonYaw < 5 || sCSideButtonYaw > 28) {
            play_sound_cbutton_up();
        }
    }
    if (sCSideButtonYaw == 28) {
        if (sBehindMarioSoundTimer < 5 || sBehindMarioSoundTimer > 28) {
            play_sound_cbutton_up();
        }
    }

    // C-Button input. Note: Camera rotates in the opposite direction of the button (airplane controls)
    //! @bug C-Right and C-Up take precedence due to the way input is handled here

    // Rotate right
    if (sCButtonsPressed & L_CBUTTONS) {
        if (gPlayer1Controller->buttonPressed & L_CBUTTONS) {
            play_sound_cbutton_side();
        }
        if (dist < maxDist) {
            camera_approach_f32_symmetric_bool(&dist, maxDist, 5.f);
        }
        goalYawOff = -0x3FF8;
        sCSideButtonYaw = 30;
        yawSpeed = 2;
    }
    // Rotate left
    if (sCButtonsPressed & R_CBUTTONS) {
        if (gPlayer1Controller->buttonPressed & R_CBUTTONS) {
            play_sound_cbutton_side();
        }
        if (dist < maxDist) {
            camera_approach_f32_symmetric_bool(&dist, maxDist, 5.f);
        }
        goalYawOff = 0x3FF8;
        sCSideButtonYaw = 30;
        yawSpeed = 2;
    }
    // Rotate up
    if (sCButtonsPressed & D_CBUTTONS) {
        if (gPlayer1Controller->buttonPressed & (U_CBUTTONS | D_CBUTTONS)) {
            play_sound_cbutton_side();
        }
        if (dist < maxDist) {
            camera_approach_f32_symmetric_bool(&dist, maxDist, 5.f);
        }
        goalPitch = -0x3000;
        sBehindMarioSoundTimer = 30;
        pitchInc = 0x800;
    }
    // Rotate down
    if (sCButtonsPressed & U_CBUTTONS) {
        if (gPlayer1Controller->buttonPressed & (U_CBUTTONS | D_CBUTTONS)) {
            play_sound_cbutton_side();
        }
        if (dist < maxDist) {
            camera_approach_f32_symmetric_bool(&dist, maxDist, 5.f);
        }
        goalPitch = 0x3000;
        sBehindMarioSoundTimer = 30;
        pitchInc = 0x800;
    }

    approach_s16_asymptotic_bool(&yaw, marioYaw + goalYawOff, yawSpeed);
    camera_approach_s16_symmetric_bool(&pitch, goalPitch, pitchInc);
    if (dist < 300.f) {
        dist = 300.f;
    }
    vec3f_set_dist_and_angle(focus, pos, dist, pitch, yaw);
#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
    if (gCurrLevelArea == AREA_WDW_MAIN) {
        yaw = clamp_positions_and_find_yaw(pos, focus, 4508.f, -3739.f, 4508.f, -3739.f);
    }
    if (gCurrLevelArea == AREA_THI_HUGE) {
        yaw = clamp_positions_and_find_yaw(pos, focus, 8192.f, -8192.f, 8192.f, -8192.f);
    }
    if (gCurrLevelArea == AREA_THI_TINY) {
        yaw = clamp_positions_and_find_yaw(pos, focus, 2458.f, -2458.f, 2458.f, -2458.f);
    }
#endif

    return yaw;
}

/**
 * "Behind Mario" mode: used when Mario is flying, on the water's surface, or shot from a cannon
 */
s32 mode_behind_mario(struct Camera *c) {
    struct MarioState *marioState = &gMarioStates[0];
    struct Surface *floor;
    Vec3f newPos;
    f32 waterHeight;
    f32 floorHeight;
    f32 distCamToFocus;
    s16 camPitch;
    s16 camYaw;
    s16 yaw;

    gCameraMovementFlags &= ~CAM_MOVING_INTO_MODE;
    vec3f_copy(newPos, c->pos);
    yaw = update_behind_mario_camera(c, c->focus, newPos);
    c->pos[0] = newPos[0];
    c->pos[2] = newPos[2];

    // Keep the camera above the water surface if swimming
    if (c->mode == WATER_SURFACE_CAMERA_MODE) {
        floorHeight = find_floor(c->pos[0], c->pos[1], c->pos[2], &floor);
        newPos[1] = marioState->waterLevel + 120;
        if (newPos[1] < (floorHeight += 120.f)) {
            newPos[1] = floorHeight;
        }
    }
    approach_camera_height(c, newPos[1], 50.f);
    waterHeight = find_water_level(c->pos[0], c->pos[2]) + 100.f;
    if (c->pos[1] <= waterHeight) {
        gCameraMovementFlags |= CAM_MOVE_SUBMERGED;
    } else {
        gCameraMovementFlags &= ~CAM_MOVE_SUBMERGED;
    }

    resolve_geometry_collisions(c->pos);
    // Prevent camera getting too far away
    vec3f_get_dist_and_angle(c->focus, c->pos, &distCamToFocus, &camPitch, &camYaw);
    if (distCamToFocus > 800.f) {
        distCamToFocus = 800.f;
        vec3f_set_dist_and_angle(c->focus, c->pos, distCamToFocus, camPitch, camYaw);
    }
    pan_ahead_of_player(c);

    return yaw;
}

/**
 * Update the camera in slide and hoot mode.
 *
 * In slide mode, keep the camera 800 units from Mario
 */
s16 update_slide_camera(struct Camera *c) {
    struct Surface *floor;
    f32 floorHeight;
    Vec3f pos;
    f32 distCamToFocus;
    f32 maxCamDist;
    f32 pitchScale;
    s16 camPitch;
    s16 camYaw;
    s16 goalPitch = 0x1555;
    s16 goalYaw = sMarioCamState->faceAngle[1] + DEGREES(180);

    // Zoom in when inside the CCM shortcut
    if (sStatusFlags & CAM_FLAG_CCM_SLIDE_SHORTCUT) {
        sLakituDist = approach_f32(sLakituDist, -600.f, 20.f, 20.f);
    } else {
        sLakituDist = approach_f32(sLakituDist, 0.f, 20.f, 20.f);
    }

    // No C-Button input in this mode, notify the player with a buzzer
    play_camera_buzz_if_cbutton();

    // Focus on Mario
    vec3f_copy(c->focus, sMarioCamState->pos);
    c->focus[1] += 50.f;

    vec3f_get_dist_and_angle(c->focus, c->pos, &distCamToFocus, &camPitch, &camYaw);
    maxCamDist = 800.f;

    // In hoot mode, zoom further out and rotate faster
    if (sMarioCamState->action == ACT_RIDING_HOOT) {
        maxCamDist = 1000.f;
        goalPitch = 0x2800;
        camera_approach_s16_symmetric_bool(&camYaw, goalYaw, 0x100);
    } else {
        camera_approach_s16_symmetric_bool(&camYaw, goalYaw, 0x80);
    }
    camera_approach_s16_symmetric_bool(&camPitch, goalPitch, 0x100);

    // Hoot mode
    if (sMarioCamState->action != ACT_RIDING_HOOT && sMarioGeometry.currFloorType == SURFACE_DEATH_PLANE) {
        vec3f_set_dist_and_angle(c->focus, pos, maxCamDist + sLakituDist, camPitch, camYaw);
        c->pos[0] = pos[0];
        c->pos[2] = pos[2];
        camera_approach_f32_symmetric_bool(&c->pos[1], c->focus[1], 30.f);
        vec3f_get_dist_and_angle(c->pos, c->focus, &distCamToFocus, &camPitch, &camYaw);
        pitchScale = (distCamToFocus - maxCamDist + sLakituDist) / 10000.f;
        if (pitchScale > 1.f) {
            pitchScale = 1.f;
        }
        camPitch += 0x1000 * pitchScale;
        vec3f_set_dist_and_angle(c->pos, c->focus, distCamToFocus, camPitch, camYaw);

    // Slide mode
    } else {
        vec3f_set_dist_and_angle(c->focus, c->pos, maxCamDist + sLakituDist, camPitch, camYaw);
        sStatusFlags |= CAM_FLAG_BLOCK_SMOOTH_MOVEMENT;

        // Stay above the slide floor
        floorHeight = find_floor(c->pos[0], c->pos[1] + 200.f, c->pos[2], &floor) + 125.f;
        if (c->pos[1] < floorHeight) {
            c->pos[1] = floorHeight;
        }
        // Stay closer than maxCamDist
        vec3f_get_dist_and_angle(c->focus, c->pos, &distCamToFocus, &camPitch, &camYaw);
        if (distCamToFocus > maxCamDist + sLakituDist) {
            distCamToFocus = maxCamDist + sLakituDist;
            vec3f_set_dist_and_angle(c->focus, c->pos, distCamToFocus, camPitch, camYaw);
        }
    }

    camYaw = calculate_yaw(c->focus, c->pos);
    return camYaw;
}

void mode_behind_mario_camera(struct Camera *c) {
    c->nextYaw = mode_behind_mario(c);
}

s32 nop_update_water_camera(UNUSED struct Camera *c, UNUSED Vec3f focus, UNUSED Vec3f pos) {
   return 0;
}

/**
 * Exactly the same as BEHIND_MARIO
 */
void mode_water_surface_camera(struct Camera *c) {
    c->nextYaw = mode_behind_mario(c);
}

/**
 * Used in sModeTransitions for CLOSE and FREE_ROAM mode
 */
s32 update_mario_camera(UNUSED struct Camera *c, Vec3f focus, Vec3f pos) {
    s16 yaw = sMarioCamState->faceAngle[1] + sModeOffsetYaw + DEGREES(180);
    focus_on_mario(focus, pos, 125.f, 125.f, gCameraZoomDist, 0x05B0, yaw);

    return sMarioCamState->faceAngle[1];
}

/**
 * Update the camera in default, close, and free roam mode
 *
 * The camera moves behind Mario, and can rotate all the way around
 */
s16 update_default_camera(struct Camera *c) {
    Vec3f tempPos;
    Vec3f cPos;
    struct Surface *marioFloor;
    struct Surface *cFloor;
    struct Surface *tempFloor;
    struct Surface *ceil;
    f32 camFloorHeight;
    f32 tempFloorHeight;
    f32 marioFloorHeight;
    f32 dist;
    f32 zoomDist;
    f32 waterHeight;
    f32 gasHeight;
    s16 avoidYaw;
    s16 pitch;
    s16 yaw;
    s16 yawGoal = sMarioCamState->faceAngle[1] + DEGREES(180);
    f32 posHeight;
    f32 focHeight;
    f32 distFromWater;
    s16 tempPitch;
    s16 tempYaw;
    f32 xzDist;
    s16 nextYawVel;
    s16 yawVel = 0;
    f32 scale;
    s32 avoidStatus = 0;
    s32 closeToMario = FALSE;
    f32 ceilHeight = find_ceil(gLakituState.goalPos[0],
                               gLakituState.goalPos[1],
                               gLakituState.goalPos[2], &ceil);
    s16 yawDir;

    handle_c_button_movement(c);
    vec3f_get_dist_and_angle(sMarioCamState->pos, c->pos, &dist, &pitch, &yaw);

    // If C-Down is active, determine what distance the camera should be from Mario
    if (gCameraMovementFlags & CAM_MOVE_ZOOMED_OUT) {
        //! In Mario mode, the camera is zoomed out further than in Lakitu mode (1400 vs 1200)
        if (set_cam_angle(0) == CAM_ANGLE_MARIO) {
            zoomDist = gCameraZoomDist + 1050;
        } else {
            zoomDist = gCameraZoomDist + 400;
        }
    } else {
        zoomDist = gCameraZoomDist;
    }

    if (sMarioCamState->action & ACT_FLAG_HANGING ||
        sMarioCamState->action == ACT_RIDING_HOOT) {
        zoomDist *= 0.8f;
        set_handheld_shake(HAND_CAM_SHAKE_HANG_OWL);
    }

    // If not zooming out, only allow dist to decrease
    if (sZoomAmount == 0.f) {
        if (dist > zoomDist) {
            if ((dist -= 50.f) < zoomDist) {
                dist = zoomDist;
            }
        }
    } else {
        if ((sZoomAmount -= 30.f) < 0.f) {
            sZoomAmount = 0.f;
        }
        if (dist > zoomDist) {
            if ((dist -= 30.f) < zoomDist) {
                dist = zoomDist;
            }
        }
        if (dist < zoomDist) {
            if ((dist += 30.f) > zoomDist) {
                dist = zoomDist;
            }
        }
    }

    // Determine how fast to rotate the camera
    if (sCSideButtonYaw == 0) {
        if (c->mode == CAMERA_MODE_FREE_ROAM) {
            nextYawVel = 0xC0;
        } else {
            nextYawVel = 0x100;
        }
        if ((gPlayer1Controller->stickX != 0.f || gPlayer1Controller->stickY != 0.f) != 0) {
            nextYawVel = 0x20;
        }
    } else {
        if (sCSideButtonYaw < 0) {
            yaw += 0x200;
        }
        if (sCSideButtonYaw > 0) {
            yaw -= 0x200;
        }
        camera_approach_s16_symmetric_bool(&sCSideButtonYaw, 0, 0x100);
        nextYawVel = 0;
    }
    sYawSpeed = 0x400;
    xzDist = calc_hor_dist(sMarioCamState->pos, c->pos);

    if (sStatusFlags & CAM_FLAG_BEHIND_MARIO_POST_DOOR) {
        if (xzDist >= 250) {
            sStatusFlags &= ~CAM_FLAG_BEHIND_MARIO_POST_DOOR;
        }
        if (abss((sMarioCamState->faceAngle[1] - yaw) / 2) < 0x1800) {
            sStatusFlags &= ~CAM_FLAG_BEHIND_MARIO_POST_DOOR;
            yaw = sCameraYawAfterDoorCutscene + DEGREES(180);
            dist = 800.f;
            sStatusFlags |= CAM_FLAG_BLOCK_SMOOTH_MOVEMENT;
        }
    } else if (xzDist < 250) {
        // Turn rapidly if very close to Mario
        c->pos[0] += (250 - xzDist) * sins(yaw);
        c->pos[2] += (250 - xzDist) * coss(yaw);
        if (sCSideButtonYaw == 0) {
            nextYawVel = 0x1000;
            sYawSpeed = 0;
            vec3f_get_dist_and_angle(sMarioCamState->pos, c->pos, &dist, &pitch, &yaw);
        }
        closeToMario |= 1;
    }

    if (-16 < gPlayer1Controller->stickY) {
        c->yaw = yaw;
    }

    calc_y_to_curr_floor(&posHeight, 1, 200, &focHeight, 0.9f, 200);
    vec3f_copy(cPos, c->pos);
    avoidStatus = rotate_camera_around_walls(c, cPos, &avoidYaw, 0x600);
    // If a wall is blocking the view of Mario, then rotate in the calculated direction
    if (avoidStatus == AVOID_STATUS_WALL_COVERING_MARIO) {
        sAvoidYawVel = yaw;
        sStatusFlags |= CAM_FLAG_COLLIDED_WITH_WALL;
        // Rotate to avoid the wall
        approach_s16_asymptotic_bool(&yaw, avoidYaw, 10);
        sAvoidYawVel = (sAvoidYawVel - yaw) / 0x100;
    } else {
        if (gMarioStates[0].forwardVel == 0.f) {
            if (sStatusFlags & CAM_FLAG_COLLIDED_WITH_WALL) {
                if ((yawGoal - yaw) / 0x100 >= 0) {
                    yawDir = -1;
                } else {
                    yawDir = 1;
                }
                if ((sAvoidYawVel > 0 && yawDir > 0) || (sAvoidYawVel < 0 && yawDir < 0)) {
                    yawVel = nextYawVel;
                }
            } else {
                yawVel = nextYawVel;
            }
        } else {
            if (nextYawVel == 0x1000) {
                yawVel = nextYawVel;
            }
            sStatusFlags &= ~CAM_FLAG_COLLIDED_WITH_WALL;
        }

        // If a wall is near the camera, turn twice as fast
        if (avoidStatus != AVOID_STATUS_NONE) {
            yawVel += yawVel;
        }
        // ...Unless the camera already rotated from being close to Mario
        if ((closeToMario & 1) && avoidStatus != AVOID_STATUS_NONE) {
            yawVel = 0;
        }
        if (yawVel != 0 && get_dialog_id() == DIALOG_NONE) {
            camera_approach_s16_symmetric_bool(&yaw, yawGoal, yawVel);
        }
    }

    // Only zoom out if not obstructed by walls and Lakitu hasn't collided with any
    if (avoidStatus == AVOID_STATUS_NONE && !(sStatusFlags & CAM_FLAG_COLLIDED_WITH_WALL)) {
        approach_f32_asymptotic_bool(&dist, zoomDist - 100.f, 0.05f);
    }
    vec3f_set_dist_and_angle(sMarioCamState->pos, cPos, dist, pitch, yaw);
    cPos[1] += posHeight + 125.f;

    // Move the camera away from walls and set the collision flag
    if (collide_with_walls(cPos, 10.f, 80.f) != 0) {
        sStatusFlags |= CAM_FLAG_COLLIDED_WITH_WALL;
    }

    c->focus[0] = sMarioCamState->pos[0];
    c->focus[1] = sMarioCamState->pos[1] + 125.f + focHeight;
    c->focus[2] = sMarioCamState->pos[2];

    marioFloorHeight = 125.f + sMarioGeometry.currFloorHeight;
    marioFloor = sMarioGeometry.currFloor;
    camFloorHeight = find_floor(cPos[0], cPos[1] + 50.f, cPos[2], &cFloor) + 125.f;
    for (scale = 0.1f; scale < 1.f; scale += 0.2f) {
        scale_along_line(tempPos, cPos, sMarioCamState->pos, scale);
        tempFloorHeight = find_floor(tempPos[0], tempPos[1], tempPos[2], &tempFloor) + 125.f;
        if (tempFloor != NULL && tempFloorHeight > marioFloorHeight) {
            marioFloorHeight = tempFloorHeight;
            marioFloor = tempFloor;
        }
    }

    // Lower the camera in Mario mode
    if (sSelectionFlags & CAM_MODE_MARIO_ACTIVE) {
        marioFloorHeight -= 35.f;
        camFloorHeight -= 35.f;
        c->focus[1] -= 25.f;
    }

    // If there's water below the camera, decide whether to keep the camera above the water surface
    waterHeight = find_water_level(cPos[0], cPos[2]);
    if (waterHeight != FLOOR_LOWER_LIMIT) {
        waterHeight += 125.f;
        distFromWater = waterHeight - marioFloorHeight;
        if (!(gCameraMovementFlags & CAM_MOVE_METAL_BELOW_WATER)) {
            if (distFromWater > 800.f && (sMarioCamState->action & ACT_FLAG_METAL_WATER)) {
                gCameraMovementFlags |= CAM_MOVE_METAL_BELOW_WATER;
            }
        } else {
            if (distFromWater < 400.f || !(sMarioCamState->action & ACT_FLAG_METAL_WATER)) {
                gCameraMovementFlags &= ~CAM_MOVE_METAL_BELOW_WATER;
            }
        }
        // If not wearing the metal cap, always stay above
        if (!(gCameraMovementFlags & CAM_MOVE_METAL_BELOW_WATER) && camFloorHeight < waterHeight) {
            camFloorHeight = waterHeight;
        }
    } else {
        gCameraMovementFlags &= ~CAM_MOVE_METAL_BELOW_WATER;
    }

    cPos[1] = camFloorHeight;
    vec3f_copy(tempPos, cPos);
    tempPos[1] -= 125.f;
    if (marioFloor != NULL && camFloorHeight <= marioFloorHeight) {
        avoidStatus = is_range_behind_surface(c->focus, tempPos, marioFloor, 0, SURFACE_NULL);
        if (avoidStatus != AVOID_STATUS_WALL_NEAR_CAMERA && ceilHeight > marioFloorHeight) {
            camFloorHeight = marioFloorHeight;
        }
    }

    posHeight = 0.f;
    if (c->mode == CAMERA_MODE_FREE_ROAM) {
        if (gCameraMovementFlags & CAM_MOVE_ZOOMED_OUT) {
            posHeight = 375.f;
            if (gCurrLevelArea == AREA_SSL_PYRAMID) {
                posHeight /= 2;
            }
        } else {
            posHeight = 100.f;
        }
    }
    if ((gCameraMovementFlags & CAM_MOVE_ZOOMED_OUT) && (sSelectionFlags & CAM_MODE_MARIO_ACTIVE)) {
        posHeight = 610.f;
#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
        if (gCurrLevelArea == AREA_SSL_PYRAMID || gCurrLevelNum == LEVEL_CASTLE) {
            posHeight /= 2;
        }
#endif
    }

    // Make Lakitu fly above the gas
    gasHeight = find_poison_gas_level(cPos[0], cPos[2]);
    if (gasHeight != FLOOR_LOWER_LIMIT) {
        if ((gasHeight += 130.f) > c->pos[1]) {
            c->pos[1] = gasHeight;
        }
    }

    if (sMarioCamState->action & ACT_FLAG_HANGING || sMarioCamState->action == ACT_RIDING_HOOT) {
        camFloorHeight = sMarioCamState->pos[1] + 400.f;
        if (c->mode == CAMERA_MODE_FREE_ROAM) {
            camFloorHeight -= 100.f;
        }
        ceilHeight = CELL_HEIGHT_LIMIT;
        vec3f_copy(c->focus, sMarioCamState->pos);
    }

    if (sMarioCamState->action & ACT_FLAG_ON_POLE) {
        camFloorHeight = gMarioStates[0].usedObj->oPosY + 125.f;
        if (sMarioCamState->pos[1] - 100.f > camFloorHeight) {
            camFloorHeight = sMarioCamState->pos[1] - 100.f;
        }
        ceilHeight = CELL_HEIGHT_LIMIT;
        vec3f_copy(c->focus, sMarioCamState->pos);
    }
    if (camFloorHeight != FLOOR_LOWER_LIMIT) {
        camFloorHeight += posHeight;
        approach_camera_height(c, camFloorHeight, 20.f);
    }
    c->pos[0] = cPos[0];
    c->pos[2] = cPos[2];
    cPos[0] = gLakituState.goalPos[0];
    cPos[1] = c->pos[1];
    cPos[2] = gLakituState.goalPos[2];
    vec3f_get_dist_and_angle(cPos, c->pos, &dist, &tempPitch, &tempYaw);
    // Prevent the camera from lagging behind too much
    if (dist > 50.f) {
        dist = 50.f;
        vec3f_set_dist_and_angle(cPos, c->pos, dist, tempPitch, tempYaw);
    }
    if (sMarioGeometry.currFloorType != SURFACE_DEATH_PLANE) {
        vec3f_get_dist_and_angle(c->focus, c->pos, &dist, &tempPitch, &tempYaw);
        if (dist > zoomDist) {
            dist = zoomDist;
            vec3f_set_dist_and_angle(c->focus, c->pos, dist, tempPitch, tempYaw);
        }
    }
    if (ceilHeight != CELL_HEIGHT_LIMIT) {
        if (c->pos[1] > (ceilHeight -= 150.f)
            && (avoidStatus = is_range_behind_surface(c->pos, sMarioCamState->pos, ceil, 0, -1)) == 1) {
            c->pos[1] = ceilHeight;
        }
    }
#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
    if (gCurrLevelArea == AREA_WDW_TOWN) {
        yaw = clamp_positions_and_find_yaw(c->pos, c->focus, 2254.f, -3789.f, 3790.f, -2253.f);
    }
#endif
    return yaw;
}

/**
 * The default camera mode
 * Used by close and free roam modes
 */
void mode_default_camera(struct Camera *c) {
    set_fov_function(CAM_FOV_DEFAULT);
    c->nextYaw = update_default_camera(c);
    pan_ahead_of_player(c);
}

/**
 * The mode used by close and free roam
 */
void mode_lakitu_camera(struct Camera *c) {
    gCameraZoomDist = 800.f;
    mode_default_camera(c);
}

/**
 * When no other mode is active and the current R button mode is Mario
 */
void mode_mario_camera(struct Camera *c) {
    gCameraZoomDist = 350.f;
    mode_default_camera(c);
}

/**
 * Rotates the camera around the spiral staircase.
 */
s32 update_spiral_stairs_camera(struct Camera *c, Vec3f focus, Vec3f pos) {
    /// The returned yaw
    s16 camYaw;
    /// The focus (Mario)'s yaw around the stairs
    s16 focYaw;
    /// The camera's yaw around the stairs
    s16 posYaw;
    Vec3f cPos;
    Vec3f checkPos;
    struct Surface *floor;
    f32 focusHeight;
    f32 floorHeight;
    f32 focY;

    handle_c_button_movement(c);
    // Set base pos to the center of the staircase
    vec3f_set(sFixedModeBasePosition, -1280.f, 614.f, 1740.f);

    // Focus on Mario, and move the focus up the staircase with him
    calc_y_to_curr_floor(&focusHeight, 1.f, 200.f, &focusHeight, 0.9f, 200.f);
    focus[0] = sMarioCamState->pos[0];
    focY = sMarioCamState->pos[1] + 125.f + focusHeight;
    focus[2] = sMarioCamState->pos[2];

    vec3f_copy(cPos, pos);
    vec3f_get_yaw(sFixedModeBasePosition, focus, &focYaw);
    vec3f_get_yaw(sFixedModeBasePosition, cPos,  &posYaw);

    sSpiralStairsYawOffset = posYaw - focYaw;
    // posYaw will change if Mario is more than 90 degrees around the stairs, relative to the camera
    if (sSpiralStairsYawOffset < DEGREES(-90)) {
        sSpiralStairsYawOffset = DEGREES(-90);
    }
    if (sSpiralStairsYawOffset > DEGREES(90)) {
        sSpiralStairsYawOffset = DEGREES(90);
    }
    focYaw += sSpiralStairsYawOffset;
    posYaw = focYaw;

    vec3f_set_dist_and_angle(sFixedModeBasePosition, cPos, 300.f, 0, posYaw);

    // Move the camera's y coord up/down the staircase
    checkPos[0] = focus[0] + (cPos[0] - focus[0]) * 0.7f;
    checkPos[1] = focus[1] + (cPos[1] - focus[1]) * 0.7f + 300.f;
    checkPos[2] = focus[2] + (cPos[2] - focus[2]) * 0.7f;
    floorHeight = find_floor(checkPos[0], checkPos[1] + 50.f, checkPos[2], &floor);

    if (floorHeight != FLOOR_LOWER_LIMIT) {
        if (floorHeight < sMarioGeometry.currFloorHeight) {
            floorHeight = sMarioGeometry.currFloorHeight;
        }
        pos[1] = approach_f32(pos[1], (floorHeight += 125.f), 30.f, 30.f);
    }

    camera_approach_f32_symmetric_bool(&focus[1], focY, 30.f);
    pos[0] = cPos[0];
    pos[2] = cPos[2];
    camYaw = calculate_yaw(focus, pos);

    return camYaw;
}

/**
 * The mode used in the spiral staircase in the castle
 */
void mode_spiral_stairs_camera(struct Camera *c) {
    c->nextYaw = update_spiral_stairs_camera(c, c->focus, c->pos);
}

s32 update_slide_or_0f_camera(UNUSED struct Camera *c, Vec3f focus, Vec3f pos) {
    s16 yaw = sMarioCamState->faceAngle[1] + sModeOffsetYaw + DEGREES(180);

    focus_on_mario(focus, pos, 125.f, 125.f, 800.f, DEGREES(30), yaw);
    return sMarioCamState->faceAngle[1];
}

static UNUSED void unused_mode_0f_camera(struct Camera *c) {
    if (gPlayer1Controller->buttonPressed & U_CBUTTONS) {
        gCameraMovementFlags |= CAM_MOVE_C_UP_MODE;
    }
    c->nextYaw = update_slide_camera(c);
}

/**
 * Slide/hoot mode.
 * In this mode, the camera is always at the back of Mario, because Mario generally only moves forward.
 */
void mode_slide_camera(struct Camera *c) {
    if (sMarioGeometry.currFloorType == SURFACE_CLOSE_CAMERA ||
        sMarioGeometry.currFloorType == SURFACE_NO_CAM_COL_SLIPPERY) {
        mode_lakitu_camera(c);
    } else {
        if (gPlayer1Controller->buttonPressed & U_CBUTTONS) {
            gCameraMovementFlags |= CAM_MOVE_C_UP_MODE;
        }
        c->nextYaw = update_slide_camera(c);
    }
}

void store_lakitu_cam_info_for_c_up(struct Camera *c) {
    vec3f_copy(sCameraStoreCUp.pos, c->pos);
    vec3f_sub(sCameraStoreCUp.pos, sMarioCamState->pos);
    // Only store the y value, and as an offset from Mario, for some reason
    vec3f_set(sCameraStoreCUp.focus, 0.f, c->focus[1] - sMarioCamState->pos[1], 0.f);
}

/**
 * Start C-Up mode. The actual mode change is handled in update_mario_inputs() in mario.c
 *
 * @see update_mario_inputs
 */
void set_mode_c_up(struct Camera *c) {
    if (!(gCameraMovementFlags & CAM_MOVE_C_UP_MODE)) {
        gCameraMovementFlags |= CAM_MOVE_C_UP_MODE;
        store_lakitu_cam_info_for_c_up(c);
        sCameraSoundFlags &= ~CAM_SOUND_C_UP_PLAYED;
    }
}

/**
 * Zoom the camera out of C-Up mode, avoiding moving into a wall, if possible, by searching for an open
 * direction.
 */
void exit_c_up(struct Camera *c) {
    struct Surface *surface;
    Vec3f checkFoc;
    Vec3f curPos;
    // Variables for searching for an open direction
    s32 searching = FALSE;
    /// The current sector of the circle that we are checking
    s32 sector;
    f32 ceilHeight;
    f32 floorHeight;
    f32 curDist;
    f32 d;
    s16 curPitch;
    s16 curYaw;
    s16 checkYaw = 0;

    if ((gCameraMovementFlags & CAM_MOVE_C_UP_MODE) && !(gCameraMovementFlags & CAM_MOVE_STARTED_EXITING_C_UP)) {
        vec3f_copy(checkFoc, c->focus);
        checkFoc[0] = sMarioCamState->pos[0];
        checkFoc[2] = sMarioCamState->pos[2];
        vec3f_get_dist_and_angle(checkFoc, c->pos, &curDist, &curPitch, &curYaw);
        vec3f_copy(curPos, c->pos);
        curDist = 80.f;

        // Search for an open direction to zoom out in, if the camera is changing to close, free roam,
        // or spiral-stairs mode
        if (sModeInfo.lastMode == CAMERA_MODE_SPIRAL_STAIRS || sModeInfo.lastMode == CAMERA_MODE_CLOSE
            || sModeInfo.lastMode == CAMERA_MODE_FREE_ROAM) {
            searching = TRUE;
            // Check the whole circle around Mario for an open direction to zoom out to
            for (sector = 0; sector < 16 && searching == 1; sector++) {
                vec3f_set_dist_and_angle(checkFoc, curPos, curDist, 0, curYaw + checkYaw);

                // If there are no walls this way,
                if (f32_find_wall_collision(&curPos[0], &curPos[1], &curPos[2], 20.f, 50.f) == 0) {

                    // Start close to Mario, check for walls, floors, and ceilings all the way to the
                    // zoomed out distance
                    for (d = curDist; d < gCameraZoomDist; d += 20.f) {
                        vec3f_set_dist_and_angle(checkFoc, curPos, d, 0, curYaw + checkYaw);

                        // Check if we're zooming out into a floor or ceiling
                        ceilHeight = find_ceil(curPos[0], curPos[1] - 150.f, curPos[2], &surface) + -10.f;
                        if (surface != NULL && ceilHeight < curPos[1]) {
                            break;
                        }
                        floorHeight = find_floor(curPos[0], curPos[1] + 150.f, curPos[2], &surface) + 10.f;
                        if (surface != NULL && floorHeight > curPos[1]) {
                            break;
                        }

                        // Stop checking this direction if there is a wall blocking the way
                        if (f32_find_wall_collision(&curPos[0], &curPos[1], &curPos[2], 20.f, 50.f) == 1) {
                            break;
                        }
                    }

                    // If there was no collision found all the way to the max distance, it's an opening
                    if (d >= gCameraZoomDist) {
                        searching = FALSE;
                    }
                }

                // Alternate left and right, checking each 1/16th (22.5 degrees) of the circle
                if (searching == 1) {
                    checkYaw = -checkYaw;
                    if (checkYaw < 0) {
                        checkYaw -= 0x1000;
                    } else {
                        checkYaw += 0x1000;
                    }
                }
            }

            // Update the stored focus and pos to the direction found in the search
            if (!searching) {
                vec3f_set_dist_and_angle(checkFoc, sCameraStoreCUp.pos, gCameraZoomDist, 0, curYaw + checkYaw);
                vec3f_copy(sCameraStoreCUp.focus, checkFoc);
                vec3f_sub(sCameraStoreCUp.pos, sMarioCamState->pos);
                vec3f_sub(sCameraStoreCUp.focus, sMarioCamState->pos);
            }

            gCameraMovementFlags |= CAM_MOVE_STARTED_EXITING_C_UP;
            transition_next_state(c, 15);
        } else {
            // Let the next camera mode handle it
            gCameraMovementFlags &= ~(CAM_MOVE_STARTED_EXITING_C_UP | CAM_MOVE_C_UP_MODE);
            vec3f_set_dist_and_angle(checkFoc, c->pos, curDist, curPitch, curYaw + checkYaw);
        }
        play_sound_cbutton_down();
    }
}

/**
 * The mode used when C-Up is pressed.
 */
s32 update_c_up(UNUSED struct Camera *c, Vec3f focus, Vec3f pos) {
    s16 pitch = sCUpCameraPitch;
    s16 yaw = sMarioCamState->faceAngle[1] + sModeOffsetYaw + DEGREES(180);

    focus_on_mario(focus, pos, 125.f, 125.f, 250.f, pitch, yaw);
    return sMarioCamState->faceAngle[1];
}

/**
 * Make Mario's head move in C-Up mode.
 */
void move_mario_head_c_up(UNUSED struct Camera *c) {
    sCUpCameraPitch += (s16)(gPlayer1Controller->stickY * 10.f);
    sModeOffsetYaw -= (s16)(gPlayer1Controller->stickX * 10.f);

    // Bound looking up to nearly 80 degrees.
    if (sCUpCameraPitch > 0x38E3) {
        sCUpCameraPitch = 0x38E3;
    }
    // Bound looking down to -45 degrees
    if (sCUpCameraPitch < -0x2000) {
        sCUpCameraPitch = -0x2000;
    }

    // Bound the camera yaw to +-120 degrees
    if (sModeOffsetYaw > 0x5555) {
        sModeOffsetYaw = 0x5555;
    }
    if (sModeOffsetYaw < -0x5555) {
        sModeOffsetYaw = -0x5555;
    }

    // Give Mario's neck natural-looking constraints
    sMarioCamState->headRotation[0] = sCUpCameraPitch * 3 / 4;
    sMarioCamState->headRotation[1] = sModeOffsetYaw * 3 / 4;
}

/**
 * Zooms the camera in for C-Up mode
 */
void move_into_c_up(struct Camera *c) {
    struct LinearTransitionPoint *start = &sModeInfo.transitionStart;
    struct LinearTransitionPoint *end = &sModeInfo.transitionEnd;

    f32 dist  = end->dist  - start->dist;
    s16 pitch = end->pitch - start->pitch;
    s16 yaw   = end->yaw   - start->yaw;

    // Linearly interpolate from start to end position's polar coordinates
    dist  = start->dist  + dist  * sModeInfo.frame / sModeInfo.max;
    pitch = start->pitch + pitch * sModeInfo.frame / sModeInfo.max;
    yaw   = start->yaw   + yaw   * sModeInfo.frame / sModeInfo.max;

    // Linearly interpolate the focus from start to end
    c->focus[0] = start->focus[0] + (end->focus[0] - start->focus[0]) * sModeInfo.frame / sModeInfo.max;
    c->focus[1] = start->focus[1] + (end->focus[1] - start->focus[1]) * sModeInfo.frame / sModeInfo.max;
    c->focus[2] = start->focus[2] + (end->focus[2] - start->focus[2]) * sModeInfo.frame / sModeInfo.max;

    vec3f_add(c->focus, sMarioCamState->pos);
    vec3f_set_dist_and_angle(c->focus, c->pos, dist, pitch, yaw);

    sMarioCamState->headRotation[0] = 0;
    sMarioCamState->headRotation[1] = 0;

    // Finished zooming in
    if (++sModeInfo.frame == sModeInfo.max) {
        gCameraMovementFlags &= ~CAM_MOVING_INTO_MODE;
    }
}

/**
 * The main update function for C-Up mode
 */
void mode_c_up_camera(struct Camera *c) {
    // Play a sound when entering C-Up mode
    if (!(sCameraSoundFlags & CAM_SOUND_C_UP_PLAYED)) {
        play_sound_cbutton_up();
        sCameraSoundFlags |= CAM_SOUND_C_UP_PLAYED;
    }

    // Zoom in first
    if (gCameraMovementFlags & CAM_MOVING_INTO_MODE) {
        gCameraMovementFlags |= CAM_MOVE_C_UP_MODE;
        move_into_c_up(c);
        return;
    }

    if (!(gCameraMovementFlags & CAM_MOVE_STARTED_EXITING_C_UP)) {
        // Normal update
        move_mario_head_c_up(c);
        update_c_up(c, c->focus, c->pos);
    } else {
        // Exiting C-Up
        if (sStatusFlags & CAM_FLAG_TRANSITION_OUT_OF_C_UP) {
            // Retrieve the previous position and focus
            vec3f_copy(c->pos, sCameraStoreCUp.pos);
            vec3f_add(c->pos, sMarioCamState->pos);
            vec3f_copy(c->focus, sCameraStoreCUp.focus);
            vec3f_add(c->focus, sMarioCamState->pos);
            // Make Mario look forward
            camera_approach_s16_symmetric_bool(&sMarioCamState->headRotation[0], 0, 1024);
            camera_approach_s16_symmetric_bool(&sMarioCamState->headRotation[1], 0, 1024);
        } else {
            // Finished exiting C-Up
            gCameraMovementFlags &= ~(CAM_MOVE_STARTED_EXITING_C_UP | CAM_MOVE_C_UP_MODE);
        }
    }
    sPanDistance = 0.f;

    // Exit C-Up mode
    if (gPlayer1Controller->buttonPressed & (A_BUTTON | B_BUTTON | D_CBUTTONS | L_CBUTTONS | R_CBUTTONS)) {
        exit_c_up(c);
    }
}

/**
 * Used when Mario is in a cannon.
 */
s32 update_in_cannon(UNUSED struct Camera *c, Vec3f focus, Vec3f pos) {
    focus_on_mario(pos, focus, 125.f + sCannonYOffset, 125.f, 800.f,
                                    sMarioCamState->faceAngle[0], sMarioCamState->faceAngle[1]);
    return sMarioCamState->faceAngle[1];
}

/**
 * Updates the camera when Mario is in a cannon.
 * sCannonYOffset is used to make the camera rotate down when Mario has just entered the cannon
 */
void mode_cannon_camera(struct Camera *c) {
    sLakituPitch = 0;
    gCameraMovementFlags &= ~CAM_MOVING_INTO_MODE;
    c->nextYaw = update_in_cannon(c, c->focus, c->pos);
    if (gPlayer1Controller->buttonPressed & A_BUTTON) {
        set_camera_mode(c, CAMERA_MODE_BEHIND_MARIO, 1);
        sPanDistance = 0.f;
        sCannonYOffset = 0.f;
        sStatusFlags &= ~CAM_FLAG_BLOCK_SMOOTH_MOVEMENT;
    } else {
        sCannonYOffset = approach_f32(sCannonYOffset, 0.f, 100.f, 100.f);
    }
}

/**
 * Cause Lakitu to fly to the next Camera position and focus over a number of frames.
 *
 * At the end of each frame, Lakitu's position and focus ("state") are stored.
 * Calling this function makes next_lakitu_state() fly from the last frame's state to the
 * current frame's calculated state.
 *
 * @see next_lakitu_state()
 */
void transition_next_state(UNUSED struct Camera *c, s16 frames) {
    if (!(sStatusFlags & CAM_FLAG_FRAME_AFTER_CAM_INIT)) {
        sStatusFlags |= (CAM_FLAG_START_TRANSITION | CAM_FLAG_TRANSITION_OUT_OF_C_UP);
        sModeTransition.framesLeft = frames;
    }
}

/**
 * Sets the camera mode to `newMode` and initializes sModeTransition with `numFrames` frames
 *
 * Used to change the camera mode to 'level-oriented' modes
 *      namely: RADIAL/OUTWARD_RADIAL, 8_DIRECTIONS, FREE_ROAM, CLOSE, SPIRAL_STAIRS, and SLIDE_HOOT
 */
void transition_to_camera_mode(struct Camera *c, s16 newMode, s16 numFrames) {
    if (c->mode != newMode) {
        sModeInfo.newMode = (newMode != -1) ? newMode : sModeInfo.lastMode;
        sModeInfo.lastMode = c->mode;
        c->mode = sModeInfo.newMode;

        // Clear movement flags that would affect the transition
        gCameraMovementFlags &= (u16)~(CAM_MOVE_RESTRICT | CAM_MOVE_ROTATE);
        if (!(sStatusFlags & CAM_FLAG_FRAME_AFTER_CAM_INIT)) {
            transition_next_state(c, numFrames);
            sCUpCameraPitch = 0;
            sModeOffsetYaw = 0;
            sLakituDist = 0;
            sLakituPitch = 0;
            sAreaYawChange = 0;
            sPanDistance = 0.f;
            sCannonYOffset = 0.f;
        }
    }
}

/**
 * Used to change the camera mode between its default/previous and certain Mario-oriented modes,
 *      namely: C_UP, WATER_SURFACE, CLOSE, and BEHIND_MARIO
 *
 * Stores the current pos and focus in sModeInfo->transitionStart, and
 * stores the next pos and focus into sModeInfo->transitionEnd. These two fields are used in
 * move_into_c_up().
 *
 * @param mode the mode to change to, or -1 to switch to the previous mode
 * @param frames number of frames the transition should last, only used when entering C_UP
 */
void set_camera_mode(struct Camera *c, s16 mode, s16 frames) {
    struct LinearTransitionPoint *start = &sModeInfo.transitionStart;
    struct LinearTransitionPoint *end = &sModeInfo.transitionEnd;

#ifdef ENABLE_VANILLA_CAM_PROCESSING
    if (mode == CAMERA_MODE_WATER_SURFACE && gCurrLevelArea == AREA_TTM_OUTSIDE) {
    } else {
#endif
        // Clear movement flags that would affect the transition
        gCameraMovementFlags &= (u16)~(CAM_MOVE_RESTRICT | CAM_MOVE_ROTATE);
        gCameraMovementFlags |= CAM_MOVING_INTO_MODE;
        if (mode == CAMERA_MODE_NONE) {
            mode = CAMERA_MODE_CLOSE;
        }
        sCUpCameraPitch = 0;
        sModeOffsetYaw = 0;
        sLakituDist = 0;
        sLakituPitch = 0;
        sAreaYawChange = 0;

        sModeInfo.newMode = (mode != -1) ? mode : sModeInfo.lastMode;
        sModeInfo.lastMode = c->mode;
        sModeInfo.max = frames;
        sModeInfo.frame = 1;

        c->mode = sModeInfo.newMode;
        gLakituState.mode = c->mode;

        vec3f_copy(end->focus, c->focus);
        vec3f_sub(end->focus, sMarioCamState->pos);

        vec3f_copy(end->pos, c->pos);
        vec3f_sub(end->pos, sMarioCamState->pos);

#ifndef ENABLE_VANILLA_CAM_PROCESSING
        if (mode == CAMERA_MODE_8_DIRECTIONS) {
            // Helps transition from any camera mode to 8dir
            s8DirModeYawOffset = snap_to_45_degrees(c->yaw);
        }
#endif

        sAreaYaw = sModeTransitions[sModeInfo.newMode](c, end->focus, end->pos);

        // End was updated by sModeTransitions
        vec3f_sub(end->focus, sMarioCamState->pos);
        vec3f_sub(end->pos, sMarioCamState->pos);

        vec3f_copy(start->focus, gLakituState.curFocus);
        vec3f_sub(start->focus, sMarioCamState->pos);

        vec3f_copy(start->pos, gLakituState.curPos);
        vec3f_sub(start->pos, sMarioCamState->pos);

        vec3f_get_dist_and_angle(start->focus, start->pos, &start->dist, &start->pitch, &start->yaw);
        vec3f_get_dist_and_angle(end->focus, end->pos, &end->dist, &end->pitch, &end->yaw);
#ifdef ENABLE_VANILLA_CAM_PROCESSING
    }
#endif
}

/**
 * Updates Lakitu's position/focus and applies camera shakes.
 */
void update_lakitu(struct Camera *c) {
    struct Surface *floor = NULL;
    Vec3f newPos;
    Vec3f newFoc;
    f32 distToFloor;
    s16 newYaw;

    if (!(gCameraMovementFlags & CAM_MOVE_PAUSE_SCREEN)) {
        newYaw = next_lakitu_state(newPos, newFoc, c->pos, c->focus, sOldPosition, sOldFocus,
                                   c->nextYaw);
        set_or_approach_s16_symmetric(&c->yaw, newYaw, sYawSpeed);
        sStatusFlags &= ~CAM_FLAG_UNUSED_CUTSCENE_ACTIVE;

        // Update old state
        vec3f_copy(sOldPosition, newPos);
        vec3f_copy(sOldFocus, newFoc);

        gLakituState.yaw = c->yaw;
        gLakituState.nextYaw = c->nextYaw;
        vec3f_copy(gLakituState.goalPos, c->pos);
        vec3f_copy(gLakituState.goalFocus, c->focus);

        // Simulate Lakitu flying to the new position and turning towards the new focus
        set_or_approach_vec3f_asymptotic(gLakituState.curPos, newPos,
                                         gLakituState.posHSpeed, gLakituState.posVSpeed,
                                         gLakituState.posHSpeed);
        set_or_approach_vec3f_asymptotic(gLakituState.curFocus, newFoc,
                                         gLakituState.focHSpeed, gLakituState.focVSpeed,
                                         gLakituState.focHSpeed);
        // Adjust Lakitu's speed back to normal
        set_or_approach_f32_asymptotic(&gLakituState.focHSpeed, 0.8f, 0.05f);
        set_or_approach_f32_asymptotic(&gLakituState.focVSpeed, 0.3f, 0.05f);
        set_or_approach_f32_asymptotic(&gLakituState.posHSpeed, 0.3f, 0.05f);
        set_or_approach_f32_asymptotic(&gLakituState.posVSpeed, 0.3f, 0.05f);

        // Turn on smooth movement when it hasn't been blocked for 2 frames
        if (sStatusFlags & CAM_FLAG_BLOCK_SMOOTH_MOVEMENT) {
            sStatusFlags &= ~CAM_FLAG_BLOCK_SMOOTH_MOVEMENT;
        } else {
            sStatusFlags |= CAM_FLAG_SMOOTH_MOVEMENT;
        }

        vec3f_copy(gLakituState.pos, gLakituState.curPos);
        vec3f_copy(gLakituState.focus, gLakituState.curFocus);

        if (c->cutscene) {
            vec3f_add(gLakituState.focus, sPlayer2FocusOffset);
            vec3_zero(sPlayer2FocusOffset);
        }

        vec3f_get_dist_and_angle(gLakituState.pos, gLakituState.focus, &gLakituState.focusDistance,
                                 &gLakituState.oldPitch, &gLakituState.oldYaw);

        gLakituState.roll = 0;

        // Apply camera shakes
        shake_camera_pitch(gLakituState.pos, gLakituState.focus);
        shake_camera_yaw(gLakituState.pos, gLakituState.focus);
        shake_camera_roll(&gLakituState.roll);
        shake_camera_handheld(gLakituState.pos, gLakituState.focus);

        if (sMarioCamState->action == ACT_DIVE && gLakituState.lastFrameAction != ACT_DIVE) {
            set_camera_shake_from_hit(SHAKE_HIT_FROM_BELOW);
        }

        gLakituState.roll += sHandheldShakeRoll;
        gLakituState.roll += gLakituState.keyDanceRoll;

        if (c->mode != CAMERA_MODE_C_UP && c->cutscene == CUTSCENE_NONE) {
            gCollisionFlags |= COLLISION_FLAG_CAMERA;
            distToFloor = find_floor(gLakituState.pos[0],
                                     gLakituState.pos[1] + 20.0f,
                                     gLakituState.pos[2], &floor);
            if (distToFloor != FLOOR_LOWER_LIMIT) {
                if (gLakituState.pos[1] < (distToFloor += 100.0f)) {
                    gLakituState.pos[1] = distToFloor;
                } else {
                    gCollisionFlags &= ~COLLISION_FLAG_CAMERA;
                }
            }
        }

        vec3f_copy(sModeTransition.marioPos, sMarioCamState->pos);
    }
    clamp_pitch(gLakituState.pos, gLakituState.focus, 0x3E00, -0x3E00);
    gLakituState.mode = c->mode;
    gLakituState.defMode = c->defMode;
}

/**
 * The main camera update function.
 * Gets controller input, checks for cutscenes, handles mode changes, and moves the camera
 */
void update_camera(struct Camera *c) {
    PROFILER_GET_SNAPSHOT_TYPE(PROFILER_DELTA_COLLISION);
    gCamera = c;
    update_camera_hud_status(c);
    if (c->cutscene == CUTSCENE_NONE
#ifdef PUPPYCAM
        && !gPuppyCam.enabled
#endif
        && gCurrentArea->camera->mode != CAMERA_MODE_INSIDE_CANNON) {
        // Only process R_TRIG if 'fixed' is not selected in the menu
        if (cam_select_alt_mode(CAM_SELECTION_NONE) == CAM_SELECTION_MARIO) {
            if (gPlayer1Controller->buttonPressed & R_TRIG) {
                if (set_cam_angle(0) == CAM_ANGLE_LAKITU) {
                    set_cam_angle(CAM_ANGLE_MARIO);
                } else {
                    set_cam_angle(CAM_ANGLE_LAKITU);
                }
            }
        }
        play_sound_if_cam_switched_to_lakitu_or_mario();
    }

    // Initialize the camera
    sStatusFlags &= ~CAM_FLAG_FRAME_AFTER_CAM_INIT;
    if (gCameraMovementFlags & CAM_MOVE_INIT_CAMERA) {
        init_camera(c);
        gCameraMovementFlags &= ~CAM_MOVE_INIT_CAMERA;
        sStatusFlags |= CAM_FLAG_FRAME_AFTER_CAM_INIT;
    }

#ifdef PUPPYCAM
    if (!gPuppyCam.enabled || c->cutscene != CUTSCENE_NONE || gCurrentArea->camera->mode == CAMERA_MODE_INSIDE_CANNON) {
#endif
    // Store previous geometry information
    sMarioGeometry.prevFloorHeight = sMarioGeometry.currFloorHeight;
    sMarioGeometry.prevCeilHeight = sMarioGeometry.currCeilHeight;
    sMarioGeometry.prevFloor = sMarioGeometry.currFloor;
    sMarioGeometry.prevCeil = sMarioGeometry.currCeil;
    sMarioGeometry.prevFloorType = sMarioGeometry.currFloorType;
    sMarioGeometry.prevCeilType = sMarioGeometry.currCeilType;

    find_mario_floor_and_ceil(&sMarioGeometry);
    gCollisionFlags |= COLLISION_FLAG_CAMERA;
    vec3f_copy(c->pos, gLakituState.goalPos);
    vec3f_copy(c->focus, gLakituState.goalFocus);

    c->yaw = gLakituState.yaw;
    c->nextYaw = gLakituState.nextYaw;
    c->mode = gLakituState.mode;
    c->defMode = gLakituState.defMode;
#ifdef ENABLE_VANILLA_CAM_PROCESSING
    camera_course_processing(c);
#else
    if (gCurrDemoInput != NULL) camera_course_processing(c);
#endif
    sCButtonsPressed = find_c_buttons_pressed(sCButtonsPressed, gPlayer1Controller->buttonPressed, gPlayer1Controller->buttonDown);

    if (c->cutscene != CUTSCENE_NONE) {
        sYawSpeed = 0;
        play_cutscene(c);
        sFramesSinceCutsceneEnded = 0;
    } else {
        // Clear the recent cutscene after 8 frames
        if (gRecentCutscene != CUTSCENE_NONE && sFramesSinceCutsceneEnded < 8) {
            sFramesSinceCutsceneEnded++;
            if (sFramesSinceCutsceneEnded >= 8) {
                gRecentCutscene = CUTSCENE_NONE;
                sFramesSinceCutsceneEnded = 0;
            }
        }
    }
    // If not in a cutscene, do mode processing
    if (c->cutscene == CUTSCENE_NONE) {
        sYawSpeed = 0x400;

        if (sSelectionFlags & CAM_MODE_MARIO_ACTIVE) {
            switch (c->mode) {
                case CAMERA_MODE_BEHIND_MARIO:
                    mode_behind_mario_camera(c);
                    break;

                case CAMERA_MODE_C_UP:
                    mode_c_up_camera(c);
                    break;

                case CAMERA_MODE_WATER_SURFACE:
                    mode_water_surface_camera(c);
                    break;

                case CAMERA_MODE_INSIDE_CANNON:
                    mode_cannon_camera(c);
                    break;

                default:
                    mode_mario_camera(c);
            }
        } else {
            switch (c->mode) {
                case CAMERA_MODE_BEHIND_MARIO:
                    mode_behind_mario_camera(c);
                    break;

                case CAMERA_MODE_C_UP:
                    mode_c_up_camera(c);
                    break;

                case CAMERA_MODE_WATER_SURFACE:
                    mode_water_surface_camera(c);
                    break;

                case CAMERA_MODE_INSIDE_CANNON:
                    mode_cannon_camera(c);
                    break;

                case CAMERA_MODE_8_DIRECTIONS:
                    mode_8_directions_camera(c);
                    break;

                case CAMERA_MODE_RADIAL:
                    mode_radial_camera(c);
                    break;

                case CAMERA_MODE_OUTWARD_RADIAL:
                    mode_outward_radial_camera(c);
                    break;

                case CAMERA_MODE_CLOSE:
                    mode_lakitu_camera(c);
                    break;

                case CAMERA_MODE_FREE_ROAM:
                    mode_lakitu_camera(c);
                    break;

                case CAMERA_MODE_BOSS_FIGHT:
                    mode_boss_fight_camera(c);
                    break;

                case CAMERA_MODE_PARALLEL_TRACKING:
                    mode_parallel_tracking_camera(c);
                    break;

                case CAMERA_MODE_SLIDE_HOOT:
                    mode_slide_camera(c);
                    break;

                case CAMERA_MODE_FIXED:
                    mode_fixed_camera(c);
                    break;

                case CAMERA_MODE_SPIRAL_STAIRS:
                    mode_spiral_stairs_camera(c);
                    break;
            }
        }
    }
#ifdef PUPPYCAM
    }
#endif
    // Start any Mario-related cutscenes
    start_cutscene(c, get_cutscene_from_mario_status(c));
    gCollisionFlags &= ~COLLISION_FLAG_CAMERA;
#ifdef PUPPYCAM
    if (!gPuppyCam.enabled || c->cutscene != 0 || gCurrentArea->camera->mode == CAMERA_MODE_INSIDE_CANNON) {
#endif
#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
    if (gCurrLevelNum != LEVEL_CASTLE) {
#endif
        // If fixed camera is selected as the alternate mode, then fix the camera as long as the right
        // trigger is held
        if ((c->cutscene == CUTSCENE_NONE &&
            (gPlayer1Controller->buttonDown & R_TRIG) && cam_select_alt_mode(0) == CAM_SELECTION_FIXED)
            || (gCameraMovementFlags & CAM_MOVE_FIX_IN_PLACE)
            || (sMarioCamState->action) == ACT_GETTING_BLOWN) {

            // If this is the first frame that R_TRIG is held, play the "click" sound
            if (c->cutscene == CUTSCENE_NONE && (gPlayer1Controller->buttonPressed & R_TRIG)
                && cam_select_alt_mode(0) == CAM_SELECTION_FIXED) {
                sCameraSoundFlags |= CAM_SOUND_FIXED_ACTIVE;
                play_sound_rbutton_changed();
            }

            // Fixed mode only prevents Lakitu from moving. The camera pos still updates, so
            // Lakitu will fly to his next position as normal whenever R_TRIG is released.
            gLakituState.posHSpeed = 0.f;
            gLakituState.posVSpeed = 0.f;

            vec3f_get_yaw(gLakituState.focus, gLakituState.pos, &c->nextYaw);
            c->yaw = c->nextYaw;
            gCameraMovementFlags &= ~CAM_MOVE_FIX_IN_PLACE;
        } else {
            // Play the "click" sound when fixed mode is released
            if (sCameraSoundFlags & CAM_SOUND_FIXED_ACTIVE) {
                play_sound_rbutton_changed();
                sCameraSoundFlags &= ~CAM_SOUND_FIXED_ACTIVE;
            }
        }
#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
    } else {
        if ((gPlayer1Controller->buttonPressed & R_TRIG) && (cam_select_alt_mode(0) == CAM_SELECTION_FIXED)) {
            play_sound_button_change_blocked();
        }
    }
#endif

    update_lakitu(c);
#ifdef PUPPYCAM
    }
    // Just a cute little bit that syncs puppycamera up to vanilla when playing a vanilla cutscene :3
    if (c->cutscene != CUTSCENE_NONE) {
        gPuppyCam.yawTarget = gCamera->yaw;
        gPuppyCam.yaw = gCamera->yaw;
        if (gMarioState->action == ACT_ENTERING_STAR_DOOR) { // god this is stupid and the fact I have to continue doing this is testament to the idiocy of the star door cutscene >:(
            gPuppyCam.yawTarget = gMarioState->faceAngle[1] + 0x8000;
            gPuppyCam.yaw = gMarioState->faceAngle[1] + 0x8000;
        }
    }
    if (c->cutscene == CUTSCENE_NONE
        && gPuppyCam.enabled
        && gCurrentArea->camera->mode != CAMERA_MODE_INSIDE_CANNON) {
        // Clear the recent cutscene after 8 frames
        if (gRecentCutscene != CUTSCENE_NONE && sFramesSinceCutsceneEnded < 8) {
            sFramesSinceCutsceneEnded++;
            if (sFramesSinceCutsceneEnded >= 8) {
                gRecentCutscene = CUTSCENE_NONE;
                sFramesSinceCutsceneEnded = 0;
            }
        }
        puppycam_loop();
        // Apply camera shakes
        shake_camera_pitch(gLakituState.pos, gLakituState.focus);
        shake_camera_yaw(gLakituState.pos, gLakituState.focus);
        shake_camera_roll(&gLakituState.roll);
        shake_camera_handheld(gLakituState.pos, gLakituState.focus);

        if ((sMarioCamState->action == ACT_DIVE)
         && (gLakituState.lastFrameAction != ACT_DIVE)) {
            set_camera_shake_from_hit(SHAKE_HIT_FROM_BELOW);
        }
        gLakituState.roll += sHandheldShakeRoll;
        gLakituState.roll += gLakituState.keyDanceRoll;
    }
#endif
    gLakituState.lastFrameAction = sMarioCamState->action;
    profiler_update(PROFILER_TIME_CAMERA, profiler_get_delta(PROFILER_DELTA_COLLISION) - first);
}

/**
 * Reset all the camera variables to their arcane defaults
 */
void reset_camera(struct Camera *c) {
    gCamera = c;
    s2ndRotateFlags = 0;
    sStatusFlags = 0;
    gCutsceneTimer = 0;
    sCutsceneShot = 0;
    gCutsceneObjSpawn = CUTSCENE_OBJ_NONE;
    gObjCutsceneDone = FALSE;
    gCutsceneFocus = NULL;
    gSecondCameraFocus = NULL;
    sCButtonsPressed = 0;
    vec3f_copy(sModeTransition.marioPos, sMarioCamState->pos);
    sModeTransition.framesLeft = 0;
    gCameraMovementFlags = CAM_MOVE_INIT_CAMERA;
    sStatusFlags = 0;
    sCameraSoundFlags = 0;
    sCUpCameraPitch = 0;
    sModeOffsetYaw = 0;
    sSpiralStairsYawOffset = 0;
    sLakituDist = 0;
    sLakituPitch = 0;
    sAreaYaw = 0;
    sAreaYawChange = 0.f;
    sPanDistance = 0.f;
    sCannonYOffset = 0.f;
    sZoomAmount = 0.f;
    sZeroZoomDist = 0.f;
    sBehindMarioSoundTimer = 0;
    sCSideButtonYaw = 0;
    s8DirModeBaseYaw = 0;
    s8DirModeYawOffset = 0;
    c->doorStatus = DOOR_DEFAULT;
    sMarioCamState->headRotation[0] = 0;
    sMarioCamState->headRotation[1] = 0;
    // sLuigiCamState->headRotation[0] = 0;
    // sLuigiCamState->headRotation[1] = 0;
    sMarioCamState->cameraEvent = CAM_EVENT_NONE;
    sMarioCamState->usedObj = NULL;
    gLakituState.shakeMagnitude[0] = 0;
    gLakituState.shakeMagnitude[1] = 0;
    gLakituState.shakeMagnitude[2] = 0;
    gLakituState.unusedVec2[0] = 0;
    gLakituState.unusedVec2[1] = 0;
    gLakituState.unusedVec2[2] = 0;
    gLakituState.unusedVec1[0] = 0.f;
    gLakituState.unusedVec1[1] = 0.f;
    gLakituState.unusedVec1[2] = 0.f;
    gLakituState.lastFrameAction = 0;
    set_fov_function(CAM_FOV_DEFAULT);
    sFOVState.fov = 45.f;
    sFOVState.fovOffset = 0.f;
    sFOVState.unusedIsSleeping = 0;
    sFOVState.shakeAmplitude = 0.f;
    sFOVState.shakePhase = 0;
    sObjectCutscene = CUTSCENE_NONE;
    gRecentCutscene = CUTSCENE_NONE;
}

void init_camera(struct Camera *c) {
    struct Surface *floor = NULL;
    Vec3f marioOffset;
    s32 i;

    sCreditsPlayer2Pitch = 0;
    sCreditsPlayer2Yaw = 0;
    gPrevLevel = gCurrLevelArea / 16;
    gCurrLevelArea = gCurrLevelNum * 16 + gCurrentArea->index;
    sSelectionFlags &= CAM_MODE_MARIO_SELECTED;
    sFramesPaused = 0;
    gLakituState.mode = c->mode;
    gLakituState.defMode = c->defMode;
    gLakituState.posHSpeed = 0.3f;
    gLakituState.posVSpeed = 0.3f;
    gLakituState.focHSpeed = 0.8f;
    gLakituState.focVSpeed = 0.3f;
    gLakituState.roll = 0;
    gLakituState.keyDanceRoll = 0;
    gLakituState.unused = 0;
    sStatusFlags &= ~CAM_FLAG_SMOOTH_MOVEMENT;
    vec3_zero(sCastleEntranceOffset);
    vec3_zero(sPlayer2FocusOffset);
    find_mario_floor_and_ceil(&sMarioGeometry);
    sMarioGeometry.prevFloorHeight = sMarioGeometry.currFloorHeight;
    sMarioGeometry.prevCeilHeight = sMarioGeometry.currCeilHeight;
    sMarioGeometry.prevFloor = sMarioGeometry.currFloor;
    sMarioGeometry.prevCeil = sMarioGeometry.currCeil;
    sMarioGeometry.prevFloorType = sMarioGeometry.currFloorType;
    sMarioGeometry.prevCeilType = sMarioGeometry.currCeilType;
    for (i = 0; i < 32; i++) {
        sCurCreditsSplinePos[i].index = -1;
        sCurCreditsSplineFocus[i].index = -1;
    }
    sCutsceneSplineSegment = 0;
    sCutsceneSplineSegmentProgress = 0.f;
    sHandheldShakeInc = 0.f;
    sHandheldShakeTimer = 0.f;
    sHandheldShakeMag = 0;
    for (i = 0; i < 4; i++) {
        sHandheldShakeSpline[i].index = -1;
    }
    sHandheldShakePitch = 0;
    sHandheldShakeYaw = 0;
    sHandheldShakeRoll = 0;
    c->cutscene = CUTSCENE_NONE;
    marioOffset[0] = 0.f;
    marioOffset[1] = 125.f;
    marioOffset[2] = 400.f;

    // Set the camera's starting position or start a cutscene for certain levels
    switch (gCurrLevelNum) {
        // Calls the initial cutscene when you enter Bowser battle levels
        // Note: This replaced an "old" way to call these cutscenes using
        // a camEvent value: CAM_EVENT_BOWSER_INIT
        case LEVEL_BOWSER_1:
            // Since Bowser 1 has a demo entry, check for it
            // If it is, then set CamAct to the end to directly activate Bowser
            // If it isn't, then start cutscene
            if (gCurrDemoInput == NULL) {
                start_cutscene(c, CUTSCENE_ENTER_BOWSER_ARENA);
            } else if (gSecondCameraFocus != NULL) {
                gSecondCameraFocus->oBowserCamAct = BOWSER_CAM_ACT_END;
            }
            break;
        case LEVEL_BOWSER_2:
            start_cutscene(c, CUTSCENE_ENTER_BOWSER_ARENA);
            break;
        case LEVEL_BOWSER_3:
            start_cutscene(c, CUTSCENE_ENTER_BOWSER_ARENA);
            break;

#ifdef ENABLE_VANILLA_CAM_PROCESSING
        //! Hardcoded position checks determine which cutscene to play when Mario enters castle grounds.
        case LEVEL_CASTLE_GROUNDS:
            if (is_within_100_units_of_mario(-1328.f, 260.f, 4664.f) != 1) {
                marioOffset[0] = -400.f;
                marioOffset[2] = -800.f;
            }
            if (is_within_100_units_of_mario(-6901.f, 2376.f, -6509.f) == 1) {
                start_cutscene(c, CUTSCENE_EXIT_WATERFALL);
            }
            if (is_within_100_units_of_mario(5408.f, 4500.f, 3637.f) == 1) {
                start_cutscene(c, CUTSCENE_EXIT_FALL_WMOTR);
            }
            gLakituState.mode = CAMERA_MODE_FREE_ROAM;
            break;
        case LEVEL_SA:
            marioOffset[2] = 200.f;
            break;
        case LEVEL_CASTLE_COURTYARD:
            marioOffset[2] = -300.f;
            break;
        case LEVEL_LLL:
            gCameraMovementFlags |= CAM_MOVE_ZOOMED_OUT;
            break;
        case LEVEL_CASTLE:
            marioOffset[2] = 150.f;
            break;
        case LEVEL_RR:
            vec3f_set(sFixedModeBasePosition, -2985.f, 478.f, -5568.f);
            break;
#endif
    }
    if (c->mode == CAMERA_MODE_8_DIRECTIONS) {
        gCameraMovementFlags |= CAM_MOVE_ZOOMED_OUT;
    }
    switch (gCurrLevelArea) {
#ifdef ENABLE_VANILLA_CAM_PROCESSING
        case AREA_SSL_EYEROK:
            vec3f_set(marioOffset, 0.f, 500.f, -100.f);
            break;
        case AREA_CCM_SLIDE:
            marioOffset[2] = -300.f;
            break;
        case AREA_THI_WIGGLER:
            marioOffset[2] = -300.f;
            break;
        case AREA_SL_IGLOO:
            marioOffset[2] = -300.f;
            break;
        case AREA_SL_OUTSIDE:
            if (is_within_100_units_of_mario(257.f, 2150.f, 1399.f) == 1) {
                marioOffset[2] = -300.f;
            }
            break;
        case AREA_CCM_OUTSIDE:
            gCameraMovementFlags |= CAM_MOVE_ZOOMED_OUT;
            break;
        case AREA_TTM_OUTSIDE:
            gLakituState.mode = CAMERA_MODE_RADIAL;
            break;
#endif
    }

    // Set the camera pos to marioOffset (relative to Mario), added to Mario's position
    offset_rotated(c->pos, sMarioCamState->pos, marioOffset, sMarioCamState->faceAngle);
    if (c->mode != CAMERA_MODE_BEHIND_MARIO) {
        c->pos[1] = find_floor(sMarioCamState->pos[0], sMarioCamState->pos[1] + 100.f,
                               sMarioCamState->pos[2], &floor) + 125.f;
    }
    vec3f_copy(c->focus, sMarioCamState->pos);
    vec3f_copy(gLakituState.curPos, c->pos);
    vec3f_copy(gLakituState.curFocus, c->focus);
    vec3f_copy(gLakituState.goalPos, c->pos);
    vec3f_copy(gLakituState.goalFocus, c->focus);
    vec3f_copy(gLakituState.pos, c->pos);
    vec3f_copy(gLakituState.focus, c->focus);
    if (c->mode == CAMERA_MODE_FIXED) {
        set_fixed_cam_axis_sa_lobby(c->mode);
    }
    store_lakitu_cam_info_for_c_up(c);
    gLakituState.yaw = calculate_yaw(c->focus, c->pos);
    gLakituState.nextYaw = gLakituState.yaw;
    c->yaw = gLakituState.yaw;
    c->nextYaw = gLakituState.yaw;
#ifdef PUPPYCAM
    puppycam_init();
#endif
}

void select_mario_cam_mode(void) {
    sSelectionFlags = CAM_MODE_MARIO_SELECTED;
}

void object_pos_to_vec3f(Vec3f dst, struct Object *obj) {
    dst[0] = obj->oPosX;
    dst[1] = obj->oPosY;
    dst[2] = obj->oPosZ;
}

void vec3f_to_object_pos(struct Object *obj, Vec3f src) {
    obj->oPosX = src[0];
    obj->oPosY = src[1];
    obj->oPosZ = src[2];
}

/**
 * Produces values using a cubic b-spline curve. Basically Q is the used output,
 * u is a value between 0 and 1 that represents the position along the spline,
 * and a0-a3 are parameters that define the spline.
 *
 * The spline is described at www2.cs.uregina.ca/~anima/408/Notes/Interpolation/UniformBSpline.htm
 */
void evaluate_cubic_spline(f32 u, Vec3f Q, Vec3f spline1, Vec3f spline2, Vec3f spline3, Vec3f spline4) {
    f32 B[4];
    if (u > 1.0f) u = 1.0f;

    register f32 nu = 1.0f - u;
    register f32 su = sqr(u);
    register f32 hcu = (su * u) / 2.0f;

    B[0] = (nu * nu * nu) / 6.0f;
    B[1] = hcu - su + (2.0f / 3.0f);
    B[2] = -hcu + (su / 2.0f) + (u / 2.0f) + (1.0f / 6.0f);
    B[3] =  hcu / 3.0f;

    Q[0] = (B[0] * spline1[0]) + (B[1] * spline2[0]) + (B[2] * spline3[0]) + (B[3] * spline4[0]);
    Q[1] = (B[0] * spline1[1]) + (B[1] * spline2[1]) + (B[2] * spline3[1]) + (B[3] * spline4[1]);
    Q[2] = (B[0] * spline1[2]) + (B[1] * spline2[2]) + (B[2] * spline3[2]) + (B[3] * spline4[2]);
}

/**
 * Computes the point that is `progress` percent of the way through segment `splineSegment` of `spline`,
 * and stores the result in `p`. `progress` and `splineSegment` are updated if `progress` becomes >= 1.0.
 *
 * When neither of the next two points' speeds == 0, the number of frames is between 1 and 255. Otherwise
 * it's infinite.
 *
 * To calculate the number of frames it will take to progress through a spline segment:
 * If the next two speeds are the same and nonzero, it's 1.0 / firstSpeed.
 *
 * s1 and s2 are short hand for first/secondSpeed. The progress at any frame n is defined by a recurrency relation:
 *      p(n+1) = (s2 - s1 + 1) * p(n) + s1
 * Which can be written as
 *      p(n) = (s2 * ((s2 - s1 + 1)^(n) - 1)) / (s2 - s1)
 *
 * Solving for the number of frames:
 *      n = log(((s2 - s1) / s1) + 1) / log(s2 - s1 + 1)
 *
 * @return 1 if the point has reached the end of the spline, when `progress` reaches 1.0 or greater, and
 * the 4th CutsceneSplinePoint in the current segment away from spline[splineSegment] has an index of -1.
 */
s32 move_point_along_spline(Vec3f p, struct CutsceneSplinePoint spline[], s16 *splineSegment, f32 *progress) {
    s32 finished = FALSE;
    Vec3f controlPoints[4];
    s32 i = 0;
    f32 u = *progress;
    f32 progressChange;
    f32 firstSpeed = 0;
    f32 secondSpeed = 0;
    s32 segment = *splineSegment;

    if (*splineSegment < 0) {
        segment = 0;
        u = 0;
    }
    if (spline[segment].index == -1 || spline[segment + 1].index == -1 || spline[segment + 2].index == -1) {
        return 1;
    }

    for (i = 0; i < 4; i++) {
        controlPoints[i][0] = spline[segment + i].point[0];
        controlPoints[i][1] = spline[segment + i].point[1];
        controlPoints[i][2] = spline[segment + i].point[2];
    }
    evaluate_cubic_spline(u, p, controlPoints[0], controlPoints[1], controlPoints[2], controlPoints[3]);

    if (spline[*splineSegment + 1].speed != 0) {
        firstSpeed = 1.0f / spline[*splineSegment + 1].speed;
    }
    if (spline[*splineSegment + 2].speed != 0) {
        secondSpeed = 1.0f / spline[*splineSegment + 2].speed;
    }
    progressChange = (secondSpeed - firstSpeed) * *progress + firstSpeed;

    if (1 <= (*progress += progressChange)) {
        (*splineSegment)++;
        if (spline[*splineSegment + 3].index == -1) {
            *splineSegment = 0;
            finished = 1;
        }
        (*progress)--;
    }
    return finished;
}

/**
 * If `selection` is 0, just get the current selection
 * If `selection` is 1, select 'Mario' as the alt mode.
 * If `selection` is 2, select 'fixed' as the alt mode.
 *
 * @return the current selection
 */
s32 cam_select_alt_mode(s32 selection) {
    s32 mode = CAM_SELECTION_FIXED;

    if (selection == CAM_SELECTION_MARIO) {
        if (!(sSelectionFlags & CAM_MODE_MARIO_SELECTED)) {
            sSelectionFlags |= CAM_MODE_MARIO_SELECTED;
        }
        sCameraSoundFlags |= CAM_SOUND_UNUSED_SELECT_MARIO;
    }

    // The alternate mode is up-close, but the player just selected fixed in the pause menu
    if (selection == CAM_SELECTION_FIXED && (sSelectionFlags & CAM_MODE_MARIO_SELECTED)) {
        // So change to normal mode in case the user paused in up-close mode
        set_cam_angle(CAM_ANGLE_LAKITU);
        sSelectionFlags &= ~CAM_MODE_MARIO_SELECTED;
        sCameraSoundFlags |= CAM_SOUND_UNUSED_SELECT_FIXED;
    }

    if (sSelectionFlags & CAM_MODE_MARIO_SELECTED) {
        mode = CAM_SELECTION_MARIO;
    }
    return mode;
}

/**
 * Sets the camera angle to either Lakitu or Mario mode. Returns the current mode.
 *
 * If `mode` is 0, just returns the current mode.
 * If `mode` is 1, start Mario mode
 * If `mode` is 2, start Lakitu mode
 */
s32 set_cam_angle(s32 mode) {
    s32 curMode = CAM_ANGLE_LAKITU;

    // Switch to Mario mode
    if (mode == CAM_ANGLE_MARIO && !(sSelectionFlags & CAM_MODE_MARIO_ACTIVE)) {
        sSelectionFlags |= CAM_MODE_MARIO_ACTIVE;
        if (gCameraMovementFlags & CAM_MOVE_ZOOMED_OUT) {
            sSelectionFlags |= CAM_MODE_LAKITU_WAS_ZOOMED_OUT;
            gCameraMovementFlags &= ~CAM_MOVE_ZOOMED_OUT;
        }
        sCameraSoundFlags |= CAM_SOUND_MARIO_ACTIVE;
    }

    // Switch back to normal mode
    if (mode == CAM_ANGLE_LAKITU && (sSelectionFlags & CAM_MODE_MARIO_ACTIVE)) {
        sSelectionFlags &= ~CAM_MODE_MARIO_ACTIVE;
        if (sSelectionFlags & CAM_MODE_LAKITU_WAS_ZOOMED_OUT) {
            sSelectionFlags &= ~CAM_MODE_LAKITU_WAS_ZOOMED_OUT;
            gCameraMovementFlags |= CAM_MOVE_ZOOMED_OUT;
        } else {
            gCameraMovementFlags &= ~CAM_MOVE_ZOOMED_OUT;
        }
        sCameraSoundFlags |= CAM_SOUND_NORMAL_ACTIVE;
    }
    if (sSelectionFlags & CAM_MODE_MARIO_ACTIVE) {
        curMode = CAM_ANGLE_MARIO;
    }
    return curMode;
}

/**
 * Enables the handheld shake effect for this frame.
 *
 * @see shake_camera_handheld()
 */
void set_handheld_shake(u8 mode) {
    switch (mode) {
        // They're not in numerical order because that would be too simple...
        case HAND_CAM_SHAKE_CUTSCENE: // Lowest increment
            sHandheldShakeMag = 0x600;
            sHandheldShakeInc = 0.04f;
            break;
        case HAND_CAM_SHAKE_LOW: // Lowest magnitude
            sHandheldShakeMag = 0x300;
            sHandheldShakeInc = 0.06f;
            break;
        case HAND_CAM_SHAKE_HIGH: // Highest mag and inc
            sHandheldShakeMag = 0x1000;
            sHandheldShakeInc = 0.1f;
            break;
        case HAND_CAM_SHAKE_UNUSED: // Never used
            sHandheldShakeMag = 0x600;
            sHandheldShakeInc = 0.07f;
            break;
        case HAND_CAM_SHAKE_HANG_OWL: // exactly the same as UNUSED...
            sHandheldShakeMag = 0x600;
            sHandheldShakeInc = 0.07f;
            break;
        case HAND_CAM_SHAKE_STAR_DANCE: // Slightly steadier than HANG_OWL and UNUSED
            sHandheldShakeMag = 0x400;
            sHandheldShakeInc = 0.07f;
            break;
        default:
            sHandheldShakeMag = 0x0;
            sHandheldShakeInc = 0.f;
    }
}

/**
 * When sHandheldShakeMag is nonzero, this function adds small random offsets to `focus` every time
 * sHandheldShakeTimer increases above 1.0, simulating the camera shake caused by unsteady hands.
 *
 * This function must be called every frame in order to actually apply the effect, since the effect's
 * mag and inc are set to 0 every frame at the end of this function.
 */
void shake_camera_handheld(Vec3f pos, Vec3f focus) {
    s32 i;
    Vec3f shakeOffset;
    Vec3f shakeSpline[4];
    f32 dist;
    s16 pitch, yaw;

    if (sHandheldShakeMag == 0) {
        vec3_zero(shakeOffset);
    } else {
        for (i = 0; i < 4; i++) {
            shakeSpline[i][0] = sHandheldShakeSpline[i].point[0];
            shakeSpline[i][1] = sHandheldShakeSpline[i].point[1];
            shakeSpline[i][2] = sHandheldShakeSpline[i].point[2];
        }
        evaluate_cubic_spline(sHandheldShakeTimer, shakeOffset, shakeSpline[0],
                              shakeSpline[1], shakeSpline[2], shakeSpline[3]);
        if (1.f <= (sHandheldShakeTimer += sHandheldShakeInc)) {
            // The first 3 control points are always (0,0,0), so the random spline is always just a
            // straight line
            for (i = 0; i < 3; i++) {
                vec3s_copy(sHandheldShakeSpline[i].point, sHandheldShakeSpline[i + 1].point);
            }
            random_vec3s(sHandheldShakeSpline[3].point, sHandheldShakeMag, sHandheldShakeMag, sHandheldShakeMag / 2);
            sHandheldShakeTimer -= 1.f;

            // Code dead, this is set to be 0 before it is used.
            sHandheldShakeInc = random_float() * 0.5f;
            if (sHandheldShakeInc < 0.02f) {
                sHandheldShakeInc = 0.02f;
            }
        }
    }

    approach_s16_asymptotic_bool(&sHandheldShakePitch, shakeOffset[0], 0x08);
    approach_s16_asymptotic_bool(&sHandheldShakeYaw, shakeOffset[1], 0x08);
    approach_s16_asymptotic_bool(&sHandheldShakeRoll, shakeOffset[2], 0x08);

    if (sHandheldShakePitch | sHandheldShakeYaw) {
        vec3f_get_dist_and_angle(pos, focus, &dist, &pitch, &yaw);
        pitch += sHandheldShakePitch;
        yaw += sHandheldShakeYaw;
        vec3f_set_dist_and_angle(pos, focus, dist, pitch, yaw);
    }

    // Unless called every frame, the effect will stop after the first time.
    sHandheldShakeMag = 0;
    sHandheldShakeInc = 0.0f;
}

/**
 * Updates C Button input state and stores it in `currentState`
 */
s32 find_c_buttons_pressed(u16 currentState, u16 buttonsPressed, u16 buttonsDown) {
    buttonsPressed &= CBUTTON_MASK;
    buttonsDown &= CBUTTON_MASK;

    if (buttonsPressed & L_CBUTTONS) {
        currentState |= L_CBUTTONS;
        currentState &= ~R_CBUTTONS;
    }
    if (!(buttonsDown & L_CBUTTONS)) {
        currentState &= ~L_CBUTTONS;
    }

    if (buttonsPressed & R_CBUTTONS) {
        currentState |= R_CBUTTONS;
        currentState &= ~L_CBUTTONS;
    }
    if (!(buttonsDown & R_CBUTTONS)) {
        currentState &= ~R_CBUTTONS;
    }

    if (buttonsPressed & U_CBUTTONS) {
        currentState |= U_CBUTTONS;
        currentState &= ~D_CBUTTONS;
    }
    if (!(buttonsDown & U_CBUTTONS)) {
        currentState &= ~U_CBUTTONS;
    }

    if (buttonsPressed & D_CBUTTONS) {
        currentState |= D_CBUTTONS;
        currentState &= ~U_CBUTTONS;
    }
    if (!(buttonsDown & D_CBUTTONS)) {
        currentState &= ~D_CBUTTONS;
    }

    return currentState;
}

/**
 * Determine which icon to show on the HUD
 */
s32 update_camera_hud_status(struct Camera *c) {
    s16 status = CAM_STATUS_NONE;

    if (c->cutscene != CUTSCENE_NONE
        || ((gPlayer1Controller->buttonDown & R_TRIG) && cam_select_alt_mode(0) == CAM_SELECTION_FIXED)) {
        status |= CAM_STATUS_FIXED;
    } else if (set_cam_angle(0) == CAM_ANGLE_MARIO) {
        status |= CAM_STATUS_MARIO;
    } else {
        status |= CAM_STATUS_LAKITU;
    }
    if (gCameraMovementFlags & CAM_MOVE_ZOOMED_OUT) {
        status |= CAM_STATUS_C_DOWN;
    }
    if (gCameraMovementFlags & CAM_MOVE_C_UP_MODE) {
        status |= CAM_STATUS_C_UP;
    }
    set_hud_camera_status(status);
    return status;
}

/**
 * Check `pos` for collisions within `radius`, and update `pos`
 *
 * @return the number of collisions found
 */
s32 collide_with_walls(Vec3f pos, f32 offsetY, f32 radius) {
    struct WallCollisionData collisionData;
    struct Surface *wall = NULL;
    f32 normX, normY, normZ;
    f32 originOffset;
    f32 offset;
    f32 offsetAbsolute;
    Vec3f newPos[MAX_REFERENCED_WALLS];
    s32 i;
    s32 numCollisions = 0;

    collisionData.x = pos[0];
    collisionData.y = pos[1];
    collisionData.z = pos[2];
    collisionData.radius = radius;
    collisionData.offsetY = offsetY;
    numCollisions = find_wall_collisions(&collisionData);
    if (numCollisions != 0) {
        for (i = 0; i < collisionData.numWalls; i++) {
            wall = collisionData.walls[collisionData.numWalls - 1];
            vec3f_copy(newPos[i], pos);
            normX = wall->normal.x;
            normY = wall->normal.y;
            normZ = wall->normal.z;
            originOffset = wall->originOffset;
            offset = normX * newPos[i][0] + normY * newPos[i][1] + normZ * newPos[i][2] + originOffset;
            offsetAbsolute = absf(offset);
            if (offsetAbsolute < radius) {
                newPos[i][0] += normX * (radius - offset);
                newPos[i][2] += normZ * (radius - offset);
                vec3f_copy(pos, newPos[i]);
            }
        }
    }
    return numCollisions;
}

/**
 * Compare a vector to a position, return TRUE if they match.
 */
s32 vec3f_compare(Vec3f pos, f32 posX, f32 posY, f32 posZ) {
    return pos[0] == posX
        && pos[1] == posY
        && pos[2] == posZ;
}

void clamp_pitch(Vec3f from, Vec3f to, s16 maxPitch, s16 minPitch) {
    s16 pitch;
    s16 yaw;
    f32 dist;

    vec3f_get_dist_and_angle(from, to, &dist, &pitch, &yaw);
    pitch = CLAMP(pitch, minPitch, maxPitch);
    vec3f_set_dist_and_angle(from, to, dist, pitch, yaw);
}

s32 is_within_100_units_of_mario(f32 posX, f32 posY, f32 posZ) {
    Vec3f pos;
    vec3f_set(pos, posX, posY, posZ);

    return calc_abs_dist_squared(sMarioCamState->pos, pos) < sqr(100.f);
}

s32 set_or_approach_f32_asymptotic(f32 *dst, f32 goal, f32 scale) {
    if (sStatusFlags & CAM_FLAG_SMOOTH_MOVEMENT) {
        approach_f32_asymptotic_bool(dst, goal, scale);
    } else {
        *dst = goal;
    }
    if (*dst == goal) {
        return FALSE;
    } else {
        return TRUE;
    }
}

/**
 * Applies the approach_f32_asymptotic_bool function to each of the X, Y, & Z components of the given
 * vector.
 */
void approach_vec3f_asymptotic(Vec3f current, Vec3f target, f32 xMul, f32 yMul, f32 zMul) {
    approach_f32_asymptotic_bool(&current[0], target[0], xMul);
    approach_f32_asymptotic_bool(&current[1], target[1], yMul);
    approach_f32_asymptotic_bool(&current[2], target[2], zMul);
}

/**
 * Applies the set_or_approach_f32_asymptotic_bool function to each of the X, Y, & Z components of the
 * given vector.
 */
void set_or_approach_vec3f_asymptotic(Vec3f dst, Vec3f goal, f32 xMul, f32 yMul, f32 zMul) {
    set_or_approach_f32_asymptotic(&dst[0], goal[0], xMul);
    set_or_approach_f32_asymptotic(&dst[1], goal[1], yMul);
    set_or_approach_f32_asymptotic(&dst[2], goal[2], zMul);
}

/**
 * Applies the approach_s32_asymptotic function to each of the X, Y, & Z components of the given
 * vector.
 */
void approach_vec3s_asymptotic(Vec3s current, Vec3s target, s16 xMul, s16 yMul, s16 zMul) {
    approach_s16_asymptotic_bool(&current[0], target[0], xMul);
    approach_s16_asymptotic_bool(&current[1], target[1], yMul);
    approach_s16_asymptotic_bool(&current[2], target[2], zMul);
}

s32 camera_approach_s16_symmetric_bool(s16 *current, s16 target, s16 increment) {
    s16 dist = target - *current;

    if (increment < 0) {
        increment = -1 * increment;
    }
    if (dist > 0) {
        dist -= increment;
        if (dist >= 0) {
            *current = target - dist;
        } else {
            *current = target;
        }
    } else {
        dist += increment;
        if (dist <= 0) {
            *current = target - dist;
        } else {
            *current = target;
        }
    }
    if (*current == target) {
        return FALSE;
    } else {
        return TRUE;
    }
}

s32 camera_approach_s16_symmetric(s16 current, s16 target, s16 increment) {
    s16 dist = target - current;

    if (increment < 0) {
        increment = -1 * increment;
    }
    if (dist > 0) {
        dist -= increment;
        if (dist >= 0) {
            current = target - dist;
        } else {
            current = target;
        }
    } else {
        dist += increment;
        if (dist <= 0) {
            current = target - dist;
        } else {
            current = target;
        }
    }
    return current;
}

s32 set_or_approach_s16_symmetric(s16 *current, s16 target, s16 increment) {
    if (sStatusFlags & CAM_FLAG_SMOOTH_MOVEMENT) {
        return camera_approach_s16_symmetric_bool(current, target, increment);
    } else {
        *current = target;
    }
    if (*current == target) {
        return FALSE;
    } else {
        return TRUE;
    }
}

/**
 * Approaches a value by a given increment, returns FALSE if the target is reached.
 * Appears to be a strange way of implementing approach_f32_symmetric from object_helpers.c.
 * It could possibly be an older version of the function
 */
s32 camera_approach_f32_symmetric_bool(f32 *current, f32 target, f32 increment) {
    f32 dist = target - *current;

    if (increment < 0) {
        increment = -1 * increment;
    }
    if (dist > 0) {
        dist -= increment;
        if (dist > 0) {
            *current = target - dist;
        } else {
            *current = target;
        }
    } else {
        dist += increment;
        if (dist < 0) {
            *current = target - dist;
        } else {
            *current = target;
        }
    }
    if (*current == target) {
        return FALSE;
    } else {
        return TRUE;
    }
}

/**
 * Nearly the same as the above function, this one returns the new value in place of a bool.
 */
f32 camera_approach_f32_symmetric(f32 current, f32 target, f32 increment) {
    f32 dist = target - current;

    if (increment < 0) {
        increment = -1 * increment;
    }
    if (dist > 0) {
        dist -= increment;
        if (dist > 0) {
            current = target - dist;
        } else {
            current = target;
        }
    } else {
        dist += increment;
        if (dist < 0) {
            current = target - dist;
        } else {
            current = target;
        }
    }
    return current;
}

/**
 * Generate a vector with all three values about zero. The
 * three ranges determine how wide the range about zero.
 */
void random_vec3s(Vec3s dst, s16 xRange, s16 yRange, s16 zRange) {
    f32 randomFloat;
    f32 tempXRange;
    f32 tempYRange;
    f32 tempZRange;

    randomFloat = random_float();
    tempXRange = xRange;
    dst[0] = randomFloat * tempXRange - tempXRange / 2;

    randomFloat = random_float();
    tempYRange = yRange;
    dst[1] = randomFloat * tempYRange - tempYRange / 2;

    randomFloat = random_float();
    tempZRange = zRange;
    dst[2] = randomFloat * tempZRange - tempZRange / 2;
}

/**
 * Decrease value by multiplying it by the distance from (`posX`, `posY`, `posZ`) to
 * the camera divided by `maxDist`
 *
 * @return the reduced value
 */
s16 reduce_by_dist_from_camera(s16 value, f32 maxDist, f32 posX, f32 posY, f32 posZ) {
    Vec3f pos;
    f32 dist;
    s16 pitch, yaw;
    s16 goalPitch, goalYaw;
    s16 result = 0;
    // Direction from pos to (Lakitu's) goalPos
    f32 goalDX = gLakituState.goalPos[0] - posX;
    f32 goalDY = gLakituState.goalPos[1] - posY;
    f32 goalDZ = gLakituState.goalPos[2] - posZ;

    dist = sqrtf(goalDX * goalDX + goalDY * goalDY + goalDZ * goalDZ);
    if (maxDist > dist) {
        pos[0] = posX;
        pos[1] = posY;
        pos[2] = posZ;
        vec3f_get_dist_and_angle(gLakituState.goalPos, pos, &dist, &pitch, &yaw);
        if (dist < maxDist) {
            calculate_angles(gLakituState.goalPos, gLakituState.goalFocus, &goalPitch, &goalYaw);
            pitch -= goalPitch;
            yaw -= goalYaw;
            dist -= 2000.f;
            if (dist < 0.f) {
                dist = 0.f;
            }
            maxDist -= 2000.f;
            if (maxDist < 2000.f) {
                maxDist = 2000.f;
            }
            result = value * (1.f - dist / maxDist);
            if (pitch < -0x1800 || pitch > 0x400 ||
                yaw   < -0x1800 || yaw >   0x1800) {
                result /= 2;
            }
        }
    }
    return result;
}

s32 clamp_positions_and_find_yaw(Vec3f pos, Vec3f origin, f32 xMax, f32 xMin, f32 zMax, f32 zMin) {
    s16 yaw = gCamera->nextYaw;

    if (pos[0] >= xMax) {
        pos[0] = xMax;
    }
    if (pos[0] <= xMin) {
        pos[0] = xMin;
    }
    if (pos[2] >= zMax) {
        pos[2] = zMax;
    }
    if (pos[2] <= zMin) {
        pos[2] = zMin;
    }
    yaw = calculate_yaw(origin, pos);
    return yaw;
}

/**
 * The yaw passed here is the yaw of the direction FROM Mario TO Lakitu.
 *
 * wallYaw always has 90 degrees added to it before this is called -- it's parallel to the wall.
 *
 * @return the new yaw from Mario to rotate towards.
 *
 * @warning this is jank. It actually returns the yaw that will rotate further INTO the wall. So, the
 *          developers just add 180 degrees to the result.
 */
s32 calc_avoid_yaw(s16 yawFromMario, s16 wallYaw) {
    s16 yawDiff;
    yawDiff = wallYaw - yawFromMario + DEGREES(90);

    if (yawDiff < 0) {
        // Deflect to the right
        yawFromMario = wallYaw;
    } else {
        // Note: this favors the left side if the wall is exactly perpendicular to the camera.
        // Deflect to the left
        yawFromMario = wallYaw + DEGREES(180);
    }
    return yawFromMario;
}

/**
 * Checks if `surf` is within the rect prism defined by xMax, yMax, and zMax
 *
 * @param surf surface to check
 * @param xMax absolute-value max size in x, set to -1 to ignore
 * @param yMax absolute-value max size in y, set to -1 to ignore
 * @param zMax absolute-value max size in z, set to -1 to ignore
 */
s32 is_surf_within_bounding_box(struct Surface *surf, f32 xMax, f32 yMax, f32 zMax) {
    // Surface vertex coordinates
    Vec3s sx, sy, sz;
    // Max delta between x, y, and z
    s16 dxMax = 0;
    s16 dyMax = 0;
    s16 dzMax = 0;
    // Current deltas between x, y, and z
    f32 dx, dy, dz;
    s32 i, j;
    // result
    s32 smaller = FALSE;

    sx[0] = surf->vertex1[0];
    sx[1] = surf->vertex2[0];
    sx[2] = surf->vertex3[0];
    sy[0] = surf->vertex1[1];
    sy[1] = surf->vertex2[1];
    sy[2] = surf->vertex3[1];
    sz[0] = surf->vertex1[2];
    sz[1] = surf->vertex2[2];
    sz[2] = surf->vertex3[2];

    for (i = 0; i < 3; i++) {
        j = i + 1;
        if (j >= 3) {
            j = 0;
        }
        dx = abss(sx[i] - sx[j]);
        if (dx > dxMax) {
            dxMax = dx;
        }
        dy = abss(sy[i] - sy[j]);
        if (dy > dyMax) {
            dyMax = dy;
        }
        dz = abss(sz[i] - sz[j]);
        if (dz > dzMax) {
            dzMax = dz;
        }
    }
    if (yMax != -1.f) {
        if (dyMax < yMax) {
            smaller = TRUE;
        }
    }
    if (xMax != -1.f && zMax != -1.f) {
        if (dxMax < xMax && dzMax < zMax) {
            smaller = TRUE;
        }
    }
    return smaller;
}

/**
 * Checks if `pos` is behind the surface, using the dot product.
 *
 * Because the function only uses `surf`s first vertex, some surfaces can shadow others.
 */
s32 is_behind_surface(Vec3f pos, struct Surface *surf) {
    s32 behindSurface = 0;
    // Surface normal
    f32 normX = (surf->vertex2[1] - surf->vertex1[1]) * (surf->vertex3[2] - surf->vertex2[2]) -
                (surf->vertex3[1] - surf->vertex2[1]) * (surf->vertex2[2] - surf->vertex1[2]);
    f32 normY = (surf->vertex2[2] - surf->vertex1[2]) * (surf->vertex3[0] - surf->vertex2[0]) -
                (surf->vertex3[2] - surf->vertex2[2]) * (surf->vertex2[0] - surf->vertex1[0]);
    f32 normZ = (surf->vertex2[0] - surf->vertex1[0]) * (surf->vertex3[1] - surf->vertex2[1]) -
                (surf->vertex3[0] - surf->vertex2[0]) * (surf->vertex2[1] - surf->vertex1[1]);
    f32 dirX = surf->vertex1[0] - pos[0];
    f32 dirY = surf->vertex1[1] - pos[1];
    f32 dirZ = surf->vertex1[2] - pos[2];

    if (dirX * normX + dirY * normY + dirZ * normZ < 0) {
        behindSurface = 1;
    }
    return behindSurface;
}

/**
 * Checks if the whole circular sector is behind the surface.
 */
s32 is_range_behind_surface(Vec3f from, Vec3f to, struct Surface *surf, s16 range, s16 surfType) {
    s32 behindSurface = TRUE;
    s32 leftBehind = 0;
    s32 rightBehind = 0;
    f32 checkDist;
    s16 checkPitch;
    s16 checkYaw;
    Vec3f checkPos;

    if (surf != NULL) {
        if (surfType == SURFACE_NULL || surf->type != surfType) {
            if (range == 0) {
                behindSurface = is_behind_surface(to, surf);
            } else {
                vec3f_get_dist_and_angle(from, to, &checkDist, &checkPitch, &checkYaw);
                vec3f_set_dist_and_angle(from, checkPos, checkDist, checkPitch, checkYaw + range);
                leftBehind = is_behind_surface(checkPos, surf);
                vec3f_set_dist_and_angle(from, checkPos, checkDist, checkPitch, checkYaw - range);
                rightBehind = is_behind_surface(checkPos, surf);
                behindSurface = leftBehind * rightBehind;
            }
        }
    }
    return behindSurface;
}

s32 is_mario_behind_surface(UNUSED struct Camera *c, struct Surface *surf) {
    s32 behindSurface = is_behind_surface(sMarioCamState->pos, surf);

    return behindSurface;
}

/**
 * Calculates the distance between two points and sets a vector to a point
 * scaled along a line between them. Typically, somewhere in the middle.
 */
void scale_along_line(Vec3f dst, Vec3f from, Vec3f to, f32 scale) {
    dst[0] = (to[0] - from[0]) * scale + from[0];
    dst[1] = (to[1] - from[1]) * scale + from[1];
    dst[2] = (to[2] - from[2]) * scale + from[2];
}
/**
 * Effectively created a rectangular prism defined by a vector starting at the center
 * and extending to the corners. If the position is in this box, the function returns true.
 */
s32 is_pos_in_bounds(Vec3f pos, Vec3f center, Vec3f bounds, s16 boundsYaw) {
    Vec3f rel;
    vec3_diff(rel, center, pos);

    rotate_in_xz(rel, rel, boundsYaw);

    return (-bounds[0] < rel[0] && rel[0] < bounds[0] &&
            -bounds[1] < rel[1] && rel[1] < bounds[1] &&
            -bounds[2] < rel[2] && rel[2] < bounds[2]);
}

s16 calculate_pitch(Vec3f from, Vec3f to) {
    f32 dx = to[0] - from[0];
    f32 dy = to[1] - from[1];
    f32 dz = to[2] - from[2];
    s16 pitch = atan2s(sqrtf(dx * dx + dz * dz), dy);

    return pitch;
}

s16 calculate_yaw(Vec3f from, Vec3f to) {
    f32 dx = to[0] - from[0];
    // UNUSED f32 dy = to[1] - from[1];
    f32 dz = to[2] - from[2];

    return atan2s(dz, dx);
}

/**
 * Calculates the pitch and yaw between two vectors.
 */
void calculate_angles(Vec3f from, Vec3f to, s16 *pitch, s16 *yaw) {
    f32 dx = to[0] - from[0];
    f32 dy = to[1] - from[1];
    f32 dz = to[2] - from[2];

    *pitch = atan2s(sqrtf(sqr(dx) + sqr(dz)), dy);
    *yaw = atan2s(dz, dx);
}

/**
 * Finds the distance between two vectors.
 */
f32 calc_abs_dist(Vec3f a, Vec3f b) {
    register f32 distX = b[0] - a[0];
    register f32 distY = b[1] - a[1];
    register f32 distZ = b[2] - a[2];

    return sqrtf(sqr(distX) + sqr(distY) + sqr(distZ));
}

f32 calc_abs_dist_squared(Vec3f a, Vec3f b) {
    register f32 distX = b[0] - a[0];
    register f32 distY = b[1] - a[1];
    register f32 distZ = b[2] - a[2];

    return (sqr(distX) + sqr(distY) + sqr(distZ));
}

/**
 * Finds the horizontal distance between two vectors.
 */
f32 calc_hor_dist(Vec3f a, Vec3f b) {
    register f32 distX = b[0] - a[0];
    register f32 distZ = b[2] - a[2];

    return sqrtf(sqr(distX) + sqr(distZ));
}

/**
 * Rotates a vector in the horizontal plane and copies it to a new vector.
 */
void rotate_in_xz(Vec3f dst, Vec3f src, s16 yaw) {
    register f32 x = src[0];
    register f32 z = src[2];
    register f32 sy = sins(yaw);
    register f32 cy = coss(yaw);

    dst[0] = z * sy + x * cy;
    dst[1] = src[1];
    dst[2] = z * cy - x * sy;
}

/**
 * Rotates a vector in the YZ plane and copies it to a new vector.
 *
 * Note: This function also flips the Z axis, so +Z moves forward, not backward like it would in world
 * space. If possible, use vec3f_set_dist_and_angle()
 */
void rotate_in_yz(Vec3f dst, Vec3f src, s16 pitch) {
    dst[2] = -(src[2] * coss(pitch) - src[1] * sins(pitch));
    dst[1] =   src[2] * sins(pitch) + src[1] * coss(pitch);
    dst[0] =   src[0];
}

/**
 * Start shaking the camera's pitch (up and down)
 */
void set_camera_pitch_shake(s16 mag, s16 decay, s16 inc) {
    if (gLakituState.shakeMagnitude[0] < mag) {
        gLakituState.shakeMagnitude[0] = mag;
        gLakituState.shakePitchDecay = decay;
        gLakituState.shakePitchVel = inc;
    }
}

/**
 * Start shaking the camera's yaw (side to side)
 */
void set_camera_yaw_shake(s16 mag, s16 decay, s16 inc) {
    if (abss(mag) > abss(gLakituState.shakeMagnitude[1])) {
        gLakituState.shakeMagnitude[1] = mag;
        gLakituState.shakeYawDecay = decay;
        gLakituState.shakeYawVel = inc;
    }
}

/**
 * Start shaking the camera's roll (rotate screen clockwise and counterclockwise)
 */
void set_camera_roll_shake(s16 mag, s16 decay, s16 inc) {
    if (gLakituState.shakeMagnitude[2] < mag) {
        gLakituState.shakeMagnitude[2] = mag;
        gLakituState.shakeRollDecay = decay;
        gLakituState.shakeRollVel = inc;
    }
}

/**
 * Start shaking the camera's pitch, but reduce `mag` by it's distance from the camera
 */
void set_pitch_shake_from_point(s16 mag, s16 decay, s16 inc, f32 maxDist, f32 posX, f32 posY, f32 posZ) {
    mag = reduce_by_dist_from_camera(mag, maxDist, posX, posY, posZ);
    if (mag != 0) {
        set_camera_pitch_shake(mag, decay, inc);
    }
}

/**
 * Start shaking the camera's yaw, but reduce `mag` by it's distance from the camera
 */
void set_yaw_shake_from_point(s16 mag, s16 decay, s16 inc, f32 maxDist, f32 posX, f32 posY, f32 posZ) {
    mag = reduce_by_dist_from_camera(mag, maxDist, posX, posY, posZ);
    if (mag != 0) {
        set_camera_yaw_shake(mag, decay, inc);
    }
}

/**
 * Update the shake offset by `increment`
 */
void increment_shake_offset(s16 *offset, s16 increment) {
    if (increment == -0x8000) {
        *offset = (*offset & 0x8000) + 0xC000;
    } else {
        *offset += increment;
    }
}

/**
 * Apply a vertical shake to the camera by adjusting its pitch
 */
void shake_camera_pitch(Vec3f pos, Vec3f focus) {
    f32 dist;
    s16 pitch, yaw;

    if (gLakituState.shakeMagnitude[0] | gLakituState.shakeMagnitude[1]) {
        vec3f_get_dist_and_angle(pos, focus, &dist, &pitch, &yaw);
        pitch += gLakituState.shakeMagnitude[0] * sins(gLakituState.shakePitchPhase);
        vec3f_set_dist_and_angle(pos, focus, dist, pitch, yaw);
        increment_shake_offset(&gLakituState.shakePitchPhase, gLakituState.shakePitchVel);
        if (camera_approach_s16_symmetric_bool(&gLakituState.shakeMagnitude[0], 0,
                                               gLakituState.shakePitchDecay) == 0) {
            gLakituState.shakePitchPhase = 0;
        }
    }
}

/**
 * Apply a horizontal shake to the camera by adjusting its yaw
 */
void shake_camera_yaw(Vec3f pos, Vec3f focus) {
    f32 dist;
    s16 pitch, yaw;

    if (gLakituState.shakeMagnitude[1] != 0) {
        vec3f_get_dist_and_angle(pos, focus, &dist, &pitch, &yaw);
        yaw += gLakituState.shakeMagnitude[1] * sins(gLakituState.shakeYawPhase);
        vec3f_set_dist_and_angle(pos, focus, dist, pitch, yaw);
        increment_shake_offset(&gLakituState.shakeYawPhase, gLakituState.shakeYawVel);
        if (camera_approach_s16_symmetric_bool(&gLakituState.shakeMagnitude[1], 0,
                                               gLakituState.shakeYawDecay) == 0) {
            gLakituState.shakeYawPhase = 0;
        }
    }
}

/**
 * Apply a rotational shake to the camera by adjusting its roll
 */
void shake_camera_roll(s16 *roll) {
    if (gLakituState.shakeMagnitude[2] != 0) {
        increment_shake_offset(&gLakituState.shakeRollPhase, gLakituState.shakeRollVel);
        *roll += gLakituState.shakeMagnitude[2] * sins(gLakituState.shakeRollPhase);
        if (camera_approach_s16_symmetric_bool(&gLakituState.shakeMagnitude[2], 0,
                                               gLakituState.shakeRollDecay) == 0) {
            gLakituState.shakeRollPhase = 0;
        }
    }
}

/**
 * Add an offset to the camera's yaw, used in levels that are inside a rectangular building, like the
 * pyramid or TTC.
 */
s32 offset_yaw_outward_radial(struct Camera *c, s16 areaYaw) {
    s16 yawGoal = DEGREES(60);
    s16 yaw = sModeOffsetYaw;
    Vec3f areaCenter;
    s16 dYaw;
    switch (gCurrLevelArea) {
        case AREA_TTC:
            areaCenter[0] = c->areaCenX;
            areaCenter[1] = sMarioCamState->pos[1];
            areaCenter[2] = c->areaCenZ;
            if (sqr(800.f) > calc_abs_dist_squared(areaCenter, sMarioCamState->pos)) {
                yawGoal = 0x3800;
            }
            break;
        case AREA_SSL_PYRAMID:
            // This mask splits the 360 degrees of yaw into 4 corners. It adds 45 degrees so that the yaw
            // offset at the corner will be 0, but the yaw offset near the center will face more towards
            // the direction Mario is running in.
            yawGoal = (areaYaw & 0xC000) - areaYaw + DEGREES(45);
            if (yawGoal < 0) {
                yawGoal = -yawGoal;
            }
            yawGoal = yawGoal / 32 * 48;
            break;
        case AREA_LLL_OUTSIDE:
            yawGoal = 0;
            break;
    }
    dYaw = gMarioStates[0].forwardVel / 32.f * 128.f;

    if (sAreaYawChange < 0) {
        camera_approach_s16_symmetric_bool(&yaw, -yawGoal, dYaw);
    }
    if (sAreaYawChange > 0) {
        camera_approach_s16_symmetric_bool(&yaw, yawGoal, dYaw);
    }
    // When the final yaw is out of [-60,60] degrees, approach yawGoal faster than dYaw will ever be,
    // making the camera lock in one direction until yawGoal drops below 60 (or Mario presses a C button)
    if (yaw < -DEGREES(60)) {
        //! Maybe they meant to reverse yawGoal's sign?
        camera_approach_s16_symmetric_bool(&yaw, -yawGoal, 0x200);
    }
    if (yaw > DEGREES(60)) {
        //! Maybe they meant to reverse yawGoal's sign?
        camera_approach_s16_symmetric_bool(&yaw, yawGoal, 0x200);
    }
    return yaw;
}

/**
 * Plays the background music that starts while peach reads the intro message.
 */
void cutscene_intro_peach_play_message_music(void) {
    play_music(SEQ_PLAYER_LEVEL, SEQUENCE_ARGS(4, SEQ_EVENT_PEACH_MESSAGE), 0);
}

/**
 * Plays the music that starts after peach fades and Lakitu appears.
 */
void cutscene_intro_peach_play_lakitu_flying_music(void) {
    play_music(SEQ_PLAYER_LEVEL, SEQUENCE_ARGS(15, SEQ_EVENT_CUTSCENE_INTRO), 0);
}

void play_camera_buzz_if_cdown(void) {
    if (gPlayer1Controller->buttonPressed & D_CBUTTONS) {
        play_sound_button_change_blocked();
    }
}

void play_camera_buzz_if_cbutton(void) {
    if (gPlayer1Controller->buttonPressed & CBUTTON_MASK) {
        play_sound_button_change_blocked();
    }
}

void play_camera_buzz_if_c_sideways(void) {
    if (gPlayer1Controller->buttonPressed & (L_CBUTTONS | R_CBUTTONS)) {
        play_sound_button_change_blocked();
    }
}

void play_sound_cbutton_up(void) {
    play_sound(SOUND_MENU_CAMERA_ZOOM_IN, gGlobalSoundSource);
}

void play_sound_cbutton_down(void) {
    play_sound(SOUND_MENU_CAMERA_ZOOM_OUT, gGlobalSoundSource);
}

void play_sound_cbutton_side(void) {
    play_sound(SOUND_MENU_CAMERA_TURN, gGlobalSoundSource);
}

void play_sound_button_change_blocked(void) {
    play_sound(SOUND_MENU_CAMERA_BUZZ, gGlobalSoundSource);
}

void play_sound_rbutton_changed(void) {
    play_sound(SOUND_MENU_CLICK_CHANGE_VIEW, gGlobalSoundSource);
}

void play_sound_if_cam_switched_to_lakitu_or_mario(void) {
    if (sCameraSoundFlags & CAM_SOUND_MARIO_ACTIVE) {
        play_sound_rbutton_changed();
    }
    if (sCameraSoundFlags & CAM_SOUND_NORMAL_ACTIVE) {
        play_sound_rbutton_changed();
    }
    sCameraSoundFlags &= ~(CAM_SOUND_MARIO_ACTIVE | CAM_SOUND_NORMAL_ACTIVE);
}

/**
 * Handles input for radial, outwards radial, parallel tracking, and 8 direction mode.
 */
void radial_camera_input(struct Camera *c) {
    if ((gCameraMovementFlags & CAM_MOVE_ENTERED_ROTATE_SURFACE) || !(gCameraMovementFlags & CAM_MOVE_ROTATE)) {

        // If C-L or C-R are pressed, the camera is rotating
        if (gPlayer1Controller->buttonPressed & (L_CBUTTONS | R_CBUTTONS)) {
            gCameraMovementFlags &= ~CAM_MOVE_ENTERED_ROTATE_SURFACE;
            //  @bug this does not clear the rotation flags set by the surface. It's possible to set
            //       both ROTATE_LEFT and ROTATE_RIGHT, locking the camera.
            //       Ex: If a surface set CAM_MOVE_ROTATE_RIGHT and the user presses C-R, it locks the
            //       camera until a different mode is activated
        }

        // Rotate Right and left
        if (gPlayer1Controller->buttonPressed & R_CBUTTONS) {
            if (sModeOffsetYaw > -0x800) {
                // The camera is now rotating right
                if (!(gCameraMovementFlags & CAM_MOVE_ROTATE_RIGHT)) {
                    gCameraMovementFlags |= CAM_MOVE_ROTATE_RIGHT;
                }

                if (c->mode == CAMERA_MODE_RADIAL) {
                    // if > ~48 degrees, we're rotating for the second time.
                    if (sModeOffsetYaw > 0x22AA) {
                        s2ndRotateFlags |= CAM_MOVE_ROTATE_RIGHT;
                    }

                    if (sModeOffsetYaw == DEGREES(105)) {
                        play_sound_button_change_blocked();
                    } else {
                        play_sound_cbutton_side();
                    }
                } else {
                    if (sModeOffsetYaw == DEGREES(60)) {
                        play_sound_button_change_blocked();
                    } else {
                        play_sound_cbutton_side();
                    }
                }
            } else {
                gCameraMovementFlags |= CAM_MOVE_RETURN_TO_MIDDLE;
                play_sound_cbutton_up();
            }
        }
        if (gPlayer1Controller->buttonPressed & L_CBUTTONS) {
            if (sModeOffsetYaw < 0x800) {
                if (!(gCameraMovementFlags & CAM_MOVE_ROTATE_LEFT)) {
                    gCameraMovementFlags |= CAM_MOVE_ROTATE_LEFT;
                }

                if (c->mode == CAMERA_MODE_RADIAL) {
                    // if < ~48 degrees, we're rotating for the second time.
                    if (sModeOffsetYaw < -0x22AA) {
                        s2ndRotateFlags |= CAM_MOVE_ROTATE_LEFT;
                    }

                    if (sModeOffsetYaw == DEGREES(-105)) {
                        play_sound_button_change_blocked();
                    } else {
                        play_sound_cbutton_side();
                    }
                } else {
                    if (sModeOffsetYaw == DEGREES(-60)) {
                        play_sound_button_change_blocked();
                    } else {
                        play_sound_cbutton_side();
                    }
                }
            } else {
                gCameraMovementFlags |= CAM_MOVE_RETURN_TO_MIDDLE;
                play_sound_cbutton_up();
            }
        }
    }

    // Zoom in / enter C-Up
    if (gPlayer1Controller->buttonPressed & U_CBUTTONS) {
        if (gCameraMovementFlags & CAM_MOVE_ZOOMED_OUT) {
            gCameraMovementFlags &= ~CAM_MOVE_ZOOMED_OUT;
            play_sound_cbutton_up();
        } else {
            set_mode_c_up(c);
        }
    }

    // Zoom out
    if (gPlayer1Controller->buttonPressed & D_CBUTTONS) {
        if (gCameraMovementFlags & CAM_MOVE_ZOOMED_OUT) {
            gCameraMovementFlags |= CAM_MOVE_ALREADY_ZOOMED_OUT;
            play_camera_buzz_if_cdown();
        } else {
            gCameraMovementFlags |= CAM_MOVE_ZOOMED_OUT;
            play_sound_cbutton_down();
        }
    }
}

/**
 * Starts a cutscene dialog. Only has an effect when `trigger` is 1
 */
void trigger_cutscene_dialog(s32 trigger) {
    if (trigger == 1) start_object_cutscene_without_focus(CUTSCENE_READ_MESSAGE);
}

/**
 * Updates the camera based on which C buttons are pressed this frame
 */
void handle_c_button_movement(struct Camera *c) {
    s16 cSideYaw;

    // Zoom in
    if (gPlayer1Controller->buttonPressed & U_CBUTTONS) {
        if (c->mode != CAMERA_MODE_FIXED && (gCameraMovementFlags & CAM_MOVE_ZOOMED_OUT)) {
            gCameraMovementFlags &= ~CAM_MOVE_ZOOMED_OUT;
            play_sound_cbutton_up();
        } else {
            set_mode_c_up(c);
            if (sZeroZoomDist > gCameraZoomDist) {
                sZoomAmount = -gCameraZoomDist;
            } else {
                sZoomAmount = gCameraZoomDist;
            }
        }
    }
    if (c->mode != CAMERA_MODE_FIXED) {
        // Zoom out
        if (gPlayer1Controller->buttonPressed & D_CBUTTONS) {
            if (gCameraMovementFlags & CAM_MOVE_ZOOMED_OUT) {
                gCameraMovementFlags |= CAM_MOVE_ALREADY_ZOOMED_OUT;
                sZoomAmount = gCameraZoomDist + 400.f;
                play_camera_buzz_if_cdown();
            } else {
                gCameraMovementFlags |= CAM_MOVE_ZOOMED_OUT;
                sZoomAmount = gCameraZoomDist + 400.f;
                play_sound_cbutton_down();
            }
        }

        // Rotate left or right
        cSideYaw = 0x1000;
        if (gPlayer1Controller->buttonPressed & R_CBUTTONS) {
            if (gCameraMovementFlags & CAM_MOVE_ROTATE_LEFT) {
                gCameraMovementFlags &= ~CAM_MOVE_ROTATE_LEFT;
            } else {
                gCameraMovementFlags |= CAM_MOVE_ROTATE_RIGHT;
                if (sCSideButtonYaw == 0) {
                    play_sound_cbutton_side();
                }
                sCSideButtonYaw = -cSideYaw;
            }
        }
        if (gPlayer1Controller->buttonPressed & L_CBUTTONS) {
            if (gCameraMovementFlags & CAM_MOVE_ROTATE_RIGHT) {
                gCameraMovementFlags &= ~CAM_MOVE_ROTATE_RIGHT;
            } else {
                gCameraMovementFlags |= CAM_MOVE_ROTATE_LEFT;
                if (sCSideButtonYaw == 0) {
                    play_sound_cbutton_side();
                }
                sCSideButtonYaw = cSideYaw;
            }
        }
    }
}

/**
 * Zero the 10 cvars.
 */
void clear_cutscene_vars(UNUSED struct Camera *c) {
    s32 i;

    for (i = 0; i < 10; i++) {
        sCutsceneVars[i].unused1 = 0;
        vec3_zero(sCutsceneVars[i].point);
        vec3_zero(sCutsceneVars[i].unusedPoint);
        vec3_zero(sCutsceneVars[i].angle);
        sCutsceneVars[i].unused2 = 0;
    }
}

/**
 * Start the cutscene, `cutscene`, if it is not already playing.
 */
void start_cutscene(struct Camera *c, u8 cutscene) {
    if (c->cutscene != cutscene) {
        c->cutscene = cutscene;
        clear_cutscene_vars(c);
    }
}

/**
 * Look up the victory dance cutscene in sDanceCutsceneTable
 *
 * First the index entry is determined based on the course and the star that was just picked up
 * Like the entries in sZoomOutAreaMasks, each entry represents two stars
 * The current courses's 4 bits of the index entry are used as the actual index into sDanceCutsceneTable
 *
 * @return the victory cutscene to use
 */
s32 determine_dance_cutscene(UNUSED struct Camera *c) {
#ifdef NON_STOP_STARS
    return CUTSCENE_DANCE_DEFAULT;
#else
    u8 cutscene = CUTSCENE_NONE;
    u8 cutsceneIndex = 0;
    u8 starIndex = (gLastCompletedStarNum - 1) / 2;
    u8 courseNum = gCurrCourseNum;

    if (starIndex > 3) {
        starIndex = 0;
    }
    if (courseNum > COURSE_MAX) {
        courseNum = COURSE_NONE;
    }
    cutsceneIndex = sDanceCutsceneIndexTable[courseNum][starIndex];

    if (gLastCompletedStarNum & 1) {
        // Odd stars take the lower four bytes
        cutsceneIndex &= 0xF;
    } else {
        // Even stars use the upper four bytes
        cutsceneIndex = cutsceneIndex >> 4;
    }
    cutscene = sDanceCutsceneTable[cutsceneIndex];
    return cutscene;
#endif
}

/**
 * @return `pullResult` or `pushResult` depending on Mario's door action
 */
u8 open_door_cutscene(u8 pullResult, u8 pushResult) {
    if (sMarioCamState->action == ACT_PULLING_DOOR) {
        return pullResult;
    }
    if (sMarioCamState->action == ACT_PUSHING_DOOR) {
        return pushResult;
    }
    return CUTSCENE_NONE;
}

/**
 * If no cutscenes are playing, determines if a cutscene should play based on Mario's action and
 * cameraEvent
 *
 * @return the cutscene that should start, 0 if none
 */
u8 get_cutscene_from_mario_status(struct Camera *c) {
    u8 cutscene = c->cutscene;

    if (cutscene == CUTSCENE_NONE) {
        // A cutscene started by an object, if any, will start if nothing else happened
        cutscene = sObjectCutscene;
        sObjectCutscene = CUTSCENE_NONE;
        if (sMarioCamState->cameraEvent == CAM_EVENT_DOOR) {
            switch (gCurrLevelArea) {
                case AREA_CASTLE_LOBBY:
                    //! doorStatus is never DOOR_ENTER_LOBBY when cameraEvent == 6, because
                    //! doorStatus is only used for the star door in the lobby, which uses
                    //! ACT_ENTERING_STAR_DOOR
                    if (c->mode == CAMERA_MODE_SPIRAL_STAIRS || c->mode == CAMERA_MODE_CLOSE || c->doorStatus == DOOR_ENTER_LOBBY) {
                        cutscene = open_door_cutscene(CUTSCENE_DOOR_PULL_MODE, CUTSCENE_DOOR_PUSH_MODE);
                    } else {
                        cutscene = open_door_cutscene(CUTSCENE_DOOR_PULL, CUTSCENE_DOOR_PUSH);
                    }
                    break;
                case AREA_BBH:
                    //! Castle Lobby uses 0 to mean 'no special modes', but BBH uses 1...
                    if (c->doorStatus == DOOR_LEAVING_SPECIAL) {
                        cutscene = open_door_cutscene(CUTSCENE_DOOR_PULL, CUTSCENE_DOOR_PUSH);
                    } else {
                        cutscene = open_door_cutscene(CUTSCENE_DOOR_PULL_MODE, CUTSCENE_DOOR_PUSH_MODE);
                    }
                    break;
                default:
                    cutscene = open_door_cutscene(CUTSCENE_DOOR_PULL, CUTSCENE_DOOR_PUSH);
                    break;
            }
        }
        if (sMarioCamState->cameraEvent == CAM_EVENT_DOOR_WARP) {
            cutscene = CUTSCENE_DOOR_WARP;
        }
        if (sMarioCamState->cameraEvent == CAM_EVENT_CANNON) {
            cutscene = CUTSCENE_ENTER_CANNON;
        }
        if (SURFACE_IS_PAINTING_WARP(sMarioGeometry.currFloorType)) {
            cutscene = CUTSCENE_ENTER_PAINTING;
        }
        switch (sMarioCamState->action) {
            case ACT_DEATH_EXIT:
                cutscene = CUTSCENE_DEATH_EXIT;
                break;
            case ACT_EXIT_AIRBORNE:
                cutscene = CUTSCENE_EXIT_PAINTING_SUCC;
                break;
            case ACT_SPECIAL_EXIT_AIRBORNE:
                if (gPrevLevel == LEVEL_BOWSER_1 || gPrevLevel == LEVEL_BOWSER_2
                    || gPrevLevel == LEVEL_BOWSER_3) {
                    cutscene = CUTSCENE_EXIT_BOWSER_SUCC;
                } else {
                    cutscene = CUTSCENE_EXIT_SPECIAL_SUCC;
                }
                break;
            case ACT_SPECIAL_DEATH_EXIT:
                if (gPrevLevel == LEVEL_BOWSER_1 || gPrevLevel == LEVEL_BOWSER_2
                    || gPrevLevel == LEVEL_BOWSER_3) {
                    cutscene = CUTSCENE_EXIT_BOWSER_DEATH;
                } else {
                    cutscene = CUTSCENE_NONPAINTING_DEATH;
                }
                break;
            case ACT_ENTERING_STAR_DOOR:
                if (c->doorStatus == DOOR_DEFAULT) {
                    cutscene = CUTSCENE_SLIDING_DOORS_OPEN;
                } else {
                    cutscene = CUTSCENE_DOOR_PULL_MODE;
                }
                break;
            case ACT_UNLOCKING_KEY_DOOR:
                cutscene = CUTSCENE_UNLOCK_KEY_DOOR;
                break;
            case ACT_WATER_DEATH:
                cutscene = CUTSCENE_WATER_DEATH;
                break;
            case ACT_DEATH_ON_BACK:
                cutscene = CUTSCENE_DEATH_ON_BACK;
                break;
            case ACT_DEATH_ON_STOMACH:
                cutscene = CUTSCENE_DEATH_ON_STOMACH;
                break;
            case ACT_STANDING_DEATH:
                cutscene = CUTSCENE_STANDING_DEATH;
                break;
            case ACT_SUFFOCATION:
                cutscene = CUTSCENE_SUFFOCATION_DEATH;
                break;
            case ACT_QUICKSAND_DEATH:
                cutscene = CUTSCENE_QUICKSAND_DEATH;
                break;
            case ACT_ELECTROCUTION:
                cutscene = CUTSCENE_STANDING_DEATH;
                break;
            case ACT_STAR_DANCE_EXIT:
                cutscene = determine_dance_cutscene(c);
                break;
            case ACT_STAR_DANCE_WATER:
                cutscene = determine_dance_cutscene(c);
                break;
            case ACT_STAR_DANCE_NO_EXIT:
                cutscene = CUTSCENE_DANCE_DEFAULT;
                break;
        }
        switch (sMarioCamState->cameraEvent) {
            case CAM_EVENT_START_INTRO:
                cutscene = CUTSCENE_INTRO_PEACH;
                break;
            case CAM_EVENT_START_GRAND_STAR:
                cutscene = CUTSCENE_GRAND_STAR;
                break;
            case CAM_EVENT_START_ENDING:
                cutscene = CUTSCENE_ENDING;
                break;
            case CAM_EVENT_START_END_WAVING:
                cutscene = CUTSCENE_END_WAVING;
                break;
            case CAM_EVENT_START_CREDITS:
                cutscene = CUTSCENE_CREDITS;
                break;
        }
    }
    //! doorStatus is reset every frame. CameraTriggers need to constantly set doorStatus
    c->doorStatus = DOOR_DEFAULT;

    return cutscene;
}

/**
 * Moves the camera when Mario has triggered a warp
 */
void warp_camera(f32 displacementX, f32 displacementY, f32 displacementZ) {
    Vec3f displacement;
    struct MarioState *marioStates = &gMarioStates[0];
    struct LinearTransitionPoint *start = &sModeInfo.transitionStart;
    struct LinearTransitionPoint *end = &sModeInfo.transitionEnd;

    gCurrLevelArea = gCurrLevelNum * 16 + gCurrentArea->index;
    displacement[0] = displacementX;
    displacement[1] = displacementY;
    displacement[2] = displacementZ;
    vec3f_add(gLakituState.curPos, displacement);
    vec3f_add(gLakituState.curFocus, displacement);
    vec3f_add(gLakituState.goalPos, displacement);
    vec3f_add(gLakituState.goalFocus, displacement);
    marioStates->waterLevel += displacementY;

    vec3f_add(start->focus, displacement);
    vec3f_add(start->pos, displacement);
    vec3f_add(end->focus, displacement);
    vec3f_add(end->pos, displacement);
}

/**
 * Make the camera's y coordinate approach `goal`,
 * unless smooth movement is off, in which case the y coordinate is simply set to `goal`
 */
void approach_camera_height(struct Camera *c, f32 goal, f32 inc) {
    if (sStatusFlags & CAM_FLAG_SMOOTH_MOVEMENT) {
        if (c->pos[1] < goal) {
            if ((c->pos[1] += inc) > goal) {
                c->pos[1] = goal;
            }
        } else {
            if ((c->pos[1] -= inc) < goal) {
                c->pos[1] = goal;
            }
        }
    } else {
        c->pos[1] = goal;
    }
}

/**
 * Set the camera's focus to Mario's position, and add several relative offsets.
 *
 * @param leftRight offset to Mario's left/right, relative to his faceAngle
 * @param yOff y offset
 * @param forwBack offset to Mario's front/back, relative to his faceAngle
 * @param yawOff offset to Mario's faceAngle, changes the direction of `leftRight` and `forwBack`
 */
void set_focus_rel_mario(struct Camera *c, f32 leftRight, f32 yOff, f32 forwBack, s16 yawOff) {
    s16 yaw;
    f32 focFloorYOff;

    calc_y_to_curr_floor(&focFloorYOff, 1.f, 200.f, &focFloorYOff, 0.9f, 200.f);
    yaw = sMarioCamState->faceAngle[1] + yawOff;
    c->focus[2] = sMarioCamState->pos[2] + forwBack * coss(yaw) - leftRight * sins(yaw);
    c->focus[0] = sMarioCamState->pos[0] + forwBack * sins(yaw) + leftRight * coss(yaw);
    c->focus[1] = sMarioCamState->pos[1] + yOff + focFloorYOff;
}

/**
 * Set the camera's position to Mario's position, and add several relative offsets. Unused.
 *
 * @param leftRight offset to Mario's left/right, relative to his faceAngle
 * @param yOff y offset
 * @param forwBack offset to Mario's front/back, relative to his faceAngle
 * @param yawOff offset to Mario's faceAngle, changes the direction of `leftRight` and `forwBack`
 */
UNUSED static void unused_set_pos_rel_mario(struct Camera *c, f32 leftRight, f32 yOff, f32 forwBack, s16 yawOff) {
    u16 yaw = sMarioCamState->faceAngle[1] + yawOff;

    c->pos[0] = sMarioCamState->pos[0] + forwBack * sins(yaw) + leftRight * coss(yaw);
    c->pos[1] = sMarioCamState->pos[1] + yOff;
    c->pos[2] = sMarioCamState->pos[2] + forwBack * coss(yaw) - leftRight * sins(yaw);
}

/**
 * Rotates the offset `to` according to the pitch and yaw values in `rotation`.
 * Adds `from` to the rotated offset, and stores the result in `dst`.
 *
 * @warning Flips the Z axis, so that relative to `rotation`, -Z moves forwards and +Z moves backwards.
 */
void offset_rotated(Vec3f dst, Vec3f from, Vec3f to, Vec3s rotation) {
    Vec3f pitchRotated;

    // First rotate the direction by rotation's pitch
    //! The Z axis is flipped here.
    pitchRotated[2] = -(to[2] * coss(rotation[0]) - to[1] * sins(rotation[0]));
    pitchRotated[1] =   to[2] * sins(rotation[0]) + to[1] * coss(rotation[0]);
    pitchRotated[0] =   to[0];

    // Rotate again by rotation's yaw
    dst[0] = from[0] + pitchRotated[2] * sins(rotation[1]) + pitchRotated[0] * coss(rotation[1]);
    dst[1] = from[1] + pitchRotated[1];
    dst[2] = from[2] + pitchRotated[2] * coss(rotation[1]) - pitchRotated[0] * sins(rotation[1]);
}

/**
 * Rotates the offset defined by (`xTo`, `yTo`, `zTo`) according to the pitch and yaw values in `rotation`.
 * Adds `from` to the rotated offset, and stores the result in `dst`.
 *
 * @warning Flips the Z axis, so that relative to `rotation`, -Z moves forwards and +Z moves backwards.
 */
void offset_rotated_coords(Vec3f dst, Vec3f from, Vec3s rotation, f32 xTo, f32 yTo, f32 zTo) {
    Vec3f to;

    vec3f_set(to, xTo, yTo, zTo);
    offset_rotated(dst, from, to, rotation);
}

void determine_pushing_or_pulling_door(s16 *rotation) {
    if (sMarioCamState->action == ACT_PULLING_DOOR) {
        *rotation = 0;
    } else {
        *rotation = DEGREES(-180);
    }
}

/**
 * Calculate Lakitu's next position and focus, according to gCamera's state,
 * and store them in `newPos` and `newFoc`.
 *
 * @param newPos where Lakitu should fly towards this frame
 * @param newFoc where Lakitu should look towards this frame
 *
 * @param curPos gCamera's pos this frame
 * @param curFoc gCamera's foc this frame
 *
 * @param oldPos gCamera's pos last frame
 * @param oldFoc gCamera's foc last frame
 *
 * @return Lakitu's next yaw, which is the same as the yaw passed in if no transition happened
 */
s16 next_lakitu_state(Vec3f newPos, Vec3f newFoc, Vec3f curPos, Vec3f curFoc,
                      Vec3f oldPos, Vec3f oldFoc, s16 yaw) {
    s16 yawVelocity;
    s16 pitchVelocity;
    f32 distVelocity;
    f32 goalDist;
    s16 goalPitch;
    s16 goalYaw;
    f32 distTimer = sModeTransition.framesLeft;
    s16 angleTimer = sModeTransition.framesLeft;
    Vec3f nextPos;
    Vec3f nextFoc;
    Vec3f startPos;
    Vec3f startFoc;
    s32 i;
    f32 floorHeight;
    struct Surface *floor;

    // If not transitioning, just use gCamera's current pos and foc
    vec3f_copy(newPos, curPos);
    vec3f_copy(newFoc, curFoc);

    if (sStatusFlags & CAM_FLAG_START_TRANSITION) {
        for (i = 0; i < 3; i++) {
            // Add Mario's displacement from this frame to the last frame's pos and focus
            // Makes the transition start from where the camera would have moved
            startPos[i] = oldPos[i] + sMarioCamState->pos[i] - sModeTransition.marioPos[i];
            startFoc[i] = oldFoc[i] + sMarioCamState->pos[i] - sModeTransition.marioPos[i];
        }


        vec3f_get_dist_and_angle(curFoc, startFoc, &sModeTransition.focDist, &sModeTransition.focPitch,
                                 &sModeTransition.focYaw);
        vec3f_get_dist_and_angle(curFoc, startPos, &sModeTransition.posDist, &sModeTransition.posPitch,
                                 &sModeTransition.posYaw);
        sStatusFlags &= ~CAM_FLAG_START_TRANSITION;
    }

    // Transition from the last mode to the current one
    if (sModeTransition.framesLeft > 0) {
        vec3f_get_dist_and_angle(curFoc, curPos, &goalDist, &goalPitch, &goalYaw);
        distVelocity = abss(goalDist - sModeTransition.posDist) / distTimer;
        pitchVelocity = abss(goalPitch - sModeTransition.posPitch) / angleTimer;
        yawVelocity = abss(goalYaw - sModeTransition.posYaw) / angleTimer;

        camera_approach_f32_symmetric_bool(&sModeTransition.posDist, goalDist, distVelocity);
        camera_approach_s16_symmetric_bool(&sModeTransition.posYaw, goalYaw, yawVelocity);
        camera_approach_s16_symmetric_bool(&sModeTransition.posPitch, goalPitch, pitchVelocity);
        vec3f_set_dist_and_angle(curFoc, nextPos, sModeTransition.posDist, sModeTransition.posPitch,
                                 sModeTransition.posYaw);

        vec3f_get_dist_and_angle(curPos, curFoc, &goalDist, &goalPitch, &goalYaw);
        pitchVelocity = sModeTransition.focPitch / (s16) sModeTransition.framesLeft;
        yawVelocity = sModeTransition.focYaw / (s16) sModeTransition.framesLeft;
        distVelocity = sModeTransition.focDist / sModeTransition.framesLeft;

        camera_approach_s16_symmetric_bool(&sModeTransition.focPitch, goalPitch, pitchVelocity);
        camera_approach_s16_symmetric_bool(&sModeTransition.focYaw, goalYaw, yawVelocity);
        camera_approach_f32_symmetric_bool(&sModeTransition.focDist, 0, distVelocity);
        vec3f_set_dist_and_angle(curFoc, nextFoc, sModeTransition.focDist, sModeTransition.focPitch,
                                 sModeTransition.focYaw);

        vec3f_copy(newFoc, nextFoc);
        vec3f_copy(newPos, nextPos);

        if (gCamera->cutscene != 0 || !(gCameraMovementFlags & CAM_MOVE_C_UP_MODE)) {
            floorHeight = find_floor(newPos[0], newPos[1], newPos[2], &floor);
            if (floorHeight != FLOOR_LOWER_LIMIT) {
                if ((floorHeight += 125.f) > newPos[1]) {
                    newPos[1] = floorHeight;
                }
            }
            f32_find_wall_collision(&newPos[0], &newPos[1], &newPos[2], 0.f, 100.f);
        }
        sModeTransition.framesLeft--;
        yaw = calculate_yaw(newFoc, newPos);
    } else {
        sModeTransition.posDist = 0.f;
        sModeTransition.posPitch = 0;
        sModeTransition.posYaw = 0;
        sStatusFlags &= ~CAM_FLAG_TRANSITION_OUT_OF_C_UP;
    }
    vec3f_copy(sModeTransition.marioPos, sMarioCamState->pos);
    return yaw;
}

static UNUSED void stop_transitional_movement(void) {
    sStatusFlags &= ~(CAM_FLAG_START_TRANSITION | CAM_FLAG_TRANSITION_OUT_OF_C_UP);
    sModeTransition.framesLeft = 0;
}

/**
 * Start fixed camera mode, setting the base position to (`x`, `y`, `z`)
 *
 * @return TRUE if the base pos was updated
 */
s32 set_camera_mode_fixed(struct Camera *c, s16 x, s16 y, s16 z) {
    s32 basePosSet = FALSE;
    f32 posX = x;
    f32 posY = y;
    f32 posZ = z;

    if (sFixedModeBasePosition[0] != posX || sFixedModeBasePosition[1] != posY
        || sFixedModeBasePosition[2] != posZ) {
        basePosSet = TRUE;
        sStatusFlags &= ~CAM_FLAG_SMOOTH_MOVEMENT;
    }
    vec3f_set(sFixedModeBasePosition, posX, posY, posZ);
    if (c->mode != CAMERA_MODE_FIXED) {
        sStatusFlags &= ~CAM_FLAG_SMOOTH_MOVEMENT;
        c->mode = CAMERA_MODE_FIXED;
        vec3f_set(c->pos, sFixedModeBasePosition[0], sMarioCamState->pos[1],
                  sFixedModeBasePosition[2]);
    }
    return basePosSet;
}

void set_camera_mode_8_directions(struct Camera *c) {
    if (c->mode != CAMERA_MODE_8_DIRECTIONS) {
        c->mode = CAMERA_MODE_8_DIRECTIONS;
        sStatusFlags &= ~CAM_FLAG_SMOOTH_MOVEMENT;
        s8DirModeBaseYaw = 0;
        s8DirModeYawOffset = 0;
    }
}

/**
 * If the camera mode is not already the boss fight camera (camera with two foci)
 * set it to be so.
 */
void set_camera_mode_boss_fight(struct Camera *c) {
    if (c->mode != CAMERA_MODE_BOSS_FIGHT) {
        transition_to_camera_mode(c, CAMERA_MODE_BOSS_FIGHT, 15);
        sModeOffsetYaw = c->nextYaw - DEGREES(45);
    }
}

void set_camera_mode_close_cam(u8 *mode) {
    if (*mode != CAMERA_MODE_CLOSE) {
        sStatusFlags &= ~CAM_FLAG_SMOOTH_MOVEMENT;
        *mode = CAMERA_MODE_CLOSE;
    }
}

/**
 * Change to radial mode.
 * If the difference in yaw between pos -> Mario and pos > focus is < 90 degrees, transition.
 * Otherwise jump to radial mode.
 */
void set_camera_mode_radial(struct Camera *c, s16 transitionTime) {
    Vec3f focus;
    s16 yaw;

    focus[0] = c->areaCenX;
    focus[1] = sMarioCamState->pos[1];
    focus[2] = c->areaCenZ;
    if (c->mode != CAMERA_MODE_RADIAL) {
        yaw = calculate_yaw(focus, sMarioCamState->pos) - calculate_yaw(c->focus, c->pos) + DEGREES(90);
        if (yaw > 0) {
            transition_to_camera_mode(c, CAMERA_MODE_RADIAL, transitionTime);
        } else {
            c->mode = CAMERA_MODE_RADIAL;
            sStatusFlags &= ~CAM_FLAG_SMOOTH_MOVEMENT;
        }
        sModeOffsetYaw = 0;
    }
}

/**
 * Start parallel tracking mode using the path `path`
 */
void parallel_tracking_init(struct Camera *c, struct ParallelTrackingPoint *path) {
    if (c->mode != CAMERA_MODE_PARALLEL_TRACKING) {
        sParTrackPath = path;
        sParTrackIndex = 0;
        sParTrackTransOff.pos[0] = 0.f;
        sParTrackTransOff.pos[1] = 0.f;
        sParTrackTransOff.pos[2] = 0.f;
        // Place the camera in the middle of the path
        c->pos[0] = (sParTrackPath[0].pos[0] + sParTrackPath[1].pos[0]) / 2;
        c->pos[1] = (sParTrackPath[0].pos[1] + sParTrackPath[1].pos[1]) / 2;
        c->pos[2] = (sParTrackPath[0].pos[2] + sParTrackPath[1].pos[2]) / 2;
        sStatusFlags &= ~CAM_FLAG_SMOOTH_MOVEMENT;
        c->mode = CAMERA_MODE_PARALLEL_TRACKING;
    }
}

/**
 * Set the fixed camera base pos depending on the current level area
 */
void set_fixed_cam_axis_sa_lobby(UNUSED s16 preset) {
    switch (gCurrLevelArea) {
        case AREA_SA:
            vec3f_set(sFixedModeBasePosition, 646.f, 143.f, -1513.f);
            break;

        case AREA_CASTLE_LOBBY:
            vec3f_set(sFixedModeBasePosition, -577.f, 143.f, 1443.f);
            break;
    }
}

/**
 * Block area-specific CameraTrigger and special surface modes.
 * Generally, block area mode changes if:
 *      Mario is wearing the metal cap, or at the water's surface, or the camera is in Mario mode
 *
 * However, if the level is WDW, DDD, or COTMC (levels that have metal cap and water):
 *      Only block area mode changes if Mario is in a cannon,
 *      or if the camera is in Mario mode and Mario is not swimming or in water with the metal cap
 */
#ifdef ENABLE_VANILLA_CAM_PROCESSING
void check_blocking_area_processing(const u8 *mode) {
    if (sMarioCamState->action & ACT_FLAG_METAL_WATER ||
                        *mode == DEEP_WATER_CAMERA_MODE || *mode == WATER_SURFACE_CAMERA_MODE) {
        sStatusFlags |= CAM_FLAG_BLOCK_AREA_PROCESSING;
    }

    if (gCurrLevelNum == LEVEL_DDD || gCurrLevelNum == LEVEL_WDW || gCurrLevelNum == LEVEL_COTMC) {
        sStatusFlags &= ~CAM_FLAG_BLOCK_AREA_PROCESSING;
    }

    if ((*mode == DEEP_WATER_CAMERA_MODE &&
            !(sMarioCamState->action & (ACT_FLAG_SWIMMING | ACT_FLAG_METAL_WATER))) ||
         *mode == CAMERA_MODE_INSIDE_CANNON) {
        sStatusFlags |= CAM_FLAG_BLOCK_AREA_PROCESSING;
    }
#else
void check_blocking_area_processing(UNUSED const u8 *mode) {
    sStatusFlags |= CAM_FLAG_BLOCK_AREA_PROCESSING;
#endif
}

void cam_rr_exit_building_side(struct Camera *c) {
    set_camera_mode_8_directions(c);
    s8DirModeBaseYaw = DEGREES(90);
}

void cam_rr_exit_building_top(struct Camera *c) {
    set_camera_mode_8_directions(c);
    if (c->pos[1] < 6343.f) {
        c->pos[1] = 7543.f;
        gLakituState.goalPos[1] = c->pos[1];
        gLakituState.curPos[1] = c->pos[1];
        sStatusFlags &= ~CAM_FLAG_SMOOTH_MOVEMENT;
    }
}

void cam_rr_enter_building_window(struct Camera *c) {
    if (c->mode != CAMERA_MODE_FIXED) {
        set_camera_mode_fixed(c, -2974, 478, -3975);
    }
}

void cam_rr_enter_building(struct Camera *c) {
    if (c->mode != CAMERA_MODE_FIXED) {
        set_camera_mode_fixed(c, -2953, 798, -3943);
    }
    // Prevent the camera from being above the roof
    if (c->pos[1] > 6043.f) {
        c->pos[1] = 6043.f;
    }
}

void cam_rr_enter_building_side(struct Camera *c) {
    if (c->mode != CAMERA_MODE_FIXED) {
        sStatusFlags &= ~CAM_FLAG_SMOOTH_MOVEMENT;
        c->mode = CAMERA_MODE_FIXED;
    }
}

/**
 * Fix the camera in place as Mario gets exits out the MC cave into the waterfall.
 */
void cam_cotmc_exit_waterfall(UNUSED struct Camera *c) {
    gCameraMovementFlags |= CAM_MOVE_FIX_IN_PLACE;
}

/**
 * Sets 8 directional mode and blocks the next trigger from processing.
 * Activated when Mario is walking in front of the snowman's head.
 */
void cam_sl_snowman_head_8dir(struct Camera *c) {
    sStatusFlags |= CAM_FLAG_BLOCK_AREA_PROCESSING;
    transition_to_camera_mode(c, CAMERA_MODE_8_DIRECTIONS, 60);
    s8DirModeBaseYaw = 0x1D27;
}

/**
 * Sets free roam mode in SL, called by a trigger that covers a large area and surrounds the 8 direction
 * trigger.
 */
void cam_sl_free_roam(struct Camera *c) {
    transition_to_camera_mode(c, CAMERA_MODE_FREE_ROAM, 60);
}

/**
 * Warps the camera underneath the floor, used in HMC to move under the elevator platforms
 */
void move_camera_through_floor_while_descending(struct Camera *c, f32 height) {
    if ((sMarioGeometry.currFloorHeight < height - 100.f)
        && (sMarioGeometry.prevFloorHeight > sMarioGeometry.currFloorHeight)) {
        c->pos[1] = height - 400.f;
        gLakituState.curPos[1] = height - 400.f;
        gLakituState.goalPos[1] = height - 400.f;
    }
}

void cam_hmc_enter_maze(struct Camera *c) {
    s16 pitch, yaw;
    f32 dist;

    if (c->pos[1] > -102.f) {
        vec3f_get_dist_and_angle(c->focus, gLakituState.goalPos, &dist, &pitch, &yaw);
        vec3f_set_dist_and_angle(c->focus, gLakituState.goalPos, 300.f, pitch, yaw);
        gLakituState.goalPos[1] = -800.f;
        c->pos[1] = gLakituState.goalPos[1];
        gLakituState.curPos[1] = gLakituState.goalPos[1];
        sStatusFlags &= ~CAM_FLAG_SMOOTH_MOVEMENT;
    }
}

void cam_hmc_elevator_black_hole(struct Camera *c) {
    move_camera_through_floor_while_descending(c, 1536.f);
}

void cam_hmc_elevator_maze_emergency_exit(struct Camera *c) {
    move_camera_through_floor_while_descending(c, 2355.f);
}

void cam_hmc_elevator_lake(struct Camera *c) {
    move_camera_through_floor_while_descending(c, 1843.f);
}

void cam_hmc_elevator_maze(struct Camera *c) {
    move_camera_through_floor_while_descending(c, 1843.f);
}

/**
 * Starts the "Enter Pyramid Top" cutscene.
 */
void cam_ssl_enter_pyramid_top(UNUSED struct Camera *c) {
    start_object_cutscene_without_focus(CUTSCENE_ENTER_PYRAMID_TOP);
}

/**
 * Change to close mode in the center of the pyramid. Outside this trigger, the default mode is outwards
 * radial.
 */
void cam_ssl_pyramid_center(struct Camera *c) {
    sStatusFlags |= CAM_FLAG_BLOCK_AREA_PROCESSING;
    transition_to_camera_mode(c, CAMERA_MODE_CLOSE, 90);
}

/**
 * Changes the mode back to outward radial in the boss room inside the pyramid.
 */
void cam_ssl_boss_room(struct Camera *c) {
    sStatusFlags |= CAM_FLAG_BLOCK_AREA_PROCESSING;
    transition_to_camera_mode(c, CAMERA_MODE_OUTWARD_RADIAL, 90);
}

/**
 * Moves the camera to through the tunnel by forcing sModeOffsetYaw
 */
void cam_thi_move_cam_through_tunnel(UNUSED struct Camera *c) {
    if (sModeOffsetYaw < DEGREES(60)) {
        sModeOffsetYaw = DEGREES(60);
    }
}

/**
 * Aligns the camera to look through the tunnel
 */
void cam_thi_look_through_tunnel(UNUSED struct Camera *c) {
    // ~82.5 degrees
    if (sModeOffsetYaw > 0x3AAA) {
        sModeOffsetYaw = 0x3AAA;
    }
}

/**
 * Starts the pool entrance cutscene if Mario is not exiting the pool.
 * Used in both the castle and HMC.
 */
void cam_castle_hmc_start_pool_cutscene(struct Camera *c) {
    if ((sMarioCamState->action != ACT_SPECIAL_DEATH_EXIT)
        && (sMarioCamState->action != ACT_SPECIAL_EXIT_AIRBORNE)) {
        start_cutscene(c, CUTSCENE_ENTER_POOL);
    }
}

/**
 * Sets the fixed mode pos offset so that the camera faces the doorway when Mario is near the entrance
 * to the castle lobby
 */
void cam_castle_lobby_entrance(UNUSED struct Camera *c) {
    vec3f_set(sCastleEntranceOffset, -813.f - sFixedModeBasePosition[0],
              378.f - sFixedModeBasePosition[1], 1103.f - sFixedModeBasePosition[2]);
}

/**
 * Make the camera look up the stairs from the 2nd to 3rd floor of the castle
 */
void cam_castle_look_upstairs(struct Camera *c) {
    struct Surface *floor;
    f32 floorHeight = find_floor(c->pos[0], c->pos[1], c->pos[2], &floor);

    // If Mario is on the first few steps, fix the camera pos, making it look up
    if ((sMarioGeometry.currFloorHeight > 1229.f) && (floorHeight < 1229.f)
        && (sCSideButtonYaw == 0)) {
        vec3f_set(c->pos, -227.f, 1425.f, 1533.f);
    }
}

/**
 * Make the camera look down the stairs towards the basement star door
 */
void cam_castle_basement_look_downstairs(struct Camera *c) {
    struct Surface *floor;
    f32 floorHeight = find_floor(c->pos[0], c->pos[1], c->pos[2], &floor);

    // Fix the camera pos, making it look downwards. Only active on the top few steps
    if ((floorHeight > -110.f) && (sCSideButtonYaw == 0)) {
        vec3f_set(c->pos, -980.f, 249.f, -1398.f);
    }
}

/**
 * Enter the fixed-mode castle lobby. A trigger for this is placed in every entrance so that the camera
 * changes to fixed mode.
 */
void cam_castle_enter_lobby(struct Camera *c) {
    if (c->mode != CAMERA_MODE_FIXED) {
        sStatusFlags &= ~CAM_FLAG_SMOOTH_MOVEMENT;
        set_fixed_cam_axis_sa_lobby(c->mode);
        c->mode = CAMERA_MODE_FIXED;
    }
}

/**
 * Starts spiral stairs mode.
 */
void cam_castle_enter_spiral_stairs(struct Camera *c) {
    transition_to_camera_mode(c, CAMERA_MODE_SPIRAL_STAIRS, 20);
}

/**
 * unused, starts close mode if the camera is in spiral stairs mode.
 * This was replaced with cam_castle_close_mode
 */
static UNUSED void cam_castle_leave_spiral_stairs(struct Camera *c) {
    if (c->mode == CAMERA_MODE_SPIRAL_STAIRS) {
        transition_to_camera_mode(c, CAMERA_MODE_CLOSE, 30);
    } else {
        set_camera_mode_close_cam(&c->mode);
    }
}

/**
 * The default mode when outside of the lobby and spiral staircase. A trigger for this is placed at
 * every door leaving the lobby and spiral staircase.
 */
void cam_castle_close_mode(struct Camera *c) {
    set_camera_mode_close_cam(&c->mode);
}

/**
 * Functions the same as cam_castle_close_mode, but sets doorStatus so that the camera will enter
 * fixed-mode when Mario leaves the room.
 */
void cam_castle_leave_lobby_sliding_door(struct Camera *c) {
    cam_castle_close_mode(c);
    c->doorStatus = DOOR_ENTER_LOBBY;
}

/**
 * Just calls cam_castle_enter_lobby
 */
void cam_castle_enter_lobby_sliding_door(struct Camera *c) {
    cam_castle_enter_lobby(c);
}

void cam_bbh_room_6(struct Camera *c) {
    parallel_tracking_init(c, sBBHLibraryParTrackPath);
}

void cam_bbh_fall_off_roof(struct Camera *c) {
    set_camera_mode_close_cam(&c->mode);
}

void cam_bbh_fall_into_pool(struct Camera *c) {
    Vec3f dir;
    set_camera_mode_close_cam(&c->mode);
    vec3f_set(dir, 0.f, 0.f, 300.f);
    offset_rotated(gLakituState.goalPos, sMarioCamState->pos, dir, sMarioCamState->faceAngle);
    gLakituState.goalPos[1] = -2300.f;
    vec3f_copy(c->pos, gLakituState.goalPos);
    sStatusFlags &= ~CAM_FLAG_SMOOTH_MOVEMENT;
}

void cam_bbh_room_1(struct Camera *c) {
    set_camera_mode_fixed(c, 956, 440, 1994);
}

void cam_bbh_leave_front_door(struct Camera *c) {
    c->doorStatus = DOOR_LEAVING_SPECIAL;
    cam_bbh_room_1(c);
}

void cam_bbh_room_2_lower(struct Camera *c) {
    set_camera_mode_fixed(c, 2591, 400, 1284);
}

void cam_bbh_room_4(struct Camera *c) {
    set_camera_mode_fixed(c, 3529, 340, -1384);
}

void cam_bbh_room_8(struct Camera *c) {
    set_camera_mode_fixed(c, -500, 740, -1306);
}

/**
 * In BBH's room 5's library (the first floor room with the vanish cap/boo painting)
 * set the camera mode to fixed and position to (-2172, 200, 675)
 */
void cam_bbh_room_5_library(struct Camera *c) {
    set_camera_mode_fixed(c, -2172, 200, 675);
}

/**
 * In BBH's room 5 (the first floor room with the vanish cap/boo painting)
 * set the camera mode to to the hidden room's position
 * if coming from the library.
 */
void cam_bbh_room_5_library_to_hidden_transition(struct Camera *c) {
    if (set_camera_mode_fixed(c, -2172, 200, 675) == 1) {
        transition_next_state(c, 20);
    }
}

void cam_bbh_room_5_hidden_to_library_transition(struct Camera *c) {
    if (set_camera_mode_fixed(c, -1542, 320, -307) == 1) {
        transition_next_state(c, 20);
    }
}

void cam_bbh_room_5_hidden(struct Camera *c) {
    c->doorStatus = DOOR_LEAVING_SPECIAL;
    set_camera_mode_fixed(c, -1542, 320, -307);
}

void cam_bbh_room_3(struct Camera *c) {
    set_camera_mode_fixed(c, -1893, 320, 2327);
}

void cam_bbh_room_7_mr_i(struct Camera *c) {
    set_camera_mode_fixed(c, 1371, 360, -1302);
}

void cam_bbh_room_7_mr_i_to_coffins_transition(struct Camera *c) {
    if (set_camera_mode_fixed(c, 1371, 360, -1302) == 1) {
        transition_next_state(c, 20);
    }
}

void cam_bbh_room_7_coffins_to_mr_i_transition(struct Camera *c) {
    if (set_camera_mode_fixed(c, 2115, 260, -772) == 1) {
        transition_next_state(c, 20);
    }
}

void cam_bbh_elevator_room_lower(struct Camera *c) {
    c->doorStatus = DOOR_LEAVING_SPECIAL;
    set_camera_mode_close_cam(&c->mode);
}

void cam_bbh_room_0_back_entrance(struct Camera *c) {
    set_camera_mode_close_cam(&c->mode);
}

void cam_bbh_elevator(struct Camera *c) {
    if (c->mode == CAMERA_MODE_FIXED) {
        set_camera_mode_close_cam(&c->mode);
        c->pos[1] = -405.f;
        gLakituState.goalPos[1] = -405.f;
    }
}

void cam_bbh_room_12_upper(struct Camera *c) {
    c->doorStatus = DOOR_LEAVING_SPECIAL;
    set_camera_mode_fixed(c, -2932, 296, 4429);
}

void cam_bbh_enter_front_door(struct Camera *c) {
    set_camera_mode_close_cam(&c->mode);
}

void cam_bbh_room_2_library(struct Camera *c) {
    set_camera_mode_fixed(c, 3493, 440, 617);
}

void cam_bbh_room_2_library_to_trapdoor_transition(struct Camera *c) {
    if (set_camera_mode_fixed(c, 3493, 440, 617) == 1) {
        transition_next_state(c, 20);
    }
}

void cam_bbh_room_2_trapdoor(struct Camera *c) {
    set_camera_mode_fixed(c, 3502, 440, 1217);
}

void cam_bbh_room_2_trapdoor_transition(struct Camera *c) {
    if (set_camera_mode_fixed(c, 3502, 440, 1217) == 1) {
        transition_next_state(c, 20);
    }
}

void cam_bbh_room_9_attic(struct Camera *c) {
    set_camera_mode_fixed(c, -670, 460, 372);
}

void cam_bbh_room_9_attic_transition(struct Camera *c) {
    if (set_camera_mode_fixed(c, -670, 460, 372) == 1) {
        transition_next_state(c, 20);
    }
}

void cam_bbh_room_9_mr_i_transition(struct Camera *c) {
    if (set_camera_mode_fixed(c, 131, 380, -263) == 1) {
        transition_next_state(c, 20);
    }
}

void cam_bbh_room_13_balcony(struct Camera *c) {
    set_camera_mode_fixed(c, 210, 420, 3109);
}

void cam_bbh_room_0(struct Camera *c) {
    c->doorStatus = DOOR_LEAVING_SPECIAL;
    set_camera_mode_fixed(c, -204, 807, 204);
}

void cam_ccm_enter_slide_shortcut(UNUSED struct Camera *c) {
    sStatusFlags |= CAM_FLAG_CCM_SLIDE_SHORTCUT;
}

void cam_ccm_leave_slide_shortcut(UNUSED struct Camera *c) {
    sStatusFlags &= ~CAM_FLAG_CCM_SLIDE_SHORTCUT;
}

/**
 * Apply any modes that are triggered by special floor surface types
 */
u32 surface_type_modes(struct Camera *c) {
    u32 modeChanged = 0;

    switch (sMarioGeometry.currFloorType) {
        case SURFACE_CLOSE_CAMERA:
            transition_to_camera_mode(c, CAMERA_MODE_CLOSE, 90);
            modeChanged++;
            break;

        case SURFACE_CAMERA_FREE_ROAM:
            transition_to_camera_mode(c, CAMERA_MODE_FREE_ROAM, 90);
            modeChanged++;
            break;

        case SURFACE_NO_CAM_COL_SLIPPERY:
            transition_to_camera_mode(c, CAMERA_MODE_CLOSE, 90);
            modeChanged++;
            break;
    }
    return modeChanged;
}

/**
 * Set the camera mode to `mode` if Mario is not standing on a special surface
 */
u32 set_mode_if_not_set_by_surface(struct Camera *c, u8 mode) {
    u32 modeChanged = surface_type_modes(c);

    if ((modeChanged == 0) && (mode != 0)) {
        transition_to_camera_mode(c, mode, 90);
    }

    return modeChanged;
}

/**
 * Used in THI, check if Mario is standing on any of the special surfaces in that area
 */
void surface_type_modes_thi(struct Camera *c) {
    switch (sMarioGeometry.currFloorType) {
        case SURFACE_CLOSE_CAMERA:
            if (c->mode != CAMERA_MODE_CLOSE) {
                transition_to_camera_mode(c, CAMERA_MODE_FREE_ROAM, 90);
            }
            break;

        case SURFACE_CAMERA_FREE_ROAM:
            if (c->mode != CAMERA_MODE_CLOSE) {
                transition_to_camera_mode(c, CAMERA_MODE_FREE_ROAM, 90);
            }
            break;

        case SURFACE_NO_CAM_COL_SLIPPERY:
            if (c->mode != CAMERA_MODE_CLOSE) {
                transition_to_camera_mode(c, CAMERA_MODE_FREE_ROAM, 90);
            }
            break;

        case SURFACE_CAMERA_8_DIR:
            transition_to_camera_mode(c, CAMERA_MODE_8_DIRECTIONS, 90);
            break;

        default:
            transition_to_camera_mode(c, CAMERA_MODE_RADIAL, 90);
    }
}

/**
 * Terminates a list of CameraTriggers.
 */
#define NULL_TRIGGER                                                                                    \
    { 0, NULL, 0, 0, 0, 0, 0, 0, 0 }

/**
 * The SL triggers operate camera behavior in front of the snowman who blows air.
 * The first sets a 8 direction mode, while the latter (which encompasses the former)
 * sets free roam mode.
 *
 * This behavior is exploitable, since the ranges assume that Mario must pass through the latter on
 * exit. Using hyperspeed, the earlier area can be directly exited from, keeping the changes it applies.
 */
struct CameraTrigger sCamSL[] = {
    { 1, cam_sl_snowman_head_8dir, 1119, 3584, 1125, 1177, 358, 358, -0x1D27 },
    // This trigger surrounds the previous one
    { 1, cam_sl_free_roam, 1119, 3584, 1125, 4096, 4096, 4096, -0x1D27 },
    NULL_TRIGGER
};

/**
 * The THI triggers are specifically for the tunnel near the start of the Huge Island.
 * The first helps the camera from getting stuck on the starting side, the latter aligns with the
 * tunnel. Both sides achieve their effect by editing the camera yaw.
 */
struct CameraTrigger sCamTHI[] = {
    { 1, cam_thi_move_cam_through_tunnel, -4609, -2969, 6448, 100, 300, 300, 0 },
    { 1, cam_thi_look_through_tunnel,     -4809, -2969, 6448, 100, 300, 300, 0 },
    NULL_TRIGGER
};

/**
 * The HMC triggers are mostly for warping the camera below platforms, but the second trigger is used to
 * start the cutscene for entering the CotMC pool.
 */
struct CameraTrigger sCamHMC[] = {
    { 1, cam_hmc_enter_maze, 1996, 102, 0, 205, 100, 205, 0 },
    { 1, cam_castle_hmc_start_pool_cutscene, 3350, -4689, 4800, 600, 50, 600, 0 },
    { 1, cam_hmc_elevator_black_hole, -3278, 1236, 1379, 358, 200, 358, 0 },
    { 1, cam_hmc_elevator_maze_emergency_exit, -2816, 2055, -2560, 358, 200, 358, 0 },
    { 1, cam_hmc_elevator_lake, -3532, 1543, -7040, 358, 200, 358, 0 },
    { 1, cam_hmc_elevator_maze, -972, 1543, -7347, 358, 200, 358, 0 },
    NULL_TRIGGER
};

/**
 * The SSL triggers are for starting the enter pyramid top cutscene,
 * setting close mode in the middle of the pyramid, and setting the boss fight camera mode to outward
 * radial.
 */
struct CameraTrigger sCamSSL[] = {
    { 1, cam_ssl_enter_pyramid_top, -2048, 1080, -1024, 150, 150, 150, 0 },
    { 2, cam_ssl_pyramid_center, 0, -104, -104, 1248, 1536, 2950, 0 },
    { 2, cam_ssl_pyramid_center, 0, 2500, 256, 515, 5000, 515, 0 },
    { 3, cam_ssl_boss_room, 0, -1534, -2040, 1000, 800, 1000, 0 },
    NULL_TRIGGER
};

/**
 * The RR triggers are for changing between fixed and 8 direction mode when entering / leaving the building at
 * the end of the ride.
 */
struct CameraTrigger sCamRR[] = {
    { 1, cam_rr_exit_building_side, -4197, 3819, -3087, 1769, 1490, 342, 0 },
    { 1, cam_rr_enter_building_side, -4197, 3819, -3771, 769, 490, 342, 0 },
    { 1, cam_rr_enter_building_window, -5603, 4834, -5209, 300, 600, 591, 0 },
    { 1, cam_rr_enter_building, -2609, 3730, -5463, 300, 650, 577, 0 },
    { 1, cam_rr_exit_building_top, -4196, 7343, -5155, 4500, 1000, 4500, 0 },
    { 1, cam_rr_enter_building, -4196, 6043, -5155, 500, 300, 500, 0 },
    NULL_TRIGGER,
};

/**
 * These triggers are unused, but because the first trigger surrounds the BoB tower and activates radial
 * mode (which is called "tower mode" in the patent), it's speculated they belonged to BoB.
 *
 * This table contains the only instance of a CameraTrigger with an area set to -1, and it sets the mode
 * to free_roam when Mario is not walking up the tower.
 */
struct CameraTrigger sCamBOB[] = {
    {  1, cam_bob_tower, 2468, 2720, -4608, 3263, 1696, 3072, 0 },
    { -1, cam_bob_default_free_roam, 0, 0, 0, 0, 0, 0, 0 },
    NULL_TRIGGER
};

/**
 * The CotMC trigger is only used to prevent fix Lakitu in place when Mario exits through the waterfall.
 */
struct CameraTrigger sCamCotMC[] = {
    { 1, cam_cotmc_exit_waterfall, 0, 1500, 3500, 550, 10000, 1500, 0 },
    NULL_TRIGGER
};

/**
 * The CCM triggers are used to set the flag that says when Mario is in the slide shortcut.
 */
struct CameraTrigger sCamCCM[] = {
    { 2, cam_ccm_enter_slide_shortcut, -4846, 2061, 27, 1229, 1342, 396, 0 },
    { 2, cam_ccm_leave_slide_shortcut, -6412, -3917, -6246, 307, 185, 132, 0 },
    NULL_TRIGGER
};

/**
 * The Castle triggers are used to set the camera to fixed mode when entering the lobby, and to set it
 * to close mode when leaving it. They also set the mode to spiral staircase.
 *
 * There are two triggers for looking up and down straight staircases when Mario is at the start,
 * and one trigger that starts the enter pool cutscene when Mario enters HMC.
 */
struct CameraTrigger sCamCastle[] = {
    { 1, cam_castle_close_mode, -1100, 657, -1346, 300, 150, 300, 0 },
    { 1, cam_castle_enter_lobby, -1099, 657, -803, 300, 150, 300, 0 },
    { 1, cam_castle_close_mode, -2304, -264, -4072, 140, 150, 140, 0 },
    { 1, cam_castle_close_mode, -2304, 145, -1344, 140, 150, 140, 0 },
    { 1, cam_castle_enter_lobby, -2304, 145, -802, 140, 150, 140, 0 },
    //! Sets the camera mode when leaving secret aquarium
    { 1, cam_castle_close_mode, 2816, 1200, -256, 100, 100, 100, 0 },
    { 1, cam_castle_close_mode, 256, -161, -4226, 140, 150, 140, 0 },
    { 1, cam_castle_close_mode, 256, 145, -1344, 140, 150, 140, 0 },
    { 1, cam_castle_enter_lobby, 256, 145, -802, 140, 150, 140, 0 },
    { 1, cam_castle_close_mode, -1023, 44, -4870, 140, 150, 140, 0 },
    { 1, cam_castle_close_mode, -459, 145, -1020, 140, 150, 140, 0x6000 },
    { 1, cam_castle_enter_lobby, -85, 145, -627, 140, 150, 140, 0 },
    { 1, cam_castle_close_mode, -1589, 145, -1020, 140, 150, 140, -0x6000 },
    { 1, cam_castle_enter_lobby, -1963, 145, -627, 140, 150, 140, 0 },
    { 1, cam_castle_leave_lobby_sliding_door, -2838, 657, -1659, 200, 150, 150, 0x2000 },
    { 1, cam_castle_enter_lobby_sliding_door, -2319, 512, -1266, 300, 150, 300, 0x2000 },
    { 1, cam_castle_close_mode, 844, 759, -1657, 40, 150, 40, -0x2000 },
    { 1, cam_castle_enter_lobby, 442, 759, -1292, 140, 150, 140, -0x2000 },
    { 2, cam_castle_enter_spiral_stairs, -1000, 657, 1740, 200, 300, 200, 0 },
    { 2, cam_castle_enter_spiral_stairs, -996, 1348, 1814, 200, 300, 200, 0 },
    { 2, cam_castle_close_mode, -946, 657, 2721, 50, 150, 50, 0 },
    { 2, cam_castle_close_mode, -996, 1348, 907, 50, 150, 50, 0 },
    { 2, cam_castle_close_mode, -997, 1348, 1450, 140, 150, 140, 0 },
    { 1, cam_castle_close_mode, -4942, 452, -461, 140, 150, 140, 0x4000 },
    { 1, cam_castle_close_mode, -3393, 350, -793, 140, 150, 140, 0x4000 },
    { 1, cam_castle_enter_lobby, -2851, 350, -792, 140, 150, 140, 0x4000 },
    { 1, cam_castle_enter_lobby, 803, 350, -228, 140, 150, 140, -0x4000 },
    //! Duplicate camera trigger outside JRB door
    { 1, cam_castle_enter_lobby, 803, 350, -228, 140, 150, 140, -0x4000 },
    { 1, cam_castle_close_mode, 1345, 350, -229, 140, 150, 140, 0x4000 },
    { 1, cam_castle_close_mode, -946, -929, 622, 300, 150, 300, 0 },
    { 2, cam_castle_look_upstairs, -205, 1456, 2508, 210, 928, 718, 0 },
    { 1, cam_castle_basement_look_downstairs, -1027, -587, -718, 318, 486, 577, 0 },
    { 1, cam_castle_lobby_entrance, -1023, 376, 1830, 300, 400, 300, 0 },
    { 3, cam_castle_hmc_start_pool_cutscene, 2485, -1689, -2659, 600, 50, 600, 0 },
    NULL_TRIGGER
};

/**
 * The BBH triggers are the most complex, they cause the camera to enter fixed mode for each room,
 * transition between rooms, and enter free roam when outside.
 *
 * The triggers are also responsible for warping the camera below platforms.
 */
struct CameraTrigger sCamBBH[] = {
    { 1, cam_bbh_enter_front_door, 742, 0, 2369, 200, 200, 200, 0 },
    { 1, cam_bbh_leave_front_door, 741, 0, 1827, 200, 200, 200, 0 },
    { 1, cam_bbh_room_1, 222, 0, 1458, 200, 200, 200, 0 },
    { 1, cam_bbh_room_1, 222, 0, 639, 200, 200, 200, 0 },
    { 1, cam_bbh_room_1, 435, 0, 222, 200, 200, 200, 0 },
    { 1, cam_bbh_room_1, 1613, 0, 222, 200, 200, 200, 0 },
    { 1, cam_bbh_room_1, 1827, 0, 1459, 200, 200, 200, 0 },
    { 1, cam_bbh_room_1, -495, 819, 1407, 200, 200, 200, 0 },
    { 1, cam_bbh_room_1, -495, 819, 640, 250, 200, 200, 0 },
    { 1, cam_bbh_room_1, 179, 819, 222, 200, 200, 200, 0 },
    { 1, cam_bbh_room_1, 1613, 819, 222, 200, 200, 200, 0 },
    { 1, cam_bbh_room_1, 1827, 819, 486, 200, 200, 200, 0 },
    { 1, cam_bbh_room_1, 1827, 819, 1818, 200, 200, 200, 0 },
    { 1, cam_bbh_room_2_lower, 2369, 0, 1459, 200, 200, 200, 0 },
    { 1, cam_bbh_room_2_lower, 3354, 0, 1347, 200, 200, 200, 0 },
    { 1, cam_bbh_room_2_lower, 2867, 514, 1843, 512, 102, 409, 0 },
    { 1, cam_bbh_room_4, 3354, 0, 804, 200, 200, 200, 0 },
    { 1, cam_bbh_room_4, 1613, 0, -320, 200, 200, 200, 0 },
    { 1, cam_bbh_room_8, 435, 0, -320, 200, 200, 200, 0 },
    { 1, cam_bbh_room_5_library, -2021, 0, 803, 200, 200, 200, 0 },
    { 1, cam_bbh_room_5_library, -320, 0, 640, 200, 200, 200, 0 },
    { 1, cam_bbh_room_5_library_to_hidden_transition, -1536, 358, -254, 716, 363, 102, 0 },
    { 1, cam_bbh_room_5_hidden_to_library_transition, -1536, 358, -459, 716, 363, 102, 0 },
    { 1, cam_bbh_room_5_hidden, -1560, 0, -1314, 200, 200, 200, 0 },
    { 1, cam_bbh_room_3, -320, 0, 1459, 200, 200, 200, 0 },
    { 1, cam_bbh_room_3, -2021, 0, 1345, 200, 200, 200, 0 },
    { 1, cam_bbh_room_2_library, 2369, 819, 486, 200, 200, 200, 0 },
    { 1, cam_bbh_room_2_library, 2369, 1741, 486, 200, 200, 200, 0 },
    { 1, cam_bbh_room_2_library_to_trapdoor_transition, 2867, 1228, 1174, 716, 414, 102, 0 },
    { 1, cam_bbh_room_2_trapdoor_transition, 2867, 1228, 1378, 716, 414, 102, 0 },
    { 1, cam_bbh_room_2_trapdoor, 2369, 819, 1818, 200, 200, 200, 0 },
    { 1, cam_bbh_room_9_attic, 1829, 1741, 486, 200, 200, 200, 0 },
    { 1, cam_bbh_room_9_attic, 741, 1741, 1587, 200, 200, 200, 0 },
    { 1, cam_bbh_room_9_attic_transition, 102, 2048, -191, 100, 310, 307, 0 },
    { 1, cam_bbh_room_9_mr_i_transition, 409, 2048, -191, 100, 310, 307, 0 },
    { 1, cam_bbh_room_13_balcony, 742, 1922, 2164, 200, 200, 200, 0 },
    { 1, cam_bbh_fall_off_roof, 587, 1322, 2677, 1000, 400, 600, 0 },
    { 1, cam_bbh_room_3, -1037, 819, 1408, 200, 200, 200, 0 },
    { 1, cam_bbh_room_3, -1970, 1024, 1345, 200, 200, 200, 0 },
    { 1, cam_bbh_room_8, 179, 819, -320, 200, 200, 200, 0 },
    { 1, cam_bbh_room_7_mr_i, 1613, 819, -320, 200, 200, 200, 0 },
    { 1, cam_bbh_room_7_mr_i_to_coffins_transition, 2099, 1228, -819, 102, 414, 716, 0 },
    { 1, cam_bbh_room_7_coffins_to_mr_i_transition, 2304, 1228, -819, 102, 414, 716, 0 },
    { 1, cam_bbh_room_6, -1037, 819, 640, 200, 200, 200, 0 },
    { 1, cam_bbh_room_6, -1970, 1024, 803, 200, 200, 200, 0 },
    { 1, cam_bbh_room_1, 1827, 819, 1818, 200, 200, 200, 0 },
    { 1, cam_bbh_fall_into_pool, 2355, -1112, -193, 1228, 500, 1343, 0 },
    { 1, cam_bbh_fall_into_pool, 2355, -1727, 1410, 1228, 500, 705, 0 },
    { 1, cam_bbh_elevator_room_lower, 0, -2457, 1827, 250, 200, 250, 0 },
    { 1, cam_bbh_elevator_room_lower, 0, -2457, 2369, 250, 200, 250, 0 },
    { 1, cam_bbh_elevator_room_lower, 0, -2457, 4929, 250, 200, 250, 0 },
    { 1, cam_bbh_elevator_room_lower, 0, -2457, 4387, 250, 200, 250, 0 },
    { 1, cam_bbh_room_0_back_entrance, 1887, -2457, 204, 250, 200, 250, 0 },
    { 1, cam_bbh_room_0, 1272, -2457, 204, 250, 200, 250, 0 },
    { 1, cam_bbh_room_0, -1681, -2457, 204, 250, 200, 250, 0 },
    { 1, cam_bbh_room_0_back_entrance, -2296, -2457, 204, 250, 200, 250, 0 },
    { 1, cam_bbh_elevator, -2939, -605, 5367, 800, 100, 800, 0 },
    { 1, cam_bbh_room_12_upper, -2939, -205, 5367, 300, 100, 300, 0 },
    { 1, cam_bbh_room_12_upper, -2332, -204, 4714, 250, 200, 250, 0x6000 },
    { 1, cam_bbh_room_0_back_entrance, -1939, -204, 4340, 250, 200, 250, 0x6000 },
    NULL_TRIGGER
};

#define _ NULL
#define STUB_LEVEL(_0, _1, _2, _3, _4, _5, _6, _7, cameratable) cameratable,
#define DEFINE_LEVEL(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, cameratable) cameratable,

/*
 * This table has an extra 2 levels after the last unknown_38 stub level. What I think
 * the programmer was thinking was that the table is null terminated and so used the
 * level count as a correspondence to the ID of the final level, but the enum represents
 * an ID *after* the last stub level, not before or during it.
 *
 * Each table is terminated with NULL_TRIGGER
 */
struct CameraTrigger *sCameraTriggers[LEVEL_COUNT + 1] = {
    NULL,
    #include "levels/level_defines.h"
};
#undef _
#undef STUB_LEVEL
#undef DEFINE_LEVEL

/**
 * Activates any CameraTriggers that Mario is inside.
 * Then, applies area-specific processing to the camera, such as setting the default mode, or changing
 * the mode based on the terrain type Mario is standing on.
 *
 * @return the camera's mode after processing, although this is unused in the code
 */
s16 camera_course_processing(struct Camera *c) {
    s16 level = gCurrLevelNum;
    s8 area = gCurrentArea->index;
    // Bounds iterator
    u32 b;
    // Camera trigger's bounding box
    Vec3f center, bounds;
    u32 insideBounds = FALSE;
    u8 oldMode = c->mode;

    if (c->mode == CAMERA_MODE_C_UP) {
        c->mode = sModeInfo.lastMode;
    }
    check_blocking_area_processing(&c->mode);
    if (level > LEVEL_COUNT + 1) {
        level = LEVEL_COUNT + 1;
    }

    if (sCameraTriggers[level] != NULL) {
        b = 0;

        // Process positional triggers.
        // All triggered events are called, not just the first one.
        while (sCameraTriggers[level][b].event != NULL) {

            // Check only the current area's triggers
            if (sCameraTriggers[level][b].area == area) {
                // Copy the bounding box into center and bounds
                vec3f_set(center, sCameraTriggers[level][b].centerX,
                                  sCameraTriggers[level][b].centerY,
                                  sCameraTriggers[level][b].centerZ);
                vec3f_set(bounds, sCameraTriggers[level][b].boundsX,
                                  sCameraTriggers[level][b].boundsY,
                                  sCameraTriggers[level][b].boundsZ);

                // Check if Mario is inside the bounds
                if (is_pos_in_bounds(sMarioCamState->pos, center, bounds,
                                                   sCameraTriggers[level][b].boundsYaw) == TRUE) {
                    //! This should be checked before calling is_pos_in_bounds. (It doesn't belong
                    //! outside the while loop because some events disable area processing)
                    if (!(sStatusFlags & CAM_FLAG_BLOCK_AREA_PROCESSING)) {
                        sCameraTriggers[level][b].event(c);
                        insideBounds = TRUE;
                    }
                }
            }

            if ((sCameraTriggers[level])[b].area == -1) {
                // Default triggers are only active if Mario is not already inside another trigger
                if (!insideBounds) {
                    if (!(sStatusFlags & CAM_FLAG_BLOCK_AREA_PROCESSING)) {
                        sCameraTriggers[level][b].event(c);
                    }
                }
            }

            b++;
        }
    }
#if defined(ENABLE_VANILLA_CAM_PROCESSING) && !defined(FORCED_CAMERA_MODE) && !defined(USE_COURSE_DEFAULT_MODE)
    // Area-specific camera processing
    if (!(sStatusFlags & CAM_FLAG_BLOCK_AREA_PROCESSING)) {
        switch (gCurrLevelArea) {
            case AREA_WF:
                if (sMarioCamState->action == ACT_RIDING_HOOT) {
                    transition_to_camera_mode(c, CAMERA_MODE_SLIDE_HOOT, 60);
                } else {
                    switch (sMarioGeometry.currFloorType) {
                        case SURFACE_CAMERA_8_DIR:
                            transition_to_camera_mode(c, CAMERA_MODE_8_DIRECTIONS, 90);
                            s8DirModeBaseYaw = DEGREES(90);
                            break;

                        case SURFACE_BOSS_FIGHT_CAMERA:
                            if (gCurrActNum == 1) {
                                set_camera_mode_boss_fight(c);
                            } else {
                                set_camera_mode_radial(c, 60);
                            }
                            break;
                        default:
                            set_camera_mode_radial(c, 60);
                    }
                }
                break;

            case AREA_BBH:
                // if camera is fixed at bbh_room_13_balcony_camera (but as floats)
                if (vec3f_compare(sFixedModeBasePosition, 210.f, 420.f, 3109.f) == TRUE) {
                    if (sMarioCamState->pos[1] < 1800.f) {
                        transition_to_camera_mode(c, CAMERA_MODE_CLOSE, 30);
                    }
                }
                break;

            case AREA_SSL_PYRAMID:
                set_mode_if_not_set_by_surface(c, CAMERA_MODE_OUTWARD_RADIAL);
                break;

            case AREA_SSL_OUTSIDE:
                set_mode_if_not_set_by_surface(c, CAMERA_MODE_RADIAL);
                break;

            case AREA_THI_HUGE:
                break;

            case AREA_THI_TINY:
                surface_type_modes_thi(c);
                break;

            case AREA_TTC:
                set_mode_if_not_set_by_surface(c, CAMERA_MODE_OUTWARD_RADIAL);
                break;

            case AREA_BOB:
                if (set_mode_if_not_set_by_surface(c, CAMERA_MODE_NONE) == 0) {
                    if (sMarioGeometry.currFloorType == SURFACE_BOSS_FIGHT_CAMERA) {
                        set_camera_mode_boss_fight(c);
                    } else {
                        if (c->mode == CAMERA_MODE_CLOSE) {
                            transition_to_camera_mode(c, CAMERA_MODE_RADIAL, 60);
                        } else {
                            set_camera_mode_radial(c, 60);
                        }
                    }
                }
                break;

            case AREA_WDW_MAIN:
                switch (sMarioGeometry.currFloorType) {
                    case SURFACE_INSTANT_WARP_1B:
                        c->defMode = CAMERA_MODE_RADIAL;
                        break;
                }
                break;

            case AREA_WDW_TOWN:
                switch (sMarioGeometry.currFloorType) {
                    case SURFACE_INSTANT_WARP_1C:
                        c->defMode = CAMERA_MODE_CLOSE;
                        break;
                }
                break;

            case AREA_DDD_WHIRLPOOL:
                //! @bug this does nothing
                gLakituState.defMode = CAMERA_MODE_OUTWARD_RADIAL;
                break;

            case AREA_DDD_SUB:
                if ((c->mode != CAMERA_MODE_BEHIND_MARIO)
                    && (c->mode != CAMERA_MODE_WATER_SURFACE)) {
                    if (((sMarioCamState->action & ACT_FLAG_ON_POLE) != 0)
                        || (sMarioGeometry.currFloorHeight > 800.f)) {
                        transition_to_camera_mode(c, CAMERA_MODE_8_DIRECTIONS, 60);

                    } else {
                        if (sMarioCamState->pos[1] < 800.f) {
                            transition_to_camera_mode(c, CAMERA_MODE_FREE_ROAM, 60);
                        }
                    }
                }
                //! @bug this does nothing
                gLakituState.defMode = CAMERA_MODE_FREE_ROAM;
                break;
        }
    }
#endif

    sStatusFlags &= ~CAM_FLAG_BLOCK_AREA_PROCESSING;
    if (oldMode == CAMERA_MODE_C_UP) {
        sModeInfo.lastMode = c->mode;
        c->mode = oldMode;
    }
    return c->mode;
}

/**
 * Move `pos` between the nearest floor and ceiling
 */
void resolve_geometry_collisions(Vec3f pos) {
    struct Surface *surf;

    f32_find_wall_collision(&pos[0], &pos[1], &pos[2], 0.f, 100.f);
    f32 floorY = find_floor(pos[0], pos[1] + 50.f, pos[2], &surf);
    f32 ceilY = find_ceil(pos[0], pos[1] - 50.f, pos[2], &surf);

    if ((FLOOR_LOWER_LIMIT != floorY) && (CELL_HEIGHT_LIMIT == ceilY)) {
        if (pos[1] < (floorY += 125.f)) {
            pos[1] = floorY;
        }
    }

    if ((FLOOR_LOWER_LIMIT == floorY) && (CELL_HEIGHT_LIMIT != ceilY)) {
        if (pos[1] > (ceilY -= 125.f)) {
            pos[1] = ceilY;
        }
    }

    if ((FLOOR_LOWER_LIMIT != floorY) && (CELL_HEIGHT_LIMIT != ceilY)) {
        floorY += 125.f;
        ceilY -= 125.f;

        if ((pos[1] <= floorY) && (pos[1] < ceilY)) {
            pos[1] = floorY;
        }
        if ((pos[1] > floorY) && (pos[1] >= ceilY)) {
            pos[1] = ceilY;
        }
        if ((pos[1] <= floorY) && (pos[1] >= ceilY)) {
            pos[1] = (floorY + ceilY) * 0.5f;
        }
    }
}

/**
 * Checks for any walls obstructing Mario from view, and calculates a new yaw that the camera should
 * rotate towards.
 *
 * @param[out] avoidYaw the angle (from Mario) that the camera should rotate towards to avoid the wall.
 *                      The camera then approaches avoidYaw until Mario is no longer obstructed.
 *                      avoidYaw is always parallel to the wall.
 * @param yawRange      how wide of an arc to check for walls obscuring Mario.
 *
 * @return 3 if a wall is covering Mario, 1 if a wall is only near the camera.
 */
s32 rotate_camera_around_walls(UNUSED struct Camera *c, Vec3f cPos, s16 *avoidYaw, s16 yawRange) {
    struct WallCollisionData colData;
    struct Surface *wall;
    f32 dummyDist, checkDist;
    f32 coarseRadius;
    f32 fineRadius;
    s16 wallYaw, horWallNorm;
    s16 dummyPitch;
    // The yaw of the vector from Mario to the camera.
    s16 yawFromMario;
    s32 status = 0;
    /// The current iteration. The algorithm takes 8 equal steps from Mario back to the camera.
    s32 step = 0;

    vec3f_get_dist_and_angle(sMarioCamState->pos, cPos, &dummyDist, &dummyPitch, &yawFromMario);
    sStatusFlags &= ~CAM_FLAG_CAM_NEAR_WALL;
    colData.offsetY = 100.0f;
    // The distance from Mario to Lakitu
    checkDist = 0.0f;
    /// The radius used to find potential walls to avoid.
    /// @bug Increases to 250.f, but the max collision radius is 200.f
    coarseRadius = 150.0f;
    /// This only increases when there is a wall collision found in the coarse pass
    fineRadius = 100.0f;

    for (step = 0; step < 8; step++) {
        // Start at Mario, move backwards to Lakitu's position
        colData.x = sMarioCamState->pos[0] + ((cPos[0] - sMarioCamState->pos[0]) * checkDist);
        colData.y = sMarioCamState->pos[1] + ((cPos[1] - sMarioCamState->pos[1]) * checkDist);
        colData.z = sMarioCamState->pos[2] + ((cPos[2] - sMarioCamState->pos[2]) * checkDist);
        colData.radius = coarseRadius;
        // Increase the coarse check radius
        camera_approach_f32_symmetric_bool(&coarseRadius, 250.f, 30.f);

        if (find_wall_collisions(&colData) != 0) {
            wall = colData.walls[colData.numWalls - 1];

            // If we're over halfway from Mario to Lakitu, then there's a wall near the camera, but
            // not necessarily obstructing Mario
            if (step >= 5) {
                sStatusFlags |= CAM_FLAG_CAM_NEAR_WALL;
                if (status <= 0) {
                    status = 1;
                    wall = colData.walls[colData.numWalls - 1];
                    // wallYaw is parallel to the wall, not perpendicular
                    wallYaw = atan2s(wall->normal.z, wall->normal.x) + DEGREES(90);
                    // Calculate the avoid direction. The function returns the opposite direction so add 180
                    // degrees.
                    *avoidYaw = calc_avoid_yaw(yawFromMario, wallYaw) + DEGREES(180);
                }
            }

            colData.x = sMarioCamState->pos[0] + ((cPos[0] - sMarioCamState->pos[0]) * checkDist);
            colData.y = sMarioCamState->pos[1] + ((cPos[1] - sMarioCamState->pos[1]) * checkDist);
            colData.z = sMarioCamState->pos[2] + ((cPos[2] - sMarioCamState->pos[2]) * checkDist);
            colData.radius = fineRadius;
            // Increase the fine check radius
            camera_approach_f32_symmetric_bool(&fineRadius, 200.f, 20.f);

            if (find_wall_collisions(&colData) != 0) {
                wall = colData.walls[colData.numWalls - 1];
                horWallNorm = atan2s(wall->normal.z, wall->normal.x);
                wallYaw = horWallNorm + DEGREES(90);
                // If Mario would be blocked by the surface, then avoid it
                if ((is_range_behind_surface(sMarioCamState->pos, cPos, wall, yawRange, SURFACE_WALL_MISC) == 0)
                    && (is_mario_behind_surface(c, wall) == TRUE)
                    // Also check if the wall is tall enough to cover Mario
                    && (is_surf_within_bounding_box(wall, -1.f, 150.f, -1.f) == FALSE)) {
                    // Calculate the avoid direction. The function returns the opposite direction so add 180
                    // degrees.
                    *avoidYaw = calc_avoid_yaw(yawFromMario, wallYaw) + DEGREES(180);
                    camera_approach_s16_symmetric_bool(avoidYaw, horWallNorm, yawRange);
                    status = 3;
                    step = 8;
                }
            }
        }
        checkDist += 0.125f;
    }

    return status;
}

/**
 * Stores type and height of the nearest floor and ceiling to Mario in `pg`
 *
 * Note: Also finds the water level, but waterHeight is unused
 */
void find_mario_floor_and_ceil(struct PlayerGeometry *pg) {
    struct Surface *surf;
    s32 tempCollisionFlags = gCollisionFlags;
    gCollisionFlags |= COLLISION_FLAG_CAMERA;

    if (find_floor(sMarioCamState->pos[0], sMarioCamState->pos[1] + 10.f,
                   sMarioCamState->pos[2], &surf) != FLOOR_LOWER_LIMIT) {
        pg->currFloorType = surf->type;
    } else {
        pg->currFloorType = 0;
    }

    if (find_ceil(sMarioCamState->pos[0], sMarioCamState->pos[1] - 10.f,
                  sMarioCamState->pos[2], &surf) != CELL_HEIGHT_LIMIT) {
        pg->currCeilType = surf->type;
    } else {
        pg->currCeilType = 0;
    }

    gCollisionFlags &= ~COLLISION_FLAG_CAMERA;
    pg->currFloorHeight = find_floor(sMarioCamState->pos[0],
                                     sMarioCamState->pos[1] + 10.f,
                                     sMarioCamState->pos[2], &pg->currFloor);
    pg->currCeilHeight = find_ceil(sMarioCamState->pos[0],
                                   sMarioCamState->pos[1] - 10.f,
                                   sMarioCamState->pos[2], &pg->currCeil);
    pg->waterHeight = find_water_level(sMarioCamState->pos[0], sMarioCamState->pos[2]);
    gCollisionFlags = tempCollisionFlags;
}

/**
 * Start a cutscene focusing on an object
 * This will play if nothing else happened in the same frame, like exiting or warping.
 */
void start_object_cutscene(u8 cutscene, struct Object *obj) {
    sObjectCutscene = cutscene;
    gRecentCutscene = CUTSCENE_NONE;
    gCutsceneFocus = obj;
    gObjCutsceneDone = FALSE;
}

/**
 * Start a low-priority cutscene without focusing on an object
 * This will play if nothing else happened in the same frame, like exiting or warping.
 */
void start_object_cutscene_without_focus(u8 cutscene) {
    sObjectCutscene = cutscene;
    sCutsceneDialogResponse = DIALOG_RESPONSE_NONE;
}

UNUSED s32 unused_dialog_cutscene_response(u8 cutscene) {
    // if not in a cutscene, start this one
    if ((gCamera->cutscene == 0) && (sObjectCutscene == 0)) {
        sObjectCutscene = cutscene;
    }

    // if playing this cutscene and Mario responded, return the response
    if ((gCamera->cutscene == cutscene) && (sCutsceneDialogResponse)) {
        return sCutsceneDialogResponse;
    } else {
        return 0;
    }
}

s16 cutscene_object_with_dialog(u8 cutscene, struct Object *obj, s16 dialogID) {
    s16 response = DIALOG_RESPONSE_NONE;

    if ((gCamera->cutscene == CUTSCENE_NONE) && (sObjectCutscene == CUTSCENE_NONE)) {
        if (gRecentCutscene != cutscene) {
            start_object_cutscene(cutscene, obj);
            if (dialogID != DIALOG_NONE) {
                sCutsceneDialogID = dialogID;
            } else {
                sCutsceneDialogID = DIALOG_001;
            }
        } else {
            response = sCutsceneDialogResponse;
        }

        gRecentCutscene = CUTSCENE_NONE;
    }
    return response;
}

s16 cutscene_object_without_dialog(u8 cutscene, struct Object *obj) {
    return cutscene_object_with_dialog(cutscene, obj, DIALOG_NONE);
}

/**
 * @return 0 if not started, 1 if started, and -1 if finished
 */
s16 cutscene_object(u8 cutscene, struct Object *obj) {
    s16 status = 0;

    if ((gCamera->cutscene == 0) && (sObjectCutscene == 0)) {
        if (gRecentCutscene != cutscene) {
            start_object_cutscene(cutscene, obj);
            status = 1;
        } else {
            status = -1;
        }
    }
    return status;
}

/**
 * Update the camera's yaw and nextYaw. This is called from cutscenes to ignore the camera mode's yaw.
 */
void update_camera_yaw(struct Camera *c) {
    c->nextYaw = calculate_yaw(c->focus, c->pos);
    c->yaw = c->nextYaw;
}

void cutscene_reset_spline(void) {
    sCutsceneSplineSegment = 0;
    sCutsceneSplineSegmentProgress = 0;
}

void stop_cutscene_and_retrieve_stored_info(struct Camera *c) {
    gCutsceneTimer = CUTSCENE_STOP;
    c->cutscene = 0;
    vec3f_copy(c->focus, sCameraStoreCutscene.focus);
    vec3f_copy(c->pos, sCameraStoreCutscene.pos);
}

void cap_switch_save(UNUSED s16 param) {
    save_file_do_save(gCurrSaveFileNum - 1);
}

void init_spline_point(struct CutsceneSplinePoint *splinePoint, s8 index, u8 speed, Vec3s point) {
    splinePoint->index = index;
    splinePoint->speed = speed;
    vec3s_copy(splinePoint->point, point);
}

// TODO: (Scrub C)
void copy_spline_segment(struct CutsceneSplinePoint dst[], struct CutsceneSplinePoint src[]) {
    s32 j = 0;
    s32 i = 0;

    init_spline_point(&dst[i], src[j].index, src[j].speed, src[j].point);
    i++;
    do {
        do {
            init_spline_point(&dst[i], src[j].index, src[j].speed, src[j].point);
            i++;
            j++;
        } while (src[j].index != -1);
    } while (j > 16);

    // Create the end of the spline by duplicating the last point
    init_spline_point(&dst[i + 0],  0, src[j].speed, src[j].point);
    init_spline_point(&dst[i + 1],  0,            0, src[j].point);
    init_spline_point(&dst[i + 2],  0,            0, src[j].point);
    init_spline_point(&dst[i + 3], -1,            0, src[j].point);
}

/**
 * Triggers Mario to enter a dialog state. This is used to make Mario look at the focus of a cutscene,
 * for example, bowser.
 * @param state 0 = stop, 1 = start, 2 = start and look up, and 3 = start and look down
 *
 * @return if Mario left the dialog state, return CUTSCENE_LOOP, else return gCutsceneTimer
 */
s16 cutscene_common_set_dialog_state(s32 state) {
    s16 timer = gCutsceneTimer;
    // If the dialog ended, return CUTSCENE_LOOP, which would end the cutscene shot
    if (set_mario_npc_dialog(state) == MARIO_DIALOG_STATUS_SPEAK) {
        timer = CUTSCENE_LOOP;
    }
    return timer;
}

/// Unused SSL cutscene?
static UNUSED void unused_cutscene_mario_dialog_looking_down(UNUSED struct Camera *c) {
    gCutsceneTimer = cutscene_common_set_dialog_state(MARIO_DIALOG_LOOK_DOWN);
}

/**
 * Cause Mario to enter the normal dialog state.
 */
void cutscene_mario_dialog(UNUSED struct Camera *c) {
    gCutsceneTimer = cutscene_common_set_dialog_state(MARIO_DIALOG_LOOK_FRONT);
}

/// Unused SSL cutscene?
static UNUSED void unused_cutscene_mario_dialog_looking_up(UNUSED struct Camera *c) {
    gCutsceneTimer = cutscene_common_set_dialog_state(MARIO_DIALOG_LOOK_UP);
}

/**
 * Lower the volume (US only) and start the peach letter background music
 */
void cutscene_intro_peach_start_letter_music(UNUSED struct Camera *c) {
#if defined(VERSION_US) || defined(VERSION_SH)
    seq_player_lower_volume(SEQ_PLAYER_LEVEL, 60, 40);
#endif
    cutscene_intro_peach_play_message_music();
}

/**
 * Raise the volume (not in JP) and start the flying music.
 */
void cutscene_intro_peach_start_flying_music(UNUSED struct Camera *c) {
    seq_player_unlower_volume(SEQ_PLAYER_LEVEL, 60);
    cutscene_intro_peach_play_lakitu_flying_music();
}

#ifdef VERSION_EU
/**
 * Lower the volume for the letter background music. In US, this happens on the same frame as the music
 * starts.
 */
void cutscene_intro_peach_eu_lower_volume(UNUSED struct Camera *c) {
    seq_player_lower_volume(SEQ_PLAYER_LEVEL, 60, 40);
}
#endif

void reset_pan_distance(UNUSED struct Camera *c) {
    sPanDistance = 0;
}

/**
 * Easter egg: the player 2 controller can move the camera's focus in the ending and credits.
 */
void player2_rotate_cam(struct Camera *c, s16 minPitch, s16 maxPitch, s16 minYaw, s16 maxYaw) {
    f32 distCamToFocus;
    s16 pitch, yaw, pitchCap;

    // Change the camera rotation to match the 2nd player's stick
    approach_s16_asymptotic_bool(&sCreditsPlayer2Yaw, -(s16)(gPlayer2Controller->stickX * 250.f), 4);
    approach_s16_asymptotic_bool(&sCreditsPlayer2Pitch, -(s16)(gPlayer2Controller->stickY * 265.f), 4);
    vec3f_get_dist_and_angle(c->pos, c->focus, &distCamToFocus, &pitch, &yaw);

    pitchCap = 0x3800 - pitch;
    if (pitchCap < 0) {
        pitchCap = 0;
    }
    if (maxPitch > pitchCap) {
        maxPitch = pitchCap;
    }

    pitchCap = -0x3800 - pitch;
    if (pitchCap > 0) {
        pitchCap = 0;
    }
    if (minPitch < pitchCap) {
        minPitch = pitchCap;
    }

    if (sCreditsPlayer2Pitch > maxPitch) {
        sCreditsPlayer2Pitch = maxPitch;
    }
    if (sCreditsPlayer2Pitch < minPitch) {
        sCreditsPlayer2Pitch = minPitch;
    }

    if (sCreditsPlayer2Yaw > maxYaw) {
        sCreditsPlayer2Yaw = maxYaw;
    }
    if (sCreditsPlayer2Yaw < minYaw) {
        sCreditsPlayer2Yaw = minYaw;
    }

    pitch += sCreditsPlayer2Pitch;
    yaw += sCreditsPlayer2Yaw;
    vec3f_set_dist_and_angle(c->pos, sPlayer2FocusOffset, distCamToFocus, pitch, yaw);
    vec3f_sub(sPlayer2FocusOffset, c->focus);
}

/**
 * Store camera info for the cannon opening cutscene
 */
void store_info_cannon(struct Camera *c) {
    vec3f_copy(sCameraStoreCutscene.pos, c->pos);
    vec3f_copy(sCameraStoreCutscene.focus, c->focus);
    sCameraStoreCutscene.panDist = sPanDistance;
    sCameraStoreCutscene.cannonYOffset = sCannonYOffset;
}

/**
 * Retrieve camera info for the cannon opening cutscene
 */
void retrieve_info_cannon(struct Camera *c) {
    vec3f_copy(c->pos, sCameraStoreCutscene.pos);
    vec3f_copy(c->focus, sCameraStoreCutscene.focus);
    sPanDistance = sCameraStoreCutscene.panDist;
    sCannonYOffset = sCameraStoreCutscene.cannonYOffset;
}

/**
 * Store camera info for the star spawn cutscene
 */
void store_info_star(struct Camera *c) {
    reset_pan_distance(c);
    vec3f_copy(sCameraStoreCutscene.pos, c->pos);
    sCameraStoreCutscene.focus[0] = sMarioCamState->pos[0];
    sCameraStoreCutscene.focus[1] = c->focus[1];
    sCameraStoreCutscene.focus[2] = sMarioCamState->pos[2];
}

/**
 * Retrieve camera info for the star spawn cutscene
 */
void retrieve_info_star(struct Camera *c) {
    vec3f_copy(c->pos, sCameraStoreCutscene.pos);
    vec3f_copy(c->focus, sCameraStoreCutscene.focus);
}

/**
 * Rotate the camera's focus around the camera's position by incYaw and incPitch
 */
void pan_camera(struct Camera *c, s16 incPitch, s16 incYaw) {
    f32 distCamToFocus;
    s16 pitch, yaw;

    vec3f_get_dist_and_angle(c->pos, c->focus, &distCamToFocus, &pitch, &yaw);
    pitch += incPitch;
    yaw += incYaw;
    vec3f_set_dist_and_angle(c->pos, c->focus, distCamToFocus, pitch, yaw);
}

void cutscene_shake_explosion(UNUSED struct Camera *c) {
    set_environmental_camera_shake(SHAKE_ENV_EXPLOSION);
    cutscene_set_fov_shake_preset(1);
}

static UNUSED void unused_start_bowser_bounce_shake(UNUSED struct Camera *c) {
    set_environmental_camera_shake(SHAKE_ENV_BOWSER_THROW_BOUNCE);
}

/**
 * Change the spherical coordinates of `to` relative to `from` by `incDist`, `incPitch`, and `incYaw`
 *
 * @param from    the base position
 * @param[out] to the destination position
 */
void rotate_and_move_vec3f(Vec3f to, Vec3f from, f32 incDist, s16 incPitch, s16 incYaw) {
    f32 dist;
    s16 pitch, yaw;

    vec3f_get_dist_and_angle(from, to, &dist, &pitch, &yaw);
    pitch += incPitch;
    yaw += incYaw;
    dist += incDist;
    vec3f_set_dist_and_angle(from, to, dist, pitch, yaw);
}

void set_flag_post_door(struct Camera *c) {
    sStatusFlags |= CAM_FLAG_BEHIND_MARIO_POST_DOOR;
    sCameraYawAfterDoorCutscene = calculate_yaw(c->focus, c->pos);
}

void cutscene_soften_music(UNUSED struct Camera *c) {
    seq_player_lower_volume(SEQ_PLAYER_LEVEL, 60, 40);
}

void cutscene_unsoften_music(UNUSED struct Camera *c) {
    seq_player_unlower_volume(SEQ_PLAYER_LEVEL, 60);
}

/**
 * Adjust the camera focus towards a point `dist` units in front of Mario.
 * @param dist distance in Mario's forward direction. Note that this is relative to Mario, so a negative
 *        distance will focus in front of Mario, and a positive distance will focus behind him.
 */
void focus_in_front_of_mario(struct Camera *c, f32 dist, f32 speed) {
    Vec3f goalFocus, offset;

    offset[0] = 0.f;
    offset[2] = dist;
    offset[1] = 100.f;

    offset_rotated(goalFocus, sMarioCamState->pos, offset, sMarioCamState->faceAngle);
    approach_vec3f_asymptotic(c->focus, goalFocus, speed, speed, speed);
}

/**
 * If the camera's yaw is out of the range of `absYaw` +- `yawMax`, then set the yaw to `absYaw`
 */
void star_dance_bound_yaw(struct Camera *c, s16 absYaw, s16 yawMax) {
    s16 yaw;

    vec3f_get_yaw(sMarioCamState->pos, c->pos, &yaw);
    s16 yawFromAbs = yaw - absYaw;

    // Because angles are s16, this checks if yaw is negative
    if ((yawFromAbs & 0x8000) != 0) {
        yawFromAbs = -yawFromAbs;
    }
    if (yawFromAbs > yawMax) {
        yaw = absYaw;
        c->nextYaw = yaw;
        c->yaw = yaw;
    }
}

/**
 * Moves the camera to Mario's side when Mario starts ACT_WATER_DEATH
 * Note that ACT_WATER_DEATH only starts when Mario gets hit by an enemy under water. It does not start
 * when Mario drowns.
 */
void water_death_move_to_mario_side(struct Camera *c) {
    f32 dist;
    s16 pitch, yaw;

    vec3f_get_dist_and_angle(sMarioCamState->pos, c->pos, &dist, &pitch, &yaw);
    approach_s16_asymptotic_bool(&yaw, (sMarioCamState->faceAngle[1] - 0x3000), 8);
    vec3f_set_dist_and_angle(sMarioCamState->pos, c->pos, dist, pitch, yaw);
}

/**
 * Ends the double door cutscene.
 */
void cutscene_double_doors_end(struct Camera *c) {
    set_flag_post_door(c);
    c->cutscene = 0;
    sStatusFlags |= CAM_FLAG_SMOOTH_MOVEMENT;
}

/* TODO:
 * The next two arrays are both related to levels, and they look generated.
 * These should be split into their own file.
 */

/**
 * Converts the u32 given in DEFINE_COURSE to a u8 with the odd and even digits rotated into the right
 * order for sDanceCutsceneIndexTable
 */
#define DROT(value, index) ((value >> (32 - (index + 1) * 8)) & 0xF0) >> 4 | \
                           ((value >> (32 - (index + 1) * 8)) & 0x0F) << 4

#define DANCE_ENTRY(c) { DROT(c, 0), DROT(c, 1), DROT(c, 2), DROT(c, 3) },

#define DEFINE_COURSE(_0, cutscenes) DANCE_ENTRY(cutscenes)
#define DEFINE_COURSES_END()
#define DEFINE_BONUS_COURSE(_0, cutscenes) DANCE_ENTRY(cutscenes)

/**
 * Each hex digit is an index into sDanceCutsceneTable.
 *
 * 0: Lakitu flies away after the dance
 * 1: Only rotates the camera, doesn't zoom out
 * 2: The camera goes to a close up of Mario
 * 3: Bowser keys and the grand star
 * 4: Default, used for 100 coin stars, 8 red coin stars in bowser levels, and secret stars
 */
u8 sDanceCutsceneIndexTable[][4] = {
    #include "levels/course_defines.h"
    { 0x44, 0x44, 0x44, 0x04 }, // (26) Why go to all this trouble to save bytes and do this?!
};
#undef DEFINE_COURSE
#undef DEFINE_COURSES_END
#undef DEFINE_BONUS_COURSE

#undef DANCE_ENTRY
#undef DROT

/**
 * These masks set whether or not the camera zooms out when game is paused.
 *
 * Each entry is used by two levels. Even levels use the low 4 bits, odd levels use the high 4 bits
 * Because areas are 1-indexed, a mask of 0x1 will make area 1 (not area 0) zoom out.
 *
 * In zoom_out_if_paused_and_outside(), the current area is converted to a shift.
 * Then the value of (1 << shift) is &'d with the level's mask,
 * and if the result is non-zero, the camera will zoom out.
 */
u8 sZoomOutAreaMasks[] = {
    ZOOMOUT_AREA_MASK(0,0,0,0, 0,0,0,0), // Unused         | Unused
    ZOOMOUT_AREA_MASK(0,0,0,0, 0,0,0,0), // Unused         | Unused
    ZOOMOUT_AREA_MASK(0,0,0,0, 1,0,0,0), // BBH            | CCM
    ZOOMOUT_AREA_MASK(0,0,0,0, 0,0,0,0), // CASTLE_INSIDE  | HMC
    ZOOMOUT_AREA_MASK(1,0,0,0, 1,0,0,0), // SSL            | BOB
    ZOOMOUT_AREA_MASK(1,0,0,0, 1,0,0,0), // SL             | WDW
    ZOOMOUT_AREA_MASK(0,0,0,0, 1,1,0,0), // JRB            | THI
    ZOOMOUT_AREA_MASK(0,0,0,0, 1,0,0,0), // TTC            | RR
    ZOOMOUT_AREA_MASK(1,0,0,0, 1,0,0,0), // CASTLE_GROUNDS | BITDW
    ZOOMOUT_AREA_MASK(0,0,0,0, 1,0,0,0), // VCUTM          | BITFS
    ZOOMOUT_AREA_MASK(0,0,0,0, 1,0,0,0), // SA             | BITS
    ZOOMOUT_AREA_MASK(1,0,0,0, 0,0,0,0), // LLL            | DDD
    ZOOMOUT_AREA_MASK(1,0,0,0, 0,0,0,0), // WF             | ENDING
    ZOOMOUT_AREA_MASK(0,0,0,0, 0,0,0,0), // COURTYARD      | PSS
    ZOOMOUT_AREA_MASK(0,0,0,0, 1,0,0,0), // COTMC          | TOTWC
    ZOOMOUT_AREA_MASK(1,0,0,0, 1,0,0,0), // BOWSER_1       | WMOTR
    ZOOMOUT_AREA_MASK(0,0,0,0, 1,0,0,0), // Unused         | BOWSER_2
    ZOOMOUT_AREA_MASK(1,0,0,0, 0,0,0,0), // BOWSER_3       | Unused
    ZOOMOUT_AREA_MASK(1,0,0,0, 0,0,0,0), // TTM            | Unused
    ZOOMOUT_AREA_MASK(0,0,0,0, 0,0,0,0), // Unused         | Unused
};

STATIC_ASSERT(ARRAY_COUNT(sZoomOutAreaMasks) - 1 == LEVEL_MAX / 2, "Make sure you edit sZoomOutAreaMasks when adding / removing courses.");

/**
 * Start shaking the camera's field of view.
 *
 * @param shakeSpeed How fast the shake should progress through its period. The shake offset is
 *                   calculated from coss(), so this parameter can be thought of as an angular velocity.
 */
void set_fov_shake(s16 amplitude, s16 decay, s16 shakeSpeed) {
    if (amplitude > sFOVState.shakeAmplitude) {
        sFOVState.shakeAmplitude = amplitude;
        sFOVState.decay = decay;
        sFOVState.shakeSpeed = shakeSpeed;
    }
}

/**
 * Start shaking the camera's field of view, but reduce `amplitude` by distance from camera
 */
void set_fov_shake_from_point(s16 amplitude, s16 decay, s16 shakeSpeed, f32 maxDist, f32 posX, f32 posY, f32 posZ) {
    amplitude = reduce_by_dist_from_camera(amplitude, maxDist, posX, posY, posZ);

    if (amplitude != 0) {
        set_fov_shake(amplitude, decay, shakeSpeed);
    }
}

static UNUSED void unused_deactivate_sleeping_camera(UNUSED struct MarioState *m) {
    sStatusFlags &= ~CAM_FLAG_SLEEPING;
}




/**
 * Change the camera's FOV mode.
 *
 * @see geo_camera_fov
 */
void set_fov_function(u8 func) {
    sFOVState.fovFunc = func;
}

/**
 * Start a preset fov shake. Used in cutscenes
 */
void cutscene_set_fov_shake_preset(u8 preset) {
    switch (preset) {
        case 1:
            set_fov_shake(0x100, 0x30, 0x8000);
            break;
        case 2:
            set_fov_shake(0x400, 0x20, 0x4000);
            break;
    }
}

/**
 * Start a preset fov shake that is reduced by the point's distance from the camera.
 * Used in set_camera_shake_from_point
 *
 * @see set_camera_shake_from_point
 */
void set_fov_shake_from_point_preset(u8 preset, f32 posX, f32 posY, f32 posZ) {
    switch (preset) {
        case SHAKE_FOV_SMALL:
            set_fov_shake_from_point(0x100, 0x30, 0x8000, 3000.f, posX, posY, posZ);
            break;
        case SHAKE_FOV_MEDIUM:
            set_fov_shake_from_point(0x200, 0x30, 0x8000, 4000.f, posX, posY, posZ);
            break;
        case SHAKE_FOV_LARGE:
            set_fov_shake_from_point(0x300, 0x30, 0x8000, 6000.f, posX, posY, posZ);
            break;
        case SHAKE_FOV_UNUSED:
            set_fov_shake_from_point(0x800, 0x20, 0x4000, 3000.f, posX, posY, posZ);
            break;
    }
}

/**
 * Offset an object's position in a random direction within the given bounds.
 */
static UNUSED void unused_displace_obj_randomly(struct Object *obj, f32 xRange, f32 yRange, f32 zRange) {
    f32 rnd = random_float();

    obj->oPosX += (rnd * xRange - xRange / 2.f);
    obj->oPosY += (rnd * yRange - yRange / 2.f);
    obj->oPosZ += (rnd * zRange - zRange / 2.f);
}

/**
 * Rotate an object in a random direction within the given bounds.
 */
static UNUSED void unused_rotate_obj_randomly(struct Object *obj, f32 pitchRange, f32 yawRange) {
    f32 rnd = random_float();

    obj->oMoveAnglePitch += (s16)(rnd * pitchRange - pitchRange / 2.f);
    obj->oMoveAngleYaw   += (s16)(rnd *   yawRange -   yawRange / 2.f);
}

/**
 * Rotate the object towards the point `point`.
 */
void obj_rotate_towards_point(struct Object *obj, Vec3f point, s16 pitchOff, s16 yawOff, s16 pitchDiv, s16 yawDiv) {
    f32 dist;
    s16 pitch, yaw;
    Vec3f oPos;

    object_pos_to_vec3f(oPos, obj);
    vec3f_get_dist_and_angle(oPos, point, &dist, &pitch, &yaw);
    obj->oMoveAnglePitch = approach_s16_asymptotic(obj->oMoveAnglePitch, pitchOff - pitch, pitchDiv);
    obj->oMoveAngleYaw = approach_s16_asymptotic(obj->oMoveAngleYaw, yaw + yawOff, yawDiv);
}

#include "behaviors/intro_peach.inc.c"
#include "behaviors/intro_lakitu.inc.c"
#include "behaviors/end_birds_1.inc.c"
#include "behaviors/end_birds_2.inc.c"
#include "behaviors/intro_scene.inc.c"
