#ifndef CAMERA_H
#define CAMERA_H

#include <PR/ultratypes.h>

#include "types.h"
#include "area.h"
#include "engine/geo_layout.h"
#include "engine/graph_node.h"
#include "puppycam2.h"

#include "level_table.h"

// X position of the mirror
#define CASTLE_MIRROR_X 4331.53f

/**
 * @file camera.h
 * Constants, defines, and structs used by the camera system.
 * @see camera.c
 */

#ifndef ABS2
#define ABS2(x) ((x) >= 0.f ? (x) : -(x))
#endif

#define LEVEL_AREA_INDEX(levelNum, areaNum) (((levelNum) << 4) + (areaNum))

/**
 * Helper macro for defining which areas of a level should zoom out the camera when the game is paused.
 * Because a mask is used by two levels, the pattern will repeat when more than 4 areas are used by a level.
 */
#define ZOOMOUT_AREA_MASK(level1Area1, level1Area2, level1Area3, level1Area4, \
                          level2Area1, level2Area2, level2Area3, level2Area4) \
    ((level2Area4) << 7 |                                                     \
     (level2Area3) << 6 |                                                     \
     (level2Area2) << 5 |                                                     \
     (level2Area1) << 4 |                                                     \
     (level1Area4) << 3 |                                                     \
     (level1Area3) << 2 |                                                     \
     (level1Area2) << 1 |                                                     \
     (level1Area1) << 0)


#define AREA_BBH                LEVEL_AREA_INDEX(LEVEL_BBH, 1)
#define AREA_CCM_OUTSIDE        LEVEL_AREA_INDEX(LEVEL_CCM, 1)
#define AREA_CCM_SLIDE          LEVEL_AREA_INDEX(LEVEL_CCM, 2)
#define AREA_CASTLE_LOBBY       LEVEL_AREA_INDEX(LEVEL_CASTLE, 1)
#define AREA_CASTLE_TIPPY       LEVEL_AREA_INDEX(LEVEL_CASTLE, 2)
#define AREA_CASTLE_BASEMENT    LEVEL_AREA_INDEX(LEVEL_CASTLE, 3)
#define AREA_HMC                LEVEL_AREA_INDEX(LEVEL_HMC, 1)
#define AREA_SSL_OUTSIDE        LEVEL_AREA_INDEX(LEVEL_SSL, 1)
#define AREA_SSL_PYRAMID        LEVEL_AREA_INDEX(LEVEL_SSL, 2)
#define AREA_SSL_EYEROK         LEVEL_AREA_INDEX(LEVEL_SSL, 3)
#define AREA_BOB                LEVEL_AREA_INDEX(LEVEL_BOB, 1)
#define AREA_SL_OUTSIDE         LEVEL_AREA_INDEX(LEVEL_SL, 1)
#define AREA_SL_IGLOO           LEVEL_AREA_INDEX(LEVEL_SL, 2)
#define AREA_WDW_MAIN           LEVEL_AREA_INDEX(LEVEL_WDW, 1)
#define AREA_WDW_TOWN           LEVEL_AREA_INDEX(LEVEL_WDW, 2)
#define AREA_JRB_MAIN           LEVEL_AREA_INDEX(LEVEL_JRB, 1)
#define AREA_JRB_SHIP           LEVEL_AREA_INDEX(LEVEL_JRB, 2)
#define AREA_THI_HUGE           LEVEL_AREA_INDEX(LEVEL_THI, 1)
#define AREA_THI_TINY           LEVEL_AREA_INDEX(LEVEL_THI, 2)
#define AREA_THI_WIGGLER        LEVEL_AREA_INDEX(LEVEL_THI, 3)
#define AREA_TTC                LEVEL_AREA_INDEX(LEVEL_TTC, 1)
#define AREA_RR                 LEVEL_AREA_INDEX(LEVEL_RR, 1)
#define AREA_CASTLE_GROUNDS     LEVEL_AREA_INDEX(LEVEL_CASTLE_GROUNDS, 1)
#define AREA_BITDW              LEVEL_AREA_INDEX(LEVEL_BITDW, 1)
#define AREA_VCUTM              LEVEL_AREA_INDEX(LEVEL_VCUTM, 1)
#define AREA_BITFS              LEVEL_AREA_INDEX(LEVEL_BITFS, 1)
#define AREA_SA                 LEVEL_AREA_INDEX(LEVEL_SA, 1)
#define AREA_BITS               LEVEL_AREA_INDEX(LEVEL_BITS, 1)
#define AREA_LLL_OUTSIDE        LEVEL_AREA_INDEX(LEVEL_LLL, 1)
#define AREA_LLL_VOLCANO        LEVEL_AREA_INDEX(LEVEL_LLL, 2)
#define AREA_DDD_WHIRLPOOL      LEVEL_AREA_INDEX(LEVEL_DDD, 1)
#define AREA_DDD_SUB            LEVEL_AREA_INDEX(LEVEL_DDD, 2)
#define AREA_WF                 LEVEL_AREA_INDEX(LEVEL_WF, 1)
#define AREA_ENDING             LEVEL_AREA_INDEX(LEVEL_ENDING, 1)
#define AREA_COURTYARD          LEVEL_AREA_INDEX(LEVEL_CASTLE_COURTYARD, 1)
#define AREA_PSS                LEVEL_AREA_INDEX(LEVEL_PSS, 1)
#define AREA_COTMC              LEVEL_AREA_INDEX(LEVEL_COTMC, 1)
#define AREA_TOTWC              LEVEL_AREA_INDEX(LEVEL_TOTWC, 1)
#define AREA_BOWSER_1           LEVEL_AREA_INDEX(LEVEL_BOWSER_1, 1)
#define AREA_WMOTR              LEVEL_AREA_INDEX(LEVEL_WMOTR, 1)
#define AREA_BOWSER_2           LEVEL_AREA_INDEX(LEVEL_BOWSER_2, 1)
#define AREA_BOWSER_3           LEVEL_AREA_INDEX(LEVEL_BOWSER_3, 1)
#define AREA_TTM_OUTSIDE        LEVEL_AREA_INDEX(LEVEL_TTM, 1)

#define CAM_MODE_MARIO_ACTIVE           0x01
#define CAM_MODE_LAKITU_WAS_ZOOMED_OUT  0x02
#define CAM_MODE_MARIO_SELECTED         0x04

enum CameraSelection {
    CAM_SELECTION_NONE,
    CAM_SELECTION_MARIO,
    CAM_SELECTION_FIXED,
};

enum CameraAngle {
    CAM_ANGLE_NONE,
    CAM_ANGLE_MARIO,
    CAM_ANGLE_LAKITU,
};

enum CameraModes {
    /* 0*/ CAMERA_MODE_NONE,
    /* 1*/ CAMERA_MODE_RADIAL,
    /* 2*/ CAMERA_MODE_OUTWARD_RADIAL,
    /* 3*/ CAMERA_MODE_BEHIND_MARIO,
    /* 4*/ CAMERA_MODE_CLOSE, // Inside Castle / Big Boo's Haunt
    /* 5*/ CAMERA_MODE_5,
    /* 6*/ CAMERA_MODE_C_UP,
    /* 7*/ CAMERA_MODE_7,
    /* 8*/ CAMERA_MODE_WATER_SURFACE,
    /* 9*/ CAMERA_MODE_SLIDE_HOOT,
    /*10*/ CAMERA_MODE_INSIDE_CANNON,
    /*11*/ CAMERA_MODE_BOSS_FIGHT,
    /*12*/ CAMERA_MODE_PARALLEL_TRACKING,
    /*13*/ CAMERA_MODE_FIXED,
    /*14*/ CAMERA_MODE_8_DIRECTIONS, // AKA Parallel Camera, Bowser Courses & Rainbow Ride
    /*15*/ CAMERA_MODE_0F,
    /*16*/ CAMERA_MODE_FREE_ROAM,
    /*17*/ CAMERA_MODE_SPIRAL_STAIRS
};

enum CameraMovementFlags {
    CAM_MOVE_RETURN_TO_MIDDLE       = (1 <<  0), // 0x0001
    CAM_MOVE_ZOOMED_OUT             = (1 <<  1), // 0x0002
    CAM_MOVE_ROTATE_RIGHT           = (1 <<  2), // 0x0004
    CAM_MOVE_ROTATE_LEFT            = (1 <<  3), // 0x0008
    CAM_MOVE_ENTERED_ROTATE_SURFACE = (1 <<  4), // 0x0010
    CAM_MOVE_METAL_BELOW_WATER      = (1 <<  5), // 0x0020
    CAM_MOVE_FIX_IN_PLACE           = (1 <<  6), // 0x0040
    CAM_MOVE_UNKNOWN_8              = (1 <<  7), // 0x0080
    CAM_MOVING_INTO_MODE            = (1 <<  8), // 0x0100
    CAM_MOVE_STARTED_EXITING_C_UP   = (1 <<  9), // 0x0200
    CAM_MOVE_UNKNOWN_11             = (1 << 10), // 0x0400
    CAM_MOVE_INIT_CAMERA            = (1 << 11), // 0x0800
    CAM_MOVE_ALREADY_ZOOMED_OUT     = (1 << 12), // 0x1000
    CAM_MOVE_C_UP_MODE              = (1 << 13), // 0x2000
    CAM_MOVE_SUBMERGED              = (1 << 14), // 0x4000
    CAM_MOVE_PAUSE_SCREEN           = (1 << 15), // 0x8000

    CAM_MOVE_ROTATE   = (CAM_MOVE_ROTATE_RIGHT | CAM_MOVE_ROTATE_LEFT | CAM_MOVE_RETURN_TO_MIDDLE),
    /// These flags force the camera to move a certain way
    CAM_MOVE_RESTRICT = (CAM_MOVE_ENTERED_ROTATE_SURFACE | CAM_MOVE_METAL_BELOW_WATER | CAM_MOVE_FIX_IN_PLACE | CAM_MOVE_UNKNOWN_8),
};

enum CameraSounds {
    CAM_SOUND_C_UP_PLAYED           = (1 <<  0), // 0x01
    CAM_SOUND_MARIO_ACTIVE          = (1 <<  1), // 0x02
    CAM_SOUND_NORMAL_ACTIVE         = (1 <<  2), // 0x04
    CAM_SOUND_UNUSED_SELECT_MARIO   = (1 <<  3), // 0x08
    CAM_SOUND_UNUSED_SELECT_FIXED   = (1 <<  4), // 0x10
    CAM_SOUND_FIXED_ACTIVE          = (1 <<  5), // 0x20
};

enum CameraFlags {
    CAM_FLAG_SMOOTH_MOVEMENT        = (1 <<  0), // 0x0001
    CAM_FLAG_BLOCK_SMOOTH_MOVEMENT  = (1 <<  1), // 0x0002
    CAM_FLAG_FRAME_AFTER_CAM_INIT   = (1 <<  2), // 0x0004
    CAM_FLAG_CHANGED_PARTRACK_INDEX = (1 <<  3), // 0x0008
    CAM_FLAG_CCM_SLIDE_SHORTCUT     = (1 <<  4), // 0x0010
    CAM_FLAG_CAM_NEAR_WALL          = (1 <<  5), // 0x0020
    CAM_FLAG_SLEEPING               = (1 <<  6), // 0x0040
    CAM_FLAG_UNUSED_7               = (1 <<  7), // 0x0080
    CAM_FLAG_UNUSED_8               = (1 <<  8), // 0x0100
    CAM_FLAG_COLLIDED_WITH_WALL     = (1 <<  9), // 0x0200
    CAM_FLAG_START_TRANSITION       = (1 << 10), // 0x0400
    CAM_FLAG_TRANSITION_OUT_OF_C_UP = (1 << 11), // 0x0800
    CAM_FLAG_BLOCK_AREA_PROCESSING  = (1 << 12), // 0x1000
    CAM_FLAG_UNUSED_13              = (1 << 13), // 0x2000
    CAM_FLAG_UNUSED_CUTSCENE_ACTIVE = (1 << 14), // 0x4000
    CAM_FLAG_BEHIND_MARIO_POST_DOOR = (1 << 15), // 0x8000
};

enum CameraStatus {
    CAM_STATUS_NONE   = (0 << 0), // 0x00
    CAM_STATUS_MARIO  = (1 << 0), // 0x01
    CAM_STATUS_LAKITU = (1 << 1), // 0x02
    CAM_STATUS_FIXED  = (1 << 2), // 0x04
    CAM_STATUS_C_DOWN = (1 << 3), // 0x08
    CAM_STATUS_C_UP   = (1 << 4), // 0x10

    CAM_STATUS_MODE_GROUP   = (CAM_STATUS_MARIO | CAM_STATUS_LAKITU | CAM_STATUS_FIXED),
    CAM_STATUS_C_MODE_GROUP = (CAM_STATUS_C_DOWN | CAM_STATUS_C_UP),
};

enum CameraShake {
    SHAKE_NONE,
    SHAKE_ATTACK,
    SHAKE_GROUND_POUND,
    SHAKE_SMALL_DAMAGE,
    SHAKE_MED_DAMAGE,
    SHAKE_LARGE_DAMAGE,
    SHAKE_UNUSED_6,
    SHAKE_UNUSED_7,
    SHAKE_HIT_FROM_BELOW,
    SHAKE_FALL_DAMAGE,
    SHAKE_SHOCK
};

enum CameraShakeEnv {
    SHAKE_ENV_NONE,
    SHAKE_ENV_EXPLOSION,
    SHAKE_ENV_BOWSER_THROW_BOUNCE,
    SHAKE_ENV_BOWSER_JUMP,
    SHAKE_ENV_UNUSED_4,
    SHAKE_ENV_UNUSED_5,
    SHAKE_ENV_UNUSED_6,
    SHAKE_ENV_UNUSED_7,
    SHAKE_ENV_PYRAMID_EXPLODE,
    SHAKE_ENV_JRB_SHIP_DRAIN,
    SHAKE_ENV_FALLING_BITS_PLAT
};

enum CameraShakeFov {
    SHAKE_FOV_NONE,
    SHAKE_FOV_SMALL,
    SHAKE_FOV_UNUSED,
    SHAKE_FOV_MEDIUM,
    SHAKE_FOV_LARGE
};

enum CameraShakePos {
    SHAKE_POS_NONE,
    SHAKE_POS_SMALL,
    SHAKE_POS_MEDIUM,
    SHAKE_POS_LARGE,
    SHAKE_POS_BOWLING_BALL
};

enum Cutscenes {
    CUTSCENE_NONE,
    CUTSCENE_DOOR_PULL = 130,
    CUTSCENE_DOOR_PUSH,
    CUTSCENE_UNUSED_132,
    CUTSCENE_ENTER_CANNON,
    CUTSCENE_ENTER_PAINTING,
    CUTSCENE_DEATH_EXIT,
    CUTSCENE_UNUSED_136,
    CUTSCENE_UNUSED_137,
    CUTSCENE_UNUSED_138,
    CUTSCENE_DOOR_WARP,
    CUTSCENE_DOOR_PULL_MODE,
    CUTSCENE_DOOR_PUSH_MODE,
    CUTSCENE_INTRO_PEACH,
    CUTSCENE_DANCE_ROTATE,
    CUTSCENE_ENTER_BOWSER_ARENA,
    CUTSCENE_0F_UNUSED, // Never activated, stub cutscene functions
    CUTSCENE_UNUSED_146,
    CUTSCENE_UNUSED_EXIT, // Never activated
    CUTSCENE_UNUSED_148,
    CUTSCENE_SLIDING_DOORS_OPEN,
    CUTSCENE_PREPARE_CANNON,
    CUTSCENE_UNLOCK_KEY_DOOR,
    CUTSCENE_STANDING_DEATH,
    CUTSCENE_DEATH_ON_STOMACH,
    CUTSCENE_DEATH_ON_BACK,
    CUTSCENE_QUICKSAND_DEATH,
    CUTSCENE_SUFFOCATION_DEATH,
    CUTSCENE_EXIT_BOWSER_SUCC,
    CUTSCENE_EXIT_BOWSER_DEATH, // Never activated
    CUTSCENE_WATER_DEATH, // Not in cutscene switch
    CUTSCENE_EXIT_PAINTING_SUCC,
    CUTSCENE_CAP_SWITCH_PRESS,
    CUTSCENE_DIALOG,
    CUTSCENE_RACE_DIALOG,
    CUTSCENE_ENTER_PYRAMID_TOP,
    CUTSCENE_DANCE_FLY_AWAY,
    CUTSCENE_DANCE_CLOSEUP,
    CUTSCENE_KEY_DANCE,
    CUTSCENE_SSL_PYRAMID_EXPLODE, // Never activated
    CUTSCENE_EXIT_SPECIAL_SUCC,
    CUTSCENE_NONPAINTING_DEATH,
    CUTSCENE_READ_MESSAGE,
    CUTSCENE_ENDING,
    CUTSCENE_STAR_SPAWN,
    CUTSCENE_GRAND_STAR,
    CUTSCENE_DANCE_DEFAULT,
    CUTSCENE_RED_COIN_STAR_SPAWN,
    CUTSCENE_END_WAVING,
    CUTSCENE_CREDITS,
    CUTSCENE_EXIT_WATERFALL,
    CUTSCENE_EXIT_FALL_WMOTR,
    CUTSCENE_ENTER_POOL
};

/**
 * Stop the cutscene.
 */
#define CUTSCENE_STOP         0x8000
/**
 * Play the current cutscene shot indefinitely (until canceled).
 */
#define CUTSCENE_LOOP         0x7FFF

enum CameraHandCamShake {
    HAND_CAM_SHAKE_OFF,
    HAND_CAM_SHAKE_CUTSCENE,
    HAND_CAM_SHAKE_UNUSED,
    HAND_CAM_SHAKE_HANG_OWL,
    HAND_CAM_SHAKE_HIGH,
    HAND_CAM_SHAKE_STAR_DANCE,
    HAND_CAM_SHAKE_LOW
};

enum CameraDoor {
    DOOR_DEFAULT,
    DOOR_LEAVING_SPECIAL,
    DOOR_ENTER_LOBBY
};

// Might rename these to reflect what they are used for instead "SET_45" etc.
enum CameraFov {
    CAM_FOV_NONE,
    CAM_FOV_SET_45,
    CAM_FOV_DEFAULT,
    CAM_FOV_UNUSED_3,
    CAM_FOV_APP_45,
    CAM_FOV_SET_30,
    CAM_FOV_APP_20,
    CAM_FOV_BBH,
    CAM_FOV_UNUSED_8,
    CAM_FOV_APP_80,
    CAM_FOV_APP_30,
    CAM_FOV_APP_60,
    CAM_FOV_ZOOM_30,
    CAM_FOV_SET_29
};

enum CameraEvent {
    CAM_EVENT_NONE,
    CAM_EVENT_CANNON,
    CAM_EVENT_SHOT_FROM_CANNON,
    CAM_EVENT_UNUSED_3,
    CAM_EVENT_BOWSER_INIT,
    CAM_EVENT_DOOR_WARP,
    CAM_EVENT_DOOR,
    CAM_EVENT_BOWSER_JUMP,
    CAM_EVENT_BOWSER_THROW_BOUNCE,
    CAM_EVENT_START_INTRO,
    CAM_EVENT_START_GRAND_STAR,
    CAM_EVENT_START_ENDING,
    CAM_EVENT_START_END_WAVING,
    CAM_EVENT_START_CREDITS
};

enum AvoidStatus {
    AVOID_STATUS_NONE,
    AVOID_STATUS_WALL_NEAR_CAMERA,
    AVOID_STATUS_2,
    AVOID_STATUS_WALL_COVERING_MARIO,
};

/**
 * A copy of player information that is relevant to the camera.
 */
struct PlayerCameraState {
    /**
     * Mario's action on this frame.
     */
    /*0x00*/ u32 action;
    /*0x04*/ Vec3f pos;
    /*0x10*/ Vec3s faceAngle;
    /*0x16*/ Vec3s headRotation;
    /*0x1C*/ s16 unused;
    /**
     * Set to nonzero when an event, such as entering a door, starting the credits, or throwing bowser,
     * has happened on this frame.
     */
    /*0x1E*/ s16 cameraEvent;
    /*0x20*/ struct Object *usedObj;
};

/**
 * Struct containing info that is used when transition_next_state() is called. Stores the intermediate
 * distances and angular displacements from lakitu's goal position and focus.
 */
struct TransitionInfo {
    /*0x00*/ s16 posPitch;
    /*0x02*/ s16 posYaw;
    /*0x04*/ f32 posDist;
    /*0x08*/ s16 focPitch;
    /*0x0A*/ s16 focYaw;
    /*0x0C*/ f32 focDist;
    /*0x10*/ s32 framesLeft;
    /*0x14*/ Vec3f marioPos;
    /*0x20*/ u8 unused; // for the structs to align, there has to be an extra unused variable here. type is unknown.
};

/**
 * A point that's used in a spline, controls the direction to move the camera in
 * during the shake effect.
 */
struct HandheldShakePoint {
    /*0x00*/ s8 index; // only set to -1
    /*0x04 (aligned)*/ u32 unused;
    /*0x08*/ Vec3s point;
}; // size = 0x10

// These are the same type, but the name that is used depends on context.
/**
 * A function that is called by CameraTriggers and cutscene shots.
 * These are concurrent: multiple CameraEvents can occur on the same frame.
 */
typedef void (*CameraEvent)(struct Camera *c);
/**
 * The same type as a CameraEvent, but because these are generally longer, and happen in sequential
 * order, they're are called "shots," a term taken from cinematography.
 *
 * To further tell the difference: CutsceneShots usually call multiple CameraEvents at once, but only
 * one CutsceneShot is ever called on a given frame.
 */
typedef CameraEvent CutsceneShot;

/**
 * Defines a bounding box which activates an event while Mario is inside
 */
struct CameraTrigger {
    /**
     * The area this should be checked in, or -1 if it should run in every area of the level.
     *
     * Triggers with area set to -1 are run by default, they don't care if Mario is inside their bounds.
     * However, they are only active if Mario is not already inside an area-specific trigger's
     * boundaries.
     */
    s8 area;
    /// A function that gets called while Mario is in the trigger bounds
    CameraEvent event;
    // The (x,y,z) position of the center of the bounding box
    s16 centerX;
    s16 centerY;
    s16 centerZ;
    // The max displacement in x, y, and z from the center for a point to be considered inside the
    // bounding box
    s16 boundsX;
    s16 boundsY;
    s16 boundsZ;
    /// This angle rotates Mario's offset from the box's origin, before it is checked for being inside.
    s16 boundsYaw;
};

/**
 * A camera shot that is active for a number of frames.
 * Together, a sequence of shots makes up a cutscene.
 */
struct Cutscene {
    /// The function that gets called.
    CutsceneShot shot;
    /// How long the shot lasts.
    s16 duration;
};

/**
 * Info for the camera's field of view and the FOV shake effect.
 */
struct CameraFOVStatus {
    /// The current function being used to set the camera's field of view (before any fov shake is applied).
    /*0x00*/ u8 fovFunc;
    /// The current field of view in degrees
    /*0x04*/ f32 fov;

    // Fields used by shake_camera_fov()

    /// The amount to change the current fov by in the fov shake effect.
    /*0x08*/ f32 fovOffset;
    /// A bool set in fov_default() but unused otherwise
    /*0x0C*/ u32 unusedIsSleeping;
    /// The range in degrees to shake fov
    /*0x10*/ f32 shakeAmplitude;
    /// Used to calculate fovOffset, the phase through the shake's period.
    /*0x14*/ s16 shakePhase;
    /// How much to progress through the shake period
    /*0x16*/ s16 shakeSpeed;
    /// How much to decrease shakeAmplitude each frame.
    /*0x18*/ s16 decay;
};

/**
 * Information for a control point in a spline segment.
 */
struct CutsceneSplinePoint {
    /* The index of this point in the spline. Ignored except for -1, which ends the spline.
       An index of -1 should come four points after the start of the last segment. */
    s8 index;
    /* Roughly controls the number of frames it takes to progress through the spline segment.
       See move_point_along_spline() in camera.c */
    u8 speed;
    Vec3s point;
};

/**
 * Struct containing the nearest floor and ceiling to the player, as well as the previous floor and
 * ceiling. It also stores their distances from the player's position.
 */
struct PlayerGeometry {
    /*0x00*/ struct Surface *currFloor;
    /*0x04*/ f32 currFloorHeight;
    /*0x08*/ s16 currFloorType;
    /*0x0C*/ struct Surface *currCeil;
    /*0x10*/ s16 currCeilType;
    /*0x14*/ f32 currCeilHeight;
    /*0x18*/ struct Surface *prevFloor;
    /*0x1C*/ f32 prevFloorHeight;
    /*0x20*/ s16 prevFloorType;
    /*0x24*/ struct Surface *prevCeil;
    /*0x28*/ f32 prevCeilHeight;
    /*0x2C*/ s16 prevCeilType;
    /*0x30*/ f32 waterHeight;
};

/**
 * Point used in transitioning between camera modes and C-Up.
 */
struct LinearTransitionPoint {
    Vec3f focus;
    Vec3f pos;
    f32 dist;
    s16 pitch;
    s16 yaw;
};

/**
 * Info about transitioning between camera modes.
 */
struct ModeTransitionInfo {
    s16 newMode;
    s16 lastMode;
    s16 max;
    s16 frame;
    struct LinearTransitionPoint transitionStart;
    struct LinearTransitionPoint transitionEnd;
};

/**
 * A point in a path used by update_parallel_tracking_camera
 */
struct ParallelTrackingPoint {
    /// Whether this point is the start of a path
    s16 startOfPath;
    /// Point used to define a line segment to follow
    Vec3f pos;
    /// The distance Mario can move along the line before the camera should move
    f32 distThresh;
    /// The percentage that the camera should move from the line to Mario
    f32 zoom;
};

/**
 * Stores the camera's info
 */
struct CameraStoredInfo {
    /*0x00*/ Vec3f pos;
    /*0x0C*/ Vec3f focus;
    /*0x18*/ f32 panDist;
    /*0x1C*/ f32 cannonYOffset;
};

/**
 * Struct used to store cutscene info, like the camera's target position/focus.
 *
 * See the sCutsceneVars[] array in camera.c for more details.
 */
struct CutsceneVariable {
    /// Perhaps an index
    s32 unused1;
    Vec3f point;
    Vec3f unusedPoint;
    Vec3s angle;
    /// Perhaps a boolean or an extra angle
    s16 unused2;
};

/**
 * The main camera struct. Gets updated by the active camera mode and the current level/area. In
 * update_lakitu, its pos and focus are used to calculate lakitu's next position and focus, which are
 * then used to render the game.
 */
struct Camera {
    /*0x00*/ u8 mode; // What type of mode the camera uses (see defines above)
    /*0x01*/ u8 defMode;
    /**
     * Determines what direction Mario moves in when the analog stick is moved.
     *
     * @warning This is NOT the camera's xz-rotation in world space. This is the angle calculated from the
     *          camera's focus TO the camera's position, instead of the other way around like it should
     *          be. It's effectively the opposite of the camera's actual yaw. Use
     *          vec3f_get_dist_and_angle() if you need the camera's yaw.
     */
    /*0x02*/ s16 yaw;
    /*0x04*/ Vec3f focus;
    /*0x10*/ Vec3f pos;
    /*0x1C*/ Vec3f unusedVec1;
    /// The x coordinate of the "center" of the area. The camera will rotate around this point.
    /// For example, this is what makes the camera rotate around the hill in BoB
    /*0x28*/ f32 areaCenX;
    /// The z coordinate of the "center" of the area. The camera will rotate around this point.
    /// For example, this is what makes the camera rotate around the hill in BoB
    /*0x2C*/ f32 areaCenZ;
    /*0x30*/ u8 cutscene;
    /*0x31*/ u8 filler1[8];
    /*0x3A*/ s16 nextYaw;
    /*0x3C*/ u8 filler2[40];
    /*0x64*/ u8 doorStatus;
    /// The y coordinate of the "center" of the area. Unlike areaCenX and areaCenZ, this is only used
    /// when paused. See zoom_out_if_paused_and_outside
    /*0x68*/ f32 areaCenY;
};

/**
 * A struct containing info pertaining to lakitu, such as his position and focus, and what
 * camera-related effects are happening to him, like camera shakes.
 *
 * This struct's pos and focus are what is actually used to render the game.
 *
 * @see update_lakitu()
 */
struct LakituState {
    /**
     * Lakitu's position, which (when CAM_FLAG_SMOOTH_MOVEMENT is set), approaches his goalPos every frame.
     */
    /*0x00*/ Vec3f curFocus;
    /**
     * Lakitu's focus, which (when CAM_FLAG_SMOOTH_MOVEMENT is set), approaches his goalFocus every frame.
     */
    /*0x0C*/ Vec3f curPos;
    /**
     * The focus point that lakitu turns towards every frame.
     * If CAM_FLAG_SMOOTH_MOVEMENT is unset, this is the same as curFocus.
     */
    /*0x18*/ Vec3f goalFocus;
    /**
     * The point that lakitu flies towards every frame.
     * If CAM_FLAG_SMOOTH_MOVEMENT is unset, this is the same as curPos.
     */
    /*0x24*/ Vec3f goalPos;

    /*0x30*/ u8 filler1[12]; // extra unused Vec3f?

    /// Copy of the active camera mode
    /*0x3C*/ u8 mode;
    /// Copy of the default camera mode
    /*0x3D*/ u8 defMode;

    /*0x3E*/ u8 filler2[10];

    /*0x48*/ f32 focusDistance; // unused
    /*0x4C*/ s16 oldPitch; // unused
    /*0x4E*/ s16 oldYaw;   // unused
    /*0x50*/ s16 oldRoll;  // unused

    /// The angular offsets added to lakitu's pitch, yaw, and roll
    /*0x52*/ Vec3s shakeMagnitude;

    // shake pitch, yaw, and roll phase: The progression through the camera shake (a cosine wave).
    // shake pitch, yaw, and roll vel: The speed of the camera shake.
    // shake pitch, yaw, and roll decay: The shake's deceleration.
    /*0x58*/ s16 shakePitchPhase;
    /*0x5A*/ s16 shakePitchVel;
    /*0x5C*/ s16 shakePitchDecay;

    /*0x60*/ Vec3f unusedVec1;
    /*0x6C*/ Vec3s unusedVec2;
    /*0x72*/ u8 filler3[8];

    /// Used to rotate the screen when rendering.
    /*0x7A*/ s16 roll;
    /// Copy of the camera's yaw.
    /*0x7C*/ s16 yaw;
    /// Copy of the camera's next yaw.
    /*0x7E*/ s16 nextYaw;
    /// The actual focus point the game uses to render.
    /*0x80*/ Vec3f focus;
    /// The actual position the game is rendered from.
    /*0x8C*/ Vec3f pos;

    // Shake variables: See above description
    /*0x98*/ s16 shakeRollPhase;
    /*0x9A*/ s16 shakeRollVel;
    /*0x9C*/ s16 shakeRollDecay;
    /*0x9E*/ s16 shakeYawPhase;
    /*0xA0*/ s16 shakeYawVel;
    /*0xA2*/ s16 shakeYawDecay;

    // focH,Vspeed: how fast lakitu turns towards his goalFocus.
    /// By default HSpeed is 0.8, so lakitu turns 80% of the horz distance to his goal each frame.
    /*0xA4*/ f32 focHSpeed;
    /// By default VSpeed is 0.3, so lakitu turns 30% of the vert distance to his goal each frame.
    /*0xA8*/ f32 focVSpeed;

    // posH,Vspeed: How fast lakitu flies towards his goalPos.
    /// By default they are 0.3, so lakitu will fly 30% of the way towards his goal each frame.
    /*0xAC*/ f32 posHSpeed;
    /*0xB0*/ f32 posVSpeed;

    /// The roll offset applied during part of the key dance cutscene
    /*0xB4*/ s16 keyDanceRoll;
    /// Mario's action from the previous frame. Only used to determine if Mario just finished a dive.
    /*0xB8*/ u32 lastFrameAction;
    /*0xBC*/ s16 unused;
};

// BSS
extern s16 sSelectionFlags;
extern s16 sCameraSoundFlags;
extern u16 sCButtonsPressed;
extern struct PlayerCameraState gPlayerCameraState[2];
extern struct LakituState gLakituState;
extern s16 gCameraMovementFlags;
extern s32 gObjCutsceneDone;
extern struct Camera *gCamera;
extern struct Object *gCutsceneFocus;
extern struct Object *gSecondCameraFocus;
extern u8 gRecentCutscene;

// TODO: sort all of this extremely messy shit out after the split

void set_camera_shake_from_hit(s16 shake);
void set_environmental_camera_shake(s16 shake);
void set_camera_shake_from_point(s16 shake, f32 posX, f32 posY, f32 posZ);
void move_mario_head_c_up(UNUSED struct Camera *c);
void transition_next_state(UNUSED struct Camera *c, s16 frames);
void set_camera_mode(struct Camera *c, s16 mode, s16 frames);
void update_camera(struct Camera *c);
void reset_camera(struct Camera *c);
void init_camera(struct Camera *c);
void select_mario_cam_mode(void);
Gfx *geo_camera_main(s32 callContext, struct GraphNode *g, void *context);
void object_pos_to_vec3f(Vec3f dst, struct Object *obj);
void vec3f_to_object_pos(struct Object *obj, Vec3f src);
s32 move_point_along_spline(Vec3f p, struct CutsceneSplinePoint spline[], s16 *splineSegment, f32 *progress);
s32 cam_select_alt_mode(s32 angle);
s32 set_cam_angle(s32 mode);
void set_handheld_shake(u8 mode);
void shake_camera_handheld(Vec3f pos, Vec3f focus);
s32 find_c_buttons_pressed(u16 currentState, u16 buttonsPressed, u16 buttonsDown);
s32 update_camera_hud_status(struct Camera *c);
s32 collide_with_walls(Vec3f pos, f32 offsetY, f32 radius);
void clamp_pitch(Vec3f from, Vec3f to, s16 maxPitch, s16 minPitch);
s32 is_within_100_units_of_mario(f32 posX, f32 posY, f32 posZ);
s32 set_or_approach_f32_asymptotic(f32 *dst, f32 goal, f32 scale);
void approach_vec3f_asymptotic(Vec3f current, Vec3f target, f32 xMul, f32 yMul, f32 zMul);
void set_or_approach_vec3f_asymptotic(Vec3f dst, Vec3f goal, f32 xMul, f32 yMul, f32 zMul);
s32 camera_approach_s16_symmetric_bool(s16 *current, s16 target, s16 increment);
s32 set_or_approach_s16_symmetric(s16 *current, s16 target, s16 increment);
s32 camera_approach_f32_symmetric_bool(f32 *current, f32 target, f32 increment);
f32 camera_approach_f32_symmetric(f32 value, f32 target, f32 increment);
void random_vec3s(Vec3s dst, s16 xRange, s16 yRange, s16 zRange);
s32 clamp_positions_and_find_yaw(Vec3f pos, Vec3f origin, f32 xMax, f32 xMin, f32 zMax, f32 zMin);
s32 is_range_behind_surface(Vec3f from, Vec3f to, struct Surface *surf, s16 range, s16 surfType);
void scale_along_line(Vec3f dest, Vec3f from, Vec3f to, f32 scale);
s32 calculate_pitch(Vec3f from, Vec3f to);
s32 calculate_yaw(Vec3f from, Vec3f to);
void calculate_angles(Vec3f from, Vec3f to, s16 *pitch, s16 *yaw);
f32 calc_abs_dist(Vec3f a, Vec3f b);
f32 calc_abs_dist_squared(Vec3f a, Vec3f b);
f32 calc_hor_dist(Vec3f a, Vec3f b);
void rotate_in_xz(Vec3f dst, Vec3f src, s16 yaw);
void rotate_in_yz(Vec3f dst, Vec3f src, s16 pitch);
void set_camera_pitch_shake(s16 mag, s16 decay, s16 inc);
void set_camera_yaw_shake(s16 mag, s16 decay, s16 inc);
void set_camera_roll_shake(s16 mag, s16 decay, s16 inc);
void set_pitch_shake_from_point(s16 mag, s16 decay, s16 inc, f32 maxDist, f32 posX, f32 posY, f32 posZ);
void shake_camera_pitch(Vec3f pos, Vec3f focus);
void shake_camera_yaw(Vec3f pos, Vec3f focus);
void shake_camera_roll(s16 *roll);
s32 offset_yaw_outward_radial(struct Camera *c, s16 areaYaw);
void play_camera_buzz_if_cdown(void);
void play_camera_buzz_if_cbutton(void);
void play_camera_buzz_if_c_sideways(void);
void play_sound_cbutton_up(void);
void play_sound_cbutton_down(void);
void play_sound_cbutton_side(void);
void play_sound_button_change_blocked(void);
void play_sound_rbutton_changed(void);
void play_sound_if_cam_switched_to_lakitu_or_mario(void);
void radial_camera_input(struct Camera *c);
void trigger_cutscene_dialog(s32 trigger);
void handle_c_button_movement(struct Camera *c);
void start_cutscene(struct Camera *c, u8 cutscene);
u8 get_cutscene_from_mario_status(struct Camera *c);
void warp_camera(f32 displacementX, f32 displacementY, f32 displacementZ);
void approach_camera_height(struct Camera *c, f32 goal, f32 inc);
void offset_rotated(Vec3f dst, Vec3f from, Vec3f to, Vec3s rotation);
s16 next_lakitu_state(Vec3f newPos, Vec3f newFoc, Vec3f curPos, Vec3f curFoc, Vec3f oldPos, Vec3f oldFoc, s16 yaw);
void set_fixed_cam_axis_sa_lobby(UNUSED s16 preset);
s16 camera_course_processing(struct Camera *c);
void resolve_geometry_collisions(Vec3f pos);
s32 rotate_camera_around_walls(struct Camera *c, Vec3f cPos, s16 *avoidYaw, s16 yawRange);
void find_mario_floor_and_ceil(struct PlayerGeometry *pg);
void start_object_cutscene_without_focus(u8 cutscene);
s16 cutscene_object_with_dialog(u8 cutscene, struct Object *obj, s16 dialogID);
s16 cutscene_object_without_dialog(u8 cutscene, struct Object *obj);
s16 cutscene_object(u8 cutscene, struct Object *obj);
void play_cutscene(struct Camera *c);
void cutscene_event(CameraEvent event, struct Camera * c, s16 start, s16 end);
void cutscene_spawn_obj(u32 obj, s16 frame);
void set_fov_shake(s16 amplitude, s16 decay, s16 shakeSpeed);

void set_fov_function(u8 func);
void cutscene_set_fov_shake_preset(u8 preset);
void set_fov_shake_from_point_preset(u8 preset, f32 posX, f32 posY, f32 posZ);
void obj_rotate_towards_point(struct Object *obj, Vec3f point, s16 pitchOff, s16 yawOff, s16 pitchDiv, s16 yawDiv);

Gfx *geo_camera_fov(s32 callContext, struct GraphNode *g, UNUSED void *context);

#endif // CAMERA_H
