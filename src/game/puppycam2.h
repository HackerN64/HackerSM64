#ifndef PUPPYCAM2_H
#define PUPPYCAM2_H

#ifdef PUPPYCAM

#define PUPPYCAM_FLAGS_CUTSCENE    0x0001
#define PUPPYCAM_FLAGS_SMOOTH      0x0002

#define PUPPY_ERROR_POOL_FULL 0x1

#define PUPPY_NULL 15151
#define MAX_PUPPYCAM_VOLUMES 128

#define PUPPYCAM_BEHAVIOUR_TEMPORARY 0x0
#define PUPPYCAM_BEHAVIOUR_PERMANENT 0x1

#define PUPPYVOLUME_SHAPE_BOX 0x0
#define PUPPYVOLUME_SHAPE_CYLINDER 0x1

#define PUPPYCAM_MODE3_ZOOMED_IN 0x1
#define PUPPYCAM_MODE3_ZOOMED_MED 0x2
#define PUPPYCAM_MODE3_ZOOMED_OUT 0x4
#define PUPPYCAM_MODE3_ENTER_FIRST_PERSON 0x8

#include "include/command_macros_base.h"

#define PUPPYVOLUME(x, y, z, length, height, width, yaw, functionptr, anglesptr, addflags, removeflags, flagpersistance, room, shape) \
    CMD_BBH(0x3D, 0x24, x), \
    CMD_HHHHHH(y, z, length, height, width, yaw), \
    CMD_PTR(functionptr), \
    CMD_PTR(anglesptr), \
    CMD_W(addflags), \
    CMD_W(removeflags), \
    CMD_BBH(flagpersistance, shape, room)

//Some macros for the sake of basic human sanity.
#define CLAMP(x, low, high)  (((x) > (high)) ? (high) : (((x) < (low)) ? (low) : (x)))
#define ABS(x) ((x) > 0.f ? (x) : -(x))

struct gPuppyOptions
{
    s16 analogue;
    s16 sensitivityX;
    s16 sensitivityY;
    s16 invertX;
    s16 invertY;
    s16 turnAggression;
    s16 inputType;
};

struct gPuppyStruct
{
    s16 yaw; //Horizontal Direction the game reads as the active value.
    s16 yawTarget; //Horizontal Direction that yaw tries to be.
    f32 yawAcceleration; //Horizontal Direction that sets yawTarget.
    s16 pitch; //Vertical Direction the game reads as the active value.
    s16 pitchTarget; //Vertical Direction that pitch tries to be.
    f32 pitchAcceleration; //Vertical Direction that sets pitchTarget.
    s16 posHeight[2]; //The first index is the ground offset of pos[1], the second index is the ground offset of focus[1].
    s16 zoom; //How far the camera is currently zoomed out
    u8 zoomSet; //The current setting of which zoompoint to set the target to.
    s16 zoomTarget; //The value that zoom tries to be.
    s16 zoomPoints[3]; //An array containing distances.
    s16 targetFloorHeight; //Mario's current floor height
    s16 lastTargetFloorHeight; //Mirror's mario's floor height when his velocity is not above 0.
    Vec3s pos; //Where the camera is
    Vec3s focus; //Where the camera's looking
    Vec3s pan; //An offset of the camera's focus
    s32 intendedFlags; //The flagset the camera tries to be when it's not held hostage.
    s32 flags; //Behaviour flags that affect different properties of the camera's behaviour
    Vec3s shake; //How much the camera's shaking
    u8 shakeFrames; //How long the camera's shaking for
    f32 shakeForce; //How violently the camera's shaking
    s32 framesSinceC[2]; //Counts the number of frames since the last C left or right press, to track double presses.
    s16 collisionDistance; //Tries to be zoom, but will be overwritten by collision detection
    struct Object *targetObj; //The object that the focus will base its positioning off. Usually Mario.
    struct Object *targetObj2; //This is the second focus point that the camera will focus on. It'll focus between them.
    s16 povHeight; //An offset of the focus object's Y value.
    s16 floorY[2]; //Floor offsets, to allow a grace period before following Mario into the air.
    u8 opacity; //A value set by collision distance, to fade Mario out if you're too close.
    s8 stick2[2];//The value that's set and read for analogue stick.
    u8 stickN[2]; //This is set when the stick is neutral. It's to prevent rapidfire input.
    u8 enabled; //A boolean that decides whether to use vanilla camera or puppy camera. Of course, anybody with this enabled is obligated to a death sentence :)
    s16 swimPitch; //Pitch adjustment that's applied when swimming. All pitch adjustment is clamped.
    s16 edgePitch; //Pitch adjustment that's applied when stood near an edge. All pitch adjustment is clamped.
    s16 moveZoom; //A small zoom value that's added on top of the regular zoom when moving. It's pretty subtle, but gives the feeling of a bit of speed.
    u8 mode3Flags; //A flagset for classic mode.
    u8 moveFlagAdd; //A bit that multiplies movement rate of axes when moving, to centre them faster.
    s16 targetDist[2];

    u8 cutscene; //A boolean that decides whether a cutscene is active
    s32 (*sceneFunc)();
    u8 sceneInput; //A boolean that decides whether the controller updates during the scene.
    s32 sceneTimer; //The cutscene timer that goes up during a cutscene.
    Vec3s scenePos; //Where the camera is during a cutscene
    Vec3s sceneFocus; //Where the camera looks during a cutscene

    struct gPuppyOptions options;

};

//A second container for bounds that have 2 pairs of coordinates. Optional.
struct sPuppyAngles
{
    Vec3s pos;
    Vec3s focus;
    s16 yaw;
    s16 pitch;
    s16 zoom;
};

//A bounding volume for activating puppycamera scripts and angles.
struct sPuppyVolume
{
    Vec3s pos; //The set position of the volume
    Vec3s radius; //Where it extends.
    s16 rot; //The rotational angle of the volume.
    s32 *func; //a pointer to a function. Optional.
    struct sPuppyAngles *angles; //A pointer to a gPuppyAngles struct. Optional
    s32 flagsAdd; //Adds behaviour flags.
    s32 flagsRemove; //Removes behaviour flags.
    u8 flagPersistance; //Decides if adding or removing the flags is temporary or permanent.
    u8 shape;
    s16 room;
};

enum gPuppyCamBeh
{
    PUPPYCAM_BEHAVIOUR_X_MOVEMENT = 0x0001,
    PUPPYCAM_BEHAVIOUR_Y_MOVEMENT = 0x0002,
    PUPPYCAM_BEHAVIOUR_Z_MOVEMENT = 0x0004,

    PUPPYCAM_BEHAVIOUR_YAW_ROTATION = 0x0008,
    PUPPYCAM_BEHAVIOUR_PITCH_ROTATION = 0x0010,
    PUPPYCAM_BEHAVIOUR_ZOOM_CHANGE = 0x0020,

    PUPPYCAM_BEHAVIOUR_INPUT_NORMAL = 0x0040,
    PUPPYCAM_BEHAVIOUR_INPUT_8DIR = 0x0080,
    PUPPYCAM_BEHAVIOUR_INPUT_4DIR = 0x0100,
    PUPPYCAM_BEHAVIOUR_INPUT_2D = 0x0200,

    PUPPYCAM_BEHAVIOUR_SLIDE_CORRECTION = 0x0400,
    PUPPYCAM_BEHAVIOUR_TURN_HELPER = 0x0800,
    PUPPYCAM_BEHAVIOUR_HEIGHT_HELPER = 0x1000,
    PUPPYCAM_BEHAVIOUR_PANSHIFT = 0x2000,

    PUPPYCAM_BEHAVIOUR_COLLISION = 0x4000,


    PUPPYCAM_BEHAVIOUR_DEFAULT = PUPPYCAM_BEHAVIOUR_X_MOVEMENT | PUPPYCAM_BEHAVIOUR_Y_MOVEMENT | PUPPYCAM_BEHAVIOUR_Z_MOVEMENT |
    PUPPYCAM_BEHAVIOUR_YAW_ROTATION | PUPPYCAM_BEHAVIOUR_PITCH_ROTATION | PUPPYCAM_BEHAVIOUR_ZOOM_CHANGE |
    PUPPYCAM_BEHAVIOUR_HEIGHT_HELPER | PUPPYCAM_BEHAVIOUR_TURN_HELPER | PUPPYCAM_BEHAVIOUR_INPUT_NORMAL | PUPPYCAM_BEHAVIOUR_PANSHIFT | PUPPYCAM_BEHAVIOUR_COLLISION
};

extern u8 gPCOptionOpen;
extern s32 gPuppyError;
extern struct gPuppyStruct gPuppyCam;
extern struct sPuppyVolume *sPuppyVolumeStack[MAX_PUPPYCAM_VOLUMES];
extern u16 gPuppyVolumeCount;
extern struct MemoryPool *gPuppyMemoryPool;
extern void puppycam_boot(void);
extern void puppycam_init(void);
extern void puppycam_loop(void);
extern void puppycam_shake(s16 x, s16 y, s16 z);
extern void find_surface_on_ray(Vec3f orig, Vec3f dir, struct Surface **hit_surface, Vec3f hit_pos);
extern f32 approach_f32_asymptotic(f32 current, f32 target, f32 multiplier);
extern void puppycam_default_config(void);
extern s16 LENCOS(s16 length, s16 direction);
extern s16 LENSIN(s16 length, s16 direction);
extern void puppycam_display_options(void);
extern void puppycam_set_save(void);
extern void puppycam_check_pause_buttons(void);
extern void puppycam_activate_cutscene(s32 *scene, s32 lockinput);
extern void puppycam_render_option_text();
void puppycam_warp(f32 displacementX, f32 displacementY, f32 displacementZ);

#endif

#endif // PUPPYCAM2_H
