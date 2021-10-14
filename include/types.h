#ifndef TYPES_H
#define TYPES_H

// This file contains various data types used in Super Mario 64 that don't yet
// have an appropriate header.

#include <ultra64.h>
#include "macros.h"
#include "config.h"

#define BIT(i)  (1 << (i))
#define BITMASK(size) ((BIT(size)) - 1)
#define SHIFTED_BITMASK(size, shift) (BITMASK(size) << shift)

// #define COND_BIT(cond, dst, flag) { (dst) &= ~(flag); if (cond) (dst) |= (flag); }
#define COND_BIT(cond, dst, flag) { \
    if ((cond)) {                   \
        (dst) |=  (flag);           \
    } else {                        \
        (dst) &= ~(flag);           \
    }                               \
}

#define SCREEN_CENTER_X (SCREEN_WIDTH  / 2)
#define SCREEN_CENTER_Y (SCREEN_HEIGHT / 2)

struct Config {
    f32 audioFrequency;
#ifdef WIDE
    s16 widescreen;
#endif
    u8 tvType;
};

struct Controller {
  /*0x00*/ s16 rawStickX;       //
  /*0x02*/ s16 rawStickY;       //
  /*0x04*/ f32 stickX;          // [-64, 64] positive is right
  /*0x08*/ f32 stickY;          // [-64, 64] positive is up
  /*0x0C*/ f32 stickMag;        // distance from center [0, 64]
  /*0x10*/ u16 buttonDown;
  /*0x12*/ u16 buttonPressed;
  /*0x14*/ OSContStatus *statusData;
  /*0x18*/ OSContPad *controllerData;
#if ENABLE_RUMBLE
  /*0x1C*/ s32 port;
#endif
};

// -- Booleans --

typedef u8  Bool8;
typedef u16 Bool16;
typedef u32 Bool32;

// -- Vectors --

typedef u8 Vec2uc[2];
typedef s8  Vec2c[2];
typedef s16 Vec2s[2];
typedef s32 Vec2i[2];
typedef f32 Vec2f[2];
typedef f64 Vec2d[2];

typedef u8 Vec3uc[3];
typedef s8  Vec3c[3];
typedef s16 Vec3s[3];
typedef s32 Vec3i[3];
typedef f32 Vec3f[3]; // X, Y, Z, where Y is up
typedef f64 Vec3d[3];

typedef u8 Vec4uc[4];
typedef s8  Vec4c[4];
typedef s16 Vec4s[4];
typedef s32 Vec4i[4];
typedef f32 Vec4f[4];
typedef f64 Vec4d[4];

typedef f32 Mat2[2][2];
typedef f32 Mat3[3][3];
typedef f32 Mat4[4][4];

// -- Scripts --
typedef uintptr_t GeoLayout;
typedef uintptr_t LevelScript;
typedef uintptr_t BehaviorScript;

// -- Mario/Objects --
typedef s32 MarioAction;
typedef s32 MarioActionArg;
typedef u32 MarioInteraction;
typedef u32 InteractType;
typedef u32 MarioStep;

typedef void (*ObjActionFunc)(void);

typedef s8  ObjAction8;
typedef s32 ObjAction32;
typedef s16 ColFlags;

// -- Angle --
typedef s16 Angle;
typedef u16 UAngle;
typedef s32 Angle32;
typedef Angle Vec3a[3];

// -- Collision --
typedef ROOM_DATA_TYPE RoomData;
typedef COLLISION_DATA_TYPE Collision; // Collision is by default an s16, but it's best to have it match the type of COLLISION_DATA_TYPE
typedef Collision TerrainData;
typedef Collision Vec3t[3];
typedef Collision SurfaceType;

typedef f32       Normal;
typedef Normal    Vec3n[3];

// -- Colors/Textures --

// 0.0f to 1.0f
typedef f32    ColorF;
typedef ColorF AlphaF;
typedef ColorF ColorRGBf[3];
typedef ColorF ColorRGBAf[4];
typedef ColorF ColorHSVf[3];
typedef ColorF ColorHSVAf[4];

// 0 to 255
typedef u8    Color;
typedef Color Alpha;
typedef Color ColorRGB[3];
typedef Color ColorRGBA[4];
typedef Color ColorHSV[3];
typedef Color ColorHSVA[4];

// Formats:
typedef u32   CompositeColor;  // to be casted to one of the following:
typedef u8    I4;              // u4
typedef u8    I8;
typedef u8    IA4;             // u4, components u3 & u1
typedef u8    IA8;
typedef u8    IA16Component;
typedef u16   IA16;
typedef u8    RGBA16Component; // components u5 & u1
typedef u16   RGBA16;
typedef u32   RGBA16Return32;
typedef u32   RGBA16FILL;      // RGBA16 but twice, Used for gDPSetFillColor
typedef u8    RGBA32Component;
typedef u32   RGBA32;
typedef u8    CI4;             // u4
typedef u8    CI8;
typedef Color Texture;
typedef u32   Texture32;

typedef s16   TextureCoord;

// -- Models --

typedef u8  ModelID8;
typedef u16 ModelID16;
typedef u32 ModelID32;

// -- Animations --
typedef s16 AnimValue;
typedef u16 AnimIndex;
typedef s16 AnimID16;
typedef s32 AnimID32;
typedef s16 AnimFrame16;
typedef s32 AnimFrame32;
typedef s32 AnimAccel;

// -- Misc. Data --

typedef s16 DialogID16;
typedef s32 DialogID;
typedef s32 DrawingLayer;
typedef s16 PaintingData;
typedef s32 CameraTransitionAngle;
typedef s16 Movtex;
typedef s16 MacroObject;
typedef s16 Trajectory;
typedef u8  CutsceneID;

typedef u8 uchar;

enum SpTaskState {
    SPTASK_STATE_NOT_STARTED,
    SPTASK_STATE_RUNNING,
    SPTASK_STATE_INTERRUPTED,
    SPTASK_STATE_FINISHED,
    SPTASK_STATE_FINISHED_DP
};

struct SPTask {
    /*0x00*/ OSTask task;
    /*0x40*/ OSMesgQueue *msgqueue;
    /*0x44*/ OSMesg msg;
    /*0x48*/ enum SpTaskState state;
}; // size = 0x4C, align = 0x8

struct VblankHandler {
    OSMesgQueue *queue;
    OSMesg msg;
};

enum AnimFlags {
    ANIM_FLAG_NOLOOP     = BIT(0), // 0x01
    ANIM_FLAG_FORWARD    = BIT(1), // 0x02
    ANIM_FLAG_NO_ACCEL   = BIT(2), // 0x04
    ANIM_FLAG_HOR_TRANS  = BIT(3), // 0x08
    ANIM_FLAG_VERT_TRANS = BIT(4), // 0x10
    ANIM_FLAG_DISABLED   = BIT(5), // 0x20
    ANIM_FLAG_NO_TRANS   = BIT(6), // 0x40
    ANIM_FLAG_UNUSED     = BIT(7), // 0x80
};

struct Animation {
    /*0x00*/ s16 flags;
    /*0x02*/ s16 animYTransDivisor;
    /*0x04*/ s16 startFrame;
    /*0x06*/ s16 loopStart;
    /*0x08*/ s16 loopEnd;
    /*0x0A*/ s16 unusedBoneCount;
    /*0x0C*/ const s16 *values;
    /*0x10*/ const u16 *index;
    /*0x14*/ u32 length; // only used with Mario animations to determine how much to load. 0 otherwise.
};

#define ANIMINDEX_NUMPARTS(animindex) (sizeof(animindex) / sizeof(u16) / 6 - 1)

struct GraphNode {
    /*0x00*/ s16 type; // structure type
    /*0x02*/ s16 flags; // hi = drawing layer, lo = rendering modes
    /*0x04*/ struct GraphNode *prev;
    /*0x08*/ struct GraphNode *next;
    /*0x0C*/ struct GraphNode *parent;
    /*0x10*/ struct GraphNode *children;
};

struct AnimInfo {
    /*0x00 0x38*/ s16 animID;
    /*0x02 0x3A*/ s16 animYTrans;
    /*0x04 0x3C*/ struct Animation *curAnim;
    /*0x08 0x40*/ s16 animFrame;
    /*0x0A 0x42*/ u16 animTimer;
    /*0x0C 0x44*/ s32 animFrameAccelAssist;
    /*0x10 0x48*/ s32 animAccel;
};

struct GraphNodeObject {
    /*0x00*/ struct GraphNode node;
    /*0x14*/ struct GraphNode *sharedChild;
    /*0x18*/ s8 areaIndex;
    /*0x19*/ s8 activeAreaIndex;
    /*0x1A*/ Vec3s angle;
    /*0x20*/ Vec3f pos;
    /*0x2C*/ Vec3f scale;
    /*0x38*/ struct AnimInfo animInfo;
    /*0x4C*/ struct SpawnInfo *spawnInfo;
    /*0x50*/ Mat4 *throwMatrix; // matrix ptr
    /*0x54*/ Vec3f cameraToObject;
#ifdef OBJECTS_REJ
    u16 ucode;
#endif
};

struct ObjectNode {
    struct GraphNodeObject gfx;
    struct ObjectNode *next;
    struct ObjectNode *prev;
};

#ifdef PUPPYLIGHTS
struct PuppyLight {
    Vec3t pos[2];   // The location of the light. First index is the absolute position, second index are offsets.
    s16 yaw;        // Used by cubes. Allows epic rotating of the volume.
    RoomData room;  // Which room to use. -1 is visible from all rooms.
    s8 epicentre;   // What percentage inside the volume you'll be before maximum light strength is applied. (E.g: 100 will be full strength always, and 0 will be full strength at the centre.)
    u8 flags;       // Some stuff to define how the volume is used. Mostly just shape stuff, but can potentially have other uses.
    ColorRGBA rgba; // Colour. Go on, take even the tiniest guess as to what this entails.
    u8 area;        // Which section of the level this light is stored in.
    u8 active: 1;   // Whether the light will actually work. Mostly intended to be used for objects.
};
#endif

// NOTE: Since ObjectNode is the first member of Object, it is difficult to determine
// whether some of these pointers point to ObjectNode or Object.

#ifdef PUPPYLIGHTS
#define MAX_OBJECT_FIELDS 0x51
#else
#define MAX_OBJECT_FIELDS 0x50
#endif

struct Object {
    /*0x000*/ struct ObjectNode header;
    /*0x068*/ struct Object *parentObj;
    /*0x06C*/ struct Object *prevObj;
    /*0x070*/ u32 collidedObjInteractTypes;
    /*0x074*/ s16 activeFlags;
    /*0x076*/ s16 numCollidedObjs;
    /*0x078*/ struct Object *collidedObjs[4];
    /*0x088*/
    union {
        // Object fields. See object_fields.h.
        u32 asU32[MAX_OBJECT_FIELDS];
        s32 asS32[MAX_OBJECT_FIELDS];
        s16 asS16[MAX_OBJECT_FIELDS][2];
        f32 asF32[MAX_OBJECT_FIELDS];
#if !IS_64_BIT
        s16 *asS16P[MAX_OBJECT_FIELDS];
        s32 *asS32P[MAX_OBJECT_FIELDS];
        struct Animation **asAnims[MAX_OBJECT_FIELDS];
        struct Waypoint *asWaypoint[MAX_OBJECT_FIELDS];
        struct ChainSegment *asChainSegment[MAX_OBJECT_FIELDS];
        struct Object *asObject[MAX_OBJECT_FIELDS];
        struct Surface *asSurface[MAX_OBJECT_FIELDS];
        void *asVoidPtr[MAX_OBJECT_FIELDS];
        const void *asConstVoidPtr[MAX_OBJECT_FIELDS];
#endif
    } rawData;
#if IS_64_BIT
    union {
        s16 *asS16P[MAX_OBJECT_FIELDS];
        s32 *asS32P[MAX_OBJECT_FIELDS];
        struct Animation **asAnims[MAX_OBJECT_FIELDS];
        struct Waypoint *asWaypoint[MAX_OBJECT_FIELDS];
        struct ChainSegment *asChainSegment[MAX_OBJECT_FIELDS];
        struct Object *asObject[MAX_OBJECT_FIELDS];
        struct Surface *asSurface[MAX_OBJECT_FIELDS];
        void *asVoidPtr[MAX_OBJECT_FIELDS];
        const void *asConstVoidPtr[MAX_OBJECT_FIELDS];
    } ptrData;
#endif
    /*0x1C8*/ u32 unused1;
    /*0x1CC*/ const BehaviorScript *curBhvCommand;
    /*0x1D0*/ u32 bhvStackIndex;
    /*0x1D4*/ uintptr_t bhvStack[8];
    /*0x1F4*/ s16 bhvDelayTimer;
    /*0x1F6*/ s16 respawnInfoType;
    /*0x1F8*/ f32 hitboxRadius;
    /*0x1FC*/ f32 hitboxHeight;
    /*0x200*/ f32 hurtboxRadius;
    /*0x204*/ f32 hurtboxHeight;
    /*0x208*/ f32 hitboxDownOffset;
    /*0x20C*/ const BehaviorScript *behavior;
    /*0x210*/ u32 unused2;
    /*0x214*/ struct Object *platform;
    /*0x218*/ void *collisionData;
    /*0x21C*/ Mat4 transform;
    /*0x25C*/ void *respawnInfo;
#ifdef PUPPYLIGHTS
    struct PuppyLight puppylight;
#endif
};

struct ObjectHitbox {
    /*0x00*/ u32 interactType;
    /*0x04*/ u8 downOffset;
    /*0x05*/ s8 damageOrCoinValue;
    /*0x06*/ s8 health;
    /*0x07*/ s8 numLootCoins;
    /*0x08*/ s16 radius;
    /*0x0A*/ s16 height;
    /*0x0C*/ s16 hurtboxRadius;
    /*0x0E*/ s16 hurtboxHeight;
};

struct Waypoint {
    s16 flags;
    Vec3s pos;
};

struct Normal {
    /*0x00*/ f32 x;
    /*0x04*/ f32 y;
    /*0x08*/ f32 z;
};

struct Surface {
    /*0x00*/ TerrainData type;
    /*0x02*/ TerrainData force;
    /*0x04*/ s8 flags;
    /*0x05*/ RoomData room;
    /*0x06*/ s16 lowerY;
    /*0x08*/ s16 upperY;
    /*0x0A*/ Vec3t vertex1;
    /*0x10*/ Vec3t vertex2;
    /*0x16*/ Vec3t vertex3;
    /*0x1C*/ struct Normal normal;
    /*0x28*/ f32 originOffset;
    /*0x2C*/ struct Object *object;
};

#define PUNCH_STATE_TIMER_MASK          0b00111111
#define PUNCH_STATE_TYPES_MASK          0b11000000

enum PunchStateTypes {
    PUNCH_STATE_TYPE_FIRST_PUNCH  = (0 << 6),
    PUNCH_STATE_TYPE_SECOND_PUNCH = (1 << 6),
    PUNCH_STATE_TYPE_KICK         = (2 << 6),
};

struct MarioBodyState {
    /*0x00*/ u32 action;
    /*0x04*/ s8 capState; /// see MarioCapGSCId
    /*0x05*/ s8 eyeState;
    /*0x06*/ s8 handState;
    /*0x07*/ s8 wingFlutter; /// whether Mario's wing cap wings are fluttering
    /*0x08*/ s16 modelState;
    /*0x0A*/ s8 grabPos;
    /*0x0B*/ u8 punchState; /// 2 bits for type of punch, 6 bits for punch animation timer
    /*0x0C*/ Vec3s torsoAngle;
    /*0x12*/ Vec3s headAngle;
    /*0x18*/ Vec3f heldObjLastPosition; /// also known as HOLP
    // u8 filler[4];
};

struct MarioState {
    /*0x00*/ u16 playerID;
    /*0x02*/ u16 input;
    /*0x04*/ u32 flags;
    /*0x08*/ u32 particleFlags;
    /*0x0C*/ u32 action;
    /*0x10*/ u32 prevAction;
    /*0x14*/ u32 terrainSoundAddend;
    /*0x18*/ u16 actionState;
    /*0x1A*/ u16 actionTimer;
    /*0x1C*/ u32 actionArg;
    /*0x20*/ f32 intendedMag;
    /*0x24*/ s16 intendedYaw;
    /*0x26*/ s16 invincTimer;
    /*0x28*/ u8 framesSinceA;
    /*0x29*/ u8 framesSinceB;
    /*0x2A*/ u8 wallKickTimer;
    /*0x2B*/ u8 doubleJumpTimer;
    /*0x2C*/ Vec3s faceAngle;
    /*0x32*/ Vec3s angleVel;
    /*0x38*/ s16 slideYaw;
    /*0x3A*/ s16 twirlYaw;
    /*0x3C*/ Vec3f pos;
    /*0x48*/ Vec3f vel;
    /*0x54*/ f32 forwardVel;
    /*0x58*/ f32 slideVelX;
    /*0x5C*/ f32 slideVelZ;
    /*0x60*/ struct Surface *wall;
    /*0x64*/ struct Surface *ceil;
    /*0x68*/ struct Surface *floor;
    /*0x6C*/ f32 ceilHeight;
    /*0x70*/ f32 floorHeight;
    /*0x74*/ s16 floorYaw;
    #define floorAngle floorYaw
    /*0x76*/ s16 waterLevel;
    /*0x78*/ struct Object *interactObj;
    /*0x7C*/ struct Object *heldObj;
    /*0x80*/ struct Object *usedObj;
    /*0x84*/ struct Object *riddenObj;
    /*0x88*/ struct Object *marioObj;
    /*0x8C*/ struct SpawnInfo *spawnInfo;
    /*0x90*/ struct Area *area;
    /*0x94*/ struct PlayerCameraState *statusForCamera;
    /*0x98*/ struct MarioBodyState *marioBodyState;
    /*0x9C*/ struct Controller *controller;
    /*0xA0*/ struct DmaHandlerList *animList;
    /*0xA4*/ u32 collidedObjInteractTypes;
    /*0xA8*/ s16 numCoins;
    /*0xAA*/ s16 numStars;
    /*0xAC*/ s8 numKeys; // Unused key mechanic
    /*0xAD*/ s8 numLives;
    /*0xAE*/ s16 health;
    /*0xB0*/ s16 animYTrans;
    /*0xB2*/ u8 hurtCounter;
    /*0xB3*/ u8 healCounter;
    /*0xB4*/ u8 squishTimer;
    /*0xB5*/ u8 fadeWarpOpacity;
    /*0xB6*/ u16 capTimer;
    /*0xB8*/ s16 prevNumStarsForDialog;
    /*0xBC*/ f32 peakHeight;
    /*0xC0*/ f32 quicksandDepth;
    /*0xC4*/ f32 windGravity;
    // -- HackerSM64 MarioState fields begin --
#ifdef BREATH_METER
             s16 breath;
             u8  breathCounter;
#endif
#ifdef PREVENT_DEATH_LOOP
             u8  isDead : 1;
#endif
           Vec3f lastSafePos;
           Vec3f prevPos;
             f32 lateralSpeed;
             f32 moveSpeed;
           Angle movePitch;
           Angle moveYaw;
           Angle ceilYaw;
           Angle wallYaw;
    // -- HackerSM64 MarioState fields end --
};

#endif // TYPES_H
