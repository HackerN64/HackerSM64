#ifndef AREA_H
#define AREA_H

#include <PR/ultratypes.h>

#include "types.h"
#include "camera.h"
#include "engine/graph_node.h"

struct WarpNode {
    /*00*/ u8 id;
    /*01*/ u8 destLevel;
    /*02*/ u8 destArea;
    /*03*/ u8 destNode;
};

struct ObjectWarpNode {
    /*0x00*/ struct WarpNode node;
    /*0x04*/ struct ObjectWarpNode *next;
};

struct InstantWarp {
    /*0x00*/ u8 id; // 0 = 0x1B / 1 = 0x1C / 2 = 0x1D / 3 = 0x1E
    /*0x01*/ u8 area;
    /*0x04*/ Vec3f displacement;
};

struct SpawnInfo {
    /*0x00*/ Vec3s startPos;
    /*0x06*/ Vec3s startAngle;
    /*0x0C*/ s8 areaIndex;
    /*0x0D*/ s8 activeAreaIndex;
    /*0x0E*/ u8 respawnInfo;
    /*0x10*/ u32 behaviorArg;
    /*0x14*/ void *behaviorScript;
    /*0x18*/ struct GraphNode *model;
    /*0x1C*/ struct SpawnInfo *next;
};

struct UnusedArea28 {
    /*0x00*/ s16 unk00;
    /*0x02*/ s16 unk02;
    /*0x04*/ s16 unk04;
    /*0x06*/ s16 unk06;
    /*0x08*/ s16 unk08;
};

struct Whirlpool {
    /*0x00*/ Vec3s pos;
    /*0x03*/ s16 strength;
};

enum AreaFlags {
    AREA_FLAG_UNLOAD,
    AREA_FLAG_LOAD
};

struct Area {
    /*0x00*/ s8 index;
    /*0x01*/ s8 flags; // Only has 1 flag: 0x01 = Is this the active area?
    /*0x02*/ TerrainData terrainType; // default terrain of the level (set from level script cmd 0x31)
    /*0x04*/ struct GraphNodeRoot *graphNode; // geometry layout data
    /*0x08*/ TerrainData *terrainData; // collision data (set from level script cmd 0x2E)
    /*0x0C*/ RoomData *surfaceRooms; // (set from level script cmd 0x2F)
    /*0x10*/ struct ObjectWarpNode *warpNodes;
    /*0x14*/ struct WarpNode *paintingWarpNodes;
    /*0x18*/ struct InstantWarp *instantWarps;
    /*0x1C*/ struct SpawnInfo *objectSpawnInfos;
    /*0x20*/ struct Camera *camera;
    /*0x24*/ struct UnusedArea28 *unused; // Filled by level script 0x3A, but is unused.
    /*0x28*/ struct Whirlpool *whirlpools[2];
    /*0x30*/ u8 dialog[2]; // Level start dialog number (set by level script cmd 0x30)
    /*0x32*/ u16 musicParam;
    /*0x34*/ u16 musicParam2;
    /*0x36*/ u8 useEchoOverride; // Should area echo be overridden using echoOverride?
    /*0x37*/ s8 echoOverride; // Value used to override the area echo values defined in level_defines.h
#ifdef BETTER_REVERB
    /*0x38*/ u8 betterReverbPreset;
#endif
};

// All the transition data to be used in screen_transition.c
struct WarpTransitionData {
    /*0x00*/ u8 red;
    /*0x01*/ u8 green;
    /*0x02*/ u8 blue;

    /*0x04*/ s16 startTexRadius;
    /*0x06*/ s16 endTexRadius;
    /*0x08*/ s16 startTexX;
    /*0x0A*/ s16 startTexY;
    /*0x0C*/ s16 endTexX;
    /*0x0E*/ s16 endTexY;

    /*0x10*/ s16 angleSpeed;
};

enum WarpTransitionFadeDirections {
    WARP_TRANSITION_FADE_FROM,
    WARP_TRANSITION_FADE_INTO
};

enum WarpTransitionTypes {
    WARP_TRANSITION_TYPE_COLOR  = 0x00,
    WARP_TRANSITION_TYPE_STAR   = 0x08,
    WARP_TRANSITION_TYPE_CIRCLE = 0x0A,
    WARP_TRANSITION_TYPES_LARGE = 0x0E,
    WARP_TRANSITION_TYPE_MARIO  = 0x10,
    WARP_TRANSITION_TYPE_BOWSER = 0x12,
    WARP_TRANSITION_TYPES_MASK  = 0x1E
};


enum WarpTransitions {
    WARP_TRANSITION_FADE_FROM_COLOR  = (WARP_TRANSITION_TYPE_COLOR  | WARP_TRANSITION_FADE_FROM), // 0x00
    WARP_TRANSITION_FADE_INTO_COLOR  = (WARP_TRANSITION_TYPE_COLOR  | WARP_TRANSITION_FADE_INTO), // 0x01
    WARP_TRANSITION_FADE_FROM_STAR   = (WARP_TRANSITION_TYPE_STAR   | WARP_TRANSITION_FADE_FROM), // 0x08
    WARP_TRANSITION_FADE_INTO_STAR   = (WARP_TRANSITION_TYPE_STAR   | WARP_TRANSITION_FADE_INTO), // 0x09
    WARP_TRANSITION_FADE_FROM_CIRCLE = (WARP_TRANSITION_TYPE_CIRCLE | WARP_TRANSITION_FADE_FROM), // 0x0A
    WARP_TRANSITION_FADE_INTO_CIRCLE = (WARP_TRANSITION_TYPE_CIRCLE | WARP_TRANSITION_FADE_INTO), // 0x0B
    WARP_TRANSITION_FADES_FROM_LARGE = (WARP_TRANSITION_TYPES_LARGE | WARP_TRANSITION_FADE_FROM), // 0x0E
    WARP_TRANSITION_FADES_INTO_LARGE = (WARP_TRANSITION_TYPES_LARGE | WARP_TRANSITION_FADE_INTO), // 0x0F
    WARP_TRANSITION_FADE_FROM_MARIO  = (WARP_TRANSITION_TYPE_MARIO  | WARP_TRANSITION_FADE_FROM), // 0x10
    WARP_TRANSITION_FADE_INTO_MARIO  = (WARP_TRANSITION_TYPE_MARIO  | WARP_TRANSITION_FADE_INTO), // 0x11
    WARP_TRANSITION_FADE_FROM_BOWSER = (WARP_TRANSITION_TYPE_BOWSER | WARP_TRANSITION_FADE_FROM), // 0x12
    WARP_TRANSITION_FADE_INTO_BOWSER = (WARP_TRANSITION_TYPE_BOWSER | WARP_TRANSITION_FADE_INTO), // 0x13
};

struct WarpTransition {
    /*0x00*/ u8 isActive;       // Is the transition active. (either TRUE or FALSE)
    /*0x01*/ u8 type;           // Determines the type of transition to use (circle, star, etc.)
    /*0x02*/ u8 time;           // Amount of time to complete the transition (in frames)
    /*0x03*/ u8 pauseRendering; // Should the game stop rendering. (either TRUE or FALSE)
    /*0x04*/ struct WarpTransitionData data;
};

enum CurrSaveFileNum {
    SAVE_FILE_NUM_A = 0x1,
    SAVE_FILE_NUM_B,
    SAVE_FILE_NUM_C,
    SAVE_FILE_NUM_D,
};

enum MenuOption {
    MENU_OPT_NONE,
    MENU_OPT_1,
    MENU_OPT_2,
    MENU_OPT_3,
    MENU_OPT_DEFAULT = MENU_OPT_1,

    // Course Pause Menu
    MENU_OPT_CONTINUE = MENU_OPT_1,
    MENU_OPT_EXIT_COURSE = MENU_OPT_2,
    MENU_OPT_CAMERA_ANGLE_R = MENU_OPT_3,

    // Save Menu
    MENU_OPT_SAVE_AND_CONTINUE = MENU_OPT_1,
    MENU_OPT_SAVE_AND_QUIT = MENU_OPT_2,
    MENU_OPT_CONTINUE_DONT_SAVE = MENU_OPT_3
};

extern struct GraphNode **gLoadedGraphNodes;
extern struct SpawnInfo gPlayerSpawnInfos[];
extern struct GraphNode *gGraphNodePointers[MODEL_ID_COUNT];
extern struct Area gAreaData[];
extern struct WarpTransition gWarpTransition;
extern s16 gCurrCourseNum;
extern s16 gCurrActNum;
extern s16 gCurrAreaIndex;
extern s16 gSavedCourseNum;
extern s16 gMenuOptSelectIndex;
extern s16 gSaveOptSelectIndex;

extern struct SpawnInfo *gMarioSpawnInfo;

extern struct Area *gAreas;
extern struct Area *gCurrentArea;

extern s16 gCurrSaveFileNum;
extern s16 gCurrLevelNum;


void override_viewport_and_clip(Vp *a, Vp *b, u8 c, u8 d, u8 e);
void print_intro_text(void);
u32 get_mario_spawn_type(struct Object *obj);
struct ObjectWarpNode *area_get_warp_node(u8 id);
struct Object *get_destination_warp_object(u8 warpDestId);
void clear_areas(void);
void clear_area_graph_nodes(void);
void load_area(s32 index);
void unload_area(void);
void load_mario_area(void);
void unload_mario_area(void);
void change_area(s32 index);
void area_update_objects(void);
void play_transition(s16 transType, s16 time, u8 red, u8 green, u8 blue);
void play_transition_after_delay(s16 transType, s16 time, u8 red, u8 green, u8 blue, s16 delay);
void render_game(void);

#endif // AREA_H
