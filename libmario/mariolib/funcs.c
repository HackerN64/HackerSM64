#include <PR/ultratypes.h>
#include <types.h>
#include <sm64.h>
#include "area.h"
#include "level_update.h"
#include "surface_collision.h"
#include "mario.h"
#include "object_list_processor.h"

void spawn_wind_particles(s16 pitch, s16 yaw) {}
s32 save_file_get_flags() { return 0; }
void *segmented_to_virtual(const void *addr) { return (void*)addr; }
void *virtual_to_segmented(u32 segment, const void *addr) { return (void*)addr; }
void *alloc_display_list(u32 size) { return NULL; }

void dl_rgba16_begin_cutscene_msg_fade() {}
void dl_rgba16_stop_cutscene_msg_fade(void) {}
void print_text_fmt_int(s32 x, s32 y, const char *str, s32 n) {}
void print_credits_str_ascii(s16 x, s16 y, const char *str) {}
void print_debug_top_down_objectinfo(const char *str, s32 number) {}
void play_transition(s16 transType, s16 time, u8 red, u8 green, u8 blue) {}
void create_dialog_box(s16 dialog) {}
void create_dialog_box_with_response(s16 dialog) {}
void create_dialog_box_with_var(s16 dialog, s32 dialogVar) {}
void create_dialog_inverted_box(s16 dialog) {}
void trigger_cutscene_dialog(s32 trigger) {}
s16 get_dialog_id(void) {}
void fade_into_special_warp(u32 arg, u32 color) {}
s16 cutscene_object_without_dialog(u8 cutscene, struct Object *o) { return 0; }
s16 cutscene_object_with_dialog(u8 cutscene, struct Object *o, s16 dialogID) { return 0; }

void set_camera_mode(struct Camera *c, s16 mode, s16 frames) {}
void set_camera_shake_from_hit(s16 shake) {}
f32 camera_approach_f32_symmetric(f32 current, f32 target, f32 increment) {}
void set_camera_shake_from_point(s16 shake, f32 posX, f32 posY, f32 posZ) {}

void bhv_spawn_star_no_level_exit(u32 sp20) {}
s16 level_trigger_warp(struct MarioState *m, s32 warpOp) { return 0; }
void load_level_init_text(u32 arg) {}
void set_cutscene_message(s16 xOffset, s16 yOffset, s16 msgIndex, s16 msgDuration) {}
s32 save_file_get_total_star_count(s32 fileIndex, s32 minCourse, s32 maxCourse) { return 0; }
void set_menu_mode(s16 mode) {}

void play_music(u8 player, u16 seqArgs, u16 fadeTimer) {}
void raise_background_noise(s32 a) {}
void play_toads_jingle(void) {}
void play_menu_sounds(s16 soundMenuFlags) {}
void stop_sound(u32 soundBits, f32 *pos) {}
void *play_sound(s32 soundBits, f32 *pos) {}
void set_sound_moving_speed(u8 bank, u8 speed) {}
void play_infinite_stairs_music() {}
void lower_background_noise(s32 a) {}
void seq_player_lower_volume(u8 player, u16 fadeDuration, u8 percentage) {}
void seq_player_unlower_volume(u8 player, u16 fadeDuration) {}
void play_cutscene_music(u16 seqArgs) {}
void play_course_clear(void) {}
void sound_banks_enable(UNUSED u8 player, u16 bankMask) {}
void disable_background_sound(void) {}
void enable_background_sound(void) {}
void play_peachs_jingle(void) {}
void cur_obj_play_sound_2(s32 soundMagic) {}
void play_shell_music(void) {}
void stop_shell_music(void) {}
void play_cap_music(void) {}
void stop_cap_music(void) {}
void fadeout_cap_music(void) {}

Gfx *gdm_gettestdl(s32 id) { return NULL; }
void gd_vblank(void) {}
s32 gd_sfx_to_play(void) { return 0; }
void gd_copy_p1_contpad(OSContPad *p1cont) {}

void save_file_do_save(s32 fileIndex) {}
void save_file_set_flags(u32 flags) {}
void save_file_clear_flags(u32 flags) {}
void override_viewport_and_clip(Vp *a, Vp *b, u8 c, u8 d, u8 e) {}
void reset_cutscene_msg_fade(void) {}
u32 get_door_save_file_flag(struct Object *door) { return 0; }
s32 save_file_get_cap_pos(Vec3s capPos) {}

struct GraphNode *geo_add_child(struct GraphNode *parent, struct GraphNode *childNode) { return childNode; }
struct GraphNode *geo_remove_child(struct GraphNode *graphNode) { return graphNode->parent; }
void geo_obj_init_animation(struct GraphNodeObject *graphNode, struct Animation **animPtrAddr) {}
void geo_obj_init(struct GraphNodeObject *graphNode, void *sharedChild, Vec3f pos, Vec3s angle) {}
void geo_obj_init_animation_accel(struct GraphNodeObject *graphNode, struct Animation **animPtrAddr, u32 animAccel) {}

struct GraphNodeObject *init_graph_node_object(struct AllocOnlyPool *pool,
                                               struct GraphNodeObject *graphNode,
                                               struct GraphNode *sharedChild, Vec3f pos, Vec3s angle,
                                               Vec3f scale) { return graphNode; }

void mario_grab_used_object(struct MarioState *m) {}
void mario_drop_held_object(struct MarioState *m) {}
u32 mario_check_object_grab(struct MarioState *m) { return FALSE; }
void mario_throw_held_object(struct MarioState *m) {}
void mario_stop_riding_object(struct MarioState *m) {}
void mario_stop_riding_and_holding(struct MarioState *m) {}
void mario_blow_off_cap(struct MarioState *m, f32 capSpeed) {}

void spawn_default_star(f32 sp20, f32 sp24, f32 sp28) {}
void spawn_mist_particles_variable(s32 count, s32 offsetY, f32 size) {}
void create_sound_spawner(s32 soundMagic) {}
void spawn_triangle_break_particles(s16 numTris, s16 triModel, f32 triSize, s16 triAnimState) {}
struct Object *create_object(const BehaviorScript *bhvScript) { return NULL; }

s16 gFindFloorIncludeSurfaceIntangible = FALSE;
s8 gDebugLevelSelect = FALSE;
s8 gShowDebugText = FALSE;
s16 gDebugInfo[16][8];
u32 gTimeStopState = 0;
s8 gNeverEnteredCastle = 1;
u16 gAreaUpdateCounter = 0;
struct Controller *gPlayer1Controller;
struct DmaHandlerList gMarioAnimsBuf;
float gPaintingMarioYEntry = 0.0f;

struct MarioState gMarioStates[1];
struct HudDisplay gHudDisplay;
struct MarioState *gMarioState = &gMarioStates[0];
struct Object MarioObjectInstance;
struct Object *gMarioObject = &MarioObjectInstance;
struct SpawnInfo gPlayerSpawnInfos[1];
struct SpawnInfo *gMarioSpawnInfo = &gPlayerSpawnInfos[0];
struct GraphNode **gLoadedGraphNodes = NULL;
struct Area *gAreas = gAreaData;
struct Area gCurrentAreaInstance;
struct Area *gCurrentArea = &gCurrentAreaInstance;
struct Object *gCurrentObject = &MarioObjectInstance;
struct Object *gCutsceneFocus = NULL;
struct Object *gSecondCameraFocus = NULL;
s16 gWarpTransDelay = 0;
u32 gFBSetColor = 0;
u32 gWarpTransFBSetColor = 0;
u8 gWarpTransRed = 0;
u8 gWarpTransGreen = 0;
u8 gWarpTransBlue = 0;
s16 gCurrSaveFileNum = 1;
s16 gCurrLevelNum = LEVEL_MIN;
u32 gAudioRandom = 0;
s32 gDialogResponse = 0;

struct Camera *gCamera;

s16 gCurrCourseNum;
s16 gCurrActNum;
s16 gCurrAreaIndex;
s16 gSavedCourseNum;
s16 gPauseScreenMode;
s16 gSaveOptSelectIndex;
s16 gMarioCurrentRoom;

s16 gCurrAreaIndex = 0;
u8 gLastCompletedCourseNum = 1;
u8 gLastCompletedStarNum = 0;
s8 sUnusedGotGlobalCoinHiScore = 0;
u8 gGotFileCoinHiScore = FALSE;
u8 gCurrCourseStarFlags = 0;
u8 gSpecialTripleJump = FALSE;

s16 gCameraMovementFlags = 0;

struct CreditsEntry *gCurrCreditsEntry = NULL;

struct GraphNodeRoot *gCurGraphNodeRoot = NULL;
struct GraphNodeMasterList *gCurGraphNodeMasterList = NULL;
struct GraphNodePerspective *gCurGraphNodeCamFrustum = NULL;
struct GraphNodeCamera *gCurGraphNodeCamera = NULL;
struct GraphNodeObject *gCurGraphNodeObject = NULL;
struct GraphNodeHeldObject *gCurGraphNodeHeldObject = NULL;
struct PlayerCameraState gPlayerCameraState[2];
struct WarpTransition gWarpTransition;

const BehaviorScript bhvSparkleSpawn[] = { 0 };
const BehaviorScript bhvJumpingBox[] = { 0 };
const BehaviorScript bhvEndToad[] = { 0 };
const BehaviorScript bhvEndPeach[] = { 0 };
const BehaviorScript bhvCelebrationStar[] = { 0 };
const BehaviorScript bhvStaticObject[] = { 0 };
const BehaviorScript bhvTree[] = { 0 };
const BehaviorScript bhvGiantPole[] = { 0 };
const BehaviorScript bhvBowserKeyUnlockDoor[] = { 0 };
const BehaviorScript bhvUnlockDoorStar[] = { 0 };
const BehaviorScript bhvCarrySomethingDropped[] = { 0 };
const BehaviorScript bhvCarrySomethingHeld[] = { 0 };
const BehaviorScript bhvCarrySomethingThrown[] = { 0 };
const BehaviorScript bhvBowserKeyCourseExit[] = { 0 };
const BehaviorScript bhvKoopaShellUnderwater[] = { 0 };
const BehaviorScript bhvMrIBlueCoin[] = { 0 };
const BehaviorScript bhvNormalCap[] = { 0 };
const BehaviorScript bhvBowser[] = { 0 };
const BehaviorScript bhvBlueCoinJumping[] = { 0 };
const BehaviorScript bhvSingleCoinGetsSpawned[] = { 0 };
const BehaviorScript bhvSpawnedStarNoLevelExit[] = { 0 };
const BehaviorScript bhvWhitePuffExplosion[] = { 0 };

struct Area gAreaData[8];
s8 gDoorAdjacentRooms[60][2];
struct ObjectNode gObjectListArray[16];
struct ObjectNode gFreeObjectList;
struct ObjectNode *gObjectLists = gObjectListArray;
s16 gPrevFrameObjectCount = 0;

f32 gGlobalSoundSource[3] = { 0.0f, 0.0f, 0.0f };


// s16 gCheckingSurfaceCollisionsForCamera;
// s16 gFindFloorIncludeSurfaceIntangible;
// s16 *gEnvironmentRegions;
// s32 gEnvironmentLevels[20];
// s8 gDoorAdjacentRooms[60][2];
// s16 gMarioCurrentRoom;
// s16 D_8035FEE2;
// s16 D_8035FEE4;
// s16 gTHIWaterDrained;
// s16 gTTCSpeedSetting;
// s16 gMarioShotFromCannon;
// s16 gCCMEnteredSlide;
s16 gNumRoomedObjectsInMarioRoom = 0;
s16 gNumRoomedObjectsNotInMarioRoom = 0;
// s16 gWDWWaterLevelChanging;
// s16 gMarioOnMerryGoRound;

void (*gGoddardVblankCallback)(void) = NULL;

s32 atan2s(f32 y, f32 x);

s16 mario_obj_angle_to_object(struct MarioState *m, struct Object *o) {
    f32 dx = o->oPosX - m->pos[0];
    f32 dz = o->oPosZ - m->pos[2];

    return atan2s(dz, dx);
}

void guMtxF2L(float mf[4][4], Mtx *m) {
    int r, c;
    s32 tmp1;
    s32 tmp2;
    s32 *m1 = &m->m[0][0];
    s32 *m2 = &m->m[2][0];
    for (r = 0; r < 4; r++) {
        for (c = 0; c < 2; c++) {
            tmp1 = mf[r][2 * c] * 65536.0f;
            tmp2 = mf[r][2 * c + 1] * 65536.0f;
            *m1++ = (tmp1 & 0xffff0000) | ((tmp2 >> 0x10) & 0xffff);
            *m2++ = ((tmp1 << 0x10) & 0xffff0000) | (tmp2 & 0xffff);
        }
    }
}

void guMtxL2F(float mf[4][4], Mtx *m) {
    int r, c;
    u32 tmp1;
    u32 tmp2;
    u32 *m1;
    u32 *m2;
    s32 stmp1, stmp2;
    m1 = (u32 *) &m->m[0][0];
    m2 = (u32 *) &m->m[2][0];
    for (r = 0; r < 4; r++) {
        for (c = 0; c < 2; c++) {
            tmp1 = (*m1 & 0xffff0000) | ((*m2 >> 0x10) & 0xffff);
            tmp2 = ((*m1++ << 0x10) & 0xffff0000) | (*m2++ & 0xffff);
            stmp1 = *(s32 *) &tmp1;
            stmp2 = *(s32 *) &tmp2;
            mf[r][c * 2 + 0] = stmp1 / 65536.0f;
            mf[r][c * 2 + 1] = stmp2 / 65536.0f;
        }
    }
}

/**
 * Update the animation frame of an object. The animation flags determine
 * whether it plays forwards or backwards, and whether it stops or loops at
 * the end etc.
 */
s32 geo_update_animation_frame(struct AnimInfo *obj, s32 *accelAssist) {
    s32 result;
    struct Animation *anim;

    anim = obj->curAnim;

    if (obj->animTimer == gAreaUpdateCounter || anim->flags & ANIM_FLAG_NO_ACCEL) {
        if (accelAssist != NULL) {
            accelAssist[0] = obj->animFrameAccelAssist;
        }

        return obj->animFrame;
    }

    if (anim->flags & ANIM_FLAG_FORWARD) {
        if (obj->animAccel) {
            result = obj->animFrameAccelAssist - obj->animAccel;
        } else {
            result = (obj->animFrame - 1) << 16;
        }

        if (GET_HIGH_S16_OF_32(result) < anim->loopStart) {
            if (anim->flags & ANIM_FLAG_NOLOOP) {
                SET_HIGH_S16_OF_32(result, anim->loopStart);
            } else {
                SET_HIGH_S16_OF_32(result, anim->loopEnd - 1);
            }
        }
    } else {
        if (obj->animAccel != 0) {
            result = obj->animFrameAccelAssist + obj->animAccel;
        } else {
            result = (obj->animFrame + 1) << 16;
        }

        if (GET_HIGH_S16_OF_32(result) >= anim->loopEnd) {
            if (anim->flags & ANIM_FLAG_NOLOOP) {
                SET_HIGH_S16_OF_32(result, anim->loopEnd - 1);
            } else {
                SET_HIGH_S16_OF_32(result, anim->loopStart);
            }
        }
    }

    if (accelAssist != 0) {
        accelAssist[0] = result;
    }

    return GET_HIGH_S16_OF_32(result);
}

/**
 * Retrieves an index into animation data based on the attribute pointer
 * An attribute is an x-, y- or z-component of the translation / rotation for a part
 * Each attribute is a pair of s16's, where the first s16 represents the maximum frame
 * and the second s16 the actual index. This index can be used to index in the array
 * with actual animation values.
 */
s32 retrieve_animation_index(s32 frame, u16 **attributes) {
    s32 result;

    if (frame < (*attributes)[0]) {
        result = (*attributes)[1] + frame;
    } else {
        result = (*attributes)[1] + (*attributes)[0] - 1;
    }

    *attributes += 2;

    return result;
}

void check_death_barrier(struct MarioState *m) {
    if (m->pos[1] < m->floorHeight + 2048.0f) {
        if (level_trigger_warp(m, WARP_OP_WARP_FLOOR) == 20 && !(m->flags & MARIO_FALL_SOUND_PLAYED)) {
            play_sound(SOUND_MARIO_WAAAOOOW, m->marioObj->header.gfx.cameraToObject);
        }
    }
}

void update_mario_sound_and_camera(struct MarioState *m);
s32 drop_and_set_mario_action(struct MarioState *m, u32 action, u32 actionArg);

void check_lava_boost(struct MarioState *m) {
    if (!(m->action & ACT_FLAG_RIDING_SHELL) && m->pos[1] < m->floorHeight + 10.0f) {
        if (!(m->flags & MARIO_METAL_CAP)) {
            m->hurtCounter += (m->flags & MARIO_CAP_ON_HEAD) ? 12 : 18;
        }

        update_mario_sound_and_camera(m);
        drop_and_set_mario_action(m, ACT_LAVA_BOOST, 0);
    }
}

void pss_begin_slide(UNUSED struct MarioState *m) {}
void pss_end_slide(struct MarioState *m) {}

void mario_handle_special_floors(struct MarioState *m) {
    if ((m->action & ACT_GROUP_MASK) == ACT_GROUP_CUTSCENE) {
        return;
    }

    if (m->floor != NULL) {
        s32 floorType = m->floor->type;

        switch (floorType) {
            case SURFACE_DEATH_PLANE:
            case SURFACE_VERTICAL_WIND:
                check_death_barrier(m);
                break;

            case SURFACE_WARP:
                level_trigger_warp(m, WARP_OP_WARP_FLOOR);
                break;

            case SURFACE_TIMER_START:
                pss_begin_slide(m);
                break;

            case SURFACE_TIMER_END:
                pss_end_slide(m);
                break;
        }

        if (!(m->action & ACT_FLAG_AIR) && !(m->action & ACT_FLAG_SWIMMING)) {
            switch (floorType) {
                case SURFACE_BURNING:
                    check_lava_boost(m);
                    break;
            }
        }
    }
}

/**
 * Find the height of the highest floor below a point.
 */
f32 find_floor_height(f32 x, f32 y, f32 z) {
    struct Surface *floor;

    f32 floorHeight = find_floor(x, y, z, &floor);

    return floorHeight;
}

s32 f32_find_wall_collision(f32 *xPtr, f32 *yPtr, f32 *zPtr, f32 offsetY, f32 radius) {
    struct WallCollisionData collision;
    s32 numCollisions = 0;

    collision.offsetY = offsetY;
    collision.radius = radius;

    collision.x = *xPtr;
    collision.y = *yPtr;
    collision.z = *zPtr;

    collision.numWalls = 0;

    numCollisions = find_wall_collisions(&collision);

    *xPtr = collision.x;
    *yPtr = collision.y;
    *zPtr = collision.z;

    return numCollisions;
}

s32 find_poison_gas_level(s32 x, s32 z) {
    s32 gasLevel = FLOOR_LOWER_LIMIT;
    return gasLevel;
}

u32 gGlobalTimer = 0;
struct Controller gControllers[1];

void copy_mario_state_to_object(void) {
    s32 i = 0;
    // L is real
    if (gCurrentObject != gMarioObject) {
        i += 1;
    }

    gCurrentObject->oVelX = gMarioStates[i].vel[0];
    gCurrentObject->oVelY = gMarioStates[i].vel[1];
    gCurrentObject->oVelZ = gMarioStates[i].vel[2];

    gCurrentObject->oPosX = gMarioStates[i].pos[0];
    gCurrentObject->oPosY = gMarioStates[i].pos[1];
    gCurrentObject->oPosZ = gMarioStates[i].pos[2];

    gCurrentObject->oMoveAnglePitch = gCurrentObject->header.gfx.angle[0];
    gCurrentObject->oMoveAngleYaw = gCurrentObject->header.gfx.angle[1];
    gCurrentObject->oMoveAngleRoll = gCurrentObject->header.gfx.angle[2];

    gCurrentObject->oFaceAnglePitch = gCurrentObject->header.gfx.angle[0];
    gCurrentObject->oFaceAngleYaw = gCurrentObject->header.gfx.angle[1];
    gCurrentObject->oFaceAngleRoll = gCurrentObject->header.gfx.angle[2];

    gCurrentObject->oAngleVelPitch = gMarioStates[i].angleVel[0];
    gCurrentObject->oAngleVelYaw = gMarioStates[i].angleVel[1];
    gCurrentObject->oAngleVelRoll = gMarioStates[i].angleVel[2];
}

extern const struct Animation *gMarioAnims[];
extern u32 gMarioNumAnims;

#include <stdio.h>
#include <string.h>
#include <time.h>

s32 load_patchable_table(struct DmaHandlerList *list, s32 index) {
    s32 ret = FALSE;
    const struct Animation *addr;

    if (index < gMarioNumAnims) {
        addr = gMarioAnims[index];
        if (list->currentAddr != (u8*)addr) {
            memcpy(list->bufTarget, addr, sizeof(struct Animation));
            list->currentAddr = (u8*)addr;
        }
    }
    return ret;
}

s32 execute_mario_action(struct Object *);

struct Animation gTargetAnim;

void delay(int milliseconds)
{
    long pause;
    clock_t now,then;

    pause = milliseconds*(CLOCKS_PER_SEC/1000);
    now = then = clock();
    while( (now-then) < pause )
        now = clock();
}

u8 gCurAnimType;
u8 gCurAnimEnabled;
s16 gCurrAnimFrame;
f32 gCurAnimTranslationMultiplier;
u16 *gCurrAnimAttribute;
s16 *gCurAnimData;
// after processing an object, the type is reset to this
#define ANIM_TYPE_NONE                  0

// Not all parts have full animation: to save space, some animations only
// have xz, y, or no translation at all. All animations have rotations though
#define ANIM_TYPE_TRANSLATION           1
#define ANIM_TYPE_VERTICAL_TRANSLATION  2
#define ANIM_TYPE_LATERAL_TRANSLATION   3
#define ANIM_TYPE_NO_TRANSLATION        4

void geo_set_animation_globals(struct AnimInfo *node, s32 hasAnimation) {
    struct Animation *anim = node->curAnim;

    if (hasAnimation) {
        node->animFrame = geo_update_animation_frame(node, &node->animFrameAccelAssist);
    }
    node->animTimer = gAreaUpdateCounter;
    if (anim->flags & ANIM_FLAG_HOR_TRANS) {
        gCurAnimType = ANIM_TYPE_VERTICAL_TRANSLATION;
    } else if (anim->flags & ANIM_FLAG_VERT_TRANS) {
        gCurAnimType = ANIM_TYPE_LATERAL_TRANSLATION;
    } else if (anim->flags & ANIM_FLAG_NO_TRANS) {
        gCurAnimType = ANIM_TYPE_NO_TRANSLATION;
    } else {
        gCurAnimType = ANIM_TYPE_TRANSLATION;
    }

    gCurrAnimFrame = node->animFrame;
    gCurAnimEnabled = (anim->flags & ANIM_FLAG_DISABLED) == 0;
    gCurrAnimAttribute = segmented_to_virtual((void *) anim->index);
    gCurAnimData = segmented_to_virtual((void *) anim->values);

    if (anim->animYTransDivisor == 0) {
        gCurAnimTranslationMultiplier = 1.0f;
    } else {
        gCurAnimTranslationMultiplier = (f32) node->animYTrans / (f32) anim->animYTransDivisor;
    }
}

struct Camera areaCamera;

#ifdef _WIN32
#define EXPORT __declspec( dllexport )
#define ADDCALL __cdecl
#else
#define EXPORT // __attribute__ ((visibility ("default")))
#define ADDCALL
#endif

typedef f32 FindFloorHandler_t(f32 x, f32 y, f32 z, struct Surface *floor, s32 *foundFloor);
typedef f32 FindCeilHandler_t(f32 x, f32 y, f32 z, struct Surface *ceil, s32 *foundCeil);
typedef s32 FindWallHandler_t(f32 x, f32 y, f32 z, f32 offsetY, f32 radius, struct Surface walls[4], Vec3f posOut);
typedef f32 FindWaterLevelHandler_t(f32 x, f32 z);

FindFloorHandler_t *gFloorHandler = NULL;
FindCeilHandler_t *gCeilHandler = NULL;
FindWallHandler_t *gWallHandler = NULL;
FindWaterLevelHandler_t *gWaterLevelHandler = NULL;

struct Surface surfacePool[0x100];
s32 surfacesUsed = 0;

f32 find_floor(f32 xPos, f32 yPos, f32 zPos, struct Surface **pfloor) { 
    f32 height = FLOOR_LOWER_LIMIT;
    *pfloor = NULL;

    if (gFloorHandler) {
        struct Surface *curSurface = &surfacePool[surfacesUsed];
        s32 foundFloor;
        height = gFloorHandler(xPos, yPos, zPos, curSurface, &foundFloor);
        if (foundFloor) {
            *pfloor = curSurface;
            surfacesUsed++;
        }
    }

    return height;
}

f32 find_ceil(f32 xPos, f32 yPos, f32 zPos, struct Surface **pceil) {
    f32 height = CELL_HEIGHT_LIMIT;
    *pceil = NULL;

    if (gCeilHandler) {
        struct Surface *curSurface = &surfacePool[surfacesUsed];
        s32 foundCeil;
        height = gCeilHandler(xPos, yPos, zPos, curSurface, &foundCeil);
        if (foundCeil) {
            *pceil = curSurface;
            surfacesUsed++;
        }
    }

    return height;
}

s32 find_wall_collisions(struct WallCollisionData *colData) {
    s32 numWalls = 0;

    if (gWallHandler) {
        struct Surface *curSurface = &surfacePool[surfacesUsed];
        Vec3f posOut;
        numWalls = gWallHandler(colData->x, colData->y, colData->z, colData->offsetY, colData->radius, curSurface, posOut);
        colData->walls[0] = &curSurface[0];
        colData->walls[1] = &curSurface[1];
        colData->walls[2] = &curSurface[2];
        colData->walls[3] = &curSurface[3];
        colData->x = posOut[0];
        colData->y = posOut[1];
        colData->z = posOut[2];
        surfacesUsed += (numWalls > 4 ? 4 : numWalls);
    }

    colData->numWalls = numWalls;
    return numWalls;
}

s32 find_water_level(s32 x, s32 z) {
    s32 waterHeight = FLOOR_LOWER_LIMIT;
    if (gWaterLevelHandler) {
        waterHeight = gWaterLevelHandler(x, z);
    }
    return waterHeight;
}

struct Object *mario_get_collided_object(struct MarioState *m, u32 interactType) {
    return NULL;
}

#include "math_util.h"

void check_kick_or_punch_wall(struct MarioState *m) {
    if (m->flags & (MARIO_PUNCHING | MARIO_KICKING | MARIO_TRIPPING)) {
        struct WallCollisionData detector;
        detector.x = m->pos[0] + 50.0f * sins(m->faceAngle[1]);
        detector.z = m->pos[2] + 50.0f * coss(m->faceAngle[1]);
        detector.y = m->pos[1];
        detector.offsetY = 80.0f;
        detector.radius = 5.0f;

        if (find_wall_collisions(&detector) > 0) {
            if (m->action != ACT_MOVE_PUNCHING || m->forwardVel >= 0.0f) {
                if (m->action == ACT_PUNCHING) {
                    m->action = ACT_MOVE_PUNCHING;
                }

                mario_set_forward_vel(m, -48.0f);
                play_sound(SOUND_ACTION_HIT_2, m->marioObj->header.gfx.cameraToObject);
                m->particleFlags |= PARTICLE_TRIANGLE;
            } else if (m->action & ACT_FLAG_AIR) {
                mario_set_forward_vel(m, -16.0f);
                play_sound(SOUND_ACTION_HIT_2, m->marioObj->header.gfx.cameraToObject);
                m->particleFlags |= PARTICLE_TRIANGLE;
            }
        }
    }
}

void mario_process_interactions(struct MarioState *m) {
    check_kick_or_punch_wall(m);
    m->flags &= ~MARIO_PUNCHING & ~MARIO_KICKING & ~MARIO_TRIPPING;
}

f32 find_room_floor(f32 x, f32 y, f32 z, struct Surface **pfloor) {
    return find_floor(x, y, z, pfloor);
}

/**
 * Collides with walls and returns the most recent wall.
 */
void resolve_and_return_wall_collisions(Vec3f pos, f32 offset, f32 radius, struct WallCollisionData *collisionData) {
    collisionData->x = pos[0];
    collisionData->y = pos[1];
    collisionData->z = pos[2];
    collisionData->radius = radius;
    collisionData->offsetY = offset;

    find_wall_collisions(collisionData);

    pos[0] = collisionData->x;
    pos[1] = collisionData->y;
    pos[2] = collisionData->z;
}

EXPORT void ADDCALL init(FindFloorHandler_t *floorHandler, FindCeilHandler_t *ceilHandler, FindWallHandler_t *wallHandler, FindWaterLevelHandler_t *waterHandler) {
    gFloorHandler = floorHandler;
    gCeilHandler = ceilHandler;
    gWallHandler = wallHandler;
    gWaterLevelHandler = waterHandler;
    memset(gMarioState, 0, sizeof(struct MarioState));
    gMarioState->pos[0] = 0.0f;
    gMarioState->pos[1] = 100.0f;
    gMarioState->pos[2] = 0.0f;
    gMarioState->action = ACT_FREEFALL;
    gMarioState->marioObj = gMarioObject;
    gMarioState->marioBodyState = &gBodyStates[0];
    gMarioState->controller = &gControllers[0];
    gMarioState->area = &gAreaData[0];
    gMarioState->animList = &gMarioAnimsBuf;
    gMarioState->animList->bufTarget = &gTargetAnim;
    gMarioState->statusForCamera = &gPlayerCameraState[0];
    gMarioState->health = 0x880;
    gMarioState->marioObj->header.gfx.animInfo.curAnim = gMarioState->animList->bufTarget;
    gMarioState->marioObj->header.gfx.animInfo.curAnim->loopEnd = 100;
    gMarioState->marioObj->header.gfx.animInfo.animFrame = 0;
    gMarioState->marioObj->header.gfx.animInfo.animFrameAccelAssist = 0;
    gMarioState->marioObj->header.gfx.animInfo.animAccel = 0x10000;
    gMarioState->marioObj->header.gfx.animInfo.animTimer = 0;
    gMarioObject->header.gfx.node.flags |= GRAPH_RENDER_HAS_ANIMATION;
    gMarioState->area->camera = &areaCamera;
    areaCamera.yaw = 0;
}

#include "graph_node.h"

Gfx *geo_mario_hand_foot_scaler(s32 callContext, struct GraphNode *node, UNUSED Mat4 *c);

EXPORT void ADDCALL step(s32 buttons, f32 stickX, f32 stickY) {
    struct GraphNodeGenerated handFootScalerNode;
    struct GraphNodeScale scaleNode;
    handFootScalerNode.fnNode.node.next = (struct GraphNode*)&scaleNode;

    gControllers[0].buttonPressed = buttons & ~gControllers[0].buttonDown;
    gControllers[0].buttonDown = buttons;

    gControllers[0].stickX = stickX * 64.0f;
    gControllers[0].stickY = stickY * 64.0f;
    gControllers[0].stickMag = 64.0f * sqrtf(stickX * stickX + stickY * stickY);

    surfacesUsed = 0;

    execute_mario_action(gCurrentObject);
    copy_mario_state_to_object();

    handFootScalerNode.parameter = 1;
    geo_mario_hand_foot_scaler(GEO_CONTEXT_RENDER, (struct GraphNode*)&handFootScalerNode, NULL);
    handFootScalerNode.parameter = 0;
    geo_mario_hand_foot_scaler(GEO_CONTEXT_RENDER, (struct GraphNode*)&handFootScalerNode, NULL);
    handFootScalerNode.parameter = 2;
    geo_mario_hand_foot_scaler(GEO_CONTEXT_RENDER, (struct GraphNode*)&handFootScalerNode, NULL);

    s32 hasAnimation = (gMarioObject->header.gfx.node.flags & GRAPH_RENDER_HAS_ANIMATION) != 0;
    if (gMarioObject->header.gfx.animInfo.curAnim != NULL) {
        geo_set_animation_globals(&gMarioObject->header.gfx.animInfo, hasAnimation);
    }

    gGlobalTimer++;
    gAreaUpdateCounter++;
}

EXPORT void ADDCALL getMarioPosition(Vec3f pos) {
    pos[0] = gMarioObject->header.gfx.pos[0];
    pos[1] = gMarioObject->header.gfx.pos[1];
    pos[2] = gMarioObject->header.gfx.pos[2];
}

EXPORT void ADDCALL setMarioPosition(Vec3f pos) {
    gMarioState->pos[0] = pos[0];
    gMarioState->pos[1] = pos[1];
    gMarioState->pos[2] = pos[2];
}

EXPORT void ADDCALL getMarioVelocity(Vec3f vel) {
    vel[0] = gMarioState->vel[0];
    vel[1] = gMarioState->vel[1];
    vel[2] = gMarioState->vel[2];
}

EXPORT void ADDCALL setMarioVelocity(Vec3f pos) {
    gMarioState->vel[0] = pos[0];
    gMarioState->vel[1] = pos[1];
    gMarioState->vel[2] = pos[2];
}

EXPORT void ADDCALL getMarioRotation(Vec3f rot) {
    rot[0] = gMarioObject->header.gfx.angle[0] * (180.0f / 32768.0f);
    rot[1] = gMarioObject->header.gfx.angle[1] * (180.0f / 32768.0f);
    rot[2] = gMarioObject->header.gfx.angle[2] * (180.0f / 32768.0f);
}

EXPORT void ADDCALL setMarioRotation(Vec3f rot) {
    gMarioState->faceAngle[0] = (s16)(rot[0] * (32768.0f / 180.0f));
    gMarioState->faceAngle[1] = (s16)(rot[1] * (32768.0f / 180.0f));
    gMarioState->faceAngle[2] = (s16)(rot[2] * (32768.0f / 180.0f));
}

EXPORT void ADDCALL getMarioScale(Vec3f scale) {
    scale[0] = gMarioObject->header.gfx.scale[0];
    scale[1] = gMarioObject->header.gfx.scale[1];
    scale[2] = gMarioObject->header.gfx.scale[2];
}

EXPORT void ADDCALL getMarioTorsoRotation(Vec3f out) {
    struct MarioBodyState *bodyState = &gBodyStates[0];
    s32 action = gMarioState->action;
    if (action != ACT_BUTT_SLIDE &&
        action != ACT_HOLD_BUTT_SLIDE &&
        action != ACT_WALKING &&
        action != ACT_RIDING_SHELL_GROUND) {
        vec3s_copy(bodyState->torsoAngle, gVec3sZero);
    }
    out[0] = (180.0f / 32768.0f) * bodyState->torsoAngle[1];
    out[1] = (180.0f / 32768.0f) * bodyState->torsoAngle[2];
    out[2] = (180.0f / 32768.0f) * bodyState->torsoAngle[0];
}

EXPORT struct MarioState *ADDCALL getMarioState() {
    return gMarioState;
}

EXPORT s32 ADDCALL getMarioAnimIndex() {
    return gMarioObject->header.gfx.animInfo.animID;
}

EXPORT s32 ADDCALL getMarioAnimFrame() {
    return gMarioObject->header.gfx.animInfo.animFrame;
}

EXPORT void ADDCALL setMarioHealth(s32 health) {
    gMarioState->health = health;
}

EXPORT void ADDCALL setCameraYaw(f32 angle) {
    gMarioState->area->camera->yaw = angle * (32768.0f / 180.0f);
}

struct AnimData {
    Vec3f rootTranslation;
    Vec3f boneRotations[20];
};

EXPORT void ADDCALL getMarioAnimData(struct AnimData *out) {
    if (gMarioObject->header.gfx.animInfo.curAnim != NULL) {
        u16 *animIndices = (u16*)gMarioObject->header.gfx.animInfo.curAnim->index;
        s16 *animValues = (s16*)gMarioObject->header.gfx.animInfo.curAnim->values;
        s32 curFrame = gMarioObject->header.gfx.animInfo.animFrame;
        // f32 animTranslationScale = gMarioObject->header.gfx.animInfo.animYTrans / (f32)gMarioObject->header.gfx.animInfo.curAnim->animYTransDivisor;
        int i;
        if (animIndices != NULL && animValues != NULL) {
            out->rootTranslation[0] = animValues[retrieve_animation_index(curFrame, &animIndices)];
            out->rootTranslation[1] = animValues[retrieve_animation_index(curFrame, &animIndices)];
            out->rootTranslation[2] = animValues[retrieve_animation_index(curFrame, &animIndices)];
            for (i = 0; i < 20; i++) {
                out->boneRotations[i][0] = (180.0f / 32768.0f) * animValues[retrieve_animation_index(curFrame, &animIndices)];
                out->boneRotations[i][1] = (180.0f / 32768.0f) * animValues[retrieve_animation_index(curFrame, &animIndices)];
                out->boneRotations[i][2] = (180.0f / 32768.0f) * animValues[retrieve_animation_index(curFrame, &animIndices)];
            }
        }
    }
}
