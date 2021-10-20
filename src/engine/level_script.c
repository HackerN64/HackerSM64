#include <ultra64.h>
#ifdef NO_SEGMENTED_MEMORY
#include <string.h>
#endif

#include "sm64.h"
#include "audio/external.h"
#include "buffers/framebuffers.h"
#include "buffers/zbuffer.h"
#include "game/area.h"
#include "game/game_init.h"
#include "game/mario.h"
#include "game/memory.h"
#include "game/object_helpers.h"
#include "game/object_list_processor.h"
#include "game/save_file.h"
#include "game/sound_init.h"
#include "goddard/renderer.h"
#include "geo_layout.h"
#include "graph_node.h"
#include "level_script.h"
#include "level_misc_macros.h"
#include "level_commands.h"
#include "math_util.h"
#include "surface_collision.h"
#include "surface_load.h"
#include "string.h"
#include "game/puppycam2.h"
#include "game/puppyprint.h"
#include "game/puppylights.h"

#include "config.h"

#define NUM_PAINTINGS 45

#define CMD_GET(type, offset) (*(type *) (CMD_PROCESS_OFFSET(offset) + (u8 *) sCurrentCmd))

// These are equal
#define CMD_NEXT ((struct LevelCommand *) ((u8 *) sCurrentCmd + (sCurrentCmd->size << CMD_SIZE_SHIFT)))
#define NEXT_CMD ((struct LevelCommand *) ((sCurrentCmd->size << CMD_SIZE_SHIFT) + (u8 *) sCurrentCmd))

struct LevelCommand {
    /*00*/ u8 type;
    /*01*/ u8 size;
    /*02*/ // variable sized argument data
};

enum ScriptStatus {
    SCRIPT_RUNNING =  1,
    SCRIPT_PAUSED  =  0,
    SCRIPT_PAUSED2 = -1
};

static uintptr_t sStack[NUM_TLB_SEGMENTS];

static struct AllocOnlyPool *sLevelPool = NULL;

static u16 sDelayFrames  = 0;
static u16 sDelayFrames2 = 0;

static s16 sCurrAreaIndex = -1;

static uintptr_t *sStackTop  = sStack;
static uintptr_t *sStackBase = NULL;

static s16 sScriptStatus;
static s32 sRegister;
static struct LevelCommand *sCurrentCmd;

static s32 eval_script_op(s8 op, s32 arg) {
    s32 result = FALSE;
    switch (op) {
        case OP_AND:  result =  (sRegister &  arg); break;
        case OP_NAND: result = !(sRegister &  arg); break;
        case OP_EQ:   result =  (sRegister == arg); break;
        case OP_NEQ:  result =  (sRegister != arg); break;
        case OP_LT:   result =  (sRegister <  arg); break;
        case OP_LEQ:  result =  (sRegister <= arg); break;
        case OP_GT:   result =  (sRegister >  arg); break;
        case OP_GEQ:  result =  (sRegister >= arg); break;
    }
    return result;
}

static void level_cmd_load_and_execute(void) {
    main_pool_push_state();
    load_segment(CMD_GET(s16, 2), CMD_GET(void *, 4), CMD_GET(void *, 8), MEMORY_POOL_LEFT, CMD_GET(void *, 16), CMD_GET(void *, 20));

    *sStackTop++ = (uintptr_t) NEXT_CMD;
    *sStackTop++ = (uintptr_t) sStackBase;
    sStackBase   = sStackTop;

    sCurrentCmd = segmented_to_virtual(CMD_GET(void *, 12));
}

static void level_cmd_exit_and_execute(void) {
    void *targetAddr = CMD_GET(void *, 12);

    main_pool_pop_state();
    main_pool_push_state();

    load_segment(CMD_GET(s16, 2), CMD_GET(void *, 4), CMD_GET(void *, 8),
            MEMORY_POOL_LEFT, CMD_GET(void *, 16), CMD_GET(void *, 20));

    sStackTop = sStackBase;
    sCurrentCmd = segmented_to_virtual(targetAddr);
}

static void level_cmd_exit(void) {
    main_pool_pop_state();

    sStackTop = sStackBase;
    sStackBase = (uintptr_t *) *(--sStackTop);
    sCurrentCmd = (struct LevelCommand *) *(--sStackTop);
}

static void level_cmd_sleep(void) {
    sScriptStatus = SCRIPT_PAUSED;

    if (sDelayFrames == 0) {
        sDelayFrames = CMD_GET(s16, 2);
    } else if (--sDelayFrames == 0) {
        sCurrentCmd = CMD_NEXT;
        sScriptStatus = SCRIPT_RUNNING;
    }
}

static void level_cmd_sleep2(void) {
    sScriptStatus = SCRIPT_PAUSED2;

    if (sDelayFrames2 == 0) {
        sDelayFrames2 = CMD_GET(s16, 2);
    } else if (--sDelayFrames2 == 0) {
        sCurrentCmd = CMD_NEXT;
        sScriptStatus = SCRIPT_RUNNING;
    }
}

static void level_cmd_jump(void) {
    sCurrentCmd = segmented_to_virtual(CMD_GET(void *, 4));
}

static void level_cmd_jump_and_link(void) {
    *sStackTop++ = (uintptr_t) NEXT_CMD;
    sCurrentCmd = segmented_to_virtual(CMD_GET(void *, 4));
}

static void level_cmd_return(void) {
    sCurrentCmd = (struct LevelCommand *) *(--sStackTop);
}

static void level_cmd_jump_and_link_push_arg(void) {
    *sStackTop++ = (uintptr_t) NEXT_CMD;
    *sStackTop++ = CMD_GET(s16, 2);
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_jump_repeat(void) {
    s32 val = *(sStackTop - 1);

    if (val == 0) {
        sCurrentCmd = (struct LevelCommand *) *(sStackTop - 2);
    } else if (--val != 0) {
        *(sStackTop - 1) = val;
        sCurrentCmd = (struct LevelCommand *) *(sStackTop - 2);
    } else {
        sCurrentCmd = CMD_NEXT;
        sStackTop -= 2;
    }
}

static void level_cmd_loop_begin(void) {
    *sStackTop++ = (uintptr_t) NEXT_CMD;
    *sStackTop++ = 0;
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_loop_until(void) {
    if (eval_script_op(CMD_GET(u8, 2), CMD_GET(s32, 4))) {
        sCurrentCmd = CMD_NEXT;
        sStackTop -= 2;
    } else {
        sCurrentCmd = (struct LevelCommand *) *(sStackTop - 2);
    }
}

static void level_cmd_jump_if(void) {
    if (eval_script_op(CMD_GET(u8, 2), CMD_GET(s32, 4))) {
        sCurrentCmd = segmented_to_virtual(CMD_GET(void *, 8));
    } else {
        sCurrentCmd = CMD_NEXT;
    }
}

static void level_cmd_jump_and_link_if(void) {
    if (eval_script_op(CMD_GET(u8, 2), CMD_GET(s32, 4))) {
        *sStackTop++ = (uintptr_t) NEXT_CMD;
        sCurrentCmd = segmented_to_virtual(CMD_GET(void *, 8));
    } else {
        sCurrentCmd = CMD_NEXT;
    }
}

static void level_cmd_skip_if(void) {
    if (!eval_script_op(CMD_GET(u8, 2), CMD_GET(s32, 4))) {
        do {
            sCurrentCmd = CMD_NEXT;
        } while ((sCurrentCmd->type == LEVEL_CMD_SKIP) || (sCurrentCmd->type == LEVEL_CMD_SKIPPABLE_NOP));
    }

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_skip(void) {
    do {
        sCurrentCmd = CMD_NEXT;
    } while (sCurrentCmd->type == LEVEL_CMD_SKIPPABLE_NOP);

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_skippable_nop(void) {
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_call(void) {
    typedef s32 (*Func)(s16, s32);
    Func func = CMD_GET(Func, 4);
    sRegister = func(CMD_GET(s16, 2), sRegister);
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_call_loop(void) {
    typedef s32 (*Func)(s16, s32);
    Func func = CMD_GET(Func, 4);
    sRegister = func(CMD_GET(s16, 2), sRegister);

    if (sRegister == 0) {
        sScriptStatus = SCRIPT_PAUSED;
    } else {
        sScriptStatus = SCRIPT_RUNNING;
        sCurrentCmd = CMD_NEXT;
    }
}

static void level_cmd_set_register(void) {
    sRegister = CMD_GET(s16, 2);
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_push_pool_state(void) {
    main_pool_push_state();
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_pop_pool_state(void) {
    main_pool_pop_state();
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_load_to_fixed_address(void) {
    load_to_fixed_pool_addr(CMD_GET(void *, 4), CMD_GET(void *, 8), CMD_GET(void *, 12));
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_load_raw(void) {
    load_segment(CMD_GET(s16, 2), CMD_GET(void *, 4), CMD_GET(void *, 8),
            MEMORY_POOL_LEFT, CMD_GET(void *, 12), CMD_GET(void *, 16));
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_load_yay0(void) {
    load_segment_decompress(CMD_GET(s16, 2), CMD_GET(void *, 4), CMD_GET(void *, 8));
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_load_mario_head(void) {
#ifdef KEEP_MARIO_HEAD
    // TODO: Fix these hardcoded sizes
    void *addr = main_pool_alloc(DOUBLE_SIZE_ON_64_BIT(0xE1000), MEMORY_POOL_LEFT);
    if (addr != NULL) {
        gdm_init(addr, DOUBLE_SIZE_ON_64_BIT(0xE1000));
        gd_add_to_heap(gZBuffer, sizeof(gZBuffer)); // 0x25800
        gd_add_to_heap(gFramebuffer0, (3 * sizeof(gFramebuffer0))); // 0x70800
        gdm_setup();
        gdm_maketestdl(CMD_GET(s16, 2));
    }
#endif
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_load_yay0_texture(void) {
    load_segment_decompress_heap(CMD_GET(s16, 2), CMD_GET(void *, 4), CMD_GET(void *, 8));
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_change_area_skybox(void) {
    u8 areaCheck = CMD_GET(s16, 2);
    gAreaSkyboxStart[areaCheck - 1] = CMD_GET(void *, 4);
    gAreaSkyboxEnd[areaCheck - 1] = CMD_GET(void *, 8);
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_init_level(void) {
    init_graph_node_start(NULL, (struct GraphNodeStart *) &gObjParentGraphNode);
    clear_objects();
    clear_areas();
    main_pool_push_state();
    for (u8 clearPointers = 0; clearPointers < AREA_COUNT; clearPointers++) {
        gAreaSkyboxStart[clearPointers] = 0;
        gAreaSkyboxEnd[clearPointers] = 0;
    }

    sCurrentCmd = CMD_NEXT;
}

extern s32 gTlbEntries;
extern u8 gTlbSegments[NUM_TLB_SEGMENTS];

// This clears all the temporary bank TLB maps. group0, common1 and behavourdata are always loaded,
// and they're also loaded first, so that means we just leave the first 3 indexes mapped.
void unmap_tlbs(void) {
    s32 i;
    for (i = 0; i < NUM_TLB_SEGMENTS; i++) {
        if (gTlbSegments[i]/* && i != 0x17 && i != 0x16 && i != 0x13*/) {
            while (gTlbSegments[i] > 0) {
                osUnmapTLB(gTlbEntries);
                gTlbSegments[i]--;
                gTlbEntries--;
            }
        }
    }
}

static void level_cmd_clear_level(void) {
    clear_objects();
    clear_area_graph_nodes();
    clear_areas();
    main_pool_pop_state();
    unmap_tlbs();

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_alloc_level_pool(void) {
    if (sLevelPool == NULL) {
        sLevelPool = alloc_only_pool_init((main_pool_available() - sizeof(struct AllocOnlyPool)), MEMORY_POOL_LEFT);
    }

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_free_level_pool(void) {
    s32 i;

    alloc_only_pool_resize(sLevelPool, sLevelPool->usedSpace);
    sLevelPool = NULL;

    for (i = 0; i < AREA_COUNT; i++) {
        if (gAreaData[i].terrainData != NULL) {
            alloc_surface_pools();
            break;
        }
    }

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_begin_area(void) {
    u8 areaIndex = CMD_GET(u8, 2);
    void *geoLayoutAddr = CMD_GET(void *, 4);

    if (areaIndex < AREA_COUNT) {
        struct GraphNodeRoot *screenArea =
            (struct GraphNodeRoot *) process_geo_layout(sLevelPool, geoLayoutAddr);
        struct GraphNodeCamera *node = (struct GraphNodeCamera *) screenArea->views[0];

        sCurrAreaIndex = areaIndex;
        screenArea->areaIndex = areaIndex;
        gAreas[areaIndex].graphNode = screenArea;

        if (node != NULL) {
            gAreas[areaIndex].camera = (struct Camera *) node->config.camera;
        } else {
            gAreas[areaIndex].camera = NULL;
        }
    }

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_end_area(void) {
    sCurrAreaIndex = -1;
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_load_model_from_dl(void) {
    ModelID16 model = CMD_GET(ModelID16, 0xA);
    s16 layer = CMD_GET(u16, 0x8);
    void *dl_ptr = CMD_GET(void *, 4);

    if (model < MODEL_ID_COUNT) {
        gLoadedGraphNodes[model] =
            (struct GraphNode *) init_graph_node_display_list(sLevelPool, 0, layer, dl_ptr);
    }

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_load_model_from_geo(void) {
    ModelID16 model = CMD_GET(ModelID16, 2);
    void *geo = CMD_GET(void *, 4);

    if (model < MODEL_ID_COUNT) {
        gLoadedGraphNodes[model] = process_geo_layout(sLevelPool, geo);
    }

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_23(void) {
    ModelID16 model = (CMD_GET(ModelID16, 2) & 0x0FFF);
    s16 layer = (((u16)CMD_GET(s16, 2)) >> 12);
    void *dl = CMD_GET(void *, 4);
    s32 scale = CMD_GET(s32, 8);

    if (model < MODEL_ID_COUNT) {
        // GraphNodeScale has a GraphNode at the top. This
        // is being stored to the array, so cast the pointer.
        gLoadedGraphNodes[model] = (struct GraphNode *) init_graph_node_scale(sLevelPool, 0, layer, dl, scale);
    }

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_init_mario(void) {
    vec3_zero(gMarioSpawnInfo->startPos);
    vec3_zero(gMarioSpawnInfo->startAngle);

    gMarioSpawnInfo->activeAreaIndex = -1;
    gMarioSpawnInfo->areaIndex = 0;
    gMarioSpawnInfo->behaviorArg = CMD_GET(u32, 4);
    gMarioSpawnInfo->behaviorScript = CMD_GET(void *, 8);
    gMarioSpawnInfo->model = gLoadedGraphNodes[CMD_GET(ModelID16, 0x2)]; // u8, 3?
    gMarioSpawnInfo->next = NULL;

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_place_object(void) {
    u8 val7 = (1 << (gCurrActNum - 1));
    ModelID16 model;
    struct SpawnInfo *spawnInfo;

    if (sCurrAreaIndex != -1 && ((CMD_GET(u8, 2) & val7) || CMD_GET(u8, 2) == 0x1F)) {
        model = CMD_GET(u32, 0x18);
        spawnInfo = alloc_only_pool_alloc(sLevelPool, sizeof(struct SpawnInfo));

        spawnInfo->startPos[0] = CMD_GET(s16, 4);
        spawnInfo->startPos[1] = CMD_GET(s16, 6);
        spawnInfo->startPos[2] = CMD_GET(s16, 8);

        spawnInfo->startAngle[0] = CMD_GET(s16, 10) * 0x8000 / 180;
        spawnInfo->startAngle[1] = CMD_GET(s16, 12) * 0x8000 / 180;
        spawnInfo->startAngle[2] = CMD_GET(s16, 14) * 0x8000 / 180;

        spawnInfo->areaIndex = sCurrAreaIndex;
        spawnInfo->activeAreaIndex = sCurrAreaIndex;

        spawnInfo->behaviorArg = CMD_GET(u32, 16);
        spawnInfo->behaviorScript = CMD_GET(void *, 20);
        spawnInfo->model = gLoadedGraphNodes[model];
        spawnInfo->next = gAreas[sCurrAreaIndex].objectSpawnInfos;

        gAreas[sCurrAreaIndex].objectSpawnInfos = spawnInfo;
    }

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_create_warp_node(void) {
    if (sCurrAreaIndex != -1) {
        struct ObjectWarpNode *warpNode =
            alloc_only_pool_alloc(sLevelPool, sizeof(struct ObjectWarpNode));

        warpNode->node.id = CMD_GET(u8, 2);
        warpNode->node.destLevel = CMD_GET(u8, 3) + CMD_GET(u8, 6);
        warpNode->node.destArea = CMD_GET(u8, 4);
        warpNode->node.destNode = CMD_GET(u8, 5);

        warpNode->object = NULL;

        warpNode->next = gAreas[sCurrAreaIndex].warpNodes;
        gAreas[sCurrAreaIndex].warpNodes = warpNode;
    }

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_create_instant_warp(void) {
    s32 i;
    struct InstantWarp *warp;

    if (sCurrAreaIndex != -1) {
        if (gAreas[sCurrAreaIndex].instantWarps == NULL) {
            gAreas[sCurrAreaIndex].instantWarps =
                alloc_only_pool_alloc(sLevelPool, INSTANT_WARP_INDEX_STOP * sizeof(struct InstantWarp));

            for (i = INSTANT_WARP_INDEX_START; i < INSTANT_WARP_INDEX_STOP; i++) {
                gAreas[sCurrAreaIndex].instantWarps[i].id = 0;
            }
        }

        warp = gAreas[sCurrAreaIndex].instantWarps + CMD_GET(u8, 2);

        warp[0].id = 1;
        warp[0].area = CMD_GET(u8, 3);

        warp[0].displacement[0] = CMD_GET(s16, 4);
        warp[0].displacement[1] = CMD_GET(s16, 6);
        warp[0].displacement[2] = CMD_GET(s16, 8);
    }

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_set_terrain_type(void) {
    if (sCurrAreaIndex != -1) {
        gAreas[sCurrAreaIndex].terrainType |= CMD_GET(s16, 2);
    }

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_create_painting_warp_node(void) {
    s32 i;
    struct WarpNode *node;

    if (sCurrAreaIndex != -1) {
        if (gAreas[sCurrAreaIndex].paintingWarpNodes == NULL) {
            gAreas[sCurrAreaIndex].paintingWarpNodes =
                alloc_only_pool_alloc(sLevelPool, (NUM_PAINTINGS * sizeof(struct WarpNode)));

            for (i = 0; i < NUM_PAINTINGS; i++) {
                gAreas[sCurrAreaIndex].paintingWarpNodes[i].id = 0;
            }
        }

        node = &gAreas[sCurrAreaIndex].paintingWarpNodes[CMD_GET(u8, 2)];

        node->id = 1;
        node->destLevel = (CMD_GET(u8, 3) + CMD_GET(u8, 6));
        node->destArea = CMD_GET(u8, 4);
        node->destNode = CMD_GET(u8, 5);
    }

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_3A(void) {
    struct UnusedArea28 *val4;

    if (sCurrAreaIndex != -1) {
        if ((val4 = gAreas[sCurrAreaIndex].unused) == NULL) {
            val4 = gAreas[sCurrAreaIndex].unused =
                alloc_only_pool_alloc(sLevelPool, sizeof(struct UnusedArea28));
        }

        val4->unk00 = CMD_GET(s16,  2);
        val4->unk02 = CMD_GET(s16,  4);
        val4->unk04 = CMD_GET(s16,  6);
        val4->unk06 = CMD_GET(s16,  8);
        val4->unk08 = CMD_GET(s16, 10);
    }

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_create_whirlpool(void) {
    struct Whirlpool *whirlpool;
    s32 index = CMD_GET(u8, 2);
    s32 beatBowser2 = ((save_file_get_flags() & (SAVE_FLAG_HAVE_KEY_2 | SAVE_FLAG_UNLOCKED_UPSTAIRS_DOOR)) != 0);

    if (CMD_GET(u8, 3) == WHIRLPOOL_COND_ALWAYS
    || (CMD_GET(u8, 3) == WHIRLPOOL_COND_BOWSER2_NOT_BEATEN   && !beatBowser2)
    || (CMD_GET(u8, 3) == WHIRLPOOL_COND_BOWSER2_BEATEN       &&  beatBowser2)
    || (CMD_GET(u8, 3) == WHIRLPOOL_COND_AT_LEAST_SECOND_STAR && gCurrActNum >= 2)) {
        if (sCurrAreaIndex != -1 && index < 2) {
            if ((whirlpool = gAreas[sCurrAreaIndex].whirlpools[index]) == NULL) {
                whirlpool = alloc_only_pool_alloc(sLevelPool, sizeof(struct Whirlpool));
                gAreas[sCurrAreaIndex].whirlpools[index] = whirlpool;
            }

            vec3s_set(whirlpool->pos, CMD_GET(s16, 4), CMD_GET(s16, 6), CMD_GET(s16, 8));
            whirlpool->strength = CMD_GET(s16, 10);
        }
    }

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_set_blackout(void) {
    osViBlack(CMD_GET(u8, 2));
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_set_gamma(void) {
    osViSetSpecialFeatures(CMD_GET(u8, 2) == 0 ? OS_VI_GAMMA_OFF : OS_VI_GAMMA_ON);
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_set_terrain_data(void) {
    if (sCurrAreaIndex != -1) {
#ifndef NO_SEGMENTED_MEMORY
        gAreas[sCurrAreaIndex].terrainData = segmented_to_virtual(CMD_GET(void *, 4));
#else
        // The game modifies the terrain data and must be reset upon level reload.
        Collision *data = segmented_to_virtual(CMD_GET(void *, 4));
        u32 size = get_area_terrain_size(data) * sizeof(Collision);
        gAreas[sCurrAreaIndex].terrainData = alloc_only_pool_alloc(sLevelPool, size);
        memcpy(gAreas[sCurrAreaIndex].terrainData, data, size);
#endif
    }
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_set_rooms(void) {
    if (sCurrAreaIndex != -1) {
        gAreas[sCurrAreaIndex].surfaceRooms = segmented_to_virtual(CMD_GET(void *, 4));
    }
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_set_macro_objects(void) {
    if (sCurrAreaIndex != -1) {
#ifndef NO_SEGMENTED_MEMORY
        gAreas[sCurrAreaIndex].macroObjects = segmented_to_virtual(CMD_GET(void *, 4));
#else
        // The game modifies the macro object data (for example marking coins as taken),
        // so it must be reset when the level reloads.
        MacroObject *data = segmented_to_virtual(CMD_GET(void *, 4));
        s32 len = 0;
        while (data[len++] != MACRO_OBJECT_END()) {
            len += 4;
        }
        gAreas[sCurrAreaIndex].macroObjects = alloc_only_pool_alloc(sLevelPool, len * sizeof(MacroObject));
        memcpy(gAreas[sCurrAreaIndex].macroObjects, data, len * sizeof(MacroObject));
#endif
    }
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_load_area(void) {
    s16 areaIndex = CMD_GET(u8, 2);

    stop_sounds_in_continuous_banks();
    load_area(areaIndex);

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_unload_area(void) {
    unload_area();
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_set_mario_start_pos(void) {
    gMarioSpawnInfo->areaIndex = CMD_GET(u8, 2);

#if IS_64_BIT
    vec3s_set(gMarioSpawnInfo->startPos, CMD_GET(s16, 6), CMD_GET(s16, 8), CMD_GET(s16, 10));
#else
    vec3s_copy(gMarioSpawnInfo->startPos, CMD_GET(Vec3s, 6));
#endif
    vec3s_set(gMarioSpawnInfo->startAngle, 0, DEGREES(CMD_GET(s16, 4)), 0);

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_2C(void) {
    unload_mario_area();
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_2D(void) {
    area_update_objects();
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_set_transition(void) {
    if (gCurrentArea != NULL) {
        play_transition(CMD_GET(u8, 2), CMD_GET(u8, 3), CMD_GET(u8, 4), CMD_GET(u8, 5), CMD_GET(u8, 6));
    }
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_nop(void) {
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_show_dialog(void) {
    if (sCurrAreaIndex != -1) {
        if (CMD_GET(u8, 2) < 2) {
            gAreas[sCurrAreaIndex].dialog[CMD_GET(u8, 2)] = CMD_GET(u8, 3);
        }
    }
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_set_music(void) {
    if (sCurrAreaIndex != -1) {
        gAreas[sCurrAreaIndex].musicParam = CMD_GET(s16, 2);
        gAreas[sCurrAreaIndex].musicParam2 = CMD_GET(s16, 4);
    }
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_set_menu_music(void) {
    set_background_music(0, CMD_GET(s16, 2), 0);
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_38(void) {
    fadeout_music(CMD_GET(s16, 2));
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_get_or_set_var(void) {
    if (CMD_GET(u8, 2) == OP_SET) {
        switch (CMD_GET(u8, 3)) {
            case VAR_CURR_SAVE_FILE_NUM: gCurrSaveFileNum = sRegister; break;
            case VAR_CURR_COURSE_NUM:    gCurrCourseNum   = sRegister; break;
            case VAR_CURR_ACT_NUM:       gCurrActNum      = sRegister; break;
            case VAR_CURR_LEVEL_NUM:     gCurrLevelNum    = sRegister; break;
            case VAR_CURR_AREA_INDEX:    gCurrAreaIndex   = sRegister; break;
        }
    } else {
        switch (CMD_GET(u8, 3)) {
            case VAR_CURR_SAVE_FILE_NUM: sRegister = gCurrSaveFileNum; break;
            case VAR_CURR_COURSE_NUM:    sRegister = gCurrCourseNum;   break;
            case VAR_CURR_ACT_NUM:       sRegister = gCurrActNum;      break;
            case VAR_CURR_LEVEL_NUM:     sRegister = gCurrLevelNum;    break;
            case VAR_CURR_AREA_INDEX:    sRegister = gCurrAreaIndex;   break;
        }
    }

    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_puppyvolume(void) {
#ifdef PUPPYCAM
    if ((sPuppyVolumeStack[gPuppyVolumeCount] = mem_pool_alloc(gPuppyMemoryPool, sizeof(struct sPuppyVolume))) == NULL) {
        sCurrentCmd = CMD_NEXT;
        gPuppyError |= PUPPY_ERROR_POOL_FULL;
#if PUPPYPRINT_DEBUG
        append_puppyprint_log("Puppycamera volume allocation failed.");
#endif
        return;
    }

    sPuppyVolumeStack[gPuppyVolumeCount]->pos[0] = CMD_GET(s16, 2);
    sPuppyVolumeStack[gPuppyVolumeCount]->pos[1] = CMD_GET(s16, 4);
    sPuppyVolumeStack[gPuppyVolumeCount]->pos[2] = CMD_GET(s16, 6);

    sPuppyVolumeStack[gPuppyVolumeCount]->radius[0] = CMD_GET(s16, 8);
    sPuppyVolumeStack[gPuppyVolumeCount]->radius[1] = CMD_GET(s16, 10);
    sPuppyVolumeStack[gPuppyVolumeCount]->radius[2] = CMD_GET(s16, 12);

    sPuppyVolumeStack[gPuppyVolumeCount]->rot = CMD_GET(s16, 14);

    sPuppyVolumeStack[gPuppyVolumeCount]->func = CMD_GET(void *, 16);
    sPuppyVolumeStack[gPuppyVolumeCount]->angles = segmented_to_virtual(CMD_GET(void *, 20));

    sPuppyVolumeStack[gPuppyVolumeCount]->flagsAdd = CMD_GET(s32, 24);
    sPuppyVolumeStack[gPuppyVolumeCount]->flagsRemove = CMD_GET(s32, 28);

    sPuppyVolumeStack[gPuppyVolumeCount]->flagPersistance = CMD_GET(u8, 32);

    sPuppyVolumeStack[gPuppyVolumeCount]->shape = CMD_GET(u8, 33);
    sPuppyVolumeStack[gPuppyVolumeCount]->room = CMD_GET(s16, 34);
    sPuppyVolumeStack[gPuppyVolumeCount]->area = sCurrAreaIndex;

    gPuppyVolumeCount++;
#endif
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_puppylight_environment(void) {
#ifdef PUPPYLIGHTS
    Lights1 temp = gdSPDefLights1(CMD_GET(u8, 2), CMD_GET(u8, 3), CMD_GET(u8, 4), CMD_GET(u8, 5), CMD_GET(u8, 6), CMD_GET(u8, 7), CMD_GET(u8, 8), CMD_GET(u8, 9), CMD_GET(u8, 10));

    memcpy(&gLevelLight, &temp, sizeof(Lights1));
    levelAmbient = TRUE;
#endif
    sCurrentCmd = CMD_NEXT;
}

static void level_cmd_puppylight_node(void) {
#ifdef PUPPYLIGHTS
    if ((gPuppyLights[gNumLights] = mem_pool_alloc(gLightsPool, sizeof(struct PuppyLight))) == NULL) {
#if PUPPYPRINT_DEBUG
        append_puppyprint_log("Puppylight allocation failed.");
#endif
        sCurrentCmd = CMD_NEXT;
        return;
    }

    gPuppyLights[gNumLights]->rgba[0] = CMD_GET(u8, 2);
    gPuppyLights[gNumLights]->rgba[1] = CMD_GET(u8, 3);
    gPuppyLights[gNumLights]->rgba[2] = CMD_GET(u8, 4);
    gPuppyLights[gNumLights]->rgba[3] = CMD_GET(u8, 5);

    gPuppyLights[gNumLights]->pos[0][0] = CMD_GET(s16, 6);
    gPuppyLights[gNumLights]->pos[0][1] = CMD_GET(s16, 8);
    gPuppyLights[gNumLights]->pos[0][2] = CMD_GET(s16, 10);

    gPuppyLights[gNumLights]->pos[1][0] = CMD_GET(s16, 12);
    gPuppyLights[gNumLights]->pos[1][1] = CMD_GET(s16, 14);
    gPuppyLights[gNumLights]->pos[1][2] = CMD_GET(s16, 16);
    gPuppyLights[gNumLights]->yaw = CMD_GET(s16, 18);

    gPuppyLights[gNumLights]->epicentre = CMD_GET(u8, 20);
    gPuppyLights[gNumLights]->flags = CMD_GET(u8, 21);
    gPuppyLights[gNumLights]->active = TRUE;
    gPuppyLights[gNumLights]->area = sCurrAreaIndex;
    gPuppyLights[gNumLights]->room = CMD_GET(s16, 22);

    gNumLights++;

#endif
    sCurrentCmd = CMD_NEXT;
}

static void (*LevelScriptJumpTable[])(void) = {
    /*00*/ level_cmd_load_and_execute,
    /*01*/ level_cmd_exit_and_execute,
    /*02*/ level_cmd_exit,
    /*03*/ level_cmd_sleep,
    /*04*/ level_cmd_sleep2,
    /*05*/ level_cmd_jump,
    /*06*/ level_cmd_jump_and_link,
    /*07*/ level_cmd_return,
    /*08*/ level_cmd_jump_and_link_push_arg,
    /*09*/ level_cmd_jump_repeat,
    /*0A*/ level_cmd_loop_begin,
    /*0B*/ level_cmd_loop_until,
    /*0C*/ level_cmd_jump_if,
    /*0D*/ level_cmd_jump_and_link_if,
    /*0E*/ level_cmd_skip_if,
    /*0F*/ level_cmd_skip,
    /*10*/ level_cmd_skippable_nop,
    /*11*/ level_cmd_call,
    /*12*/ level_cmd_call_loop,
    /*13*/ level_cmd_set_register,
    /*14*/ level_cmd_push_pool_state,
    /*15*/ level_cmd_pop_pool_state,
    /*16*/ level_cmd_load_to_fixed_address,
    /*17*/ level_cmd_load_raw,
    /*18*/ level_cmd_load_yay0,
    /*19*/ level_cmd_load_mario_head,
    /*1A*/ level_cmd_load_yay0_texture,
    /*1B*/ level_cmd_init_level,
    /*1C*/ level_cmd_clear_level,
    /*1D*/ level_cmd_alloc_level_pool,
    /*1E*/ level_cmd_free_level_pool,
    /*1F*/ level_cmd_begin_area,
    /*20*/ level_cmd_end_area,
    /*21*/ level_cmd_load_model_from_dl,
    /*22*/ level_cmd_load_model_from_geo,
    /*23*/ level_cmd_23,
    /*24*/ level_cmd_place_object,
    /*25*/ level_cmd_init_mario,
    /*26*/ level_cmd_create_warp_node,
    /*27*/ level_cmd_create_painting_warp_node,
    /*28*/ level_cmd_create_instant_warp,
    /*29*/ level_cmd_load_area,
    /*2A*/ level_cmd_unload_area,
    /*2B*/ level_cmd_set_mario_start_pos,
    /*2C*/ level_cmd_2C,
    /*2D*/ level_cmd_2D,
    /*2E*/ level_cmd_set_terrain_data,
    /*2F*/ level_cmd_set_rooms,
    /*30*/ level_cmd_show_dialog,
    /*31*/ level_cmd_set_terrain_type,
    /*32*/ level_cmd_nop,
    /*33*/ level_cmd_set_transition,
    /*34*/ level_cmd_set_blackout,
    /*35*/ level_cmd_set_gamma,
    /*36*/ level_cmd_set_music,
    /*37*/ level_cmd_set_menu_music,
    /*38*/ level_cmd_38,
    /*39*/ level_cmd_set_macro_objects,
    /*3A*/ level_cmd_3A,
    /*3B*/ level_cmd_create_whirlpool,
    /*3C*/ level_cmd_get_or_set_var,
    /*3D*/ level_cmd_puppyvolume,
    /*3E*/ level_cmd_change_area_skybox,
    /*3F*/ level_cmd_puppylight_environment,
    /*40*/ level_cmd_puppylight_node,
};

struct LevelCommand *level_script_execute(struct LevelCommand *cmd) {
    sScriptStatus = SCRIPT_RUNNING;
    sCurrentCmd = cmd;

    while (sScriptStatus == SCRIPT_RUNNING) {
        LevelScriptJumpTable[sCurrentCmd->type]();
    }

    init_rcp(CLEAR_ZBUFFER);
    render_game();
    end_master_display_list();
    alloc_display_list(0);

    return sCurrentCmd;
}
