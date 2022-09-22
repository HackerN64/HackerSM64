#include <PR/ultratypes.h>

#include "area.h"
#include "engine/math_util.h"
#include "game_init.h"
#include "gfx_dimensions.h"
#include "main.h"
#include "memory.h"
#include "print.h"
#include "rendering_graph_node.h"
#include "shadow.h"
#include "sm64.h"
#include "game_init.h"
#include "puppyprint.h"
#include "debug_box.h"
#include "level_update.h"
#include "behavior_data.h"
#include "string.h"
#include "color_presets.h"

#include "config.h"
#include "config/config_world.h"

#include <point_lights.h>

/**
 * This file contains the code that processes the scene graph for rendering.
 * The scene graph is responsible for drawing everything except the HUD / text boxes.
 * First the root of the scene graph is processed when geo_process_root
 * is called from level_script.c. The rest of the tree is traversed recursively
 * using the function geo_process_node_and_siblings, which switches over all
 * geo node types and calls a specialized function accordingly.
 * The types are defined in engine/graph_node.h
 *
 * The scene graph typically looks like:
 * - Root (viewport)
 *  - Master list
 *   - Ortho projection
 *    - Background (skybox)
 *  - Master list
 *   - Perspective
 *    - Camera
 *     - <area-specific display lists>
 *     - Object parent
 *      - <group with 240 object nodes>
 *  - Master list
 *   - Script node (Cannon overlay)
 *
 */

s16 gMatStackIndex;
ALIGNED16 Mat4 gMatStack[32];
ALIGNED16 Mtx *gMatStackFixed[32];
f32 sAspectRatio;

/**
 * Animation nodes have state in global variables, so this struct captures
 * the animation state so a 'context switch' can be made when rendering the
 * held object.
 */
struct GeoAnimState {
    /*0x00*/ u8 type;
    /*0x01*/ u8 enabled;
    /*0x02*/ s16 frame;
    /*0x04*/ f32 translationMultiplier;
    /*0x08*/ u16 *attribute;
    /*0x0C*/ s16 *data;
};

// For some reason, this is a GeoAnimState struct, but the current state consists
// of separate global variables. It won't match EU otherwise.
struct GeoAnimState gGeoTempState;

u8 gCurrAnimType;
u8 gCurrAnimEnabled;
s16 gCurrAnimFrame;
f32 gCurrAnimTranslationMultiplier;
u16 *gCurrAnimAttribute;
s16 *gCurrAnimData;

struct AllocOnlyPool *gDisplayListHeap;

struct RenderModeContainer {
    u32 modes[LAYER_COUNT];
};

/* Rendermode settings for cycle 1 for all 8 or 13 layers. */
struct RenderModeContainer renderModeTable_1Cycle[2] = { { {
        G_RM_OPA_SURF,                      // LAYER_FORCE
        G_RM_AA_OPA_SURF,                   // LAYER_OPAQUE
        G_RM_AA_OPA_SURF,                   // LAYER_OPAQUE_INTER
        G_RM_AA_OPA_SURF,                   // LAYER_OPAQUE_DECAL
        G_RM_AA_TEX_EDGE,                   // LAYER_ALPHA
#if SILHOUETTE
        G_RM_AA_TEX_EDGE | ZMODE_DEC,       // LAYER_ALPHA_DECAL
        G_RM_AA_OPA_SURF,                   // LAYER_SILHOUETTE_OPAQUE
        G_RM_AA_TEX_EDGE,                   // LAYER_SILHOUETTE_ALPHA
        G_RM_AA_OPA_SURF,                   // LAYER_OCCLUDE_SILHOUETTE_OPAQUE
        G_RM_AA_TEX_EDGE,                   // LAYER_OCCLUDE_SILHOUETTE_ALPHA
#endif
        G_RM_AA_XLU_SURF,                   // LAYER_TRANSPARENT_DECAL
        G_RM_AA_XLU_SURF,                   // LAYER_TRANSPARENT
        G_RM_AA_XLU_SURF,                   // LAYER_TRANSPARENT_INTER
    } },
    { {
        /* z-buffered */
        G_RM_ZB_OPA_SURF,                   // LAYER_FORCE
        G_RM_AA_ZB_OPA_SURF,                // LAYER_OPAQUE
        G_RM_AA_ZB_OPA_INTER,               // LAYER_OPAQUE_INTER
        G_RM_AA_ZB_OPA_DECAL,               // LAYER_OPAQUE_DECAL
        G_RM_AA_ZB_TEX_EDGE,                // LAYER_ALPHA
#if SILHOUETTE
        G_RM_AA_ZB_TEX_EDGE | ZMODE_DEC,    // LAYER_ALPHA_DECAL
        G_RM_AA_ZB_OPA_SURF,                // LAYER_SILHOUETTE_OPAQUE
        G_RM_AA_ZB_TEX_EDGE,                // LAYER_SILHOUETTE_ALPHA
        G_RM_AA_ZB_OPA_SURF,                // LAYER_OCCLUDE_SILHOUETTE_OPAQUE
        G_RM_AA_ZB_TEX_EDGE,                // LAYER_OCCLUDE_SILHOUETTE_ALPHA
#endif
        G_RM_AA_ZB_XLU_DECAL,               // LAYER_TRANSPARENT_DECAL
        G_RM_AA_ZB_XLU_SURF,                // LAYER_TRANSPARENT
        G_RM_AA_ZB_XLU_INTER,               // LAYER_TRANSPARENT_INTER
    } } };

/* Rendermode settings for cycle 2 for all 13 layers. */
struct RenderModeContainer renderModeTable_2Cycle[2] = { { {
        G_RM_OPA_SURF2,                     // LAYER_FORCE
        G_RM_AA_OPA_SURF2,                  // LAYER_OPAQUE
        G_RM_AA_OPA_SURF2,                  // LAYER_OPAQUE_INTER
        G_RM_AA_OPA_SURF2,                  // LAYER_OPAQUE_DECAL
        G_RM_AA_TEX_EDGE2,                  // LAYER_ALPHA
#if SILHOUETTE
        G_RM_AA_TEX_EDGE2 | ZMODE_DEC,      // LAYER_ALPHA_DECAL
        G_RM_AA_OPA_SURF2,                  // LAYER_SILHOUETTE_OPAQUE
        G_RM_AA_TEX_EDGE2,                  // LAYER_SILHOUETTE_ALPHA
        G_RM_AA_OPA_SURF2,                  // LAYER_OCCLUDE_SILHOUETTE_OPAQUE
        G_RM_AA_TEX_EDGE2,                  // LAYER_OCCLUDE_SILHOUETTE_ALPHA
#endif
        G_RM_AA_XLU_SURF2,                  // LAYER_TRANSPARENT_DECAL
        G_RM_AA_XLU_SURF2,                  // LAYER_TRANSPARENT
        G_RM_AA_XLU_SURF2,                  // LAYER_TRANSPARENT_INTER
    } },
    { {
        /* z-buffered */
        G_RM_ZB_OPA_SURF2,                  // LAYER_FORCE
        G_RM_AA_ZB_OPA_SURF2,               // LAYER_OPAQUE
        G_RM_AA_ZB_OPA_INTER2,              // LAYER_OPAQUE_INTER
        G_RM_AA_ZB_OPA_DECAL2,              // LAYER_OPAQUE_DECAL
        G_RM_AA_ZB_TEX_EDGE2,               // LAYER_ALPHA
#if SILHOUETTE
        G_RM_AA_ZB_TEX_EDGE2 | ZMODE_DEC,   // LAYER_ALPHA_DECAL
        G_RM_AA_ZB_OPA_SURF2,               // LAYER_SILHOUETTE_OPAQUE
        G_RM_AA_ZB_TEX_EDGE2,               // LAYER_SILHOUETTE_ALPHA
        G_RM_AA_ZB_OPA_SURF2,               // LAYER_OCCLUDE_SILHOUETTE_OPAQUE
        G_RM_AA_ZB_TEX_EDGE2,               // LAYER_OCCLUDE_SILHOUETTE_ALPHA
#endif
        G_RM_AA_ZB_XLU_DECAL2,              // LAYER_TRANSPARENT_DECAL
        G_RM_AA_ZB_XLU_SURF2,               // LAYER_TRANSPARENT
        G_RM_AA_ZB_XLU_INTER2,              // LAYER_TRANSPARENT_INTER
    } } };

ALIGNED16 struct GraphNodeRoot *gCurGraphNodeRoot = NULL;
ALIGNED16 struct GraphNodeMasterList *gCurGraphNodeMasterList = NULL;
ALIGNED16 struct GraphNodePerspective *gCurGraphNodeCamFrustum = NULL;
ALIGNED16 struct GraphNodeCamera *gCurGraphNodeCamera = NULL;
ALIGNED16 struct GraphNodeObject *gCurGraphNodeObject = NULL;
ALIGNED16 struct GraphNodeHeldObject *gCurGraphNodeHeldObject = NULL;
u16 gAreaUpdateCounter = 0;
LookAt* gCurLookAt;

#if SILHOUETTE
// AA_EN        Enable anti aliasing (not actually used for AA in this case).
// IM_RD        Enable reading coverage value.
// CLR_ON_CVG   Don't change the color unless coverage overflows. This helps prevent triangle overlap.
// CVG_DST_WRAP Wrap the coverage value on overflow.
// CVG_X_ALPHA  Coverage and alpha will be multiplied and both will be the same. This makes texture alpha work (eg. Wing Cap wings).
// FORCE_BL     Force Blending.
#define SIL_CVG_THRESHOLD    0x3F // 32..255, 63 seems to give best results
#define SCHWA (AA_EN | IM_RD | CLR_ON_CVG | CVG_DST_WRAP | CVG_X_ALPHA | FORCE_BL)
static const Gfx dl_silhouette_begin[] = {
    gsDPPipeSync(),
    // Set the render mode for the silhouette so that it gets its color and alpha from the fog register.
    gsDPSetRenderMode((SCHWA | GBL_c1(G_BL_CLR_FOG, G_BL_A_FOG, G_BL_CLR_MEM, G_BL_1MA)),
                      (SCHWA | GBL_c2(G_BL_CLR_FOG, G_BL_A_FOG, G_BL_CLR_MEM, G_BL_1MA))),
    // Set the silhouette's color & alpha.
    gsDPSetFogColor(0, 0, 0, SILHOUETTE),
    // Hacky way to prevent triangle overlap. 32..255. 63 seems to give best results.
    gsDPSetEnvColor(0, 0, 0, 0x3F),
    gsSPEndDisplayList(),
};

static const Gfx dl_silhouette_end[] = {
    gsDPPipeSync(),
    gsDPSetFogColor(  0,   0,   0, 255), // Reset fog color & alpha
    gsDPSetEnvColor(255, 255, 255, 255), // Reset env color & alpha
    gsSPEndDisplayList(),
};
#undef SCHWA
#endif

struct RenderPhase {
    u8 startLayer;
    u8 endLayer;
#ifdef OBJECTS_REJ
    u8 ucode;
#endif
};

//                                               startLayer                      endLayer                        ucode
static struct RenderPhase sRenderPhases[] = {
#ifdef OBJECTS_REJ
 #if SILHOUETTE
    // Silhouette, .rej
    /* RENDER_PHASE_ZEX_BEFORE_SILHOUETTE   */ { LAYER_FIRST,                    LAYER_LAST_BEFORE_SILHOUETTE,   GRAPH_NODE_UCODE_DEFAULT },
    /* RENDER_PHASE_REJ_ZB                  */ { LAYER_ZB_FIRST,                 LAYER_LAST_BEFORE_SILHOUETTE,   GRAPH_NODE_UCODE_REJ     },
    /* RENDER_PHASE_REJ_SILHOUETTE          */ { LAYER_SILHOUETTE_FIRST,         LAYER_SILHOUETTE_LAST,          GRAPH_NODE_UCODE_REJ     },
    /* RENDER_PHASE_REJ_NON_SILHOUETTE      */ { LAYER_SILHOUETTE_FIRST,         LAYER_SILHOUETTE_LAST,          GRAPH_NODE_UCODE_REJ     },
    /* RENDER_PHASE_REJ_OCCLUDE_SILHOUETTE  */ { LAYER_OCCLUDE_SILHOUETTE_FIRST, LAYER_OCCLUDE_SILHOUETTE_LAST,  GRAPH_NODE_UCODE_REJ     },
    /* RENDER_PHASE_ZEX_AFTER_SILHOUETTE    */ { LAYER_OCCLUDE_SILHOUETTE_FIRST, LAYER_LAST,                     GRAPH_NODE_UCODE_DEFAULT },
    /* RENDER_PHASE_REJ_NON_ZB              */ { LAYER_NON_ZB_FIRST,             LAYER_LAST,                     GRAPH_NODE_UCODE_REJ     },
 #else
    // No silhouette, .rej
    /* RENDER_PHASE_ZEX_BG                  */ { LAYER_FIRST,                    LAYER_FIRST,                    GRAPH_NODE_UCODE_DEFAULT },
    /* RENDER_PHASE_REJ_ZB                  */ { LAYER_ZB_FIRST,                 LAYER_ZB_LAST,                  GRAPH_NODE_UCODE_REJ     },
    /* RENDER_PHASE_ZEX_ALL                 */ { LAYER_ZB_FIRST,                 LAYER_LAST,                     GRAPH_NODE_UCODE_DEFAULT },
    /* RENDER_PHASE_REJ_NON_ZB              */ { LAYER_NON_ZB_FIRST,             LAYER_LAST,                     GRAPH_NODE_UCODE_REJ     },
 #endif
#else
 #if SILHOUETTE
    // Silhouette, no .rej
    /* RENDER_PHASE_ZEX_BEFORE_SILHOUETTE   */ { LAYER_FIRST,                    LAYER_LAST_BEFORE_SILHOUETTE    },
    /* RENDER_PHASE_ZEX_SILHOUETTE          */ { LAYER_SILHOUETTE_FIRST,         LAYER_SILHOUETTE_LAST           },
    /* RENDER_PHASE_ZEX_NON_SILHOUETTE      */ { LAYER_SILHOUETTE_FIRST,         LAYER_SILHOUETTE_LAST           },
    /* RENDER_PHASE_ZEX_OCCLUDE_SILHOUETTE  */ { LAYER_OCCLUDE_SILHOUETTE_FIRST, LAYER_OCCLUDE_SILHOUETTE_LAST   },
    /* RENDER_PHASE_ZEX_AFTER_SILHOUETTE    */ { LAYER_OCCLUDE_SILHOUETTE_FIRST, LAYER_LAST                      },
 #else
    // No silhouette, no .rej
    /* RENDER_PHASE_ZEX_ALL                 */ { LAYER_FIRST,                    LAYER_LAST                      },
 #endif
#endif
};

extern const Gfx init_rsp[];

#ifdef OBJECTS_REJ
void switch_ucode(s32 ucode) {
    // Set the ucode and RCP settings
    switch (ucode) {
        default: // GRAPH_NODE_UCODE_DEFAULT
        case GRAPH_NODE_UCODE_DEFAULT:
            gSPLoadUcodeL(gDisplayListHead++, gspF3DZEX2_NoN_PosLight_fifo); // F3DZEX2_PosLight
            // Reload the necessary RSP settings
            gSPDisplayList(gDisplayListHead++, init_rsp);
            break;
        case GRAPH_NODE_UCODE_REJ:
            // Use .rej Microcode, skip sub-pixel processing on console
            if (gIsConsole) {
                gSPLoadUcodeL(gDisplayListHead++, gspF3DLX2_Rej_fifo); // F3DLX2_Rej
            } else {
                gSPLoadUcodeL(gDisplayListHead++, gspF3DEX2_Rej_fifo); // F3DEX2_Rej
            }
            // Reload the necessary RSP settings
            gSPDisplayList(gDisplayListHead++, init_rsp);
            // Set the clip ratio (see init_rsp)
            gSPClipRatio(gDisplayListHead++, FRUSTRATIO_2);
            break;
    }
}
#endif

#define UPPER_FIXED(x) ((int)((unsigned int)((x) * 0x10000) >> 16))
#define LOWER_FIXED(x) ((int)((unsigned int)((x) * 0x10000) & 0xFFFF))

// Fixed-point identity matrix with the inverse of world scale
Mtx identityMatrixWorldScale = {{
    {UPPER_FIXED(1.0f / WORLD_SCALE) << 16, 0x00000000,
     UPPER_FIXED(1.0f / WORLD_SCALE) <<  0, 0x00000000},
    {0x00000000,                            UPPER_FIXED(1.0f / WORLD_SCALE) << 16,
     0x00000000,                            UPPER_FIXED(1.0f)               <<  0},
    {LOWER_FIXED(1.0f / WORLD_SCALE) << 16, 0x00000000,
     LOWER_FIXED(1.0f / WORLD_SCALE) <<  0, 0x00000000},
    {0x00000000,                            LOWER_FIXED(1.0f / WORLD_SCALE) << 16,
     0x00000000,                            LOWER_FIXED(1.0f)               <<  0}
}};

/**
 * Process a master list node. This has been modified, so now it runs twice, for each microcode.
 * It iterates through the first 5 layers of if the first index using F3DLX2.Rej, then it switches
 * to F3DZEX and iterates through all layers, then switches back to F3DLX2.Rej and finishes the last
 * 3. It does this, because layers 5-7 are non zbuffered, and just doing 0-7 of ZEX, then 0-7 of REJ
 * would make the ZEX 0-4 render on top of Rej's 5-7.
 */
void geo_process_master_list_sub(struct GraphNodeMasterList *node) {
    struct RenderPhase *renderPhase;
    struct DisplayListNode *currList;
    s32 currLayer     = LAYER_FIRST;
    s32 startLayer    = LAYER_FIRST;
    s32 endLayer      = LAYER_LAST;
    s32 ucode         = GRAPH_NODE_UCODE_DEFAULT;
    s32 phaseIndex    = RENDER_PHASE_FIRST;
    s32 enableZBuffer = (node->node.flags & GRAPH_RENDER_Z_BUFFER) != 0;
    struct RenderModeContainer *mode1List = &renderModeTable_1Cycle[enableZBuffer];
    struct RenderModeContainer *mode2List = &renderModeTable_2Cycle[enableZBuffer];

    // Loop through the render phases
    for (phaseIndex = RENDER_PHASE_FIRST; phaseIndex < RENDER_PHASE_END; phaseIndex++) {
        // Get the render phase information.
        renderPhase = &sRenderPhases[phaseIndex];
        startLayer  = renderPhase->startLayer;
        endLayer    = renderPhase->endLayer;
#ifdef OBJECTS_REJ
        ucode       = renderPhase->ucode;
        // Set the ucode for the current render phase
        switch_ucode(ucode);
        gSPLookAt(gDisplayListHead++, gCurLookAt);
#endif
        if (enableZBuffer) {
            // Enable z buffer.
            gDPPipeSync(gDisplayListHead++);
            gSPSetGeometryMode(gDisplayListHead++, G_ZBUFFER);
        }
        // Iterate through the layers on the current render phase.
        for (currLayer = startLayer; currLayer <= endLayer; currLayer++) {
            // Set 'currList' to the first DisplayListNode on the current layer.
            currList = node->listHeads[ucode][currLayer];
#if defined(DISABLE_AA) || !SILHOUETTE
            // Set the render mode for the current layer.
            gDPSetRenderMode(gDisplayListHead++, mode1List->modes[currLayer],
                                                 mode2List->modes[currLayer]);
#else
            if (phaseIndex == RENDER_PHASE_NON_SILHOUETTE) {
                // To properly cover the silhouette, disable AA.
                // The silhouette model does not have AA due to the hack used to prevent triangle overlap.
                gDPSetRenderMode(gDisplayListHead++, (mode1List->modes[currLayer] & ~IM_RD),
                                                     (mode2List->modes[currLayer] & ~IM_RD));
            } else {
                // Set the render mode for the current dl.
                gDPSetRenderMode(gDisplayListHead++, mode1List->modes[currLayer],
                                                     mode2List->modes[currLayer]);
            }
#endif
            // Iterate through all the displaylists on the current layer.
            while (currList != NULL) {
                // Add the display list's transformation to the master list.
                gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(currList->transform),
                          (G_MTX_MODELVIEW | G_MTX_LOAD | G_MTX_NOPUSH));
#if SILHOUETTE
                if (phaseIndex == RENDER_PHASE_SILHOUETTE) {
                    // Add the current display list to the master list, with silhouette F3D.
                    gSPDisplayList(gDisplayListHead++, dl_silhouette_begin);
                    gSPDisplayList(gDisplayListHead++, currList->displayList);
                    gSPDisplayList(gDisplayListHead++, dl_silhouette_end);
                } else {
                    // Add the current display list to the master list.
                    gSPDisplayList(gDisplayListHead++, currList->displayList);
                }
#else
                // Add the current display list to the master list.
                gSPDisplayList(gDisplayListHead++, currList->displayList);
#endif
                // Move to the next DisplayListNode.
                currList = currList->next;
            }
        }
    }

    if (enableZBuffer) {
        // Disable z buffer.
        gDPPipeSync(gDisplayListHead++);
        gSPClearGeometryMode(gDisplayListHead++, G_ZBUFFER);
    }
#ifdef OBJECTS_REJ
 #if defined(F3DEX_GBI_2) && defined(VISUAL_DEBUG)
    if (hitboxView) render_debug_boxes(DEBUG_UCODE_REJ);
 #endif
    switch_ucode(GRAPH_NODE_UCODE_DEFAULT);
#endif
#ifdef VISUAL_DEBUG
    if ( hitboxView) render_debug_boxes(DEBUG_UCODE_DEFAULT | DEBUG_BOX_CLEAR);
    // Load the world scale identity matrix
    gSPMatrix(gDisplayListHead++, &identityMatrixWorldScale, G_MTX_MODELVIEW | G_MTX_LOAD | G_MTX_NOPUSH);
    if (surfaceView) visual_surface_loop();
#endif
}

/**
 * Appends the display list to one of the master lists based on the layer
 * parameter. Look at the RenderModeContainer struct to see the corresponding
 * render modes of layers.
 */
void geo_append_display_list(void *displayList, s32 layer) {
    s32 ucode = GRAPH_NODE_UCODE_DEFAULT;
#ifdef F3DEX_GBI_2
    gSPLookAt(gDisplayListHead++, gCurLookAt);
#endif
#if defined(OBJECTS_REJ) || SILHOUETTE
    if (gCurGraphNodeObject != NULL) {
 #ifdef OBJECTS_REJ
        ucode = gCurGraphNodeObject->ucode;
 #endif
 #if SILHOUETTE
        if (gCurGraphNodeObject->node.flags & GRAPH_RENDER_SILHOUETTE) {
            switch (layer) {
                case LAYER_OPAQUE: layer = LAYER_SILHOUETTE_OPAQUE; break;
                case LAYER_ALPHA:  layer = LAYER_SILHOUETTE_ALPHA;  break;
            }
        }
        if (gCurGraphNodeObject->node.flags & GRAPH_RENDER_OCCLUDE_SILHOUETTE) {
            switch (layer) {
                case LAYER_OPAQUE: layer = LAYER_OCCLUDE_SILHOUETTE_OPAQUE; break;
                case LAYER_ALPHA:  layer = LAYER_OCCLUDE_SILHOUETTE_ALPHA;  break;
            }
        }
 #endif // SILHOUETTE
    }
#endif // F3DEX_GBI_2 || SILHOUETTE
    if (gCurGraphNodeMasterList != NULL) {
        struct DisplayListNode *listNode =
            alloc_only_pool_alloc(gDisplayListHeap, sizeof(struct DisplayListNode));

        listNode->transform = gMatStackFixed[gMatStackIndex];
        listNode->displayList = displayList;
        listNode->next = NULL;
        if (gCurGraphNodeMasterList->listHeads[ucode][layer] == NULL) {
            gCurGraphNodeMasterList->listHeads[ucode][layer] = listNode;
        } else {
            gCurGraphNodeMasterList->listTails[ucode][layer]->next = listNode;
        }
        gCurGraphNodeMasterList->listTails[ucode][layer] = listNode;
    }
}

static void inc_mat_stack() {
    Mtx *mtx = alloc_display_list(sizeof(*mtx));
    gMatStackIndex++;
    mtxf_to_mtx(mtx, gMatStack[gMatStackIndex]);
    gMatStackFixed[gMatStackIndex] = mtx;
}

static void append_dl_and_return(struct GraphNodeDisplayList *node) {
    if (node->displayList != NULL) {
        geo_append_display_list(node->displayList, GET_GRAPH_NODE_LAYER(node->node.flags));
    }
    if (node->node.children != NULL) {
        geo_process_node_and_siblings(node->node.children);
    }
    gMatStackIndex--;
}

/**
 * Process the master list node.
 */
void geo_process_master_list(struct GraphNodeMasterList *node) {
    s32 ucode, layer;

    if (gCurGraphNodeMasterList == NULL && node->node.children != NULL) {
        gCurGraphNodeMasterList = node;
        for (ucode = 0; ucode < GRAPH_NODE_NUM_UCODES; ucode++) {
            for (layer = LAYER_FIRST; layer < LAYER_COUNT; layer++) {
                node->listHeads[ucode][layer] = NULL;
            }
        }
        geo_process_node_and_siblings(node->node.children);
        geo_process_master_list_sub(gCurGraphNodeMasterList);
        gCurGraphNodeMasterList = NULL;
    }
}

/**
 * Process an orthographic projection node.
 */
 void geo_process_ortho_projection(struct GraphNodeOrthoProjection *node) {
    if (node->node.children != NULL) {
        Mtx *mtx = alloc_display_list(sizeof(*mtx));
        f32 scale = node->scale / 2.0f;
        f32 left = (gCurGraphNodeRoot->x - gCurGraphNodeRoot->width) * scale;
        f32 right = (gCurGraphNodeRoot->x + gCurGraphNodeRoot->width) * scale;
        f32 top = (gCurGraphNodeRoot->y - gCurGraphNodeRoot->height) * scale;
        f32 bottom = (gCurGraphNodeRoot->y + gCurGraphNodeRoot->height) * scale;

        guOrtho(mtx, left, right, bottom, top, -2.0f, 2.0f, 1.0f);
        gSPPerspNormalize(gDisplayListHead++, 0xFFFF);
        gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(mtx), G_MTX_PROJECTION | G_MTX_LOAD | G_MTX_NOPUSH);

        geo_process_node_and_siblings(node->node.children);
    }
}

/**
 * Process a perspective projection node.
 */
void geo_process_perspective(struct GraphNodePerspective *node) {
    if (node->fnNode.func != NULL) {
        node->fnNode.func(GEO_CONTEXT_RENDER, &node->fnNode.node, gMatStack[gMatStackIndex]);
    }
    if (node->fnNode.node.children != NULL) {
        u16 perspNorm;
        Mtx *mtx = alloc_display_list(sizeof(*mtx));
#ifdef WIDE
        if (gConfig.widescreen && gCurrLevelNum != 0x01){
            sAspectRatio = 16.0f / 9.0f; // 1.775f
        } else {
            sAspectRatio = 4.0f / 3.0f; // 1.33333f
        }
#else
        sAspectRatio = 4.0f / 3.0f; // 1.33333f
#endif

        guPerspective(mtx, &perspNorm, node->fov, sAspectRatio, node->near / WORLD_SCALE, node->far / WORLD_SCALE, 1.0f);
        gSPPerspNormalize(gDisplayListHead++, perspNorm);

        gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(mtx), G_MTX_PROJECTION | G_MTX_LOAD | G_MTX_NOPUSH);

        gCurGraphNodeCamFrustum = node;
        geo_process_node_and_siblings(node->fnNode.node.children);
        gCurGraphNodeCamFrustum = NULL;
    }
}

static f32 get_dist_from_camera(Vec3f pos) {
    return -((gCameraTransform[0][2] * pos[0])
           + (gCameraTransform[1][2] * pos[1])
           + (gCameraTransform[2][2] * pos[2])
           +  gCameraTransform[3][2]);
}

/**
 * Process a level of detail node. From the current transformation matrix,
 * the perpendicular distance to the camera is extracted and the children
 * of this node are only processed if that distance is within the render
 * range of this node.
 */
void geo_process_level_of_detail(struct GraphNodeLevelOfDetail *node) {
#ifdef AUTO_LOD
    f32 distanceFromCam = gIsConsole ? get_dist_from_camera(gMatStack[gMatStackIndex][3]) : 50.0f;
#else
    f32 distanceFromCam = get_dist_from_camera(gMatStack[gMatStackIndex][3]);
#endif

    if ((f32)node->minDistance <= distanceFromCam
        && distanceFromCam < (f32)node->maxDistance
        && node->node.children != 0) {
        geo_process_node_and_siblings(node->node.children);
    }
}

/**
 * Process a switch case node. The node's selection function is called
 * if it is 0, and among the node's children, only the selected child is
 * processed next.
 */
void geo_process_switch(struct GraphNodeSwitchCase *node) {
    struct GraphNode *selectedChild = node->fnNode.node.children;
    s32 i;

    if (node->fnNode.func != NULL) {
        node->fnNode.func(GEO_CONTEXT_RENDER, &node->fnNode.node, gMatStack[gMatStackIndex]);
    }
    for (i = 0; selectedChild != NULL && node->selectedCase > i; i++) {
        selectedChild = selectedChild->next;
    }
    if (selectedChild != NULL) {
        geo_process_node_and_siblings(selectedChild);
    }
}

extern struct MarioState *gMarioState;

struct SceneLight gPointLights[MAX_POINT_LIGHTS];
s8 gLightDir[3] = {0x28, 0x28, 0x28};
u8 gOverrideDirectionalLight = FALSE;
u8 gOverrideAmbientLight = FALSE;
u8 gPointLightCount = 0;
u8 gAreaPointLightCount = 0;

Lights1* gCurDirectionalLight;

/**
 * Gets the square of the distance between two vectors
 * The first vector is of floats, the second is of s16
 * All math operations are done on integers
 */
u32 vec3f_vec3s_dist_sq(Vec3f p1, Vec3s p2)
{
    s32 dx = p2[0] - (s32)p1[0];
    s32 dy = p2[1] - (s32)p1[1];
    s32 dz = p2[2] - (s32)p1[2];
    return (u32)(dx * dx) + (u32)(dy * dy) + (u32)(dz * dz);
}

#include "src/engine/surface_collision.h"

int gPointLightCompatibilityMode = 0;
Mat4 *viewMat;

/**
 * Creates a displaylist to set the active point lights closest to a given location
 */
Gfx* createPointLightsDl(Vec3f pos, f32 yOffset)
{
    Gfx *pointLightsDl, *pointLightsDlHead;

    // The lights to be used for this object
    struct SceneLight *lights[MAX_POINT_LIGHTS_ACTIVE];

    // The number of lights selected to be active for this object
    s32 numLightsPicked = 0;

    // The square of the distances to each point light
    u32 distancesSq[MAX_POINT_LIGHTS_ACTIVE];

    // The distance of the furthest light selected
    u32 maxDistanceSq;

    // The index of the furthest away light from this object
    // i.e. if index 1 in lights is the furthest light from this object, then this is 1
    u32 maxIndex = 0;

    // The square of the distance to the current light being checked
    u32 curDistSq;

    // The index of a point light being added (its position in Light *lights[])
    s32 newIndex;

    // Iterator variables
    s32 i,j;
    
    // Raycast variables
    Vec3f dir, hit;
    struct Surface* surf;

    if (gPointLightCount)
    {
        // Probe higher by the given offset (used since most objects have their origin at the bottom)
        pos[1] += yOffset;
    }

    // Find the closest lights
    for (i = 0; i < gPointLightCount; i++)
    {
        // Reset newIndex so we can use it to check if the current light was added
        newIndex = -1;

        // Get the distance to the current light from the object
        curDistSq = vec3f_vec3s_dist_sq(pos, gPointLights[i].worldPos);

        // Skip this point light if it is too far away to matter
        if (curDistSq > MAX_POINT_LIGHT_DIST * MAX_POINT_LIGHT_DIST) continue;

        // Skip this point light if it is set to occlude and is occluded
        // If the object and the light are at the same position, skip the raycast
        if (curDistSq != 0 && gPointLights[i].flags & LIGHT_FLAG_OCCLUDE)
        {
            dir[0] = gPointLights[i].worldPos[0] - pos[0];
            dir[1] = gPointLights[i].worldPos[1] - pos[1];
            dir[2] = gPointLights[i].worldPos[2] - pos[2];
            // Raycast will return a nonnegative distance in the case of a hit
            if (raycast(pos, dir, sqrtf(curDistSq), hit, &surf) >= 0)
            {
                continue;
            }
        }

        // If we haven't filled all the active light slots, just add this one
        if (numLightsPicked < MAX_POINT_LIGHTS_ACTIVE)
        {
            // Record the index this light was placed into and update the picked light count
            newIndex = numLightsPicked;
            numLightsPicked++;
        }
        // Otherwise, we need to check if this one is closer than any of the ones picked thus far
        else
        {
            // Check if this light is closer than the furthest away one to be used
            // If it is, then we remove the furthest light and add this one
            if (curDistSq < distancesSq[maxIndex])
            {
                newIndex = maxIndex;
            }
        }

        // If this light was added, add it to the lights array, update its distance,
        // and update the distance order of the other lights
        if (newIndex != -1)
        {
            // Place this light in the lights array
            lights[newIndex] = &gPointLights[i];

            // Update the distance to this light
            distancesSq[newIndex] = curDistSq;

            // Set this light to the be furthest one away
            // This will get updated in the following loop
            maxDistanceSq = curDistSq;
            maxIndex = newIndex;

            // Iterate over every light, checking to see if it is further than the current furthest
            // If it is, then set it to be the furthest light instead
            for (j = 0; j < MAX_POINT_LIGHTS_ACTIVE; j++)
            {
                // Skip checking the current light, since we assumed it was the furthest already
                if (j == newIndex) continue;

                // Check if the light being checked is further than current furthest
                if (maxDistanceSq < distancesSq[j])
                {
                    maxDistanceSq = distancesSq[j];
                    maxIndex = j;
                }
            }
        }
    }
    
    // Allocate a displaylist with room for each gSPLight and the gSPEndDisplayList
    pointLightsDlHead = pointLightsDl = alloc_display_list(sizeof(Gfx) * (numLightsPicked + 4));

    gSPNumLights(pointLightsDl++, NUMLIGHTS_1 + numLightsPicked);
    
    gSPLight(pointLightsDl++, &gCurDirectionalLight->l, LIGHT_1);

    // Add the gSPLights to the display list
    for (i = 0; i < numLightsPicked; i++)
    {
        if (gPointLightCompatibilityMode)
        {
            Light *curLight = alloc_display_list(sizeof(Light));
            u8 color[3];
            f32 lightDist;
            f32 lightScale;

            bzero(curLight, sizeof(Light));

            color[0] = lights[i]->l.pl.col[0];
            color[1] = lights[i]->l.pl.col[1];
            color[2] = lights[i]->l.pl.col[2];

            dir[0] = lights[i]->worldPos[0] - pos[0];
            dir[1] = lights[i]->worldPos[1] - pos[1];
            dir[2] = lights[i]->worldPos[2] - pos[2];

            lightDist = sqrtf(distancesSq[i]);
            lightScale = 1.0f / ((1.0f / 65536.0f) * (
                0.25f * lights[i]->l.pl.constant_attenuation +
                2.0f * lightDist * lights[i]->l.pl.linear_attenuation +
                0.3f * lightDist * lightDist * lights[i]->l.pl.quadratic_attenuation) + 1.0f);

            curLight->l.col[0] = curLight->l.colc[0] = (u8)(color[0] * lightScale + 0.5f);
            curLight->l.col[1] = curLight->l.colc[1] = (u8)(color[1] * lightScale + 0.5f);
            curLight->l.col[2] = curLight->l.colc[2] = (u8)(color[2] * lightScale + 0.5f);
            
            dir[0] *= 120.0f / (lightDist);
            dir[1] *= 120.0f / (lightDist);
            dir[2] *= 120.0f / (lightDist);

            curLight->l.dir[0] = (s8)(dir[0]);
            curLight->l.dir[1] = (s8)(dir[1]);
            curLight->l.dir[2] = (s8)(dir[2]);

            gSPLight(pointLightsDl++, curLight, LIGHT_2 + i);
        }
        else
        {
            gSPLight(pointLightsDl++, &lights[i]->l, LIGHT_2 + i);
        }
    }

    if (gPointLightCount)
    {
        // Restore the original position
        pos[1] -= yOffset;
    }

    gSPLight(pointLightsDl++, &gCurDirectionalLight->a, LIGHT_2 + numLightsPicked);

    // Terminate the display list
    gSPEndDisplayList(pointLightsDl);

    // Return the head of the created display list
    return pointLightsDlHead;
}

// Sets the scene's directional light, overrides whatever may be set in the area's geolayout
void set_directional_light(Vec3f direction, s32 red, s32 green, s32 blue)
{
    Vec3f directionNormalized;
    vec3f_copy(directionNormalized, direction);
    vec3f_normalize(directionNormalized);
    gLightDir[0] = (s8)(s32)(directionNormalized[0] * 0x40);
    gLightDir[1] = (s8)(s32)(directionNormalized[1] * 0x40);
    gLightDir[2] = (s8)(s32)(directionNormalized[2] * 0x40);
    gCurDirectionalLight->l[0].l.colc[0] = gCurDirectionalLight->l[0].l.col[0] = red;
    gCurDirectionalLight->l[0].l.colc[1] = gCurDirectionalLight->l[0].l.col[1] = green;
    gCurDirectionalLight->l[0].l.colc[2] = gCurDirectionalLight->l[0].l.col[2] = blue;
    gOverrideDirectionalLight = TRUE;
}

// Sets the scene's ambient light, overrides whatever may be set in the area's geolayout
void set_ambient_light(s32 red, s32 green, s32 blue)
{
    gCurDirectionalLight->a.l.colc[0] = gCurDirectionalLight->a.l.col[0] = red;
    gCurDirectionalLight->a.l.colc[1] = gCurDirectionalLight->a.l.col[1] = green;
    gCurDirectionalLight->a.l.colc[2] = gCurDirectionalLight->a.l.col[2] = blue;
    gOverrideAmbientLight = TRUE;
}

// Emits a point light with the given parameters
void emit_light(Vec3f pos, s32 red, s32 green, s32 blue, u32 quadraticFalloff, u32 linearFalloff, u32 constantFalloff)
{
    gPointLights[gPointLightCount].l.pl.colc[0] = gPointLights[gPointLightCount].l.pl.col[0] = red;
    gPointLights[gPointLightCount].l.pl.colc[1] = gPointLights[gPointLightCount].l.pl.col[1] = green;
    gPointLights[gPointLightCount].l.pl.colc[2] = gPointLights[gPointLightCount].l.pl.col[2] = blue;
    gPointLights[gPointLightCount].l.pl.constant_attenuation = (constantFalloff == 0) ? 8 : constantFalloff;
    gPointLights[gPointLightCount].l.pl.linear_attenuation = linearFalloff;
    gPointLights[gPointLightCount].l.pl.quadratic_attenuation = quadraticFalloff;
    gPointLights[gPointLightCount].worldPos[0] = pos[0];
    gPointLights[gPointLightCount].worldPos[1] = pos[1];
    gPointLights[gPointLightCount].worldPos[2] = pos[2];
    gPointLightCount++;
}
Mat4 gCameraTransform;

Lights1 defaultLight = gdSPDefLights1(
    0x3F, 0x3F, 0x3F, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00
);

Vec3f globalLightDirection = { 0x28, 0x28, 0x28 };

void setup_global_light() {
    gCurDirectionalLight = (Lights1*)alloc_display_list(sizeof(Lights1));
    bcopy(&defaultLight, gCurDirectionalLight, sizeof(Lights1));
    gSPSetLights1(gDisplayListHead++, (*gCurDirectionalLight));
}

void set_global_light_direction() {
    // Set the light direction
    gCurDirectionalLight->l->l.dir[0] = gLightDir[0];
    gCurDirectionalLight->l->l.dir[1] = gLightDir[1];
    gCurDirectionalLight->l->l.dir[2] = gLightDir[2];
}

/**
 * Process a camera node.
 */
void geo_process_camera(struct GraphNodeCamera *node) {
    Mtx *rollMtx = alloc_display_list(sizeof(*rollMtx));
    Gfx *setLightsDL = alloc_display_list(sizeof(Gfx) * 3);
    Gfx *levelLightsDL;
    Vec3f probePos = {0, 0, 0};
    s32 i;
    Mtx *viewMtx = alloc_display_list(sizeof(Mtx));

    if (node->fnNode.func != NULL) {
        node->fnNode.func(GEO_CONTEXT_RENDER, &node->fnNode.node, gMatStack[gMatStackIndex]);
    }
    mtxf_rotate_xy(rollMtx, node->rollScreen);

    gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(rollMtx), G_MTX_PROJECTION | G_MTX_MUL | G_MTX_NOPUSH);
    geo_append_display_list(setLightsDL, LAYER_OPAQUE);

    mtxf_lookat(gCameraTransform, node->pos, node->focus, node->roll);

    // Calculate the lookAt
#ifdef F3DEX_GBI_2
    // @bug This is where the LookAt values should be calculated but aren't.
    // As a result, environment mapping is broken on Fast3DEX2 without the
    // changes below.
    Mat4* cameraMatrix = &gCameraTransform;
 #ifdef FIX_REFLECT_MTX
    gCurLookAt->l[0].l.dir[0] = (s8)(127.0f * (*cameraMatrix)[0][0]);
    gCurLookAt->l[0].l.dir[1] = (s8)(127.0f * (*cameraMatrix)[1][0]);
    gCurLookAt->l[0].l.dir[2] = (s8)(127.0f * (*cameraMatrix)[2][0]);
    gCurLookAt->l[1].l.dir[0] = (s8)(127.0f * -(*cameraMatrix)[0][1]);
    gCurLookAt->l[1].l.dir[1] = (s8)(127.0f * -(*cameraMatrix)[1][1]);
    gCurLookAt->l[1].l.dir[2] = (s8)(127.0f * -(*cameraMatrix)[2][1]);
 #else
    gCurLookAt->l[0].l.dir[0] = (s8)(127.0f * (*cameraMatrix)[0][0]);
    gCurLookAt->l[0].l.dir[1] = (s8)(127.0f * (*cameraMatrix)[1][0]);
    gCurLookAt->l[0].l.dir[2] = (s8)(127.0f * (*cameraMatrix)[2][0]);
    gCurLookAt->l[1].l.dir[0] = (s8)(127.0f * (*cameraMatrix)[0][1]);
    gCurLookAt->l[1].l.dir[1] = (s8)(127.0f * (*cameraMatrix)[1][1]);
    gCurLookAt->l[1].l.dir[2] = (s8)(127.0f * (*cameraMatrix)[2][1]);
 #endif
#endif // F3DEX_GBI_2

    // Make a copy of the view matrix and scale its translation based on WORLD_SCALE
    Mat4 scaledCamera;
    mtxf_copy(scaledCamera, gCameraTransform);
    for (int i = 0; i < 3; i++) {
        scaledCamera[3][i] /= WORLD_SCALE;
    }
    
    // Convert the scaled matrix to fixed-point and integrate it into the projection matrix stack
    guMtxF2L(scaledCamera, viewMtx);
    gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(viewMtx), G_MTX_PROJECTION | G_MTX_MUL | G_MTX_NOPUSH);

    if (node->fnNode.node.children != 0) {
        gCurGraphNodeCamera = node;
        node->matrixPtr = &gCameraTransform;
        geo_process_node_and_siblings(node->fnNode.node.children);
        gCurGraphNodeCamera = NULL;
    }


    // Copy the light's position into the light struct and account for WORLD_SCALE
    for (i = 0; i < gPointLightCount; i++)
    {
        gPointLights[i].l.pl.pos[0] = gPointLights[i].worldPos[0] / WORLD_SCALE;
        gPointLights[i].l.pl.pos[1] = gPointLights[i].worldPos[1] / WORLD_SCALE;
        gPointLights[i].l.pl.pos[2] = gPointLights[i].worldPos[2] / WORLD_SCALE;
    }

    set_global_light_direction();
    gOverrideAmbientLight = FALSE;
    gOverrideDirectionalLight = FALSE;
    
    // Set up the light display list
    // This has to be done after the area's GeoLayout is processed, as
    // some point lights may be defined there instead of by objects
    if (gPointLightCount > 0)
    {
        // Enable point lighting
        gSPSetGeometryMode(setLightsDL++, G_POINT_LIGHTING);
    }
    else
    {
        // Disable point lighting (may not be required, but doesn't hurt)
        gSPClearGeometryMode(setLightsDL++, G_POINT_LIGHTING);
    }

    // Enable the lights closes to the given probe position as the level's lighting
    levelLightsDL = createPointLightsDl(probePos, 300.0f);
    gSPDisplayList(setLightsDL++, levelLightsDL);

    // Terminate the point lighting DL
    gSPEndDisplayList(setLightsDL++);

    gMatStackIndex--;
}

/**
 * Process a translation / rotation node. A transformation matrix based
 * on the node's translation and rotation is created and pushed on both
 * the float and fixed point matrix stacks.
 * For the rest it acts as a normal display list node.
 */
void geo_process_translation_rotation(struct GraphNodeTranslationRotation *node) {
    Vec3f translation;

    vec3s_to_vec3f(translation, node->translation);
    mtxf_rotate_zxy_and_translate_and_mul(node->rotation, translation, gMatStack[gMatStackIndex + 1], gMatStack[gMatStackIndex]);

    inc_mat_stack();
    append_dl_and_return((struct GraphNodeDisplayList *)node);
}

/**
 * Process a translation node. A transformation matrix based on the node's
 * translation is created and pushed on both the float and fixed point matrix stacks.
 * For the rest it acts as a normal display list node.
 */
void geo_process_translation(struct GraphNodeTranslation *node) {
    Vec3f translation;

    vec3s_to_vec3f(translation, node->translation);
    mtxf_rotate_zxy_and_translate_and_mul(gVec3sZero, translation, gMatStack[gMatStackIndex + 1], gMatStack[gMatStackIndex]);

    inc_mat_stack();
    append_dl_and_return((struct GraphNodeDisplayList *)node);
}

/**
 * Process a rotation node. A transformation matrix based on the node's
 * rotation is created and pushed on both the float and fixed point matrix stacks.
 * For the rest it acts as a normal display list node.
 */
void geo_process_rotation(struct GraphNodeRotation *node) {
    mtxf_rotate_zxy_and_translate_and_mul(node->rotation, gVec3fZero, gMatStack[gMatStackIndex + 1], gMatStack[gMatStackIndex]);

    inc_mat_stack();
    append_dl_and_return(((struct GraphNodeDisplayList *)node));
}

/**
 * Process a scaling node. A transformation matrix based on the node's
 * scale is created and pushed on both the float and fixed point matrix stacks.
 * For the rest it acts as a normal display list node.
 */
void geo_process_scale(struct GraphNodeScale *node) {
    Vec3f scaleVec;

    vec3f_set(scaleVec, node->scale, node->scale, node->scale);
    mtxf_scale_vec3f(gMatStack[gMatStackIndex + 1], gMatStack[gMatStackIndex], scaleVec);

    inc_mat_stack();
    append_dl_and_return((struct GraphNodeDisplayList *)node);
}

/**
 * Process a billboard node. A transformation matrix is created that makes its
 * children face the camera, and it is pushed on the floating point and fixed
 * point matrix stacks.
 * For the rest it acts as a normal display list node.
 */
void geo_process_billboard(struct GraphNodeBillboard *node) {
    Vec3f translation;
    Vec3f scale = { 1.0f, 1.0f, 1.0f };

    vec3s_to_vec3f(translation, node->translation);

    if (gCurGraphNodeHeldObject != NULL) {
        vec3f_copy(scale, gCurGraphNodeHeldObject->objNode->header.gfx.scale);
    } else if (gCurGraphNodeObject != NULL) {
        vec3f_copy(scale, gCurGraphNodeObject->scale);
    }

    mtxf_billboard(gMatStack[gMatStackIndex + 1], gMatStack[gMatStackIndex], translation, scale, gCurGraphNodeCamera->roll);

    inc_mat_stack();
    append_dl_and_return((struct GraphNodeDisplayList *)node);
}

/**
 * Process a display list node. It draws a display list without first pushing
 * a transformation on the stack, so all transformations are inherited from the
 * parent node. It processes its children if it has them.
 */
void geo_process_display_list(struct GraphNodeDisplayList *node) {
    append_dl_and_return((struct GraphNodeDisplayList *)node);

    gMatStackIndex++;
}

/**
 * Process a generated list. Instead of storing a pointer to a display list,
 * the list is generated on the fly by a function.
 */
void geo_process_generated_list(struct GraphNodeGenerated *node) {
    if (node->fnNode.func != NULL) {
        Gfx *list = node->fnNode.func(GEO_CONTEXT_RENDER, &node->fnNode.node, (struct AllocOnlyPool *) gMatStack[gMatStackIndex]);

        if (list != NULL) {
            geo_append_display_list((void *) VIRTUAL_TO_PHYSICAL(list), GET_GRAPH_NODE_LAYER(node->fnNode.node.flags));
        }
    }
    if (node->fnNode.node.children != NULL) {
        geo_process_node_and_siblings(node->fnNode.node.children);
    }
}

/**
 * Process a background node. Tries to retrieve a background display list from
 * the function of the node. If that function is null or returns null, a black
 * rectangle is drawn instead.
 */
void geo_process_background(struct GraphNodeBackground *node) {
    Gfx *list = NULL;

    if (node->fnNode.func != NULL) {
        list = node->fnNode.func(GEO_CONTEXT_RENDER, &node->fnNode.node,
                                 (struct AllocOnlyPool *) gMatStack[gMatStackIndex]);
    }
    if (list != NULL) {
        geo_append_display_list((void *) VIRTUAL_TO_PHYSICAL(list), GET_GRAPH_NODE_LAYER(node->fnNode.node.flags));
    } else if (gCurGraphNodeMasterList != NULL) {
#ifndef F3DEX_GBI_2E
        Gfx *gfxStart = alloc_display_list(sizeof(Gfx) * 7);
#else
        Gfx *gfxStart = alloc_display_list(sizeof(Gfx) * 8);
#endif
        Gfx *gfx = gfxStart;

        gDPPipeSync(gfx++);
        gDPSetCycleType(gfx++, G_CYC_FILL);
        gDPSetFillColor(gfx++, node->background);
        gDPFillRectangle(gfx++, GFX_DIMENSIONS_RECT_FROM_LEFT_EDGE(0), gBorderHeight,
        GFX_DIMENSIONS_RECT_FROM_RIGHT_EDGE(0) - 1, SCREEN_HEIGHT - gBorderHeight - 1);
        gDPPipeSync(gfx++);
        gDPSetCycleType(gfx++, G_CYC_1CYCLE);
        gSPEndDisplayList(gfx++);

        geo_append_display_list((void *) VIRTUAL_TO_PHYSICAL(gfxStart), LAYER_FORCE);
    }
    if (node->fnNode.node.children != NULL) {
        geo_process_node_and_siblings(node->fnNode.node.children);
    }
}

/**
 * Render an animated part. The current animation state is not part of the node
 * but set in global variables. If an animated part is skipped, everything afterwards desyncs.
 */
void geo_process_animated_part(struct GraphNodeAnimatedPart *node) {
    Vec3s rotation = { 0, 0, 0 };
    Vec3f translation = { node->translation[0], node->translation[1], node->translation[2] };

    if (gCurrAnimType == ANIM_TYPE_TRANSLATION) {
        translation[0] += gCurrAnimData[retrieve_animation_index(gCurrAnimFrame, &gCurrAnimAttribute)]
                          * gCurrAnimTranslationMultiplier;
        translation[1] += gCurrAnimData[retrieve_animation_index(gCurrAnimFrame, &gCurrAnimAttribute)]
                          * gCurrAnimTranslationMultiplier;
        translation[2] += gCurrAnimData[retrieve_animation_index(gCurrAnimFrame, &gCurrAnimAttribute)]
                          * gCurrAnimTranslationMultiplier;
        gCurrAnimType = ANIM_TYPE_ROTATION;
    } else {
        if (gCurrAnimType == ANIM_TYPE_LATERAL_TRANSLATION) {
            translation[0] +=
                gCurrAnimData[retrieve_animation_index(gCurrAnimFrame, &gCurrAnimAttribute)]
                * gCurrAnimTranslationMultiplier;
            gCurrAnimAttribute += 2;
            translation[2] +=
                gCurrAnimData[retrieve_animation_index(gCurrAnimFrame, &gCurrAnimAttribute)]
                * gCurrAnimTranslationMultiplier;
            gCurrAnimType = ANIM_TYPE_ROTATION;
        } else {
            if (gCurrAnimType == ANIM_TYPE_VERTICAL_TRANSLATION) {
                gCurrAnimAttribute += 2;
                translation[1] +=
                    gCurrAnimData[retrieve_animation_index(gCurrAnimFrame, &gCurrAnimAttribute)]
                    * gCurrAnimTranslationMultiplier;
                gCurrAnimAttribute += 2;
                gCurrAnimType = ANIM_TYPE_ROTATION;
            } else if (gCurrAnimType == ANIM_TYPE_NO_TRANSLATION) {
                gCurrAnimAttribute += 6;
                gCurrAnimType = ANIM_TYPE_ROTATION;
            }
        }
    }

    if (gCurrAnimType == ANIM_TYPE_ROTATION) {
        rotation[0] = gCurrAnimData[retrieve_animation_index(gCurrAnimFrame, &gCurrAnimAttribute)];
        rotation[1] = gCurrAnimData[retrieve_animation_index(gCurrAnimFrame, &gCurrAnimAttribute)];
        rotation[2] = gCurrAnimData[retrieve_animation_index(gCurrAnimFrame, &gCurrAnimAttribute)];
    }

    mtxf_rotate_xyz_and_translate_and_mul(rotation, translation, gMatStack[gMatStackIndex + 1], gMatStack[gMatStackIndex]);

    inc_mat_stack();
    append_dl_and_return(((struct GraphNodeDisplayList *)node));
}

/**
 * Initialize the animation-related global variables for the currently drawn
 * object's animation.
 */
void geo_set_animation_globals(struct AnimInfo *node, s32 hasAnimation) {
    struct Animation *anim = node->curAnim;

    if (hasAnimation) {
        node->animFrame = geo_update_animation_frame(node, &node->animFrameAccelAssist);
    }
    node->animTimer = gAreaUpdateCounter;
    if (anim->flags & ANIM_FLAG_HOR_TRANS) {
        gCurrAnimType = ANIM_TYPE_VERTICAL_TRANSLATION;
    } else if (anim->flags & ANIM_FLAG_VERT_TRANS) {
        gCurrAnimType = ANIM_TYPE_LATERAL_TRANSLATION;
    } else if (anim->flags & ANIM_FLAG_NO_TRANS) {
        gCurrAnimType = ANIM_TYPE_NO_TRANSLATION;
    } else {
        gCurrAnimType = ANIM_TYPE_TRANSLATION;
    }

    gCurrAnimFrame = node->animFrame;
    gCurrAnimEnabled = (anim->flags & ANIM_FLAG_DISABLED) == 0;
    gCurrAnimAttribute = segmented_to_virtual((void *) anim->index);
    gCurrAnimData = segmented_to_virtual((void *) anim->values);

    if (anim->animYTransDivisor == 0) {
        gCurrAnimTranslationMultiplier = 1.0f;
    } else {
        gCurrAnimTranslationMultiplier = (f32) node->animYTrans / (f32) anim->animYTransDivisor;
    }
}

/**
 * Process a shadow node. Renders a shadow under an object offset by the
 * translation of the first animated component and rotated according to
 * the floor below it.
 */
void geo_process_shadow(struct GraphNodeShadow *node) {
#ifndef DISABLE_SHADOWS
    if (gCurGraphNodeCamera != NULL && gCurGraphNodeObject != NULL) {
        Vec3f shadowPos;
        f32 shadowScale;

        if (gCurGraphNodeHeldObject != NULL) {
            vec3f_copy(shadowPos, gMatStack[gMatStackIndex][3]);
            shadowScale = node->shadowScale * gCurGraphNodeHeldObject->objNode->header.gfx.scale[0];
        } else {
            vec3f_copy(shadowPos, gCurGraphNodeObject->pos);
            shadowScale = node->shadowScale * gCurGraphNodeObject->scale[0];
        }

        s8 shifted = (gCurrAnimEnabled
                      && (gCurrAnimType == ANIM_TYPE_TRANSLATION
                       || gCurrAnimType == ANIM_TYPE_LATERAL_TRANSLATION)
        );

        if (shifted) {
            struct GraphNode *geo = node->node.children;
            f32 objScale = 1.0f;
            if (geo != NULL && geo->type == GRAPH_NODE_TYPE_SCALE) {
                objScale = ((struct GraphNodeScale *) geo)->scale;
            }

            f32 animScale = gCurrAnimTranslationMultiplier * objScale;
            Vec3f animOffset;
            animOffset[0] = gCurrAnimData[retrieve_animation_index(gCurrAnimFrame, &gCurrAnimAttribute)] * animScale;
            animOffset[1] = 0.0f;
            gCurrAnimAttribute += 2;
            animOffset[2] = gCurrAnimData[retrieve_animation_index(gCurrAnimFrame, &gCurrAnimAttribute)] * animScale;
            gCurrAnimAttribute -= 6;

            // simple matrix rotation so the shadow offset rotates along with the object
            f32 sinAng = sins(gCurGraphNodeObject->angle[1]);
            f32 cosAng = coss(gCurGraphNodeObject->angle[1]);

            shadowPos[0] += animOffset[0] * cosAng + animOffset[2] * sinAng;
            shadowPos[2] += -animOffset[0] * sinAng + animOffset[2] * cosAng;
        }

        Gfx *shadowList = create_shadow_below_xyz(shadowPos, shadowScale * 0.5f,
                                                  node->shadowSolidity, node->shadowType, shifted);

        if (shadowList != NULL) {
            mtxf_shadow(gMatStack[gMatStackIndex + 1],
                gCurrShadow.floorNormal, shadowPos, gCurrShadow.scale, gCurGraphNodeObject->angle[1]);

            inc_mat_stack();
            geo_append_display_list(
                (void *) VIRTUAL_TO_PHYSICAL(shadowList),
                gCurrShadow.isDecal ? LAYER_TRANSPARENT_DECAL : LAYER_TRANSPARENT
            );

            gMatStackIndex--;
        }
    }
#endif
    if (node->node.children != NULL) {
        geo_process_node_and_siblings(node->node.children);
    }
}

/**
 * Check whether an object is in view to determine whether it should be drawn.
 * This is known as frustum culling.
 * It checks whether the object is far away, very close / behind the camera,
 * or horizontally out of view. It does not check whether it is vertically
 * out of view. It assumes a sphere of 300 units around the object's position
 * unless the object has a culling radius node that specifies otherwise.
 *
 * The matrix parameter should be the top of the matrix stack, which is the
 * object's transformation matrix times the camera 'look-at' matrix. The math
 * is counter-intuitive, but it checks column 3 (translation vector) of this
 * matrix to determine where the origin (0,0,0) in object space will be once
 * transformed to camera space (x+ = right, y+ = up, z = 'coming out the screen').
 * In 3D graphics, you typically model the world as being moved in front of a
 * static camera instead of a moving camera through a static world, which in
 * this case simplifies calculations. Note that the perspective matrix is not
 * on the matrix stack, so there are still calculations with the fov to compute
 * the slope of the lines of the frustum.
 *
 *        z-
 *
 *  \     |     /
 *   \    |    /
 *    \   |   /
 *     \  |  /
 *      \ | /
 *       \|/
 *        C       x+
 *
 * Since (0,0,0) is unaffected by rotation, columns 0, 1 and 2 are ignored.
 */
s32 obj_is_in_view(struct GraphNodeObject *node) {
    if (node->node.flags & GRAPH_RENDER_INVISIBLE) {
        return FALSE;
    }

    struct GraphNode *geo = node->sharedChild;

    s16 cullingRadius;

    if (geo != NULL && geo->type == GRAPH_NODE_TYPE_CULLING_RADIUS) {
        cullingRadius = ((struct GraphNodeCullingRadius *) geo)->cullingRadius;
    } else {
        cullingRadius = 300;
    }

    // Don't render if the object is close to or behind the camera
    if (node->cameraToObject[2] > -100.0f + cullingRadius) {
        return FALSE;
    }

    //! This makes the HOLP not update when the camera is far away, and it
    //  makes PU travel safe when the camera is locked on the main map.
    //  If Mario were rendered with a depth over 65536 it would cause overflow
    //  when converting the transformation matrix to a fixed point matrix.
    if (node->cameraToObject[2] < -20000.0f - cullingRadius) {
        return FALSE;
    }

    // half of the fov in in-game angle units instead of degrees
    s16 halfFov = (((((gCurGraphNodeCamFrustum->fov * sAspectRatio) / 2.0f) + 1.0f) * 32768.0f) / 180.0f) + 0.5f;

    f32 hScreenEdge = -node->cameraToObject[2] * tans(halfFov);
    // -matrix[3][2] is the depth, which gets multiplied by tan(halfFov) to get
    // the amount of units between the center of the screen and the horizontal edge
    // given the distance from the object to the camera.

    // This multiplication should really be performed on 4:3 as well,
    // but the issue will be more apparent on widescreen.
    // HackerSM64: This multiplication is done regardless of aspect ratio to fix object pop-in on the edges of the screen (which happens at 4:3 too)
    // hScreenEdge *= GFX_DIMENSIONS_ASPECT_RATIO;

    // Check whether the object is horizontally in view
    if (node->cameraToObject[0] > hScreenEdge + cullingRadius) {
        return FALSE;
    }
    if (node->cameraToObject[0] < -hScreenEdge - cullingRadius) {
        return FALSE;
    }

    return TRUE;
}

#ifdef VISUAL_DEBUG
void visualise_object_hitbox(struct Object *node) {
    Vec3f bnds1, bnds2;
    // This will create a cylinder that visualises their hitbox.
    // If they do not have a hitbox, it will be a small white cube instead.
    if (node->oIntangibleTimer != -1) {
        vec3f_set(bnds1, node->oPosX, (node->oPosY - node->hitboxDownOffset), node->oPosZ);
        vec3f_set(bnds2, node->hitboxRadius, node->hitboxHeight-node->hitboxDownOffset, node->hitboxRadius);
        if (node->behavior == segmented_to_virtual(bhvWarp)
            || node->behavior == segmented_to_virtual(bhvDoorWarp)
            || node->behavior == segmented_to_virtual(bhvFadingWarp)) {
            debug_box_color(COLOR_RGBA32_DEBUG_WARP);
        } else {
            debug_box_color(COLOR_RGBA32_DEBUG_HITBOX);
        }

        debug_box(bnds1, bnds2, (DEBUG_SHAPE_CYLINDER | DEBUG_UCODE_REJ));
        vec3f_set(bnds1, node->oPosX, (node->oPosY - node->hitboxDownOffset), node->oPosZ);
        vec3f_set(bnds2, node->hurtboxRadius, node->hurtboxHeight, node->hurtboxRadius);
        debug_box_color(COLOR_RGBA32_DEBUG_HURTBOX);
        debug_box(bnds1, bnds2, (DEBUG_SHAPE_CYLINDER | DEBUG_UCODE_REJ));
    } else {
        vec3f_set(bnds1, node->oPosX, (node->oPosY - 15), node->oPosZ);
        vec3f_set(bnds2, 30, 30, 30);
        debug_box_color(COLOR_RGBA32_DEBUG_POSITION);
        debug_box(bnds1, bnds2, (DEBUG_SHAPE_BOX | DEBUG_UCODE_REJ));
    }
}
#endif

/**
 * Process an object node.
 */
void geo_process_object(struct Object *node) {
    s32 i;
    if (node->header.gfx.areaIndex == gCurGraphNodeRoot->areaIndex) {
        if (node->header.gfx.throwMatrix != NULL) {
            mtxf_scale_vec3f(gMatStack[gMatStackIndex + 1], *node->header.gfx.throwMatrix, node->header.gfx.scale);
        } else if (node->header.gfx.node.flags & GRAPH_RENDER_BILLBOARD) {
            mtxf_billboard(gMatStack[gMatStackIndex + 1], gMatStack[gMatStackIndex],
                           node->header.gfx.pos, node->header.gfx.scale, gCurGraphNodeCamera->roll);
        } else {
            mtxf_rotate_zxy_and_translate(gMatStack[gMatStackIndex + 1], node->header.gfx.pos, node->header.gfx.angle);
            mtxf_scale_vec3f(gMatStack[gMatStackIndex + 1], gMatStack[gMatStackIndex + 1], node->header.gfx.scale);
        }

        node->header.gfx.throwMatrix = &gMatStack[++gMatStackIndex];
        linear_mtxf_mul_vec3f_and_translate(gCameraTransform, node->header.gfx.cameraToObject, (*node->header.gfx.throwMatrix)[3]);

        // FIXME: correct types
        if (node->header.gfx.animInfo.curAnim != NULL) {
            geo_set_animation_globals(&node->header.gfx.animInfo, (node->header.gfx.node.flags & GRAPH_RENDER_HAS_ANIMATION) != 0);
        }

        if (obj_is_in_view(&node->header.gfx)) {            
            // Create the displaylist to set the active point lights
            Gfx* pointLightsDl = createPointLightsDl(&node->oPosX, 80.0f);

            // Put the lights on every layer, this can be optimized in the future
            // It will require some geolayout command to specify which layers this object uses
            // Maybe this can be implemented in a GEO_ASM call, where the parameter is a layer mask
            for (i = LAYER_FORCE; i <= LAYER_TRANSPARENT_INTER; i++)
            {
                geo_append_display_list(pointLightsDl, i);
            }
            gMatStackIndex--;
            inc_mat_stack();

            if (node->header.gfx.sharedChild != NULL) {
#ifdef VISUAL_DEBUG
                if (hitboxView) visualise_object_hitbox(node);
#endif
                gCurGraphNodeObject = (struct GraphNodeObject *) node;
                node->header.gfx.sharedChild->parent = &node->header.gfx.node;
                geo_process_node_and_siblings(node->header.gfx.sharedChild);
                node->header.gfx.sharedChild->parent = NULL;
                gCurGraphNodeObject = NULL;
            }
            if (node->header.gfx.node.children != NULL) {
                geo_process_node_and_siblings(node->header.gfx.node.children);
            }
        }

        gMatStackIndex--;
        gCurrAnimType = ANIM_TYPE_NONE;
        node->header.gfx.throwMatrix = NULL;
    }
}

/**
 * Process an object parent node. Temporarily assigns itself as the parent of
 * the subtree rooted at 'sharedChild' and processes the subtree, after which the
 * actual children are be processed. (in practice they are null though)
 */
void geo_process_object_parent(struct GraphNodeObjectParent *node) {
    if (node->sharedChild != NULL) {
        node->sharedChild->parent = (struct GraphNode *) node;
        geo_process_node_and_siblings(node->sharedChild);
        node->sharedChild->parent = NULL;
    }
    if (node->node.children != NULL) {
        geo_process_node_and_siblings(node->node.children);
    }
}

/**
 * Process a held object node.
 */
void geo_process_held_object(struct GraphNodeHeldObject *node) {
    Mat4 mat;
    Vec3f translation;
    Mat4 tempMtx;

#ifdef F3DEX_GBI_2
    gSPLookAt(gDisplayListHead++, gCurLookAt);
#endif

    if (node->fnNode.func != NULL) {
        node->fnNode.func(GEO_CONTEXT_RENDER, &node->fnNode.node, gMatStack[gMatStackIndex]);
    }
    if (node->objNode != NULL && node->objNode->header.gfx.sharedChild != NULL) {
        vec3_prod_val(translation, node->translation, 0.25f);

        mtxf_translate(mat, translation);
        mtxf_copy(gMatStack[gMatStackIndex + 1], *gCurGraphNodeObject->throwMatrix);
        vec3f_copy(gMatStack[gMatStackIndex + 1][3], gMatStack[gMatStackIndex][3]);
        mtxf_copy(tempMtx, gMatStack[gMatStackIndex + 1]);
        mtxf_mul(gMatStack[gMatStackIndex + 1], mat, tempMtx);
        mtxf_scale_vec3f(gMatStack[gMatStackIndex + 1], gMatStack[gMatStackIndex + 1], node->objNode->header.gfx.scale);

        if (node->fnNode.func != NULL) {
            node->fnNode.func(GEO_CONTEXT_HELD_OBJ, &node->fnNode.node, (struct AllocOnlyPool *) gMatStack[gMatStackIndex + 1]);
        }

        inc_mat_stack();
        gGeoTempState.type = gCurrAnimType;
        gGeoTempState.enabled = gCurrAnimEnabled;
        gGeoTempState.frame = gCurrAnimFrame;
        gGeoTempState.translationMultiplier = gCurrAnimTranslationMultiplier;
        gGeoTempState.attribute = gCurrAnimAttribute;
        gGeoTempState.data = gCurrAnimData;
        gCurrAnimType = ANIM_TYPE_NONE;
        gCurGraphNodeHeldObject = (void *) node;
        if (node->objNode->header.gfx.animInfo.curAnim != NULL) {
            geo_set_animation_globals(&node->objNode->header.gfx.animInfo, (node->objNode->header.gfx.node.flags & GRAPH_RENDER_HAS_ANIMATION) != 0);
        }

        geo_process_node_and_siblings(node->objNode->header.gfx.sharedChild);
        gCurGraphNodeHeldObject = NULL;
        gCurrAnimType = gGeoTempState.type;
        gCurrAnimEnabled = gGeoTempState.enabled;
        gCurrAnimFrame = gGeoTempState.frame;
        gCurrAnimTranslationMultiplier = gGeoTempState.translationMultiplier;
        gCurrAnimAttribute = gGeoTempState.attribute;
        gCurrAnimData = gGeoTempState.data;
        gMatStackIndex--;
    }

    if (node->fnNode.node.children != NULL) {
        geo_process_node_and_siblings(node->fnNode.node.children);
    }
}

#include <stdio.h>

/**
 * Advanced lighting engine
 * Processes a scene light, setting its position and other properties
 */
void geo_process_scene_light(struct GraphNodeSceneLight *node)
{    
    switch (node->lightType)
    {
        case LIGHT_TYPE_DIRECTIONAL:
            if (!gOverrideDirectionalLight)
            {
                // Set the directional light color
                gCurDirectionalLight->l->l.colc[0] = gCurDirectionalLight->l->l.col[0] = node->color[0];
                gCurDirectionalLight->l->l.colc[1] = gCurDirectionalLight->l->l.col[1] = node->color[1];
                gCurDirectionalLight->l->l.colc[2] = gCurDirectionalLight->l->l.col[2] = node->color[2];

                // Set the pre transformed light direction
                gLightDir[0] = node->a;
                gLightDir[1] = node->b;
                gLightDir[2] = node->c;
            }
            break;
        case LIGHT_TYPE_POINT:
        case LIGHT_TYPE_POINT_OCCLUDE:
            // Set the given point light's color
            node->light->l.pl.colc[0] = node->light->l.pl.col[0] = node->color[0];
            node->light->l.pl.colc[1] = node->light->l.pl.col[1] = node->color[1];
            node->light->l.pl.colc[2] = node->light->l.pl.col[2] = node->color[2];

            // Truncates, but is faster
            node->light->worldPos[0] = (s16)(s32)gMatStack[gMatStackIndex][3][0];
            node->light->worldPos[1] = (s16)(s32)gMatStack[gMatStackIndex][3][1];
            node->light->worldPos[2] = (s16)(s32)gMatStack[gMatStackIndex][3][2];

            // More accurate (rounding instead of flooring), but more costly
            //vec3f_to_vec3s(node->light->worldPos, pos);

            node->light->l.pl.quadratic_attenuation = node->a;
            node->light->l.pl.linear_attenuation = node->b;
            node->light->l.pl.constant_attenuation = (node->c == 0) ? 8 : node->c;
            break;
        case LIGHT_TYPE_AMBIENT:
            if (!gOverrideAmbientLight)
            {
                // Set the ambient light color
                gCurDirectionalLight->a.l.colc[0] = gCurDirectionalLight->a.l.col[0] = node->color[0];
                gCurDirectionalLight->a.l.colc[1] = gCurDirectionalLight->a.l.col[1] = node->color[1];
                gCurDirectionalLight->a.l.colc[2] = gCurDirectionalLight->a.l.col[2] = node->color[2];
            }
            break;
    }

    if (node->node.children != NULL) {
        geo_process_node_and_siblings(node->node.children);
    }
}

/**
 * Processes the children of the given GraphNode if it has any
 */
void geo_try_process_children(struct GraphNode *node) {
    if (node->children != NULL) {
        geo_process_node_and_siblings(node->children);
    }
}

/**
 * Process a generic geo node and its siblings.
 * The first argument is the start node, and all its siblings will
 * be iterated over.
 */
void geo_process_node_and_siblings(struct GraphNode *firstNode) {
    s32 iterateChildren = TRUE;
    struct GraphNode *curGraphNode = firstNode;
    struct GraphNode *parent = curGraphNode->parent;

    // In the case of a switch node, exactly one of the children of the node is
    // processed instead of all children like usual
    if (parent != NULL) {
        iterateChildren = (parent->type != GRAPH_NODE_TYPE_SWITCH_CASE);
    }

    do {
        if (curGraphNode->flags & GRAPH_RENDER_ACTIVE) {
            if (curGraphNode->flags & GRAPH_RENDER_CHILDREN_FIRST) {
                geo_try_process_children(curGraphNode);
            } else {
                switch (curGraphNode->type) {
                    case GRAPH_NODE_TYPE_ORTHO_PROJECTION:     geo_process_ortho_projection    ((struct GraphNodeOrthoProjection     *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_PERSPECTIVE:          geo_process_perspective         ((struct GraphNodePerspective         *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_MASTER_LIST:          geo_process_master_list         ((struct GraphNodeMasterList          *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_LEVEL_OF_DETAIL:      geo_process_level_of_detail     ((struct GraphNodeLevelOfDetail       *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_SWITCH_CASE:          geo_process_switch              ((struct GraphNodeSwitchCase          *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_CAMERA:               geo_process_camera              ((struct GraphNodeCamera              *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_TRANSLATION_ROTATION: geo_process_translation_rotation((struct GraphNodeTranslationRotation *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_TRANSLATION:          geo_process_translation         ((struct GraphNodeTranslation         *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_ROTATION:             geo_process_rotation            ((struct GraphNodeRotation            *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_OBJECT:               geo_process_object              ((struct Object                       *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_ANIMATED_PART:        geo_process_animated_part       ((struct GraphNodeAnimatedPart        *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_BILLBOARD:            geo_process_billboard           ((struct GraphNodeBillboard           *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_DISPLAY_LIST:         geo_process_display_list        ((struct GraphNodeDisplayList         *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_SCALE:                geo_process_scale               ((struct GraphNodeScale               *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_SHADOW:               geo_process_shadow              ((struct GraphNodeShadow              *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_OBJECT_PARENT:        geo_process_object_parent       ((struct GraphNodeObjectParent        *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_GENERATED_LIST:       geo_process_generated_list      ((struct GraphNodeGenerated           *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_BACKGROUND:           geo_process_background          ((struct GraphNodeBackground          *) curGraphNode); break;
                    case GRAPH_NODE_TYPE_HELD_OBJ:             geo_process_held_object         ((struct GraphNodeHeldObject          *) curGraphNode); break;
                    // Advanced lighting engine
                    case GRAPH_NODE_TYPE_SCENE_LIGHT:          geo_process_scene_light         ((struct GraphNodeSceneLight          *) curGraphNode); break;
                    default:                                   geo_try_process_children        ((struct GraphNode                    *) curGraphNode); break;
                }
            }
        } else {
            if (curGraphNode->type == GRAPH_NODE_TYPE_OBJECT) {
                ((struct GraphNodeObject *) curGraphNode)->throwMatrix = NULL;
            }
        }
    } while (iterateChildren && (curGraphNode = curGraphNode->next) != firstNode);
}

/**
 * Process a root node. This is the entry point for processing the scene graph.
 * The root node itself sets up the viewport, then all its children are processed
 * to set up the projection and draw display lists.
 */
void geo_process_root(struct GraphNodeRoot *node, Vp *b, Vp *c, s32 clearColor) {
    if (node->node.flags & GRAPH_RENDER_ACTIVE) {
        Mtx *initialMatrix;
        Vp *viewport = alloc_display_list(sizeof(*viewport));

        gDisplayListHeap = alloc_only_pool_init(main_pool_available() - sizeof(struct AllocOnlyPool), MEMORY_POOL_LEFT);
        initialMatrix = alloc_display_list(sizeof(*initialMatrix));
        gCurLookAt = (LookAt*)alloc_display_list(sizeof(LookAt));
        bzero(gCurLookAt, sizeof(LookAt));
        gCurLookAt->l[1].l.col[1] = 0x80;
        gCurLookAt->l[1].l.colc[1] = 0x80;
        gMatStackIndex = 0;
        gCurrAnimType = ANIM_TYPE_NONE;
        vec3s_set(viewport->vp.vtrans, node->x * 4, node->y * 4, 511);
        vec3s_set(viewport->vp.vscale, node->width * 4, node->height * 4, 511);

        if (b != NULL) {
            clear_framebuffer(clearColor);
            make_viewport_clip_rect(b);
            *viewport = *b;
        }

        else if (c != NULL) {
            clear_framebuffer(clearColor);
            make_viewport_clip_rect(c);
        }

        mtxf_identity(gMatStack[gMatStackIndex]);
        mtxf_to_mtx(initialMatrix, gMatStack[gMatStackIndex]);
        gMatStackFixed[gMatStackIndex] = initialMatrix;
        gSPViewport(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(viewport));
        gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(gMatStackFixed[gMatStackIndex]),
                  G_MTX_MODELVIEW | G_MTX_LOAD | G_MTX_NOPUSH);
        gCurGraphNodeRoot = node;
        if (node->node.children != NULL) {
            geo_process_node_and_siblings(node->node.children);
        }
        gCurGraphNodeRoot = NULL;
#ifdef VANILLA_DEBUG
        if (gShowDebugText) {
            print_text_fmt_int(180, 36, "MEM %d", gDisplayListHeap->totalSpace - gDisplayListHeap->usedSpace);
        }
#endif
        main_pool_free(gDisplayListHeap);
    }
}
