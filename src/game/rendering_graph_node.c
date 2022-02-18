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
    /*0x08*/ AnimIndex *attribute;
    /*0x0C*/ AnimValue *data;
};

// For some reason, this is a GeoAnimState struct, but the current state consists
// of separate global variables. It won't match EU otherwise.
struct GeoAnimState gGeoTempState;

u8 gCurrAnimType;
u8 gCurrAnimEnabled;
s16 gCurrAnimFrame;
f32 gCurrAnimTranslationMultiplier;
AnimIndex *gCurrAnimAttribute;
AnimValue *gCurrAnimData;

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
        G_RM_AA_TEX_EDGE | ZMODE_DEC,       // LAYER_ALPHA_DECAL
#if SILHOUETTE
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
        G_RM_AA_ZB_TEX_EDGE | ZMODE_DEC,    // LAYER_ALPHA_DECAL
#if SILHOUETTE
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
        G_RM_AA_TEX_EDGE2 | ZMODE_DEC,      // LAYER_ALPHA_DECAL
#if SILHOUETTE
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
        G_RM_AA_ZB_TEX_EDGE2 | ZMODE_DEC,   // LAYER_ALPHA_DECAL
#if SILHOUETTE
        G_RM_AA_ZB_OPA_SURF2,               // LAYER_SILHOUETTE_OPAQUE
        G_RM_AA_ZB_TEX_EDGE2,               // LAYER_SILHOUETTE_ALPHA
        G_RM_AA_ZB_OPA_SURF2,               // LAYER_OCCLUDE_SILHOUETTE_OPAQUE
        G_RM_AA_ZB_TEX_EDGE2,               // LAYER_OCCLUDE_SILHOUETTE_ALPHA
#endif
        G_RM_AA_ZB_XLU_DECAL2,              // LAYER_TRANSPARENT_DECAL
        G_RM_AA_ZB_XLU_SURF2,               // LAYER_TRANSPARENT
        G_RM_AA_ZB_XLU_INTER2,              // LAYER_TRANSPARENT_INTER
    } } };

ALIGNED16 struct GraphNodeRoot        *gCurGraphNodeRoot       = NULL;
ALIGNED16 struct GraphNodeMasterList  *gCurGraphNodeMasterList = NULL;
ALIGNED16 struct GraphNodePerspective *gCurGraphNodeCamFrustum = NULL;
ALIGNED16 struct GraphNodeCamera      *gCurGraphNodeCamera     = NULL;
ALIGNED16 struct GraphNodeObject      *gCurGraphNodeObject     = NULL;
ALIGNED16 struct GraphNodeHeldObject  *gCurGraphNodeHeldObject = NULL;
u16 gAreaUpdateCounter = 0;

LookAt *gCurLookAt = NULL;

#if SILHOUETTE
// AA_EN        Enable anti aliasing (not actually used for AA in this case).
// IM_RD        Enable reading coverage value.
// CLR_ON_CVG   Don't change the color unless coverage overflows. This helps prevent triangle overlap.
// CVG_DST_WRAP Wrap the coverage value on overflow.
// CVG_X_ALPHA  Coverage and alpha will be multiplied and both will be the same. This makes texture alpha work (eg. Wing Cap wings).
// FORCE_BL     Force Blending.
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

//     enum RenderPhases                         startLayer                      endLayer                        ucode
static struct RenderPhase sRenderPhases[] = {
#ifdef OBJECTS_REJ
 #if SILHOUETTE
    // Silhouette, .rej
    /* RENDER_PHASE_ZEX_BEFORE_SILHOUETTE   */ { LAYER_FIRST,                    LAYER_LAST_BEFORE_SILHOUETTE,   GRAPH_NODE_UCODE_DEFAULT }, //  0 -  5, zex
    /* RENDER_PHASE_REJ_ZB                  */ { LAYER_ZB_FIRST,                 LAYER_LAST_BEFORE_SILHOUETTE,   GRAPH_NODE_UCODE_REJ     }, //  1 -  5, rej
    /* RENDER_PHASE_REJ_SILHOUETTE          */ { LAYER_SILHOUETTE_FIRST,         LAYER_SILHOUETTE_LAST,          GRAPH_NODE_UCODE_REJ     }, //  6 -  7, rej (silhouette)
    /* RENDER_PHASE_REJ_NON_SILHOUETTE      */ { LAYER_SILHOUETTE_FIRST,         LAYER_SILHOUETTE_LAST,          GRAPH_NODE_UCODE_REJ     }, //  6 -  7, rej (non-silhouette)
    /* RENDER_PHASE_REJ_OCCLUDE_SILHOUETTE  */ { LAYER_OCCLUDE_SILHOUETTE_FIRST, LAYER_OCCLUDE_SILHOUETTE_LAST,  GRAPH_NODE_UCODE_REJ     }, //  8 -  9, rej
    /* RENDER_PHASE_ZEX_AFTER_SILHOUETTE    */ { LAYER_OCCLUDE_SILHOUETTE_FIRST, LAYER_LAST,                     GRAPH_NODE_UCODE_DEFAULT }, //  8 - 12, zex
    /* RENDER_PHASE_REJ_NON_ZB              */ { LAYER_NON_ZB_FIRST,             LAYER_LAST,                     GRAPH_NODE_UCODE_REJ     }, // 10 - 12, rej
 #else
    // No silhouette, .rej
    /* RENDER_PHASE_ZEX_BG                  */ { LAYER_FIRST,                    LAYER_FIRST,                    GRAPH_NODE_UCODE_DEFAULT }, // 0 - 0, zex
    /* RENDER_PHASE_REJ_ZB                  */ { LAYER_ZB_FIRST,                 LAYER_ZB_LAST,                  GRAPH_NODE_UCODE_REJ     }, // 1 - 5, rej
    /* RENDER_PHASE_ZEX_ALL                 */ { LAYER_ZB_FIRST,                 LAYER_LAST,                     GRAPH_NODE_UCODE_DEFAULT }, // 1 - 8, zex
    /* RENDER_PHASE_REJ_NON_ZB              */ { LAYER_NON_ZB_FIRST,             LAYER_LAST,                     GRAPH_NODE_UCODE_REJ     }, // 6 - 8, rej
 #endif
#else
 #if SILHOUETTE
    // Silhouette, no .rej
    /* RENDER_PHASE_ZEX_BEFORE_SILHOUETTE   */ { LAYER_FIRST,                    LAYER_LAST_BEFORE_SILHOUETTE   }, //  0 -  5
    /* RENDER_PHASE_ZEX_SILHOUETTE          */ { LAYER_SILHOUETTE_FIRST,         LAYER_SILHOUETTE_LAST          }, //  6 -  7 (silhouette)
    /* RENDER_PHASE_ZEX_NON_SILHOUETTE      */ { LAYER_SILHOUETTE_FIRST,         LAYER_SILHOUETTE_LAST          }, //  6 -  7 (non-silhouette)
    /* RENDER_PHASE_ZEX_OCCLUDE_SILHOUETTE  */ { LAYER_OCCLUDE_SILHOUETTE_FIRST, LAYER_OCCLUDE_SILHOUETTE_LAST  }, //  8 -  9
    /* RENDER_PHASE_ZEX_AFTER_SILHOUETTE    */ { LAYER_NON_ZB_FIRST,             LAYER_LAST                     }, // 10 - 12
 #else
    // No silhouette, no .rej
    /* RENDER_PHASE_ZEX_ALL                 */ { LAYER_FIRST,                    LAYER_LAST                     }, // 0 - 8
 #endif
#endif
};

extern const Gfx init_rsp[];

#ifdef OBJECTS_REJ
void switch_ucode(Gfx* dlHead, s32 ucode) {
    // Set the ucode and RCP settings
    switch (ucode) {
        default: // GRAPH_NODE_UCODE_DEFAULT
        case GRAPH_NODE_UCODE_DEFAULT:
            gSPLoadUcodeL(dlHead++, gspF3DZEX2_NoN_PosLight_fifo); // F3DZEX2_PosLight
            // Reload the necessary RSP settings
            gSPDisplayList(dlHead++, init_rsp);
            break;
        case GRAPH_NODE_UCODE_REJ:
            // Use .rej Microcode, skip sub-pixel processing on console.
            if (gIsConsole) {
                gSPLoadUcodeL(dlHead++, gspF3DLX2_Rej_fifo); // F3DLX2_Rej
            } else {
                gSPLoadUcodeL(dlHead++, gspF3DEX2_Rej_fifo); // F3DEX2_Rej
            }
            // Reload the necessary RSP settings.
            gSPDisplayList(dlHead++, init_rsp);
            // Set the clip ratio (see init_rsp).
            gSPClipRatio(dlHead++, FRUSTRATIO_2);
            break;
    }
}
#endif

/**
 * Process a master list node. This has been modified, so now it iterates through sRenderPhases to determine the draw order.
 */
void geo_process_master_list_sub(struct GraphNodeMasterList *node) {
    struct RenderPhase *renderPhase;
    struct DisplayListNode *currList;
    s32 currLayer     = LAYER_FIRST;
    s32 startLayer    = LAYER_FIRST;
    s32 endLayer      = LAYER_LAST;
    s32 ucode         = GRAPH_NODE_UCODE_DEFAULT;
    s32 phaseIndex    = RENDER_PHASE_FIRST;
    s32 enableZBuffer = ((node->node.flags & GRAPH_RENDER_Z_BUFFER) != 0);
    struct RenderModeContainer *mode1List = &renderModeTable_1Cycle[enableZBuffer];
    struct RenderModeContainer *mode2List = &renderModeTable_2Cycle[enableZBuffer];

    Gfx* dlHead = gDisplayListHead;

    // Loop through the render phases
    for (phaseIndex = RENDER_PHASE_FIRST; phaseIndex < RENDER_PHASE_END; phaseIndex++) {
        // Get the render phase information.
        renderPhase = &sRenderPhases[phaseIndex];
        startLayer = renderPhase->startLayer;
        endLayer   = renderPhase->endLayer;
#ifdef OBJECTS_REJ
        if (ucode != renderPhase->ucode) {
            ucode = renderPhase->ucode;
            // Set the ucode for the current render phase
            switch_ucode(dlHead, ucode);
            gSPLookAt(dlHead++, gCurLookAt);
        }
#endif
        if (enableZBuffer) {
            // Enable z buffer.
            gDPPipeSync(dlHead++);
            gSPSetGeometryMode(dlHead++, G_ZBUFFER);
        }
        // Iterate through the layers on the current render phase.
        for (currLayer = startLayer; currLayer <= endLayer; currLayer++) {
            // Set 'currList' to the first DisplayListNode on the current layer.
            currList = node->listHeads[ucode][currLayer];
#if defined(DISABLE_AA) || !SILHOUETTE
            // Set the render mode for the current layer.
            gDPSetRenderMode(dlHead++, mode1List->modes[currLayer],
                                       mode2List->modes[currLayer]);
#else
            if (phaseIndex == RENDER_PHASE_NON_SILHOUETTE) {
                // To properly cover the silhouette, disable AA.
                // The silhouette model does not have AA due to the hack used to prevent triangle overlap.
                gDPSetRenderMode(dlHead++, (mode1List->modes[currLayer] & ~IM_RD),
                                           (mode2List->modes[currLayer] & ~IM_RD));
            } else {
                // Set the render mode for the current dl.
                gDPSetRenderMode(dlHead++, mode1List->modes[currLayer],
                                           mode2List->modes[currLayer]);
            }
#endif
            // Iterate through all the displaylists on the current layer.
            while (currList != NULL) {
                // Add the display list's transformation to the master list.
                gSPMatrix(dlHead++, VIRTUAL_TO_PHYSICAL(currList->transform),
                          (G_MTX_MODELVIEW | G_MTX_LOAD | G_MTX_NOPUSH));
#if SILHOUETTE
                if (phaseIndex == RENDER_PHASE_SILHOUETTE) {
                    // Add the current display list to the master list, with silhouette F3D.
                    gSPDisplayList(dlHead++, dl_silhouette_begin);
                    gSPDisplayList(dlHead++, currList->displayList);
                    gSPDisplayList(dlHead++, dl_silhouette_end);
                } else {
                    // Add the current display list to the master list.
                    gSPDisplayList(dlHead++, currList->displayList);
                }
#else
                // Add the current display list to the master list.
                gSPDisplayList(dlHead++, currList->displayList);
#endif
                // Move to the next DisplayListNode.
                currList = currList->next;
            }
        }
    }

    if (enableZBuffer) {
        // Disable z buffer.
        gDPPipeSync(dlHead++);
        gSPClearGeometryMode(dlHead++, G_ZBUFFER);
    }

    gDisplayListHead = dlHead;

#ifdef OBJECTS_REJ
 #if defined(F3DEX_GBI_2) && defined(VISUAL_DEBUG)
    if (gVisualHitboxView) {
        render_debug_boxes(DEBUG_UCODE_REJ);
    }
 #endif
    switch_ucode(gDisplayListHead, GRAPH_NODE_UCODE_DEFAULT);
#endif

#ifdef VISUAL_DEBUG
    if (gVisualHitboxView) {
        render_debug_boxes(DEBUG_UCODE_DEFAULT | DEBUG_BOX_CLEAR);
    }
    if (gVisualSurfaceView) {
        // Renders twice, once as decal and once as non-decal.
        visual_surface_loop(FALSE);
        visual_surface_loop(TRUE);
    }
#endif
}

/**
 * Appends the display list to one of the master lists based on the layer
 * parameter. Look at the RenderModeContainer struct to see the corresponding
 * render modes of layers.
 */
void geo_append_display_list(void *displayList, s32 layer) {
    s32 ucode = GRAPH_NODE_UCODE_DEFAULT;

    gSPLookAt(gDisplayListHead++, gCurLookAt);

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
#endif // OBJECTS_REJ || SILHOUETTE
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

void inc_mat_stack() {
    Mtx *mtx = alloc_display_list(sizeof(*mtx));
    gMatStackIndex++;
    mtxf_to_mtx(mtx, gMatStack[gMatStackIndex]);
    gMatStackFixed[gMatStackIndex] = mtx;
}

void append_dl_and_return(struct GraphNodeDisplayList *node) {
    if (node->displayList != NULL) {
        geo_append_display_list(node->displayList, node->node.drawingLayer);
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
        f32 scale = node->scale * construct_float(0.5f);
        f32 left   = (gCurGraphNodeRoot->x - gCurGraphNodeRoot->width ) * scale;
        f32 right  = (gCurGraphNodeRoot->x + gCurGraphNodeRoot->width ) * scale;
        f32 top    = (gCurGraphNodeRoot->y - gCurGraphNodeRoot->height) * scale;
        f32 bottom = (gCurGraphNodeRoot->y + gCurGraphNodeRoot->height) * scale;

        Gfx* dlHead = gDisplayListHead;

        guOrtho(mtx, left, right, bottom, top, construct_float(-2.0f), construct_float(2.0f), construct_float(1.0f));
        gSPPerspNormalize(dlHead++, 0xFFFF);
        gSPMatrix(dlHead++, VIRTUAL_TO_PHYSICAL(mtx), (G_MTX_PROJECTION | G_MTX_LOAD | G_MTX_NOPUSH));

        gDisplayListHead = dlHead;

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
            sAspectRatio = construct_float(16.0f / 9.0f); // 1.775f
        } else {
            sAspectRatio = construct_float(4.0f / 3.0f); // 1.33333f
        }
#else
        sAspectRatio = construct_float(4.0f / 3.0f); // 1.33333f
#endif

        Gfx* dlHead = gDisplayListHead;

        guPerspective(mtx, &perspNorm, node->fov, sAspectRatio, node->near, node->far, 1.0f);
        gSPPerspNormalize(dlHead++, perspNorm);

        gSPMatrix(dlHead++, VIRTUAL_TO_PHYSICAL(mtx), (G_MTX_PROJECTION | G_MTX_LOAD | G_MTX_NOPUSH));

        gDisplayListHead = dlHead;

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

Mat4 gCameraTransform;

Lights1 defaultLight = gdSPDefLights1(
    0x3F, 0x3F, 0x3F, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00
);

Vec3f globalLightDirection = {
    0x28, 0x28, 0x28
};

void setup_global_light() {
    Lights1 *curLight = (Lights1*)alloc_display_list(sizeof(Lights1));
    bcopy(&defaultLight, curLight, sizeof(Lights1));

    Vec3f transformedLightDirection;

    linear_mtxf_transpose_mul_vec3f(gCameraTransform, transformedLightDirection, globalLightDirection);

    curLight->l->l.dir[0] = (s8)(transformedLightDirection[0]);
    curLight->l->l.dir[1] = (s8)(transformedLightDirection[1]);
    curLight->l->l.dir[2] = (s8)(transformedLightDirection[2]);
    gSPSetLights1(gDisplayListHead++, (*curLight));
}

/**
 * Process a camera node.
 */
void geo_process_camera(struct GraphNodeCamera *node) {
    Mtx *rollMtx = alloc_display_list(sizeof(*rollMtx));
    Mtx *viewMtx = alloc_display_list(sizeof(Mtx));

    if (node->fnNode.func != NULL) {
        node->fnNode.func(GEO_CONTEXT_RENDER, &node->fnNode.node, gMatStack[gMatStackIndex]);
    }

    mtx_rotate_xy(rollMtx, node->rollScreen);

    gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(rollMtx), (G_MTX_PROJECTION | G_MTX_MUL | G_MTX_NOPUSH));

    mtxf_lookat(gCameraTransform, node->pos, node->focus, node->roll);

    // Calculate the lookAt
    Mat4 *cameraMatrix = &gCameraTransform;
#ifdef FIX_REFLECT_MTX
    gCurLookAt->l[0].l.dir[0] = (s8)(127.0f *  (*cameraMatrix)[0][0]);
    gCurLookAt->l[0].l.dir[1] = (s8)(127.0f *  (*cameraMatrix)[1][0]);
    gCurLookAt->l[0].l.dir[2] = (s8)(127.0f *  (*cameraMatrix)[2][0]);
    gCurLookAt->l[1].l.dir[0] = (s8)(127.0f * -(*cameraMatrix)[0][1]);
    gCurLookAt->l[1].l.dir[1] = (s8)(127.0f * -(*cameraMatrix)[1][1]);
    gCurLookAt->l[1].l.dir[2] = (s8)(127.0f * -(*cameraMatrix)[2][1]);
#else
    gCurLookAt->l[0].l.dir[0] = (s8)(127.0f *  (*cameraMatrix)[0][0]);
    gCurLookAt->l[0].l.dir[1] = (s8)(127.0f *  (*cameraMatrix)[1][0]);
    gCurLookAt->l[0].l.dir[2] = (s8)(127.0f *  (*cameraMatrix)[2][0]);
    gCurLookAt->l[1].l.dir[0] = (s8)(127.0f *  (*cameraMatrix)[0][1]);
    gCurLookAt->l[1].l.dir[1] = (s8)(127.0f *  (*cameraMatrix)[1][1]);
    gCurLookAt->l[1].l.dir[2] = (s8)(127.0f *  (*cameraMatrix)[2][1]);
#endif

    // Make a copy of the view matrix and scale it based on WORLD_SCALE
    Mat4 scaledCamera;
    mtxf_copy(scaledCamera, gCameraTransform);
    for (s32 i = 0; i < 3; i++) {
        for (s32 j = 0; j < 3; j++) {
            scaledCamera[i][j] *= WORLD_SCALE;
        }
    }

    // Convert the scaled matrix to fixed-point and integrate it into the projection matrix stack
    guMtxF2L(scaledCamera, viewMtx);
    gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(viewMtx),
              (G_MTX_PROJECTION | G_MTX_MUL | G_MTX_NOPUSH));
    setup_global_light();

    if (node->fnNode.node.children != 0) {
        gCurGraphNodeCamera = node;
        node->matrixPtr = &gCameraTransform;
        geo_process_node_and_siblings(node->fnNode.node.children);
        gCurGraphNodeCamera = NULL;
    }
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
    Vec3f scale;

    vec3s_to_vec3f(translation, node->translation);

    if (gCurGraphNodeHeldObject != NULL) {
        vec3f_copy(scale, gCurGraphNodeHeldObject->objNode->header.gfx.scale);
    } else if (gCurGraphNodeObject != NULL) {
        vec3f_copy(scale, gCurGraphNodeObject->scale);
    } else {
        vec3_same(scale, construct_float(1.0f));
    }

    mtxf_billboard(gMatStack[gMatStackIndex + 1], gMatStack[gMatStackIndex], translation, scale, gCurGraphNodeCamera->roll);

    inc_mat_stack();
    append_dl_and_return((struct GraphNodeDisplayList *)node);
}

/**
 * Process a Z offset node. It shifts the node either towards or away from the camera,
 * keeping it centered on the original position in view.
 */
void geo_process_z_offset(struct GraphNodeZOffset *node) {
    f32 zOffset = node->zOffset;
    Vec3f dirFromCamera;

    if (zOffset != 0 && gCurGraphNodeCamera != NULL) {
        vec3f_diff(dirFromCamera, gMatStack[gMatStackIndex][3], gCurGraphNodeCamera->pos);
        vec3f_normalize(dirFromCamera);

        gMatStack[gMatStackIndex][3][0] += dirFromCamera[0] * zOffset;
        gMatStack[gMatStackIndex][3][1] += dirFromCamera[1] * zOffset;
        gMatStack[gMatStackIndex][3][2] += dirFromCamera[2] * zOffset;
    }

    mtxf_copy(gMatStack[gMatStackIndex + 1], gMatStack[gMatStackIndex]);

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
            geo_append_display_list((void *) VIRTUAL_TO_PHYSICAL(list), node->fnNode.node.drawingLayer);
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
        geo_append_display_list((void *) VIRTUAL_TO_PHYSICAL(list), node->fnNode.node.drawingLayer);
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
                         (GFX_DIMENSIONS_RECT_FROM_RIGHT_EDGE(0) - 1), (SCREEN_HEIGHT - gBorderHeight - 1));
        gDPPipeSync(gfx++);
        gDPSetCycleType(gfx++, G_CYC_1CYCLE);
        gSPEndDisplayList(gfx++);

        geo_append_display_list((void *) VIRTUAL_TO_PHYSICAL(gfxStart), LAYER_FORCE);
    }
    if (node->fnNode.node.children != NULL) {
        geo_process_node_and_siblings(node->fnNode.node.children);
    }
}

static s16 get_curr_anim_info(void) {
    return gCurrAnimData[retrieve_animation_index(gCurrAnimFrame, &gCurrAnimAttribute)];
}

/**
 * Render an animated part. The current animation state is not part of the node
 * but set in global variables. If an animated part is skipped, everything afterwards desyncs.
 */
void geo_process_animated_part(struct GraphNodeAnimatedPart *node) {
    Vec3s rotation = { 0, 0, 0 };
    Vec3f translation = { node->translation[0], node->translation[1], node->translation[2] };

    switch (gCurrAnimType) {
        case ANIM_TYPE_TRANSLATION:
            translation[0] += get_curr_anim_info() * gCurrAnimTranslationMultiplier;
            translation[1] += get_curr_anim_info() * gCurrAnimTranslationMultiplier;
            translation[2] += get_curr_anim_info() * gCurrAnimTranslationMultiplier;
            gCurrAnimType = ANIM_TYPE_ROTATION;
            break;
        case ANIM_TYPE_VERTICAL_TRANSLATION:
            gCurrAnimAttribute += 2;
            translation[1] += get_curr_anim_info() * gCurrAnimTranslationMultiplier;
            gCurrAnimAttribute += 2;
            gCurrAnimType = ANIM_TYPE_ROTATION;
            break;
        case ANIM_TYPE_LATERAL_TRANSLATION:
            translation[0] += get_curr_anim_info() * gCurrAnimTranslationMultiplier;
            gCurrAnimAttribute += 2;
            translation[2] += get_curr_anim_info() * gCurrAnimTranslationMultiplier;
            gCurrAnimType = ANIM_TYPE_ROTATION;
            break;
        case ANIM_TYPE_NO_TRANSLATION:
            gCurrAnimAttribute += 6;
            gCurrAnimType = ANIM_TYPE_ROTATION;
            break;
    }

    if (gCurrAnimType == ANIM_TYPE_ROTATION) {
        rotation[0] = get_curr_anim_info();
        rotation[1] = get_curr_anim_info();
        rotation[2] = get_curr_anim_info();
    }

    mtxf_rotate_xyz_and_translate_and_mul(rotation, translation, gMatStack[gMatStackIndex + 1], gMatStack[gMatStackIndex]);

    inc_mat_stack();
    append_dl_and_return((struct GraphNodeDisplayList *)node);
}

/**
 * Render an animated part that has an initial rotation value
 */
void geo_process_bone(struct GraphNodeBone *node) {
    Vec3s rotation    = { node->rotation[0],    node->rotation[1],    node->rotation[2]    };
    Vec3f translation = { node->translation[0], node->translation[1], node->translation[2] };

    switch (gCurrAnimType) {
        case ANIM_TYPE_TRANSLATION:
            translation[0] += get_curr_anim_info() * gCurrAnimTranslationMultiplier;
            translation[1] += get_curr_anim_info() * gCurrAnimTranslationMultiplier;
            translation[2] += get_curr_anim_info() * gCurrAnimTranslationMultiplier;
            gCurrAnimType = ANIM_TYPE_ROTATION;
            break;
        case ANIM_TYPE_VERTICAL_TRANSLATION:
            gCurrAnimAttribute += 2;
            translation[1] += get_curr_anim_info() * gCurrAnimTranslationMultiplier;
            gCurrAnimAttribute += 2;
            gCurrAnimType = ANIM_TYPE_ROTATION;
            break;
        case ANIM_TYPE_LATERAL_TRANSLATION:
            translation[0] += get_curr_anim_info() * gCurrAnimTranslationMultiplier;
            gCurrAnimAttribute += 2;
            translation[2] += get_curr_anim_info() * gCurrAnimTranslationMultiplier;
            gCurrAnimType = ANIM_TYPE_ROTATION;
            break;
        case ANIM_TYPE_NO_TRANSLATION:
            gCurrAnimAttribute += 6;
            gCurrAnimType = ANIM_TYPE_ROTATION;
            break;
    }

    if (gCurrAnimType == ANIM_TYPE_ROTATION) {
        rotation[0] += get_curr_anim_info();
        rotation[1] += get_curr_anim_info();
        rotation[2] += get_curr_anim_info();
    }

    mtxf_rotate_xyz_and_translate_and_mul(rotation, translation, gMatStack[gMatStackIndex + 1], gMatStack[gMatStackIndex]);

    inc_mat_stack();
    append_dl_and_return((struct GraphNodeDisplayList *)node);
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
        gCurrAnimTranslationMultiplier = construct_float(1.0f);
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
            f32 objScale = construct_float(1.0f);
            if (geo != NULL && geo->type == GRAPH_NODE_TYPE_SCALE) {
                objScale = ((struct GraphNodeScale *) geo)->scale;
            }

            f32 animScale = gCurrAnimTranslationMultiplier * objScale;
            Vec3f animOffset;
            animOffset[0] = get_curr_anim_info() * animScale;
            animOffset[1] = 0.0f;
            gCurrAnimAttribute += 2;
            animOffset[2] = get_curr_anim_info() * animScale;
            gCurrAnimAttribute -= 6;

            // simple matrix rotation so the shadow offset rotates along with the object
            f32 sinAng = sins(gCurGraphNodeObject->angle[1]);
            f32 cosAng = coss(gCurGraphNodeObject->angle[1]);

            shadowPos[0] += ( animOffset[0] * cosAng) + (animOffset[2] * sinAng);
            shadowPos[2] += (-animOffset[0] * sinAng) + (animOffset[2] * cosAng);
        }

        Gfx *shadowList = create_shadow_below_xyz(shadowPos, (shadowScale * construct_float(0.5f)),
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
    // Don't render if the object is invisible.
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

    // Don't render if the object is close to or behind the camera.
    if (node->cameraToObject[2] > -100 + cullingRadius) {
        return FALSE;
    }

    //! This makes the HOLP not update when the camera is far away, and it
    //  makes PU travel safe when the camera is locked on the main map.
    //  If Mario were rendered with a depth over 65536 it would cause overflow
    //  when converting the transformation matrix to a fixed point matrix.
    if (node->cameraToObject[2] < -20000 - cullingRadius) {
        return FALSE;
    }

    // half of the fov in in-game angle units instead of degrees
    s16 halfFov = (degrees_to_angle(((gCurGraphNodeCamFrustum->fov * sAspectRatio) * construct_float(0.5f)) + construct_float(1.0f)) + construct_float(0.5f));

    f32 hScreenEdge = -node->cameraToObject[2] * tans(halfFov);
    // -node->cameraToObject[2] is the depth, which gets multiplied by tan(halfFov)
    // to getthe amount of units between the center of the screen and the horizontal
    // edge given the distance from the object to the camera.

    // This multiplication should really be performed on 4:3 as well,
    // but the issue will be more apparent on widescreen.
    // HackerSM64: This multiplication is done regardless of aspect ratio to fix object pop-in on the edges of the screen (which happens at 4:3 too)
    // hScreenEdge *= GFX_DIMENSIONS_ASPECT_RATIO;

    // The edges of the view
    f32 max = ( hScreenEdge + cullingRadius);
    f32 min = (-hScreenEdge - cullingRadius);

    // Check whether the object is horizontally in view
    if (node->cameraToObject[0] > max || node->cameraToObject[0] < min) return FALSE;
#ifdef VERTICAL_CULLING
    // Check whether the object is vertically in view
    if (node->cameraToObject[1] > max || node->cameraToObject[1] < min) return FALSE;
#endif

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
            gMatStackIndex--;
            inc_mat_stack();

            if (node->header.gfx.sharedChild != NULL) {
#ifdef VISUAL_DEBUG
                if (gVisualHitboxView) {
                    visualise_object_hitbox(node);
                }
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
    Vec3f translation;

    gSPLookAt(gDisplayListHead++, gCurLookAt);

    if (node->fnNode.func != NULL) {
        node->fnNode.func(GEO_CONTEXT_RENDER, &node->fnNode.node, gMatStack[gMatStackIndex]);
    }
    if (node->objNode != NULL && node->objNode->header.gfx.sharedChild != NULL) {
        vec3_prod_val(translation, node->translation, 0.25f);

        mtxf_held_object(gMatStack[gMatStackIndex + 1], gMatStack[gMatStackIndex], *gCurGraphNodeObject->throwMatrix, translation, node->objNode->header.gfx.scale);

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

/**
 * Processes the children of the given GraphNode if it has any
 */
void geo_try_process_children(struct GraphNode *node) {
    if (node->children != NULL) {
        geo_process_node_and_siblings(node->children);
    }
}

typedef void (*GeoProcessFunc)(struct GraphNode *node);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wincompatible-pointer-types"

// See enum 'GraphNodeTypes' in 'graph_node.h'.
static GeoProcessFunc GeoProcessJumpTable[] = {
    /*GRAPH_NODE_TYPE_ORTHO_PROJECTION      */ geo_process_ortho_projection,
    /*GRAPH_NODE_TYPE_PERSPECTIVE           */ geo_process_perspective,
    /*GRAPH_NODE_TYPE_MASTER_LIST           */ geo_process_master_list,
    /*GRAPH_NODE_TYPE_LEVEL_OF_DETAIL       */ geo_process_level_of_detail,
    /*GRAPH_NODE_TYPE_SWITCH_CASE           */ geo_process_switch,
    /*GRAPH_NODE_TYPE_CAMERA                */ geo_process_camera,
    /*GRAPH_NODE_TYPE_TRANSLATION_ROTATION  */ geo_process_translation_rotation,
    /*GRAPH_NODE_TYPE_OBJECT                */ geo_process_object,
    /*GRAPH_NODE_TYPE_ANIMATED_PART         */ geo_process_animated_part,
    /*GRAPH_NODE_TYPE_BONE                  */ geo_process_bone,
    /*GRAPH_NODE_TYPE_BILLBOARD             */ geo_process_billboard,
    /*GRAPH_NODE_TYPE_DISPLAY_LIST          */ geo_process_display_list,
    /*GRAPH_NODE_TYPE_SCALE                 */ geo_process_scale,
    /*GRAPH_NODE_TYPE_SHADOW                */ geo_process_shadow,
    /*GRAPH_NODE_TYPE_OBJECT_PARENT         */ geo_process_object_parent,
    /*GRAPH_NODE_TYPE_GENERATED_LIST        */ geo_process_generated_list,
    /*GRAPH_NODE_TYPE_BACKGROUND            */ geo_process_background,
    /*GRAPH_NODE_TYPE_HELD_OBJ              */ geo_process_held_object,
    /*GRAPH_NODE_TYPE_Z_OFFSET              */ geo_process_z_offset,
    /*GRAPH_NODE_TYPE_CULLING_RADIUS        */ geo_try_process_children,
    /*GRAPH_NODE_TYPE_ROOT                  */ geo_try_process_children,
    /*GRAPH_NODE_TYPE_START                 */ geo_try_process_children,
};

#pragma GCC diagnostic pop

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
                GeoProcessJumpTable[curGraphNode->type](curGraphNode);
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

        gDisplayListHeap = alloc_only_pool_init((main_pool_available() - sizeof(struct AllocOnlyPool)), MEMORY_POOL_LEFT);
        initialMatrix = alloc_display_list(sizeof(*initialMatrix));
        gCurLookAt = (LookAt*)alloc_display_list(sizeof(LookAt));
        bzero(gCurLookAt, sizeof(LookAt));
        gCurLookAt->l[1].l.col[1] = 0x80;
        gCurLookAt->l[1].l.colc[1] = 0x80;
        gMatStackIndex = 0;
        gCurrAnimType = ANIM_TYPE_NONE;
        vec3s_set(viewport->vp.vtrans, (node->x     * 4), (node->y      * 4), 511);
        vec3s_set(viewport->vp.vscale, (node->width * 4), (node->height * 4), 511);

        if (b != NULL) {
            clear_framebuffer(clearColor);
            make_viewport_clip_rect(b);
            *viewport = *b;
        } else if (c != NULL) {
            clear_framebuffer(clearColor);
            make_viewport_clip_rect(c);
        }

        mtxf_identity(gMatStack[gMatStackIndex]);
        mtxf_to_mtx(initialMatrix, gMatStack[gMatStackIndex]);
        gMatStackFixed[gMatStackIndex] = initialMatrix;

        Gfx* dlHead = gDisplayListHead;

        gSPViewport(dlHead++, VIRTUAL_TO_PHYSICAL(viewport));
        gSPMatrix(dlHead++, VIRTUAL_TO_PHYSICAL(gMatStackFixed[gMatStackIndex]),
                  (G_MTX_MODELVIEW | G_MTX_LOAD | G_MTX_NOPUSH));

        gDisplayListHead = dlHead;

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
