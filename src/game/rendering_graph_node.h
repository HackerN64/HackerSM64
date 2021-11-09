#ifndef RENDERING_GRAPH_NODE_H
#define RENDERING_GRAPH_NODE_H

#include <PR/ultratypes.h>

#include "engine/graph_node.h"

extern struct GraphNodeRoot        *gCurGraphNodeRoot;
extern struct GraphNodeMasterList  *gCurGraphNodeMasterList;
extern struct GraphNodePerspective *gCurGraphNodeCamFrustum;
extern struct GraphNodeCamera      *gCurGraphNodeCamera;
extern struct GraphNodeObject      *gCurGraphNodeObject;
extern struct GraphNodeHeldObject  *gCurGraphNodeHeldObject;
#define gCurGraphNodeObjectNode ((struct Object *)gCurGraphNodeObject)
extern u16 gAreaUpdateCounter;

enum AnimType {
    // after processing an object, the type is reset to this
    ANIM_TYPE_NONE,
    // Not all parts have full animation: to save space, some animations only
    // have xz, y, or no translation at all. All animations have rotations though
    ANIM_TYPE_TRANSLATION,
    ANIM_TYPE_VERTICAL_TRANSLATION,
    ANIM_TYPE_LATERAL_TRANSLATION,
    ANIM_TYPE_NO_TRANSLATION,
    // Every animation includes rotation, after processing any of the above
    // translation types the type is set to this
    ANIM_TYPE_ROTATION
};

#ifdef OBJECTS_REJ
enum HeadsList {
    LIST_HEADS_ZEX,
    LIST_HEADS_REJ,
};
#endif

#define IS_LAYER_ZB(    layer) (((layer) >= LAYER_ZB_FIRST    ) || ((layer) <= LAYER_ZB_LAST))
#define IS_LAYER_NON_ZB(layer) (((layer) >= LAYER_NON_ZB_FIRST) || ((layer) <= LAYER_LAST   ))

#if SILHOUETTE
#define IS_LAYER_SILHOUETTE(layer) (((layer) >= LAYER_SILHOUETTE_FIRST) || ((layer) <= LAYER_SILHOUETTE_LAST))
#ifdef OBJECTS_REJ
enum RenderPhase { // Silhouette, .rej
    RENDER_PHASE_ZEX_BG,
    RENDER_PHASE_REJ_ZB,
    RENDER_PHASE_ZEX_BEFORE_SILHOUETTE,
    RENDER_PHASE_REJ_SILHOUETTE,
    RENDER_PHASE_REJ_NON_SILHOUETTE,
    RENDER_PHASE_REJ_OCCLUDE_SILHOUETTE,
    RENDER_PHASE_ZEX_AFTER_SILHOUETTE,
    RENDER_PHASE_REJ_NON_ZB,
    RENDER_PHASE_END,
};
#else
enum RenderPhase { // Silhouette, no .rej
    RENDER_PHASE_ZEX_BEFORE_SILHOUETTE,
    RENDER_PHASE_ZEX_SILHOUETTE,
    RENDER_PHASE_ZEX_NON_SILHOUETTE,
    RENDER_PHASE_ZEX_OCCLUDE_SILHOUETTE,
    RENDER_PHASE_ZEX_AFTER_SILHOUETTE,
    RENDER_PHASE_END,
};
#endif
#else
#ifdef OBJECTS_REJ
enum RenderPhase { // No silhouette, .rej
    RENDER_PHASE_ZEX_BG,
    RENDER_PHASE_REJ_ZB,
    RENDER_PHASE_ZEX_ALL,
    RENDER_PHASE_REJ_NON_ZB,
    RENDER_PHASE_END,
};
#else
enum RenderPhase { // No silhouette, no .rej
    RENDER_PHASE_ZEX_ALL,
    RENDER_PHASE_END,
};
#endif
#endif
#define RENDER_PHASE_FIRST 0

void geo_process_node_and_siblings(struct GraphNode *firstNode);
void geo_process_root(struct GraphNodeRoot *node, Vp *b, Vp *c, s32 clearColor);

#endif // RENDERING_GRAPH_NODE_H
