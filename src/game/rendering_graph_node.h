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
extern Vec3f globalLightDirection;

#define GRAPH_ROOT_PERSP 0
#define GRAPH_ROOT_ORTHO 1

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

#define IS_LAYER_ZB(    layer) (((layer) >= LAYER_ZB_FIRST    ) || ((layer) <= LAYER_ZB_LAST))
#define IS_LAYER_NON_ZB(layer) (((layer) >= LAYER_NON_ZB_FIRST) || ((layer) <= LAYER_LAST   ))

enum RenderModeTypes {
    RENDER_NO_ZB = 0,
    RENDER_ZB,
};

struct RenderModeContainer {
    u32 modes[LAYER_COUNT];
};

#if SILHOUETTE
    enum RenderPhases {
        RENDER_PHASE_ZEX_BEFORE_SILHOUETTE,
        RENDER_PHASE_ZEX_SILHOUETTE,
        RENDER_PHASE_ZEX_NON_SILHOUETTE,
        RENDER_PHASE_ZEX_OCCLUDE_SILHOUETTE,
        RENDER_PHASE_ZEX_AFTER_SILHOUETTE,
        RENDER_PHASE_END,
    };
    #define RENDER_PHASE_SILHOUETTE RENDER_PHASE_ZEX_SILHOUETTE
    #define RENDER_PHASE_NON_SILHOUETTE RENDER_PHASE_ZEX_NON_SILHOUETTE
#else
    enum RenderPhases {
        RENDER_PHASE_ZEX_ALL,
        RENDER_PHASE_END,
    };
#endif

#if SILHOUETTE
#define IS_LAYER_SILHOUETTE(layer) (((layer) >= LAYER_SILHOUETTE_FIRST) || ((layer) <= LAYER_SILHOUETTE_LAST))
#endif

#define RENDER_PHASE_FIRST 0

void geo_process_node_and_siblings(struct GraphNode *firstNode);
void geo_process_root(struct GraphNodeRoot *node, Vp *b, Vp *c, s32 clearColor);

#endif // RENDERING_GRAPH_NODE_H
