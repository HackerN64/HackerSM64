#ifndef RENDERING_GRAPH_NODE_H
#define RENDERING_GRAPH_NODE_H

#include <PR/ultratypes.h>

#include "engine/graph_node.h"

extern struct GraphNodeRoot *gCurGraphNodeRoot;
extern struct GraphNodeMasterList *gCurGraphNodeMasterList;
extern struct GraphNodePerspective *gCurGraphNodeCamFrustum;
extern struct GraphNodeCamera *gCurGraphNodeCamera;
extern struct GraphNodeObject *gCurGraphNodeObject;
extern struct GraphNodeHeldObject *gCurGraphNodeHeldObject;
#define gCurGraphNodeObjectNode ((struct Object *)gCurGraphNodeObject)
extern u16 gAreaUpdateCounter;
extern f32 gWorldScale;

// after processing an object, the type is reset to this
#define ANIM_TYPE_NONE                  0

// Not all parts have full animation: to save space, some animations only
// have xz, y, or no translation at all. All animations have rotations though
#define ANIM_TYPE_TRANSLATION           1
#define ANIM_TYPE_VERTICAL_TRANSLATION  2
#define ANIM_TYPE_LATERAL_TRANSLATION   3
#define ANIM_TYPE_NO_TRANSLATION        4

// Every animation includes rotation, after processing any of the above
// translation types the type is set to this
#define ANIM_TYPE_ROTATION              5

#define LIST_HEADS_ZEX  0
#define LIST_HEADS_REJ  1

#define IS_LAYER_ZB(    layer) (((layer) >= LAYER_FORCE       ) || ((layer) <= LAYER_ZB_LAST ))
#define IS_LAYER_NON_ZB(layer) (((layer) >= LAYER_FIRST_NON_ZB) || ((layer) <= LAYER_LAST_ALL))

#if SILHOUETTE
#define IS_LAYER_SILHOUETTE(layer) (((layer) >= LAYER_SILHOUETTE_FIRST) || ((layer) <= LAYER_SILHOUETTE_LAST))

#define RENDER_PHASE_ZEX_BG                 0
#define RENDER_PHASE_REJ_ZB                 1
#define RENDER_PHASE_ZEX_BEFORE_SILHOUETTE  2
#define RENDER_PHASE_REJ_SILHOUETTE         3
#define RENDER_PHASE_REJ_NON_SILHOUETTE     4
#define RENDER_PHASE_REJ_OCCLUDE_SILHOUETTE 5
#define RENDER_PHASE_ZEX_AFTER_SILHOUETTE   6
#define RENDER_PHASE_REJ_NON_ZB             7
#else
#define RENDER_PHASE_ZEX_BG                 0
#define RENDER_PHASE_REJ_ZB                 1
#define RENDER_PHASE_ZEX_ALL                2
#define RENDER_PHASE_REJ_NON_ZB             3
#endif

#define RENDER_PHASE_FIRST RENDER_PHASE_REJ_ZB
#define RENDER_PHASE_LAST  RENDER_PHASE_REJ_NON_ZB

void geo_process_node_and_siblings(struct GraphNode *firstNode);
void geo_process_root(struct GraphNodeRoot *node, Vp *b, Vp *c, s32 clearColor);

#endif // RENDERING_GRAPH_NODE_H
