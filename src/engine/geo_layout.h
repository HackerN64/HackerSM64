#ifndef GEO_LAYOUT_H
#define GEO_LAYOUT_H

#include <PR/ultratypes.h>

#include "game/memory.h"
#include "macros.h"
#include "types.h"

#define CMD_SIZE_SHIFT (sizeof(void *) >> 3)
#define CMD_PROCESS_OFFSET(offset) (((offset) & 3) | (((offset) & ~3) << CMD_SIZE_SHIFT))

#define cur_geo_cmd_u8(offset) \
    (gGeoLayoutCommand[CMD_PROCESS_OFFSET(offset)])

#define cur_geo_cmd_s16(offset) \
    (*(s16 *) &gGeoLayoutCommand[CMD_PROCESS_OFFSET(offset)])

#define cur_geo_cmd_u16(offset) \
    (*(u16 *) &gGeoLayoutCommand[CMD_PROCESS_OFFSET(offset)])

#define cur_geo_cmd_s32(offset) \
    (*(s32 *) &gGeoLayoutCommand[CMD_PROCESS_OFFSET(offset)])

#define cur_geo_cmd_u32(offset) \
    (*(u32 *) &gGeoLayoutCommand[CMD_PROCESS_OFFSET(offset)])

#define cur_geo_cmd_f32(offset) \
    (*(f32 *) &gGeoLayoutCommand[CMD_PROCESS_OFFSET(offset)])

#define cur_geo_cmd_ptr(offset) \
    (*(void **) &gGeoLayoutCommand[CMD_PROCESS_OFFSET(offset)])

extern struct AllocOnlyPool *gGraphNodePool;
extern struct GraphNode *gCurRootGraphNode;
extern struct GraphNode **gGeoViews;
extern u16 gGeoNumViews;
extern uintptr_t gGeoLayoutStack[];
extern struct GraphNode *gCurGraphNodeList[];
extern s16 gCurGraphNodeIndex;
extern s16 gGeoLayoutStackIndex;
extern s16 gGeoLayoutReturnIndex;
extern u8 *gGeoLayoutCommand;
extern struct GraphNode gObjParentGraphNode;

struct GraphNode *process_geo_layout(struct AllocOnlyPool *pool, void *segptr);

#endif // GEO_LAYOUT_H
