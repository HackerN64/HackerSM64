#ifndef GEO_LAYOUT_H
#define GEO_LAYOUT_H

#include <PR/ultratypes.h>

#include "game/memory.h"
#include "macros.h"
#include "types.h"

enum GeoCommandFlags {
    GEO_CMD_FLAGS_RESET,
    GEO_CMD_FLAGS_SET,
    GEO_CMD_FLAGS_CLEAR
};

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

void geo_layout_cmd_branch_and_link(void);
void geo_layout_cmd_end(void);
void geo_layout_cmd_branch(void);
void geo_layout_cmd_return(void);
void geo_layout_cmd_open_node(void);
void geo_layout_cmd_close_node(void);
void geo_layout_cmd_assign_as_view(void);
void geo_layout_cmd_update_node_flags(void);
void geo_layout_cmd_node_root(void);
void geo_layout_cmd_node_ortho_projection(void);
void geo_layout_cmd_node_perspective(void);
void geo_layout_cmd_node_start(void);
void geo_layout_cmd_nop3(void);
void geo_layout_cmd_node_master_list(void);
void geo_layout_cmd_node_level_of_detail(void);
void geo_layout_cmd_node_switch_case(void);
void geo_layout_cmd_node_camera(void);
void geo_layout_cmd_node_translation_rotation(void);
void geo_layout_cmd_node_translation(void);
void geo_layout_cmd_node_rotation(void);
void geo_layout_cmd_node_scale(void);
void geo_layout_cmd_nop2(void);
void geo_layout_cmd_node_animated_part(void);
void geo_layout_cmd_node_billboard(void);
void geo_layout_cmd_node_display_list(void);
void geo_layout_cmd_node_shadow(void);
void geo_layout_cmd_node_object_parent(void);
void geo_layout_cmd_node_generated(void);
void geo_layout_cmd_node_background(void);
void geo_layout_cmd_nop(void);
void geo_layout_cmd_copy_view(void);
void geo_layout_cmd_node_held_obj(void);
void geo_layout_cmd_node_culling_radius(void);
void geo_layout_cmd_scene_light(void); // Advanced lighting engine

struct GraphNode *process_geo_layout(struct AllocOnlyPool *pool, void *segptr);

#endif // GEO_LAYOUT_H
