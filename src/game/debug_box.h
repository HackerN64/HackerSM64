#ifndef DEBUG_BOX_H
#define DEBUG_BOX_H

#ifdef VISUAL_DEBUG

/**
 * @file debug_box.h
 * Draws debug boxes, see debug_box.c for details
 */

#include "types.h"

/**
 * The max amount of debug boxes before debug_box() just returns.
 * You can set this to something higher, but you might run out of space in the gfx pool.
 */
#define MAX_DEBUG_BOXES 512

enum DebugBoxFlags {
    DEBUG_SHAPE_BOX      = BIT(0), // 0x01
    DEBUG_SHAPE_CYLINDER = BIT(1), // 0x02
    DEBUG_UCODE_DEFAULT  = BIT(2), // 0x04
#ifdef OBJECTS_REJ
    DEBUG_UCODE_REJ      = BIT(3), // 0x08
#else
    DEBUG_UCODE_REJ      = DEBUG_UCODE_DEFAULT,
#endif
    DEBUG_BOX_CLEAR      = BIT(4), // 0x10
};

enum VisualDebugViewCycles {
    VISUAL_DEBUG_VIEW_NONE,
    VISUAL_DEBUG_VIEW_HITBOXES,
    VISUAL_DEBUG_VIEW_SURFACES,
    VISUAL_DEBUG_VIEW_HITBOXES_AND_SURFACES,
    VISUAL_DEBUG_NUM_VIEWS,
};

extern u8 hitboxView;
extern u8 surfaceView;
extern void debug_box_input(void);

void debug_box_color(u32 color);
void debug_box(Vec3f center, Vec3f bounds, s32 type);
void debug_box_rot(Vec3f center, Vec3f bounds, s16 yaw, s32 type);

void debug_box_pos(Vec3f pMin, Vec3f pMax, s32 type);
void debug_box_pos_rot(Vec3f pMin, Vec3f pMax, s16 yaw, s32 type);

void render_debug_boxes(s32 type);
extern void visual_surface_loop(s32 isDecal);

#endif

#endif /* DEBUG_BOX_H */
