#ifndef GEO_COMMANDS_H
#define GEO_COMMANDS_H

#include "command_macros_base.h"

#include "game/shadow.h"
#include "game/object_helpers.h"
#include "game/behavior_actions.h"
#include "game/segment2.h"
#include "game/mario_misc.h"
#include "game/mario_actions_cutscene.h"

#define GEO_PARAMS_LAST_BIT 7
#define GEO_PARAMS_HAS_DL_BIT BIT(GEO_PARAMS_LAST_BIT) // 0x80
#define GEO_PARAMS_SHIFT 4
#define GEO_PARAMS_DRAWING_LAYER_MASK BITMASK(GEO_PARAMS_SHIFT) // 0xF
#define GEO_PARAMS_ARGS_MASK (BITMASK(GEO_PARAMS_LAST_BIT - GEO_PARAMS_SHIFT) << GEO_PARAMS_SHIFT) // 0x70

// GEO_CMD_UPDATE_NODE_FLAGS
enum GeoCommandFlags {
    GEO_CMD_FLAGS_RESET,
    GEO_CMD_FLAGS_SET,
    GEO_CMD_FLAGS_CLEAR
};

// Sky background params
enum SkyBackgroundParams {
    BACKGROUND_OCEAN_SKY,
    BACKGROUND_FLAMING_SKY,
    BACKGROUND_UNDERWATER_CITY,
    BACKGROUND_BELOW_CLOUDS,
    BACKGROUND_SNOW_MOUNTAINS,
    BACKGROUND_DESERT,
    BACKGROUND_HAUNTED,
    BACKGROUND_GREEN_SKY,
    BACKGROUND_ABOVE_CLOUDS,
    BACKGROUND_PURPLE_SKY
};

enum GeoLayoutCommands {
    /*0x00*/ GEO_CMD_BRANCH_AND_LINK,
    /*0x01*/ GEO_CMD_END,
    /*0x02*/ GEO_CMD_BRANCH,
    /*0x03*/ GEO_CMD_RETURN,
    /*0x04*/ GEO_CMD_OPEN_NODE,
    /*0x05*/ GEO_CMD_CLOSE_NODE,
    /*0x06*/ GEO_CMD_ASSIGN_AS_VIEW,
    /*0x07*/ GEO_CMD_UPDATE_NODE_FLAGS,
    /*0x08*/ GEO_CMD_NODE_ROOT,
    /*0x09*/ GEO_CMD_NODE_ORTHO_PROJECTION,
    /*0x0A*/ GEO_CMD_NODE_PERSPECTIVE,
    /*0x0B*/ GEO_CMD_NODE_START,
    /*0x0C*/ GEO_CMD_NODE_MASTER_LIST,
    /*0x0D*/ GEO_CMD_NODE_LEVEL_OF_DETAIL,
    /*0x0E*/ GEO_CMD_NODE_SWITCH_CASE,
    /*0x0F*/ GEO_CMD_NODE_CAMERA,
    /*0x10*/ GEO_CMD_NODE_TRANSLATION_ROTATION,
    /*0x11*/ GEO_CMD_NODE_ANIMATED_PART,
    /*0x12*/ GEO_CMD_NODE_BILLBOARD,
    /*0x13*/ GEO_CMD_NODE_DISPLAY_LIST,
    /*0x14*/ GEO_CMD_NODE_SHADOW,
    /*0x15*/ GEO_CMD_NODE_OBJECT_PARENT,
    /*0x16*/ GEO_CMD_NODE_GENERATED,
    /*0x17*/ GEO_CMD_NODE_BACKGROUND,
    /*0x18*/ GEO_CMD_NOP_1,
    /*0x19*/ GEO_CMD_COPY_VIEW,
    /*0x1A*/ GEO_CMD_NODE_HELD_OBJ,
    /*0x1B*/ GEO_CMD_NODE_SCALE,
    /*0x1C*/ GEO_CMD_NOP_2,
    /*0x1D*/ GEO_CMD_NOP_3,
    /*0x1E*/ GEO_CMD_NODE_CULLING_RADIUS,
    /*0x1F*/ GEO_CMD_BONE,
    /*0x20*/ GEO_CMD_Z_OFFSET,
};

// geo layout macros

/**
 * GEO_CMD_BRANCH_AND_LINK: Branch and store return address
 *   0x04: scriptTarget, segment address of geo layout
 */
#define GEO_BRANCH_AND_LINK(scriptTarget) \
    CMD_BBH(GEO_CMD_BRANCH_AND_LINK, 0x00, 0x0000), \
    CMD_PTR(scriptTarget)

/**
 * GEO_CMD_END: Terminate geo layout
 *   0x01-0x03: unused
 */
#define GEO_END() \
    CMD_BBH(GEO_CMD_END, 0x00, 0x0000)

/**
 * GEO_CMD_BRANCH: Branch
 *   0x01: if 1, store next geo layout address on stack
 *   0x02-0x03: unused
 *   0x04: scriptTarget, segment address of geo layout
 */
#define GEO_BRANCH(type, scriptTarget) \
    CMD_BBH(GEO_CMD_BRANCH, type, 0x0000), \
    CMD_PTR(scriptTarget)

/**
 * GEO_CMD_RETURN: Return from branch
 *   0x01-0x03: unused
 */
#define GEO_RETURN() \
    CMD_BBH(GEO_CMD_RETURN, 0x00, 0x0000)

/**
 * GEO_CMD_OPEN_NODE: Open node
 *   0x01-0x03: unused
 */
#define GEO_OPEN_NODE() \
    CMD_BBH(GEO_CMD_OPEN_NODE, 0x00, 0x0000)

/**
 * GEO_CMD_CLOSE_NODE: Close node
 *   0x01-0x03: unused
 */
#define GEO_CLOSE_NODE() \
    CMD_BBH(GEO_CMD_CLOSE_NODE, 0x00, 0x0000)

/**
 * GEO_CMD_ASSIGN_AS_VIEW: Register the current node at the given index in the gGeoViews array
 *   0x01: unused
 *   0x02: s16 index
 */
#define GEO_ASSIGN_AS_VIEW(index) \
    CMD_BBH(GEO_CMD_ASSIGN_AS_VIEW, 0x00, index)

/**
 * GEO_CMD_UPDATE_NODE_FLAGS: Update current scene graph node flags
 *   0x01: u8 operation (0 = reset, 1 = set, 2 = clear)
 *   0x02: s16 bits
 */
#define GEO_UPDATE_NODE_FLAGS(operation, flagBits) \
    CMD_BBH(GEO_CMD_UPDATE_NODE_FLAGS, operation, flagBits)

/**
 * GEO_CMD_NODE_ROOT: Create screen area scene graph node
 *   0x01: unused
 *   0x02: s16 num entries (+2) to allocate
 *   0x04: s16 x
 *   0x06: s16 y
 *   0x08: s16 width
 *   0x0A: s16 height
 */
#define GEO_NODE_SCREEN_AREA(numEntries, x, y, width, height) \
    CMD_BBH(GEO_CMD_NODE_ROOT, 0x00, numEntries), \
    CMD_HH(x, y), \
    CMD_HH(width, height)

/**
 * GEO_CMD_NODE_ORTHO_PROJECTION: Create orthographic projection scene graph node
 *   0x02: s16 scale as percentage
 */
#define GEO_NODE_ORTHO(scale) \
    CMD_BBH(GEO_CMD_NODE_ORTHO_PROJECTION, 0x00, scale)

/**
 * GEO_CMD_NODE_PERSPECTIVE: Create camera frustum scene graph node
 *   0x01: u8  if nonzero, enable function field
 *   0x02: s16 field of view
 *   0x04: u16 near
 *   0x06: u16 far
 *   0x08: [GraphNodeFunc function]
*/
#define GEO_CAMERA_FRUSTUM(fov, near, far) \
    CMD_BBH(GEO_CMD_NODE_PERSPECTIVE, 0x00, fov), \
    CMD_HH(near, far)
#define GEO_CAMERA_FRUSTUM_WITH_FUNC(fov, near, far, func) \
    CMD_BBH(GEO_CMD_NODE_PERSPECTIVE, 0x01, fov), \
    CMD_HH(near, far), \
    CMD_PTR(func)

/**
 * GEO_CMD_NODE_START: Create a root scene graph node
 *   0x01-0x03: unused
 */
#define GEO_NODE_START() \
    CMD_BBH(GEO_CMD_NODE_START, 0x00, 0x0000)

/**
 * GEO_CMD_NODE_MASTER_LIST: Create zbuffer-toggling scene graph node
 *   0x01: u8 enableZBuffer (1 = on, 0 = off)
 *   0x02-0x03: unused
 */
#define GEO_ZBUFFER(enable) \
    CMD_BBH(GEO_CMD_NODE_MASTER_LIST, enable, 0x0000)

/**
 * GEO_CMD_NODE_LEVEL_OF_DETAIL: Create render range scene graph node
 *   0x01-0x03: unused
 *   0x04: s16 minDistance
 *   0x06: s16 maxDistance
 */
#define GEO_RENDER_RANGE(minDistance, maxDistance) \
    CMD_BBH(GEO_CMD_NODE_LEVEL_OF_DETAIL, 0x00, 0x0000), \
    CMD_HH(minDistance, maxDistance)

/**
 * GEO_CMD_NODE_SWITCH_CASE: Create switch-case scene graph node
 *   0x01: unused
 *   0x02: s16 numCases
 *   0x04: GraphNodeFunc caseSelectorFunc
 */
#define GEO_SWITCH_CASE(count, function) \
    CMD_BBH(GEO_CMD_NODE_SWITCH_CASE, 0x00, count), \
    CMD_PTR(function)

/**
 * GEO_CMD_NODE_CAMERA: Create a camera scene graph node.
 *   0x01: unused
 *   0x02: s16 camera type
 *   0x04: s16 posX
 *   0x06: s16 posY
 *   0x08: s16 posZ
 *   0x0A: s16 focusX
 *   0x0C: s16 focusY
 *   0x0E: s16 focusZ
 *   0x10: GraphNodeFunc function
 */
#define GEO_CAMERA(type, x1, y1, z1, x2, y2, z2, function) \
    CMD_BBH(GEO_CMD_NODE_CAMERA, 0x00, type), \
    CMD_HHHHHH(x1, y1, z1, x2, y2, z2), \
    CMD_PTR(function)

/**
 * GEO_CMD_NODE_TRANSLATION_ROTATION: Create translation & rotation scene graph node with optional display list
 * Four different versions of 0x10
 *   cmd+0x01: u8 params
 *     0b1000_0000: if set, enable displayList field and drawingLayer
 *     0b0111_0000: fieldLayout (determines how rest of data is formatted
 *     0b0000_1111: drawingLayer
 *
 *   Translate & Rotate
 *     0x04: s16 xTranslation
 *     0x06: s16 yTranslation
 *     0x08: s16 zTranslation
 *     0x0A: s16 xRotation
 *     0x0C: s16 yRotation
 *     0x0E: s16 zRotation
 *     0x10: [u32 displayList: if MSbit of params set, display list segmented address]
 */
#define GEO_TRANSLATE_ROTATE(layer, tx, ty, tz, rx, ry, rz) \
    CMD_BBH(GEO_CMD_NODE_TRANSLATION_ROTATION, (layer), 0x0000), \
    CMD_HHHHHH(tx, ty, tz, rx, ry, rz)
#define GEO_TRANSLATE_ROTATE_WITH_DL(layer, tx, ty, tz, rx, ry, rz, displayList) \
    CMD_BBH(GEO_CMD_NODE_TRANSLATION_ROTATION, (layer | GEO_PARAMS_HAS_DL_BIT), 0x0000), \
    CMD_HHHHHH(tx, ty, tz, rx, ry, rz), \
    CMD_PTR(displayList)

/**
 * GEO_CMD_NODE_TRANSLATION_ROTATION: Translate
 *     0x04: s16 xTranslation
 *     0x06: s16 yTranslation
 *     0x08: s16 zTranslation
 *     0x0A: s16 xRotation (0)
 *     0x0C: s16 yRotation (0)
 *     0x0E: s16 zRotation (0)
 *     0x10: [u32 displayList: if MSbit of params set, display list segmented address]
 */
#define GEO_TRANSLATE(layer, tx, ty, tz) \
    CMD_BBH(GEO_CMD_NODE_TRANSLATION_ROTATION, (layer), 0x0000), \
    CMD_HHHHHH(tx, ty, tz, 0, 0, 0)
#define GEO_TRANSLATE_WITH_DL(layer, tx, ty, tz, displayList) \
    CMD_BBH(GEO_CMD_NODE_TRANSLATION_ROTATION, (layer | GEO_PARAMS_HAS_DL_BIT), 0x0000), \
    CMD_HHHHHH(tx, ty, tz, 0, 0, 0), \
    CMD_PTR(displayList)

/**
 * GEO_CMD_NODE_TRANSLATION_ROTATION: Rotate
 *     0x04: s16 xTranslation (0)
 *     0x06: s16 yTranslation (0)
 *     0x08: s16 zTranslation (0)
 *     0x0A: s16 xRotation
 *     0x0C: s16 yRotation
 *     0x0E: s16 zRotation
 *     0x10: [u32 displayList: if MSbit of params set, display list segmented address]
 */
#define GEO_ROTATE(layer, rx, ry, rz) \
    CMD_BBH(GEO_CMD_NODE_TRANSLATION_ROTATION, (layer), 0x0000), \
    CMD_HHHHHH(0, 0, 0, rx, ry, rz)
#define GEO_ROTATE_WITH_DL(layer, rx, ry, rz, displayList) \
    CMD_BBH(GEO_CMD_NODE_TRANSLATION_ROTATION, (layer | GEO_PARAMS_HAS_DL_BIT), 0x0000), \
    CMD_HHHHHH(0, 0, 0, rx, ry, rz), \
    CMD_PTR(displayList)

/**
 * GEO_CMD_NODE_TRANSLATION_ROTATION: Rotate Y degrees (unused)
 *     0x04: s16 xTranslation (0)
 *     0x06: s16 yTranslation (0)
 *     0x08: s16 zTranslation (0)
 *     0x0A: s16 xRotation (0)
 *     0x0C: s16 yRotation (converted from degrees)
 *     0x0E: s16 zRotation (0)
 *     0x10: [u32 displayList: if MSbit of params set, display list segmented address]
 */
#define GEO_ROTATE_Y(layer, ry) \
    CMD_BBH(GEO_CMD_NODE_TRANSLATION_ROTATION, (layer), 0x0000), \
    CMD_HHHHHH(0, 0, 0, 0, degrees_to_angle(ry), 0)
#define GEO_ROTATE_Y_WITH_DL(layer, ry, displayList) \
    CMD_BBH(GEO_CMD_NODE_TRANSLATION_ROTATION, (layer | GEO_PARAMS_HAS_DL_BIT), 0x0000), \
    CMD_HHHHHH(0, 0, 0, 0, degrees_to_angle(ry), 0), \
    CMD_PTR(displayList)

// Backwards compatibility
#define GEO_TRANSLATE_NODE          GEO_TRANSLATE
#define GEO_TRANSLATE_NODE_WITH_DL  GEO_TRANSLATE_WITH_DL
#define GEO_ROTATION_NODE           GEO_ROTATE
#define GEO_ROTATION_NODE_WITH_DL   GEO_ROTATE_WITH_DL

/**
 * GEO_CMD_NODE_ANIMATED_PART: Create a scene graph node that is rotated by the object's animation.
 *   0x01: u8 drawingLayer
 *   0x02: s16 xTranslation
 *   0x04: s16 yTranslation
 *   0x06: s16 zTranslation
 *   0x08: u32 displayList: dislay list segmented address
 */
#define GEO_ANIMATED_PART(layer, x, y, z, displayList) \
    CMD_BBH(GEO_CMD_NODE_ANIMATED_PART, layer, x), \
    CMD_HH(y, z), \
    CMD_PTR(displayList)

/**
 * GEO_CMD_NODE_BILLBOARD: Create billboarding node with optional display list
 *   0x01: u8 params
 *      0b1000_0000: if set, enable displayList field and drawingLayer
 *      0b0000_1111: drawingLayer
 *   0x02: s16 xTranslation
 *   0x04: s16 yTranslation
 *   0x06: s16 zTranslation
 *   0x08: [u32 displayList: if MSbit of params is set, display list segmented address]
 */
#define GEO_BILLBOARD_WITH_PARAMS(layer, tx, ty, tz) \
    CMD_BBH(GEO_CMD_NODE_BILLBOARD, layer, tx), \
    CMD_HH(ty, tz)
#define GEO_BILLBOARD_WITH_PARAMS_AND_DL(layer, tx, ty, tz, displayList) \
    CMD_BBH(GEO_CMD_NODE_BILLBOARD, (layer | GEO_PARAMS_HAS_DL_BIT), tx), \
    CMD_HH(ty, tz), \
    CMD_PTR(displayList)
#define GEO_BILLBOARD() \
    GEO_BILLBOARD_WITH_PARAMS(0, 0, 0, 0)

/**
 * GEO_CMD_NODE_DISPLAY_LIST: Create plain display list scene graph node
 *   0x01: u8 drawingLayer
 *   0x02-0x03: unused
 *   0x04: u32 displayList: display list segmented address
 */
#define GEO_DISPLAY_LIST(layer, displayList) \
    CMD_BBH(GEO_CMD_NODE_DISPLAY_LIST, layer, 0x0000), \
    CMD_PTR(displayList)

/**
 * GEO_CMD_NODE_SHADOW: Create shadow scene graph node
 *   0x01: unused
 *   0x02: s16 shadowType (cast to u8)
 *   0x04: s16 shadowSolidity (cast to u8)
 *   0x06: s16 shadowScale
 */
#define GEO_SHADOW(type, solidity, scale) \
    CMD_BBH(GEO_CMD_NODE_SHADOW, 0x00, type), \
    CMD_HH(solidity, scale)

/**
 * GEO_CMD_NODE_OBJECT_PARENT: Create render object scene graph node
 *   0x01-0x03: unused
 */
#define GEO_RENDER_OBJ() \
    CMD_BBH(GEO_CMD_NODE_OBJECT_PARENT, 0x00, 0x0000)

/**
 * GEO_CMD_NODE_GENERATED: Create dynamically generated displaylist scene graph node
 *   0x01: unused
 *   0x04: s32 parameter
 *   0x08: GraphNodeFunc function
 */
#define GEO_ASM(param, function) \
    CMD_BBH(GEO_CMD_NODE_GENERATED, 0x00, 0x0000), \
    CMD_W(param), \
    CMD_PTR(function)

/**
 * GEO_CMD_NODE_BACKGROUND: Create background scene graph node
 *   0x02: s16 background: background ID, or RGBA5551 color if backgroundFunc is null
 *   0x04: GraphNodeFunc backgroundFunc
 */
#define GEO_BACKGROUND(background, function) \
    CMD_BBH(GEO_CMD_NODE_BACKGROUND, 0x00, background), \
    CMD_PTR(function)
#define GEO_BACKGROUND_COLOR(background) \
    GEO_BACKGROUND(background, NULL)

/**
 * GEO_CMD_NOP_1: No operation
 */
#define GEO_NOP_1() \
    CMD_BBH(GEO_CMD_NOP_1, 0x00, 0x0000), \
    CMD_HH(0x0000, 0x0000)

/**
 * GEO_CMD_COPY_VIEW: Copy the shared children from an object parent node from a specific view
 * to a newly created object parent.
 *   0x02: s16 index of array
 */
#define GEO_COPY_VIEW(index) \
    CMD_BBH(GEO_CMD_COPY_VIEW, 0x00, index)

/**
 * GEO_CMD_NODE_HELD_OBJ: Create a held object scene graph node
 *  cmd+0x01: u8 unused
 *  cmd+0x02: s16 offsetX
 *  cmd+0x04: s16 offsetY
 *  cmd+0x06: s16 offsetZ
 *  cmd+0x08: GraphNodeFunc nodeFunc
 */
#define GEO_HELD_OBJECT(param, ux, uy, uz, nodeFunc) \
    CMD_BBH(GEO_CMD_NODE_HELD_OBJ, param, ux), \
    CMD_HH(uy, uz), \
    CMD_PTR(nodeFunc)

/**
 * GEO_CMD_NODE_SCALE: Create scale scene graph node with optional display list
 *   0x01: u8 params
 *     0b1000_0000: if set, enable displayList field and drawingLayer
 *     0b0000_1111: drawingLayer
 *   0x02-0x03: unused
 *   0x04: u32 scale (0x10000 = 1.0)
 *   0x08: [u32 displayList: if MSbit of params is set, display list segment address]
 */
#define GEO_SCALE(layer, scale) \
    CMD_BBH(GEO_CMD_NODE_SCALE, layer, 0x0000), \
    CMD_W(scale)
#define GEO_SCALE_WITH_DL(layer, scale, displayList) \
    CMD_BBH(GEO_CMD_NODE_SCALE, (layer | GEO_PARAMS_HAS_DL_BIT), 0x0000), \
    CMD_W(scale), \
    CMD_PTR(displayList)

/**
 * GEO_CMD_NOP_2: No operation
 */
#define GEO_NOP_2() \
    CMD_BBH(GEO_CMD_NOP_2, 0x00, 0x0000), \
    CMD_HH(0x0000, 0x0000)

/**
 * GEO_CMD_NOP_3: No operation
 */
#define GEO_NOP_3() \
    CMD_BBH(GEO_CMD_NOP_3, 0x00, 0x0000), \
    CMD_HH(0x0000, 0x0000), \
    CMD_HH(0x0000, 0x0000), \
    CMD_HH(0x0000, 0x0000)

/**
 * GEO_CMD_NODE_CULLING_RADIUS: Create a scene graph node that specifies for an object the radius that
 * is used for frustum culling.
 *   0x01: unused
 *   0x02: s16 cullingRadius
 */
#define GEO_CULLING_RADIUS(cullingRadius) \
    CMD_BBH(GEO_CMD_NODE_CULLING_RADIUS, 0x00, cullingRadius)

/**
 * GEO_CMD_BONE: Create a scene graph node that is rotated by the object's animation + an initial rotation.
 *   0x01: u8 drawingLayer
 *   0x02: unused
 *   0x04: s16 xTranslation
 *   0x08: s16 yTranslation
 *   0x0A: s16 zTranslation
 *   0x0C: s16 xRotation
 *   0x0E: s16 yRotation
 *   0x10: s16 zRotation
 *   0x12: u32 displayList: dislay list segmented address
 */
#define GEO_BONE(layer, tx, ty, tz, rx, ry, rz, displayList) \
    CMD_BBH(GEO_CMD_BONE, layer, 0x0000), \
    CMD_HHHHHH(tx, ty, tz, rx, ry, rz), \
    CMD_PTR(displayList)

/**
 * GEO_CMD_Z_OFFSET: Create a z offset scene graph node.
 *   0x01: unused
 *   0x02: s16 zOffset
 */
#define GEO_Z_OFFSET(zOffset) \
    CMD_BBH(GEO_CMD_Z_OFFSET, 0x00, zOffset)

#endif // GEO_COMMANDS_H
