#ifndef PUPPYLIGHTS_H
#define PUPPYLIGHTS_H

#ifdef PUPPYLIGHTS

#include "types.h"
#include "command_macros_base.h"

// The maximum number of lights that can be loaded at once. Any further lights that attempt to be created past this will simply not spawn.
#define MAX_LIGHTS 32
// The maximum number of dynamic lights available at one time.
#define MAX_LIGHTS_DYNAMIC 8

// Two shapes. Choose your destiny.
enum PuppyLightFlags {
    PUPPYLIGHT_FLAGS_NONE     = 0x0,
    PUPPYLIGHT_SHAPE_CUBE     = BIT(0), // 0x01
    PUPPYLIGHT_SHAPE_CYLINDER = BIT(1), // 0x02
    PUPPYLIGHT_DYNAMIC        = BIT(2), // 0x04
    PUPPYLIGHT_DIRECTIONAL    = BIT(3), // 0x08
    PUPPYLIGHT_SHADOW         = BIT(4), // 0x10
    PUPPYLIGHT_WET            = BIT(5), // 0x20
    PUPPYLIGHT_DELETE         = BIT(6), // 0x40
    PUPPYLIGHT_IGNORE_Y       = BIT(7), // 0x80
};

enum PuppyLightIterateFlags {
    LIGHTFLAG_DIRECTIONAL_OFFSET = BIT(0) // 0x1
};

#define PUPPYLIGHT_ENVIRONMENT(ambientR, ambientG, ambientB, diffuseR, diffuseG, diffuseB, diffuseX, diffuseY, diffuseZ) \
    CMD_BBBB(LEVEL_CMD_PUPPYLIGHT_ENVIRONMENT, 0x0C, ambientR, ambientG), \
    CMD_BBBB(ambientB, diffuseR, diffuseG, diffuseB), \
    CMD_BBBB(diffuseX, diffuseY, diffuseZ, 0x0)

#define PUPPYLIGHT_NODE(r, g, b, a, x, y, z, offsetX, offsetY, offsetZ, yaw, epicentre, flags, room) \
    CMD_BBBB(LEVEL_CMD_PUPPYLIGHT_NODE, 0x18, r, g), \
    CMD_BBH(b, a, x), \
    CMD_HH(y, z), \
    CMD_HH(offsetX, offsetY), \
    CMD_HH(offsetZ, yaw), \
    CMD_BBH(epicentre, flags, room)

// How much RAM is allocated to puppylights
#define PUPPYLIGHTS_POOL (sizeof(struct PuppyLight) * MAX_LIGHTS)

extern Lights1 gLevelLight;
extern u16 gNumLights;
extern u8 levelAmbient;
extern struct PuppyLight *gPuppyLights[MAX_LIGHTS];
extern struct MemoryPool *gLightsPool;
extern void puppylights_run(Lights1 *src, struct Object *obj, s32 flags, u32 baseColour);
extern void puppylights_object_emit(struct Object *obj);
extern void cur_obj_enable_light(void);
extern void cur_obj_disable_light(void);
extern void obj_enable_light(struct Object *obj);
extern void obj_disable_light(struct Object *obj);
extern void set_light_properties(struct PuppyLight *light, s32 x, s32 y, s32 z, s32 offsetX, s32 offsetY, s32 offsetZ, s32 yaw, s32 epicentre, s32 colour, s32 flags, s32 room, s32 active);
extern void puppylights_allocate(void);
extern void delete_lights(void);

#endif // PUPPYLIGHTS

#endif // !PUPPYLIGHTS_H
