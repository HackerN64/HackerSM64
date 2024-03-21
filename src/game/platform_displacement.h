#ifndef PLATFORM_DISPLACEMENT_H
#define PLATFORM_DISPLACEMENT_H

#include <PR/ultratypes.h>

#include "types.h"

#include "config.h"

struct PlatformDisplacementInfo {
    Vec3f prevPos;
    Vec3f prevTransformedPos;
    Vec3f prevTransformedYawVec;
    s16 prevYaw;
    struct Object *prevPlatform;
    u32 prevTimer;
};

void update_mario_platform(void);
void update_platform_displacement_info(struct PlatformDisplacementInfo *displaceInfo, Vec3f pos, s16 yaw, struct Object *platform);
void apply_platform_displacement(struct PlatformDisplacementInfo *displaceInfo, Vec3f pos, s16 *yaw, struct Object *platform);
void apply_mario_platform_displacement(void);
void clear_mario_platform(void);

#endif // PLATFORM_DISPLACEMENT_H
