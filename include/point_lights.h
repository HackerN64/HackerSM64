#ifndef __POINT_LIGHTS_H__
#define __POINT_LIGHTS_H__

#include <PR/ultratypes.h>
#include <PR/gbi.h>
#include <macros.h>
#include <config.h>

#define LIGHT_TYPE_AMBIENT       0
#define LIGHT_TYPE_DIRECTIONAL   1
#define LIGHT_TYPE_POINT         2
#define LIGHT_TYPE_POINT_OCCLUDE 3

#define LIGHT_FLAG_OCCLUDE (1 << 0)


STATIC_ASSERT(MAX_POINT_LIGHTS_ACTIVE <= 6, "You cannot apply more than 6 point lights at a time!");

struct SceneLight {
    Light l;
    Vec3s worldPos;
    s16 flags;
};

extern int gPointLightCompatibilityMode;
extern Lights1 gDirectionalLight;
extern s8 gLightDir[3];
extern u8 gLightDirTransformEnabled;
extern struct SceneLight gPointLights[];
extern u8 gAreaPointLightCount;
extern u8 gPointLightCount;

// Sets the scene's directional light, overrides whatever may be set in the area's geolayout
void set_directional_light(Vec3f direction, s32 red, s32 green, s32 blue);

// Sets the scene's ambient light, overrides whatever may be set in the area's geolayout
void set_ambient_light(s32 red, s32 green, s32 blue);

// Emits a point light with the given parameters
void emit_light(Vec3f pos, s32 red, s32 green, s32 blue, u32 quadraticFalloff, u32 linearFalloff, u32 constantFalloff);

#endif
