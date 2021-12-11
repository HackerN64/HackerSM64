#ifndef ENVFX_SNOW_H
#define ENVFX_SNOW_H

#include <PR/ultratypes.h>
#include "types.h"

struct EnvFxParticle {
    s8 isAlive;
    s16 animFrame; // lava bubbles and flowers have frame animations
    s32 xPos;
    s32 yPos;
    s32 zPos;
    s32 angleAndDist[2]; // for whirpools, [0] = angle from center, [1] = distance from center
    s32 unusedBubbleVar; // set to zero for bubbles when respawning, never used elsewhere
    s32 bubbleY; // for Bubbles, yPos is always set to this
    u8 filler[24];
};

extern s8 gEnvFxMode;

extern struct EnvFxParticle *gEnvFxBuffer;
extern Vec3i gSnowCylinderLastPos;
extern s16 gSnowParticleCount;

Gfx *envfx_update_particles(s32 mode, Vec3s marioPos, Vec3s camTo, Vec3s camFrom);
void orbit_from_positions(Vec3s from, Vec3s to, s16 *radius, s16 *pitch, s16 *yaw);
void rotate_triangle_vertices(Vec3s vertex1, Vec3s vertex2, Vec3s vertex3, s16 pitch, s16 yaw);

#endif // ENVFX_SNOW_H
