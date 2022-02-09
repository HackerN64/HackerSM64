#ifndef ENVFX_SNOW_H
#define ENVFX_SNOW_H

#include <PR/ultratypes.h>
#include "types.h"

// Vanilla was 15, newer f3d microcodes allow for 30.
#define ENVFX_VERTEX_BUFFER 30

struct EnvFxParticle {
    /*0x00*/ Vec3i pos;
    /*0x0C*/ s32 angleFromCenter; // for whirpools, angle from center
    /*0x10*/ s32 distFromCenter; // for whirpools, distance from center
    /*0x14*/ s32 bubbleY; // for Bubbles, yPos is always set to this
    /*0x18*/ s16 animFrame; // lava bubbles and flowers have frame animations
    /*0x1A*/ s8 isAlive : 1;
};

extern s8 gEnvFxMode;

extern struct EnvFxParticle *gEnvFxBuffer;

Gfx *envfx_update_particles(s32 mode, Vec3s marioPos, Vec3s camTo, Vec3s camFrom);
void rotate_triangle_vertices(Vec3s vertex1, Vec3s vertex2, Vec3s vertex3, s16 pitch, s16 yaw);
void append_particle_vertex_buffer(Gfx *gfx, s32 index, Vec3s vertex1, Vec3s vertex2, Vec3s vertex3, Vtx *template);

#endif // ENVFX_SNOW_H
