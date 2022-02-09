#include <ultra64.h>

#include "sm64.h"
#include "dialog_ids.h"
#include "game_init.h"
#include "memory.h"
#include "ingame_menu.h"
#include "envfx_snow.h"
#include "envfx_bubbles.h"
#include "engine/surface_collision.h"
#include "engine/math_util.h"
#include "engine/behavior_script.h"
#include "audio/external.h"
#include "obj_behaviors.h"
#include "textures.h"
#include "level_geo.h"

/**
 * This file contains the function that handles 'environment effects',
 * which are particle effects related to the level type that, unlike
 * object-based particle effects, are rendered more efficiently by manually
 * generating display lists instead of drawing each particle separately.
 * This file implements snow effects, while in 'envfx_bubbles.c' the
 * implementation for flowers (unused), lava bubbles and jet stream bubbles
 * can be found.
 * The main entry point for envfx is at the bottom of this file, which is
 * called from geo_envfx_main in level_geo.c
 */

struct EnvFxParticle *gEnvFxBuffer;
static Vec3i sSnowCylinderLastPos;
static s16 sSnowParticleCount;
static s16 sSnowParticleMaxCount;

/* DATA */
s8 gEnvFxMode = ENVFX_MODE_NONE;

/// Template for a snow particle triangle
static const Vtx gSnowTempVtx[3] = {
    {{{    -5,      5,      0}, 0, {     0,      0}, {0x7F, 0x7F, 0x7F, 0xFF}}},
    {{{    -5,     -5,      0}, 0, {     0,    960}, {0x7F, 0x7F, 0x7F, 0xFF}}},
    {{{     5,      5,      0}, 0, {   960,      0}, {0x7F, 0x7F, 0x7F, 0xFF}}},
};

/**
 * Initialize snow particles by allocating a buffer for storing their state
 * and setting a start amount.
 */
s32 envfx_init_snow(s32 mode) {
    switch (mode) {
        case ENVFX_MODE_NONE:
            return FALSE;

        case ENVFX_SNOW_NORMAL:
            sSnowParticleMaxCount = 140;
            sSnowParticleCount    = 5;
            break;

        case ENVFX_SNOW_WATER:
            sSnowParticleMaxCount = 30;
            sSnowParticleCount    = 30;
            break;

        case ENVFX_SNOW_BLIZZARD:
            sSnowParticleMaxCount = 140;
            sSnowParticleCount    = 140;
            break;
    }

    gEnvFxBuffer = mem_pool_alloc(gEffectsMemoryPool,
                                  sSnowParticleMaxCount * sizeof(struct EnvFxParticle));
    if (gEnvFxBuffer == NULL) {
        return FALSE;
    }

    bzero(gEnvFxBuffer, sSnowParticleMaxCount * sizeof(struct EnvFxParticle));

    gEnvFxMode = mode;
    return TRUE;
}

/**
 * Update the amount of snow particles on screen.
 * Normal snow starts with few flakes and slowly increases to the maximum.
 * For water snow, this is dependent on how deep underwater you are.
 * Blizzard snows starts at the maximum amount and doesn't change.
 */
void envfx_update_snowflake_count(s32 mode, Vec3s marioPos) {
    s32 globalTimer = gGlobalTimer;
    f32 waterLevel;

    switch (mode) {
        case ENVFX_SNOW_NORMAL:
            if (sSnowParticleMaxCount > sSnowParticleCount) {
                if (!(globalTimer & 63)) {
                    sSnowParticleCount += 5;
                }
            }
            break;

        case ENVFX_SNOW_WATER:
            waterLevel = find_water_level(marioPos[0], marioPos[1], marioPos[2]);

            sSnowParticleCount =
                (((s32)((waterLevel - construct_float(400.0f) - (f32) marioPos[1]) * construct_float(0.001f)) << 0x10) >> 0x10) * 5;

            if (sSnowParticleCount < 0) {
                sSnowParticleCount = 0;
            }

            if (sSnowParticleCount > sSnowParticleMaxCount) {
                sSnowParticleCount = sSnowParticleMaxCount;
            }

            break;

        case ENVFX_SNOW_BLIZZARD:
            break;
    }
}

/**
 * Deallocate the buffer storing snow particles and set the environment effect
 * to none.
 */
void envfx_cleanup_snow(void *snowParticleArray) {
    if (gEnvFxMode != ENVFX_MODE_NONE) {
        if (snowParticleArray) {
            mem_pool_free(gEffectsMemoryPool, snowParticleArray);
        }
        gEnvFxMode = ENVFX_MODE_NONE;
    }
}

/**
 * Check whether the snowflake with the given index is inside view, where
 * 'view' is a cylinder of radius 300 and height 400 centered at the input
 * x, y and z.
 */
s32 envfx_is_snowflake_alive(s32 index, s32 snowCylinderX, s32 snowCylinderY, s32 snowCylinderZ) {
    s32 x = (gEnvFxBuffer + index)->pos[0];
    s32 y = (gEnvFxBuffer + index)->pos[1];
    s32 z = (gEnvFxBuffer + index)->pos[2];

    if (sqr(x - snowCylinderX) + sqr(z - snowCylinderZ) > sqr(300)) {
        return FALSE;
    }

    if ((y < snowCylinderY - 201) || (snowCylinderY + 201 < y)) {
        return FALSE;
    }

    return TRUE;
}

/**
 * Update the position of each snowflake. Snowflakes wiggle by having a
 * random value added to their position each frame. If snowflakes get out
 * of view (where view = a small cylinder in front of the camera) their
 * position is reset to somewhere in view.
 * Since the cylinder of snow is so close to the camera, snow flakes would
 * move out of view very quickly when the camera moves. To mitigate this,
 * a portion of the difference between the previous and current snowCylinder
 * position is added to snowflakes to keep them in view for longer. That's
 * why the snow looks a bit off in 3d, it's a lot closer than you'd think
 * but appears to be further by means of hacky position updates. This might
 * have been done because larger, further away snowflakes are occluded easily
 * by level geometry, wasting many particles.
 */
void envfx_update_snow_normal(s32 snowCylinderX, s32 snowCylinderY, s32 snowCylinderZ) {
    struct EnvFxParticle *particle = gEnvFxBuffer;
    s32 i;
    s32 deltaX = (snowCylinderX - sSnowCylinderLastPos[0]);
    s32 deltaY = (snowCylinderY - sSnowCylinderLastPos[1]);
    s32 deltaZ = (snowCylinderZ - sSnowCylinderLastPos[2]);

    for (i = 0; i < sSnowParticleCount; i++) {
        particle->isAlive =
            envfx_is_snowflake_alive(i, snowCylinderX, snowCylinderY, snowCylinderZ);
        if (!particle->isAlive) {
            particle->pos[0] = (construct_float(400.0f) * random_float()) - construct_float(200.0f) + snowCylinderX + (s16)(deltaX * 2);
            particle->pos[2] = (construct_float(400.0f) * random_float()) - construct_float(200.0f) + snowCylinderZ + (s16)(deltaZ * 2);
            particle->pos[1] = (construct_float(200.0f) * random_float()) + snowCylinderY;
            particle->isAlive = TRUE;
        } else {
            particle->pos[0] += (random_float() * 2) - construct_float(1.0f) + (s16)(deltaX * construct_float(1.0f / 1.2f));
            particle->pos[2] += (random_float() * 2) - construct_float(1.0f) + (s16)(deltaZ * construct_float(1.0f / 1.2f));
            particle->pos[1] -= 2 - (s16)(deltaY * construct_float(0.8f));
        }

        particle++;
    }

    sSnowCylinderLastPos[0] = snowCylinderX;
    sSnowCylinderLastPos[1] = snowCylinderY;
    sSnowCylinderLastPos[2] = snowCylinderZ;
}

/**
 * Unused function. Basically a copy-paste of envfx_update_snow_normal,
 * but an extra 20 units is added to each snowflake x and snowflakes can
 * respawn in y-range [-200, 200] instead of [0, 200] relative to snowCylinderY
 * They also fall a bit faster (with vertical speed -5 instead of -2).
 */
void envfx_update_snow_blizzard(s32 snowCylinderX, s32 snowCylinderY, s32 snowCylinderZ) {
    struct EnvFxParticle *particle = gEnvFxBuffer;
    s32 i;
    s32 deltaX = (snowCylinderX - sSnowCylinderLastPos[0]);
    s32 deltaY = (snowCylinderY - sSnowCylinderLastPos[1]);
    s32 deltaZ = (snowCylinderZ - sSnowCylinderLastPos[2]);

    for (i = 0; i < sSnowParticleCount; i++) {
        particle->isAlive =
            envfx_is_snowflake_alive(i, snowCylinderX, snowCylinderY, snowCylinderZ);
        if (!particle->isAlive) {
            particle->pos[0] = (construct_float(400.0f) * random_float()) - construct_float(200.0f) + snowCylinderX + (s16)(deltaX * 2);
            particle->pos[2] = (construct_float(400.0f) * random_float()) - construct_float(200.0f) + snowCylinderZ + (s16)(deltaZ * 2);
            particle->pos[1] = (construct_float(400.0f) * random_float()) - construct_float(200.0f) + snowCylinderY;
            particle->isAlive = TRUE;
        } else {
            particle->pos[0] += (random_float() * 2) - construct_float(1.0f) + (s16)(deltaX * construct_float(1.0f / 1.2f)) + construct_float(20.0f);
            particle->pos[2] += (random_float() * 2) - construct_float(1.0f) + (s16)(deltaZ * construct_float(1.0f / 1.2f));
            particle->pos[1] -= 5 - (s16)(deltaY * construct_float(0.8f));
        }

        particle++;
    }

    sSnowCylinderLastPos[0] = snowCylinderX;
    sSnowCylinderLastPos[1] = snowCylinderY;
    sSnowCylinderLastPos[2] = snowCylinderZ;
}

/**
 * Update the position of underwater snow particles. Since they are stationary,
 * they merely jump back into view when they are out of view.
 */
void envfx_update_snow_water(s32 snowCylinderX, s32 snowCylinderY, s32 snowCylinderZ) {
    struct EnvFxParticle *particle = gEnvFxBuffer;
    s32 i;

    for (i = 0; i < sSnowParticleCount; i++) {
        particle->isAlive =
            envfx_is_snowflake_alive(i, snowCylinderX, snowCylinderY, snowCylinderZ);
        if (!particle->isAlive) {
            particle->pos[0] = (construct_float(400.0f) * random_float()) - construct_float(200.0f) + snowCylinderX;
            particle->pos[1] = (construct_float(400.0f) * random_float()) - construct_float(200.0f) + snowCylinderY;
            particle->pos[2] = (construct_float(400.0f) * random_float()) - construct_float(200.0f) + snowCylinderZ;
            particle->isAlive = TRUE;
        }

        particle++;
    }
}

/**
 * Rotates the input vertices according to the give pitch and yaw. This
 * is needed for billboarding of particles.
 */
void rotate_triangle_vertices(Vec3s vertex1, Vec3s vertex2, Vec3s vertex3, s16 pitch, s16 yaw) {
    f32 cosPitch = coss(pitch);
    f32 sinPitch = sins(pitch);
    f32 cosMYaw  = coss(-yaw);
    f32 sinMYaw  = sins(-yaw);

    Vec3f v1, v2, v3;

    v1[0] = vertex1[0];
    v1[1] = vertex1[1];
    v1[2] = vertex1[2];

    v2[0] = vertex2[0];
    v2[1] = vertex2[1];
    v2[2] = vertex2[2];

    v3[0] = vertex3[0];
    v3[1] = vertex3[1];
    v3[2] = vertex3[2];

    f32 spsy =  (sinPitch * sinMYaw);
    f32 spcy = -(sinPitch * cosMYaw);
    f32 cpsy = -(cosPitch * sinMYaw);
    f32 cpcy =  (cosPitch * cosMYaw);

    vertex1[0] = (v1[0] * cosMYaw ) + (v1[1] * spsy) + (v1[2] * cpsy);
    vertex1[1] = (v1[1] * cosPitch) + (v1[2] * sinPitch);
    vertex1[2] = (v1[0] * sinMYaw ) + (v1[1] * spcy) + (v1[2] * cpcy);

    vertex2[0] = (v2[0] * cosMYaw ) + (v2[1] * spsy) + (v2[2] * cpsy);
    vertex2[1] = (v2[1] * cosPitch) + (v2[2] * sinPitch);
    vertex2[2] = (v2[0] * sinMYaw ) + (v2[1] * spcy) + (v2[2] * cpcy);

    vertex3[0] = (v3[0] * cosMYaw ) + (v3[1] * spsy) + (v3[2] * cpsy);
    vertex3[1] = (v3[1] * cosPitch) + (v3[2] * sinPitch);
    vertex3[2] = (v3[0] * sinMYaw ) + (v3[1] * spcy) + (v3[2] * cpcy);
}

/**
 * Append ENVFX_VERTEX_BUFFER vertices to 'gfx', which is enough for 5 particles starting at
 * 'index' in the buffer. The 3 input vertices represent the rotated triangle around (0,0,0)
 * that will be translated to particle positions to draw the particle image.
 */
void append_particle_vertex_buffer(Gfx *gfx, s32 index, Vec3s vertex1, Vec3s vertex2, Vec3s vertex3, Vtx *template) {
    struct EnvFxParticle *particle = gEnvFxBuffer;
    s32 i;
    Vtx *vertBuf = alloc_display_list(ENVFX_VERTEX_BUFFER * sizeof(Vtx));

    if (vertBuf == NULL) {
        return;
    }

    for (i = 0; i < ENVFX_VERTEX_BUFFER; i += 3) {
        particle = gEnvFxBuffer + index + (i / 3);

        vertBuf[i + 0] = template[0];
        vec3_sum((vertBuf + i + 0)->v.ob, particle->pos, vertex1);

        vertBuf[i + 1] = template[1];
        vec3_sum((vertBuf + i + 1)->v.ob, particle->pos, vertex2);

        vertBuf[i + 2] = template[2];
        vec3_sum((vertBuf + i + 2)->v.ob, particle->pos, vertex3);
    }

    gSPVertex(gfx, VIRTUAL_TO_PHYSICAL(vertBuf), ENVFX_VERTEX_BUFFER, 0);
}

/**
 * Updates positions of snow particles and returns a pointer to a display list
 * drawing all snowflakes.
 */
Gfx *envfx_update_snow(s32 snowMode, Vec3s marioPos, Vec3s camFrom, Vec3s camTo) {
    s32 i;
    s16 radius, pitch, yaw;
    Vec3s snowCylinderPos;

    // Change these to make snowflakes smaller or bigger
    Vec3s vertex1 = { -5,  5,  0 };
    Vec3s vertex2 = { -5, -5,  0 };
    Vec3s vertex3 = {  5,  5,  0 };

    Gfx *gfxStart = (Gfx *) alloc_display_list((sSnowParticleCount * 6 + 3) * sizeof(Gfx));
    Gfx *gfx = gfxStart;

    if (gfxStart == NULL) {
        return NULL;
    }

    envfx_update_snowflake_count(snowMode, marioPos);

    // Note: to and from are inverted here, so the resulting vector goes towards the camera
    vec3s_get_dist_and_angle(camTo, camFrom, &radius, &pitch, &yaw);

    switch (snowMode) {
        case ENVFX_SNOW_NORMAL:
            // ensure the snow cylinder is no further than 250 units in front
            // of the camera, and no closer than 1 unit.
            if (radius > 250) {
                radius -= 250;
            } else {
                radius = 1;
            }

            vec3s_set_dist_and_angle(camTo, snowCylinderPos, radius, pitch, yaw);
            envfx_update_snow_normal(snowCylinderPos[0], snowCylinderPos[1], snowCylinderPos[2]);
            break;

        case ENVFX_SNOW_WATER:
            if (radius > 500) {
                radius -= 500;
            } else {
                radius = 1;
            }

            vec3s_set_dist_and_angle(camTo, snowCylinderPos, radius, pitch, yaw);
            envfx_update_snow_water(snowCylinderPos[0], snowCylinderPos[1], snowCylinderPos[2]);
            break;
        case ENVFX_SNOW_BLIZZARD:
            if (radius > 250) {
                radius -= 250;
            } else {
                radius = 1;
            }

            vec3s_set_dist_and_angle(camTo, snowCylinderPos, radius, pitch, yaw);
            envfx_update_snow_blizzard(snowCylinderPos[0], snowCylinderPos[1], snowCylinderPos[2]);
            break;
    }

    rotate_triangle_vertices((s16 *) &vertex1, (s16 *) &vertex2, (s16 *) &vertex3, pitch, yaw);

    if (snowMode == ENVFX_SNOW_NORMAL || snowMode == ENVFX_SNOW_BLIZZARD) {
        gSPDisplayList(gfx++, &envfx_dl_gray_snowflake); // snowflake with gray edge
    } else if (snowMode == ENVFX_SNOW_WATER) {
        gSPDisplayList(gfx++, &envfx_dl_blue_snowflake); // snowflake with blue edge
    }

    for (i = 0; i < sSnowParticleCount; i += 5) {
        append_particle_vertex_buffer(gfx++, i, (s16 *) &vertex1, (s16 *) &vertex2, (s16 *) &vertex3, (Vtx *) &gSnowTempVtx);

        gSP1Triangle(gfx++,  0,  1,  2, 0x0);
        gSP1Triangle(gfx++,  3,  4,  5, 0x0);
        gSP1Triangle(gfx++,  6,  7,  8, 0x0);
        gSP1Triangle(gfx++,  9, 10, 11, 0x0);
        gSP1Triangle(gfx++, 12, 13, 14, 0x0);
    }

    gSPDisplayList(gfx++, &envfx_dl_end);
    gSPEndDisplayList(gfx++);

    return gfxStart;
}

/**
 * Updates the environment effects (snow, flowers, bubbles)
 * and returns a display list drawing them.
 */
Gfx *envfx_update_particles(s32 mode, Vec3s marioPos, Vec3s camTo, Vec3s camFrom) {
    Gfx *gfx;

    if (get_dialog_id() != DIALOG_NONE) {
        return NULL;
    }

    if (gEnvFxMode != ENVFX_MODE_NONE && gEnvFxMode != mode) {
        mode = ENVFX_MODE_NONE;
    }

    if (mode >= ENVFX_BUBBLE_START) {
        return envfx_update_bubbles(mode, marioPos, camTo, camFrom);
    }

    if (gEnvFxMode == ENVFX_MODE_NONE && !envfx_init_snow(mode)) {
        return NULL;
    }

    switch (mode) {
        case ENVFX_MODE_NONE:
            envfx_cleanup_snow(gEnvFxBuffer);
            return NULL;

        case ENVFX_SNOW_NORMAL:
            gfx = envfx_update_snow(1, marioPos, camFrom, camTo);
            break;

        case ENVFX_SNOW_WATER:
            gfx = envfx_update_snow(2, marioPos, camFrom, camTo);
            break;

        case ENVFX_SNOW_BLIZZARD:
            gfx = envfx_update_snow(3, marioPos, camFrom, camTo);
            break;

        default:
            return NULL;
    }

    return gfx;
}
