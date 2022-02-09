#include <ultra64.h>

#include "sm64.h"
#include "game_init.h"
#include "memory.h"
#include "envfx_snow.h"
#include "envfx_bubbles.h"
#include "engine/surface_collision.h"
#include "engine/math_util.h"
#include "engine/behavior_script.h"
#include "audio/external.h"
#include "textures.h"
#include "level_geo.h"

/**
 * This file implements environment effects that are not snow:
 * Flowers (unused), lava bubbles and jet stream/whirlpool bubbles.
 * Refer to 'envfx_snow.c' for more info about environment effects.
 * Note that the term 'bubbles' is used as a collective name for
 * effects in this file even though flowers aren't bubbles. For the
 * sake of concise naming, flowers fall under bubbles.
 */

struct EnvFxBubblesState EnvFxBubbleConfig;
struct EnvFxBubblesState *gEnvFxBubbleConfig = &EnvFxBubbleConfig;
static Gfx *sGfxCursor; // points to end of display list for bubble particles
static s32 sBubbleParticleCount;
static s32 sBubbleParticleMaxCount;

/// Template for a bubble particle triangle
static const Vtx gBubbleTempVtx[3] = {
    {{{     0,      0,      0}, 0, {  1544,    964}, {0xFF, 0xFF, 0xFF, 0xFF}}},
    {{{     0,      0,      0}, 0, {   522,   -568}, {0xFF, 0xFF, 0xFF, 0xFF}}},
    {{{     0,      0,      0}, 0, {  -498,    964}, {0xFF, 0xFF, 0xFF, 0xFF}}},
};

/**
 * Check whether the particle with the given index is
 * laterally within distance of point (x, z). Used to
 * kill flower and bubble particles.
 */
s32 particle_is_laterally_close(s32 index, s32 x, s32 z, s32 distance) {
    s32 xPos = (gEnvFxBuffer + index)->pos[0];
    s32 zPos = (gEnvFxBuffer + index)->pos[2];

    if (sqr(xPos - x) + sqr(zPos - z) > sqr(distance)) {
        return FALSE;
    }

    return TRUE;
}

/**
 * Generate a uniform random number in range [-2000, -1000[ or [1000, 2000[
 * Used to position flower particles
 */
s32 random_flower_offset(void) {
    s32 result = (random_float() * 2000.0f) - 1000.0f;
    if (result < 0) {
        result -= 1000;
    } else {
        result += 1000;
    }

    return result;
}

/**
 * Update flower particles. Flowers are scattered randomly in front of the
 * camera, and can land on any ground
 */
void envfx_update_flower(Vec3s centerPos) {
    struct EnvFxParticle *particle = gEnvFxBuffer;
    s32 i;
    s32 globalTimer = gGlobalTimer;

    s16 centerX = centerPos[0];
    s16 centerZ = centerPos[2];

    for (i = 0; i < sBubbleParticleMaxCount; i++) {
        particle->isAlive = particle_is_laterally_close(i, centerX, centerZ, 3000);
        if (!particle->isAlive) {
            particle->pos[0] = random_flower_offset() + centerX;
            particle->pos[2] = random_flower_offset() + centerZ;
            particle->pos[1] = find_floor_height(particle->pos[0], CELL_HEIGHT_LIMIT, particle->pos[2]);
            particle->isAlive = TRUE;
            particle->animFrame = random_float() * 5.0f;
        } else if (!(globalTimer & 3)) {
            particle->animFrame++;
            if (particle->animFrame > 5) {
                particle->animFrame = 0;
            }
        }

        particle++;
    }
}

/**
 * Update the position of a lava bubble to be somewhere around centerPos
 * Uses find_floor to find the height of lava, if no floor or a non-lava
 * floor is found the bubble y is set to -10000, which is why you can see
 * occasional lava bubbles far below the course in Lethal Lava Land.
 * In the second Bowser fight arena, the visual lava is above the lava
 * floor so lava-bubbles are not normally visible, only if you bring the
 * camera below the lava plane.
 */
void envfx_set_lava_bubble_position(s32 index, Vec3s centerPos) {
    struct EnvFxParticle *particle = (gEnvFxBuffer + index);
    struct Surface *surface;

    s16 centerX = centerPos[0];
    s16 centerY = centerPos[1];
    s16 centerZ = centerPos[2];

    particle->pos[0] = (random_float() * 6000.0f) - 3000.0f + centerX;
    particle->pos[2] = (random_float() * 6000.0f) - 3000.0f + centerZ;

    if (particle->pos[0] > 8000) {
        particle->pos[0] = 16000 - particle->pos[0];
    }
    if (particle->pos[0] < -8000) {
        particle->pos[0] = -16000 - particle->pos[0];
    }

    if (particle->pos[2] > 8000) {
        particle->pos[2] = 16000 - particle->pos[2];
    }
    if (particle->pos[2] < -8000) {
        particle->pos[2] = -16000 - particle->pos[2];
    }

    f32 floorY = find_floor(particle->pos[0], (centerY + 500), particle->pos[2], &surface);

    if (surface != NULL && surface->type == SURFACE_BURNING) {
        particle->pos[1] = floorY;
    } else {
        particle->pos[1] = FLOOR_LOWER_LIMIT_MISC;
    }
}

/**
 * Update lava bubble animation and give the bubble a new position if the
 * animation is over.
 */
void envfx_update_lava(Vec3s centerPos) {
    struct EnvFxParticle *particle = gEnvFxBuffer;
    s32 i;
    s32 globalTimer = gGlobalTimer;

    for (i = 0; i < sBubbleParticleMaxCount; i++) {
        if (!particle->isAlive) {
            envfx_set_lava_bubble_position(i, centerPos);
            particle->isAlive = TRUE;
        } else if (!(globalTimer & 1)) {
            particle->animFrame++;
            if (particle->animFrame > 8) {
                particle->isAlive = FALSE;
                particle->animFrame = 0;
            }
        }

        particle++;
    }

    if (((s8)(s32)(random_float() * 16.0f)) == 8) {
        play_sound(SOUND_GENERAL_QUIET_BUBBLE2, gGlobalSoundSource);
    }
}

/**
 * Rotate the input x, y and z around the rotation origin of the whirlpool
 * according to the pitch and yaw of the whirlpool.
 */
void envfx_rotate_around_whirlpool(Vec3i pos) {
    s32 vecX = pos[0] - gEnvFxBubbleConfig->dest[0];
    s32 vecY = pos[1] - gEnvFxBubbleConfig->dest[1];
    s32 vecZ = pos[2] - gEnvFxBubbleConfig->dest[2];
    f32 cosPitch = coss(gEnvFxBubbleConfig->angle[0]);
    f32 sinPitch = sins(gEnvFxBubbleConfig->angle[0]);
    f32 cosMYaw = coss(-gEnvFxBubbleConfig->angle[1]);
    f32 sinMYaw = sins(-gEnvFxBubbleConfig->angle[1]);

    f32 rotatedX = (vecX * cosMYaw ) - (sinMYaw  * cosPitch * vecY) - (sinPitch * sinMYaw * vecZ);
    f32 rotatedY = (vecX * sinMYaw ) + (cosPitch * cosMYaw  * vecY) - (sinPitch * cosMYaw * vecZ);
    f32 rotatedZ = (vecY * sinPitch) + (cosPitch * vecZ);

    pos[0] = gEnvFxBubbleConfig->dest[0] + (s32) rotatedX;
    pos[1] = gEnvFxBubbleConfig->dest[1] + (s32) rotatedY;
    pos[2] = gEnvFxBubbleConfig->dest[2] + (s32) rotatedZ;
}

/**
 * Check whether a whirlpool bubble is alive. A bubble respawns when it is too
 * low or close to the center.
 */
s32 envfx_is_whirlpool_bubble_alive(s32 index) {
    if ((gEnvFxBuffer + index)->bubbleY < gEnvFxBubbleConfig->dest[1] - 100) {
        return FALSE;
    }

    if ((gEnvFxBuffer + index)->distFromCenter < 10) {
        return FALSE;
    }

    return TRUE;
}

/**
 * Update whirlpool particles. Whirlpool particles start high and far from
 * the center and get sucked into the sink in a spiraling motion.
 */
void envfx_update_whirlpool(void) {
    struct EnvFxParticle *particle = gEnvFxBuffer;
    s32 i;

    for (i = 0; i < sBubbleParticleMaxCount; i++) {
        particle->isAlive = envfx_is_whirlpool_bubble_alive(i);
        if (!particle->isAlive) {
            particle->distFromCenter = random_float() * 1000.0f;
            particle->angleFromCenter = random_float() * 65536.0f;
            particle->pos[0] =
                gEnvFxBubbleConfig->src[0]
                + sins(particle->angleFromCenter) * particle->distFromCenter;
            particle->pos[2] =
                gEnvFxBubbleConfig->src[2]
                + coss(particle->angleFromCenter) * particle->distFromCenter;
            particle->bubbleY =
                gEnvFxBubbleConfig->src[1] + (random_float() * 100.0f - 50.0f);
            particle->pos[1] = particle->bubbleY;
            particle->isAlive = TRUE;

            envfx_rotate_around_whirlpool(particle->pos);
        } else {
            particle->distFromCenter -= 40;
            particle->angleFromCenter +=
                (s16)(3000 - particle->distFromCenter * 2) + 0x400;
            particle->pos[0] =
                gEnvFxBubbleConfig->src[0]
                + sins(particle->angleFromCenter) * particle->distFromCenter;
            particle->pos[2] =
                gEnvFxBubbleConfig->src[2]
                + coss(particle->angleFromCenter) * particle->distFromCenter;
            particle->bubbleY -= 40 - ((s16)particle->distFromCenter / 100);
            particle->pos[1] = particle->bubbleY;

            envfx_rotate_around_whirlpool(particle->pos);
        }

        particle++;
    }
}

/**
 * Check whether a jet stream bubble should respawn. Happens if it is laterally
 * 1000 units away from the source or 1500 units above it.
 */
s32 envfx_is_jestream_bubble_alive(s32 index) {
    if (!particle_is_laterally_close(index, gEnvFxBubbleConfig->src[0],
                                     gEnvFxBubbleConfig->src[2], 1000)
        || gEnvFxBubbleConfig->src[1] + 1500 < (gEnvFxBuffer + index)->pos[1]) {
        return FALSE;
    }

    return TRUE;
}

/**
 * Update the positions of jet stream bubble particles.
 * They move up and outwards.
 */
void envfx_update_jetstream(void) {
    struct EnvFxParticle *particle = gEnvFxBuffer;
    s32 i;

    for (i = 0; i < sBubbleParticleMaxCount; i++) {
        particle->isAlive = envfx_is_jestream_bubble_alive(i);
        if (!particle->isAlive) {
            particle->distFromCenter = random_float() * 300.0f;
            particle->angleFromCenter = random_u16();
            particle->pos[0] =
                gEnvFxBubbleConfig->src[0]
                + sins(particle->angleFromCenter) * particle->distFromCenter;
            particle->pos[2] =
                gEnvFxBubbleConfig->src[2]
                + coss(particle->angleFromCenter) * particle->distFromCenter;
            particle->pos[1] =
                gEnvFxBubbleConfig->src[1] + (random_float() * 400.0f - 200.0f);
        } else {
            particle->distFromCenter += 10;
            particle->pos[0] += sins(particle->angleFromCenter) * 10.0f;
            particle->pos[2] += coss(particle->angleFromCenter) * 10.0f;
            particle->pos[1] -= (particle->distFromCenter / 30) - 50;
        }

        particle++;
    }
}

/**
 * Initialize bubble (or flower) effect by allocating a buffer to store
 * the state of each particle and setting the initial and max count.
 * Analogous to init_snow_particles, but for bubbles.
 */
s32 envfx_init_bubble(s32 mode) {
    s32 i;

    switch (mode) {
        case ENVFX_MODE_NONE:
            return FALSE;

        case ENVFX_FLOWERS:
            sBubbleParticleCount    = 30;
            sBubbleParticleMaxCount = 30;
            break;

        case ENVFX_LAVA_BUBBLES:
            sBubbleParticleCount    = 15;
            sBubbleParticleMaxCount = 15;
            break;

        case ENVFX_WHIRLPOOL_BUBBLES:
            sBubbleParticleCount = 60;
            break;

        case ENVFX_JETSTREAM_BUBBLES:
            sBubbleParticleCount = 60;
            break;
    }

    gEnvFxBuffer = mem_pool_alloc(gEffectsMemoryPool,
                                  sBubbleParticleCount * sizeof(struct EnvFxParticle));
    if (gEnvFxBuffer == NULL) {
        return FALSE;
    }

    bzero(gEnvFxBuffer, sBubbleParticleCount * sizeof(struct EnvFxParticle));
    bzero(gEnvFxBubbleConfig, sizeof(struct EnvFxBubblesState));

    if (mode == ENVFX_LAVA_BUBBLES) {
        for (i = 0; i < sBubbleParticleCount; i++) {
            (gEnvFxBuffer + i)->animFrame = random_float() * 7.0f;
        }
    }

    gEnvFxMode = mode;
    return TRUE;
}

/**
 * Update particles depending on mode.
 * Also sets the given vertices to the correct shape for each mode,
 * though they are not being rotated yet.
 */
void envfx_bubbles_update_switch(s32 mode, Vec3s camTo, Vec3s vertex1, Vec3s vertex2, Vec3s vertex3) {
    switch (mode) {
        case ENVFX_FLOWERS:
            envfx_update_flower(camTo);
            vertex1[0] =   50; vertex1[1] =   0; vertex1[2] = 0;
            vertex2[0] =    0; vertex2[1] =  75; vertex2[2] = 0;
            vertex3[0] =  -50; vertex3[1] =   0; vertex3[2] = 0;
            break;

        case ENVFX_LAVA_BUBBLES:
            envfx_update_lava(camTo);
            vertex1[0] =  100; vertex1[1] =   0; vertex1[2] = 0;
            vertex2[0] =    0; vertex2[1] = 150; vertex2[2] = 0;
            vertex3[0] = -100; vertex3[1] =   0; vertex3[2] = 0;
            break;

        case ENVFX_WHIRLPOOL_BUBBLES:
            envfx_update_whirlpool();
            vertex1[0] =   40; vertex1[1] =   0; vertex1[2] = 0;
            vertex2[0] =    0; vertex2[1] =  60; vertex2[2] = 0;
            vertex3[0] =  -40; vertex3[1] =   0; vertex3[2] = 0;
            break;

        case ENVFX_JETSTREAM_BUBBLES:
            envfx_update_jetstream();
            vertex1[0] =   40; vertex1[1] =   0; vertex1[2] = 0;
            vertex2[0] =    0; vertex2[1] =  60; vertex2[2] = 0;
            vertex3[0] =  -40; vertex3[1] =   0; vertex3[2] = 0;
            break;
    }
}

/**
 * Appends to the enfvx display list a command setting the appropriate texture
 * for a specific particle. The display list is not passed as parameter but uses
 * the global sGfxCursor instead.
 */
void envfx_set_bubble_texture(s32 mode, s16 index) {
    void **imageArr;
    s16 frame = (gEnvFxBuffer + index)->animFrame;

    switch (mode) {
        case ENVFX_FLOWERS:
            imageArr = segmented_to_virtual(&flower_bubbles_textures_ptr_0B002008);
            frame = (gEnvFxBuffer + index)->animFrame;
            break;

        case ENVFX_LAVA_BUBBLES:
            imageArr = segmented_to_virtual(&lava_bubble_ptr_0B006020);
            frame = (gEnvFxBuffer + index)->animFrame;
            break;

        case ENVFX_WHIRLPOOL_BUBBLES:
        case ENVFX_JETSTREAM_BUBBLES:
            imageArr = segmented_to_virtual(&bubble_ptr_0B006848);
            frame = 0;
            break;
        default:
            return;
    }

    gDPSetTextureImage(sGfxCursor++, G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, *(imageArr + frame));
    gSPDisplayList(sGfxCursor++, &envfx_dl_bubbles_texture_format);
}

/**
 * Updates the bubble particle positions, then generates and returns a display
 * list drawing them.
 */
Gfx *envfx_update_bubble_particles(s32 mode, UNUSED Vec3s marioPos, Vec3s camFrom, Vec3s camTo) {
    s32 i;
    s16 radius, pitch, yaw;

    Vec3s vertex1;
    Vec3s vertex2;
    Vec3s vertex3;

    Gfx *gfxStart = alloc_display_list(((sBubbleParticleMaxCount / 5) * 10 + sBubbleParticleMaxCount + 3)
                                       * sizeof(Gfx));
    if (gfxStart == NULL) {
        return NULL;
    }

    sGfxCursor = gfxStart;

    vec3s_get_dist_and_angle(camTo, camFrom, &radius, &pitch, &yaw);
    envfx_bubbles_update_switch(mode, camTo, vertex1, vertex2, vertex3);
    rotate_triangle_vertices(vertex1, vertex2, vertex3, pitch, yaw);

    gSPDisplayList(sGfxCursor++, &envfx_dl_bubbles_begin);

    for (i = 0; i < sBubbleParticleMaxCount; i += 5) {
        gDPPipeSync(sGfxCursor++);
        envfx_set_bubble_texture(mode, i);
        append_particle_vertex_buffer(sGfxCursor++, i, vertex1, vertex2, vertex3, (Vtx *) &gBubbleTempVtx);
        gSP1Triangle(sGfxCursor++,  0,  1,  2, 0x0);
        gSP1Triangle(sGfxCursor++,  3,  4,  5, 0x0);
        gSP1Triangle(sGfxCursor++,  6,  7,  8, 0x0);
        gSP1Triangle(sGfxCursor++,  9, 10, 11, 0x0);
        gSP1Triangle(sGfxCursor++, 12, 13, 14, 0x0);
    }

    gSPDisplayList(sGfxCursor++, &envfx_dl_end);
    gSPEndDisplayList(sGfxCursor++);

    return gfxStart;
}

/**
 * Set the maximum particle count from the gEnvFxBubbleConfig variable,
 * which is set by the whirlpool or jet stream behavior.
 */
void envfx_set_max_bubble_particles(s32 mode) {
    switch (mode) {
        case ENVFX_WHIRLPOOL_BUBBLES:
        case ENVFX_JETSTREAM_BUBBLES:
            sBubbleParticleMaxCount = gEnvFxBubbleConfig->particleCount;
            break;
    }
}

/**
 * Update bubble-like environment effects. Assumes the mode is larger than 10,
 * lower modes are snow effects which are updated in a different function.
 * Returns a display list drawing the particles.
 */
Gfx *envfx_update_bubbles(s32 mode, Vec3s marioPos, Vec3s camTo, Vec3s camFrom) {
    Gfx *gfx;

    if (gEnvFxMode == ENVFX_MODE_NONE && !envfx_init_bubble(mode)) {
        return NULL;
    }

    envfx_set_max_bubble_particles(mode);

    if (sBubbleParticleMaxCount == 0) {
        return NULL;
    }

    switch (mode) {
        case ENVFX_FLOWERS:
            gfx = envfx_update_bubble_particles(ENVFX_FLOWERS, marioPos, camFrom, camTo);
            break;

        case ENVFX_LAVA_BUBBLES:
            gfx = envfx_update_bubble_particles(ENVFX_LAVA_BUBBLES, marioPos, camFrom, camTo);
            break;

        case ENVFX_WHIRLPOOL_BUBBLES:
            gfx = envfx_update_bubble_particles(ENVFX_WHIRLPOOL_BUBBLES, marioPos, camFrom, camTo);
            break;

        case ENVFX_JETSTREAM_BUBBLES:
            gfx = envfx_update_bubble_particles(ENVFX_JETSTREAM_BUBBLES, marioPos, camFrom, camTo);
            break;

        default:
            return NULL;
    }

    return gfx;
}
