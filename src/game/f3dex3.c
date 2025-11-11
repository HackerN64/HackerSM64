#include <ultra64.h>

#ifdef F3DEX_GBI_3
#include "game_init.h"
#ifdef DEBUG_F3DEX3_PROFILER
#include "buffers/buffers.h"
#include "puppyprint.h"
#include "fasttext.h"
#endif

#ifdef F3DEX3_LIGHTING_ENGINE
#include "level_update.h"
#include "engine/math_util.h"
#include "config/config_world.h"
#endif

#include "f3dex3.h"

#ifdef DEBUG_F3DEX3_PROFILER
volatile u32 gF3DEX3ProfilerPage = 0;
volatile F3DEX3YieldDataFooter gRSPProfilingResults;

/**
 * Extracts the profiling information from the F3DEX3 graphics task yield data.
 */

void extract_f3dex3_profiler_data() {
   F3DEX3YieldDataFooter *footer = (F3DEX3YieldDataFooter*)((u8*)gGfxSPTaskYieldBuffer +
                                    OS_YIELD_DATA_SIZE - sizeof(F3DEX3YieldDataFooter));
    osInvalDCache(footer, sizeof(F3DEX3YieldDataFooter));
    gRSPProfilingResults.dummy_alignment[0] = footer->dummy_alignment[0];
    gRSPProfilingResults.dummy_alignment[1] = footer->dummy_alignment[1];
}

/**
 * Queries user input for the F3DEX3 profiler. See config/config_debug.h for more information.
 */

void query_f3dex3_profiler() {
    if (gPlayer1Controller->buttonPressed & L_TRIG && gPlayer1Controller->buttonPressed & D_CBUTTONS) {
        if (++gF3DEX3ProfilerPage > 4) {
            gF3DEX3ProfilerPage = 0;
        }
        gPlayer1Controller->buttonPressed &= ~D_CBUTTONS;
    }
}

void draw_f3dex3_profiler() {
    if (!gF3DEX3ProfilerPage) {
        return;
    }
    char profilerBuffers[9][32];

    sprintf(profilerBuffers[8], "Page %u of 4", gF3DEX3ProfilerPage);

    switch (gF3DEX3ProfilerPage) {
        case 4:
            sprintf(profilerBuffers[0], "Grucode cycles: %u", gRSPProfilingResults.c.ex3UcodeCycles);
            sprintf(profilerBuffers[1], "GLCK alive: %u", gRSPProfilingResults.c.commandsSampledGclkActive);
            sprintf(profilerBuffers[2], "RSP commands: %u", gRSPProfilingResults.c.dlCommandCount);
            sprintf(profilerBuffers[3], "Small RDP commands: %u", gRSPProfilingResults.c.smallRDPCommandCount);
            sprintf(profilerBuffers[4], "Matrix count: %u", gRSPProfilingResults.c.matrixCount);
            sprintf(profilerBuffers[5], "DMA stall cycles: %u", gRSPProfilingResults.c.stallDMACycles);
            sprintf(profilerBuffers[6], "Cycles / command: %f01", (f32) gRSPProfilingResults.c.ex3UcodeCycles / (f32) gRSPProfilingResults.c.dlCommandCount);
            sprintf(profilerBuffers[7], "");
        break;
        case 3:
            sprintf(profilerBuffers[0], "Vertex: %u", gRSPProfilingResults.b.vertexCount);
            sprintf(profilerBuffers[1], "Lit vertex: %u", gRSPProfilingResults.b.litVertexCount);
            sprintf(profilerBuffers[2], "Occluded triangles: %u", gRSPProfilingResults.b.occlusionPlaneCullCount);
            sprintf(profilerBuffers[3], "Clipped triangles: %u", gRSPProfilingResults.b.clippedTriCount);
            sprintf(profilerBuffers[4], "Overlay loads: %u", gRSPProfilingResults.b.allOverlayLoadCount);
            sprintf(profilerBuffers[5], "Lighting overlay loads: %u", gRSPProfilingResults.b.lightingOverlayLoadCount);
            sprintf(profilerBuffers[6], "Clipping overlay loads: %u", gRSPProfilingResults.b.clippingOverlayLoadCount);
            sprintf(profilerBuffers[7], "Miscallaneous overlay loads: %u", gRSPProfilingResults.b.miscOverlayLoadCount);
        break;
        case 2:
            sprintf(profilerBuffers[0], "Vertex cycles: %u", gRSPProfilingResults.a.vertexProcCycles);
            sprintf(profilerBuffers[1], "RSP fetched commands: %u", gRSPProfilingResults.a.fetchedDLCommandCount);
            sprintf(profilerBuffers[2], "RSP commands: %u", gRSPProfilingResults.a.dlCommandCount);
            sprintf(profilerBuffers[3], "Triangle cycles: %u", gRSPProfilingResults.a.triProcCycles);
            sprintf(profilerBuffers[4], "RSP FIFO stall: %u", gRSPProfilingResults.a.stallRDPFifoFullCycles);
            sprintf(profilerBuffers[5], "");
            sprintf(profilerBuffers[6], "");
            sprintf(profilerBuffers[7], "");
        break;
        case 1:
            sprintf(profilerBuffers[0], "Vertex: %u", gRSPProfilingResults.def.vertexCount);
            sprintf(profilerBuffers[1], "RDP triangles: %u", gRSPProfilingResults.def.rdpOutTriCount);
            sprintf(profilerBuffers[2], "RSP triangles: %u", gRSPProfilingResults.def.rspInTriCount);
            sprintf(profilerBuffers[3], "Rectangles: %u", gRSPProfilingResults.def.rectCount);
            sprintf(profilerBuffers[4], "RSP FIFO stall: %u", gRSPProfilingResults.def.stallRDPFifoFullCycles);
            sprintf(profilerBuffers[5], "");
            sprintf(profilerBuffers[6], "");
            sprintf(profilerBuffers[7], "");
        break;
    }

    prepare_blank_box();
    render_blank_box(32, 32, SCREEN_WIDTH - 32, SCREEN_HEIGHT - 32, 0, 0, 0, 168);
    finish_blank_box();

        Gfx* dlHead = gDisplayListHead;
        gDPPipeSync(dlHead++);
        gDPSetCycleType(dlHead++, G_CYC_1CYCLE);
        gDPSetRenderMode(dlHead++, G_RM_TEX_EDGE, G_RM_TEX_EDGE2);
        gDPSetTexturePersp(dlHead++, G_TP_NONE);
        gDPSetTextureFilter(dlHead++, G_TF_POINT);
        gDPSetTextureLUT(dlHead++, G_TT_NONE);
        drawSmallStringCol(&dlHead, 48, 48, profilerBuffers[0], 255, 255, 255);
        drawSmallStringCol(&dlHead, 48, 66, profilerBuffers[1], 255, 255, 255);
        drawSmallStringCol(&dlHead, 48, 84, profilerBuffers[2], 255, 255, 255);
        drawSmallStringCol(&dlHead, 48, 102, profilerBuffers[3], 255, 255, 255);
        drawSmallStringCol(&dlHead, 48, 120, profilerBuffers[4], 255, 255, 255);
        drawSmallStringCol(&dlHead, 48, 138, profilerBuffers[5], 255, 255, 255);
        drawSmallStringCol(&dlHead, 48, 156, profilerBuffers[6], 255, 255, 255);
        drawSmallStringCol(&dlHead, 48, 174, profilerBuffers[7], 255, 255, 255);
        drawSmallStringCol(&dlHead, 196, 186, profilerBuffers[8], 255, 255, 255);
        gDisplayListHead = dlHead;
}
#endif // DEBUG_F3DEX3_PROFILER
#ifdef F3DEX3_LIGHTING_ENGINE
typedef struct {
    u8 kc;
    u8 kq;
    u8 kl;
    s16 pos[3];
    u8 col[3];
    u8 size;
    u8 always;
    void *next;
} LinkedLight;

typedef struct {
    u8 col[3];
    s8 dir[3];
    u8 size;
    void *next;
} LinkedDirectionalLight;

static LinkedLight *sLinkedLightHead = NULL;
static LinkedDirectionalLight *sLinkedDirectionalLightHead = NULL;
static Ambient *sGlobalAmbientLight = NULL;

/**
  * Which light slot to start from.
  *
  * For compatibility with vanilla actors and levels, use NUMLIGHTS_3.
  * If you plan to use the lighting engine similarly to the older one by wiseguy, use NUMLIGHTS_1.
  */

u8 gLightNumBase = NUMLIGHTS_3; // Start from this slot when adding dynamic lights.

/**
  * Create an F3DEX3 point light.
  *
  * NOTE: Point lights are added based on their distance to Mario, with the amount of available point lights being how many are free after directional lights are loaded.
  * NOTE: Therefore, in a vanilla scene, the total amount of available point lights (with no global directional light) is 7.
  *
  * @x        X position of a light in worldspace.
  * @y        Y position of a light in worldspace.
  * @z        Z position of a light in worldspace.
  *
  * @r        Red color component.
  * @g        Green color component.
  * @b        Blue color component.
  *
  * @kc       Constant falloff.
  * @kq       Quadratic falloff.
  * @kl       Linear falloff.
  *
  * @size     Defines the light's size for when using F3DEX3 specular shading.
  * @always   Treat this light as closest to Mario when sorting (i.e. loading on a "first come, first served" basis).
  */

void add_point_light(s16 x, s16 y, s16 z, u8 r, u8 g, u8 b, u8 kc, u8 kq, u8 kl, u8 size, u8 always) {
    LinkedLight *light = alloc_display_list(sizeof(LinkedLight));
    LinkedLight *nextLight = sLinkedLightHead;
    light->pos[0] = x;
    light->pos[1] = y;
    light->pos[2] = z;

    light->kc = kc;
    light->kq = kq;
    light->kl = kl;

    light->col[0] = r;
    light->col[1] = g;
    light->col[2] = b;

    light->size = size;

    light->always = always;
    light->next = NULL;

    if (sLinkedLightHead == NULL) {
        sLinkedLightHead = light;
    } else {
        while (nextLight->next != NULL) {
            nextLight = nextLight->next;
        }
        nextLight->next = light;
    }
}

/**
  * Create a standard directional light.
  *
  * NOTE: These are given priority over point lights. You probably want at most 1-2 of these in a scene.
  *
  * @r      Red color component.
  * @b      Blue color component.
  * @g      Green color component.
  *
  * @x      X direction vector.
  * @y      Y direction vector.
  * @z      Z direction vector.
  *
  * @size   Defines the light's size for when using F3DEX3 specular shading.
  */

void add_directional_light(u8 r, u8 g, u8 b, s8 x, s8 y, s8 z, u8 size) {
    LinkedDirectionalLight *light = alloc_display_list(sizeof(LinkedDirectionalLight));
    LinkedDirectionalLight *nextLight = sLinkedDirectionalLightHead;

    light->col[0] = r;
    light->col[1] = g;
    light->col[2] = b;

    light->dir[0] = x;
    light->dir[1] = y;
    light->dir[2] = z;

    light->size = size;
    light->next = NULL;

    if (sLinkedDirectionalLightHead == NULL) {
        sLinkedDirectionalLightHead = light;
    } else {
        while (nextLight->next != NULL) {
            nextLight = nextLight->next;
        }
        nextLight->next = light;
    }
}

/**
  * Create a global ambient light.
  * NOTE: In the F3DEX3 microcode, ambient lights do not contribute towards the maximum amount of lights you can have.
  * NOTE: If you do not set an ambient light and add lights to your scene, the lighting engine will add one with all colors set to 0.
  *
  * @r    Red color component.
  * @g    Green color component.
  * @b    Blue color component.
  */

void set_ambient_light(u8 r, u8 g, u8 b) {
    if (sGlobalAmbientLight == NULL) {
        sGlobalAmbientLight = alloc_display_list(sizeof(Ambient));
    }

    sGlobalAmbientLight->l.col[0] = r;
    sGlobalAmbientLight->l.col[1] = g;
    sGlobalAmbientLight->l.col[2] = b;

    *(u32*)&sGlobalAmbientLight->l.colc[0] = *(u32*)&sGlobalAmbientLight->l.col[0];
}

static f32 calculate_distance_from_mario(s16 x, s16 y, s16 z) {
    if (gMarioState->marioObj == NULL) { // No distance calc on Mario-less levels
        return 0.0f;
    }

    f32 distX = ABS(gMarioState->pos[0] - (f32)x);
    f32 distY = ABS(gMarioState->pos[1] - (f32)y);
    f32 distZ = ABS(gMarioState->pos[2] - (f32)z);

    return sqr(distX + distY + distZ);
}

static void sort_linked_lights(LinkedLight **head) { // We can only load up to 9 point lights at a time, so we load the closest ones to Mario (or in a scene without Mario, the first x amount created)
    if (*head == NULL) return;

    LinkedLight *sorted = NULL;
    LinkedLight *current = *head;

    while (current != NULL) {
        LinkedLight *next = current->next;
        if (sorted == NULL || current->always == 1 || calculate_distance_from_mario(current->pos[0], current->pos[1], current->pos[2]) < calculate_distance_from_mario(sorted->pos[0], sorted->pos[1], sorted->pos[2])) {
            current->next = sorted;
            sorted = current;
        } else {
            LinkedLight *temp = sorted;
            LinkedLight *tempNext = sorted->next;
            while (temp->next != NULL && calculate_distance_from_mario(tempNext->pos[0], tempNext->pos[1], tempNext->pos[2]) < calculate_distance_from_mario(current->pos[0], current->pos[1], current->pos[2])) {
                temp = temp->next;
            }
            current->next = temp->next;
            temp->next = current;
        }
        current = next;
    }

    *head = sorted;
}

void setup_lighting_engine() {
    u8 lightNum = gLightNumBase;
    Light *l;
    LinkedDirectionalLight *cDir = sLinkedDirectionalLightHead;

    if (sLinkedDirectionalLightHead == NULL && sLinkedLightHead == NULL && sGlobalAmbientLight == NULL) { // don't even bother
        return;
    }

    while (cDir != NULL) {
        if (lightNum >= NUMLIGHTS_MAX) { // quit processing at 9 active lights
            lightNum = LIGHT_10;
            break;
        }

        l = alloc_display_list(sizeof(Light));

        bcopy(&cDir->col[0], &l->l.col[0], sizeof(u8) * 3);
        bcopy(&cDir->col[0], &l->l.colc[0], sizeof(u8) * 3);
        bcopy(&cDir->dir[0], &l->l.dir[0], sizeof(s8) * 3);

        l->l.type = 0;

        l->l.size = cDir->size;

        cDir = cDir->next;

        gSPLight(gDisplayListHead++, &l->l, lightNum);
        lightNum++;
        gSPNumLights(gDisplayListHead++, lightNum);
    }

    sort_linked_lights(&sLinkedLightHead);

    LinkedLight *cPoint = sLinkedLightHead;

    while (cPoint != NULL) {
        if (lightNum >= NUMLIGHTS_MAX) { // quit processing at 9 active lights
            lightNum = LIGHT_10;
            break;
        }

        l = alloc_display_list(sizeof(Light));

        bcopy(&cPoint->col[0], &l->p.col[0], sizeof(u8) * 3);
        bcopy(&cPoint->col[0], &l->p.colc[0], sizeof(u8) * 3);

        l->p.kc = cPoint->kc;
        l->p.kl = cPoint->kl;
        l->p.kq = cPoint->kq;

        l->p.pos[0] = cPoint->pos[0] / WORLD_SCALE;
        l->p.pos[1] = cPoint->pos[1] / WORLD_SCALE;
        l->p.pos[2] = cPoint->pos[2] / WORLD_SCALE;

        l->p.size = cPoint->size;

        cPoint = cPoint->next;

        gSPLight(gDisplayListHead++, &l->p, lightNum);
        lightNum++;
        gSPNumLights(gDisplayListHead++, lightNum);
    }

    if (sGlobalAmbientLight != NULL && lightNum != gLightNumBase) {
        gSPAmbient(gDisplayListHead++, sGlobalAmbientLight, lightNum);
        gSPNumLights(gDisplayListHead++, lightNum - 1);
    } else if (sGlobalAmbientLight == NULL && lightNum != gLightNumBase) { // We do not have an ambient light, so create one.
        set_ambient_light(0, 0, 0);
        gSPAmbient(gDisplayListHead++, sGlobalAmbientLight, lightNum);
        gSPNumLights(gDisplayListHead++, lightNum - 1);
    } else if (sGlobalAmbientLight != NULL && lightNum == gLightNumBase) { // we only set an ambient light
        gSPAmbient(gDisplayListHead++, sGlobalAmbientLight, gLightNumBase);
        gSPNumLights(gDisplayListHead++, gLightNumBase - 1);
    }

    sGlobalAmbientLight = NULL;
    sLinkedDirectionalLightHead = NULL;
    sLinkedLightHead = NULL;
}
#endif// F3DEX3_LIGHTING_ENGINE
#endif // F3DEX_GBI_3
