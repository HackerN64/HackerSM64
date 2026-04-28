/**
--------------Puppyprint 1.0 by Fazana--------------
Includes a few printing functions to fit any purpose.
print_small_text is intended to replace print_generic_string in use, as it uses a far more optimised way of doing things,
supports real time ascii conversion, and also supports many fun effects to spice up the text.
Any usage of gDPSetEnvColor should ideally be replaced with print_set_envcolour because it helps with some optimisations.

As for the profiler, you can hold dpad up, and press L to toggle the display.
Inside this display, if you press up on the dpad again, you can switch between performance, and memory view.
If you press dpad down, you can toggle the benchmarking display.
You can press dpad left or right to change which option, and you can measure game thread or audio thread performance by default.
There's also a custom option that's left blank. It runs benchmark_custom which can contain anything of your choice.
You can press dpad right to cycle between collision visuals, from surface collision, hitbox collision, both, or neither.
dpad left will toggle the logging view, which will display a number of strings you've sent through for debugging purposes, like
a modern game engine's developer's console.

- Collision marks the time it takes to generate and process collision.
- Behaviour marks the time it takes for objects to perform their behaviours. This excludes collision.
- Graph measures the time it takes to process the node graphs, which is all the 3D geometry and rendering.
- Audio measures the time it takes to process the audio samples, this excludes time spent loading.
- DMA measures the time it takes to load things. In Vanilla, Mario's animations and audio samples are loaded from ROM as needed.
**/

#include <ultra64.h>

#include "config.h"
#include "seq_ids.h"
#include "game_init.h"
#include "sound_init.h"
#include "memory.h"
#include "print.h"
#include "string.h"
#include "stdarg.h"
#include "printf.h"
#include "engine/math_util.h"
#include "engine/behavior_script.h"
#include "camera.h"
#include "puppyprint.h"
#include "level_update.h"
#include "object_list_processor.h"
#include "engine/surface_load.h"
#include "audio/data.h"
#include "audio/external.h"
#include "audio/heap.h"
#include "audio/load.h"
#include "hud.h"
#include "debug_box.h"
#include "color_presets.h"
#include "buffers/buffers.h"
#include "profiling.h"
#include "segment_symbols.h"

#ifdef PUPPYPRINT

#define TAB_WIDTH 16

#ifdef ENABLE_CREDITS_BENCHMARK
u8 fDebug = TRUE;
#else
u8 fDebug = FALSE;
#endif
u8 sPuppyprintTextBuffer[PUPPYPRINT_DEFERRED_BUFFER_SIZE];
u32 sPuppyprintTextBufferPos; // Location in the buffer of puppyprint deferred text.
ColorRGBA gCurrEnvCol;

#ifdef PUPPYPRINT_DEBUG

s8 logViewer    = FALSE;
u8 sPPDebugPage = 0;
u8 sDebugMenu   = FALSE;
u8 sDebugOption = 0;
s32 ramsizeSegment[NUM_TLB_SEGMENTS + 1] = { 0 };
s32 mempool;
u32 gPoolMem;
u32 gPPSegScroll = 0;
u32 gMiscMem = 0;
struct CallCounter gPuppyCallCounter;

#define NUM_RAM_CHARS 32

// Another epic lookup table, for text this time.
const char ramNames[][NUM_RAM_CHARS] = {
    "Buffers",
    "Main",
    "Engine",
    "Framebuffers",
    "ZBuffer",
    "Goddard",
    "Pools",
    "Collision",
    "Misc",
    "Audio Heap"
};

enum RamNames {
    RAM_BUFFERS,
    RAM_MAIN,
    RAM_ENGINE,
    RAM_FRAMEBUFFERS,
    RAM_ZBUFFER,
    RAM_GODDARD,
    RAM_POOLS,
    RAM_COLLISION,
    RAM_MISC,
    RAM_AUDIO
};

const char segNames[][NUM_RAM_CHARS] = {
    "HUD",
    "Common1 GFX",
    "Group0 GFX",
    "GroupA GFX",
    "GroupB GFX",
    "Level GFX",
    "Common0 GFX",
    "Textures",
    "Skybox",
    "Effects",
    "GroupA Geo",
    "GroupB Geo",
    "Level Geo",
    "Common0 Geo",
    "Entry",
    "Mario Anims",
    "Demos",
    "Bhv Scripts",
    "Menu",
    "Level Scripts",
    "Common1 Geo",
    "Group0 Geo",
    "",
    "Languages"
};

const s8 nameTable = sizeof(ramNames) / NUM_RAM_CHARS;

void puppyprint_calculate_ram_usage(void) {
    ramsizeSegment[RAM_BUFFERS] = (u32)&_buffersSegmentBssEnd - (u32)&_buffersSegmentBssStart - gAudioHeapSize;
    ramsizeSegment[RAM_MAIN] = (u32)&_mainSegmentEnd - (u32)&_mainSegmentStart;
    ramsizeSegment[RAM_ENGINE] = (u32)&_engineSegmentEnd - (u32)&_engineSegmentStart;
    ramsizeSegment[RAM_FRAMEBUFFERS] = (u32)&_framebuffersSegmentBssEnd - (u32)&_framebuffersSegmentBssStart;
    ramsizeSegment[RAM_ZBUFFER] = (u32)&_zbufferSegmentBssEnd - (u32)&_zbufferSegmentBssStart;
    ramsizeSegment[RAM_GODDARD] = (u32)&_goddardSegmentEnd - (u32)&_goddardSegmentStart;
    ramsizeSegment[RAM_POOLS] = gPoolMem;
    ramsizeSegment[RAM_COLLISION] = ((u32) gCurrStaticSurfacePoolEnd - (u32) gCurrStaticSurfacePool) + ((u32) gDynamicSurfacePoolEnd - (u32) gDynamicSurfacePool);
    ramsizeSegment[RAM_MISC] = gMiscMem;
    ramsizeSegment[RAM_AUDIO] = gAudioHeapSize;
}

#ifdef PUPPYPRINT_DEBUG_CYCLES
    #define CYCLE_CONV
    #define RDP_CYCLE_CONV(x) (x)
#else
    #define CYCLE_CONV OS_CYCLES_TO_USEC
    #define RDP_CYCLE_CONV(x) ((10 * (x)) / 625) // 62.5 million cycles per frame
#endif

// RGB colour lookup table for colouring all the funny ram prints.
ColorRGB colourChart[NUM_TLB_SEGMENTS + 1] = {
    { 255,   0,   0 },
    {  63,  63, 255 },
    {   0, 255,   0 },
    { 255, 255,   0 },
    { 255,   0, 255 },
    { 255, 127,   0 },
    {   0, 255, 255 },
    {  51, 255,  51 },
    { 255, 153, 153 },
    { 204,   0, 102 },
    {   0, 153, 153 },
    { 153, 255, 153 },
    { 127, 127, 255 },
    { 255, 127, 255 },
    { 218, 165,  32 },
    { 107, 142,  35 },
    { 188, 143, 143 },
    { 210, 105,  30 },
    { 154, 205,  50 },
    { 165,  42,  42 },
    { 255, 105, 180 },
    { 139,  69,  19 },
    { 250, 240, 230 },
    {  95, 158, 160 },
    {  60, 179, 113 },
    { 255,  69,   0 },
    { 128,   0,   0 },
    { 216, 191, 216 },
    { 244, 164,  96 },
    { 176, 196, 222 },
    { 255, 255, 255 }
};

void swap(int* xp, int* yp)
{
    int temp = *xp;
    *xp = *yp;
    *yp = temp;
}

void swapu(u8* xp, u8* yp)
{
    u8 temp = *xp;
    *xp = *yp;
    *yp = temp;
}

void sort_numbers(s32 *values, u8 *values2)
{
    int i, j, min_idx;

    // One by one move boundary of unsorted subarray
    for (i = 0; i < NUM_TLB_SEGMENTS; i++) {

        if (values[i] == 0)
            continue;
        // Find the minimum element in unsorted array
        min_idx = i;
        for (j = i + 1; j < NUM_TLB_SEGMENTS; j++)
            if (values[j] > values[min_idx])
                min_idx = j;

        // Swap the found minimum element
        // with the first element
        swap(&values[min_idx], &values[i]);
        swapu(&values2[min_idx], &values2[i]);
    }
}

void set_segment_memory_printout(u32 segment, u32 amount) {
    ramsizeSegment[segment + nameTable - 2] = amount;
}

void print_ram_overview(void) {
    char textBytes[64];
    s32 y = 56;
    f32 ramSize = RAM_END - 0x80000000;
    s32 tempNums[32];
    u8 tempPos[32] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};
    prepare_blank_box();
    render_blank_box(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, 0, 0, 0, 168);
    finish_blank_box();

    memcpy(tempNums, &ramsizeSegment, 32 * 4);

    sort_numbers(tempNums, tempPos);

    print_set_envcolour(255, 255, 255, 255);
    sprintf(textBytes, "Total:");
    print_small_text_light(24, 16- gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_DEFAULT);
    sprintf(textBytes, "0x%06X",RAM_END - 0x80000000);
    print_small_text_light(SCREEN_WIDTH/2, 16 - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_DEFAULT);
    sprintf(textBytes, "0x%X", mempool);
    print_small_text_light(SCREEN_WIDTH - 24, 16 - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_RIGHT, PRINT_ALL, FONT_DEFAULT);
    sprintf(textBytes, "Used:");
    print_small_text_light(24, 28- gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_DEFAULT);
    sprintf(textBytes, "0x%06X", (RAM_END - 0x80000000) - (main_pool_available() - 0x400));
    print_small_text_light(SCREEN_WIDTH/2, 28 - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_DEFAULT);
    sprintf(textBytes, "(%2.3f%%)", 100.0f - (((f32)(main_pool_available() - 0x400) / (f32)(RAM_END - 0x80000000)) * 100));
    print_small_text_light(SCREEN_WIDTH - 24, 28 - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_RIGHT, PRINT_ALL, FONT_DEFAULT);
    sprintf(textBytes, "Free:");
    print_small_text_light(24, 40 - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_DEFAULT);
    sprintf(textBytes, "0x%X", (main_pool_available() - 0x400));
    print_small_text_light(SCREEN_WIDTH/2, 40 - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_DEFAULT);
    sprintf(textBytes, "(%2.3f%%)", (((f32)(main_pool_available() - 0x400) / (f32)(RAM_END - 0x80000000)) * 100));
    print_small_text_light(SCREEN_WIDTH - 24, 40 - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_RIGHT, PRINT_ALL, FONT_DEFAULT);
    for (u8 i = 0; i < NUM_TLB_SEGMENTS; i++) {
        if (tempNums[i] == 0) {
            continue;
        }
        if (y - gPPSegScroll > 0 && y - gPPSegScroll < SCREEN_HEIGHT) {
            if (tempPos[i] < nameTable) {
                sprintf(textBytes, "%s:", ramNames[tempPos[i]]);
            } else {
                sprintf(textBytes, "%s:", segNames[tempPos[i] - nameTable]);
            }
            print_small_text_light(24, y - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_DEFAULT);
            sprintf(textBytes, "0x%X", tempNums[i]);
            print_small_text_light(SCREEN_WIDTH/2, y - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_DEFAULT);
            sprintf(textBytes, "(%2.3f%%)", ((f32)tempNums[i] / ramSize) * 100.0f);
            print_small_text_light(SCREEN_WIDTH - 24, y - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_RIGHT, PRINT_ALL, FONT_DEFAULT);
        }
        y += 12;
    }
}

static const char *audioPoolNames[NUM_AUDIO_POOLS] = {
    "Audio Init Pool:\t\t\t\t  ",
    "Notes And Buffers Pool:\t  ",
    "Persistent Sequence Pool:  ",
    "Persistent Bank Pool:\t\t  ",
    "Temporary Sequence Pool:\t  ",
    "Temporary Bank Pool:\t\t  ",
#ifdef BETTER_REVERB
    "Better Reverb Pool:\t\t  ",
#endif
};

#ifdef AUDIO_PROFILING
static const char *audioBenchmarkNames[PROFILER_TIME_SUB_AUDIO_END - PROFILER_TIME_SUB_AUDIO_START] = {
    "Sequence Processing:\t  ",
    "  Script Parsing:\t\t\t    ",
    "  Reclaim Notes:\t\t\t    ",
    "  Note Processing:\t\t    ",
    "Synthesis:\t\t\t\t\t  ",
    "  Note Processing:\t\t    ",
    "  Envelopes / Reverb:\t    ",
    "  Audio DMAs:\t\t\t\t    ",
    "Audio Update:\t\t\t\t  "
};

STATIC_ASSERT(ARRAY_COUNT(audioBenchmarkNames) == PROFILER_TIME_SUB_AUDIO_END - PROFILER_TIME_SUB_AUDIO_START, "audioBenchmarkNames has incorrect number of entries!");
#endif

static void print_audio_ram_overview(s32 x, char *textBytes) {
    s32 percentage = 0;
    s32 y = SCREEN_HEIGHT - 6;
    s32 totalMemory[2] = { 0, 0 };
    s32 audioPoolSizes[NUM_AUDIO_POOLS][2];

    puppyprint_get_allocated_pools(audioPoolSizes[0]);

    for (s8 i = NUM_AUDIO_POOLS - 1; i >= 0; i--) {
        y -= 12;

        if (audioPoolSizes[i][0] == 0) {
            percentage = 1000;
        } else {
            percentage = (((s64) audioPoolSizes[i][1] * 1000) / audioPoolSizes[i][0]);
        }

        sprintf(textBytes, "  %s%X / %X (%d.%d%%)", audioPoolNames[i],
                audioPoolSizes[i][1],
                audioPoolSizes[i][0],
                percentage / 10,
                percentage % 10);

        print_set_envcolour(colourChart[i][0],
                            colourChart[i][1],
                            colourChart[i][2], 255);
        print_small_text_light(x, y, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);

        totalMemory[0] += audioPoolSizes[i][0];
        totalMemory[1] += audioPoolSizes[i][1];
    }
    y -= 12;

    if (totalMemory[0] == 0) {
        percentage = 0;
    } else {
        percentage = (((s64) totalMemory[1] * 1000) / totalMemory[0]);
    }
    if (totalMemory[0] == gAudioHeapSize) {
        sprintf(textBytes, "TOTAL AUDIO MEMORY:\t\t%X / %X (%d.%d%%)",
                totalMemory[1],
                totalMemory[0],
                percentage / 10,
                percentage % 10);
    } else {
        sprintf(textBytes, "TOTAL AUDIO MEMORY:\t\t%X / %X (Incorrect!)",
                totalMemory[1],
                totalMemory[0]);
    }

    print_set_envcolour(255, 255, 255, 255);
    print_small_text_light(x, y, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
}

static void print_audio_overview(void) {
    char textBytes[128];
    const s32 x = 12;
    s32 y = 6;
    u32 us;
    u32 percentInt;
    u32 percentDec;

    prepare_blank_box();
    render_blank_box(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, 64, 64, 64, 95);
    finish_blank_box();

    us = OS_CYCLES_TO_USEC(all_profiling_data[PROFILER_TIME_AUDIO].total / PROFILING_BUFFER_SIZE) * 2;
    percentInt = us / 33;
    percentDec = percentInt % 10;
    percentInt /= 10;

    sprintf(textBytes, "TOTAL AUDIO CPU:\t\t%d (%d.%d%%)", us, percentInt, percentDec);

    print_set_envcolour(255, 255, 255, 255);
    print_small_text_light(x, y, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);

#ifdef AUDIO_PROFILING
    for (s32 i = 0; i < ARRAY_COUNT(audioBenchmarkNames); i++) {
        y += 12;

        us = OS_CYCLES_TO_USEC(all_profiling_data[PROFILER_TIME_SUB_AUDIO_START + i].total / PROFILING_BUFFER_SIZE) * 2;

        if (audioBenchmarkNames[i][0] != ' ') {
            percentInt = us / 33;
            percentDec = percentInt % 10;
            percentInt /= 10;
            sprintf(textBytes, "  %s%d (%d.%d%%)", audioBenchmarkNames[i], us, percentInt, percentDec);
        } else {
            sprintf(textBytes, "  %s%d", audioBenchmarkNames[i], us);
        }

        print_set_envcolour(colourChart[NUM_AUDIO_POOLS + i][0],
                            colourChart[NUM_AUDIO_POOLS + i][1],
                            colourChart[NUM_AUDIO_POOLS + i][2], 255);
        print_small_text_light(x, y, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
    }
#else
        print_set_envcolour(255, 95, 95, 255);
        print_small_text(x + 8, y + 12, "Verbose audio profiling is disabled!\nPlease toggle the <COL_7F7FFFFF>AUDIO PROFILING<COL_--------> define\n"
            "In <COL_FFFF1FFF>profiling.h<COL_-------->.", PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
#endif

    print_audio_ram_overview(x, textBytes);
}

char consoleLogTable[LOG_BUFFER_SIZE][255];

static char *write_to_buf(char *buffer, const char *data, size_t size) {
    return (char *) memcpy(buffer, data, size) + size;
}

void append_puppyprint_log(const char *str, ...) {
    char textBytes[255];

    memset(textBytes, 0, sizeof(textBytes));
    va_list arguments;
    va_start(arguments, str);
    if ((_Printf(write_to_buf, textBytes, str, arguments)) <= 0) {
        va_end(arguments);
        return;
    }
#ifdef UNF
    osSyncPrintf(textBytes);
#endif
    for (u8 i = 0; i < (LOG_BUFFER_SIZE - 1); i++) {
        memcpy(consoleLogTable[i], consoleLogTable[i + 1], 255);
    }
    memcpy(consoleLogTable[LOG_BUFFER_SIZE - 1], textBytes, 255);
    va_end(arguments);
}

#define LINE_HEIGHT (8 + ((LOG_BUFFER_SIZE - 1) * 12))
void print_console_log(void) {
    prepare_blank_box();
    render_blank_box(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, 0, 0, 0, 96);
    finish_blank_box();

    for (u8 i = 0; i < LOG_BUFFER_SIZE; i++) {
        if (consoleLogTable[i][0] == '\0') {
            continue;
        }
        print_small_text_light(16, (LINE_HEIGHT - (i * 12)), consoleLogTable[i], PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_DEFAULT);
    }
}
#undef LINE_HEIGHT

extern u8 viewCycle;
extern s16 gVisualSurfaceCount;
#ifndef VISUAL_DEBUG
    #define gVisualSurfaceCount 0
#endif

void puppyprint_render_collision(void) {
    char textBytes[128];
    sprintf(textBytes, "Static Pool Size: 0x%X\nDynamic Pool Size: 0x%X\nDynamic Pool Used: 0x%X\nSurfaces Allocated: %d\nNodes Allocated: %d", 
    gTotalStaticSurfaceData,
    DYNAMIC_SURFACE_POOL_SIZE,
    (uintptr_t)gDynamicSurfacePoolEnd - (uintptr_t)gDynamicSurfacePool,
    gSurfacesAllocated, gSurfaceNodesAllocated);
    print_small_text_light(SCREEN_WIDTH-16, 60, textBytes, PRINT_TEXT_ALIGN_RIGHT, PRINT_ALL, 1);

#ifdef VISUAL_DEBUG
    print_small_text_light(160, (SCREEN_HEIGHT - 42), "Use the dpad to toggle visual collision modes", PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_OUTLINE);
    switch (viewCycle) {
        case 0: print_small_text_light(160, (SCREEN_HEIGHT - 32), "Current view: None",                  PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_OUTLINE); break;
        case 1: print_small_text_light(160, (SCREEN_HEIGHT - 32), "Current view: Hitboxes",              PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_OUTLINE); break;
        case 2: print_small_text_light(160, (SCREEN_HEIGHT - 32), "Current view: Surfaces",              PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_OUTLINE); break;
        case 3: print_small_text_light(160, (SCREEN_HEIGHT - 32), "Current view: Hitboxes and Surfaces", PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_OUTLINE); break;
    }

    hitboxView  = ((viewCycle == 1) || (viewCycle == 3));
    surfaceView = ((viewCycle == 2) || (viewCycle == 3));
#endif
}

extern void print_fps(s32 x, s32 y);

void print_basic_profiling(void) {
    char textBytes[90];
    u32 cpuTime = profiler_get_cpu_microseconds();
    u32 rspTime = profiler_get_rsp_microseconds();
    u32 rdpTime = profiler_get_rdp_microseconds();
    print_fps(16, 40);
    sprintf(textBytes, "CPU: %dus (%d%%)\nRSP: %dus (%d%%)\nRDP: %dus (%d%%)",
            cpuTime, (cpuTime / 333),
            rspTime, (rspTime / 333),
            rdpTime, (rdpTime / 333));
    print_small_text_light(16, 52, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
}

void puppyprint_render_standard(void) {
    char textBytes[128];

    sprintf(textBytes, "Matrix Muls: %d\n\nCollision Checks\nFloors: %d\nWalls: %d\nCeilings: %d\n Water: %d\nRaycasts: %d",
            gPuppyCallCounter.matrix,
            gPuppyCallCounter.collision_floor,
            gPuppyCallCounter.collision_wall,
            gPuppyCallCounter.collision_ceil,
            gPuppyCallCounter.collision_water,
            gPuppyCallCounter.collision_raycast
    );
    print_small_text_light(SCREEN_WIDTH-16, 32, textBytes, PRINT_TEXT_ALIGN_RIGHT, PRINT_ALL, FONT_OUTLINE);
}

void puppyprint_render_minimal(void) {
    print_basic_profiling();
}

void render_coverage_map(void) {
    Gfx *tempGfxHead = gDisplayListHead;

    gDPSetCycleType(tempGfxHead++, G_CYC_1CYCLE);
    gDPSetBlendColor(tempGfxHead++, 0xFF, 0xFF, 0xFF, 0xFF);
    gDPSetPrimDepth(tempGfxHead++, 0xFFFF, 0xFFFF);
    gDPSetDepthSource(tempGfxHead++, G_ZS_PRIM);
    gDPSetRenderMode(tempGfxHead++, G_RM_VISCVG, G_RM_VISCVG2);
    gDPFillRectangle(tempGfxHead++, 0,0, SCREEN_WIDTH-1, SCREEN_HEIGHT-1);

    gDisplayListHead = tempGfxHead;
}

void puppycamera_debug_view(void) {
    char textBytes[80];
    // Very little point printing useless info if Mayro doesn't even exist.
    if (gMarioState->marioObj) {
        sprintf(textBytes, "Mario Pos\nX: %d\nY: %d\nZ: %d\nD: %X\nA: %x",
            (s32)(gMarioState->pos[0]),
            (s32)(gMarioState->pos[1]),
            (s32)(gMarioState->pos[2]),
            (u16)(gMarioState->faceAngle[1]),
            (u32)(gMarioState->action & ACT_ID_MASK));
        print_small_text_light(16, 140, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
    }
    // Same for the camera, especially so because this will crash otherwise.
    if (gCamera) {
        sprintf(textBytes, "Camera Pos\nX: %d\nY: %d\nZ: %d\nD: %X",
            (s32)(gCamera->pos[0]),
            (s32)(gCamera->pos[1]),
            (s32)(gCamera->pos[2]),
            (u16)(gCamera->yaw));
        print_small_text_light((SCREEN_WIDTH - 16), 140, textBytes, PRINT_TEXT_ALIGN_RIGHT, PRINT_ALL, FONT_OUTLINE);
    }
}

#define STUB_LEVEL(textname, _1, _2, _3, _4, _5, _6, _7, _8) textname,
#define DEFINE_LEVEL(textname, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) textname,

static char sLevelNames[][32] = {
    #include "levels/level_defines.h"
};
#undef STUB_LEVEL
#undef DEFINE_LEVEL

static s16 sLevelSelectOption = 0;
static s8 sLevelSelectOptionArea = 0;
u8 gPuppyWarp = 0;
u8 gPuppyWarpArea = 0;

void puppyprint_level_select_menu(void) {
    s32 posY;
    s32 renderedText = 0;
    char textBytes[32];
    prepare_blank_box();
    render_blank_box_rounded((SCREEN_WIDTH/2) - 80, (SCREEN_HEIGHT/2) - 60, (SCREEN_WIDTH/2) + 80, (SCREEN_HEIGHT/2) + 60, 0, 0, 0, 160);
    finish_blank_box();
    print_small_text_light(SCREEN_WIDTH/2, (SCREEN_HEIGHT/2) - 58, "Pick a level", PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_DEFAULT);
    print_small_text_light(SCREEN_WIDTH/2, (SCREEN_HEIGHT/2) + 64, "(Area must have warp node of 0x0A)\nDpad Left/Right: Area / A: Warp\nYellow is current level.", PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_OUTLINE);
    for (u32 i = 0; i < sizeof(sLevelNames) / 32; i++) {
        s32 yOffset = sLevelSelectOption > 8 ? sLevelSelectOption-8 : 0;
        posY = ((renderedText-yOffset) * 10);
        if (sLevelNames[i][0] == 0) {
            continue;
        }
        renderedText++;
        if (posY < 0 || posY > 84) {
            continue;
        }
        if ((u32) sLevelSelectOption == i) {
            sprintf(textBytes, "%s - %d", sLevelNames[i], sLevelSelectOptionArea + 1);
            print_set_envcolour(0xFF, 0x40, 0x40, 0xFF);
        }
        else
        if ((u32) gCurrLevelNum-1 == i) {
            sprintf(textBytes, "%s", sLevelNames[i]);
            print_set_envcolour(0xFF, 0xFF, 0x40, 0xFF);
        }
        else{
            sprintf(textBytes, "%s", sLevelNames[i]);
            print_set_envcolour(0xFF, 0xFF, 0xFF, 0xFF);
        }
        print_small_text_light(SCREEN_WIDTH/2, (SCREEN_HEIGHT/2) - 40 + posY, textBytes, PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_DEFAULT);
    }
}

#ifdef BETTER_REVERB

#define DEBUG_REVERB_PRESET_COUNT ARRAY_COUNT(gDebugBetterReverbSettings)

enum BetterReverbDebugFields {
    PPREVERB_USE_LIGHTWEIGHT,
    PPREVERB_DOWNSAMPLE_RATE,
    PPREVERB_IS_MONO,
    PPREVERB_FILTER_COUNT,
    PPREVERB_WINDOW_SIZE,
    PPREVERB_GAIN,
    PPREVERB_GAIN_INDEX,
    PPREVERB_REVERB_INDEX,
    PPREVERB_DELAYS_L,
    PPREVERB_DELAYS_R,
    PPREVERB_REVERB_MULTS_L,
    PPREVERB_REVERB_MULTS_R,
    PPREVERB_AREA_ECHO,
    PPREVERB_TOTAL,
};

struct BetterReverbDebugSizes {
    u8  treatAsEntryCount;
    u8  bytes;
    s32 entryMinimum;
    s32 entryMaximum;
};

static struct BetterReverbDebugSizes maxReverbEntrySizes[PPREVERB_TOTAL] = {
    {.treatAsEntryCount =  TRUE, .bytes = 1, .entryMinimum =     0, .entryMaximum =           1}, // useLightweightSettings
    {.treatAsEntryCount =  TRUE, .bytes = 1, .entryMinimum =    -1, .entryMaximum =           5}, // downsampleRate (seems to work up to 7 before things are audibly dangerous, but I suspect it's still unsafe)
    {.treatAsEntryCount =  TRUE, .bytes = 1, .entryMinimum =     0, .entryMaximum =           1}, // isMono
    {.treatAsEntryCount =  TRUE, .bytes = 1, .entryMinimum =     3, .entryMaximum = NUM_ALLPASS}, // filterCount
    {.treatAsEntryCount = FALSE, .bytes = 2, .entryMinimum =    -1, .entryMaximum =      0x7FFF}, // windowSize
    {.treatAsEntryCount = FALSE, .bytes = 2, .entryMinimum =    -1, .entryMaximum =      0x7FFF}, // gain
    {.treatAsEntryCount = FALSE, .bytes = 1, .entryMinimum =     0, .entryMaximum =        0xFF}, // gainIndex
    {.treatAsEntryCount = FALSE, .bytes = 1, .entryMinimum =     0, .entryMaximum =        0xFF}, // reverbIndex
    {.treatAsEntryCount =  TRUE, .bytes = 1, .entryMinimum =     0, .entryMaximum =           1}, // delaysL
    {.treatAsEntryCount =  TRUE, .bytes = 1, .entryMinimum =     0, .entryMaximum =           1}, // delaysR
    {.treatAsEntryCount =  TRUE, .bytes = 1, .entryMinimum =     0, .entryMaximum =           1}, // reverbMultsL
    {.treatAsEntryCount =  TRUE, .bytes = 1, .entryMinimum =     0, .entryMaximum =           1}, // reverbMultsR
    {.treatAsEntryCount = FALSE, .bytes = 1, .entryMinimum = -0x80, .entryMaximum =        0x7F}, // area echo (not part of struct)
};

#define REVERB_TOP_PRIO(x) (((x) | (0xFF << SOUNDARGS_SHIFT_PRIORITY)) & ~SOUND_NO_ECHO)
static u32 betterReverbSfx[] = {
    REVERB_TOP_PRIO(SOUND_MARIO_YAH_WAH_HOO),
    REVERB_TOP_PRIO(SOUND_MARIO_YAH_WAH_HOO + (1 << 16)),
    REVERB_TOP_PRIO(SOUND_MARIO_YAH_WAH_HOO + (2 << 16)),
    REVERB_TOP_PRIO(SOUND_MARIO_HOOHOO),
    REVERB_TOP_PRIO(SOUND_MARIO_YAHOO),
    REVERB_TOP_PRIO(SOUND_MARIO_UH),
    REVERB_TOP_PRIO(SOUND_MARIO_HRMM),
    REVERB_TOP_PRIO(SOUND_MARIO_WAH2),
    REVERB_TOP_PRIO(SOUND_MARIO_WHOA),
    REVERB_TOP_PRIO(SOUND_MARIO_EEUH),
    REVERB_TOP_PRIO(SOUND_MARIO_ATTACKED),
    REVERB_TOP_PRIO(SOUND_MARIO_OOOF),
    REVERB_TOP_PRIO(SOUND_MARIO_HERE_WE_GO),
    REVERB_TOP_PRIO(SOUND_MARIO_YAWNING),
    REVERB_TOP_PRIO(SOUND_MARIO_WAAAOOOW),
    REVERB_TOP_PRIO(SOUND_MARIO_HAHA),
    REVERB_TOP_PRIO(SOUND_MARIO_UH_LEDGE_CLIMB_FAST),
    REVERB_TOP_PRIO(SOUND_MARIO_ON_FIRE),
    REVERB_TOP_PRIO(SOUND_MARIO_DYING),
    REVERB_TOP_PRIO(SOUND_MARIO_PANTING_COLD),
    REVERB_TOP_PRIO(SOUND_MARIO_PANTING),
    REVERB_TOP_PRIO(SOUND_MARIO_MAMA_MIA),
    REVERB_TOP_PRIO(SOUND_MARIO_DROWNING),
    REVERB_TOP_PRIO(SOUND_MARIO_YAHOO_WAHA_YIPPEE),
    REVERB_TOP_PRIO(SOUND_MARIO_YAHOO_WAHA_YIPPEE + (3 << 16)),
    REVERB_TOP_PRIO(SOUND_MARIO_YAHOO_WAHA_YIPPEE + (4 << 16)),
    REVERB_TOP_PRIO(SOUND_MARIO_DOH),
    REVERB_TOP_PRIO(SOUND_MARIO_GAME_OVER),
    REVERB_TOP_PRIO(SOUND_MARIO_HELLO),
    REVERB_TOP_PRIO(SOUND_MARIO_PRESS_START_TO_PLAY),
    REVERB_TOP_PRIO(SOUND_MARIO_TWIRL_BOUNCE),
    REVERB_TOP_PRIO(SOUND_MARIO_SO_LONGA_BOWSER),
    REVERB_TOP_PRIO(SOUND_MARIO_IMA_TIRED),
};
#undef REVERB_TOP_PRIO

static s8 ppAreaEchos[DEBUG_REVERB_PRESET_COUNT] = {0};

static s32 ppReverbSide = 0;
static s32 ppReverbSelectY = 0;

s32 better_reverb_get_or_set_field(u8 shouldSet, s32 xIndex, s32 yIndex, s32 valueToSet) {
    s32 ret = 0;

    if (shouldSet) {
        if (valueToSet < maxReverbEntrySizes[yIndex].entryMinimum) {
            valueToSet = maxReverbEntrySizes[yIndex].entryMinimum;
        } else if (valueToSet > maxReverbEntrySizes[yIndex].entryMaximum) {
            valueToSet = maxReverbEntrySizes[yIndex].entryMaximum;
        }

        switch(yIndex) {
            case PPREVERB_USE_LIGHTWEIGHT:
                gDebugBetterReverbSettings[xIndex].useLightweightSettings = valueToSet;
                break;
            case PPREVERB_DOWNSAMPLE_RATE:
                gDebugBetterReverbSettings[xIndex].downsampleRate = valueToSet;
                break;
            case PPREVERB_IS_MONO:
                gDebugBetterReverbSettings[xIndex].isMono = valueToSet;
                break;
            case PPREVERB_FILTER_COUNT:
                gDebugBetterReverbSettings[xIndex].filterCount = valueToSet;
                break;
            case PPREVERB_WINDOW_SIZE:
                gDebugBetterReverbSettings[xIndex].windowSize = valueToSet;
                break;
            case PPREVERB_GAIN:
                gDebugBetterReverbSettings[xIndex].gain = valueToSet;
                break;
            case PPREVERB_GAIN_INDEX:
                gDebugBetterReverbSettings[xIndex].gainIndex = valueToSet;
                break;
            case PPREVERB_REVERB_INDEX:
                gDebugBetterReverbSettings[xIndex].reverbIndex = valueToSet;
                break;
            case PPREVERB_DELAYS_L:
                gDebugBetterReverbSettings[xIndex].delaysL = sReverbDelaysArr[valueToSet];
                break;
            case PPREVERB_DELAYS_R:
                gDebugBetterReverbSettings[xIndex].delaysR = sReverbDelaysArr[valueToSet];
                break;
            case PPREVERB_REVERB_MULTS_L:
                gDebugBetterReverbSettings[xIndex].reverbMultsL = sReverbMultsArr[valueToSet];
                break;
            case PPREVERB_REVERB_MULTS_R:
                gDebugBetterReverbSettings[xIndex].reverbMultsR = sReverbMultsArr[valueToSet];
                break;
            case PPREVERB_AREA_ECHO:
                if (gCurrentArea != NULL && !gCurrentArea->useEchoOverride) {
                    u8 level = gCurrLevelNum;
                    u8 area = gCurrAreaIndex - 1;
                    if (level > LEVEL_MAX) {
                        level = LEVEL_MAX;
                    }
                    if (area > 2) {
                        area = 2;
                    }

                    gCurrentArea->echoOverride = sLevelAreaReverbs[level][area];
                    gCurrentArea->useEchoOverride = TRUE;

                    for (u32 i = 0; i < DEBUG_REVERB_PRESET_COUNT; i++) {
                        ppAreaEchos[i] = gCurrentArea->echoOverride;
                    }
                }
                ppAreaEchos[xIndex] = valueToSet;
                break;

            ret = valueToSet;
        }
    } else {
        switch(yIndex) {
            case PPREVERB_USE_LIGHTWEIGHT:
                ret = gDebugBetterReverbSettings[xIndex].useLightweightSettings;
                break;
            case PPREVERB_DOWNSAMPLE_RATE:
                ret = gDebugBetterReverbSettings[xIndex].downsampleRate;
                break;
            case PPREVERB_IS_MONO:
                ret = gDebugBetterReverbSettings[xIndex].isMono;
                break;
            case PPREVERB_FILTER_COUNT:
                ret = gDebugBetterReverbSettings[xIndex].filterCount;
                break;
            case PPREVERB_WINDOW_SIZE:
                ret = gDebugBetterReverbSettings[xIndex].windowSize;
                break;
            case PPREVERB_GAIN:
                ret = gDebugBetterReverbSettings[xIndex].gain;
                break;
            case PPREVERB_GAIN_INDEX:
                ret = gDebugBetterReverbSettings[xIndex].gainIndex;
                break;
            case PPREVERB_REVERB_INDEX:
                ret = gDebugBetterReverbSettings[xIndex].reverbIndex;
                break;
            case PPREVERB_DELAYS_L:
                ret = (u32) ((u8*) gDebugBetterReverbSettings[xIndex].delaysL - (u8*) &sReverbDelaysArr[0][0]) / sizeof(sReverbDelaysArr[0]);
                break;
            case PPREVERB_DELAYS_R:
                ret = (u32) ((u8*) gDebugBetterReverbSettings[xIndex].delaysR - (u8*) &sReverbDelaysArr[0][0]) / sizeof(sReverbDelaysArr[0]);
                break;
            case PPREVERB_REVERB_MULTS_L:
                ret = (u32) ((u8*) gDebugBetterReverbSettings[xIndex].reverbMultsL - (u8*) &sReverbMultsArr[0][0]) / sizeof(sReverbMultsArr[0]);
                break;
            case PPREVERB_REVERB_MULTS_R:
                ret = (u32) ((u8*) gDebugBetterReverbSettings[xIndex].reverbMultsR - (u8*) &sReverbMultsArr[0][0]) / sizeof(sReverbMultsArr[0]);
                break;
            case PPREVERB_AREA_ECHO:
                if (gCurrentArea != NULL && !gCurrentArea->useEchoOverride) {
                    u8 level = gCurrLevelNum;
                    u8 area = gCurrAreaIndex - 1;
                    if (level > LEVEL_MAX) {
                        level = LEVEL_MAX;
                    }
                    if (area > 2) {
                        area = 2;
                    }

                    gCurrentArea->echoOverride = sLevelAreaReverbs[level][area];
                    gCurrentArea->useEchoOverride = TRUE;

                    for (u32 i = 0; i < DEBUG_REVERB_PRESET_COUNT; i++) {
                        ppAreaEchos[i] = gCurrentArea->echoOverride;
                    }
                }
                ret = ppAreaEchos[xIndex];
                break;
        }

        if (ret < maxReverbEntrySizes[yIndex].entryMinimum) {
            ret = maxReverbEntrySizes[yIndex].entryMinimum;
        } else if (ret > maxReverbEntrySizes[yIndex].entryMaximum) {
            ret = maxReverbEntrySizes[yIndex].entryMaximum;
        }
    }

    return ret;
}

void better_reverb_preset_menu(void) {
    char textBytes[16];
    s32 printValue;
    s32 x;
    s32 y;

    prepare_blank_box();
    render_blank_box_rounded(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, 0, 0, 0, 127);
    finish_blank_box();

    print_set_envcolour(63, 159, 127, 255),
    print_small_text(SCREEN_WIDTH/2, SCREEN_HEIGHT - 48, "<COL_3FFF9FFF>D-Pad Up/Down:<COL_--------> Scroll, <COL_3FFF9FFF>Start:<COL_--------> Switch Preset A/B", PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_OUTLINE);
    print_set_envcolour(63, 159, 127, 255),
    print_small_text(SCREEN_WIDTH/2, SCREEN_HEIGHT - 36, "<COL_3FFF9FFF>D-Pad L/R:<COL_--------> Fine Adjustment, <COL_3FFF9FFF>Z/R:<COL_--------> Coarse Adjustment", PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_OUTLINE);
    print_set_envcolour(63, 159, 127, 255),
    print_small_text(SCREEN_WIDTH/2, SCREEN_HEIGHT - 24, "<COL_3FFF9FFF>A:<COL_--------> Apply Selected Preset, <COL_3FFF9FFF>B:<COL_--------> Preview SFX", PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_OUTLINE);

    x = 32;
    y = 16;
    for (s32 j = 0; j < PPREVERB_TOTAL; j++) {
        y += 12;
        if (j == ppReverbSelectY) {
            print_set_envcolour(255, 255, 159, 255);
        } else {
            print_set_envcolour(191, 159, 191, 255);
        }
        switch(j) {
            case PPREVERB_USE_LIGHTWEIGHT:
                print_small_text_light(x, y, "Lightweight:", PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
                break;
            case PPREVERB_DOWNSAMPLE_RATE:
                print_small_text_light(x, y, "Downsample Rate:", PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
                break;
            case PPREVERB_IS_MONO:
                print_small_text_light(x, y, "Mono:", PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
                break;
            case PPREVERB_FILTER_COUNT:
                print_small_text_light(x, y, "Filter Count:", PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
                break;
            case PPREVERB_WINDOW_SIZE:
                print_small_text_light(x, y, "Window Size:", PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
                break;
            case PPREVERB_GAIN:
                print_small_text_light(x, y, "Gain:", PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
                break;
            case PPREVERB_GAIN_INDEX:
                print_small_text_light(x, y, "Gain Index:", PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
                break;
            case PPREVERB_REVERB_INDEX:
                print_small_text_light(x, y, "Reverb Index:", PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
                break;
            case PPREVERB_DELAYS_L:
                print_small_text_light(x, y, "Delays L:", PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
                break;
            case PPREVERB_DELAYS_R:
                print_small_text_light(x, y, "Delays R:", PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
                break;
            case PPREVERB_REVERB_MULTS_L:
                print_small_text_light(x, y, "Reverb Mults L:", PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
                break;
            case PPREVERB_REVERB_MULTS_R:
                print_small_text_light(x, y, "Reverb Mults R:", PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
                break;
            case PPREVERB_AREA_ECHO:
                print_small_text_light(x, y, "Area Echo:", PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
                break;
        }
    }

    for (s32 i = 0; i < DEBUG_REVERB_PRESET_COUNT; i++) {
        s32 windowSize = better_reverb_get_or_set_field(FALSE, i, PPREVERB_WINDOW_SIZE, 0);
        s32 downsampleRate = better_reverb_get_or_set_field(FALSE, i, PPREVERB_DOWNSAMPLE_RATE, 0);
        s32 isLightweight = better_reverb_get_or_set_field(FALSE, i, PPREVERB_USE_LIGHTWEIGHT, 0);
        x = (SCREEN_WIDTH/2) + (i * 84);
        y = 12;
        if (i == ppReverbSide) {
            print_set_envcolour(255, 255, 159, 255);
        } else {
            print_set_envcolour(191, 159, 191, 255);
        }
        sprintf(textBytes, "PRESET %c", 'A' + i);
        print_small_text_light(x, y, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
        y += 4;
        for (s32 j = 0; j < PPREVERB_TOTAL; j++) {
            y += 12;
            printValue = better_reverb_get_or_set_field(FALSE, i, j, 0);
            if (i == ppReverbSide && j == ppReverbSelectY) {
                print_set_envcolour(255, 159, 0, 255);
            } else if (windowSize == 0) { // All reverb disabled
                if (j == PPREVERB_WINDOW_SIZE) {
                    print_set_envcolour(255, 255, 255, 255);
                } else {
                    print_set_envcolour(95, 95, 95, 255);
                }
            } else if (downsampleRate <= 0) { // Vanilla reverb
                switch(j) {
                    case PPREVERB_DOWNSAMPLE_RATE:
                    case PPREVERB_WINDOW_SIZE:
                    case PPREVERB_GAIN:
                    case PPREVERB_AREA_ECHO:
                        print_set_envcolour(255, 255, 255, 255);
                        break;
                    default:
                        print_set_envcolour(95, 95, 95, 255);
                        break;
                }
            }
            else if (isLightweight) { // Lightweight reverb
                switch(j) {
                    case PPREVERB_FILTER_COUNT:
                    case PPREVERB_GAIN_INDEX:
                    case PPREVERB_REVERB_INDEX:
                    case PPREVERB_REVERB_MULTS_L:
                    case PPREVERB_REVERB_MULTS_R:
                        print_set_envcolour(95, 95, 95, 255);
                        break;
                    default:
                        print_set_envcolour(255, 255, 255, 255);
                        break;

                }
            } else { // Standard reverb
                print_set_envcolour(255, 255, 255, 255);
            }

            if (maxReverbEntrySizes[j].bytes == 2) {
                sprintf(textBytes, "0x%04X", (u16) printValue);
            } else if (!maxReverbEntrySizes[j].treatAsEntryCount) {
                sprintf(textBytes, "0x%02X", (u8) printValue);
            } else if (j == PPREVERB_USE_LIGHTWEIGHT || j == PPREVERB_IS_MONO) {
                if (printValue == 0) {
                    sprintf(textBytes, "FALSE");
                } else {
                    sprintf(textBytes, "TRUE");
                }
            } else {
                sprintf(textBytes, "%d", printValue);
            }

            print_small_text_light(x, y, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
        }
    }
}
#endif

u8 gLastWarpID = 0;

void puppyprint_render_general_vars(void) {
    
    char textBytes[200];
    u32 floorType = 0;
    u32 objParams = 0;
    if (gMarioState->floor) {
        floorType = gMarioState->floor->type;
    }
    if (gMarioState->interactObj) {
        objParams = gMarioState->interactObj->oBehParams;
    }


    sprintf(textBytes, "World\n\nObjects: %d/%d\n\nLevel ID: %d\nCourse ID: %d\nArea ID: %d\nRoom ID: %d\n\nInteract:   \n0x%08X\nWarp: 0x%02X", 
            gObjectCounter, 
            OBJECT_POOL_CAPACITY,
            gCurrLevelNum,
            gCurrCourseNum,
            gCurrAreaIndex,
            gMarioCurrentRoom,
            objParams,
            gLastWarpID
    );
    print_small_text_light(SCREEN_WIDTH - 16, 36, textBytes, PRINT_TEXT_ALIGN_RIGHT, PRINT_ALL, FONT_OUTLINE);

#ifndef ENABLE_CREDITS_BENCHMARK
    // Very little point printing useless info if Mario doesn't even exist.
    if (gMarioState->marioObj) {
        sprintf(textBytes, "Mario\n\nX: %d\nY: %d\nZ: %d\nYaw: 0x%04X\n\nfVel: %1.1f\nyVel: %1.1f\n\nHealth: %03X\nAction: 0x%02X\nFloor Type: 0x%02X\nWater Height: %d",
            (s32)(gMarioState->pos[0]),
            (s32)(gMarioState->pos[1]),
            (s32)(gMarioState->pos[2]),
            (u16)(gMarioState->faceAngle[1]),
            (f32)(gMarioState->forwardVel),
            (f32)(gMarioState->vel[1]),
            (s32)(gMarioState->health),
            (u32)(gMarioState->action & ACT_ID_MASK),
            (u32)(floorType),
            (s32)(gMarioState->waterLevel)
            );
        print_small_text_light(16, 36, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
        sprintf(textBytes, "Gfx Pool: %d / %d", ((u32)gDisplayListHead - ((u32)gGfxPool->buffer)) / 4, GFX_POOL_SIZE);
        print_small_text_light(SCREEN_WIDTH/2, SCREEN_HEIGHT-16, textBytes, PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_OUTLINE);
    }
#endif
}

struct PuppyPrintPage ppPages[] = {
#ifdef USE_PROFILER
    [PUPPYPRINT_PAGE_PROFILER]      = {&puppyprint_render_standard,     "Profiler"},
    [PUPPYPRINT_PAGE_MINIMAL]       = {&puppyprint_render_minimal,      "Minimal"},
#endif
    [PUPPYPRINT_PAGE_GENERAL]       = {&puppyprint_render_general_vars, "General"},
    [PUPPYPRINT_PAGE_AUDIO]         = {&print_audio_overview,           "Audio"},
    [PUPPYPRINT_PAGE_RAM]           = {&print_ram_overview,             "Segments"},
    [PUPPYPRINT_PAGE_COLLISION]     = {&puppyprint_render_collision,    "Collision"},
    [PUPPYPRINT_PAGE_LOG]           = {&print_console_log,              "Log"},
    [PUPPYPRINT_PAGE_LEVEL_SELECT]  = {&puppyprint_level_select_menu,   "Level Select"},
    [PUPPYPRINT_PAGE_COVERAGE]      = {&render_coverage_map,            "Coverage"},
#ifdef PUPPYCAM
    [PUPPYPRINT_PAGE_CAMERA]        = {&puppycamera_debug_view,         "Unlock Camera"},
#endif
#ifdef BETTER_REVERB
    [PUPPYPRINT_PAGE_BETTER_REVERB] = {&better_reverb_preset_menu,      "Reverb Config"},
#endif
};

#define MENU_BOX_WIDTH 128
#define MAX_DEBUG_OPTIONS (sizeof(ppPages) / sizeof(struct PuppyPrintPage))

void render_page_menu(void) {
    s32 i;
    s32 posY;
    s32 scrollY = (36 / (MAX_DEBUG_OPTIONS - 1));

    prepare_blank_box();
    render_blank_box_rounded(32, 32, (32 + MENU_BOX_WIDTH), (32 + 72), 0x00, 0x00, 0x00, 0xC0);
    render_blank_box_rounded(((32 + MENU_BOX_WIDTH) - 8), (32 + (scrollY * sDebugOption)), (32 + MENU_BOX_WIDTH), (32 + (scrollY * sDebugOption) + 36), 0xFF, 0xFF, 0xFF, 0xFF);
    finish_blank_box();

    for (i = 0; i < (s32)MAX_DEBUG_OPTIONS; i++) {
        s32 yOffset = ((sDebugOption > 5) ? (sDebugOption - 5) : 0);
        posY = (38 + ((i - yOffset) * 10));
        if ((posY > 32) && (posY < 90)) {
            if (sDebugOption == i) {
                print_set_envcolour(0xFF, 0x40, 0x40, 0xFF);
            } else {
                print_set_envcolour(0xFF, 0xFF, 0xFF, 0xFF);
            }

            print_small_text_light((28 + (MENU_BOX_WIDTH / 2)), posY, ppPages[i].name, PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_DEFAULT);
        }
    }
}

void puppyprint_render_profiler(void) {
    PUPPYPRINT_GET_SNAPSHOT();

    bzero(&gCurrEnvCol, sizeof(ColorRGBA));
    print_set_envcolour(255, 255, 255, 255);

    if (!fDebug) {
        profiler_update(PROFILER_TIME_PUPPYPRINT1, osGetCount() - first);
        return;
    }
    if (ppPages[sPPDebugPage].func != NULL) {
        (ppPages[sPPDebugPage].func)();
    }

    if (sDebugMenu) {
        render_page_menu();
    }
    profiler_update(PROFILER_TIME_PUPPYPRINT1, osGetCount() - first);
}

void puppyprint_profiler_process(void) {
    PUPPYPRINT_GET_SNAPSHOT();

    if (fDebug && (gPlayer1Controller->buttonPressed & L_TRIG)) {
        sDebugMenu ^= TRUE;
        if (sDebugMenu == FALSE) {
            sPPDebugPage = sDebugOption;
        }
    }

    if ((gPlayer1Controller->buttonPressed & (L_TRIG | U_JPAD))
        && (gPlayer1Controller->buttonDown & L_TRIG)
        && (gPlayer1Controller->buttonDown & U_JPAD)
    ) {
        fDebug    ^= TRUE;
        sDebugMenu = FALSE;
    }

    if (sDebugMenu) {
        if (gPlayer1Controller->buttonPressed & U_JPAD) sDebugOption--;
        if (gPlayer1Controller->buttonPressed & D_JPAD) sDebugOption++;

        if (sDebugOption == 255) {
            sDebugOption = ((sizeof(ppPages) / sizeof(struct PuppyPrintPage)) - 1);
        }

        if (sDebugOption >= (sizeof(ppPages) / sizeof(struct PuppyPrintPage))) {
            sDebugOption = 0;
        }
    } else {
        if (sPPDebugPage == PUPPYPRINT_PAGE_LEVEL_SELECT)
        {
            if (gPlayer1Controller->buttonPressed & U_JPAD) {
                sLevelSelectOption--;
                // If there is no level entry to this ID, skip over.
                while (sLevelNames[sLevelSelectOption][0] == 0 && sLevelSelectOption < LEVEL_COUNT) {
                    sLevelSelectOption--;
                }
                if (sLevelSelectOption <= 0) {
                    sLevelSelectOption = LEVEL_COUNT - 2;
                    // If there is no level entry to this ID, skip over.
                    while (sLevelNames[sLevelSelectOption][0] == 0 && sLevelSelectOption < LEVEL_COUNT) {
                        sLevelSelectOption--;
                    }
                }
            }
            if (gPlayer1Controller->buttonPressed & D_JPAD) {
                sLevelSelectOption = (sLevelSelectOption + 1) % (LEVEL_COUNT - 2);
                // If there is no level entry to this ID, skip over.
                while (sLevelNames[sLevelSelectOption][0] == 0 && sLevelSelectOption < LEVEL_COUNT) {
                    sLevelSelectOption = (sLevelSelectOption + 1) % (LEVEL_COUNT - 2);
                }
            }
            if (gPlayer1Controller->buttonPressed & R_JPAD) {
                sLevelSelectOptionArea++;
                if (sLevelSelectOptionArea > AREA_COUNT - 1) {
                    sLevelSelectOptionArea = 0;
                }
            } else if (gPlayer1Controller->buttonPressed & L_JPAD) {
                sLevelSelectOptionArea--;
                if (sLevelSelectOptionArea < 0) {
                    sLevelSelectOptionArea = AREA_COUNT - 1;
                }
            }
            if (gPlayer1Controller->buttonPressed & A_BUTTON) {
                sPPDebugPage = 0;
                gPuppyWarp = sLevelSelectOption + 1;
                gPuppyWarpArea = sLevelSelectOptionArea + 1;
            }
        } else {
            if (gCurrLevelNum > 3) {
                sLevelSelectOption = gCurrLevelNum;
            } else {
                sLevelSelectOption = LEVEL_CASTLE_GROUNDS;
            }
        }
        // Collision toggles.
#ifdef VISUAL_DEBUG
        if (sPPDebugPage == PUPPYPRINT_PAGE_COLLISION)
        {
            if (gPlayer1Controller->buttonPressed & R_JPAD)
                viewCycle++;
            if (gPlayer1Controller->buttonPressed & L_JPAD)
                viewCycle--;
            if (viewCycle == 4)
                viewCycle = 0;
            if (viewCycle == 255)
                viewCycle = 3;
        }
#endif
        if (sPPDebugPage == PUPPYPRINT_PAGE_RAM) {
            if (gPlayer1Controller->buttonDown & U_JPAD && gPPSegScroll > 0)  {
                gPPSegScroll -= 4;
            } else if (gPlayer1Controller->buttonDown & D_JPAD && gPPSegScroll < (12 * 32)){
                gPPSegScroll += 4;
            }
        }
#ifdef BETTER_REVERB
        if (sPPDebugPage == PUPPYPRINT_PAGE_BETTER_REVERB)
        {
            s32 grabbedValue = better_reverb_get_or_set_field(FALSE, ppReverbSide, ppReverbSelectY, 0);

            // These only need to be done once, but not really worth optimizing tbh
            maxReverbEntrySizes[PPREVERB_DELAYS_L].entryMaximum = gReverbDelaysArrCount - 1;
            maxReverbEntrySizes[PPREVERB_DELAYS_R].entryMaximum = gReverbDelaysArrCount - 1;
            maxReverbEntrySizes[PPREVERB_REVERB_MULTS_L].entryMaximum = gReverbMultsArrCount - 1;
            maxReverbEntrySizes[PPREVERB_REVERB_MULTS_R].entryMaximum = gReverbMultsArrCount - 1;

            if (gPlayer1Controller->buttonPressed & (A_BUTTON | B_BUTTON)) {
                u8 betterReverbLastPreset = gBetterReverbPresetValue;

                if (gCurrentArea != NULL) {
                    gCurrentArea->useEchoOverride = TRUE;
                    gBetterReverbPresetValue = (u8)(s8) (ppReverbSide - DEBUG_REVERB_PRESET_COUNT);
                    gCurrentArea->echoOverride = ppAreaEchos[ppReverbSide];
                }

                if (sAudioIsInitialized) {
                    if (gAudioLoadLock != AUDIO_LOCK_UNINITIALIZED) {
                        gAudioLoadLock = AUDIO_LOCK_LOADING;

                        init_reverb_us(0);

                        if (gAudioLoadLock != AUDIO_LOCK_UNINITIALIZED) {
                            gAudioLoadLock = AUDIO_LOCK_NOT_LOADING;
                        }
                    }
                }

                gBetterReverbPresetValue = betterReverbLastPreset;

                if (gPlayer1Controller->buttonPressed & B_BUTTON) {
                    u32 sound = random_u16() % ARRAY_COUNT(betterReverbSfx);
                    play_sound(betterReverbSfx[sound], gGlobalSoundSource);
                } else { // A_BUTTON
                    sPPDebugPage = 0;
                }
            } else if (gPlayer1Controller->buttonPressed & L_JPAD) {
                if (ppReverbSelectY == PPREVERB_FILTER_COUNT) {
                    grabbedValue -= 3;
                } else if ((ppReverbSelectY == PPREVERB_WINDOW_SIZE || ppReverbSelectY == PPREVERB_GAIN) && grabbedValue > 0 && grabbedValue <= 0x80) {
                    grabbedValue = 0;
                } else {
                    if (maxReverbEntrySizes[ppReverbSelectY].bytes == 2) {
                        grabbedValue -= 0x80;
                    } else {
                        grabbedValue -= 0x1;
                    }
                }

                better_reverb_get_or_set_field(TRUE, ppReverbSide, ppReverbSelectY, grabbedValue);
            } else if (gPlayer1Controller->buttonPressed & R_JPAD) {
                if (ppReverbSelectY == PPREVERB_FILTER_COUNT) {
                    grabbedValue += 3;
                } else if ((ppReverbSelectY == PPREVERB_WINDOW_SIZE || ppReverbSelectY == PPREVERB_GAIN) && grabbedValue < 0) {
                    grabbedValue = 0;
                }  else {
                    if (maxReverbEntrySizes[ppReverbSelectY].bytes == 2) {
                        grabbedValue += 0x80;
                    } else {
                        grabbedValue += 0x1;
                    }
                }

                better_reverb_get_or_set_field(TRUE, ppReverbSide, ppReverbSelectY, grabbedValue);
            } else if (gPlayer1Controller->buttonPressed & Z_TRIG) {
                if (ppReverbSelectY == PPREVERB_FILTER_COUNT) {
                    grabbedValue -= 3;
                } else if ((ppReverbSelectY == PPREVERB_WINDOW_SIZE || ppReverbSelectY == PPREVERB_GAIN) && grabbedValue > 0 && grabbedValue <= 0x800) {
                    grabbedValue = 0;
                }  else {
                    if (maxReverbEntrySizes[ppReverbSelectY].bytes == 2) {
                        grabbedValue -= 0x800;
                    } else if (!maxReverbEntrySizes[ppReverbSelectY].treatAsEntryCount) {
                        grabbedValue -= 0x10;
                    } else {
                        grabbedValue -= 0x1;
                    }
                }

                better_reverb_get_or_set_field(TRUE, ppReverbSide, ppReverbSelectY, grabbedValue);
            } else if (gPlayer1Controller->buttonPressed & R_TRIG) {
                if (ppReverbSelectY == PPREVERB_FILTER_COUNT) {
                    grabbedValue += 3;
                } else if ((ppReverbSelectY == PPREVERB_WINDOW_SIZE || ppReverbSelectY == PPREVERB_GAIN) && grabbedValue < 0) {
                    grabbedValue = 0;
                }  else {
                    if (maxReverbEntrySizes[ppReverbSelectY].bytes == 2) {
                        grabbedValue += 0x800;
                    } else if (!maxReverbEntrySizes[ppReverbSelectY].treatAsEntryCount) {
                        grabbedValue += 0x10;
                    } else {
                        grabbedValue += 0x1;
                    }
                }

                better_reverb_get_or_set_field(TRUE, ppReverbSide, ppReverbSelectY, grabbedValue);
            } else if (gPlayer1Controller->buttonPressed & D_JPAD) {
                ppReverbSelectY = (ppReverbSelectY + 1) % PPREVERB_TOTAL;
            } else if (gPlayer1Controller->buttonPressed & U_JPAD) {
                ppReverbSelectY = (ppReverbSelectY + PPREVERB_TOTAL - 1) % PPREVERB_TOTAL;
            } else if (gPlayer1Controller->buttonPressed & START_BUTTON) {
                ppReverbSide = (ppReverbSide + 1) % DEBUG_REVERB_PRESET_COUNT;
            }
        }
#endif
    }
    profiler_update(PROFILER_TIME_PUPPYPRINT2, osGetCount() - first);
}
#endif

s32 print_set_envcolour(u8 r, u8 g, u8 b, u8 a) {
    if ((r != gCurrEnvCol[0])
        || (g != gCurrEnvCol[1])
        || (b != gCurrEnvCol[2])
        || (a != gCurrEnvCol[3])) {
        gDPSetEnvColor(gDisplayListHead++, (Color)r, (Color)g, (Color)b, (Color)a);
        vec4_set(gCurrEnvCol, r, g, b, a);
        
        return TRUE;
    }

    return FALSE;
}

#define BLANK 0, 0, 0, ENVIRONMENT, 0, 0, 0, ENVIRONMENT

void prepare_blank_box(void) {
    gDPSetCombineMode(gDisplayListHead++, BLANK, BLANK);
}

void finish_blank_box(void) {
    gDPPipeSync(gDisplayListHead++);
    print_set_envcolour(255, 255, 255, 255);
    gSPDisplayList(gDisplayListHead++, dl_hud_img_end);
}

// This does some epic shenanigans to figure out the optimal way to draw this.
// If the width is a multiple of 4, then use fillmode (fastest)
// Otherwise, if there's transparency, it uses that rendermode, which is slower than using opaque rendermodes.
void render_blank_box(s32 x1, s32 y1, s32 x2, s32 y2, u8 r, u8 g, u8 b, u8 a) {
    if (x2 < x1)
    {
        u32 temp = x2;
        x2 = x1;
        x1 = temp;
    }
    if (y2 < y1)
    {
        u32 temp = y2;
        y2 = y1;
        y1 = temp;
    }
    if (x1 < 0) x1 = 0;
    if (y1 < 0) y1 = 0;
    if (x2 > SCREEN_WIDTH) x2 = SCREEN_WIDTH;
    if (y2 > SCREEN_HEIGHT) y2 = SCREEN_HEIGHT;
    s32 cycleadd = 0;
    gDPPipeSync(gDisplayListHead++);
    if (((absi(x1 - x2) % 4) == 0) && (a == 255)) {
        gDPSetCycleType(gDisplayListHead++, G_CYC_FILL);
        gDPSetRenderMode(gDisplayListHead++, G_RM_NOOP, G_RM_NOOP);
        cycleadd = 1;
    } else {
        gDPSetCycleType(gDisplayListHead++, G_CYC_1CYCLE);
        if (a == 255) {
            gDPSetRenderMode(gDisplayListHead++, G_RM_OPA_SURF, G_RM_OPA_SURF2);
        } else {
            gDPSetRenderMode(gDisplayListHead++, G_RM_XLU_SURF, G_RM_XLU_SURF2);
        }
        cycleadd = 0;
    }

    gDPSetFillColor(gDisplayListHead++, (GPACK_RGBA5551(r, g, b, 1) << 16) | GPACK_RGBA5551(r, g, b, 1));
    print_set_envcolour(r, g, b, a);
    gDPFillRectangle(gDisplayListHead++, x1, y1, x2 - cycleadd, y2 - cycleadd);
    gDPPipeSync(gDisplayListHead++);
}

// Same as above, but with rounded edges.
// Follows all the same rules of usage.
void render_blank_box_rounded(s32 x1, s32 y1, s32 x2, s32 y2, u8 r, u8 g, u8 b, u8 a) {
    if (x2 < x1)
    {
        u32 temp = x2;
        x2 = x1;
        x1 = temp;
    }
    if (y2 < y1)
    {
        u32 temp = y2;
        y2 = y1;
        y1 = temp;
    }
    if (x1 < 0) x1 = 0;
    if (y1 < 0) y1 = 0;
    if (x2 > SCREEN_WIDTH) x2 = SCREEN_WIDTH;
    if (y2 > SCREEN_HEIGHT) y2 = SCREEN_HEIGHT;
    s32 cycleadd = 0;
    gDPSetCycleType(gDisplayListHead++, G_CYC_1CYCLE);
    if (a == 255) {
        gDPSetRenderMode(gDisplayListHead++, G_RM_OPA_SURF, G_RM_OPA_SURF2);
    } else {
        gDPSetRenderMode(gDisplayListHead++, G_RM_XLU_SURF, G_RM_XLU_SURF2);
    }
    gDPSetFillColor(gDisplayListHead++, GPACK_RGBA5551(r, g, b, 1) << 16 | GPACK_RGBA5551(r, g, b, 1));
    print_set_envcolour(r, g, b, a);
    gDPFillRectangle(gDisplayListHead++, x1+4, y1, x2-4, y1+1);
    gDPFillRectangle(gDisplayListHead++, x1+2, y1+1, x2-2, y1+2);
    gDPFillRectangle(gDisplayListHead++, x1+1, y1+2, x2-1, y1+4);
    gDPFillRectangle(gDisplayListHead++, x1+1, y2-4, x2-1, y2-2);
    gDPFillRectangle(gDisplayListHead++, x1+2, y2-2, x2-2, y2-1);
    gDPFillRectangle(gDisplayListHead++, x1+4, y2-1, x2-4, y2);
    if (ABS(x1 - x2) % 4 == 0 && a == 255) {
        gDPSetCycleType(gDisplayListHead++, G_CYC_FILL);
        gDPSetRenderMode(gDisplayListHead++, G_RM_NOOP, G_RM_NOOP);
        cycleadd = 1;
    }
    gDPFillRectangle(gDisplayListHead++, x1, y1+4, x2 - cycleadd, y2-4 - cycleadd);
    gDPPipeSync(gDisplayListHead++);
}

s8 shakeToggle = 0;
s8 waveToggle = 0;
s8 rainbowToggle = 0;
f32 textSize = 1.0f; // The value that's used as a baseline multiplier before applying text size modifiers. Make sure to set it back when you're done.
f32 textSizeTotal = 1.0f; // The value that's read to set the text size. Do not mess with this.
f32 textSizeTemp = 1.0f; // The value that's set when modifying text size mid draw. Also do not mess with this.
u16 textTempScale = 1024; // A fixed point means of referring to scale.
u8 textOffsets[2]; // Represents the dimensions of the text (12 x 8), and written to when size is modified.
u8 topLineHeight; // Represents the peak line height of the current line. Prevents vertical overlapping.
u8 gMonoSpace = FALSE; // Ignore kerning.

s32 get_text_width(const char *str, s32 font) {
    s32 i       = 0;
    s32 textPos = 0;
    s32 wideX   = 0;
    s32 textX;
    s8 offsetY;
    u8 spaceX;
    s32 strLen = (signed)strlen(str);
    s32 commandOffset;
    u8 pad;

    textSizeTemp = 1.0f;
    textSizeTotal = textSizeTemp * textSize;

    for (i = 0; i < strLen; i++) {
        while (i < strLen && str[i] == '<') {
            commandOffset = text_iterate_command(str, i, FALSE);
            if (commandOffset == 0)
                break;

            i += commandOffset;
            if (str[i] == '\n') {
                textPos = 0;
                break;
            }
        }

        if (i >= strLen)
            break;

        get_char_from_byte(&textX, &textPos, str[i], &pad, &spaceX, &offsetY, font);
        textPos += (spaceX + 1) * textSizeTotal;
        wideX = MAX(textPos, wideX);
    }
    return wideX;
}

s32 get_text_height(const char *str) {
    s32 i= 0;
    s32 textPos;
    s32 strLen = (signed)strlen(str);
    s32 commandOffset;

    textSizeTemp = 1.0f;
    textSizeTotal = textSizeTemp * textSize;
    topLineHeight = 12 * textSizeTotal;
    textPos = topLineHeight;

    for (i = 0; i < strLen; i++) {
        while (i < strLen && str[i] == '<') {
            commandOffset = text_iterate_command(str, i, FALSE);
            if (commandOffset == 0)
                break;

            i += commandOffset;
        }

        if (i >= strLen)
            break;

        if (str[i] == '\n') {
            textPos += topLineHeight;
            topLineHeight = 12 * textSizeTotal;
            continue;
        }
    }

    return textPos;
}



const Gfx dl_small_text_begin[] = {
    gsDPPipeSync(),
    gsDPSetCycleType(    G_CYC_1CYCLE),
    gsDPSetTexturePersp( G_TP_NONE),
    gsDPSetCombineMode(  G_CC_FADEA, G_CC_FADEA),
    gsDPSetTextureFilter(G_TF_POINT),
    gsSPEndDisplayList(),
};

void set_text_size_params(void) {
    textSizeTotal = textSizeTemp * textSize;
    textTempScale = 1024/textSizeTotal;
    textOffsets[0] = 8 * textSizeTotal;
    textOffsets[1] = 12 * textSizeTotal;
    topLineHeight = MAX(12.0f * textSizeTotal, topLineHeight);
}

static s8 sTextShakeTable[] = {
    1, 1, 1, 1, 0, 1, 0, 0,
    0, 1, 1, 0, 1, 0, 0, 1,
    1, 1, 0, 1, 0, 0, 0, 0,
    1, 1, 0, 1, 0, 1, 0, 0
};

void print_small_text(s32 x, s32 y, const char *str, s32 align, s32 amount, u8 font) {
    s32 textX = 0;
    s32 textPos[2] = { 0, 0 };
    u16 wideX[16] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    s32 textLength = amount;
    s32 prevxlu = 256; // Set out of bounds, so it will *always* be different at first.
    s32 strLen = strlen(str);
    s32 commandOffset;
    f32 wavePos;
    f32 shakePos[2];
    s8 offsetY = 0;
    u8 spaceX = 0;
    u8 widthX = 0;
    u8 lines = 0;
    u8 xlu = gCurrEnvCol[3];
    u8 shakeTablePos = 0;
    struct PPTextFont **fntPtr = segmented_to_virtual(gPuppyPrintFontTable);
    struct PPTextFont *fnt = segmented_to_virtual(fntPtr[font]);

    shakeToggle = 0;
    waveToggle = 0;
    rainbowToggle = 0;
    textSizeTemp = 1.0f;
    set_text_size_params();

    if (amount <= PRINT_ALL || amount > strLen) {
        textLength = strLen;
    }

    // Calculate the text width for centre and right aligned text.
    gSPDisplayList(gDisplayListHead++, dl_small_text_begin);
    if (align == PRINT_TEXT_ALIGN_CENTRE || align == PRINT_TEXT_ALIGN_RIGHT) {
        for (s32 i = 0; i < strLen; i++) {
            while (i < strLen && str[i] == '<') {
                commandOffset = text_iterate_command(str, i, FALSE);
                if (commandOffset == 0)
                    break;

                i += commandOffset;
            }

            if (i >= strLen || str[i] == '\0')
                break;

            if (str[i] == '\n') {
                textPos[0] = 0;
                lines++;
                wideX[lines] = 0;
                continue;
            }

            get_char_from_byte(&textX, &textPos[0], str[i], &widthX, &spaceX, &offsetY, font);
            textPos[0] += (spaceX + 1) * textSizeTotal;
            wideX[lines] = MAX(textPos[0], wideX[lines]);
        }

        if (align == PRINT_TEXT_ALIGN_CENTRE) {
            textPos[0] = -(wideX[0] / 2);
        } else {
            textPos[0] = -(wideX[0]);
        }
    }

    //Reset text size properties.
    textSizeTemp = 1.0f;
    set_text_size_params();
    topLineHeight = 12.0f * textSizeTotal;
    lines = 0;
    
    shakeTablePos = gGlobalTimer % sizeof(sTextShakeTable);
    gDPLoadTextureBlock_4bS(gDisplayListHead++, fnt->tex, fnt->fmt, fnt->imW, fnt->imH, 0, (G_TX_NOMIRROR | G_TX_CLAMP), (G_TX_NOMIRROR | G_TX_CLAMP), 0, 0, G_TX_NOLOD, G_TX_NOLOD);
    
    for (s32 i = 0, j = 0; i < textLength; i++, j++) {
        if (str[i] == '\n') {
            lines++;
            if (align == PRINT_TEXT_ALIGN_RIGHT) {
                textPos[0] = -(wideX[lines]);
            } else {
                textPos[0] = -(wideX[lines] / 2);
            }
            textPos[1] += topLineHeight;
            topLineHeight = (f32) fnt->txH * textSizeTotal;
            continue;
        }

        while (i < textLength && str[i] == '<') {
            commandOffset = text_iterate_command(str, i, TRUE);
            if (commandOffset == 0)
                break;

            i += commandOffset;
            textLength += commandOffset;
        }

        if (i >= textLength || str[i] == '\0') {
            break;
        }

        if (shakeToggle) {
            shakePos[0] = (sTextShakeTable[shakeTablePos++] * textSizeTotal);
            if (shakeTablePos == sizeof(sTextShakeTable))
                shakeTablePos = 0;
            shakePos[1] = sTextShakeTable[shakeTablePos++] * textSizeTotal;
            if (shakeTablePos == sizeof(sTextShakeTable))
                shakeTablePos = 0;
        } else {
            shakePos[0] = 0;
            shakePos[1] = 0;
        }


        if (waveToggle) {
            wavePos = ((sins((gGlobalTimer * 3000) + (j * 10000))) * 2) * textSizeTotal;
        } else {
            wavePos = 0;
        }

        get_char_from_byte(&textX, &textPos[0], str[i], &widthX, &spaceX, &offsetY, font);
        s32 goddamnJMeasure = textX == 256 ? 1 : 0; // Hack to fix a rendering bug.
        if (str[i] != ' ' && str[i] != '\t') {
            if (xlu != prevxlu) {
                prevxlu = xlu;
                if (xlu > 250) {
                    gDPSetRenderMode(gDisplayListHead++, G_RM_TEX_EDGE, G_RM_TEX_EDGE2);
                } else {
                    gDPSetRenderMode(gDisplayListHead++, G_RM_XLU_SURF, G_RM_XLU_SURF);
                }
            }

            gSPScisTextureRectangle(gDisplayListHead++, (x + textPos[0] + (s16)(shakePos[0])) << 2,
                                                        (y + textPos[1] + (s16)((shakePos[1] + offsetY + wavePos))) << 2,
                                                        (x + textPos[0] + (s16)((shakePos[0] + (widthX * textSizeTotal)))) << 2,
                                                        (y + textPos[1] + (s16)((wavePos + offsetY + shakePos[1] + textOffsets[1]))) << 2,
                                                        G_TX_RENDERTILE, (textX << 6) + goddamnJMeasure, 0, textTempScale, textTempScale);
        }
        textPos[0] += (spaceX + 1) * textSizeTotal;
    }

    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);

    // Color reverted to pure white in dl_rgba16_text_end, so carry it over to gCurrEnvCol!
    // NOTE: if this behavior is ever removed, make sure gCurrEnvCol gets enforced here if the text color is ever altered in the text_iterate_command function.
    gCurrEnvCol[0] = 255; gCurrEnvCol[1] = 255; gCurrEnvCol[2] = 255; gCurrEnvCol[3] = 255;
}

// A more lightweight version of print_small_text.
// Strips all text modifiers so that only standard text remains.
// Can still support external colouring.
// Around 30% faster than regular printing.
void print_small_text_light(s32 x, s32 y, const char *str, s32 align, s32 amount, u8 font) {
    s32 textX = 0;
    s32 textPos[2] = { 0, 0 };
    u16 wideX[16] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    s32 textLength = amount;
    s32 prevxlu = 256; // Set out of bounds, so it will *always* be different at first.
    s32 strLen = strlen(str);
    s8 offsetY = 0;
    u8 spaceX = 0;
    u8 lines = 0;
    u8 widthX = 0;
    u8 xlu = gCurrEnvCol[3];
    struct PPTextFont **fntPtr = segmented_to_virtual(gPuppyPrintFontTable);
    struct PPTextFont *fnt = segmented_to_virtual(fntPtr[font]);

    if (amount <= PRINT_ALL || amount > strLen) {
        textLength = strLen;
    }

    // Calculate the text width for centre and right aligned text.
    gSPDisplayList(gDisplayListHead++, dl_small_text_begin);
    if (align == PRINT_TEXT_ALIGN_CENTRE || align == PRINT_TEXT_ALIGN_RIGHT) {
        for (s32 i = 0; i < strLen; i++) {
            if (str[i] == '\n') {
                textPos[0] = 0;
                lines++;
                wideX[lines] = 0;
                continue;
            }

            get_char_from_byte(&textX, &textPos[0], str[i], &widthX, &spaceX, &offsetY, font);
            textPos[0] += (spaceX + 1) * textSizeTotal;
            wideX[lines] = MAX(textPos[0], wideX[lines]);
        }

        if (align == PRINT_TEXT_ALIGN_CENTRE) {
            textPos[0] = -(wideX[0] / 2);
        } else {
            textPos[0] = -(wideX[0]);
        }
    }

    lines = 0;
    gDPLoadTextureBlock_4bS(gDisplayListHead++, fnt->tex, fnt->fmt, fnt->imW, fnt->imH, 0, (G_TX_NOMIRROR | G_TX_CLAMP), (G_TX_NOMIRROR | G_TX_CLAMP), 0, 0, G_TX_NOLOD, G_TX_NOLOD);
    
    for (s32 i = 0, j = 0; i < textLength; i++, j++) {
        if (str[i] == '\n') {
            lines++;
            if (align == PRINT_TEXT_ALIGN_RIGHT) {
                textPos[0] = -(wideX[lines]);
            } else {
                textPos[0] = -(wideX[lines] / 2);
            }
            textPos[1] += 12;
            continue;
        }
        
        if (i >= textLength || str[i] == '\0') {
            break;
        }

        get_char_from_byte(&textX, &textPos[0], str[i], &widthX, &spaceX, &offsetY, font);
        s32 goddamnJMeasure = textX == 256 ? 1 : 0; // Hack to fix a rendering bug.
        if (str[i] != ' ' && str[i] != '\t') {
            if (xlu != prevxlu) {
                prevxlu = xlu;
                if (xlu > 250) {
                    gDPSetRenderMode(gDisplayListHead++, G_RM_TEX_EDGE, G_RM_TEX_EDGE2);
                } else {
                    gDPSetRenderMode(gDisplayListHead++, G_RM_XLU_SURF, G_RM_XLU_SURF);
                }
            }

            gSPScisTextureRectangle(gDisplayListHead++, (x + textPos[0]) << 2,
                                                        (y + textPos[1] + offsetY) << 2,
                                                        (x + textPos[0] + widthX) << 2,
                                                        (y + textPos[1] + offsetY + fnt->txH) << 2,
                                                        G_TX_RENDERTILE, (textX << 6) + goddamnJMeasure, 0, 1024, 1024);
        }
        textPos[0] += (spaceX + 1);
    }

    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);

    // Color reverted to pure white in dl_rgba16_text_end, so carry it over to gCurrEnvCol!
    // NOTE: if this behavior is ever removed, make sure gCurrEnvCol gets enforced here if the text color is ever altered in the text_iterate_command function.
    gCurrEnvCol[0] = 255; gCurrEnvCol[1] = 255; gCurrEnvCol[2] = 255; gCurrEnvCol[3] = 255;
}

// Return color hex nibble
s32 get_hex_value_at_offset(const char *str, s32 primaryOffset, u32 nibbleOffset, u32 garbageReturnsEnv) {
    s32 val = str[primaryOffset + nibbleOffset];
    s32 shiftVal = 4 * ((nibbleOffset + 1) % 2);

    if (nibbleOffset > 7)
        garbageReturnsEnv = FALSE;

    if (val >= 'A' && val <= 'F')
        return (val - 'A' + 0xA) << shiftVal;
    if (val >= 'a' && val <= 'f')
        return (val - 'a' + 0xA) << shiftVal;
    if (val >= '0' && val <= '9')
        return (val - '0') << shiftVal;

    if (garbageReturnsEnv) // Return gCurrEnvCol color value
        return gCurrEnvCol[nibbleOffset / 2] & (0x0F << shiftVal);

    // Just return 0 otherwise
    return 0;
}

s32 text_iterate_command(const char *str, s32 i, s32 runCMD) {
    s32 len = 0;
    const char *newStr = &str[i];
    s32 lastCharIndex = (signed)strlen(newStr) - 1;

    while ((newStr[len] != '>') && (len < lastCharIndex)) len++;
    if (newStr[len] != '>')
        return 0;

    len++;

    // Ignores runCMD, because it's important this is ALWAYS ran.
    if (len == 10 && strncmp((newStr), "<SIZE_xxx>", 6) == 0) { // Set the text size here. 100 is scale 1.0, with 001 being scale 0.01. this caps at 999. Going lower than 001
        // Will make the text unreadable on console, so only do it,
        textSizeTemp = (newStr[6] - '0');
        textSizeTemp += (newStr[7] - '0')/10.0f;
        textSizeTemp += (newStr[8] - '0')/100.0f;
        textSizeTemp = CLAMP(textSizeTemp, 0.01f, 10.0f);
        set_text_size_params();
    } else if (len == 14 && strncmp((newStr), "<COL_xxxxxxxx>", 5) == 0) { // Simple text colour effect. goes up to FF for each, so FF0000FF is red.
        // Each value is taken from the string. The first is shifted left 4 bits, because it's a larger significant value, then it adds the next digit onto it.
        // Reverting to envcoluor can be achieved by passing something like <COL_-------->, or it could be combined with real colors for just partial reversion like <COL_FF00FF--> for instance.
        if (!runCMD)
            return len;

        s32 rgba[4];

        for (s32 j = 0; j < 4; j++) {
            rgba[j] = get_hex_value_at_offset(newStr, 5, 2 * j, TRUE) | get_hex_value_at_offset(newStr, 5, (2 * j) + 1, TRUE);
        }

        rainbowToggle = 0;
        gDPSetEnvColor(gDisplayListHead++, (Color) rgba[0], (Color) rgba[1], (Color) rgba[2], (Color) rgba[3]); // Don't use print_set_envcolour here
    } else if (len == 27 && strncmp((newStr), "<FADE_xxxxxxxx,xxxxxxxx,xx>", 6) == 0) { // Same as above, except it fades between two colours. The third set of numbers is the speed it fades.
        if (!runCMD)
            return len;

        s32 rgba[4];

        // Find transition speed and set timer value
        s32 spd = get_hex_value_at_offset(newStr, 24, 0, FALSE) | get_hex_value_at_offset(newStr, 24, 1, FALSE);
        f32 sTimer = sins(gGlobalTimer * spd * 50);

        for (s32 j = 0; j < 4; j++) {
            s32 col1 = get_hex_value_at_offset(newStr, 6, 2 * j, TRUE) | get_hex_value_at_offset(newStr, 6, (2 * j) + 1, TRUE);
            s32 col2 = get_hex_value_at_offset(newStr, 15, 2 * j, TRUE) | get_hex_value_at_offset(newStr, 15, (2 * j) + 1, TRUE);

            // Final color value determined by median of two colors + a point in the end-to-end width of the difference between the two colors.
            // Said point changes based on the sTimer value in the form of a sine wave, which helps to create the fading effect.
            rgba[j] = ((col1 + col2) / 2) + (s32) (sTimer * ((col1 - col2) / 2));
        }

        rainbowToggle = 0;
        gDPSetEnvColor(gDisplayListHead++, (Color) rgba[0], (Color) rgba[1], (Color) rgba[2], (Color) rgba[3]); // Don't use print_set_envcolour here
    } else if (len == 9 && strncmp((newStr), "<RAINBOW>", 9) == 0) { // Toggles the happy colours :o) Do it again to disable it.
        if (!runCMD)
            return len;

        rainbowToggle ^= 1;
        if (rainbowToggle) {
            s32 r = (coss(gGlobalTimer * 600) + 1) * 127;
            s32 g = (coss((gGlobalTimer * 600) + (0x10000 / 3)) + 1) * 127;
            s32 b = (coss((gGlobalTimer * 600) - (0x10000 / 3)) + 1) * 127;
            gDPSetEnvColor(gDisplayListHead++, (Color) r, (Color) g, (Color) b, (Color) gCurrEnvCol[3]); // Don't use print_set_envcolour here, also opt to use alpha value from gCurrEnvCol
        } else {
            gDPSetEnvColor(gDisplayListHead++, (Color) gCurrEnvCol[0], (Color) gCurrEnvCol[1], (Color) gCurrEnvCol[2], (Color) gCurrEnvCol[3]); // Reset text to envcolor
        }
    } else if (len == 7 && strncmp((newStr), "<SHAKE>", 7) == 0) { // Toggles text that shakes on the spot. Do it again to disable it.
        if (!runCMD)
            return len;

        shakeToggle ^= 1;
    } else if (len == 6 && strncmp((newStr), "<WAVE>",  6) == 0) { // Toggles text that waves around. Do it again to disable it.
        if (!runCMD)
            return len;

        waveToggle  ^= 1;
    } else {
        return 0; // Invalid command string; display everything inside to make this clear to the user.
    }

    return len;
}

void get_char_from_byte(s32 *textX, s32 *textPos, u8 letter, u8 *wideX, u8 *spaceX, s8 *offsetY, u8 font) {
    *offsetY = 0;
    u32 let = letter - '!';
    struct PPTextFont **fntPtr = segmented_to_virtual(gPuppyPrintFontTable);
    struct PPTextFont *fnt = segmented_to_virtual(fntPtr[font]);

    if (letter == ' ') {
        *spaceX = 2;
        return;
    }

    if (letter == '\t') {
        s32 offset = (*textPos % TAB_WIDTH);
        if (offset == TAB_WIDTH - 1) {
            offset -= TAB_WIDTH;
        }
        *textPos += TAB_WIDTH - offset - 1;
        *spaceX = 0;
        return;
    }

    if (fnt->kern == NULL || gMonoSpace) {
        *spaceX = fnt->txW;
    } else {
        u8 *kern = segmented_to_virtual(fnt->kern);
        *spaceX = kern[let];

    }
    if (fnt->offset == NULL) {
        *textX = let >> 2;
        *wideX = fnt->txW;
    } else {
        u16 *off = segmented_to_virtual(fnt->offset);
        *textX = off[let] >> 1;
        *wideX = (off[let + 1] - off[let]);
    }

    switch (letter) {
        // This is for the letters that sit differently on the line. It just moves them down a bit.
        case 'g': *offsetY = 2 * textSizeTotal; break;
        case 'q': *offsetY = 2 * textSizeTotal; break;
        case 'j': *offsetY = 2 * textSizeTotal; break;
        case 'p': *offsetY = 2 * textSizeTotal; break;
        case 'y': *offsetY = 2 * textSizeTotal; break;
    }
}

// Because we're using bcopy when both reading and writing to the text buffer, this doesn't care about alignment with the multi-byte types.
struct PuppyprintDeferredBufferHeader {
    u16 x;
    u16 y;
    u8 red;
    u8 green;
    u8 blue;
    u8 alpha;
    u8 alignment;
    u8 textBufferLength;
    u8 font;
    u8 isLightText;
};

static u8 gIsLightText = FALSE;

// This is where the deferred printing will be stored. When text is made, it will store text with a 12 byte header, then the rest will be the text data itself.
// The first 4 bytes of the header will be the X and Y pos
// The next 4 bytes will be the current envcolour set by print_set_envcolour
// Then the string length, text alignment, amount and font each get a byte.
// The data afterwards is the text data itself, using the string length byte to know when to stop.
#define HEADERSIZE sizeof(struct PuppyprintDeferredBufferHeader)
#define MAX_U8_STRING_SIZE (U8_MAX - 1) // Needs 255th character reserved for null terminator
void print_small_text_buffered(s32 x, s32 y, const char *str, u8 align, s32 amount, u8 font) {
    s32 strLen = strlen(str);

    if (amount <= PRINT_ALL || amount > MAX_U8_STRING_SIZE)
        amount = MAX_U8_STRING_SIZE;

    amount = MIN(strLen, amount);
    if (amount <= 0)
        return; // No point in printing an empty string

    // Compare the cursor position and the string length with null terminator, plus 12 (header size) and return if it overflows.
    if (sPuppyprintTextBufferPos + HEADERSIZE + (u8) amount + 1 > sizeof(sPuppyprintTextBuffer))
        return;

    x += 0x8000;
    y += 0x8000;

    struct PuppyprintDeferredBufferHeader header;
    header.x = (x & 0xFFFF);
    header.y = (y & 0xFFFF);
    header.red = gCurrEnvCol[0];
    header.green = gCurrEnvCol[1];
    header.blue = gCurrEnvCol[2];
    header.alpha = gCurrEnvCol[3];
    header.alignment = align;
    header.textBufferLength = (u8) amount;
    header.font = font;
    header.isLightText = gIsLightText;

    bcopy(&header, &sPuppyprintTextBuffer[sPuppyprintTextBufferPos], HEADERSIZE);
    sPuppyprintTextBufferPos += HEADERSIZE;
    bcopy(str, &sPuppyprintTextBuffer[sPuppyprintTextBufferPos], header.textBufferLength);
    sPuppyprintTextBufferPos += header.textBufferLength;
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos++] = '\0'; // Apply null terminator onto end of string
}

void print_small_text_buffered_light(s32 x, s32 y, const char *str, u8 align, s32 amount, u8 font) {
    gIsLightText = TRUE;
    print_small_text_buffered(x, y, str, align, amount, font);
    gIsLightText = FALSE;
}

void puppyprint_print_deferred(void) {
    if (sPuppyprintTextBufferPos == 0)
        return;
    bzero(&gCurrEnvCol, sizeof(ColorRGBA));
    print_set_envcolour(255, 255, 255, 255);

    for (u32 i = 0; i < sPuppyprintTextBufferPos;) {
        struct PuppyprintDeferredBufferHeader header;
        bcopy(&sPuppyprintTextBuffer[i], &header, HEADERSIZE);
        i += HEADERSIZE;

        s32 x = header.x;
        s32 y = header.y;
        x -= 0x8000;
        y -= 0x8000;
        ColorRGBA originalEnvCol = {gCurrEnvCol[0], gCurrEnvCol[1], gCurrEnvCol[2], gCurrEnvCol[3]};
        print_set_envcolour(header.red, header.green, header.blue, header.alpha);

        char *text = (char *) &sPuppyprintTextBuffer[i];
        if (header.isLightText) {
            print_small_text_light(x, y, text, header.alignment, header.textBufferLength, header.font);
        } else {
            print_small_text(x, y, text, header.alignment, header.textBufferLength, header.font);
        }

        print_set_envcolour(originalEnvCol[0], originalEnvCol[1], originalEnvCol[2], originalEnvCol[3]);
        i += header.textBufferLength + 1; // Null terminator not accounted for by text buffer length
    }

    //Reset the position back to zero, effectively clearing the buffer.
    sPuppyprintTextBufferPos = 0;
}

#endif
