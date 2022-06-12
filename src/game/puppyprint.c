/**
--------------Puppyprint 1.0 by Fazana--------------
Includes a few printing functions to fit any purpose.
print_small_text is intended to replace print_generic_string in use, as it uses a far more optimised way of doing things,
supports real time ascii conversion, and also supports many fun effects to spice up the text.
Any usage of gDPSetEnvColor should ideally be replaced with print_set_envcolour because it helps with some optimisations.
render_multi_image can be used to draw large texture rectangles consisting of multiple images on the screen.
You only need have the single image in its full form, with no need for splitting it, and simply just load it.

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
#include "game_init.h"
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
#include "audio/heap.h"
#include "hud.h"
#include "debug_box.h"
#include "color_presets.h"
#include "buffers/buffers.h"

#define PUPPYPRINT
#ifdef PUPPYPRINT

#ifdef ENABLE_CREDITS_BENCHMARK
u8 fDebug = TRUE;
#else
u8 fDebug = FALSE;
#endif
u8 sPuppyprintTextBuffer[PUPPYPRINT_DEFERRED_BUFFER_SIZE];
u32 sPuppyprintTextBufferPos; // Location in the buffer of puppyprint deferred text.
ColorRGBA gCurrEnvCol;

#if PUPPYPRINT_DEBUG
struct PuppyPrintTimers gPuppyTimers;

#define GENERAL_PAGE_TEXT_LENGTH 200

/// Add whichever times you wish to read values for.
/// Make sure there is an equal number of names to values.
void puppyprint_sprintf_general(char *str)
{
    sprintf(str,
    "Collision: %dus\n"
    "Graph: %dus\n"
    "Behaviour: %dus\n"
    "Audio: %dus\n"
    "Camera: %dus\n"
    "DMA: %dus\n"
    "Controller: %dus\n"
    ,
    gPuppyTimers.collisionTime[PERF_TOTAL],
    gPuppyTimers.graphTime[PERF_TOTAL],
    gPuppyTimers.behaviourTime[PERF_TOTAL],
    gPuppyTimers.thread4Time[PERF_TOTAL],
    gPuppyTimers.cameraTime[PERF_TOTAL],
    gPuppyTimers.dmaTime[PERF_TOTAL],
    gPuppyTimers.controllerTime[PERF_TOTAL]
    );
}

/// Add whichever times you wish to create aggregates of.
void puppyprint_calculate_average_times(void)
{
    gPuppyTimers.collisionTime[PERF_TOTAL] = OS_CYCLES_TO_USEC(gPuppyTimers.collisionTime[PERF_AGGREGATE]) / NUM_PERF_ITERATIONS;
    gPuppyTimers.behaviourTime[PERF_TOTAL] = OS_CYCLES_TO_USEC(gPuppyTimers.behaviourTime[PERF_AGGREGATE]) / NUM_PERF_ITERATIONS;
    gPuppyTimers.thread2Time[PERF_TOTAL] = OS_CYCLES_TO_USEC(gPuppyTimers.thread2Time[PERF_AGGREGATE]) / NUM_PERF_ITERATIONS;
    gPuppyTimers.thread3Time[PERF_TOTAL] = OS_CYCLES_TO_USEC(gPuppyTimers.thread3Time[PERF_AGGREGATE]) / NUM_PERF_ITERATIONS;
    gPuppyTimers.thread4Time[PERF_TOTAL] = OS_CYCLES_TO_USEC(gPuppyTimers.thread4Time[PERF_AGGREGATE]) / NUM_PERF_ITERATIONS;
    gPuppyTimers.thread5Time[PERF_TOTAL] = OS_CYCLES_TO_USEC(gPuppyTimers.thread5Time[PERF_AGGREGATE]) / NUM_PERF_ITERATIONS;
    gPuppyTimers.thread6Time[PERF_TOTAL] = OS_CYCLES_TO_USEC(gPuppyTimers.thread6Time[PERF_AGGREGATE]) / NUM_PERF_ITERATIONS;
    gPuppyTimers.graphTime[PERF_TOTAL] = OS_CYCLES_TO_USEC(gPuppyTimers.graphTime[PERF_AGGREGATE]) / NUM_PERF_ITERATIONS;
    gPuppyTimers.dmaTime[PERF_TOTAL] = OS_CYCLES_TO_USEC(gPuppyTimers.dmaTime[PERF_AGGREGATE]) / NUM_PERF_ITERATIONS;
    gPuppyTimers.dmaAudioTime[PERF_TOTAL] = OS_CYCLES_TO_USEC(gPuppyTimers.dmaAudioTime[PERF_AGGREGATE]) / NUM_PERF_ITERATIONS;
    gPuppyTimers.cameraTime[PERF_TOTAL] = OS_CYCLES_TO_USEC(gPuppyTimers.cameraTime[PERF_AGGREGATE]) / NUM_PERF_ITERATIONS;
    gPuppyTimers.profilerTime[PERF_TOTAL] = OS_CYCLES_TO_USEC(gPuppyTimers.profilerTime[PERF_AGGREGATE]) / NUM_PERF_ITERATIONS;
    gPuppyTimers.profilerTime2[PERF_TOTAL] = OS_CYCLES_TO_USEC(gPuppyTimers.profilerTime2[PERF_AGGREGATE]) / NUM_PERF_ITERATIONS;
    gPuppyTimers.controllerTime[PERF_TOTAL] = OS_CYCLES_TO_USEC(gPuppyTimers.controllerTime[PERF_AGGREGATE]) / NUM_PERF_ITERATIONS;
    gPuppyTimers.rspAudioTime[PERF_TOTAL] = OS_CYCLES_TO_USEC(gPuppyTimers.rspAudioTime[PERF_AGGREGATE]) / NUM_PERF_ITERATIONS;
    gPuppyTimers.rspGfxTime[PERF_TOTAL] = OS_CYCLES_TO_USEC(gPuppyTimers.rspGfxTime[PERF_AGGREGATE]) / NUM_PERF_ITERATIONS;
    gPuppyTimers.rdpBufTime[PERF_TOTAL] = (gPuppyTimers.rdpBufTime[PERF_AGGREGATE] * 10) / (625*NUM_PERF_ITERATIONS);
    gPuppyTimers.rdpBusTime[PERF_TOTAL] = (gPuppyTimers.rdpBusTime[PERF_AGGREGATE] * 10) / (625*NUM_PERF_ITERATIONS);
}

s8 logViewer    = FALSE;
u8 sPPDebugPage = 0;
u8 sDebugMenu   = FALSE;
u8 sDebugOption = 0;
// Profiler values
s8  perfIteration  = 0;
s32 ramsizeSegment[NUM_TLB_SEGMENTS + 1] = {
    0, 0, 0,
    0, 0, 0,
    0, 0, 0,
    0, 0, 0,
    0, 0, 0,
    0, 0, 0,
    0, 0, 0,
    0, 0, 0,
    0, 0, 0,
    0, 0, 0,
    0, 0, 0
};
s32 mempool;
u32 gPoolMem;
u32 gPPSegScroll = 0;
u32 gMiscMem = 0;

extern u8 _mainSegmentStart[];
extern u8 _mainSegmentEnd[];
extern u8 _engineSegmentStart[];
extern u8 _engineSegmentEnd[];
extern u8 _framebuffersSegmentBssStart[];
extern u8 _framebuffersSegmentBssEnd[];
extern u8 _zbufferSegmentBssStart[];
extern u8 _zbufferSegmentBssEnd[];
extern u8 _buffersSegmentBssStart[];
extern u8 _buffersSegmentBssEnd[];
extern u8 _goddardSegmentStart[];
extern u8 _goddardSegmentEnd[];

void puppyprint_calculate_ram_usage(void) {
    ramsizeSegment[0] = (u32)&_buffersSegmentBssEnd - (u32)&_buffersSegmentBssStart - sizeof(gAudioHeap);
    ramsizeSegment[1] = (u32)&_mainSegmentEnd - (u32)&_mainSegmentStart;
    ramsizeSegment[2] = (u32)&_engineSegmentEnd - (u32)&_engineSegmentStart;
    ramsizeSegment[3] = (u32)&_framebuffersSegmentBssEnd - (u32)&_framebuffersSegmentBssStart;
    ramsizeSegment[4] = (u32)&_zbufferSegmentBssEnd - (u32)&_zbufferSegmentBssStart;
    ramsizeSegment[5] = (u32)&_goddardSegmentEnd - (u32)&_goddardSegmentStart;
    ramsizeSegment[6] = gPoolMem;
    ramsizeSegment[7] = ALIGN16(SURFACE_NODE_POOL_SIZE * sizeof(struct SurfaceNode)) + 16 + ALIGN16(SURFACE_POOL_SIZE * sizeof(struct Surface)) + 16;
    ramsizeSegment[8] = gMiscMem;
    ramsizeSegment[9] = gAudioHeapSize + gAudioInitPoolSize;
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
    {   0,   0, 128 },
    { 128,   0, 128 },
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

// Change this to alter the width of the bar at the bottom.
#define RAM_BAR_LENGTH 200
#define RAM_BAR_MIN    (SCREEN_CENTER_X - (RAM_BAR_LENGTH / 2))
#define RAM_BAR_MAX    (SCREEN_CENTER_X + (RAM_BAR_LENGTH / 2))
#define RAM_BAR_TOP    (SCREEN_HEIGHT - 30)
#define RAM_BAR_BOTTOM (SCREEN_HEIGHT - 22)

void print_ram_bar(void) {
    s32 i = 0;
    f32 perfPercentage;
    s32 graphPos = 0;
    s32 prevGraph = RAM_BAR_MIN;
    s32 ramsize = osGetMemSize();

    prepare_blank_box();

    for (i = 0; i < NUM_TLB_SEGMENTS; i++) {
        if (ramsizeSegment[i] == 0) {
            continue;
        }

        perfPercentage = (RAM_BAR_LENGTH * ((f32)ramsizeSegment[i] / ramsize));
        graphPos = (prevGraph + CLAMP(perfPercentage, 1, RAM_BAR_MAX));
        render_blank_box(prevGraph, RAM_BAR_TOP, graphPos, RAM_BAR_BOTTOM,
            colourChart[i][0],
            colourChart[i][1],
            colourChart[i][2], 255);
        prevGraph = graphPos;
    }

    perfPercentage = (RAM_BAR_LENGTH * ((f32)ramsizeSegment[NUM_TLB_SEGMENTS] / ramsize));
    graphPos = (prevGraph + CLAMP(perfPercentage, 1, RAM_BAR_MAX));
    render_blank_box(prevGraph, RAM_BAR_TOP, graphPos, RAM_BAR_BOTTOM, 255, 255, 255, 255);
    prevGraph = graphPos;

    render_blank_box(prevGraph, RAM_BAR_TOP, RAM_BAR_MAX, RAM_BAR_BOTTOM, 0, 0, 0, 255);

    finish_blank_box();
}

// Another epic lookup table, for text this time.
const char ramNames[][32] = {
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

const char segNames[][32] = {
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

const s8 nameTable = sizeof(ramNames) / 32;

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
    for (i = 0; i < 32; i++) {

        if (values[i] == 0)
            continue;
        // Find the minimum element in unsorted array
        min_idx = i;
        for (j = i + 1; j < 32; j++)
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
    u32 tempNums[32];
    u8 tempPos[32] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};
    prepare_blank_box();
    render_blank_box(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, 0, 0, 0, 192);
    finish_blank_box();

    //f32 total = 0;
    //u32 total2 = 0;

    memcpy(&tempNums, &ramsizeSegment, 32 * 4);

    sort_numbers(&tempNums, &tempPos);

    print_set_envcolour(255, 255, 255, 255);
    sprintf(textBytes, "Total:");
    print_small_text(24, 16- gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_DEFAULT);
    sprintf(textBytes, "%06X",RAM_END - 0x80000000);
    print_small_text(SCREEN_WIDTH/2, 16 - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_DEFAULT);
    sprintf(textBytes, "%X", mempool);
    print_small_text(SCREEN_WIDTH - 24, 16 - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_RIGHT, PRINT_ALL, FONT_DEFAULT);
    sprintf(textBytes, "Used:");
    print_small_text(24, 28- gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_DEFAULT);
    sprintf(textBytes, "%06X", (RAM_END - 0x80000000) - (main_pool_available() - 0x400));
    print_small_text(SCREEN_WIDTH/2, 28 - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_DEFAULT);
    sprintf(textBytes, "(%2.3f_)", 100.0f - (((f32)(main_pool_available() - 0x400) / (f32)(RAM_END - 0x80000000)) * 100));
    print_small_text(SCREEN_WIDTH - 24, 28 - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_RIGHT, PRINT_ALL, FONT_DEFAULT);
    sprintf(textBytes, "Free:");
    print_small_text(24, 40 - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_DEFAULT);
    sprintf(textBytes, "%X", (main_pool_available() - 0x400));
    print_small_text(SCREEN_WIDTH/2, 40 - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_DEFAULT);
    sprintf(textBytes, "(%2.3f_)", (((f32)(main_pool_available() - 0x400) / (f32)(RAM_END - 0x80000000)) * 100));
    print_small_text(SCREEN_WIDTH - 24, 40 - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_RIGHT, PRINT_ALL, FONT_DEFAULT);
    for (u8 i = 0; i < 32; i++) {
        if (tempNums[i] == 0) {
            continue;
        }
        //total += ((f32)tempNums[i] / ramSize) * 100.0f;
        //total2 += tempNums[i];
        if (y - gPPSegScroll > 0 && y - gPPSegScroll < SCREEN_HEIGHT) {
            if (tempPos[i] < nameTable) {
                sprintf(textBytes, "%s:", ramNames[tempPos[i]]);
            } else {
                sprintf(textBytes, "%s:", segNames[tempPos[i] - nameTable]);
            }
            //print_set_envcolour(colourChart[tempPos[i]][0], colourChart[tempPos[i]][1], colourChart[tempPos[i]][2], 255);
            print_small_text(24, y - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_DEFAULT);
            sprintf(textBytes, "%X", tempNums[i]);
            print_small_text(SCREEN_WIDTH/2, y - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_DEFAULT);
            sprintf(textBytes, "(%2.3f_)", ((f32)tempNums[i] / ramSize) * 100.0f);
            print_small_text(SCREEN_WIDTH - 24, y - gPPSegScroll, textBytes, PRINT_TEXT_ALIGN_RIGHT, PRINT_ALL, FONT_DEFAULT);
        }
        y += 12;
    }
    /*sprintf(textBytes, "%2.2f", total);
    print_small_text(32, 32, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
    sprintf(textBytes, "%X", total2);
    print_small_text(32, 48, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
    sprintf(textBytes, "%X", (RAM_END - 0x80000000) - total2);
    print_small_text(32, 64, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);*/
}

const char *audioPoolNames[NUM_AUDIO_POOLS] = {
    "gAudioInitPool",
    "gNotesAndBuffersPool",
    "gSeqLoadedPool.persistent.pool",
    "gSeqLoadedPool.temporary.pool",
    "gBankLoadedPool.persistent.pool",
    "gBankLoadedPool.temporary.pool",
#if defined(BETTER_REVERB) && (defined(VERSION_US) || defined(VERSION_JP))
    "gBetterReverbPool",
#endif
};

void print_audio_ram_overview(void) {
    char textBytes[128];
    const s32 x = 16;
    s32 y = 16;
    s32 percentage = 0;
    s32 tmpY = y;
    s32 totalMemory[2] = { 0, 0 };
    s32 audioPoolSizes[NUM_AUDIO_POOLS][2];
    prepare_blank_box();
    render_blank_box(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, 0, 0, 0, 192);
    finish_blank_box();

    puppyprint_get_allocated_pools(audioPoolSizes[0]);

    y += 24;
    for (u8 i = 0; i < NUM_AUDIO_POOLS; i++) {
        if (audioPoolSizes[i][0] == 0) {
            percentage = 1000;
        } else {
            percentage = (((s64) audioPoolSizes[i][1] * 1000) / audioPoolSizes[i][0]);
        }

        sprintf(textBytes, "%s: %X / %X (%d.%d_)", audioPoolNames[i],
                audioPoolSizes[i][1],
                audioPoolSizes[i][0],
                percentage / 10,
                percentage % 10);

        print_set_envcolour(colourChart[i][0],
                            colourChart[i][1],
                            colourChart[i][2], 255);
        print_small_text(x, y, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);

        y += 12;

        totalMemory[0] += audioPoolSizes[i][0];
        totalMemory[1] += audioPoolSizes[i][1];
    }

    if (totalMemory[0] == 0) {
        percentage = 0;
    } else {
        percentage = (((s64) totalMemory[1] * 1000) / totalMemory[0]);
    }
    if (totalMemory[0] == gAudioHeapSize) {
        sprintf(textBytes, "TOTAL AUDIO MEMORY: %X / %X (%d.%d_)",
                totalMemory[1],
                totalMemory[0],
                percentage / 10,
                percentage % 10);
    } else {
        sprintf(textBytes, "TOTAL AUDIO MEMORY: %X / %X (Incorrect!)",
                totalMemory[1],
                totalMemory[0]);
    }

    print_set_envcolour(colourChart[30][0],
                        colourChart[30][1],
                        colourChart[30][2], 255);
    print_small_text(x, tmpY, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
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
        if (consoleLogTable[i] == NULL) {
            continue;
        }
        print_small_text(16, (LINE_HEIGHT - (i * 12)), consoleLogTable[i], PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_DEFAULT);
    }
}
#undef LINE_HEIGHT

extern u8 viewCycle;
extern s16 gVisualSurfaceCount;
#ifndef VISUAL_DEBUG
    #define gVisualSurfaceCount 0
#endif

void puppyprint_render_collision(void) {
    char textBytes[200];
#ifdef PUPPYPRINT_DEBUG_CYCLES
    sprintf(textBytes, "Collision: <COL_FF7F7FFF>%dc", gPuppyTimers.collisionTime[NUM_PERF_ITERATIONS]);
#else
    sprintf(textBytes, "Collision: <COL_FF7F7FFF>%dus", gPuppyTimers.collisionTime[NUM_PERF_ITERATIONS]);
#endif
    print_small_text(304, 48, textBytes, PRINT_TEXT_ALIGN_RIGHT, PRINT_ALL, 1);

    sprintf(textBytes, "Pool Size: %X#Node Size: %X#Surfaces Allocated: %d#Nodes Allocated: %d#Current Cell: %d", (SURFACE_NODE_POOL_SIZE * sizeof(struct SurfaceNode)), (SURFACE_POOL_SIZE * sizeof(struct Surface)),
            gSurfacesAllocated, gSurfaceNodesAllocated, gVisualSurfaceCount);
    print_small_text(304, 60, textBytes, PRINT_TEXT_ALIGN_RIGHT, PRINT_ALL, 1);


#ifdef VISUAL_DEBUG
    print_small_text(160, (SCREEN_HEIGHT - 42), "Use the dpad to toggle visual collision modes", PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, 1);
    switch (viewCycle) {
        case 0: print_small_text(160, (SCREEN_HEIGHT - 32), "Current view: None",                  PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, 1); break;
        case 1: print_small_text(160, (SCREEN_HEIGHT - 32), "Current view: Hitboxes",              PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, 1); break;
        case 2: print_small_text(160, (SCREEN_HEIGHT - 32), "Current view: Surfaces",              PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, 1); break;
        case 3: print_small_text(160, (SCREEN_HEIGHT - 32), "Current view: Hitboxes and Surfaces", PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, 1); break;
    }

    hitboxView  = ((viewCycle == 1) || (viewCycle == 3));
    surfaceView = ((viewCycle == 2) || (viewCycle == 3));
#endif
}

extern void print_fps(s32 x, s32 y);

void print_basic_profiling(void) {
    char textBytes[90];
    print_fps(16, 40);
#ifdef PUPPYPRINT_DEBUG_CYCLES
    sprintf(textBytes, "CPU: %dc (%d_)#RSP: %dc (%d_)#RDP: %dc (%d_)",
            gPuppyTimers.cpuTime, (gPuppyTimers.cpuTime / 15625),
            gPuppyTimers.rspTime, (gPuppyTimers.rspTime / 15625),
            gPuppyTimers.rdpTime, (gPuppyTimers.rdpTime / 15625));
#else
    sprintf(textBytes, "CPU: %dus (%d_)#RSP: %dus (%d_)#RDP: %dus (%d_)",
            gPuppyTimers.cpuTime, (gPuppyTimers.cpuTime / 333),
            gPuppyTimers.rspTime, (gPuppyTimers.rspTime / 333),
            gPuppyTimers.rdpTime, (gPuppyTimers.rdpTime / 333));
#endif
    print_small_text(16, 52, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
}

void puppyprint_render_standard(void) {
    char textBytes[GENERAL_PAGE_TEXT_LENGTH];

    print_basic_profiling();

    sprintf(textBytes, "OBJ: %d/%d", gObjectCounter, OBJECT_POOL_CAPACITY);
    print_small_text(16, 124, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);

#ifndef ENABLE_CREDITS_BENCHMARK
    // Very little point printing useless info if Mario doesn't even exist.
    if (gMarioState->marioObj) {
        sprintf(textBytes, "Mario Pos#X: %d#Y: %d#Z: %d#D: %X#A: %x",
            (s32)(gMarioState->pos[0]),
            (s32)(gMarioState->pos[1]),
            (s32)(gMarioState->pos[2]),
            (u16)(gMarioState->faceAngle[1]),
            (u32)(gMarioState->action & ACT_ID_MASK));
        print_small_text(16, 140, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
    }
#endif

    puppyprint_sprintf_general(textBytes);
    print_small_text((SCREEN_WIDTH - 24), 40, textBytes, PRINT_TEXT_ALIGN_RIGHT, PRINT_ALL, FONT_OUTLINE);
}

void puppyprint_render_minimal(void) {
    print_basic_profiling();
}

void render_coverage_map(void) {
    gDPSetCycleType(gDisplayListHead++, G_CYC_1CYCLE);
    gDPSetBlendColor(gDisplayListHead++, 0xFF, 0xFF, 0xFF, 0xFF);
    gDPSetPrimDepth(gDisplayListHead++, 0xFFFF, 0xFFFF);
    gDPSetDepthSource(gDisplayListHead++, G_ZS_PRIM);
    gDPSetRenderMode(gDisplayListHead++, G_RM_VISCVG, G_RM_VISCVG2);
    gDPFillRectangle(gDisplayListHead++, 0,0, SCREEN_WIDTH-1, SCREEN_HEIGHT-1);
}

void puppycamera_debug_view(void) {
    char textBytes[80];
    // Very little point printing useless info if Mayro doesn't even exist.
    if (gMarioState->marioObj) {
        sprintf(textBytes, "Mario Pos#X: %d#Y: %d#Z: %d#D: %X#A: %x",
            (s32)(gMarioState->pos[0]),
            (s32)(gMarioState->pos[1]),
            (s32)(gMarioState->pos[2]),
            (u16)(gMarioState->faceAngle[1]),
            (u32)(gMarioState->action & ACT_ID_MASK));
        print_small_text(16, 140, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
    }
    // Same for the camera, especially so because this will crash otherwise.
    if (gCamera) {
        sprintf(textBytes, "Camera Pos#X: %d#Y: %d#Z: %d#D: %X",
            (s32)(gCamera->pos[0]),
            (s32)(gCamera->pos[1]),
            (s32)(gCamera->pos[2]),
            (u16)(gCamera->yaw));
        print_small_text((SCREEN_WIDTH - 16), 140, textBytes, PRINT_TEXT_ALIGN_RIGHT, PRINT_ALL, FONT_OUTLINE);
    }
}

struct PuppyPrintPage ppPages[] = {
    {&puppyprint_render_standard,  "Standard" },
    {&puppyprint_render_minimal,   "Minimal"  },
    {&print_audio_ram_overview,    "Audio"    },
    {&print_ram_overview,          "Segments" },
    {&puppyprint_render_collision, "Collision"},
    {&print_console_log,           "Log"      },
    {&render_coverage_map, "Coverage"},
#ifdef PUPPYCAM
    {&puppycamera_debug_view, "Unlock Camera"},
#endif
};

#define MENU_BOX_WIDTH 128
#define MAX_DEBUG_OPTIONS (sizeof(ppPages) / sizeof(struct PuppyPrintPage))

void render_page_menu(void) {
    s32 i;
    s32 posY;
    s32 scrollY = (36 / (MAX_DEBUG_OPTIONS - 1));

    prepare_blank_box();
    render_blank_box(32, 32, (32 + MENU_BOX_WIDTH), (32 + 72), 0x00, 0x00, 0x00, 0xC0);
    render_blank_box(((32 + MENU_BOX_WIDTH) - 8), (32 + (scrollY * sDebugOption)), (32 + MENU_BOX_WIDTH), (32 + (scrollY * sDebugOption) + 36), 0xFF, 0xFF, 0xFF, 0xFF);
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

            print_small_text((28 + (MENU_BOX_WIDTH / 2)), posY, ppPages[i].name, PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, 0);
        }
    }
}

void rdp_profiler_update(u32 *time, OSTime time2) {
    time[PERF_AGGREGATE] -= time[perfIteration];
    time[perfIteration] = time2;
    time[PERF_AGGREGATE] += time[perfIteration];
}

void puppyprint_render_profiler(void) {
    OSTime first = osGetTime();
    rdp_profiler_update(gPuppyTimers.rdpBufTime, IO_READ(DPC_BUFBUSY_REG));
    rdp_profiler_update(gPuppyTimers.rdpTmmTime, IO_READ(DPC_TMEM_REG));
    rdp_profiler_update(gPuppyTimers.rdpBusTime, IO_READ(DPC_PIPEBUSY_REG));
    IO_WRITE(DPC_STATUS_REG, DPC_CLR_CLOCK_CTR | DPC_CLR_CMD_CTR | DPC_CLR_PIPE_CTR | DPC_CLR_TMEM_CTR);

    bzero(&gCurrEnvCol, sizeof(ColorRGBA));
    print_set_envcolour(255, 255, 255, 255);

    if (!fDebug) {
        profiler_update(gPuppyTimers.profilerTime, first);
        return;
    }

    (ppPages[sPPDebugPage].func)();

    if (sDebugMenu) {
        render_page_menu();
    }
    profiler_update(gPuppyTimers.profilerTime, first);
}

void profiler_update(u32 *time, OSTime time2) {
    time[PERF_AGGREGATE] -= time[perfIteration];
    time[perfIteration] = (osGetTime() - time2);
    time[PERF_AGGREGATE] += time[perfIteration];
}

void profiler_offset(u32 *time, OSTime offset) {
    time[PERF_AGGREGATE] -= offset;
    time[perfIteration] -= offset;
}

void profiler_add(u32 *time, OSTime offset) {
    time[PERF_AGGREGATE] += offset;
    time[perfIteration] += offset;
}

void puppyprint_update_rsp(u8 flags) {
    switch (flags) {
    case RSP_GFX_START:
        gPuppyTimers.rspGfxBufTime = (u32)osGetTime();
        gPuppyTimers.rspPauseTime = 0;
        break;
    case RSP_AUDIO_START:
        gPuppyTimers.rspAudioBufTime = (u32)osGetTime();
        break;
    case RSP_GFX_PAUSED:
        gPuppyTimers.rspPauseTime = (u32)osGetTime();
        break;
    case RSP_GFX_RESUME:
        gPuppyTimers.rspPauseTime = (u32)osGetTime() - gPuppyTimers.rspPauseTime;
        break;
    case RSP_GFX_FINISHED:
        gPuppyTimers.rspGfxTime[PERF_AGGREGATE] -= gPuppyTimers.rspGfxTime[perfIteration];
        gPuppyTimers.rspGfxTime[perfIteration] = (u32)(osGetTime() - gPuppyTimers.rspGfxBufTime) + gPuppyTimers.rspPauseTime;
        gPuppyTimers.rspGfxTime[PERF_AGGREGATE] += gPuppyTimers.rspGfxTime[perfIteration];
        break;
    case RSP_AUDIO_FINISHED:
        gPuppyTimers.rspAudioTime[PERF_AGGREGATE] -= gPuppyTimers.rspAudioTime[perfIteration];
        gPuppyTimers.rspAudioTime[perfIteration] = (u32)osGetTime() - gPuppyTimers.rspAudioBufTime;
        gPuppyTimers.rspAudioTime[PERF_AGGREGATE] += gPuppyTimers.rspAudioTime[perfIteration];
        break;
    }
}

void puppyprint_profiler_process(void) {
    OSTime newTime = osGetTime();

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

    // Collision toggles.
#ifdef VISUAL_DEBUG
    if (sPPDebugPage == 4)
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

    if (sPPDebugPage == 3) {
        if (gPlayer1Controller->buttonDown & U_JPAD && gPPSegScroll > 0)  {
            gPPSegScroll -= 4;
        } else if (gPlayer1Controller->buttonDown & D_JPAD && gPPSegScroll < (12 * 32)){
            gPPSegScroll += 4;
        }
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
    }

    if ((gGlobalTimer % 2) == 0) {
        if (gGlobalTimer < 2) // Nuke the timers at the beginning of the game.
            bzero(&gPuppyTimers, sizeof(struct PuppyPrintTimers));
        // Convert all total timers to microseconds, and divide by iterations.
        puppyprint_calculate_average_times();
        puppyprint_calculate_ram_usage();
        // Since audio runs twice a frame without fail, audio timers are doubled to compensate.
        gPuppyTimers.thread4Time[PERF_TOTAL] *= 2;
        gPuppyTimers.dmaAudioTime[PERF_TOTAL] *= 2;
        gPuppyTimers.dmaTime[PERF_TOTAL] += gPuppyTimers.dmaAudioTime[PERF_TOTAL];
        gPuppyTimers.cpuTime = gPuppyTimers.thread2Time[PERF_TOTAL] + gPuppyTimers.thread3Time[PERF_TOTAL] + gPuppyTimers.thread4Time[PERF_TOTAL] + gPuppyTimers.thread5Time[PERF_TOTAL] +
        gPuppyTimers.thread6Time[PERF_TOTAL]; //Thread timers are all added together to get the total CPU time.
        gPuppyTimers.threadsTime = gPuppyTimers.thread2Time[PERF_TOTAL] + gPuppyTimers.thread3Time[PERF_TOTAL] + gPuppyTimers.thread6Time[PERF_TOTAL];
        gPuppyTimers.rspTime = gPuppyTimers.rspAudioTime[PERF_TOTAL] + gPuppyTimers.rspGfxTime[PERF_TOTAL];
        gPuppyTimers.rdpTime = MAX(gPuppyTimers.rdpBufTime[PERF_TOTAL], gPuppyTimers.rdpTmmTime[PERF_TOTAL]);
        gPuppyTimers.rdpTime = MAX(gPuppyTimers.rdpBusTime[PERF_TOTAL], gPuppyTimers.rdpTime);
        // The iQue Player's RDP registers are halved to appear correct.
#if BBPLAYER == 1
            gPuppyTimers.rdpTime /= 2;
#endif
    }

    if (perfIteration++ == (NUM_PERF_ITERATIONS - 1)) {
        perfIteration = 0;
    }
    profiler_update(gPuppyTimers.profilerTime2, newTime);
}
#endif

void print_set_envcolour(u8 r, u8 g, u8 b, u8 a) {
    if ((r != gCurrEnvCol[0])
        || (g != gCurrEnvCol[1])
        || (b != gCurrEnvCol[2])
        || (a != gCurrEnvCol[3])) {
        gDPSetEnvColor(gDisplayListHead++, (Color)r, (Color)g, (Color)b, (Color)a);
        vec4_set(gCurrEnvCol, r, g, b, a);
    }
}

#define BLANK 0, 0, 0, ENVIRONMENT, 0, 0, 0, ENVIRONMENT

void prepare_blank_box(void) {
    gDPSetCombineMode(gDisplayListHead++, BLANK, BLANK);
}

void finish_blank_box(void) {
    print_set_envcolour(255, 255, 255, 255);
    gSPDisplayList(gDisplayListHead++, dl_hud_img_end);
}

// This does some epic shenanigans to figure out the optimal way to draw this.
// If the width is a multiple of 4, then use fillmode (fastest)
// Otherwise, if there's transparency, it uses that rendermode, which is slower than using opaque rendermodes.
void render_blank_box(s32 x1, s32 y1, s32 x2, s32 y2, u8 r, u8 g, u8 b, u8 a) {

    if (x1 < 0) x1 = 0;
    if (y1 < 0) y1 = 0;
    if (x2 > SCREEN_WIDTH) x2 = SCREEN_WIDTH;
    if (y2 > SCREEN_HEIGHT) y2 = SCREEN_HEIGHT;
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
    s32 cycleadd = 0;
    if (((absi(x1 - x2) % 4) == 0) && (a == 255)) {
        gDPSetCycleType( gDisplayListHead++, G_CYC_FILL);
        gDPSetRenderMode(gDisplayListHead++, G_RM_NOOP, G_RM_NOOP);
        cycleadd = 1;
    } else {
        gDPSetCycleType(gDisplayListHead++, G_CYC_1CYCLE);
        if (a == 255) {
            gDPSetRenderMode(gDisplayListHead++, G_RM_OPA_SURF, G_RM_OPA_SURF2);
        } else {
            gDPSetRenderMode(gDisplayListHead++, G_RM_CLD_SURF, G_RM_CLD_SURF2);
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
    if (x1 < 0) x1 = 0;
    if (y1 < 0) y1 = 0;
    if (x2 > SCREEN_WIDTH) x2 = SCREEN_WIDTH;
    if (y2 > SCREEN_HEIGHT) y2 = SCREEN_HEIGHT;
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
    s32 cycleadd = 0;
    gDPSetCycleType(gDisplayListHead++, G_CYC_1CYCLE);
    if (a == 255) {
        gDPSetRenderMode(gDisplayListHead++, G_RM_OPA_SURF, G_RM_OPA_SURF2);
    } else {
        gDPSetRenderMode(gDisplayListHead++, G_RM_CLD_SURF, G_RM_CLD_SURF2);
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

extern s32 text_iterate_command(const char *str, s32 i, s32 runCMD);
extern void get_char_from_byte(u8 letter, s32 *textX, s32 *textY, u8 *spaceX, s8 *offsetY, u8 font);

s8 shakeToggle = 0;
s8 waveToggle = 0;
s8 rainbowToggle = 0;
f32 textSize = 1.0f; // The value that's used as a baseline multiplier before applying text size modifiers. Make sure to set it back when you're done.
f32 textSizeTotal = 1.0f; // The value that's read to set the text size. Do not mess with this.
f32 textSizeTemp = 1.0f; // The value that's set when modifying text size mid draw. Also do not mess with this.

s32 get_text_width(const char *str, s32 font) {
    s32 i       = 0;
    s32 textPos = 0;
    s32 wideX   = 0;
    s32 textX, textY;
    s8 offsetY;
    u8 spaceX;
    s32 strLen = (signed)strlen(str);
    s32 commandOffset;

    textSizeTemp = 1.0f;
    textSizeTotal = textSizeTemp * textSize;

    for (i = 0; i < strLen; i++) {
        if (str[i] == '#' || str[i] == 0x0A) {
            textPos = 0;
            continue;
        }
        while (i < strLen && str[i] == '<') {
            commandOffset = text_iterate_command(str, i, FALSE);
            if (commandOffset == 0)
                break;

            i += commandOffset;
        }

        if (i >= strLen)
            break;

        get_char_from_byte(str[i], &textX, &textY, &spaceX, &offsetY, font);
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
    textPos = 12 * textSizeTotal;

    for (i = 0; i < strLen; i++) {
        if (str[i] == '#' || str[i] == 0x0A) {
            textPos += 12 * textSizeTotal;
            continue;
        }
        while (i < strLen && str[i] == '<') {
            commandOffset = text_iterate_command(str, i, FALSE);
            if (commandOffset == 0)
                break;

            i += commandOffset;
        }
    }

    return textPos;
}

s32 puppyprint_strlen(const char *str) {
    s32 i= 0;
    s32 strLen = (signed)strlen(str);
    s32 commandOffset;
    s32 len = 0;

    for (i = 0; i < strLen; i++, len++) {
        while (i < strLen && str[i] == '<') {
            commandOffset = text_iterate_command(str, i, FALSE);
            if (commandOffset == 0)
                break;

            i += commandOffset;
        }
    }

    return len;
}

const Gfx dl_small_text_begin[] = {
    gsDPPipeSync(),
    gsDPSetCycleType(    G_CYC_1CYCLE),
    gsDPSetTexturePersp( G_TP_NONE),
    gsDPSetCombineMode(  G_CC_FADEA, G_CC_FADEA),
    gsDPSetTextureFilter(G_TF_POINT),
    gsSPEndDisplayList(),
};

void print_small_text(s32 x, s32 y, const char *str, s32 align, s32 amount, u8 font) {
    s32 textX = 0;
    s32 textY = 0;
    s8 offsetY = 0;
    s32 textPos[2] = { 0, 0 };
    u8 spaceX = 0;
    s32 wideX[12] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    s32 tx = amount;
    s32 tx2 = tx;
    s8 shakePos[2];
    f32 wavePos;
    u8 lines = 0;
    u8 xlu = gCurrEnvCol[3];
    s32 prevxlu = 256; // Set out of bounds, so it will *always* be different at first.
    s32 strLen = puppyprint_strlen(str);
    s32 commandOffset;
    Texture *(*fontTex)[] = segmented_to_virtual(&puppyprint_font_lut);

    shakeToggle = 0;
    waveToggle = 0;
    rainbowToggle = 0;
    textSizeTemp = 1.0f;
    textSizeTotal = textSizeTemp * textSize;

    if (amount == PRINT_ALL || amount > strLen) {
        tx = strLen;
        tx2 = tx;
    }

    gSPDisplayList(gDisplayListHead++, dl_small_text_begin);
    if (align == PRINT_TEXT_ALIGN_CENTRE || align == PRINT_TEXT_ALIGN_RIGHT) {
        for (s32 i = 0; i < strLen; i++) {
            if (str[i] == '#' || str[i] == 0x0A) {
                textPos[0] = 0;
                lines++;
                wideX[lines] = 0;
                continue;
            }
            while (i < strLen && str[i] == '<') {
                commandOffset = text_iterate_command(str, i, FALSE);
                if (commandOffset == 0)
                    break;

                i += commandOffset;
                tx2 += commandOffset;
            }

            if (i >= strLen)
                break;

            get_char_from_byte(str[i], &textX, &textY, &spaceX, &offsetY, font);
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
    tx2 = tx;
    gDPLoadTextureBlock_4b(gDisplayListHead++, (*fontTex)[font], G_IM_FMT_I, 128, 60, (G_TX_NOMIRROR | G_TX_CLAMP), (G_TX_NOMIRROR | G_TX_CLAMP), 0, 0, 0, G_TX_NOLOD, G_TX_NOLOD);
    for (s32 i = 0, j = 0; i < tx2; i++, j++) {
        if (str[i] == '#' || str[i] == 0x0A) {
            lines++;
            if (align == PRINT_TEXT_ALIGN_RIGHT) {
                textPos[0] = -(wideX[lines]    );
            } else {
                textPos[0] = -(wideX[lines] / 2);
            }
            textPos[1] += 12 * textSizeTotal;
            continue;
        }

        while (i < tx2 && str[i] == '<') {
            commandOffset = text_iterate_command(str, i, TRUE);
            if (commandOffset == 0)
                break;

            i += commandOffset;
            tx2 += commandOffset;
        }

        if (i >= tx2)
            break;

        if (shakeToggle) {
            shakePos[0] = (-1 + (random_u16() & 0x1));
            shakePos[1] = (-1 + (random_u16() & 0x1));
        } else {
            shakePos[0] = 0;
            shakePos[1] = 0;
        }

        if (waveToggle) {
            wavePos = ((sins((gGlobalTimer * 3000) + (j * 10000))) * 2);
        } else {
            wavePos = 0;
        }

        get_char_from_byte(str[i], &textX, &textY, &spaceX, &offsetY, font);
        if (xlu != prevxlu) {
            prevxlu = xlu;
            if (xlu > 250) {
                gDPSetRenderMode(gDisplayListHead++, G_RM_TEX_EDGE, G_RM_TEX_EDGE2);
            } else {
                gDPSetRenderMode(gDisplayListHead++, G_RM_XLU_SURF, G_RM_XLU_SURF);
            }
        }

        gSPScisTextureRectangle(gDisplayListHead++, (x + textPos[0] + (s16)(shakePos[0] * textSizeTotal)) << 2,
                                                    (y + textPos[1] + (s16)((shakePos[1] + offsetY + wavePos) * textSizeTotal)) << 2,
                                                    (x + textPos[0] + (s16)((shakePos[0] + 8) * textSizeTotal)) << 2,
                                                    (y + textPos[1] + (s16)((wavePos + offsetY + shakePos[1] + 12) * textSizeTotal)) << 2,
                                                    G_TX_RENDERTILE, (textX << 6), (textY << 6), 1024/textSizeTotal, 1024/textSizeTotal);
        textPos[0] += (spaceX + 1) * textSizeTotal;
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
    if (strncmp((newStr), "<SIZE_xxx>", 6) == 0 && len == 10) { // Set the text size here. 100 is scale 1.0, with 001 being scale 0.01. this caps at 999. Going lower than 001
        // Will make the text unreadable on console, so only do it,
        textSizeTemp = (newStr[6] - '0');
        textSizeTemp += (newStr[7] - '0')/10.0f;
        textSizeTemp += (newStr[8] - '0')/100.0f;
        textSizeTemp = CLAMP(textSizeTemp, 0.01f, 10.0f);
        textSizeTotal = textSizeTemp * textSize;
    } else if (strncmp((newStr), "<COL_xxxxxxxx>", 5) == 0 && len == 14) { // Simple text colour effect. goes up to FF for each, so FF0000FF is red.
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
    } else if (strncmp((newStr), "<FADE_xxxxxxxx,xxxxxxxx,xx>", 6) == 0 && len == 27) { // Same as above, except it fades between two colours. The third set of numbers is the speed it fades.
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
    } else if (strncmp((newStr), "<RAINBOW>", 9) == 0) { // Toggles the happy colours :o) Do it again to disable it.
        if (!runCMD)
            return len;

        rainbowToggle ^= 1;
        if (rainbowToggle) {
            s32 r = (coss( gGlobalTimer * 600         ) + 1) * 127;
            s32 g = (coss((gGlobalTimer * 600) + 21845) + 1) * 127;
            s32 b = (coss((gGlobalTimer * 600) - 21845) + 1) * 127;
            gDPSetEnvColor(gDisplayListHead++, (Color) r, (Color) g, (Color) b, (Color) gCurrEnvCol[3]); // Don't use print_set_envcolour here, also opt to use alpha value from gCurrEnvCol
        } else {
            gDPSetEnvColor(gDisplayListHead++, (Color) gCurrEnvCol[0], (Color) gCurrEnvCol[1], (Color) gCurrEnvCol[2], (Color) gCurrEnvCol[3]); // Reset text to envcolor
        }
    } else if (strncmp((newStr), "<SHAKE>", 7) == 0) { // Toggles text that shakes on the spot. Do it again to disable it.
        if (!runCMD)
            return len;

        shakeToggle ^= 1;
    } else if (strncmp((newStr), "<WAVE>",  6) == 0) { // Toggles text that waves around. Do it again to disable it.
        if (!runCMD)
            return len;

        waveToggle  ^= 1;
    } else {
        return 0; // Invalid command string; display everything inside to make this clear to the user.
    }

    return len;
}

void get_char_from_byte(u8 letter, s32 *textX, s32 *textY, u8 *spaceX, s8 *offsetY, u8 font) {
    *offsetY = 0;
    u8 **textKern = segmented_to_virtual(puppyprint_kerning_lut);
    u8 *textLen = segmented_to_virtual(textKern[font]);

    if (letter >= '0' && letter <= '9') { // Line 1
        *textX = ((letter - '0') * 4);
        *textY = 0;
        *spaceX = textLen[(letter - '0') +  0];
    } else if (letter >= 'A' && letter <= 'P') { // Line 2
        *textX = ((letter - 'A') * 4);
        *textY = 6;
        *spaceX = textLen[(letter - 'A') + 16];
    } else if (letter >= 'Q' && letter <= 'Z') { // Line 3
        *textX = ((letter - 'Q') * 4);
        *textY = 12;
        *spaceX = textLen[(letter - 'Q') + 32];
    } else if (letter >= 'a' && letter <= 'p') { // Line 4
        *textX = ((letter - 'a') * 4);
        *textY = 18;
        *spaceX = textLen[(letter - 'a') + 48];
    } else if (letter >= 'q' && letter <= 'z') { // Line 5
        *textX = ((letter - 'q') * 4);
        *textY = 24;
        *spaceX = textLen[(letter - 'q') + 64];
    } else if (letter == '<' || letter == '>') {
        *textX  = 128;
        *textY  =  12;
        *spaceX =   0;
    }else { // Space, the final frontier.
        *textX  = 128;
        *textY  =  12;
        *spaceX =   2;
    }

    switch (letter) {
        case '-': *textX = 40; *textY =  0; *spaceX = textLen[10]; break; // Hyphen
        case '+': *textX = 44; *textY =  0; *spaceX = textLen[11]; break; // Plus
        case '(': *textX = 48; *textY =  0; *spaceX = textLen[12]; break; // Open Bracket
        case ')': *textX = 52; *textY =  0; *spaceX = textLen[13]; break; // Close Bracket
        case '!': *textX = 56; *textY =  0; *spaceX = textLen[14]; break; // Exclamation mark
        case '?': *textX = 60; *textY =  0; *spaceX = textLen[15]; break; // Question mark

        case '"': *textX = 40; *textY = 12; *spaceX = textLen[42]; break; // Speech mark
        case'\'': *textX = 44; *textY = 12; *spaceX = textLen[43]; break; // Apostrophe
        case ':': *textX = 48; *textY = 12; *spaceX = textLen[44]; break; // Colon
        case ';': *textX = 52; *textY = 12; *spaceX = textLen[45]; break; // Semicolon
        case '.': *textX = 56; *textY = 12; *spaceX = textLen[46]; break; // Full stop
        case ',': *textX = 60; *textY = 12; *spaceX = textLen[47]; break; // Comma

        case '~': *textX = 40; *textY = 24; *spaceX = textLen[74]; break; // Tilde
        case '@': *textX = 44; *textY = 24; *spaceX = textLen[75]; break; // Umlaut
        case '^': *textX = 48; *textY = 24; *spaceX = textLen[76]; break; // Caret
        case '/': *textX = 52; *textY = 24; *spaceX = textLen[77]; break; // Slash
        case '_': *textX = 56; *textY = 24; *spaceX = textLen[78]; break; // Percent
        case '&': *textX = 60; *textY = 24; *spaceX = textLen[79]; break; // Ampersand

        // This is for the letters that sit differently on the line. It just moves them down a bit.
        case 'g': *offsetY = 1; break;
        case 'q': *offsetY = 1; break;
        case 'p': if (font == FONT_DEFAULT) *offsetY = 3; break;
        case 'y': if (font == FONT_DEFAULT) *offsetY = 1; break;
    }
}
// This is where the deferred printing will be stored. When text is made, it will store text with an 12 byte header, then the rest will be the text data itself.
// The first 4 bytes of the header will be the X and Y pos
// The next 4 bytes will be the current envcolour set by print_set_envcolour
// Then the string length, text alignment, amount and font each get a byte.
// The data afterwards is the text data itself, using the string length byte to know when to stop.
void print_small_text_buffered(s32 x, s32 y, const char *str, u8 align, s32 amount, u8 font) {
    u8 strLen = MIN((signed)strlen(str), 255);
    // Compare the cursor position and the string length, plus 11 (header size) and return if it overflows.
    if (sPuppyprintTextBufferPos + strLen + 11 > sizeof(sPuppyprintTextBuffer))
        return;
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos] = ((x + 0x4000) >> 8) & 0xFF;
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 1] = ((x + 0x40) & 0xFF);
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 2] = ((y + 0x4000) >> 8) & 0xFF;
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 3] = ((y + 0x40) & 0xFF);
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 4] = gCurrEnvCol[0];
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 5] = gCurrEnvCol[1];
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 6] = gCurrEnvCol[2];
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 7] = gCurrEnvCol[3];
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 8] = strLen;
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 9] = align;
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 10] = (amount == -1) ? 255 : amount;
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 11] = font;
    bcopy(str, &sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 12], strLen);
    sPuppyprintTextBufferPos += strLen + 12;
}

void puppyprint_print_deferred(void) {
    if (sPuppyprintTextBufferPos == 0)
        return;
    bzero(&gCurrEnvCol, sizeof(ColorRGBA));
    print_set_envcolour(255, 255, 255, 255);
    for (u32 i = 0; i < sPuppyprintTextBufferPos;)
    {
        u8 length = sPuppyprintTextBuffer[i + 8];
        char *text = mem_pool_alloc(gEffectsMemoryPool, length);
        if (text == NULL) {
            print_small_text(160, 80, "gEffectsMemoryPool is full.", PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_OUTLINE);
            return;
        }
        s32 x = ((sPuppyprintTextBuffer[i] << 8) & 0xFF) - 0x4000;
        x += (sPuppyprintTextBuffer[i + 1] & 0xFF) - 0x40;
        s32 y = ((sPuppyprintTextBuffer[i + 2] << 8) & 0xFF) - 0x4000;
        y += (sPuppyprintTextBuffer[i + 3] & 0xFF) - 0x40;
        ColorRGBA originalEnvCol = {gCurrEnvCol[0], gCurrEnvCol[1], gCurrEnvCol[2], gCurrEnvCol[3]};
        print_set_envcolour(sPuppyprintTextBuffer[i + 4], sPuppyprintTextBuffer[i + 5], sPuppyprintTextBuffer[i + 6], sPuppyprintTextBuffer[i + 7]);
        u8 alignment = sPuppyprintTextBuffer[i + 9];
        u8 amount = (sPuppyprintTextBuffer[i + 10] == 255 ? -1 : amount);
        u8 font = sPuppyprintTextBuffer[i + 11];
        bcopy(&sPuppyprintTextBuffer[i + 12], text, length);
        print_small_text(x, y, text, alignment, amount, font);
        mem_pool_free(gEffectsMemoryPool, text);
        print_set_envcolour(originalEnvCol[0], originalEnvCol[1], originalEnvCol[2], originalEnvCol[3]);
        i+=length + 12;
    }
    //Reset the position back to zero, effectively clearing the buffer.
    sPuppyprintTextBufferPos = 0;
}

void render_multi_image(Texture *image, s32 x, s32 y, s32 width, s32 height, UNUSED s32 scaleX, UNUSED s32 scaleY, s32 mode) {
    s32 posW, posH, imW, imH, modeSC, mOne;
    s32 i     = 0;
    s32 num   = 256;
    s32 maskW = 1;
    s32 maskH = 1;

    if (mode == G_CYC_COPY) {
        gDPSetCycleType( gDisplayListHead++, mode);
        gDPSetRenderMode(gDisplayListHead++, G_RM_NOOP, G_RM_NOOP2);
        modeSC = 4;
        mOne   = 1;
    } else {
        gDPSetCycleType( gDisplayListHead++, mode);
        gDPSetRenderMode(gDisplayListHead++, G_RM_XLU_SURF, G_RM_XLU_SURF2);
        modeSC = 1;
        mOne   = 0;
    }

    // Find how best to seperate the horizontal. Keep going until it finds a whole value.
    while (TRUE) {
        f32 val = (f32)width / (f32)num;

        if ((s32)val == val && (s32) val >= 1) {
            imW = num;
            break;
        }
        num /= 2;
        if (num == 1) {
            print_text(32, 32, "IMAGE WIDTH FAILURE");
            return;
        }
    }
    // Find the tile height
    imH = 64 / (imW / 32); // This gets the vertical amount.

    num = 2;
    // Find the width mask
    while (TRUE) {
        if ((s32) num == imW) {
            break;
        }
        num *= 2;
        maskW++;
        if (maskW == 9) {
            print_text(32, 32, "WIDTH MASK FAILURE");
            return;
        }
    }
    num = 2;
    // Find the height mask
    while (TRUE) {
        if ((s32) num == imH) {
            break;
        }
        num *= 2;
        maskH++;
        if (maskH == 9) {
            print_text(32, 32, "HEIGHT MASK FAILURE");
            return;
        }
    }
    num = height;
    // Find the height remainder
    s32 peakH  = height - (height % imH);
    s32 cycles = (width * peakH) / (imW * imH);

    // Pass 1
    for (i = 0; i < cycles; i++) {
        posW = 0;
        posH = i * imH;
        while (posH >= peakH) {
            posW += imW;
            posH -= peakH;
        }

        gDPLoadSync(gDisplayListHead++);
        gDPLoadTextureTile(gDisplayListHead++,
            image, G_IM_FMT_RGBA, G_IM_SIZ_16b, width, height, posW, posH, ((posW + imW) - 1), ((posH + imH) - 1), 0, (G_TX_NOMIRROR | G_TX_WRAP), (G_TX_NOMIRROR | G_TX_WRAP), maskW, maskH, 0, 0);
        gSPScisTextureRectangle(gDisplayListHead++,
            ((x + posW) << 2),
            ((y + posH) << 2),
            (((x + posW + imW) - mOne) << 2),
            (((y + posH + imH) - mOne) << 2),
            G_TX_RENDERTILE, 0, 0, (modeSC << 10), (1 << 10));
    }
    // If there's a remainder on the vertical side, then it will cycle through that too.
    if (height-peakH != 0) {
        posW = 0;
        posH = peakH;
        for (i = 0; i < (width / imW); i++) {
            posW = i * imW;
            gDPLoadSync(gDisplayListHead++);
            gDPLoadTextureTile(gDisplayListHead++,
                image, G_IM_FMT_RGBA, G_IM_SIZ_16b, width, height, posW, posH, ((posW + imW) - 1), (height - 1), 0, (G_TX_NOMIRROR | G_TX_WRAP), (G_TX_NOMIRROR | G_TX_WRAP), maskW, maskH, 0, 0);
            gSPScisTextureRectangle(gDisplayListHead++,
                (x + posW) << 2,
                (y + posH) << 2,
                ((x + posW + imW) - mOne) << 2,
                ((y + posH + imH) - mOne) << 2,
                G_TX_RENDERTILE, 0, 0, modeSC << 10, 1 << 10);
        }
    }
}

#endif
