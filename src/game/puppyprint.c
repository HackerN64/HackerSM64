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
#include "profiling.h"
#include "segment_symbols.h"

#ifdef PUPPYPRINT

#ifdef ENABLE_CREDITS_BENCHMARK
u8 fDebug = TRUE;
#else
u8 fDebug = FALSE;
#endif
u8 sPuppyprintTextBuffer[PUPPYPRINT_DEFERRED_BUFFER_SIZE];
u32 sPuppyprintTextBufferPos; // Location in the buffer of puppyprint deferred text.
ColorRGBA gCurrEnvCol;

#ifdef PUPPYPRINT_DEBUG

#define GENERAL_PAGE_TEXT_LENGTH 200

s8 logViewer    = FALSE;
u8 sPPDebugPage = 0;
u8 sDebugMenu   = FALSE;
u8 sDebugOption = 0;
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
struct CallCounter gPuppyCallCounter;

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

void puppyprint_calculate_ram_usage(void) {
    ramsizeSegment[RAM_BUFFERS] = (u32)&_buffersSegmentBssEnd - (u32)&_buffersSegmentBssStart - sizeof(gAudioHeap);
    ramsizeSegment[RAM_MAIN] = (u32)&_mainSegmentEnd - (u32)&_mainSegmentStart;
    ramsizeSegment[RAM_ENGINE] = (u32)&_engineSegmentEnd - (u32)&_engineSegmentStart;
    ramsizeSegment[RAM_FRAMEBUFFERS] = (u32)&_framebuffersSegmentBssEnd - (u32)&_framebuffersSegmentBssStart;
    ramsizeSegment[RAM_ZBUFFER] = (u32)&_zbufferSegmentBssEnd - (u32)&_zbufferSegmentBssStart;
    ramsizeSegment[RAM_GODDARD] = (u32)&_goddardSegmentEnd - (u32)&_goddardSegmentStart;
    ramsizeSegment[RAM_POOLS] = gPoolMem;
    ramsizeSegment[RAM_COLLISION] = ((u32) gCurrStaticSurfacePoolEnd - (u32) gCurrStaticSurfacePool) + ((u32) gDynamicSurfacePoolEnd - (u32) gDynamicSurfacePool);
    ramsizeSegment[RAM_MISC] = gMiscMem;
    ramsizeSegment[RAM_AUDIO] = gAudioHeapSize + gAudioInitPoolSize;
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
    for (u8 i = 0; i < 32; i++) {
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

const char *audioPoolNames[NUM_AUDIO_POOLS] = {
    "Audio Init Pool",
    "Notes And Buffers Pool",
    "Persistent Sequence Pool",
    "Persistent Bank Pool",
    "Temporary Sequence Pool",
    "Temporary Bank Pool",
#ifdef BETTER_REVERB
    "Better Reverb Pool",
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
    render_blank_box(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, 64, 64, 64, 168);
    finish_blank_box();

    puppyprint_get_allocated_pools(audioPoolSizes[0]);

    y += 24;
    for (u8 i = 0; i < NUM_AUDIO_POOLS; i++) {
        if (audioPoolSizes[i][0] == 0) {
            percentage = 1000;
        } else {
            percentage = (((s64) audioPoolSizes[i][1] * 1000) / audioPoolSizes[i][0]);
        }

        sprintf(textBytes, "%s: %X / %X (%d.%d%%)", audioPoolNames[i],
                audioPoolSizes[i][1],
                audioPoolSizes[i][0],
                percentage / 10,
                percentage % 10);

        print_set_envcolour(colourChart[i][0],
                            colourChart[i][1],
                            colourChart[i][2], 255);
        print_small_text_light(x, y, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);

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
        sprintf(textBytes, "TOTAL AUDIO MEMORY: %X / %X (%d.%d%%)",
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
    print_small_text_light(x, tmpY, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
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
    sprintf(textBytes, "Static Pool Size: 0x%X#Dynamic Pool Size: 0x%X#Dynamic Pool Used: 0x%X#Surfaces Allocated: %d#Nodes Allocated: %d", 
    gTotalStaticSurfaceData,
    DYNAMIC_SURFACE_POOL_SIZE,
    (uintptr_t)gDynamicSurfacePoolEnd - (uintptr_t)gDynamicSurfacePool,
    gSurfacesAllocated, gSurfaceNodesAllocated);
    print_small_text_light(SCREEN_WIDTH-16, 60, textBytes, PRINT_TEXT_ALIGN_RIGHT, PRINT_ALL, 1);

#ifdef VISUAL_DEBUG
    print_small_text_light(160, (SCREEN_HEIGHT - 42), "Use the dpad to toggle visual collision modes", PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, 1);
    switch (viewCycle) {
        case 0: print_small_text_light(160, (SCREEN_HEIGHT - 32), "Current view: None",                  PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, 1); break;
        case 1: print_small_text_light(160, (SCREEN_HEIGHT - 32), "Current view: Hitboxes",              PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, 1); break;
        case 2: print_small_text_light(160, (SCREEN_HEIGHT - 32), "Current view: Surfaces",              PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, 1); break;
        case 3: print_small_text_light(160, (SCREEN_HEIGHT - 32), "Current view: Hitboxes and Surfaces", PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, 1); break;
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
    sprintf(textBytes, "CPU: %dus (%d%%)#RSP: %dus (%d%%)#RDP: %dus (%d%%)",
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
        print_small_text_light(16, 140, textBytes, PRINT_TEXT_ALIGN_LEFT, PRINT_ALL, FONT_OUTLINE);
    }
    // Same for the camera, especially so because this will crash otherwise.
    if (gCamera) {
        sprintf(textBytes, "Camera Pos#X: %d#Y: %d#Z: %d#D: %X",
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

static u8 sLevelSelectOption = 0;
static u8 sLevelSelectOptionArea = 0;
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
    print_small_text_light(SCREEN_WIDTH/2, (SCREEN_HEIGHT/2) + 64, "(Area must have warp node of 0x0A)\nDpad Left: Warp / Dpad Right: Area\nYellow is current level.", PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_OUTLINE);
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
        if (sLevelSelectOption == i) {
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
    {&puppyprint_render_standard, "Profiler"},
    {&puppyprint_render_minimal, "Minimal"},
#endif
    {&puppyprint_render_general_vars, "General"},
    {&print_audio_ram_overview, "Audio"},
    {&print_ram_overview, "Segments"},
    {&puppyprint_render_collision, "Collision"},
    {&print_console_log, "Log"},
    {&puppyprint_level_select_menu, "Level Select"      },
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

            print_small_text_light((28 + (MENU_BOX_WIDTH / 2)), posY, ppPages[i].name, PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, 0);
        }
    }
}

void puppyprint_render_profiler(void) {
    u32 first = osGetCount();

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
    u32 first = osGetCount();

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
                while (sLevelNames[sLevelSelectOption][0] == 0 && sLevelSelectOption < LEVEL_COUNT) {
                    sLevelSelectOption--;
                }
            }
            if (gPlayer1Controller->buttonPressed & D_JPAD) {
                sLevelSelectOption++;
                while (sLevelNames[sLevelSelectOption][0] == 0 && sLevelSelectOption < LEVEL_COUNT) {
                    sLevelSelectOption++;
                }
            }
            if (sLevelSelectOption == 255) {
                sLevelSelectOption = (sizeof(sLevelNames)/32)-1;
                while (sLevelNames[sLevelSelectOption][0] == 0 && sLevelSelectOption < LEVEL_COUNT) {
                    sLevelSelectOption--;
                }
            }
            if (sLevelSelectOption >= (sizeof(sLevelNames)/32)) {
                while (sLevelNames[sLevelSelectOption][0] == 0 && sLevelSelectOption < LEVEL_COUNT) {
                    sLevelSelectOption++;
                }
                sLevelSelectOption = 0;
            }
            if (gPlayer1Controller->buttonPressed & R_JPAD) {
                sLevelSelectOptionArea++;
                if (sLevelSelectOptionArea > AREA_COUNT - 1) {
                    sLevelSelectOptionArea = 0;
                }
            }
            if (gPlayer1Controller->buttonPressed & L_JPAD) {
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
    }
    profiler_update(PROFILER_TIME_PUPPYPRINT2, osGetCount() - first);
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
    gDPPipeSync(gDisplayListHead++);
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
    gDPPipeSync(gDisplayListHead++);
    if (((absi(x1 - x2) % 4) == 0) && (a == 255)) {
        gDPSetCycleType( gDisplayListHead++, G_CYC_FILL);
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

extern s32 text_iterate_command(const char *str, s32 i, s32 runCMD);
extern void get_char_from_byte(u8 letter, s32 *textX, u8 *spaceX, s8 *offsetY, u8 font);

s8 shakeToggle = 0;
s8 waveToggle = 0;
s8 rainbowToggle = 0;
f32 textSize = 1.0f; // The value that's used as a baseline multiplier before applying text size modifiers. Make sure to set it back when you're done.
f32 textSizeTotal = 1.0f; // The value that's read to set the text size. Do not mess with this.
f32 textSizeTemp = 1.0f; // The value that's set when modifying text size mid draw. Also do not mess with this.
u16 textTempScale = 1024; // A fixed point means of referring to scale.
u8 textOffsets[2]; // Represents the dimensions of the text (12 x 8), and written to when size is modified.
u8 topLineHeight; // Represents the peak line height of the current line. Prevents vertical overlapping.

s32 get_text_width(const char *str, s32 font) {
    s32 i       = 0;
    s32 textPos = 0;
    s32 wideX   = 0;
    s32 textX;
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

        get_char_from_byte(str[i], &textX, &spaceX, &offsetY, font);
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
        if (str[i] == '#' || str[i] == 0x0A) {
            textPos += topLineHeight;
            topLineHeight = 12 * textSizeTotal;
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
    Texture *(*fontTex)[] = segmented_to_virtual(&puppyprint_font_lut);
    s8 offsetY = 0;
    u8 spaceX = 0;
    u8 lines = 0;
    u8 xlu = gCurrEnvCol[3];
    u8 shakeTablePos = 0;

    shakeToggle = 0;
    waveToggle = 0;
    rainbowToggle = 0;
    textSizeTemp = 1.0f;
    set_text_size_params();

    if (amount == PRINT_ALL || amount > strLen) {
        textLength = strLen;
    }

    // Calculate the text width for centre and right aligned text.
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
            }

            if (i >= strLen)
                break;

            get_char_from_byte(str[i], &textX, &spaceX, &offsetY, font);
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
    gDPLoadTextureBlock_4b(gDisplayListHead++, (*fontTex)[font], G_IM_FMT_I, 672, 12, (G_TX_NOMIRROR | G_TX_CLAMP), (G_TX_NOMIRROR | G_TX_CLAMP), 0, 0, 0, G_TX_NOLOD, G_TX_NOLOD);
    
    for (s32 i = 0, j = 0; i < textLength; i++, j++) {
        if (str[i] == '#' || str[i] == 0x0A) {
            lines++;
            if (align == PRINT_TEXT_ALIGN_RIGHT) {
                textPos[0] = -(wideX[lines]);
            } else {
                textPos[0] = -(wideX[lines] / 2);
            }
            textPos[1] += topLineHeight;
            topLineHeight = 12.0f * textSizeTotal;
            continue;
        }

        while (i < textLength && str[i] == '<') {
            commandOffset = text_iterate_command(str, i, TRUE);
            if (commandOffset == 0)
                break;

            i += commandOffset;
        }

        if (i >= textLength)
            break;

        if (shakeToggle) {
            shakePos[0] = sTextShakeTable[shakeTablePos++] * textSizeTotal;
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

        get_char_from_byte(str[i], &textX, &spaceX, &offsetY, font);
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
                                                    (x + textPos[0] + (s16)((shakePos[0] + textOffsets[0]))) << 2,
                                                    (y + textPos[1] + (s16)((wavePos + offsetY + shakePos[1] + textOffsets[1]))) << 2,
                                                    G_TX_RENDERTILE, (textX << 6), 0, textTempScale, textTempScale);
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
    Texture *(*fontTex)[] = segmented_to_virtual(&puppyprint_font_lut);
    s8 offsetY = 0;
    u8 spaceX = 0;
    u8 lines = 0;
    u8 xlu = gCurrEnvCol[3];

    if (amount == PRINT_ALL || amount > strLen) {
        textLength = strLen;
    }

    // Calculate the text width for centre and right aligned text.
    gSPDisplayList(gDisplayListHead++, dl_small_text_begin);
    if (align == PRINT_TEXT_ALIGN_CENTRE || align == PRINT_TEXT_ALIGN_RIGHT) {
        for (s32 i = 0; i < strLen; i++) {
            if (str[i] == '#' || str[i] == 0x0A) {
                textPos[0] = 0;
                lines++;
                wideX[lines] = 0;
                continue;
            }

            get_char_from_byte(str[i], &textX, &spaceX, &offsetY, font);
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
    gDPLoadTextureBlock_4b(gDisplayListHead++, (*fontTex)[font], G_IM_FMT_I, 672, 12, (G_TX_NOMIRROR | G_TX_CLAMP), (G_TX_NOMIRROR | G_TX_CLAMP), 0, 0, 0, G_TX_NOLOD, G_TX_NOLOD);
    
    for (s32 i = 0, j = 0; i < textLength; i++, j++) {
        if (str[i] == '#' || str[i] == 0x0A) {
            lines++;
            if (align == PRINT_TEXT_ALIGN_RIGHT) {
                textPos[0] = -(wideX[lines]);
            } else {
                textPos[0] = -(wideX[lines] / 2);
            }
            textPos[1] += 12;
            continue;
        }

        get_char_from_byte(str[i], &textX, &spaceX, &offsetY, font);
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
                                                    (x + textPos[0] + 8) << 2,
                                                    (y + textPos[1] + offsetY + 12) << 2,
                                                    G_TX_RENDERTILE, (textX << 6), 0, 1024, 1024);
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
        //if (!runCMD)
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
            s32 g = (coss((gGlobalTimer * 600) + 21845) + 1) * 127;
            s32 b = (coss((gGlobalTimer * 600) - 21845) + 1) * 127;
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

void font_offsets(s8 *offsetY, s32 font, u8 letter) {
    switch (font) {
        case FONT_DEFAULT:
            switch (letter) {
                // This is for the letters that sit differently on the line. It just moves them down a bit.
                case 'g': *offsetY = 1 * textSizeTotal; break;
                case 'q': *offsetY = 1 * textSizeTotal; break;
                case 'p': *offsetY = 1 * textSizeTotal; break;
                case 'y': *offsetY = 1 * textSizeTotal; break;
            }
        break;
        case FONT_PLAIN:
            switch (letter) {
                // This is for the letters that sit differently on the line. It just moves them down a bit.
                case 'g': *offsetY = 3 * textSizeTotal; break;
                case 'q': *offsetY = 3 * textSizeTotal; break;
                case 'p': *offsetY = 3 * textSizeTotal; break;
                case 'y': *offsetY = 2 * textSizeTotal; break;
            }
        break;
        case FONT_VANILLA:
            switch (letter) {
                // This is for the letters that sit differently on the line. It just moves them down a bit.
                case 'g': *offsetY = 2 * textSizeTotal; break;
                case 'q': *offsetY = 2 * textSizeTotal; break;
                case 'p': *offsetY = 3 * textSizeTotal; break;
                case 'y': *offsetY = 3 * textSizeTotal; break;
            }
        break;
    }
}

void get_char_from_byte(u8 letter, s32 *textX, u8 *spaceX, s8 *offsetY, u8 font) {
    *offsetY = 0;
    u32 let = letter - '!';
    u8 **textKern = segmented_to_virtual(puppyprint_kerning_lut);
    u8 *textLen = segmented_to_virtual(textKern[font]);

    if (letter != ' ') {
        if (letter > 'z') {
            let -= (3 + 2 + 3 + 1 + 3);
        } else if (letter > '^') {
            let -= (2 + 3 + 1 + 3);
        } else if (letter > 'Z') {
            let -= (3 + 1 + 3);
        } else if (letter > '?') {
            let -= (1 + 3);
        } else if (letter > ';') {
            let -= (3);
        }

        *textX = (let) * 4;
        *spaceX = textLen[let];

        if (font != FONT_OUTLINE) {
            font_offsets(offsetY, font, letter);
        }
    } else {
        *textX = -12;
        *spaceX = 2;
    }
}

static u8 gIsLightText = FALSE;

// This is where the deferred printing will be stored. When text is made, it will store text with an 12 byte header, then the rest will be the text data itself.
// The first 4 bytes of the header will be the X and Y pos
// The next 4 bytes will be the current envcolour set by print_set_envcolour
// Then the string length, text alignment, amount and font each get a byte.
// The data afterwards is the text data itself, using the string length byte to know when to stop.
#define HEADERSIZE 13
void print_small_text_buffered(s32 x, s32 y, const char *str, u8 align, s32 amount, u8 font) {
    u8 strLen = MIN((signed)strlen(str), 255);
    // Compare the cursor position and the string length, plus 12 (header size) and return if it overflows.
    if (sPuppyprintTextBufferPos + strLen + HEADERSIZE > sizeof(sPuppyprintTextBuffer))
        return;
    x += 0x8000;
    y += 0x8000;
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 0] = (x >> 8) & 0xFF;
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 1] = (x & 0xFF);
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 2] = (y >> 8) & 0xFF;
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 3] = (y & 0xFF);
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 4] = gCurrEnvCol[0];
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 5] = gCurrEnvCol[1];
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 6] = gCurrEnvCol[2];
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 7] = gCurrEnvCol[3];
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 8] = strLen;
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 9] = align;
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 10] = (amount == -1) ? 255 : amount;
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 11] = font;
    sPuppyprintTextBuffer[sPuppyprintTextBufferPos + 12] = gIsLightText;
    bcopy(str, &sPuppyprintTextBuffer[sPuppyprintTextBufferPos + HEADERSIZE], strLen);
    sPuppyprintTextBufferPos += strLen + HEADERSIZE;
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
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, 255);
    for (u32 i = 0; i < sPuppyprintTextBufferPos;)
    {
        u8 length = sPuppyprintTextBuffer[i + 8];
        char *text = mem_pool_alloc(gEffectsMemoryPool, length);
        if (text == NULL) {
            print_small_text_light(160, 80, "gEffectsMemoryPool is full.", PRINT_TEXT_ALIGN_CENTRE, PRINT_ALL, FONT_OUTLINE);
            return;
        }
        s32 x = ((sPuppyprintTextBuffer[i] & 0xFF) << 8);
        x += (sPuppyprintTextBuffer[i + 1] & 0xFF);
        s32 y = ((sPuppyprintTextBuffer[i + 2] & 0xFF) <<  8);
        y += (sPuppyprintTextBuffer[i + 3] & 0xFF);
        x -= 0x8000;
        y -= 0x8000;
        ColorRGBA originalEnvCol = {gCurrEnvCol[0], gCurrEnvCol[1], gCurrEnvCol[2], gCurrEnvCol[3]};
        print_set_envcolour(sPuppyprintTextBuffer[i + 4], sPuppyprintTextBuffer[i + 5], sPuppyprintTextBuffer[i + 6], sPuppyprintTextBuffer[i + 7]);
        u8 alignment = sPuppyprintTextBuffer[i + 9];
        u8 amount = (sPuppyprintTextBuffer[i + 10] == 255 ? -1 : amount);
        u8 font = sPuppyprintTextBuffer[i + 11];
        bcopy(&sPuppyprintTextBuffer[i + HEADERSIZE], text, length);
        if (sPuppyprintTextBuffer[i + 12]) {
            print_small_text_light(x, y, text, alignment, amount, font);
        } else {
            print_small_text(x, y, text, alignment, amount, font);
        }
        mem_pool_free(gEffectsMemoryPool, text);
        print_set_envcolour(originalEnvCol[0], originalEnvCol[1], originalEnvCol[2], originalEnvCol[3]);
        i+=length + HEADERSIZE;
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
