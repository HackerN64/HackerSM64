#pragma once

#include "segment2.h"

// This is how many indexes of timers are saved at once. higher creates a smoother average, but naturally uses more RAM. 15's fine.
// #define NUM_PERF_ITERATIONS   15
#define NUM_PERF_ITERATIONS   16
#define PERF_AGGREGATE NUM_PERF_ITERATIONS
#define PERF_TOTAL NUM_PERF_ITERATIONS + 1
#define LOG_BUFFER_SIZE       16
#define PUPPYPRINT_DEFERRED_BUFFER_SIZE 0x1000

#ifdef ENABLE_CREDITS_BENCHMARK
#undef NUM_PERF_ITERATIONS
#define NUM_PERF_ITERATIONS   60
#endif

struct PuppyPrintPage{
    void (*func)();
    char name[32];
};

enum Benchmark {
    BENCHMARK_NONE,
    BENCHMARK_GAME,
    BENCHMARK_AUDIO,
    BENCHMARK_GRAPHICS
};

enum PuppyprintTextAlign {
    PRINT_TEXT_ALIGN_LEFT   =  0,
    PRINT_TEXT_ALIGN_CENTRE =  1,
    PRINT_TEXT_ALIGN_CENTER =  1,
    PRINT_TEXT_ALIGN_RIGHT  =  2,
    PRINT_ALL               = -1,
};

enum rspFlags
{
    RSP_NONE,
    RSP_AUDIO_START,
    RSP_GFX_START,
    RSP_AUDIO_FINISHED,
    RSP_GFX_FINISHED,
    RSP_GFX_PAUSED,
    RSP_GFX_RESUME,
};

#if PUPPYPRINT_DEBUG
#if defined(BETTER_REVERB) && (defined(VERSION_US) || defined(VERSION_JP))
#define NUM_AUDIO_POOLS 7
#else
#define NUM_AUDIO_POOLS 6
#endif
#endif

enum PuppyFont {
    FONT_DEFAULT,
    FONT_OUTLINE,
    FONT_NUM,
};

extern u8 sPPDebugPage;
extern u8 gPuppyFont;
extern s8 perfIteration;
extern ColorRGBA gCurrEnvCol;
extern s32 ramsizeSegment[33];
extern const s8 nameTable;
extern s32 mempool;
extern f32 textSize;
extern u32 gPoolMem;
extern u32 gMiscMem;
typedef u32 PPTimer[NUM_PERF_ITERATIONS + 2];

struct PuppyPrintTimers
{
    u32 cpuTime; // Sum of multiple CPU timings, and what will be displayed.
    u32 rspTime; // Sum of multiple RSP timings, and publicly shamed on the street.
    u32 rdpTime; // Sum of multiple RDP timings, and hung by its entrails for all to see.
    u32 rspPauseTime; // Buffer that keeps track of the halt time of the Gfx task.
    u32 rspGfxBufTime; // Buffer that keeps track of the current Gfx task;
    u32 rspAudioBufTime; // Buffer that keeps track of the current Audio task;
    u32 threadsTime; // The combined processing time from thread 2, 3 and 6.
    PPTimer collisionTime; // Collision execution time.
    PPTimer behaviourTime; // Behaviour script execution time.
    PPTimer thread2Time; // Fault thread execution time.
    PPTimer thread3Time; // Task thread execution time.
    PPTimer thread4Time; // Audio thread execution time.
    PPTimer thread5Time; // Game thread execution time.
    PPTimer thread6Time; // Rumble thread execution time.
    PPTimer graphTime; // Graph Node processing time.
    PPTimer dmaTime; // thread 5 DMA time.
    PPTimer dmaAudioTime; // thread 4 DMA time.
    PPTimer cameraTime; // Camera behaviour.
    PPTimer profilerTime; // Profiler rendering time.
    PPTimer profilerTime2; // Profiler processing time.
    PPTimer controllerTime; // Controller polling time.
    PPTimer rspAudioTime; // RSP Audio processing time.
    PPTimer rspGfxTime; // RSP Graphics processing time.
    PPTimer rdpBufTime; // RDP buffer processing time.
    PPTimer rdpBusTime; // RDP pipe busy time.
    PPTimer rdpTmmTime; // RDP texture memory time.
};

extern struct PuppyPrintTimers gPuppyTimers;
extern void profiler_update(u32 *time, OSTime time2);
extern void puppyprint_profiler_process(void);
extern void puppyprint_render_profiler(void);
extern void puppyprint_profiler_finished(void);
extern void print_set_envcolour(u8 r, u8 g, u8 b, u8 a);
extern void prepare_blank_box(void);
extern void finish_blank_box(void);
extern void print_small_text(s32 x, s32 y, const char *str, s32 align, s32 amount, u8 font);
extern void render_multi_image(Texture *image, s32 x, s32 y, s32 width, s32 height, s32 scaleX, s32 scaleY, s32 mode);
extern s32  get_text_height(const char *str);
extern s32  get_text_width(const char *str, s32 font);
extern void prepare_blank_box(void);
extern void finish_blank_box(void);
extern void render_blank_box(s32 x1, s32 y1, s32 x2, s32 y2, u8 r, u8 g, u8 b, u8 a);
extern void render_blank_box_rounded(s32 x1, s32 y1, s32 x2, s32 y2, u8 r, u8 g, u8 b, u8 a);
extern void append_puppyprint_log(const char *str, ...);
extern char consoleLogTable[LOG_BUFFER_SIZE][255];
extern void profiler_offset(u32 *time, OSTime time2);
extern void puppyprint_update_rsp(u8 flags);
extern void profiler_add(u32 *time, OSTime time2);
extern void print_small_text_buffered(s32 x, s32 y, const char *str, u8 align, s32 amount, u8 font);
extern void puppyprint_print_deferred(void);
extern s32 puppyprint_strlen(const char *str);
extern void set_segment_memory_printout(u32 segment, u32 amount);
extern void print_small_text_light(s32 x, s32 y, const char *str, s32 align, s32 amount, u8 font);
extern void print_small_text_buffered_light(s32 x, s32 y, const char *str, u8 align, s32 amount, u8 font);
