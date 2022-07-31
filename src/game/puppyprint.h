#pragma once

#include "segment2.h"
#include "profiling.h"

// This is how many indexes of timers are saved at once. higher creates a smoother average, but naturally uses more RAM. 15's fine.
// #define NUM_PERF_ITERATIONS   15
#define NUM_PERF_ITERATIONS   32
#define PERF_AGGREGATE NUM_PERF_ITERATIONS
#define PERF_TOTAL NUM_PERF_ITERATIONS + 1
#define LOG_BUFFER_SIZE       16
#define PUPPYPRINT_DEFERRED_BUFFER_SIZE 0x1000

struct CallCounter {
    u16 collision_floor;
    u16 collision_wall;
    u16 collision_ceil;
    u16 collision_water;
    u16 collision_raycast;
    u16 matrix;
};

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

enum PPPages {
#ifdef USE_PROFILER
    PUPPYPRINT_PAGE_PROFILER,
    PUPPYPRINT_PAGE_MINIMAL,
#endif
    PUPPYPRINT_PAGE_GENERAL,
    PUPPYPRINT_PAGE_AUDIO,
    PUPPYPRINT_PAGE_RAM,
    PUPPYPRINT_PAGE_COLLISION,
    PUPPYPRINT_PAGE_LOG,
    PUPPYPRINT_PAGE_LEVEL_SELECT,
#ifdef PUPPYCAM
    PUPPYPRINT_PAGE_CAMERA
#endif
};

#ifdef PUPPYPRINT_DEBUG
#ifdef BETTER_REVERB
#define NUM_AUDIO_POOLS 7
#else
#define NUM_AUDIO_POOLS 6
#endif
#endif

enum PuppyFont {
    FONT_DEFAULT,
    FONT_OUTLINE,
    FONT_PLAIN,
    FONT_VANILLA,
    FONT_NUM
};

extern u8 sPPDebugPage;
extern u8 gPuppyFont;
extern ColorRGBA gCurrEnvCol;
extern s32 ramsizeSegment[33];
extern const s8 nameTable;
extern s32 mempool;
extern f32 textSize;
extern u32 gPoolMem;
extern u32 gMiscMem;
extern u8 gPuppyWarp;
extern u8 gPuppyWarpArea;
extern u8 gLastWarpID;
extern struct CallCounter gPuppyCallCounter;

extern void puppyprint_render_profiler(void);
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
extern void print_small_text_buffered(s32 x, s32 y, const char *str, u8 align, s32 amount, u8 font);
extern void puppyprint_print_deferred(void);
extern s32 puppyprint_strlen(const char *str);
extern void set_segment_memory_printout(u32 segment, u32 amount);
extern void print_small_text_light(s32 x, s32 y, const char *str, s32 align, s32 amount, u8 font);
extern void print_small_text_buffered_light(s32 x, s32 y, const char *str, u8 align, s32 amount, u8 font);
void puppyprint_profiler_process(void);
