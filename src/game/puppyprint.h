#pragma once

#include "profiling.h"

// This is how many indexes of timers are saved at once. higher creates a smoother average, but naturally uses more RAM. 32's fine.
#define NUM_PERF_ITERATIONS   32
#define PERF_AGGREGATE NUM_PERF_ITERATIONS
#define PERF_TOTAL NUM_PERF_ITERATIONS + 1
#define LOG_BUFFER_SIZE       16
#define PUPPYPRINT_DEFERRED_BUFFER_SIZE 0x1000

#ifdef PUPPYPRINT_DEBUG
#define PUPPYPRINT_ADD_COUNTER(x) x++
#define PUPPYPRINT_GET_SNAPSHOT() u32 first = osGetCount()
#define PUPPYPRINT_GET_SNAPSHOT_TYPE(type) u32 first = profiler_get_delta(type)
void append_puppyprint_log(const char *str, ...);
#else
#define PUPPYPRINT_ADD_COUNTER(x)
#define PUPPYPRINT_GET_SNAPSHOT()
#define PUPPYPRINT_GET_SNAPSHOT_TYPE(type)
#define append_puppyprint_log(...)
#endif


#ifdef PUPPYPRINT_DEBUG_CYCLES
    #define PP_CYCLE_CONV(x) (x)
    #define PP_CYCLE_STRING " cycles"
#else
    #define PP_CYCLE_CONV(x) OS_CYCLES_TO_USEC(x)
    #define PP_CYCLE_STRING "us"
#endif

struct PPTextFont {
    Texture *tex; // Pointer to the texture for the font
    u8 *kern; // Pointer to the kerning table
    u16 *offset; // Pointer to the character offset table
    u8 *pal; // Pointer to the texture pallete. Coming
    u8 fmt; // Texture format, same as F3D macros
    u8 siz; // Texture size, same as F3D macros
    u16 imW; // Texture width
    u8 imH; // Texture height
    u8 txW; // Individual character width
    u8 txH; // Individual character height
};

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
    PUPPYPRINT_PAGE_COVERAGE,
#ifdef PUPPYCAM
    PUPPYPRINT_PAGE_CAMERA,
#endif
#ifdef BETTER_REVERB
    PUPPYPRINT_PAGE_BETTER_REVERB,
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

#ifdef PUPPYPRINT_DEBUG
extern u8 sDebugMenu;
extern u8 sPPDebugPage;
#endif
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
extern s32 print_set_envcolour(u8 r, u8 g, u8 b, u8 a);
extern void prepare_blank_box(void);
extern void finish_blank_box(void);
extern void print_small_text(s32 x, s32 y, const char *str, s32 align, s32 amount, u8 font);
extern s32  get_text_height(const char *str);
extern s32  get_text_width(const char *str, s32 font);
extern void prepare_blank_box(void);
extern void finish_blank_box(void);
extern void render_blank_box(s32 x1, s32 y1, s32 x2, s32 y2, u8 r, u8 g, u8 b, u8 a);
extern void render_blank_box_rounded(s32 x1, s32 y1, s32 x2, s32 y2, u8 r, u8 g, u8 b, u8 a);
extern char consoleLogTable[LOG_BUFFER_SIZE][255];
extern void print_small_text_buffered(s32 x, s32 y, const char *str, u8 align, s32 amount, u8 font);
extern void puppyprint_print_deferred(void);
extern s32 puppyprint_strlen(const char *str);
extern void set_segment_memory_printout(u32 segment, u32 amount);
extern void print_small_text_light(s32 x, s32 y, const char *str, s32 align, s32 amount, u8 font);
extern void print_small_text_buffered_light(s32 x, s32 y, const char *str, u8 align, s32 amount, u8 font);
void puppyprint_profiler_process(void);
s32 text_iterate_command(const char *str, s32 i, s32 runCMD);
void get_char_from_byte(s32 *textX, s32 *textPos, u8 letter, u8 *wideX, u8 *spaceX, s8 *offsetY, u8 font);
