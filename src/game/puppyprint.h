#pragma once

#include "segment2.h"

#define NUM_BENCH_ITERATIONS 150
#define LOG_BUFFER_SIZE       16

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
    PRINT_TEXT_ALIGN_RIGHT  =  2,
    PRINT_ALL               = -1,
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
extern s16 benchmarkLoop;
extern s32 benchmarkTimer;
extern ColorRGBA currEnv;
extern s32 ramsizeSegment[33];
extern s8 nameTable;
extern s32 mempool;
extern u8 benchOption;

// General
extern s32 benchMark[NUM_BENCH_ITERATIONS + 2];

extern void puppyprint_profiler_process(void);
extern void puppyprint_render_profiler(void);
extern void puppyprint_profiler_finished(void);
extern void print_set_envcolour(s32 r, s32 g, s32 b, s32 a);
extern void prepare_blank_box(void);
extern void finish_blank_box(void);
extern void print_small_text(s32 x, s32 y, const char *str, s32 align, s32 amount, s32 font);
extern void render_multi_image(Texture *image, s32 x, s32 y, s32 width, s32 height, s32 scaleX, s32 scaleY, s32 mode);
extern s32  get_text_height(const char *str);
extern s32  get_text_width(const char *str, s32 font);
extern void prepare_blank_box(void);
extern void finish_blank_box(void);
extern void render_blank_box(s32 x1, s32 y1, s32 x2, s32 y2, s32 r, s32 g, s32 b, s32 a);
extern void append_puppyprint_log(const char *str, ...);
extern char consoleLogTable[LOG_BUFFER_SIZE][255];
