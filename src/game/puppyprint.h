#ifndef PUPPYPRINT_H
#define PUPPYPRINT_H

#ifdef PUPPYPRINT

//This is how many indexes of timers are saved at once. higher creates a smoother average, but naturally uses more RAM. 15's fine.
#define NUM_PERF_ITERATIONS 15
#define NUM_BENCH_ITERATIONS 150
#define LOG_BUFFER_SIZE 16

#define BENCHMARK_GAME 1
#define BENCHMARK_AUDIO 2
#define BENCHMARK_GRAPHICS 3

#define PRINT_TEXT_ALIGN_LEFT 0
#define PRINT_TEXT_ALIGN_CENTRE 1
#define PRINT_TEXT_ALIGN_RIGHT 2
#define PRINT_ALL -1

extern Texture small_font[];
extern s8 perfIteration;
extern s16 benchmarkLoop;
extern s32 benchmarkTimer;
extern u8 currEnv[4];
extern s32 ramsizeSegment[33];
extern s32 audioPool[12];
extern s8 nameTable;
extern s32 mempool;
extern u8 benchOption;

//General
extern OSTime cpuTime;
extern OSTime rspTime;
extern OSTime rdpTime;
extern OSTime ramTime;
extern OSTime loadTime;
extern OSTime rspDelta;
extern s32 benchMark[NUM_BENCH_ITERATIONS+2];

//CPU
extern OSTime collisionTime[NUM_PERF_ITERATIONS+1];
extern OSTime behaviourTime[NUM_PERF_ITERATIONS+1];
extern OSTime scriptTime[NUM_PERF_ITERATIONS+1];
extern OSTime graphTime[NUM_PERF_ITERATIONS+1];
extern OSTime audioTime[NUM_PERF_ITERATIONS+1];
extern OSTime dmaTime[NUM_PERF_ITERATIONS+1];
extern OSTime dmaAudioTime[NUM_PERF_ITERATIONS+1];
extern OSTime faultTime[NUM_PERF_ITERATIONS+1];
extern OSTime taskTime[NUM_PERF_ITERATIONS+1];
extern OSTime profilerTime[NUM_PERF_ITERATIONS+1];
//RSP
extern OSTime rspGenTime[NUM_PERF_ITERATIONS+1];
//RDP
extern OSTime bufferTime[NUM_PERF_ITERATIONS+1];
extern OSTime tmemTime[NUM_PERF_ITERATIONS+1];
extern OSTime busTime[NUM_PERF_ITERATIONS+1];

extern void profiler_update(OSTime *time, OSTime time2);
extern void puppyprint_profiler_process(void);
extern void puppyprint_render_profiler(void);
extern void puppyprint_profiler_finished(void);
extern void print_set_envcolour(s32 r, s32 g, s32 b, s32 a);
extern void prepare_blank_box(void);
extern void finish_blank_box(void);
extern void render_blank_box(s16 x1, s16 y1, s16 x2, s16 y2, u8 r, u8 g, u8 b, u8 a);
extern void print_small_text(s32 x, s32 y, const char *str, s32 align, s32 amount);
extern void render_multi_image(Texture *image, s32 x, s32 y, s32 width, s32 height, s32 scaleX, s32 scaleY, s32 mode);
extern s32 get_text_height(const char *str);
extern s32 get_text_width(const char *str);
extern void prepare_blank_box(void);
extern void finish_blank_box(void);
extern void render_blank_box(s16 x1, s16 y1, s16 x2, s16 y2, u8 r, u8 g, u8 b, u8 a);
extern void append_puppyprint_log(const char *str, ...);
extern char consoleLogTable[LOG_BUFFER_SIZE][255];

#endif

#endif // PUPPYPRINT_H
