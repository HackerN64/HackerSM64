#ifndef MAIN_H
#define MAIN_H

#include "config.h"

enum VIModes {
    MODE_NTSC,
    MODE_MPAL,
    MODE_PAL,
};

#define THREAD1_STACK 0x100
#define THREAD2_STACK 0x800
#define THREAD3_STACK 0x200
#define THREAD4_STACK 0x2000
#define THREAD5_STACK 0x2000
#define THREAD6_STACK 0x400

enum ThreadID {
    THREAD_0,
    THREAD_1_IDLE,
    THREAD_2_CRASH_SCREEN,
    THREAD_3_MAIN,
    THREAD_4_SOUND,
    THREAD_5_GAME_LOOP,
    THREAD_6_RUMBLE
};

struct RumbleData {
    u8  comm;
    u8  level;
    s16 time;
    s16 decay;
};

struct RumbleSettings {
    s16 event;
    s16 level;
    s16 timer;
    s16 count;
    s16 start;
    s16 slip;
    s16 vibrate;
    s16 decay;
};

#ifdef DEBUG_F3DEX3_PROFILER
typedef struct {  /* Default performance counters, if no CFG_PROFILING_* is enabled */
    /* Number of vertices processed by the RSP */
    u16 vertexCount;
    /* Number of tris actually drawn, after clipping and all types of culling */
    u16 rdpOutTriCount;
    /* Number of tris which processing started on the RSP (before clipping / culling) */
    u32 rspInTriCount:18;
    /* Number of fill rects and tex rects drawn */
    u32 rectCount:14;
    /* Number of cycles the RSP was stalled because the RDP FIFO was full */
    u32 stallRDPFifoFullCycles;
    /* Unused, zero */
    u32 dummy;
} F3DEX3ProfilingDefault;

typedef struct {  /* Counters for CFG_PROFILING_A */
    /* Number of cycles the RSP spent processing vertex commands, including vertex DMAs */
    u32 vertexProcCycles;
    /* Number of display list commands fetched from DRAM, >= dlCommandCount */
    u16 fetchedDLCommandCount;
    /* Number of display list commands executed */
    u16 dlCommandCount;
    /* Number of cycles the RSP was stalled because the RDP FIFO was full */
    u32 stallRDPFifoFullCycles;
    /* Number of cycles the RSP spent processing triangle commands, NOT including buffer flushes (i.e. FIFO full) */
    u32 triProcCycles;
} F3DEX3ProfilingA;

typedef struct {  /* Counters for CFG_PROFILING_B */
    /* Number of vertices processed by the RSP */
    u16 vertexCount;
    /* Number of vertices processed which had lighting enabled */
    u16 litVertexCount;
    /* Number of tris culled by the occlusion plane */
    u32 occlusionPlaneCullCount:18;
    /* Number of RSP/input triangles which got clipped */
    u32 clippedTriCount:14;
    /* Number of times any microcode overlay was loaded */
    u32 allOverlayLoadCount:18;
    /* Number of times overlay 2 (lighting) was loaded */
    u32 lightingOverlayLoadCount:14;
    /* Number of times overlay 3 (clipping) was loaded */
    u32 clippingOverlayLoadCount:18;
    /* Number of times overlay 4 (mIT matrix, matrix multiply, etc.) was loaded */
    u32 miscOverlayLoadCount:14;
} F3DEX3ProfilingB;

typedef struct {  /* Counters for CFG_PROFILING_C */
    /* Total cycles F3DEX3 believes it was running, not including SPLoadUcode */
    u32 ex3UcodeCycles;
    /* The "GCLK is alive" bit of the RDP status is sampled once every time a
    display list command is started. This counts the number of times that bit
    was 1. Divide by dlCommandCount to get an approximate measurement of the
    percentage of time the RDP was doing useful work, as opposed to waiting
    for framebuffer / Z buffer memory transactions to complete. */
    u16 commandsSampledGclkActive;
    /* Number of display list commands executed */
    u16 dlCommandCount;
    /* Number of commands sent to the RDP except for triangle commands */
    u32 smallRDPCommandCount:18;
    /* Number of matrix loads, of any type */
    u32 matrixCount:14;
    /* Number of cycles the RSP was stalled waiting for any DMAs: vertex loads,
    matrix loads, copying command buffers to the RDP FIFO, overlay loads, etc. */
    u32 stallDMACycles;
} F3DEX3ProfilingC;

typedef struct {
    union {
        F3DEX3ProfilingDefault def;
        F3DEX3ProfilingA a;
        F3DEX3ProfilingB b;
        F3DEX3ProfilingC c;
        u64 dummy_alignment[2];
    };
    u32 taskdataptr; /* Not a perf counter, can ignore */
    u32 ucode; /* Not a perf counter, can ignore */
} F3DEX3YieldDataFooter;

/* In variables.h with the ENABLE_SPEEDMETER section */
extern volatile F3DEX3YieldDataFooter gRSPProfilingResults;
#endif

extern struct Config gConfig;

// extern OSThread gUnkThread;
extern OSThread gIdleThread;
extern OSThread gMainThread;
extern OSThread gGameLoopThread;
extern OSThread gSoundThread;
#if ENABLE_RUMBLE
extern OSThread gRumblePakThread;

extern OSPfs gRumblePakPfs;
#endif

extern OSMesgQueue gPIMesgQueue;
extern OSMesgQueue gIntrMesgQueue;
extern OSMesgQueue gSPTaskMesgQueue;
#if ENABLE_RUMBLE
extern OSMesgQueue gRumblePakSchedulerMesgQueue;
extern OSMesgQueue gRumbleThreadVIMesgQueue;
#endif
extern OSMesg gDmaMesgBuf[1];
extern OSMesg gPIMesgBuf[32];
extern OSMesg gSIEventMesgBuf[1];
extern OSMesg gIntrMesgBuf[16];
extern OSMesg gUnknownMesgBuf[16];
extern OSIoMesg gDmaIoMesg;
extern OSMesg gMainReceivedMesg;
extern OSMesgQueue gDmaMesgQueue;
extern OSMesgQueue gSIEventMesgQueue;
#if ENABLE_RUMBLE
extern OSMesg gRumblePakSchedulerMesgBuf[1];
extern OSMesg gRumbleThreadVIMesgBuf[1];

extern struct RumbleData gRumbleDataQueue[3];
extern struct RumbleSettings gCurrRumbleSettings;
#endif

extern struct VblankHandler *gVblankHandler1;
extern struct VblankHandler *gVblankHandler2;
extern struct SPTask *gActiveSPTask;
extern s8 gAudioEnabled;
extern u32 gNumVblanks;
extern s8 gResetTimer;
extern s8 gNmiResetBarsTimer;
extern s8 gDebugLevelSelect;
#ifdef VANILLA_DEBUG
extern s8 gShowDebugText;
#endif

// Special struct that keeps track of whether its timer has been set.
//  Without this check, there is a bug at high CPU loads in which
//  the RCP timer gets set twice and the game tries to
//  insert __osBaseTimer into a ring buffer that only contains itself,
//  causing a particularly messy crash.
typedef struct {
    u8 started;
    OSTimer timer;
} OSTimerEx;

void set_vblank_handler(s32 index, struct VblankHandler *handler, OSMesgQueue *queue, OSMesg *msg);
void dispatch_audio_sptask(struct SPTask *spTask);
void exec_display_list(struct SPTask *spTask);
void change_vi(OSViMode *mode, int width, int height);

#endif // MAIN_H
