#ifndef MAIN_H
#define MAIN_H

#include "config.h"

enum VIModes {
    MODE_NTSC,
    MODE_MPAL,
    MODE_PAL,
};

#define THREAD1_STACK 0x0100 // Idle thread.
#define THREAD2_STACK 0x0000 // Unused (previously crash screen).
#define THREAD3_STACK 0x0200 // Main thread.
#define THREAD4_STACK 0x2000 // Sound thread (or libultra scheduler thread).
#define THREAD5_STACK 0x2000 // Game Loop thread.
#define THREAD6_STACK 0x0400 // Rumble thread.

#define THREAD1000_STACK 0x0400 // Crash screen.

enum ThreadID {
    THREAD_0_MANAGER,       // Various libultra threads.
    THREAD_1_IDLE,          // Initial thread created by main_func. Sets up various things then creates main thread and goes idle.
    THREAD_2,               // Unused (previously crash screen).
    THREAD_3_MAIN,          // Main thread.
    THREAD_4_SOUND,         // Sound thread. //! TODO: Change this to 2 because the libultra scheduler thread uses id 4.
    THREAD_5_GAME_LOOP,     // Main game loop thread.
    THREAD_6_RUMBLE,        // Rumble thread (see src/game/rumble_init.c).
    THREAD_7_HVQM,          // HVQM main thread (see HVQM_THREAD_ID in src/hvqm/hvqm.h).
    THREAD_8_TIMEKEEPER,    // HVQM timekeeper thread (see TIMEKEEPER_THREAD_ID in src/hvqm/hvqm.h).
    THREAD_9_DA_COUNTER,    // HVQM DA counterthread (see DA_COUNTER_THREAD_ID in src/hvqm/hvqm.h).
    THREAD_13_FAULT = 13,   // UNF debug thread (see FAULT_THREAD_ID in src/usb/debug.h).
    THREAD_14_USB   = 14,   // UNF USB thread (see USB_THREAD_ID in src/usb/debug.h).

    // Crash screen threads (the crash screen has its own crash thread):
    THREAD_1000_CRASH_SCREEN_0 = 1000,  // Initial crash screen thread for the normal game. Can be repurposed as a crash screen for the crash screen.
    THREAD_1001_CRASH_SCREEN_1,         // Created when THREAD_1000_CRASH_SCREEN_0 starts and becomes the new crash screen if it crashes.
    THREAD_1002_CRASH_SCREEN_2,         // Same as THREAD_1001_CRASH_SCREEN_1. If this crashes, cycles back to THREAD_1000_CRASH_SCREEN_0.
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

extern OSViMode VI;

extern struct Config gConfig;

// extern OSThread gUnkThread;
extern OSThread gIdleThread;
extern OSThread gMainThread;
extern OSThread gGameLoopThread;
extern OSThread gSoundThread;
extern OSThread hvqmThread;
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
