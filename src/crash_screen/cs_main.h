#pragma once

#include <ultra64.h>

#include "types.h"

#include "game/main.h"


// Time conversion macros (from os_convert.h)
#define FPS_COUNT 30
#define FRAMES_TO_NESC(f)   (((OSTime)(f) * 1000000000LL) / FPS_COUNT)
#define FRAMES_TO_UESC(f)   (((OSTime)(f) * 1000000LL) / FPS_COUNT)
#define FRAMES_TO_CYCLES(f) (((OSTime)(f) * OS_CPU_COUNTER) / FPS_COUNT)
#define NSEC_TO_FRAMES(n)   (((OSTime)(n) * FPS_COUNT) / 1000000000LL)
#define USEC_TO_FRAMES(n)   (((OSTime)(n) * FPS_COUNT) / 1000000LL)
#define CYCLES_TO_FRAMES(c) (((OSTime)(c) * FPS_COUNT) / OS_CPU_COUNTER)


// Adjust tc->pc when in a delay slot.
#define GET_EPC(tc) (tc->pc + (((Reg_CP0_Cause)tc->cause).BD ? sizeof(Word) : 0))


// The number of crash screen threads that will be cycled through when the crash screen crashes. This should be at least 3.
#define NUM_CRASH_SCREEN_BUFFERS 3


typedef enum CrashScreenMessageIDs {
    CRASH_SCREEN_MSG_NONE,
    CRASH_SCREEN_MSG_VI_VBLANK,
    CRASH_SCREEN_MSG_CPU_BREAK = OS_EVENT_CPU_BREAK,
    CRASH_SCREEN_MSG_SP_BREAK  = OS_EVENT_SP_BREAK,
    CRASH_SCREEN_MSG_FAULT     = OS_EVENT_FAULT,
} CrashScreenMessageIDs;


typedef struct CSThreadInfo {
    /*0x000*/ OSThread thread; /*0x1B0*/
    /*0x1B0*/ u64 stack[THREAD1000_STACK / sizeof(u64)]; /*0x400*/
    /*0x4B0*/ OSMesgQueue mesgQueue; /*0x18*/
    /*0x4C8*/ OSMesg mesg; /*0x04*/
} CSThreadInfo; /*0x4CC*/


extern CSThreadInfo* gActiveCSThreadInfo;
extern OSThread* gCrashedThread;
extern OSThread* gInspectThread;

extern Address gSelectedAddress;
extern Address gLastCSSelectedAddress;

extern Word gWatchLo;
extern Word gVIControl;

extern u32 gCountFactor;
extern u32 gTimingDiv;


void create_crash_screen_thread(void);
