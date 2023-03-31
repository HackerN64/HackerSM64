#pragma once

#include <ultra64.h>

#include "types.h"
#include "game/main.h"
#include "crash_controls.h"
#include "crash_draw.h"
#include "crash_print.h"
#include "insn_disasm.h"
#include "map_parser.h"
#include "address_select.h"


enum CrashScreenMessageIDs {
    CRASH_SCREEN_MSG_NONE,
    CRASH_SCREEN_MSG_CPU_BREAK,
    CRASH_SCREEN_MSG_FAULT,
    CRASH_SCREEN_MSG_VI_VBLANK,
};

enum CrashScreenPages {
    FIRST_PAGE,
    PAGE_CONTEXT = FIRST_PAGE,
    PAGE_ASSERTS,
#ifdef PUPPYPRINT_DEBUG
    PAGE_LOG,
#endif
    PAGE_STACK_TRACE,
    PAGE_RAM_VIEWER,
    PAGE_DISASM,
    NUM_PAGES,
    MAX_PAGES = 255U,
};

// The number of functions to save to the stack trace buffer.
#define STACK_SIZE 256 // (s32)(THREAD2_STACK / sizeof(u64))

struct CrashScreen {
    /*0x000*/ OSThread thread;
    /*0x1B0*/ u64 stack[THREAD2_STACK / sizeof(u64)];
    /*0x9B0*/ OSMesgQueue mesgQueue;
    /*0x9C8*/ OSMesg mesg;
}; /*0x9CC*/

struct CrashScreenPage {
    /*0x00*/ void (*drawFunc)(OSThread *thread);
    /*0x04*/ void (*inputFunc)(void);
    /*0x08*/ const enum ControlTypes *pageControlsList;//void (*controlsDrawFunc)(void);
    /*0x10*/ const char *name;
}; /*0x14*/


// Time conversion macros
#define FPS_COUNT 30
#define FRAMES_TO_NESC(f)   (((u64)(f) * 1000000000LL) / FPS_COUNT)
#define FRAMES_TO_UESC(f)   (((u64)(f) * 1000000LL) / FPS_COUNT)
#define FRAMES_TO_CYCLES(f) (((u64)(f) * OS_CPU_COUNTER) / FPS_COUNT)
#define NSEC_TO_FRAMES(n)   (((u64)(n) * FPS_COUNT) / 1000000000LL)
#define USEC_TO_FRAMES(n)   (((u64)(n) * FPS_COUNT) / 1000000LL)
#define CYCLES_TO_FRAMES(c) (((u64)(c) * FPS_COUNT) / OS_CPU_COUNTER)


extern struct CrashScreen gCrashScreen;
#ifdef CRASH_SCREEN_CRASH_SCREEN
extern struct CrashScreen gCrashScreen2;
#endif

extern _Bool gDrawCrashScreen;
extern _Bool gDrawBackground;
extern _Bool gCrashScreenSwitchedPage;
extern _Bool gCrashScreenUpdateFramebuffer;

extern enum CrashScreenPages gCrashPage;
extern uintptr_t gCrashAddress;
extern uintptr_t gScrollAddress;
extern uintptr_t gSelectedAddress;

extern struct CrashScreenPage gCrashScreenPages[];


void toggle_display_var(_Bool *var);
void crash_screen_draw_scroll_bar(u32 topY, u32 bottomY, u32 numVisibleEntries, u32 numTotalEntries, u32 currEntry, u32 minScrollBarHeight, RGBA32 color);
void clamp_view_to_selection(const u32 numRows, const u32 step);
void crash_screen_init(void);
