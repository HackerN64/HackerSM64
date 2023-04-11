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


//! TODO: Allow reading outside of 0x80000000-0x80800000 range.
#define VALID_RAM_START RAM_START
#define VALID_RAM_END   RAM_END
#define VALID_RAM_SIZE  (u64)(VALID_RAM_END - VALID_RAM_START)


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

struct CrashScreen {
    /*0x000*/ OSThread thread;
    /*0x1B0*/ u64 stack[THREAD2_STACK / sizeof(u64)];
    /*0x9B0*/ OSMesgQueue mesgQueue;
    /*0x9C8*/ OSMesg mesg;
}; /*0x9CC*/

struct CrashScreenPage {
    /*0x00*/ void (*drawFunc)(OSThread* thread);
    /*0x04*/ void (*inputFunc)(void);
    /*0x08*/ const enum ControlTypes* pageControlsList;
    /*0x10*/ const char* name;
    /*0x14*/ _Bool printName;
    /*0x15*/ _Bool skip;
}; /*0x16*/


// Time conversion macros
#define FPS_COUNT 30
#define FRAMES_TO_NESC(f)   (((u64)(f) * 1000000000LL) / FPS_COUNT)
#define FRAMES_TO_UESC(f)   (((u64)(f) * 1000000LL) / FPS_COUNT)
#define FRAMES_TO_CYCLES(f) (((u64)(f) * OS_CPU_COUNTER) / FPS_COUNT)
#define NSEC_TO_FRAMES(n)   (((u64)(n) * FPS_COUNT) / 1000000000LL)
#define USEC_TO_FRAMES(n)   (((u64)(n) * FPS_COUNT) / 1000000LL)
#define CYCLES_TO_FRAMES(c) (((u64)(c) * FPS_COUNT) / OS_CPU_COUNTER)


extern struct CrashScreenPage gCrashScreenPages[];
extern enum CrashScreenPages gCrashPage;

extern struct CrashScreen gCrashScreen;
#ifdef CRASH_SCREEN_CRASH_SCREEN
extern struct CrashScreen gCrashScreen2;
#endif

extern _Bool gGameCrashed;
extern _Bool gCrashScreenSwitchedPage;
extern _Bool gDrawCrashScreen;
extern _Bool gDrawBackground;
extern _Bool gCrashScreenUpdateFramebuffer;

extern uintptr_t gCrashAddress;
extern uintptr_t gScrollAddress;
extern uintptr_t gSelectedAddress;


void toggle_display_var(_Bool* var);
void crash_screen_draw_scroll_bar(u32 topY, u32 bottomY, u32 numVisibleEntries, u32 numTotalEntries, u32 currEntry, u32 minScrollBarHeight, RGBA32 color);
void clamp_view_to_selection(const u32 numRows, const u32 step);
void crash_screen_init(void);
