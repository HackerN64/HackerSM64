#pragma once

#include <ultra64.h>

#include "types.h"
#include "game/main.h"


#define NUM_CRASH_SCREEN_BUFFERS 3


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

struct CSThreadInfo {
    /*0x000*/ OSThread thread; /*0x1B0*/
    /*0x1B0*/ u64 stack[THREAD2_STACK / sizeof(u64)]; /*0x80*/
    /*0x230*/ OSMesgQueue mesgQueue; /*0x18*/
    /*0x248*/ OSMesg mesg; /*0x04*/
    /*0x24C*/ OSThread* crashedThread; /*0x04*/
}; /*0x250*/

struct CSPage {
    /*0x00*/ void (*initFunc)(void);
    /*0x04*/ void (*drawFunc)(OSThread* thread);
    /*0x08*/ void (*inputFunc)(void);
    /*0x0C*/ const enum ControlTypes* pageControlsList;
    /*0x10*/ const char* name;
    /*0x14*/ struct PACKED {
                /*0x00*/ u32             : 29;
                /*0x03*/ u32 printName   :  1;
                /*0x03*/ u32 skip        :  1;
                /*0x03*/ u32 initialized :  1;
            } flags; /*0x04*/
}; /*0x18*/


// Time conversion macros
#define FPS_COUNT 30
#define FRAMES_TO_NESC(f)   (((u64)(f) * 1000000000LL) / FPS_COUNT)
#define FRAMES_TO_UESC(f)   (((u64)(f) * 1000000LL) / FPS_COUNT)
#define FRAMES_TO_CYCLES(f) (((u64)(f) * OS_CPU_COUNTER) / FPS_COUNT)
#define NSEC_TO_FRAMES(n)   (((u64)(n) * FPS_COUNT) / 1000000000LL)
#define USEC_TO_FRAMES(n)   (((u64)(n) * FPS_COUNT) / 1000000LL)
#define CYCLES_TO_FRAMES(c) (((u64)(c) * FPS_COUNT) / OS_CPU_COUNTER)

#include "crash_controls.h"
#include "crash_draw.h"
#include "crash_print.h"
#include "insn_disasm.h"
#include "map_parser.h"
#include "address_select.h"

extern struct CSPage gCSPages[];
extern enum CrashScreenPages gCSPageID;

extern struct CSThreadInfo* gActiveCSThreadInfo;

extern uintptr_t gCrashAddress;
extern uintptr_t gScrollAddress;
extern uintptr_t gSelectedAddress;


void create_crash_screen_thread(void);
