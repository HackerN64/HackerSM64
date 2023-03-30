#pragma once

#include <ultra64.h>

#include "types.h"
#include "insn_disasm.h"
#include "game/main.h"


enum CrashScreenMessageIDs {
    CRASH_SCREEN_MSG_NONE,
    CRASH_SCREEN_MSG_CPU_BREAK,
    CRASH_SCREEN_MSG_FAULT,
    CRASH_SCREEN_MSG_VI_VBLANK,
};

enum CrashPages {
    PAGE_CONTEXT,
    PAGE_ASSERTS,
#ifdef PUPPYPRINT_DEBUG
    PAGE_LOG,
#endif
    PAGE_STACK_TRACE,
    PAGE_RAM_VIEWER,
    PAGE_DISASM,
    NUM_PAGES,
    PAGES_MAX = 255,
};

typedef union {
    struct PACKED {
        u8 up    : 1;
        u8 down  : 1;
        u8 left  : 1;
        u8 right : 1;
        u8 : 4;
    } pressed;
    struct PACKED {
        u8 : 4;
        u8 up    : 1;
        u8 down  : 1;
        u8 left  : 1;
        u8 right : 1;
    } held;
    u8 raw;
} CrashScreenDirections;

enum ControlTypes {
    CONT_DESC_LIST_END = -1,
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_SCROLL_LIST,
    CONT_DESC_CURSOR,
    CONT_DESC_CURSOR_VERTICAL,
    CONT_DESC_CURSOR_HORIZONTAL,
    CONT_DESC_JUMP_TO_ADDRESS,
    CONT_DESC_TOGGLE_ASCII,
    CONT_DESC_TOGGLE_FUNCTIONS,
    CONT_DESC_TOGGLE_UNKNOWNS,
    NUM_CONT_DESC,
};

// The number of functions to save to the stack trace buffer.
#define STACK_SIZE 256 // (s32)(THREAD2_STACK / sizeof(u64))

struct CrashScreen {
    /*0x000*/ OSThread thread;
    /*0x1B0*/ u64 stack[THREAD2_STACK / sizeof(u64)];
    /*0x9B0*/ OSMesgQueue mesgQueue;
    /*0x9C8*/ OSMesg mesg;
}; /*0x9CC*/

struct FunctionInStack {
    /*0x00*/ uintptr_t addr;
    /*0x04*/ const char *name;
}; /*0x08*/

struct CrashScreenPage {
    /*0x00*/ void (*drawFunc)(OSThread *thread);
    /*0x04*/ void (*inputFunc)(void);
    /*0x08*/ const enum ControlTypes *pageControlsList;//void (*controlsDrawFunc)(void);
    /*0x10*/ const char *name;
}; /*0x14*/

struct ControlType {
    /*0x00*/ const char *control;
    /*0x04*/ const char *description;
}; /*0x08*/

struct BranchArrow {
    /*0x00*/ uintptr_t startAddr;
    /*0x02*/ s16 branchOffset;
    /*0x04*/ s16 colorIndex;
    /*0x08*/ s32 xPos;
}; /*0x10*/

// The number of branch arrows that can be stored per-function.
#define DISASM_BRANCH_BUFFER_SIZE 0x100

// Stack Trace constants
#define STACK_TRACE_NUM_ROWS 19

// Address Select constants
#define JUMP_MENU_CHARS_X 20
#define JUMP_MENU_CHARS_Y  5

#define JUMP_MENU_W (TEXT_WIDTH( JUMP_MENU_CHARS_X))
#define JUMP_MENU_H (TEXT_HEIGHT(JUMP_MENU_CHARS_Y))

#define JUMP_MENU_X1 (SCREEN_CENTER_X - (JUMP_MENU_W / 2))
#define JUMP_MENU_Y1 (SCREEN_CENTER_Y - (JUMP_MENU_H / 2))

#define JUMP_MENU_X2 (SCREEN_CENTER_X + (JUMP_MENU_W / 2))
#define JUMP_MENU_Y2 (SCREEN_CENTER_Y + (JUMP_MENU_H / 2))

#define JUMP_MENU_MARGIN_X 10
#define JUMP_MENU_MARGIN_Y 10

// RAM Viewer constants
#define RAM_VIEWER_STEP (s32)(sizeof(uintptr_t) * 4)

#define RAM_VIEWER_NUM_ROWS 19
#define RAM_VIEWER_SHOWN_SECTION ((RAM_VIEWER_NUM_ROWS - 1) * RAM_VIEWER_STEP)

#define RAM_VIEWER_SCROLL_MIN RAM_START
#define RAM_VIEWER_SCROLL_MAX (RAM_END - RAM_VIEWER_SHOWN_SECTION)

// Disasm constants
#define DISASM_STEP (s32)sizeof(InsnData)

#define DISASM_NUM_ROWS 19
#define DISASM_SHOWN_SECTION ((DISASM_NUM_ROWS - 1) * DISASM_STEP)

#define DISASM_SCROLL_MIN RAM_START
#define DISASM_SCROLL_MAX (RAM_END - DISASM_SHOWN_SECTION)

#define DISASM_BRANCH_ARROW_START_X       TEXT_X(23)
#define DISASM_BRANCH_ARROW_OFFSET        TEXT_WIDTH(1)
#define DISASM_BRANCH_ARROW_SPACING       (TEXT_WIDTH(1) / 2)
#define DISASM_FUNCTION_SEARCH_MAX_OFFSET (1024 * DISASM_STEP)

// Time conversion macros
#define FPS_COUNT 30
#define FRAMES_TO_NESC(f)   (((u64)(f) * 1000000000LL) / FPS_COUNT)
#define FRAMES_TO_UESC(f)   (((u64)(f) * 1000000LL) / FPS_COUNT)
#define FRAMES_TO_CYCLES(f) (((u64)(f) * OS_CPU_COUNTER) / FPS_COUNT)
#define NSEC_TO_FRAMES(n)   (((u64)(n) * FPS_COUNT) / 1000000000LL)
#define USEC_TO_FRAMES(n)   (((u64)(n) * FPS_COUNT) / 1000000LL)
#define CYCLES_TO_FRAMES(c) (((u64)(c) * FPS_COUNT) / OS_CPU_COUNTER)

// Macros used to modify individual digits in a hexadecimal value.
#define GET_HEX_DIGIT(src, shift)       (((src) >> (shift)) & BITMASK(4))
#define SET_HEX_DIGIT(dst, src, shift)  (((dst) & ~(BITMASK(4) << (shift))) | ((src) << (shift)))


extern struct CrashScreen gCrashScreen;
#ifdef CRASH_SCREEN_CRASH_SCREEN
extern struct CrashScreen gCrashScreen2;
#endif

extern uintptr_t gCrashAddress;
extern s8 gCrashScreenQueueFramebufferUpdate;


void crash_screen_init(void);
