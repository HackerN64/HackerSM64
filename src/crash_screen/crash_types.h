#pragma once

#include <ultra64.h>

#include "types.h"
#include "game/main.h"


// The size of one row of the font image.
typedef u32 CSFontRow;


// Virtual RAM boundary defines.
#define VIRTUAL_RAM_START (Address)RAM_START
#define VIRTUAL_RAM_END   (Address)0xFFFFFFFF
#define VIRTUAL_RAM_SIZE  (size_t)(VIRTUAL_RAM_END - VIRTUAL_RAM_START)

// The number of crash screen threads that will be cycled through when the crash screen crashes. This should be at least 3.
#define NUM_CRASH_SCREEN_BUFFERS 3


enum CrashScreenMessageIDs {
    CRASH_SCREEN_MSG_NONE,
    CRASH_SCREEN_MSG_VI_VBLANK,
    CRASH_SCREEN_MSG_CPU_BREAK = OS_EVENT_CPU_BREAK,
    CRASH_SCREEN_MSG_SP_BREAK  = OS_EVENT_SP_BREAK,
    CRASH_SCREEN_MSG_FAULT     = OS_EVENT_FAULT,
};

//! TODO: Move this back into crash_main.h without causing build warnings.
enum CrashScreenPages {
    FIRST_PAGE,
    PAGE_CONTEXT = FIRST_PAGE,
    PAGE_ASSERTS,
#ifdef PUPPYPRINT_DEBUG
    PAGE_LOG,
#endif
    PAGE_STACK_TRACE,
#ifdef INCLUDE_DEBUG_MAP
    PAGE_MAP_VIEWER,
#endif
    PAGE_RAM_VIEWER,
    PAGE_DISASM,
    PAGE_SETTINGS,
    NUM_PAGES,
    MAX_PAGES = 255U,
};


struct CSThreadInfo {
    /*0x000*/ OSThread thread; /*0x1B0*/
    /*0x1B0*/ Register stack[THREAD2_STACK / sizeof(Register)]; /*0x400*/
    /*0x4B0*/ OSMesgQueue mesgQueue; /*0x18*/
    /*0x4C8*/ OSMesg mesg; /*0x04*/
}; /*0x4CC*/

struct CSPage {
    /*0x00*/ void (*initFunc)(void);
    /*0x04*/ void (*drawFunc)(void);
    /*0x08*/ void (*inputFunc)(void);
    /*0x0C*/ const enum ControlTypes* contList;
    /*0x10*/ const char* name;
    /*0x14*/ struct PACKED {
                /*0x00*/ u32             : 29;
                /*0x03*/ u32 printName   :  1;
                /*0x03*/ u32 crashed     :  1;
                /*0x03*/ u32 initialized :  1;
            } flags; /*0x04*/
}; /*0x18*/


typedef s32 SettingsType;


#define VALUE_NAME_SIZE 10


enum CSPrintNumberFormats {
    PRINT_NUM_FMT_HEX, // 0xFFFFFFFF
    PRINT_NUM_FMT_DEC, // 0.0
    PRINT_NUM_FMT_SCI, // XeX
};

enum CSImmediateModes {
    DISASM_IMM_MODE_HEX, // 0xFFFFFFFF
    DISASM_IMM_MODE_DEC, // 0.0
};

enum CSDisasmBranchArrowModes {
    DISASM_ARROW_MODE_OFF,
    DISASM_ARROW_MODE_SELECTION,
#ifdef INCLUDE_DEBUG_MAP
    DISASM_ARROW_MODE_FUNCTION,
#endif
    DISASM_ARROW_MODE_OVERSCAN,
};

struct CSSettingsEntry {
    /*0x00*/ const char* name;
    /*0x20*/ const char* (*valNames)[];
    /*0x24*/ SettingsType val;
    /*0x28*/ SettingsType defaultVal;
    /*0x2C*/ SettingsType lowerBound;
    /*0x30*/ SettingsType upperBound;
}; /*0x34*/

enum CSSettings {
    CS_OPT_RESET_TO_DEFAULTS,
    CS_OPT_DRAW_SCREENSHOT,
#ifdef INCLUDE_DEBUG_MAP
    CS_OPT_FUNCTION_NAMES,
#endif
    CS_OPT_PRINT_SCROLL_SPEED,
    CS_OPT_FLOATS_FMT,
    CS_OPT_MEMORY_AS_ASCII,
    CS_OPT_DISASM_BINARY,
    CS_OPT_DISASM_PSEUDOINSNS,
    CS_OPT_DISASM_IMM_FMT,
    CS_OPT_DISASM_OFFSET_ADDR,
    CS_OPT_DISASM_ARROW_MODE, //! TODO: Implement this
    NUM_CS_OPTS,
};


struct CSController {
    /*0x00*/ s16 rawStickX;
    /*0x02*/ s16 rawStickY;
    /*0x04*/ u16 buttonDown;
    /*0x06*/ u16 buttonPressed;
    /*0x08*/ u16 buttonReleased;
};


// Time conversion macros
#define FPS_COUNT 30
#define FRAMES_TO_NESC(f)   (((OSTime)(f) * 1000000000LL) / FPS_COUNT)
#define FRAMES_TO_UESC(f)   (((OSTime)(f) * 1000000LL) / FPS_COUNT)
#define FRAMES_TO_CYCLES(f) (((OSTime)(f) * OS_CPU_COUNTER) / FPS_COUNT)
#define NSEC_TO_FRAMES(n)   (((OSTime)(n) * FPS_COUNT) / 1000000000LL)
#define USEC_TO_FRAMES(n)   (((OSTime)(n) * FPS_COUNT) / 1000000LL)
#define CYCLES_TO_FRAMES(c) (((OSTime)(c) * FPS_COUNT) / OS_CPU_COUNTER)
