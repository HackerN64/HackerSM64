#pragma once

#include <ultra64.h>

#include "types.h"
#include "insn_disasm.h"

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
    PAGE_STACKTRACE,
    PAGE_RAM_VIEWER,
    PAGE_DISASM,
    PAGE_CONTROLS,
    PAGE_COUNT,
    PAGES_MAX = 255,
};

enum CrashScreenDirectionFlags {
    CRASH_SCREEN_INPUT_DIRECTION_FLAGS_NONE         = 0x0,
    CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_UP       = BIT(0),
    CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_DOWN     = BIT(1),
    CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_LEFT     = BIT(2),
    CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_RIGHT    = BIT(3),
    CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_UP    = BIT(4),
    CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_DOWN  = BIT(5),
    CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_LEFT  = BIT(6),
    CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_RIGHT = BIT(7),
};

// Crash screen font properties.
#define CRASH_SCREEN_FONT_CHAR_WIDTH     5
#define CRASH_SCREEN_FONT_CHAR_HEIGHT    7
#define CRASH_SCREEN_FONT_CHARS_PER_ROW  6
#define CRASH_SCREEN_FONT_NUM_ROWS      43

// Spacing between chars.
#define CRASH_SCREEN_CHAR_SPACING_X      1
#define CRASH_SCREEN_CHAR_SPACING_Y      3

// The amount of space each char uses.
#define CRASH_SCREEN_LETTER_WIDTH       (CRASH_SCREEN_FONT_CHAR_WIDTH  + CRASH_SCREEN_CHAR_SPACING_X) //  6
#define CRASH_SCREEN_ROW_HEIGHT         (CRASH_SCREEN_FONT_CHAR_HEIGHT + CRASH_SCREEN_CHAR_SPACING_Y) // 10

// Width and height of crash screen.
#define CRASH_SCREEN_W 270
#define CRASH_SCREEN_H 222

// Number of chars that can fit in the crash screen.
#define CRASH_SCREEN_NUM_CHARS_X ((CRASH_SCREEN_W - 1) / CRASH_SCREEN_LETTER_WIDTH) // 44
#define CRASH_SCREEN_NUM_CHARS_Y ((CRASH_SCREEN_H - 1) / CRASH_SCREEN_ROW_HEIGHT)   // 22

// Macros for string size.
#define TEXT_WIDTH(numChars)  ((numChars) * CRASH_SCREEN_LETTER_WIDTH) // n *  6
#define TEXT_HEIGHT(numChars) ((numChars) * CRASH_SCREEN_ROW_HEIGHT  ) // n * 10

// Width and height of the text grid.
#define CRASH_SCREEN_TEXT_W TEXT_WIDTH( CRASH_SCREEN_NUM_CHARS_X) // 264
#define CRASH_SCREEN_TEXT_H TEXT_HEIGHT(CRASH_SCREEN_NUM_CHARS_Y) // 220

// Number of pixels between the text and the edge of the crash screen.
#define CRASH_SCREEN_TEXT_MARGIN_X ((CRASH_SCREEN_W - CRASH_SCREEN_TEXT_W) / 2) // 3
#define CRASH_SCREEN_TEXT_MARGIN_Y ((CRASH_SCREEN_H - CRASH_SCREEN_TEXT_H) / 2) // 1

// Top left corner of crash screen (round up).
#define CRASH_SCREEN_X1 (((SCREEN_WIDTH  - CRASH_SCREEN_W) / 2) - 0) // 25
#define CRASH_SCREEN_Y1 (((SCREEN_HEIGHT - CRASH_SCREEN_H) / 2) - 1) //  8

// Bottom right corner of crash screen.
#define CRASH_SCREEN_X2 (CRASH_SCREEN_X1 + CRASH_SCREEN_W) // 295
#define CRASH_SCREEN_Y2 (CRASH_SCREEN_Y1 + CRASH_SCREEN_H) // 230

// Top left corner of the text grid.
#define CRASH_SCREEN_TEXT_X1 (CRASH_SCREEN_X1 + CRASH_SCREEN_TEXT_MARGIN_X + 0) // 28
#define CRASH_SCREEN_TEXT_Y1 (CRASH_SCREEN_Y1 + CRASH_SCREEN_TEXT_MARGIN_Y + 1) // 10

// Bottom right corner of the text grid.
#define CRASH_SCREEN_TEXT_X2 (CRASH_SCREEN_TEXT_X1 + CRASH_SCREEN_TEXT_W) // 292
#define CRASH_SCREEN_TEXT_Y2 (CRASH_SCREEN_TEXT_Y1 + CRASH_SCREEN_TEXT_H) // 230

// Macros to convert a position on the text grid to screen coords.
#define TEXT_X(numChars) (CRASH_SCREEN_TEXT_X1 + TEXT_WIDTH(numChars) ) // 28 + (n *  6)
#define TEXT_Y(numChars) (CRASH_SCREEN_TEXT_Y1 + TEXT_HEIGHT(numChars)) // 10 + (n * 10)

// Returns the Y coordinate between the Y position on the text grid, and the space above it.
#define DIVIDER_Y(numChars) (TEXT_Y(numChars) - 2)

// The size of one row of the font image.
typedef u32 FontRow;

// Size of the crashed crash screen image.
#define SRC_IMG_SIZE (SCREEN_SIZE * sizeof(Texture) / sizeof(RGBA32))

// The number of branch arrows that can be stored per-function.
#define DISASM_BRANCH_BUFFER_SIZE 0x100

// The number of functions to save to the stack trace buffer.
#define STACK_SIZE 256 // (s32)(0x800 / sizeof(u64))

struct FunctionInStack {
    /*0x00*/ uintptr_t addr;
    /*0x04*/ char *name;
}; /*0x08*/

struct CrashScreenPage {
    /*0x00*/ void (*drawFunc)(OSThread *thread);
    /*0x04*/ void (*inputFunc)(void);
}; /*0x08*/

struct BranchArrow {
    /*0x00*/ uintptr_t startAddr;
    /*0x02*/ s16 branchOffset;
    /*0x04*/ s16 colorIndex;
    /*0x08*/ s32 xPos;
}; /*0x10*/

struct CrashScreen {
    /*0x000*/ OSThread thread;
    /*0x1B0*/ u64 stack[0x800 / sizeof(u64)];
    /*0x9B0*/ OSMesgQueue mesgQueue;
    /*0x9C8*/ OSMesg mesg;
}; /*0x9CC*/

// Maximum number of chars 
#define CHAR_BUFFER_SIZE 0x100

// Stack Trace constants
#define STACK_TRACE_NUM_ROWS 18

// Address Select constants
#define JUMP_MENU_W (TEXT_WIDTH(8))
#define JUMP_MENU_H (TEXT_HEIGHT(1))

#define JUMP_MENU_X (SCREEN_CENTER_X - (JUMP_MENU_W / 2))
#define JUMP_MENU_Y (SCREEN_CENTER_Y - (JUMP_MENU_H / 2))

#define JUMP_MENU_MARGIN_X 10
#define JUMP_MENU_MARGIN_Y 10

// RAM Viewer constants
#define RAM_VIEWER_STEP (s32)(sizeof(uintptr_t) * 4)

#define RAM_VIEWER_NUM_ROWS 18
#define RAM_VIEWER_SHOWN_SECTION ((RAM_VIEWER_NUM_ROWS - 1) * RAM_VIEWER_STEP)

#define RAM_VIEWER_SCROLL_MIN RAM_START
#define RAM_VIEWER_SCROLL_MAX (RAM_END - RAM_VIEWER_SHOWN_SECTION)

// Disasm constants
#define DISASM_STEP (s32)sizeof(InsnData)

#define DISASM_NUM_ROWS 18
#define DISASM_SHOWN_SECTION ((DISASM_NUM_ROWS - 1) * DISASM_STEP)

#define DISASM_SCROLL_MIN RAM_START
#define DISASM_SCROLL_MAX (RAM_END - DISASM_SHOWN_SECTION)

#define DISASM_BRANCH_ARROW_START_X TEXT_X(23)
#define DISASM_BRANCH_ARROW_SPACING (TEXT_WIDTH(1) / 2)
#define DISASM_FUNCTION_SEARCH_MAX_OFFSET (1024 * DISASM_STEP)


void crash_screen_init(void);
