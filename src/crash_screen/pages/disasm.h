#pragma once

#include <ultra64.h>

#include "types.h"


struct BranchArrow {
    /*0x00*/ uintptr_t startAddr;
    /*0x02*/ s16 branchOffset;
    /*0x04*/ s16 colorIndex;
    /*0x08*/ s32 xPos;
}; /*0x10*/

// Disasm constants
#define DISASM_NUM_ROWS         19
#define DISASM_SHOWN_SECTION    ((DISASM_NUM_ROWS - 1) * DISASM_STEP)

#define DISASM_SCROLL_MIN       RAM_START
#define DISASM_SCROLL_MAX       (RAM_END - DISASM_SHOWN_SECTION)

#define DISASM_BRANCH_ARROW_START_X         TEXT_X(23)
#define DISASM_BRANCH_ARROW_OFFSET          TEXT_WIDTH(1)
#define DISASM_BRANCH_ARROW_SPACING         (TEXT_WIDTH(1) / 2)
#define DISASM_FUNCTION_SEARCH_MAX_OFFSET   (1024 * DISASM_STEP)

// The number of branch arrows that can be stored per-function.
#define DISASM_BRANCH_BUFFER_SIZE   0x100


extern _Bool gFillBranchBuffer;

extern const enum ControlTypes disasmPageControls[];


void draw_disasm(OSThread *thread);
void crash_screen_input_disasm(void);
