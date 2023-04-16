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

#define DISASM_SCROLL_MIN       VALID_RAM_START
#define DISASM_SCROLL_MAX       (VALID_RAM_END - DISASM_SHOWN_SECTION)

#define DISASM_BRANCH_ARROW_START_X         TEXT_X(INSN_NAME_DISPLAY_WIDTH + sizeof("R0 R0 +0x0000"))
#define DISASM_BRANCH_ARROW_OFFSET          TEXT_WIDTH(1)
#define DISASM_BRANCH_ARROW_SPACING         (TEXT_WIDTH(1) / 2)
#define DISASM_FUNCTION_SEARCH_MAX_OFFSET   (1024 * DISASM_STEP)

// The number of branch arrows that can be stored per-function.
#define DISASM_BRANCH_BUFFER_SIZE   0x100


extern _Bool gFillBranchBuffer;

extern const enum ControlTypes disasmPageControls[];


void disasm_init(void);
void disasm_draw(OSThread* thread);
void disasm_input(void);
