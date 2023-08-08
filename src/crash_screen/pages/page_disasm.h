#pragma once

#include <ultra64.h>

#include "types.h"


struct BranchArrow {
    /*0x00*/ Address startAddr;
    /*0x02*/ s16 branchOffset;
    /*0x04*/ s16 colorIndex;
    /*0x08*/ s32 xPos;
}; /*0x10*/


// Disasm constants
#ifdef INCLUDE_DEBUG_MAP
#define DISASM_NUM_ROWS         19
#else
#define DISASM_NUM_ROWS         20
#endif
#define DISASM_SHOWN_SECTION    ((DISASM_NUM_ROWS - 1) * DISASM_STEP)

#define DISASM_SCROLL_MIN       VIRTUAL_RAM_START
#define DISASM_SCROLL_MAX       (VIRTUAL_RAM_END - DISASM_SHOWN_SECTION)

#define DISASM_BRANCH_ARROW_OFFSET          4 // The distance between the line and the arrow head.
#define DISASM_BRANCH_ARROW_SPACING         3 // The spacing between each arrow's line.
#define DISASM_FUNCTION_SEARCH_MAX_OFFSET   (1024 * DISASM_STEP) // The max number of instructions to search for branches within a function.

// The number of branch arrows that can be stored per-function.
#define DISASM_BRANCH_BUFFER_SIZE   0x100


#ifdef INCLUDE_DEBUG_MAP
extern _Bool gFillBranchBuffer;
#endif

extern const enum ControlTypes disasmContList[];


void disasm_init(void);
void disasm_draw(void);
void disasm_input(void);
