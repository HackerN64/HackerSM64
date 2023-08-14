#pragma once

#include <ultra64.h>

#include "types.h"


struct BranchArrow {
    /*0x00*/ Address startAddr;
    /*0x02*/ s16 branchOffset;
    /*0x04*/ s16 colorIndex;
    /*0x08*/ s32 xPos;
}; /*0x10*/


// Disasm constants.
#define DISASM_BRNACH_ARROW_HEAD_SIZE       4 // The size of the branch arrow head.
#define DISASM_BRANCH_ARROW_HEAD_OFFSET     1 // The distance between the line and the arrow head.
#define DISASM_BRANCH_ARROW_SPACING         2 // The spacing between each arrow's line, inclusive.
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
