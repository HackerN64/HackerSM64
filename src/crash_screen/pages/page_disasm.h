#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/crash_settings.h"


enum CSDisasmBranchArrowModes {
    DISASM_ARROW_MODE_OFF,
    DISASM_ARROW_MODE_SELECTION,
#ifdef INCLUDE_DEBUG_MAP
    DISASM_ARROW_MODE_FUNCTION,
#endif
    DISASM_ARROW_MODE_OVERSCAN,
};

enum CSSettingsGroup_page_disasm {
    CS_OPT_HEADER_PAGE_DISASM,
#ifdef INCLUDE_DEBUG_MAP
    CS_OPT_DISASM_SHOW_SYMBOL,
#endif
    CS_OPT_DISASM_BINARY,
    CS_OPT_DISASM_PSEUDOINSNS,
    CS_OPT_DISASM_IMM_FMT,
    CS_OPT_DISASM_OFFSET_ADDR,
    CS_OPT_DISASM_ARROW_MODE, //! TODO: Implement overscan.
    CS_OPT_END_DISASM,
};


typedef struct BranchArrow {
    /*0x00*/ Address startAddr;
    /*0x02*/ s16 branchOffset;
    /*0x04*/ s16 colorIndex;
    /*0x08*/ s32 xPos;
} BranchArrow; /*0x10*/


// Disasm constants.
#define DISASM_BRANCH_ARROW_HEAD_SIZE       4 // The size of the branch arrow head.
#define DISASM_BRANCH_ARROW_HEAD_OFFSET     1 // The distance between the line and the arrow head.
#define DISASM_BRANCH_ARROW_SPACING         2 // The spacing between each arrow's line, inclusive.
#define DISASM_FUNCTION_SEARCH_MAX_OFFSET   (1024 * DISASM_STEP) // The max number of instructions to search for branches within a function.

// The number of branch arrows that can be stored per-function.
#define DISASM_BRANCH_BUFFER_SIZE   0x100


#ifdef INCLUDE_DEBUG_MAP
extern _Bool gFillBranchBuffer;
#endif


extern struct CSSetting cs_settings_group_page_disasm[];
extern struct CSPage gCSPage_disasm;
