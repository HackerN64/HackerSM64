#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/cs_settings.h"


enum CSDisasmBranchArrowModes {
    DISASM_ARROW_MODE_OFF,
    DISASM_ARROW_MODE_SELECTION,
#ifdef INCLUDE_DEBUG_MAP
    DISASM_ARROW_MODE_FUNCTION,
#endif // INCLUDE_DEBUG_MAP
    DISASM_ARROW_MODE_OVERSCAN,
};

enum CSSettingsGroup_page_disasm {
    CS_OPT_HEADER_PAGE_DISASM,
    CS_OPT_DISASM_SHOW_RANGE,
#ifdef INCLUDE_DEBUG_MAP
    CS_OPT_DISASM_SHOW_SYMBOL,
#endif // INCLUDE_DEBUG_MAP
    CS_OPT_DISASM_BINARY,
    CS_OPT_DISASM_PSEUDOINSNS,
    CS_OPT_DISASM_IMM_FMT,
    CS_OPT_DISASM_OFFSET_ADDR,
    CS_OPT_DISASM_ARROW_MODE, //! TODO: Implement overscan.
    CS_OPT_END_DISASM,
};


typedef struct BranchArrow {
    /*0x00*/ Address startAddr;
    /*0x04*/ s16 branchOffset;
    /*0x06*/ u16 xPos;
} BranchArrow; /*0x08*/


#define PAGE_DISASM_STEP (ssize_t)sizeof(InsnData)


// Disasm constants.
#define DISASM_BRANCH_ARROW_HEAD_SIZE       4 // The size of the branch arrow head.
#define DISASM_BRANCH_ARROW_HEAD_OFFSET     1 // The distance between the line and the arrow head.
#define DISASM_BRANCH_ARROW_SPACING         2 // The spacing between each arrow's line, inclusive.
#define DISASM_FUNCTION_SEARCH_MAX_INSNS    2048
#define DISASM_FUNCTION_SEARCH_MAX_OFFSET   (DISASM_FUNCTION_SEARCH_MAX_INSNS * PAGE_DISASM_STEP) // The max number of instructions to search for branches within a function.

// The number of branch arrows that can be stored per-function.
#define DISASM_BRANCH_BUFFER_SIZE           0x200


#ifdef INCLUDE_DEBUG_MAP
extern _Bool gFillBranchBuffer;
#endif // INCLUDE_DEBUG_MAP


extern struct CSSetting cs_settings_group_page_disasm[];
extern struct CSPage gCSPage_disasm;

void print_as_insn(const u32 charX, const u32 charY, const Address addr, const Word data);
