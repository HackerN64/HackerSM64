#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/cs_settings.h"
#include "crash_screen/cs_draw.h" // For ScreenCoord_ typedefs.


enum CSDisasmUnknownsModes {
    DISASM_UNK_MODE_HEX,
    DISASM_UNK_MODE_BIN,
    DISASM_UNK_MODE_PARSE,
};

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
    CS_OPT_DISASM_UNKNOWNS,
    CS_OPT_DISASM_PSEUDOC,
    CS_OPT_DISASM_PSEUDOINSNS,
    CS_OPT_DISASM_IMM_FMT,
#ifdef INCLUDE_DEBUG_MAP
    CS_OPT_DISASM_SYMBOL_HEADERS,
#endif // INCLUDE_DEBUG_MAP
    CS_OPT_DISASM_OFFSET_ADDR,
    CS_OPT_DISASM_ARROW_MODE, //! TODO: Implement overscan.
    CS_OPT_END_DISASM,
};


typedef union BranchArrow {
    struct {
        /*0x00*/ u16 insnIndex;
        /*0x02*/ s16 branchOffset;
    };
    u32 raw;
} BranchArrow; /*0x04*/


// The size of one instruction in bytes (4).
#define PAGE_DISASM_STEP (ssize_t)sizeof(InsnData)


// Branch arrow visual attributes.
#define DISASM_BRANCH_ARROW_HEAD_SIZE       4 // The size of the branch arrow head.
#define DISASM_BRANCH_ARROW_HEAD_OFFSET     1 // The distance between the line and the arrow head.
#define DISASM_BRANCH_ARROW_SPACING         2 // The spacing between each arrow's line, inclusive.

// The number of branch arrows that can be stored per-function.
#define DISASM_BRANCH_BUFFER_SIZE           384
#define DISASM_FUNCTION_SEARCH_MAX_INSNS    2048
#define DISASM_FUNCTION_SEARCH_MAX_OFFSET   (DISASM_FUNCTION_SEARCH_MAX_INSNS * PAGE_DISASM_STEP) // The max number of instructions to search for branches within a function.


extern struct CSSetting cs_settings_group_page_disasm[];
extern struct CSPage gCSPage_disasm;

void print_insn(const ScreenCoord_u32 x, const ScreenCoord_u32 y, const char* insnAsStr, const char* destFname);
void format_and_print_insn(const ScreenCoord_u32 x, const ScreenCoord_u32 y, const Address addr, const Word data);
