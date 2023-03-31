#include <ultra64.h>
#include <stdarg.h>
#include <string.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen/crash_screen.h"
#include "disasm.h"
#include "engine/colors.h"
#include "game/debug.h"
#include "game/game_init.h"

ALIGNED16 static struct BranchArrow sBranchArrows[DISASM_BRANCH_BUFFER_SIZE];
static u32 sNumBranchArrows = 0;

_Bool gFillBranchBuffer = FALSE;
_Bool sBranchBufferFilled = TRUE;

static const RGBA32 sBranchColors[] = {
    COLOR_RGBA32_ORANGE,
    COLOR_RGBA32_LIME,
    COLOR_RGBA32_CYAN,
    COLOR_RGBA32_MAGENTA,
    COLOR_RGBA32_YELLOW,
    COLOR_RGBA32_PINK,
    COLOR_RGBA32_LIGHT_GRAY,
    COLOR_RGBA32_LIGHT_BLUE,
};

#ifdef INCLUDE_DEBUG_MAP
void draw_loading_arrows_message(void) {
    return; //! TODO:

    crash_screen_draw_dark_rect(
        (SCREEN_CENTER_X - TEXT_WIDTH(10)), (SCREEN_CENTER_Y - TEXT_HEIGHT(1)),
        TEXT_WIDTH(20), TEXT_HEIGHT(3),
        3
    );

    crash_screen_print((SCREEN_CENTER_X - TEXT_WIDTH(8)), SCREEN_CENTER_Y, STR_COLOR_PREFIX"LOADING ARROWS...", COLOR_RGBA32_RED);

    crash_screen_update_framebuffer();
}

//! TODO: Optimize this as much as possible
//! TODO: Version that works without INCLUDE_DEBUG_MAP (check for branches relative to viewport)
void crash_screen_fill_branch_buffer(const char *fname, const uintptr_t funcAddr) {
    struct BranchArrow *currArrow = NULL;
    uintptr_t addr, checkAddr;
    InsnData insn;
    s16 branchOffset;
    s16 curBranchColorIndex = 0;
    s32 curBranchX = DISASM_BRANCH_ARROW_OFFSET;

    if (fname == NULL) {
        return;
    }

    bzero(sBranchArrows, sizeof(sBranchArrows));

    sNumBranchArrows = 0;

    s32 numBranchArrows = sNumBranchArrows;
    currArrow = sBranchArrows;

    for (addr = funcAddr; addr < (funcAddr + DISASM_FUNCTION_SEARCH_MAX_OFFSET); addr += DISASM_STEP) {
        if (numBranchArrows >= DISASM_BRANCH_BUFFER_SIZE) {
            break;
        }
        checkAddr = addr;
        if (fname != parse_map(&checkAddr)) {
            break;
        }
        insn.raw = *(uintptr_t*)addr;

        branchOffset = get_branch_offset(insn);

        if (branchOffset != 0) {
            currArrow->startAddr = addr;
            currArrow->branchOffset = branchOffset;
            currArrow->colorIndex = curBranchColorIndex;
            currArrow->xPos = curBranchX;

            curBranchX += DISASM_BRANCH_ARROW_SPACING;
            curBranchColorIndex = ((curBranchColorIndex + 1) % ARRAY_COUNT(sBranchColors));

            if ((DISASM_BRANCH_ARROW_START_X + curBranchX) > CRASH_SCREEN_TEXT_X2) {
                curBranchX = DISASM_BRANCH_ARROW_OFFSET;
            }

            currArrow++;
            numBranchArrows++;
        }
    }

    sNumBranchArrows = numBranchArrows;
}
#endif

void draw_branch_arrow(s32 startLine, s32 endLine, s32 dist, RGBA32 color, s32 printLine) {
    // Check to see if arrow is fully away from the screen.
    if (
        ((startLine >= 0)               || (endLine >= 0)) &&
        ((startLine <  DISASM_NUM_ROWS) || (endLine <  DISASM_NUM_ROWS))
    ) {
        s32 arrowStartHeight = (TEXT_Y(printLine + startLine) + 3);
        s32 arrowEndHeight   = (TEXT_Y(printLine +   endLine) + 3);

        if (startLine < 0) {
            arrowStartHeight = (TEXT_Y(printLine) - 1);
        } else if (startLine >= DISASM_NUM_ROWS) {
            arrowStartHeight = (TEXT_Y(printLine + DISASM_NUM_ROWS) - 2);
        } else {
            crash_screen_draw_rect((DISASM_BRANCH_ARROW_START_X + 1), arrowStartHeight, dist, 1, color);
        }

        if (endLine < 0) {
            arrowEndHeight = (TEXT_Y(printLine) - 1);
        } else if (endLine >= DISASM_NUM_ROWS) {
            arrowEndHeight = (TEXT_Y(printLine + DISASM_NUM_ROWS) - 2);
        } else {
            u32 x = ((DISASM_BRANCH_ARROW_START_X + dist) - DISASM_BRANCH_ARROW_OFFSET);
            crash_screen_draw_rect((x + 0), (arrowEndHeight - 0), (DISASM_BRANCH_ARROW_OFFSET + 1), 1, color);
            // Arrow head
            crash_screen_draw_rect((x + 1), (arrowEndHeight - 1), 1, 3, color);
            crash_screen_draw_rect((x + 2), (arrowEndHeight - 2), 1, 5, color);
        }

        s32 height = abss(arrowEndHeight - arrowStartHeight);

        // Middle of arrow
        crash_screen_draw_rect((DISASM_BRANCH_ARROW_START_X + dist), MIN(arrowStartHeight, arrowEndHeight), 1, height, color);
    }
}

void draw_branch_arrows(s32 printLine) {
    // Draw branch arrows from the buffer.
    struct BranchArrow *currArrow = NULL;
    for (u32 b = 0; b < sNumBranchArrows; b++) {
        currArrow = &sBranchArrows[b];
        s32 startLine = (((s32)currArrow->startAddr - (s32)gScrollAddress) / DISASM_STEP);
        s32 endLine = (startLine + currArrow->branchOffset + 1);
        draw_branch_arrow(startLine, endLine, currArrow->xPos, sBranchColors[currArrow->colorIndex], printLine);
    }

    osWritebackDCacheAll();
}

void print_as_binary(s32 charX, s32 charY, u32 data) {
    // for (u32 c = 0; c < 4; c++) {
    //     crash_screen_draw_glyph(charX + (c * TEXT_WIDTH(1)), charY, (unsigned char)(data >> ((32 - 8) - (8 * c))), COLOR_RGBA32_WHITE);
    // }
    s32 bitX = charX;
    for (u32 c = 0; c < 32; c++) {
        if ((c % 8) == 0) {
            bitX += TEXT_WIDTH(1);
        }
        crash_screen_draw_glyph(bitX, charY, (((data >> (32 - c)) % 2) ? '1' : '0'), COLOR_RGBA32_WHITE);
        bitX += TEXT_WIDTH(1);
    }
}

void draw_disasm(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;
    const char *fname = NULL;
    uintptr_t alignedSelectedAddr = (gSelectedAddress & ~(DISASM_STEP - 1));

#ifdef INCLUDE_DEBUG_MAP
    uintptr_t funcAddr = alignedSelectedAddr;
    fname = parse_map(&funcAddr);

    if (gCrashScreenSwitchedPage) {
        // draw_loading_arrows_message();
        crash_screen_fill_branch_buffer(fname, funcAddr);
    }
#endif

    clamp_view_to_selection(DISASM_NUM_ROWS, DISASM_STEP);

    u32 line = 1;

    crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_PAGE_NAME, gCrashScreenPages[PAGE_DISASM].name);
    line += crash_screen_print(TEXT_X(7), TEXT_Y(line), (STR_HEX_WORD" %s "STR_HEX_WORD"-"STR_HEX_WORD), alignedSelectedAddr, "in", gScrollAddress, (gScrollAddress + DISASM_SHOWN_SECTION));
    crash_screen_draw_divider(DIVIDER_Y(line));

    if (fname == NULL) {
        line += crash_screen_print(TEXT_X(0), TEXT_Y(line), "%s", "NOT IN A FUNCTION");
    } else {
        crash_screen_print(TEXT_X(0), TEXT_Y(line), "%s:", "IN");
        line += crash_screen_print_scroll(TEXT_X(3), TEXT_Y(line), (CRASH_SCREEN_NUM_CHARS_X - 3), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_FUNCTION_NAME, fname);
    }

    osWritebackDCacheAll();

#ifdef INCLUDE_DEBUG_MAP
    draw_branch_arrows(line);
#endif

    gCrashScreenWordWrap = FALSE;

    u32 charX = TEXT_X(0);
    u32 charY = TEXT_Y(line);

    for (u32 y = 0; y < DISASM_NUM_ROWS; y++) {
        uintptr_t addr = (gScrollAddress + (y * DISASM_STEP));
        InsnData insn;
        insn.raw = *(uintptr_t*)addr;

        charY = TEXT_Y(line + y);

        if (addr == tc->pc) {
            crash_screen_draw_rect((charX - 1), (charY - 2), (CRASH_SCREEN_TEXT_W + 1), (TEXT_HEIGHT(1) + 1), COLOR_RGBA32_CRASH_PC);
        } else if (addr == alignedSelectedAddr) {
            crash_screen_draw_rect((charX - 1), (charY - 2), (CRASH_SCREEN_TEXT_W + 1), (TEXT_HEIGHT(1) + 1), COLOR_RGBA32_CRASH_SELECT);
#ifndef INCLUDE_DEBUG_MAP
            if (is_in_code_segment(addr)) {
                s16 branchOffset = get_branch_offset(insn);
                if (branchOffset != 0) {
                    s32 startLine = (((s32)addr - (s32)gScrollAddress) / DISASM_STEP);
                    s32 endLine = (startLine + branchOffset + 1);
                    draw_branch_arrow(startLine, endLine, DISASM_BRANCH_ARROW_OFFSET, COLOR_RGBA32_CRASH_FUNCTION_NAME_2, line);
                }
            }
#endif
        }

        if (is_in_code_segment(addr)) {
            char *insnAsStr = insn_disasm(insn, &fname);
            crash_screen_print(charX, charY, "%s", insnAsStr);
            if (addr == tc->pc) {
                char crashStr[] = "<-- CRASH";
                crash_screen_print((CRASH_SCREEN_TEXT_X2 - TEXT_WIDTH(strlen(crashStr))), charY, STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_AT, crashStr);
            }
#ifdef INCLUDE_DEBUG_MAP
            if (fname != NULL) {
                crash_screen_print_scroll((charX + TEXT_WIDTH(18)), charY, 26, STR_COLOR_PREFIX"%s", sDisasmColors[DISASM_COLOR_ADDRESS], fname);
            }
#endif
        } else if (gShowRamAsAscii) { // Binary:
            print_as_binary(charX, charY, insn.raw);
        } else {
            crash_screen_print(charX, charY, STR_HEX_WORD, insn.raw);
        }
    }

    gCrashScreenWordWrap = TRUE;

    osWritebackDCacheAll();

    crash_screen_draw_divider(DIVIDER_Y(line));

    u32 line2 = (line + DISASM_NUM_ROWS);

    crash_screen_draw_divider(DIVIDER_Y(line2));

    // Scroll bar
    crash_screen_draw_scroll_bar(DIVIDER_Y(line), DIVIDER_Y(line2), DISASM_SHOWN_SECTION, TOTAL_RAM_SIZE, (gScrollAddress - DISASM_SCROLL_MIN), 4, COLOR_RGBA32_LIGHT_GRAY);

    // Scroll bar crash position marker
    crash_screen_draw_scroll_bar(DIVIDER_Y(line), DIVIDER_Y(line2), DISASM_SHOWN_SECTION, TOTAL_RAM_SIZE, (tc->pc - DISASM_SCROLL_MIN), 1, COLOR_RGBA32_CRASH_AT);

    osWritebackDCacheAll();
}

const enum ControlTypes disasmPageControls[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_CURSOR_VERTICAL,
    CONT_DESC_JUMP_TO_ADDRESS,
    CONT_DESC_TOGGLE_ASCII,
    CONT_DESC_LIST_END,
};

void check_fill_branch_buffer(void) {

}

void crash_screen_input_disasm(void) {
#ifdef INCLUDE_DEBUG_MAP
    uintptr_t oldPos = gSelectedAddress;
#endif

    // gSelectedAddress = ALIGN(gSelectedAddress, DISASM_STEP);
    if ((gCrashScreenDirectionFlags.pressed.up)
     && ((gSelectedAddress - DISASM_STEP) >= RAM_START)) {
        // Scroll up.
        gSelectedAddress -= DISASM_STEP;
        gCrashScreenUpdateBuffer = TRUE;
    }

    if ((gCrashScreenDirectionFlags.pressed.down)
     && ((gSelectedAddress + DISASM_STEP) < RAM_END)) {
        // Scroll down.
        gSelectedAddress += DISASM_STEP;
        gCrashScreenUpdateBuffer = TRUE;
    }

    if (gPlayer1Controller->buttonPressed & A_BUTTON) { //! TODO: not if address select was just closed
        open_address_select(get_branch_target_from_addr(gSelectedAddress));
    }

    if (gPlayer1Controller->buttonPressed & B_BUTTON) {
        toggle_show_ram_as_ascii();
    }

#ifdef INCLUDE_DEBUG_MAP
    if (oldPos != gSelectedAddress) {
        gSelectedAddress &= ~(DISASM_STEP - 1);
        uintptr_t newFunc = gSelectedAddress;
        parse_map(&oldPos);
        const char *fname = parse_map(&newFunc);
        if (oldPos != newFunc) {
            // draw_loading_arrows_message();
            crash_screen_fill_branch_buffer(fname, newFunc);
        }
    }
#endif
}
