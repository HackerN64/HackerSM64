#include <ultra64.h>
#include <string.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen/crash_screen.h"
#include "disasm.h"
#include "game/game_input.h"


static _Bool sDisasmShowDestFunctionNames = TRUE;
static _Bool sDisasmShowDataAsBinary = FALSE;

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
_Bool gFillBranchBuffer = FALSE;
static _Bool sContinueFillBranchBuffer = FALSE;

ALIGNED16 static struct BranchArrow sBranchArrows[DISASM_BRANCH_BUFFER_SIZE];
static u32 sNumBranchArrows = 0;

static uintptr_t sBranchBufferCurrAddr = 0x00000000;


void reset_branch_buffer(uintptr_t funcAddr) {
    bzero(sBranchArrows, sizeof(sBranchArrows));
    sNumBranchArrows = 0;

    sBranchBufferCurrAddr = funcAddr;
}

//! TODO: Optimize this as much as possible
//! TODO: Version that works without INCLUDE_DEBUG_MAP (check for branches relative to viewport)
_Bool crash_screen_fill_branch_buffer(const char* fname, uintptr_t funcAddr) {
    if (fname == NULL) {
        return FALSE;
    }

    s16 curBranchColorIndex;
    s32 curBranchX;

    if (sNumBranchArrows == 0) {
        // Start:
        curBranchColorIndex = 0;
        curBranchX = DISASM_BRANCH_ARROW_START_X;
    } else { //! TODO: Verify that this ordering is correct:
        // Continue:
        curBranchColorIndex = sBranchArrows[sNumBranchArrows - 1].colorIndex;
        curBranchX          = sBranchArrows[sNumBranchArrows - 1].xPos;
    }

    struct BranchArrow* currArrow = &sBranchArrows[sNumBranchArrows];

    OSTime startTime = osGetTime();
    while (TRUE) {
        // Too many entries searched.
        if (sBranchBufferCurrAddr > (funcAddr + DISASM_FUNCTION_SEARCH_MAX_OFFSET)) {
            return FALSE;
        }

        // Too many arrows for buffer.
        if (sNumBranchArrows >= DISASM_BRANCH_BUFFER_SIZE) {
            return FALSE;
        }

        // Left the function.
        uintptr_t checkAddr = sBranchBufferCurrAddr;
        if (fname != parse_map(&checkAddr)) {
            return FALSE;
        }

        // Get the offset for the current function;
        InsnData insn = (InsnData){ .raw = *(uintptr_t*)sBranchBufferCurrAddr };
        s16 branchOffset = check_for_branch_offset(insn);

        if (branchOffset != 0) { //! TODO: Verify ordering:
            curBranchX += DISASM_BRANCH_ARROW_SPACING;
            curBranchColorIndex = ((curBranchColorIndex + 1) % ARRAY_COUNT(sBranchColors));

            // Wrap around if extended past end of screen.
            if ((DISASM_BRANCH_ARROW_START_X + curBranchX) > CRASH_SCREEN_TEXT_X2) {
                curBranchX = DISASM_BRANCH_ARROW_OFFSET;
            }

            currArrow->startAddr    = sBranchBufferCurrAddr;
            currArrow->branchOffset = branchOffset;
            currArrow->colorIndex   = curBranchColorIndex;
            currArrow->xPos         = curBranchX;

            currArrow++;
            sNumBranchArrows++;
        }

        sBranchBufferCurrAddr += DISASM_STEP;

        // Searching took to long, so continue from the same place on the next frame.
        if ((osGetTime() - startTime) > FRAMES_TO_CYCLES(1)) {
            return TRUE;
        }
    }

    return FALSE;
}
#endif

void disasm_init(void) {
    sDisasmShowDestFunctionNames = TRUE;
    sDisasmShowDataAsBinary      = FALSE;
    gFillBranchBuffer            = FALSE;
    sContinueFillBranchBuffer    = FALSE;
    reset_branch_buffer((uintptr_t)NULL);
}

void draw_branch_arrow(s32 startLine, s32 endLine, s32 dist, RGBA32 color, s32 printLine) {
    // Check to see if arrow is fully away from the screen.
    if (
        ((startLine >= 0              ) || (endLine >= 0              )) &&
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

void disasm_draw_branch_arrows(s32 printLine) {
    // Draw branch arrows from the buffer.
    struct BranchArrow* currArrow = &sBranchArrows[0];

    for (u32 i = 0; i < sNumBranchArrows; i++) {
        s32 startLine = (((s32)currArrow->startAddr - (s32)gScrollAddress) / DISASM_STEP);
        s32 endLine = (startLine + currArrow->branchOffset + 1);

        draw_branch_arrow(startLine, endLine, currArrow->xPos, sBranchColors[currArrow->colorIndex], printLine);

        currArrow++;
    }

    osWritebackDCacheAll();
}

static void print_as_insn(const s32 charX, const s32 charY, const uintptr_t data) {
    InsnData insn = { .raw = data };
#ifndef INCLUDE_DEBUG_MAP
    s16 branchOffset = check_for_branch_offset(insn);
    if (branchOffset != 0) {
        s32 startLine = (((s32)addr - (s32)gScrollAddress) / DISASM_STEP);
        s32 endLine = (startLine + branchOffset + 1);
        draw_branch_arrow(startLine, endLine, DISASM_BRANCH_ARROW_OFFSET, COLOR_RGBA32_CRASH_FUNCTION_NAME_2, line);
    }
#endif
    const char* destFname = NULL;
    const char* insnAsStr = insn_disasm(insn, &destFname, sDisasmShowDestFunctionNames);
    // "[instruction name] [params]"
    crash_screen_print(charX, charY, "%s", insnAsStr);
#ifdef INCLUDE_DEBUG_MAP
    if (sDisasmShowDestFunctionNames && destFname != NULL) {
        // "[function name]"
        crash_screen_print_scroll((charX + TEXT_WIDTH(INSN_NAME_DISPLAY_WIDTH)), charY, (CRASH_SCREEN_NUM_CHARS_X - (INSN_NAME_DISPLAY_WIDTH)),
            STR_COLOR_PREFIX"%s",
            COLOR_RGBA32_CRASH_FUNCTION_NAME, destFname
        );
    }
#endif
}

static void print_as_binary(const s32 charX, const s32 charY, const uintptr_t data) { //! TODO: make this a formatting char, maybe \%b?
    s32 bitX = charX;

    for (u32 c = 0; c < 32; c++) {
        if ((c % 8) == 0) { // Space between each byte.
            bitX += TEXT_WIDTH(1);
        }

        crash_screen_draw_glyph(bitX, charY, (((data >> (32 - c)) & 0b1) ? '1' : '0'), COLOR_RGBA32_WHITE);

        bitX += TEXT_WIDTH(1);
    }
}

void disasm_draw_asm_entries(u32 line, u32 numLines, uintptr_t selectedAddr, uintptr_t pc) {
    u32 charX = TEXT_X(0);
    u32 charY = TEXT_Y(line);

    for (u32 y = 0; y < numLines; y++) {
        uintptr_t addr = (gScrollAddress + (y * DISASM_STEP));
        charY = TEXT_Y(line + y);

        // Draw crash and selection rectangles:
        if (addr == pc) {
            // Draw a red selection rectangle.
            crash_screen_draw_rect((charX - 1), (charY - 2), (CRASH_SCREEN_TEXT_W + 1), (TEXT_HEIGHT(1) + 1), COLOR_RGBA32_CRASH_PC);
            // "<-- CRASH"
            crash_screen_print((CRASH_SCREEN_TEXT_X2 - TEXT_WIDTH(STRLEN("<-- CRASH"))), charY, STR_COLOR_PREFIX"<-- CRASH", COLOR_RGBA32_CRASH_AT);
        } else if (addr == selectedAddr) {
            // Draw a gray selection rectangle.
            crash_screen_draw_rect((charX - 1), (charY - 2), (CRASH_SCREEN_TEXT_W + 1), (TEXT_HEIGHT(1) + 1), COLOR_RGBA32_CRASH_SELECT);
        }

        uintptr_t data = *(uintptr_t*)addr;

        if (is_in_code_segment(addr)) {
            print_as_insn(charX, charY, data);
        } else { // Outside of code segments:
            if (sDisasmShowDataAsBinary) {
                // "bbbbbbbb bbbbbbbb bbbbbbbb bbbbbbbb"
                print_as_binary(charX, charY, data);
            } else {
                // "[XXXXXXXX]"
                crash_screen_print(charX, charY, STR_HEX_WORD, data);
            }
        }
    }

    osWritebackDCacheAll();
}

//! TODO: automatically check page change:
// uintptr_t sCurrFuncAddr = 0x00000000;
// const char* sCurrFuncName = NULL;

void disasm_draw(void) {
    __OSThreadContext* tc = &gCrashedThread->context;
    const char* fname = NULL;
    uintptr_t alignedSelectedAddr = (gSelectedAddress & ~(DISASM_STEP - 1)); // ALIGN4

#ifdef INCLUDE_DEBUG_MAP
    uintptr_t funcAddr = alignedSelectedAddr;
    fname = parse_map(&funcAddr);

    //! TODO: Why doesn't this work outside of the draw function?
    if (gCSSwitchedPage) {
        gFillBranchBuffer = TRUE;
    }

    if (gFillBranchBuffer) {
        gFillBranchBuffer = FALSE;
        reset_branch_buffer(funcAddr);
        sContinueFillBranchBuffer = TRUE;
    }

    if (sContinueFillBranchBuffer) {
        sContinueFillBranchBuffer = crash_screen_fill_branch_buffer(fname, funcAddr);
    }
#endif

    clamp_view_to_selection(DISASM_NUM_ROWS, DISASM_STEP);

    u32 line = 1;

    // "[XXXXXXXX] in [XXXXXXXX]-[XXXXXXXX]"
    crash_screen_print(TEXT_X(STRLEN("DISASM") + 1), TEXT_Y(line),
        (STR_COLOR_PREFIX STR_HEX_WORD" in "STR_HEX_WORD"-"STR_HEX_WORD),
        COLOR_RGBA32_WHITE, alignedSelectedAddr, gScrollAddress, (gScrollAddress + DISASM_SHOWN_SECTION)
    );

    line++;

    if (fname == NULL) {
        // "NOT IN A FUNCTION"
        crash_screen_print(TEXT_X(0), TEXT_Y(line), "NOT IN A FUNCTION");
    } else {
        // "IN: [function name]"
        crash_screen_print(TEXT_X(0), TEXT_Y(line), "IN:");
        crash_screen_print_scroll(TEXT_X(3), TEXT_Y(line),
            (CRASH_SCREEN_NUM_CHARS_X - 3), STR_COLOR_PREFIX"%s",
            COLOR_RGBA32_CRASH_FUNCTION_NAME, fname
        );
    }

    line++;

    osWritebackDCacheAll();

#ifdef INCLUDE_DEBUG_MAP
    disasm_draw_branch_arrows(line);
#endif

    disasm_draw_asm_entries(line, DISASM_NUM_ROWS, alignedSelectedAddr, tc->pc);

    crash_screen_draw_divider(DIVIDER_Y(line));

    u32 line2 = (line + DISASM_NUM_ROWS);

    crash_screen_draw_divider(DIVIDER_Y(line2));

    // Scroll bar
    crash_screen_draw_scroll_bar(DIVIDER_Y(line), DIVIDER_Y(line2), DISASM_SHOWN_SECTION, VALID_RAM_SIZE, (gScrollAddress - DISASM_SCROLL_MIN), 4, COLOR_RGBA32_LIGHT_GRAY);

    // Scroll bar crash position marker
    crash_screen_draw_scroll_bar(DIVIDER_Y(line), DIVIDER_Y(line2), DISASM_SHOWN_SECTION, VALID_RAM_SIZE, (tc->pc - DISASM_SCROLL_MIN), 1, COLOR_RGBA32_CRASH_AT);

    osWritebackDCacheAll();
}

const enum ControlTypes disasmPageControls[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_CURSOR_VERTICAL,
    CONT_DESC_JUMP_TO_ADDRESS,
    CONT_DESC_TOGGLE_FUNCTIONS,
    CONT_DESC_LIST_END,
};

void disasm_input(void) {
#ifdef INCLUDE_DEBUG_MAP
    uintptr_t oldPos = gSelectedAddress;
#endif

    // gSelectedAddress = ALIGN(gSelectedAddress, DISASM_STEP);
    if (gCSDirectionFlags.pressed.up) {
        // Scroll up.
        if ((gSelectedAddress - DISASM_STEP) >= VALID_RAM_START)  {
            gSelectedAddress -= DISASM_STEP;
        }
    }

    if (gCSDirectionFlags.pressed.down) {
        // Scroll down.
        if ((gSelectedAddress + DISASM_STEP) < VALID_RAM_END) {
            gSelectedAddress += DISASM_STEP;
        }
    }

    u16 buttonPressed = gPlayer1Controller->buttonPressed;

    if (buttonPressed & A_BUTTON) { //! TODO: not if address select was just closed
        open_address_select(get_branch_target_from_addr(gSelectedAddress));
    }

    if (buttonPressed & B_BUTTON) {
        sDisasmShowDestFunctionNames ^= TRUE;
        sDisasmShowDataAsBinary ^= TRUE;
    }

#ifdef INCLUDE_DEBUG_MAP
    if (!is_in_same_function(oldPos, gSelectedAddress)) {
        gFillBranchBuffer = TRUE;
    }
#endif
}
