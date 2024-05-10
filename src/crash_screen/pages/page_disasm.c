#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/util/insn_disasm.h"
#include "crash_screen/util/map_parser.h"
#include "crash_screen/util/memory_read.h"
#include "crash_screen/cs_controls.h"
#include "crash_screen/cs_draw.h"
#include "crash_screen/cs_descriptions.h"
#include "crash_screen/cs_main.h"
#include "crash_screen/cs_settings.h"
#include "crash_screen/cs_pages.h"
#include "crash_screen/cs_print.h"

#include "crash_screen/popups/popup_address.h"

#include "page_disasm.h"

#ifdef UNF
#include "usb/usb.h"
#include "usb/debug.h"
#endif // UNF


const char* sValNames_asm_unknowns[] = {
    [DISASM_UNK_MODE_HEX  ] = "HEX",
    [DISASM_UNK_MODE_BIN  ] = "BINARY",
    [DISASM_UNK_MODE_PARSE] = "PARSE",
};

const char* sValNames_branch_arrow[] = {
    [DISASM_ARROW_MODE_OFF      ] = "OFF",
    [DISASM_ARROW_MODE_SELECTION] = "SELECTION",
#ifdef INCLUDE_DEBUG_MAP
    [DISASM_ARROW_MODE_FUNCTION ] = "FUNCTION",
#endif // INCLUDE_DEBUG_MAP
    [DISASM_ARROW_MODE_OVERSCAN ] = "OVERSCAN", //! TODO: Implement this mode.
};

#ifdef INCLUDE_DEBUG_MAP
    #define DISASM_ARROW_MODE_DEFAULT   DISASM_ARROW_MODE_FUNCTION
#else // !INCLUDE_DEBUG_MAP
    #define DISASM_ARROW_MODE_DEFAULT   DISASM_ARROW_MODE_SELECTION
#endif // !INCLUDE_DEBUG_MAP

struct CSSetting cs_settings_group_page_disasm[] = {
    [CS_OPT_HEADER_PAGE_DISASM      ] = { .type = CS_OPT_TYPE_HEADER,  .name = "DISASSEMBLY",                    .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_DISASM_SHOW_RANGE       ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show current address range",     .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_DISASM_SHOW_SYMBOL      ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show current symbol name",       .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_DISASM_UNKNOWNS         ] = { .type = CS_OPT_TYPE_SETTING, .name = "Unknowns handling",              .valNames = &sValNames_asm_unknowns,  .val = DISASM_UNK_MODE_HEX,       .defaultVal = DISASM_UNK_MODE_HEX,       .lowerBound = DISASM_UNK_MODE_HEX,   .upperBound = DISASM_UNK_MODE_PARSE,      },
    [CS_OPT_DISASM_PSEUDOINSNS      ] = { .type = CS_OPT_TYPE_SETTING, .name = "Pseudo-instructions",            .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_DISASM_IMM_FMT          ] = { .type = CS_OPT_TYPE_SETTING, .name = "Immediates format",              .valNames = &gValNames_print_num_fmt, .val = PRINT_NUM_FMT_HEX,         .defaultVal = PRINT_NUM_FMT_HEX,         .lowerBound = PRINT_NUM_FMT_HEX,     .upperBound = PRINT_NUM_FMT_DEC,          },
#ifdef INCLUDE_DEBUG_MAP
    [CS_OPT_DISASM_SYMBOL_HEADERS   ] = { .type = CS_OPT_TYPE_SETTING, .name = "Inline symbol headers",          .valNames = &gValNames_bool,          .val = FALSE,                     .defaultVal = FALSE,                     .lowerBound = FALSE,                 .upperBound = TRUE,                       },
#endif // INCLUDE_DEBUG_MAP
    [CS_OPT_DISASM_OFFSET_ADDR      ] = { .type = CS_OPT_TYPE_SETTING, .name = "Offsets as addresses",           .valNames = &gValNames_bool,          .val = FALSE,                     .defaultVal = FALSE,                     .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_DISASM_ARROW_MODE       ] = { .type = CS_OPT_TYPE_SETTING, .name = "Branch arrow mode",              .valNames = &sValNames_branch_arrow,  .val = DISASM_ARROW_MODE_DEFAULT, .defaultVal = DISASM_ARROW_MODE_DEFAULT, .lowerBound = DISASM_ARROW_MODE_OFF, .upperBound = DISASM_ARROW_MODE_OVERSCAN, },
    [CS_OPT_END_DISASM              ] = { .type = CS_OPT_TYPE_END, },
};


const enum ControlTypes cs_cont_list_disasm[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_PAGE_SELECT,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_HIDE_CRASH_SCREEN,
#ifdef UNF
    CONT_DESC_OS_PRINT,
#endif // UNF
    CONT_DESC_CURSOR_VERTICAL,
    CONT_DESC_JUMP_TO_ADDRESS,
#ifdef INCLUDE_DEBUG_MAP
    CONT_DESC_TOGGLE_FUNCTIONS,
#endif // INCLUDE_DEBUG_MAP
    CONT_DESC_LIST_END,
};


#define DISASM_NUM_SHOWN_ROWS 21

static u32 sDisasmViewportIndex = 0x00000000;
static u32 sDisasmNumShownRows = DISASM_NUM_SHOWN_ROWS;
static u8 sDisasmBranchStartX = 0; // The X position where branch arrows start.

#ifdef INCLUDE_DEBUG_MAP
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

static _Bool sContinueFillBranchBuffer = FALSE;

ALIGNED16 static BranchArrow sBranchArrows[DISASM_BRANCH_BUFFER_SIZE];
static u32 sNumBranchArrows = 0;

static Address sBranchBufferCurrAddr = 0x00000000;

const MapSymbol* sDisasmCurrSymbol = NULL;
const MapSymbol* sDisasmCurrBranchSymbol = NULL;


void reset_branch_buffer(Address funcAddr) {
    bzero(sBranchArrows, sizeof(sBranchArrows));
    sNumBranchArrows = 0;

    sBranchBufferCurrAddr = funcAddr;
}
#endif // INCLUDE_DEBUG_MAP

void page_disasm_init(void) {
    sDisasmViewportIndex = gSelectedAddress;

#ifdef INCLUDE_DEBUG_MAP
    sContinueFillBranchBuffer = FALSE;
    reset_branch_buffer((Address)NULL);
    sDisasmCurrSymbol         = NULL;
    sDisasmCurrBranchSymbol   = NULL;
#endif // INCLUDE_DEBUG_MAP
}

#ifdef INCLUDE_DEBUG_MAP

static u16 addr_to_insn_index(Address startAddr, Address addr) {
    return ((addr - startAddr) / PAGE_DISASM_STEP);
}

static Address insn_index_to_addr(Address startAddr, u16 insnIndex) {
    return ((insnIndex * PAGE_DISASM_STEP) + startAddr);
}

// Set this to SYMBOL_SEARCH_FORWARD for more accuracy when symbol sizes overlap at the cost of performance.
#define BRANCH_ARROW_SYMBOL_SEARCH_DIRECTION SYMBOL_SEARCH_BINARY

//! TODO: Version that checks for branches relative to viewport (overscan).
// @returns whether to continue next frame.
_Bool disasm_fill_branch_buffer(Address funcAddr) {
    const enum CSDisasmUnknownsModes unknownsMode = cs_get_setting_val(CS_OPT_GROUP_PAGE_DISASM, CS_OPT_DISASM_UNKNOWNS);
    const _Bool skipUnknowns = (unknownsMode != DISASM_UNK_MODE_PARSE);

    // Pick up where we left off.
    BranchArrow* currArrow = &sBranchArrows[sNumBranchArrows];

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

        // Check if we have left the function.
        const MapSymbol* symbol = get_map_symbol(sBranchBufferCurrAddr, BRANCH_ARROW_SYMBOL_SEARCH_DIRECTION);
        if (symbol != NULL) {
            if (skipUnknowns && !addr_is_in_text_segment(symbol->addr)) {
                return FALSE;
            }
            if (funcAddr != symbol->addr) {
                return FALSE;
            }
        } else {
            return FALSE;
        }

        // Get the offset for the current function.
        InsnData insn = { .raw = 0x00000000, };
        //! TODO: Does this need to be a safe read? It slows down the search.
        //        Probably required when skipUnknowns is FALSE?
        if (!try_read_word_aligned(&insn.raw, sBranchBufferCurrAddr)) {
            return FALSE;
        }
        s16 branchOffset = insn_check_for_branch_offset(insn);

        if (branchOffset != 0x0000) {
            currArrow->insnIndex    = addr_to_insn_index(symbol->addr, sBranchBufferCurrAddr);
            currArrow->branchOffset = branchOffset;

            currArrow++;
            sNumBranchArrows++;
        }

        sBranchBufferCurrAddr += PAGE_DISASM_STEP;

        // If branch mapping takes longer than a frame, continue from the same place on the next frame.
        //! TODO: Is this needed anymore with binary search? If so, is there a better way to do this?
        if ((osGetTime() - startTime) > FRAMES_TO_CYCLES(1)) {
            return TRUE;
        }
    }

    return FALSE;
}
#endif // INCLUDE_DEBUG_MAP

void draw_branch_arrow(CSTextCoord_s32 startLine, CSTextCoord_s32 endLine, ScreenCoord_s32 dist, RGBA32 color, u32 printLine) {
    s32 numShownRows = sDisasmNumShownRows;

    // Check to see if arrow is fully away from the screen.
    if (
        ((startLine >= 0           ) || (endLine >= 0           )) &&
        ((startLine <  numShownRows) || (endLine <  numShownRows))
    ) {
        ScreenCoord_s32 arrowStartHeight = (TEXT_Y(printLine + startLine) + 3);
        ScreenCoord_s32 arrowEndHeight   = (TEXT_Y(printLine +   endLine) + 3);

        if (startLine < 0) {
            arrowStartHeight = (TEXT_Y(printLine) - 1);
        } else if (startLine >= numShownRows) {
            arrowStartHeight = (TEXT_Y(printLine + numShownRows) - 2);
        } else {
            cs_draw_rect((sDisasmBranchStartX + 1), arrowStartHeight, dist, 1, color);
        }

        if (endLine < 0) {
            arrowEndHeight = (TEXT_Y(printLine) - 1);
        } else if (endLine >= numShownRows) {
            arrowEndHeight = (TEXT_Y(printLine + numShownRows) - 2);
        } else {
            const ScreenCoord_u32 startX = ((sDisasmBranchStartX + dist) - DISASM_BRANCH_ARROW_HEAD_OFFSET);

            cs_draw_triangle(
                (startX - DISASM_BRANCH_ARROW_HEAD_SIZE), (arrowEndHeight - DISASM_BRANCH_ARROW_HEAD_SIZE),
                DISASM_BRANCH_ARROW_HEAD_SIZE, (DISASM_BRANCH_ARROW_HEAD_SIZE * 2),
                color, CS_TRI_LEFT
            );
            cs_draw_rect(
                startX, arrowEndHeight,
                (DISASM_BRANCH_ARROW_HEAD_OFFSET + 1), 1,
                color
            );
        }

        s32 height = absi(arrowEndHeight - arrowStartHeight);

        // Middle of arrow.
        cs_draw_rect((sDisasmBranchStartX + dist), MIN(arrowStartHeight, arrowEndHeight), 1, height, color);
    }
}

void disasm_draw_branch_arrows(u32 printLine, const MapSymbol* symbol) {
    if (!IS_DEBUG_MAP_ENABLED()) {
        return;
    }

    // Draw branch arrows from the buffer.
    BranchArrow* currArrow = &sBranchArrows[0];

    int xPos = sDisasmBranchStartX;

    for (u32 i = 0; i < sNumBranchArrows; i++) {
        // Address startLineAddr = ((currArrow->insnIndex * PAGE_DISASM_STEP) + symbol->addr);
        CSTextCoord_s32 startLine = (((s32)insn_index_to_addr(symbol->addr, currArrow->insnIndex) - (CSTextCoord_s32)sDisasmViewportIndex) / PAGE_DISASM_STEP);
        CSTextCoord_s32 endLine = (startLine + currArrow->branchOffset + 1);

        xPos += (DISASM_BRANCH_ARROW_SPACING + 1);

        // Wrap around if extended past end of screen.
        if ((sDisasmBranchStartX + xPos) > CRASH_SCREEN_TEXT_X2) {
            xPos = (DISASM_BRANCH_ARROW_HEAD_SIZE + DISASM_BRANCH_ARROW_HEAD_OFFSET);
        }

        draw_branch_arrow(startLine, endLine, xPos, sBranchColors[(i % ARRAY_COUNT(sBranchColors))], printLine);

        currArrow++;
    }

    osWritebackDCacheAll();
}

void print_insn(const ScreenCoord_u32 x, const ScreenCoord_u32 y, const char* insnAsStr, const char* destFname) {
    // "[instruction name] [params]"
    cs_print(x, y, "%s", insnAsStr);

#ifdef INCLUDE_DEBUG_MAP
    if (cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_SYMBOL_NAMES) && (destFname != NULL)) {
        // "[function name]"
        cs_print_scroll((x + TEXT_WIDTH(INSN_NAME_DISPLAY_WIDTH)), y,
            (CRASH_SCREEN_NUM_CHARS_X - (INSN_NAME_DISPLAY_WIDTH)),
            STR_COLOR_PREFIX"%s",
            COLOR_RGBA32_CRASH_FUNCTION_NAME, destFname
        );
    }
#endif // INCLUDE_DEBUG_MAP
}

void format_and_print_insn(const ScreenCoord_u32 x, const ScreenCoord_u32 y, const Address addr, const Word data) {
    const char* destFname = NULL;
    const char* insnAsStr = cs_insn_to_string(addr, (InsnData)data, &destFname, TRUE);
    print_insn(x, y, insnAsStr, destFname);
}

void disasm_draw_asm_entries(CSTextCoord_u32 line, CSTextCoord_u32 numLines, Address selectedAddr, Address pc) {
    const enum CSDisasmBranchArrowModes branchArrowMode = cs_get_setting_val(CS_OPT_GROUP_PAGE_DISASM, CS_OPT_DISASM_ARROW_MODE);
    const enum CSDisasmUnknownsModes unknownsMode = cs_get_setting_val(CS_OPT_GROUP_PAGE_DISASM, CS_OPT_DISASM_UNKNOWNS);
#ifdef INCLUDE_DEBUG_MAP
    const _Bool symbolHeaders = cs_get_setting_val(CS_OPT_GROUP_PAGE_DISASM, CS_OPT_DISASM_SYMBOL_HEADERS);
    const MapSymbol* currSymbol = NULL;
    const MapSymbol* prevSymbol = NULL;
#endif // INCLUDE_DEBUG_MAP
    Address addr = sDisasmViewportIndex;

    ScreenCoord_u32 x = TEXT_X(0);
    ScreenCoord_u32 y = TEXT_Y(line);

    for (CSTextCoord_u32 row = 0; row < numLines; row++) {
        y = TEXT_Y(line + row);

#ifdef INCLUDE_DEBUG_MAP
        // Translucent divider between symbols.
        currSymbol = get_map_symbol(addr, SYMBOL_SEARCH_BINARY);
        if ((addr >= PAGE_DISASM_STEP) && (currSymbol != prevSymbol)) {
            cs_draw_divider_translucent(DIVIDER_Y(line + row));
        }
        if (symbolHeaders && (currSymbol != NULL) && (addr == currSymbol->addr)) {
            //! TODO: This allows the selection cursor to go beyond the bottom of the screen.
            CSTextCoord_u32 numChars = cs_print_symbol_name(x, y, CRASH_SCREEN_NUM_CHARS_X, currSymbol, TRUE);
            cs_print(x + TEXT_WIDTH(numChars), y, ":");
            row++;
            y = TEXT_Y(line + row);
        }
        prevSymbol = currSymbol;
#endif // INCLUDE_DEBUG_MAP

        // Draw crash and selection rectangles:
        if (addr == pc) {
            // Draw a red selection rectangle.
            cs_draw_row_crash_box(y);
            // "<-- CRASH"
            cs_print((CRASH_SCREEN_TEXT_X2 - TEXT_WIDTH(STRLEN("<-- CRASH"))), y, STR_COLOR_PREFIX"<-- CRASH", COLOR_RGBA32_CRASH_AT);
        }
        if (addr == selectedAddr) {
            // Draw a gray selection rectangle.
            cs_draw_row_selection_box(y);
        }

        Word data = 0x00000000;
        if (!try_read_word_aligned(&data, addr)) {
            cs_print(x, y, (STR_COLOR_PREFIX"*"), COLOR_RGBA32_CRASH_OUT_OF_BOUNDS);
        } else {
            if ((unknownsMode == DISASM_UNK_MODE_PARSE) || addr_is_in_text_segment(addr)) {
                format_and_print_insn(x, y, addr, data);

                if ((addr == selectedAddr) && (branchArrowMode == DISASM_ARROW_MODE_SELECTION)) {
                    InsnData insn = {
                        .raw = data,
                    };
                    s16 branchOffset = insn_check_for_branch_offset(insn);

                    if (branchOffset != 0x0000) {
                        draw_branch_arrow(row, (row + branchOffset + 1), (DISASM_BRANCH_ARROW_HEAD_SIZE + DISASM_BRANCH_ARROW_HEAD_OFFSET), COLOR_RGBA32_ORANGE, line);
                    }
                }
            } else { // Outside of code segments:
                RGBA32 color = COLOR_RGBA32_WHITE;
#ifdef INCLUDE_DEBUG_MAP
                if (currSymbol != NULL) {
                    color = COLOR_RGBA32_CRASH_VARIABLE;
                }
#endif // INCLUDE_DEBUG_MAP
                if (unknownsMode == DISASM_UNK_MODE_BIN) {
                    // "bbbbbbbb bbbbbbbb bbbbbbbb bbbbbbbb"
                    print_data_as_binary(x, y, &data, sizeof(data), color);
                    // cs_print(x, y, "%032b", data);
                } else {
                    // "[XXXXXXXX]"
                    cs_print(x, y, (STR_COLOR_PREFIX STR_HEX_WORD), color, data);
                }
            }
        }

        addr += PAGE_DISASM_STEP;
    }

    osWritebackDCacheAll();
}

//! TODO: automatically check page/address change:
// Address sCurrFuncAddr = 0x00000000;
// const char* sCurrFuncName = NULL;

void page_disasm_draw(void) {
    __OSThreadContext* tc = &gInspectThread->context;
    Address alignedSelectedAddr = ALIGNFLOOR(gSelectedAddress, PAGE_DISASM_STEP);
    Address epc = GET_EPC(tc);

    sDisasmNumShownRows = DISASM_NUM_SHOWN_ROWS;
    const _Bool showCurrentRange  = cs_get_setting_val(CS_OPT_GROUP_PAGE_DISASM, CS_OPT_DISASM_SHOW_RANGE);
    sDisasmNumShownRows -= showCurrentRange;
    const _Bool showCurrentSymbol = cs_get_setting_val(CS_OPT_GROUP_PAGE_DISASM, CS_OPT_DISASM_SHOW_SYMBOL);
    sDisasmNumShownRows -= showCurrentSymbol;

    sDisasmBranchStartX = (DISASM_BRANCH_ARROW_HEAD_SIZE + DISASM_BRANCH_ARROW_HEAD_OFFSET) +
                        cs_get_setting_val(CS_OPT_GROUP_PAGE_DISASM, CS_OPT_DISASM_OFFSET_ADDR)
                        ? TEXT_X(INSN_NAME_DISPLAY_WIDTH + STRLEN("R0, R0, 0x80XXXXXX"))
                        : TEXT_X(INSN_NAME_DISPLAY_WIDTH + STRLEN("R0, R0, +0x0000"));

    CSTextCoord_u32 line = 1;

    Address startAddr = sDisasmViewportIndex;
    Address endAddr   = (startAddr + ((sDisasmNumShownRows - 1) * PAGE_DISASM_STEP));

    if (showCurrentRange) {
        // "[XXXXXXXX] in [XXXXXXXX]-[XXXXXXXX]"
        cs_print(TEXT_X(0), TEXT_Y(line),
            (STR_COLOR_PREFIX STR_HEX_WORD" in "STR_HEX_WORD"-"STR_HEX_WORD),
            COLOR_RGBA32_WHITE, alignedSelectedAddr, startAddr, endAddr
        );
        line++;
    }

    if (showCurrentSymbol) {
        cs_print_addr_location_info(TEXT_X(0), TEXT_Y(line), CRASH_SCREEN_NUM_CHARS_X, alignedSelectedAddr, TRUE);
        line++;
    }

#ifdef INCLUDE_DEBUG_MAP
    if (cs_get_setting_val(CS_OPT_GROUP_PAGE_DISASM, CS_OPT_DISASM_ARROW_MODE) == DISASM_ARROW_MODE_FUNCTION) {
        const MapSymbol* symbol = get_map_symbol(alignedSelectedAddr, SYMBOL_SEARCH_BACKWARD);
        if (symbol != NULL) {
            disasm_draw_branch_arrows(line, symbol);
        }
        sDisasmCurrSymbol = symbol;
    }
#endif // INCLUDE_DEBUG_MAP

    disasm_draw_asm_entries(line, sDisasmNumShownRows, alignedSelectedAddr, epc);

    if (showCurrentRange || showCurrentSymbol) {
        cs_draw_divider(DIVIDER_Y(line));
    }

    CSTextCoord_u32 line2 = (line + sDisasmNumShownRows);

    cs_draw_divider(DIVIDER_Y(line2));

    ScreenCoord_u32 scrollTop = (DIVIDER_Y(line) + 1);
    ScreenCoord_u32 scrollBottom = DIVIDER_Y(line2);

    const size_t shownSection = ((sDisasmNumShownRows - 1) * PAGE_DISASM_STEP);

    // Scroll bar:
    cs_draw_scroll_bar(
        scrollTop, scrollBottom,
        shownSection, VIEW_MEM_SIZE,
        (sDisasmViewportIndex - VIEW_MEM_START),
        COLOR_RGBA32_CRASH_SCROLL_BAR, TRUE
    );

    // Scroll bar crash position marker:
    cs_draw_scroll_bar(
        scrollTop, scrollBottom,
        shownSection, VIEW_MEM_SIZE,
        (epc - VIEW_MEM_START),
        COLOR_RGBA32_CRASH_AT, FALSE
    );

    osWritebackDCacheAll();
}

static void disasm_move_up(void) {
    gSelectedAddress = ALIGNFLOOR(gSelectedAddress, PAGE_DISASM_STEP);
    // Scroll up.
    if (gSelectedAddress >= (VIEW_MEM_START + PAGE_DISASM_STEP))  {
        gSelectedAddress -= PAGE_DISASM_STEP;
    }
}

static void disasm_move_down(void) {
    gSelectedAddress = ALIGNFLOOR(gSelectedAddress, PAGE_DISASM_STEP);
    // Scroll down.
    if (gSelectedAddress <= (VIEW_MEM_END - PAGE_DISASM_STEP)) {
        gSelectedAddress += PAGE_DISASM_STEP;
    }
}

void page_disasm_input(void) {
    if (gCSDirectionFlags.pressed.up) {
        disasm_move_up();
    }

    if (gCSDirectionFlags.pressed.down) {
        disasm_move_down();
    }

    u16 buttonPressed = gCSCompositeController->buttonPressed;
    if (buttonPressed & A_BUTTON) {
        open_address_select(get_insn_branch_target_from_addr(gSelectedAddress));
    }

    sDisasmViewportIndex = cs_clamp_view_to_selection(sDisasmViewportIndex, gSelectedAddress, sDisasmNumShownRows, PAGE_DISASM_STEP);

#ifdef INCLUDE_DEBUG_MAP
    if (cs_get_setting_val(CS_OPT_GROUP_PAGE_DISASM, CS_OPT_DISASM_ARROW_MODE) == DISASM_ARROW_MODE_FUNCTION) {
        Address alignedSelectedAddress = ALIGNFLOOR(gSelectedAddress, PAGE_DISASM_STEP);
        const MapSymbol* symbol = get_map_symbol(alignedSelectedAddress, BRANCH_ARROW_SYMBOL_SEARCH_DIRECTION);
        _Bool startFillBranchBuffer = FALSE;

        if (sDisasmCurrBranchSymbol != symbol) {
            sDisasmCurrBranchSymbol = symbol;
            startFillBranchBuffer = TRUE;
        }

        if (symbol != NULL) {
            if (startFillBranchBuffer) {
                startFillBranchBuffer = FALSE;
                reset_branch_buffer(symbol->addr);
                sContinueFillBranchBuffer = TRUE;
            }

            if (sContinueFillBranchBuffer) {
                sContinueFillBranchBuffer = disasm_fill_branch_buffer(symbol->addr);
            }
        } else {
            reset_branch_buffer(alignedSelectedAddress);
            sContinueFillBranchBuffer = FALSE;
        }
    }
#endif // INCLUDE_DEBUG_MAP
}

void page_disasm_print(void) {
#ifdef UNF
    osSyncPrintf("\n");

    Address startAddr = sDisasmViewportIndex;
    Address endAddr   = (startAddr + ((sDisasmNumShownRows - 1) * PAGE_DISASM_STEP));

    osSyncPrintf("SECTION: ["STR_HEX_WORD"-"STR_HEX_WORD"]\n", startAddr, endAddr);

    for (u32 row = 0; row < sDisasmNumShownRows; row++) {
        Address addr = (startAddr + (row * PAGE_DISASM_STEP));
        osSyncPrintf("- ["STR_HEX_WORD"]: ", addr);
        Word data = 0x00000000;
        if (!try_read_word_aligned(&data, addr)) {
            osSyncPrintf("*");
        } else if (addr_is_in_text_segment(addr)) {
            const char* destFname = NULL;
            const char* insnAsStr = cs_insn_to_string(addr, (InsnData)data, &destFname, FALSE);

            osSyncPrintf("%s", insnAsStr);
 #ifdef INCLUDE_DEBUG_MAP
            if (cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_SYMBOL_NAMES) && (destFname != NULL)) {
                osSyncPrintf("%s", destFname);
            }
 #endif // INCLUDE_DEBUG_MAP
            __OSThreadContext* tc = &gInspectThread->context;
            if (addr == GET_EPC(tc)) {
                osSyncPrintf("<-- CRASH");
            }
        } else { // Outside of code segments:
            osSyncPrintf(STR_HEX_WORD, data);
        }

        osSyncPrintf("\n");
    }
#endif // UNF
}


struct CSPage gCSPage_disasm = {
    .name         = "DISASSEMBLY",
    .initFunc     = page_disasm_init,
    .drawFunc     = page_disasm_draw,
    .inputFunc    = page_disasm_input,
    .printFunc    = page_disasm_print,
    .contList     = cs_cont_list_disasm,
    .settingsList = cs_settings_group_page_disasm,
    .flags = {
        .initialized = FALSE,
        .crashed     = FALSE,
    },
};
