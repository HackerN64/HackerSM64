#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/util/floats.h"
#include "crash_screen/util/memory_read.h"
#include "crash_screen/cs_controls.h"
#include "crash_screen/cs_draw.h"
#include "crash_screen/cs_descriptions.h"
#include "crash_screen/cs_main.h"
#include "crash_screen/cs_pages.h"
#include "crash_screen/cs_print.h"
#include "crash_screen/cs_settings.h"

#include "crash_screen/popups/popup_address.h"

#include "page_memory.h"

#ifdef UNF
#include "usb/usb.h"
#include "usb/debug.h"
#endif // UNF


enum MemoryDisplayModes {
    MEMORY_MODE_HEX,
    MEMORY_MODE_ASCII,
    MEMORY_MODE_BINARY,
    MEMORY_MODE_RGBA16,
    MEMORY_MODE_RGBA32,
    MEMORY_MODE_SYMBOL,
    MEMORY_MODE_F32,
    NUM_MEMORY_VIEW_MODES,
};

const char* gValNames_mem_disp_mode[NUM_MEMORY_VIEW_MODES] = {
    [MEMORY_MODE_HEX   ] = "HEX",
    [MEMORY_MODE_ASCII ] = "ASCII",
    [MEMORY_MODE_BINARY] = "BINARY",
    [MEMORY_MODE_RGBA16] = "RGBA16",
    [MEMORY_MODE_RGBA32] = "RGBA32",
    [MEMORY_MODE_SYMBOL] = "SYMBOLS",
    [MEMORY_MODE_F32   ] = "F32",
};


struct CSSetting cs_settings_group_page_memory[] = {
    [CS_OPT_HEADER_PAGE_MEMORY      ] = { .type = CS_OPT_TYPE_HEADER,  .name = "MEMORY",                         .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                        },
    [CS_OPT_MEMORY_SHOW_RANGE       ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show current address range",     .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                        },
#ifdef INCLUDE_DEBUG_MAP
    [CS_OPT_MEMORY_SYMBOL_DIVIDERS  ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show symbol dividers",           .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                        },
    [CS_OPT_MEMORY_SHOW_SYMBOL      ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show current symbol name",       .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                        },
#endif // INCLUDE_DEBUG_MAP
    [CS_OPT_MEMORY_DISPLAY_MODE     ] = { .type = CS_OPT_TYPE_SETTING, .name = "Display mode",                   .valNames = &gValNames_mem_disp_mode, .val = MEMORY_MODE_HEX,           .defaultVal = MEMORY_MODE_HEX,           .lowerBound = MEMORY_MODE_HEX,       .upperBound = (NUM_MEMORY_VIEW_MODES - 1), },
    [CS_OPT_END_MEMORY              ] = { .type = CS_OPT_TYPE_END, },
};


const enum ControlTypes cs_cont_list_memory[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_PAGE_SELECT,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_HIDE_CRASH_SCREEN,
#ifdef UNF
    CONT_DESC_OS_PRINT,
#endif // UNF
    CONT_DESC_CURSOR,
    CONT_DESC_JUMP_TO_ADDRESS,
    CONT_DESC_TOGGLE_ASCII,
    CONT_DESC_LIST_END,
};


#define MEMORY_NUM_SHOWN_ROWS 20


static Address sRamViewViewportIndex = 0x00000000;
static u32 sRamViewNumShownRows = MEMORY_NUM_SHOWN_ROWS;

static const char gHex[0x10] = "0123456789ABCDEF";
#ifdef UNF
static u32 sMemoryViewData[MEMORY_NUM_SHOWN_ROWS][4];
#endif // UNF


void page_memory_init(void) {
    sRamViewViewportIndex = gSelectedAddress;
}

void cs_memory_draw_byte() {

}

void ram_viewer_print_data(CSTextCoord_u32 line, Address startAddr) {
    const enum MemoryDisplayModes mode = cs_get_setting_val(CS_OPT_GROUP_PAGE_MEMORY, CS_OPT_MEMORY_DISPLAY_MODE);
    const _Bool symbolDividers = cs_get_setting_val(CS_OPT_GROUP_PAGE_MEMORY, CS_OPT_MEMORY_SYMBOL_DIVIDERS);
    __OSThreadContext* tc = &gInspectThread->context;
    ScreenCoord_u32 x = (TEXT_X(SIZEOF_HEX(Address)) + 3);
    ScreenCoord_u32 y = TEXT_Y(line);

#ifdef UNF
    bzero(&sMemoryViewData, sizeof(sMemoryViewData));
#endif // UNF

//! TODO: Save symbol from prev byte in loop instead of searching twice:
// #ifdef INCLUDE_DEBUG_MAP
//     const MapSymbol* currSymbol = NULL;
//     const MapSymbol* prevByteSymbol = NULL;
//     const MapSymbol* prevRowSymbol = NULL;
// #endif // INCLUDE_DEBUG_MAP

    // Rows on screen:
    for (CSTextCoord_u32 row = 0; row < sRamViewNumShownRows; row++) {
        Address rowAddr = (startAddr + (row * PAGE_MEMORY_STEP));

        // Row header:
        // "[XXXXXXXX]"
        cs_print(TEXT_X(0), TEXT_Y(line + row), (STR_COLOR_PREFIX STR_HEX_WORD),
            ((row % 2) ? COLOR_RGBA32_CRASH_MEMORY_ROW1 : COLOR_RGBA32_CRASH_MEMORY_ROW2), rowAddr
        );

        x = (TEXT_X(SIZEOF_HEX(Word)) + 3);
        y = TEXT_Y(line + row);

        _Bool isColor = ((mode == MEMORY_MODE_RGBA16) || (mode == MEMORY_MODE_RGBA32));
        // if (!isColor) {
        //     // RGBA32 highlightColor = addr_is_in_text_segment(rowAddr) ? RGBA32_SET_ALPHA(COLOR_RGBA32_CRASH_FUNCTION_NAME, 0x3F) : RGBA32_SET_ALPHA(COLOR_RGBA32_CRASH_VARIABLE, 0x3F);
        //     RGBA32 highlightColor = addr_is_in_text_segment(rowAddr) ? COLOR_RGBA32_CRASH_FUNCTION_NAME : COLOR_RGBA32_CRASH_VARIABLE;
        //     cs_draw_rect(x, (y - 2), (TEXT_WIDTH(CRASH_SCREEN_NUM_CHARS_X - 9) + 5), TEXT_HEIGHT(1), highlightColor);
        // }

        // 4 Words per row:
        for (size_t wordOffset = 0; wordOffset < PAGE_MEMORY_WORDS_PER_ROW; wordOffset++) {
            Word_4Bytes data = {
                .word = 0x00000000,
            };
            Address currAddrAligned = (rowAddr + (wordOffset * sizeof(Word)));
            _Bool valid = try_read_word_aligned(&data.word, currAddrAligned);

#ifdef UNF
            if (valid) {
                sMemoryViewData[row][wordOffset] = data.word;
            }
#endif // UNF

            x += 2;

            const MapSymbol* destSymbol = NULL;
            // Modes that apply to the whole 4-byte Word:
            switch (mode) {
                case MEMORY_MODE_SYMBOL:
                    if (IS_DEBUG_MAP_ENABLED() && is_valid_ram_addr(data.word)) {
                        destSymbol = get_map_symbol(data.word, SYMBOL_SEARCH_BINARY);
                        if (destSymbol != NULL) {
                            cs_print_symbol_name((x - 1), y, 9, destSymbol, FALSE);
                        }
                    }
                    break;
                case MEMORY_MODE_F32:;
                    //! TODO: Setting to change alignment?
                    IEEE754_f32 f = { .asU32 = data.word, };
                    if (validate_f32(f) == FLT_ERR_NONE) {
                        cs_print(x, y, "% .3g", f.asF32);
                    }
                    break;
                //! TODO: RGBA32 and RGBA16?
                default:
                    break;
            }

            // 4 bytes per Word:
            for (size_t byteOffset = 0; byteOffset < sizeof(Word); byteOffset++) {
                Address currAddr = (currAddrAligned + byteOffset);

                RGBA32 textColor = (((mode == MEMORY_MODE_ASCII) || (byteOffset % 2)) ? COLOR_RGBA32_CRASH_MEMORY_DATA1 : COLOR_RGBA32_CRASH_MEMORY_DATA2);
                RGBA32 selectColor = COLOR_RGBA32_NONE;

                if (currAddr == gSelectedAddress) {
                    selectColor = COLOR_RGBA32_CRASH_MEMORY_SELECT;
                    textColor = RGBA32_INVERT(textColor);
                } else if (currAddr == GET_EPC(tc)) {
                    selectColor = COLOR_RGBA32_CRASH_MEMORY_PC;
                }
                _Bool selected = (selectColor != COLOR_RGBA32_NONE);

#ifdef INCLUDE_DEBUG_MAP
                // Draw symbol separator lines:
                if (symbolDividers) {
                    if (currAddr >= sizeof(Byte)) {
                        const MapSymbol *currSymbol = get_map_symbol(currAddr, SYMBOL_SEARCH_BINARY);
                        // const RGBA32 dividerColor = COLOR_RGBA32_GRAY;//  addr_is_in_text_segment(currAddr) ? RGBA32_SET_ALPHA(COLOR_RGBA32_CRASH_FUNCTION_NAME, 0x7F) : RGBA32_SET_ALPHA(COLOR_RGBA32_CRASH_VARIABLE, 0x7F);
                        const RGBA32 dividerColor = addr_is_in_text_segment(currAddr) ? COLOR_RGBA32_CRASH_FUNCTION_NAME : COLOR_RGBA32_CRASH_VARIABLE;
                        _Bool aligned0 = ((currAddr % sizeof(Word)) == 0); // (byteOffset == 0);
                        _Bool aligned3 = ((currAddr % sizeof(Word)) == (sizeof(Word) - 1)); // (byteOffset == (sizeof(Word) - 1));
                        // Prev byte:
                        const MapSymbol *otherSymbol = get_map_symbol((currAddr - sizeof(Byte)), SYMBOL_SEARCH_BINARY);
                        if (currSymbol != otherSymbol) {
                            cs_draw_rect(((x - 2) - aligned0), (y - 2), 2, (TEXT_HEIGHT(1) + 1), dividerColor);
                            if ((row != 0) && (wordOffset == 0) && aligned0) {
                                cs_draw_rect((CRASH_SCREEN_X2 - 2), ((y - TEXT_HEIGHT(1)) - 2), 1, (TEXT_HEIGHT(1) + 1), dividerColor);
                            }
                        }
                        // Prev row:
                        if (currAddr >= PAGE_MEMORY_STEP) {
                            otherSymbol = get_map_symbol((currAddr - PAGE_MEMORY_STEP), SYMBOL_SEARCH_BINARY);
                            if (currSymbol != otherSymbol) {
                                cs_draw_rect(((x - 2) - aligned0), (y - 2), (TEXT_WIDTH(2) + 1 + aligned0 + aligned3), 1, dividerColor);
                            }
                        }
                    }
                }
#endif // INCLUDE_DEBUG_MAP

                // If the display mode isn't a color, draw a solid box behind the data.
                if (selected && !isColor) {
                    cs_draw_rect((x - 1), (y - 1), (TEXT_WIDTH(2) + 1), (TEXT_HEIGHT(1) - 1), selectColor);
                }

                if (valid) {
                    Byte byte = data.byte[byteOffset];
                    switch (mode) {
                        case MEMORY_MODE_SYMBOL:
                        case MEMORY_MODE_HEX:
                            if (destSymbol == NULL) {
                                cs_draw_glyph((x + TEXT_WIDTH(0)), y, gHex[byte >> BITS_PER_HEX], textColor);
                                cs_draw_glyph((x + TEXT_WIDTH(1)), y, gHex[byte & BITMASK(BITS_PER_HEX)], textColor);
                            }
                            break;
                        case MEMORY_MODE_ASCII:
                            cs_draw_glyph((x + TEXT_WIDTH(1)), y, byte, textColor);
                            break;
                        case MEMORY_MODE_BINARY:;
                            ScreenCoord_u32 bitX = 0;
                            for (Byte bit = 0; bit < BITS_PER_BYTE; bit++) {
                                RGBA32 color = (((byte >> ((BITS_PER_BYTE - 1) - bit)) & 0b1) ? COLOR_RGBA32_LIGHT_GRAY : COLOR_RGBA32_DARK_GRAY);
                                cs_draw_rect((x + bitX), y, 1, CRASH_SCREEN_FONT_CHAR_HEIGHT, color);
                                bitX += (1 + (bit & 0x1));
                            }
                            break;
                        case MEMORY_MODE_RGBA16:;
                            RGBA16 color = data.halfword[byteOffset > 1];
                            cs_draw_rect((x - 1), (y - 1), (TEXT_WIDTH(2) + 1), (TEXT_HEIGHT(1) - 1), RGBA16_TO_RGBA32(color));
                            break;
                        case MEMORY_MODE_RGBA32:
                            cs_draw_rect((x - 1), (y - 1), (TEXT_WIDTH(2) + 1), (TEXT_HEIGHT(1) - 1), data.word);
                            break;
                        case MEMORY_MODE_F32:
                        default:
                            break;
                    }
                } else {
                    cs_draw_glyph((x + TEXT_WIDTH(1)), y, '*', COLOR_RGBA32_CRASH_OUT_OF_BOUNDS);
                }

                // If the display mode is a color, draw a translucent box on top of the data.
                if (selected && isColor) {
                    cs_draw_rect((x - 1), (y - 1), (TEXT_WIDTH(2) + 1), (TEXT_HEIGHT(1) - 1), RGBA32_SET_ALPHA(selectColor, 0x7F));
                }

                x += (TEXT_WIDTH(2) + 1);
            }
        }
    }
}

void page_memory_draw(void) {
    __OSThreadContext* tc = &gInspectThread->context;

    sRamViewNumShownRows = MEMORY_NUM_SHOWN_ROWS;
    const _Bool showCurrentRange  = cs_get_setting_val(CS_OPT_GROUP_PAGE_MEMORY, CS_OPT_MEMORY_SHOW_RANGE);
    sRamViewNumShownRows -= showCurrentRange;
    const _Bool showCurrentSymbol = cs_get_setting_val(CS_OPT_GROUP_PAGE_MEMORY, CS_OPT_MEMORY_SHOW_SYMBOL);
    sRamViewNumShownRows -= showCurrentSymbol;

    CSTextCoord_u32 line = 1;

    Address startAddr = sRamViewViewportIndex;
    Address endAddr = (startAddr + ((sRamViewNumShownRows - 1) * PAGE_MEMORY_STEP));

    if (showCurrentRange) {
        // "[XXXXXXXX] in [XXXXXXXX]-[XXXXXXXX]"
        cs_print(TEXT_X(0), TEXT_Y(line),
            (STR_COLOR_PREFIX STR_HEX_WORD" in "STR_HEX_WORD"-"STR_HEX_WORD),
            COLOR_RGBA32_WHITE, gSelectedAddress, startAddr, endAddr
        );
        line++;
    }

    if (showCurrentSymbol) {
        cs_print_addr_location_info(TEXT_X(0), TEXT_Y(line), CRASH_SCREEN_NUM_CHARS_X, gSelectedAddress, TRUE);
        line++;
    }

    if (showCurrentRange || showCurrentSymbol) {
        cs_draw_divider(DIVIDER_Y(line));
    }

    ScreenCoord_u32 x = (TEXT_X(SIZEOF_HEX(Address)) + 3);

    // Print column headers:
    for (u32 i = 0; i < (4 * sizeof(Word)); i++) {
        if ((i % 4) == 0) {
            x += 2;
        }

        // "[XX]"
        cs_print(x, TEXT_Y(line), (STR_COLOR_PREFIX STR_HEX_BYTE), ((i % 2) ? COLOR_RGBA32_CRASH_MEMORY_COL1 : COLOR_RGBA32_CRASH_MEMORY_COL2), i);

        x += (TEXT_WIDTH(2) + 1);
    }

    ram_viewer_print_data((line + 1), startAddr);

    // Veertical divider
    cs_draw_rect((TEXT_X(SIZEOF_HEX(Address)) + 2), DIVIDER_Y(line), 1, TEXT_HEIGHT(sRamViewNumShownRows + 1), COLOR_RGBA32_CRASH_DIVIDER);

    // "MEMORY"
    cs_print(TEXT_X(1), TEXT_Y(line), "MEMORY");

    line++;

    cs_draw_divider(DIVIDER_Y(line));

    CSTextCoord_u32 line2 = (line + sRamViewNumShownRows);

    cs_draw_divider(DIVIDER_Y(line2));

    ScreenCoord_u32 scrollTop = (DIVIDER_Y(line) + 1);
    ScreenCoord_u32 scrollBottom = DIVIDER_Y(line2);

    const size_t shownSection = ((sRamViewNumShownRows - 1) * PAGE_MEMORY_STEP);

    // Scroll bar:
    cs_draw_scroll_bar(
        scrollTop, scrollBottom,
        shownSection, VIEW_MEM_SIZE,
        (sRamViewViewportIndex - VIEW_MEM_START),
        COLOR_RGBA32_CRASH_SCROLL_BAR, TRUE
    );

    // Scroll bar crash position marker:
    cs_draw_scroll_bar(
        scrollTop, scrollBottom,
        shownSection, VIEW_MEM_SIZE,
        (GET_EPC(tc) - VIEW_MEM_START),
        COLOR_RGBA32_CRASH_AT, FALSE
    );

    osWritebackDCacheAll();
}

void page_memory_input(void) {
    if (gCSDirectionFlags.pressed.up) {
        // Scroll up.
        if (gSelectedAddress >= (VIEW_MEM_START + PAGE_MEMORY_STEP)) {
            gSelectedAddress -= PAGE_MEMORY_STEP;
        }
    }

    if (gCSDirectionFlags.pressed.down) {
        // Scroll down.
        if (gSelectedAddress <= (VIEW_MEM_END - PAGE_MEMORY_STEP)) {
            gSelectedAddress += PAGE_MEMORY_STEP;
        }
    }

    if (gCSDirectionFlags.pressed.left) {
        // Prevent wrapping.
        if (((gSelectedAddress - 1) & BITMASK(4)) != 0xF) {
            gSelectedAddress--;
        }
    }

    if (gCSDirectionFlags.pressed.right) {
        // Prevent wrapping.
        if (((gSelectedAddress + 1) & BITMASK(4)) != 0x0) {
            gSelectedAddress++;
        }
    }

    u16 buttonPressed = gCSCompositeController->buttonPressed;

    if (buttonPressed & A_BUTTON) {
        Word dataAtAddr = 0x00000000;
        if (try_read_word_aligned(&dataAtAddr, gSelectedAddress) && is_valid_ram_addr(dataAtAddr)) {
            open_address_select(dataAtAddr);
        } else {
            open_address_select(gSelectedAddress);
        }
    }

    if (buttonPressed & B_BUTTON) {
        cs_inc_setting(CS_OPT_GROUP_PAGE_MEMORY, CS_OPT_MEMORY_DISPLAY_MODE, 1);
    }

    sRamViewViewportIndex = cs_clamp_view_to_selection(sRamViewViewportIndex, gSelectedAddress, sRamViewNumShownRows, PAGE_MEMORY_STEP);
}

void page_memory_print(void) {
#ifdef UNF
    osSyncPrintf("\n");

    Address startAddr = sRamViewViewportIndex;
    Address endAddr = (startAddr + ((sRamViewNumShownRows - 1) * PAGE_MEMORY_STEP));

    osSyncPrintf("- SECTION: ["STR_HEX_WORD"-"STR_HEX_WORD"]\n", startAddr, endAddr);

    for (u32 row = 0; row < sRamViewNumShownRows; row++) {
        osSyncPrintf("- ["STR_HEX_WORD"]:", (startAddr + (row * PAGE_MEMORY_STEP))); // Row address.

        for (u32 wordOffset = 0; wordOffset < 4; wordOffset++) {
            osSyncPrintf(" "STR_HEX_WORD, sMemoryViewData[row][wordOffset]);
        }

        osSyncPrintf("\n");
    }
#endif // UNF
}


struct CSPage gCSPage_memory = {
    .name         = "MEMORY VIEW",
    .initFunc     = page_memory_init,
    .drawFunc     = page_memory_draw,
    .inputFunc    = page_memory_input,
    .printFunc    = page_memory_print,
    .contList     = cs_cont_list_memory,
    .settingsList = cs_settings_group_page_memory,
    .flags = {
        .initialized = FALSE,
        .crashed     = FALSE,
    },
};
