#include <ultra64.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen/crash_main.h"
#include "page_memory.h"
#include "engine/colors.h"
#include "game/input.h"


static Address sRamViewViewportIndex = 0x00000000;


const enum ControlTypes ramViewerContList[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_CURSOR,
    CONT_DESC_JUMP_TO_ADDRESS,
    CONT_DESC_TOGGLE_ASCII,
    CONT_DESC_LIST_END,
};


static const char gHex[0x10] = "0123456789ABCDEF";


void ram_view_init(void) {
    sRamViewViewportIndex = gSelectedAddress;
}

static void print_byte(u32 x, u32 y, Byte byte, RGBA32 color) {
    // "[XX]"
    if (gCSSettings[CS_OPT_MEMORY_AS_ASCII].val) {
        crash_screen_draw_glyph((x + TEXT_WIDTH(1)), y, byte, color);
    } else {
        // Faster than doing crash_screen_print:
        crash_screen_draw_glyph((x + TEXT_WIDTH(0)), y, gHex[byte >> 4], color);
        crash_screen_draw_glyph((x + TEXT_WIDTH(1)), y, gHex[byte & 0xF], color);
    }
}

static void ram_viewer_print_data(u32 line, Address startAddr) {
    __OSThreadContext* tc = &gCrashedThread->context;
    u32 charX = (TEXT_X(SIZEOF_HEX(Address)) + 3);
    u32 charY = TEXT_Y(line);

    for (u32 y = 0; y < RAM_VIEWER_NUM_ROWS; y++) {
        Address rowAddr = (startAddr + (y * RAM_VIEWER_STEP));

        // Row header:
        // "[XXXXXXXX]"
        crash_screen_print(TEXT_X(0), TEXT_Y(line + y), (STR_COLOR_PREFIX STR_HEX_WORD),
            ((y % 2) ? COLOR_RGBA32_CRASH_MEMORY_ROW1 : COLOR_RGBA32_CRASH_MEMORY_ROW2), rowAddr
        );

        charX = (TEXT_X(SIZEOF_HEX(Word)) + 3);
        charY = TEXT_Y(line + y);
        for (u32 x = 0; x < (4 * sizeof(Word)); x++) {
            Address currAddr = (rowAddr + x);

            if ((x % 4) == 0) {
                charX += 2;
            }

            RGBA32 textColor = ((gCSSettings[CS_OPT_MEMORY_AS_ASCII].val || (x % 2)) ? COLOR_RGBA32_CRASH_MEMORY_DATA1 : COLOR_RGBA32_CRASH_MEMORY_DATA2);
            RGBA32 selectColor = COLOR_RGBA32_NONE;

            if (currAddr == gSelectedAddress) {
                selectColor = COLOR_RGBA32_CRASH_MEMORY_SELECT;
                textColor = RGBA32_INVERT(textColor);
            } else if (currAddr == tc->pc) {
                selectColor = COLOR_RGBA32_CRASH_MEMORY_PC;
            }

            if (selectColor != COLOR_RGBA32_NONE) {
                crash_screen_draw_rect((charX - 1), (charY - 1), (TEXT_WIDTH(2) + 1), (TEXT_WIDTH(1) + 3), selectColor);
            }

            Byte byte = 0;
            if (try_read_byte(&byte, currAddr)) {
                print_byte(charX, charY, byte, textColor);
            } else {
                crash_screen_draw_glyph((charX + TEXT_WIDTH(1)), charY, '*', COLOR_RGBA32_CRASH_OUT_OF_BOUNDS);
            }

            charX += (TEXT_WIDTH(2) + 1);
        }
    }
}

void ram_view_draw(void) {
    __OSThreadContext* tc = &gCrashedThread->context;

    Address startAddr = sRamViewViewportIndex;
    u32 line = 1;

    // "[XXXXXXXX] in [XXXXXXXX]-[XXXXXXXX]"
    crash_screen_print(TEXT_X(STRLEN("RAM VIEW") + 1), TEXT_Y(line),
        (STR_COLOR_PREFIX STR_HEX_WORD" in "STR_HEX_WORD"-"STR_HEX_WORD),
        COLOR_RGBA32_WHITE, gSelectedAddress, startAddr, (startAddr + RAM_VIEWER_SHOWN_SECTION)
    );

    line++;

    u32 charX = (TEXT_X(SIZEOF_HEX(Address)) + 3);

    // Print column headers:
    for (u32 i = 0; i < (4 * sizeof(Word)); i++) {
        if ((i % 4) == 0) {
            charX += 2;
        }

        // "[XX]"
        crash_screen_print(charX, TEXT_Y(line), (STR_COLOR_PREFIX STR_HEX_BYTE), ((i % 2) ? COLOR_RGBA32_CRASH_MEMORY_COL1 : COLOR_RGBA32_CRASH_MEMORY_COL2), i);

        charX += (TEXT_WIDTH(2) + 1);
    }

    crash_screen_draw_divider(DIVIDER_Y(3));

    crash_screen_draw_rect((TEXT_X(SIZEOF_HEX(Address)) + 2), DIVIDER_Y(line), 1, TEXT_HEIGHT((line + RAM_VIEWER_NUM_ROWS) - 1), COLOR_RGBA32_CRASH_DIVIDER);

    // "MEMORY"
    crash_screen_print(TEXT_X(1), TEXT_Y(line), "MEMORY");

    line++;

    ram_viewer_print_data(line, startAddr);

    u32 line2 = (line + RAM_VIEWER_NUM_ROWS);

    crash_screen_draw_divider(DIVIDER_Y(line2));

    u32 scrollTop = (DIVIDER_Y(line) + 1);
    u32 scrollBottom = DIVIDER_Y(line2);

    // Scroll bar:
    crash_screen_draw_scroll_bar(
        scrollTop, scrollBottom,
        RAM_VIEWER_SHOWN_SECTION, VIRTUAL_RAM_SIZE,
        (sRamViewViewportIndex - RAM_VIEWER_SCROLL_MIN),
        COLOR_RGBA32_CRASH_DIVIDER, TRUE
    );

    // Scroll bar crash position marker:
    crash_screen_draw_scroll_bar(
        scrollTop, scrollBottom,
        RAM_VIEWER_SHOWN_SECTION, VIRTUAL_RAM_SIZE,
        (tc->pc - RAM_VIEWER_SCROLL_MIN),
        COLOR_RGBA32_CRASH_AT, FALSE
    );

    osWritebackDCacheAll();
}

void ram_view_input(void) {
    if (gCSDirectionFlags.pressed.up) {
        // Scroll up.
        if (gSelectedAddress >= (VIRTUAL_RAM_START + RAM_VIEWER_STEP)) {
            gSelectedAddress -= RAM_VIEWER_STEP;
        }
    }

    if (gCSDirectionFlags.pressed.down) {
        // Scroll down.
        if (gSelectedAddress <= (VIRTUAL_RAM_END - RAM_VIEWER_STEP)) {
            gSelectedAddress += RAM_VIEWER_STEP;
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

    u16 buttonPressed = gPlayer1Controller->buttonPressed;

    if (buttonPressed & A_BUTTON) {
        open_address_select(gSelectedAddress);
    }

    if (buttonPressed & B_BUTTON) {
        // Toggle whether the memory is printed as hex values or as ASCII chars.
        gCSSettings[CS_OPT_MEMORY_AS_ASCII].val ^= TRUE;
    }

    sRamViewViewportIndex = clamp_view_to_selection(sRamViewViewportIndex, gSelectedAddress, RAM_VIEWER_NUM_ROWS, RAM_VIEWER_STEP);
}
