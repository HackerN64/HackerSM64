#include <ultra64.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen/crash_screen.h"
#include "ram_viewer.h"
#include "engine/colors.h"
#include "game/game_input.h"


static Address sRamViewViewportIndex = 0x00000000;

static _Bool sRamViewShowAsAscii = FALSE;


const enum ControlTypes ramViewerContList[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_CURSOR,
    CONT_DESC_JUMP_TO_ADDRESS,
    CONT_DESC_TOGGLE_ASCII,
    CONT_DESC_LIST_END,
};


void ram_viewer_init(void) {
    sRamViewViewportIndex = gSelectedAddress;
    sRamViewShowAsAscii = FALSE;
}

static const char gHex[0x10] = "0123456789ABCDEF";

static void print_byte(u32 x, u32 y, Byte byte, RGBA32 color) {
    // "XX"
    if (sRamViewShowAsAscii) {
        crash_screen_draw_glyph((x + TEXT_WIDTH(1)), y, byte, color);
    } else {
        // Faster than doing crash_screen_print:
        crash_screen_draw_glyph((x + TEXT_WIDTH(0)), y, gHex[byte >> 4], color);
        crash_screen_draw_glyph((x + TEXT_WIDTH(1)), y, gHex[byte & 0xF], color);
    }
}

void ram_viewer_draw(void) {
    __OSThreadContext* tc = &gCrashedThread->context;

    sRamViewViewportIndex = clamp_view_to_selection(sRamViewViewportIndex, gSelectedAddress, RAM_VIEWER_NUM_ROWS, RAM_VIEWER_STEP);

    Address startAddr = sRamViewViewportIndex;
    u32 line = 1;

    // "[XXXXXXXX] in [XXXXXXXX]-[XXXXXXXX]"
    crash_screen_print(TEXT_X(STRLEN("RAM VIEW") + 1), TEXT_Y(line),
        (STR_COLOR_PREFIX STR_HEX_WORD" in "STR_HEX_WORD"-"STR_HEX_WORD),
        COLOR_RGBA32_WHITE, gSelectedAddress, startAddr, (startAddr + RAM_VIEWER_SHOWN_SECTION)
    );

    line++;

    u32 charX = (TEXT_X(8) + 3);

    // Print column headers:
    for (u32 i = 0; i < 16; i++) {
        if ((i % 4) == 0) {
            charX += 2;
        }

        // "[XX]"
        crash_screen_print(charX, TEXT_Y(line), (STR_COLOR_PREFIX STR_HEX_BYTE), ((i % 2) ? COLOR_RGBA32_CRASH_RAM_VIEW_H1 : COLOR_RGBA32_CRASH_RAM_VIEW_H2), i);

        charX += (TEXT_WIDTH(2) + 1);
    }

    crash_screen_draw_divider(DIVIDER_Y(3));

    crash_screen_draw_rect((TEXT_X(8) + 2), DIVIDER_Y(line), 1, TEXT_HEIGHT((line + RAM_VIEWER_NUM_ROWS) - 1), COLOR_RGBA32_LIGHT_GRAY);

    // "MEMORY"
    crash_screen_print(TEXT_X(1), TEXT_Y(line), "MEMORY");

    line++;

    charX = (TEXT_X(8) + 3);
    u32 charY = TEXT_Y(line);

    for (u32 y = 0; y < RAM_VIEWER_NUM_ROWS; y++) {
        Address rowAddr = (startAddr + (y * RAM_VIEWER_STEP));

        // "[XXXXXXXX]"
        crash_screen_print(TEXT_X(0), TEXT_Y(line + y), (STR_COLOR_PREFIX STR_HEX_WORD),
            ((y % 2) ? COLOR_RGBA32_CRASH_RAM_VIEW_B1 : COLOR_RGBA32_CRASH_RAM_VIEW_B2), rowAddr
        );

        charX = (TEXT_X(8) + 3);
        charY = TEXT_Y(line + y);
        for (u32 x = 0; x < 16; x++) {
            Address currAddr = (rowAddr + x);

            if ((x % 4) == 0) {
                charX += 2;
            }

            RGBA32 textColor = ((sRamViewShowAsAscii || (x % 2)) ? COLOR_RGBA32_WHITE : COLOR_RGBA32_LIGHT_GRAY);
            RGBA32 selectColor = COLOR_RGBA32_NONE;

            if (currAddr == gSelectedAddress) {
                selectColor = COLOR_RGBA32_WHITE;
                textColor = COLOR_RGBA32_BLACK;
            } else if (currAddr == tc->pc) {
                selectColor = COLOR_RGBA32_RED;
            }

            if (selectColor != COLOR_RGBA32_NONE) {
                crash_screen_draw_rect((charX - 1), (charY - 1), (TEXT_WIDTH(2) + 1), (TEXT_WIDTH(1) + 3), selectColor);
            }

            print_byte(charX, charY, *(Byte*)currAddr, textColor);

            charX += (TEXT_WIDTH(2) + 1);
        }
    }

    u32 line2 = (line + RAM_VIEWER_NUM_ROWS);

    crash_screen_draw_divider(DIVIDER_Y(line2));

    // Scroll bar
    crash_screen_draw_scroll_bar(DIVIDER_Y(line), DIVIDER_Y(line2), RAM_VIEWER_SHOWN_SECTION, VALID_RAM_SIZE, (sRamViewViewportIndex - RAM_VIEWER_SCROLL_MIN), 4, COLOR_RGBA32_LIGHT_GRAY);

    osWritebackDCacheAll();
}

void ram_viewer_input(void) {
    if (gCSDirectionFlags.pressed.up) {
        // Scroll up.
        if ((gSelectedAddress - RAM_VIEWER_STEP) >= VALID_RAM_START) {
            gSelectedAddress -= RAM_VIEWER_STEP;
        }
    }

    if (gCSDirectionFlags.pressed.down) {
        // Scroll down.
        if ((gSelectedAddress + RAM_VIEWER_STEP) < VALID_RAM_END) {
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

    if (buttonPressed & A_BUTTON) { //! TODO: not if address select was just closed
        open_address_select(gSelectedAddress);
    }

    if (buttonPressed & B_BUTTON) {
        // Toggle whether the memory is printed as hex values or as ASCII chars.
        sRamViewShowAsAscii ^= TRUE;
    }
}
