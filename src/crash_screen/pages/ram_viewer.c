#include <ultra64.h>
#include <string.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen/crash_screen.h"
#include "ram_viewer.h"
#include "engine/colors.h"
#include "game/game_input.h"


static _Bool sRamViewShowAsAscii = FALSE;


void ram_viewer_init(void) {
    sRamViewShowAsAscii = FALSE;
}

void ram_viewer_draw(void) {
    __OSThreadContext* tc = &gActiveCSThreadInfo->crashedThread->context;

    clamp_view_to_selection(RAM_VIEWER_NUM_ROWS, RAM_VIEWER_STEP);

    uintptr_t startAddr = gScrollAddress;
    u32 charX, charY;
    u32 line = 1;

    // "[XXXXXXXX] in [XXXXXXXX]-[XXXXXXXX]"
    crash_screen_print(TEXT_X(strlen(gCSPages[gCSPageID].name) + 1), TEXT_Y(line),
        (STR_COLOR_PREFIX STR_HEX_WORD" in "STR_HEX_WORD"-"STR_HEX_WORD),
        COLOR_RGBA32_WHITE, gSelectedAddress, startAddr, (startAddr + RAM_VIEWER_SHOWN_SECTION)
    );

    line++;
    charX = (TEXT_X(8) + 3);

    for (u32 i = 0; i < 16; i++) {
        if ((i % 4) == 0) {
            charX += 2;
        }

        // "[XX]"
        crash_screen_print(charX, TEXT_Y(line), (STR_COLOR_PREFIX STR_HEX_BYTE), ((i % 2) ? COLOR_RGBA32_CRASH_RAM_VIEW_H1 : COLOR_RGBA32_CRASH_RAM_VIEW_H2), i);

        charX += (TEXT_WIDTH(2) + 1);
    }

    crash_screen_draw_divider(DIVIDER_Y(3));

    crash_screen_draw_rect((TEXT_X(8) + 2), DIVIDER_Y(line), 1, TEXT_HEIGHT(line + RAM_VIEWER_NUM_ROWS - 1), COLOR_RGBA32_LIGHT_GRAY);

    // "MEMORY"
    crash_screen_print(TEXT_X(1), TEXT_Y(line), "%s", "MEMORY");

    line++;
    charX = (TEXT_X(8) + 3);
    charY = TEXT_Y(line);

    for (u32 y = 0; y < RAM_VIEWER_NUM_ROWS; y++) {
        uintptr_t rowAddr = startAddr + (y * RAM_VIEWER_STEP);
        // "[XXXXXXXX]"
        crash_screen_print(TEXT_X(0), TEXT_Y(line + y), (STR_COLOR_PREFIX STR_HEX_WORD), ((y % 2) ? COLOR_RGBA32_CRASH_RAM_VIEW_B1 : COLOR_RGBA32_CRASH_RAM_VIEW_B2), rowAddr);

        charX = (TEXT_X(8) + 3);
        charY = TEXT_Y(line + y);
        for (u32 x = 0; x < 16; x++) {
            uintptr_t currAddr = (rowAddr + x);

            if ((x % 4) == 0) {
                charX += 2;
            }

            RGBA32 color = ((sRamViewShowAsAscii || (x % 2)) ? COLOR_RGBA32_WHITE : COLOR_RGBA32_LIGHT_GRAY);

            if (currAddr == tc->pc) {
                crash_screen_draw_rect((charX - 1), (charY - 1), (TEXT_WIDTH(2) + 1), (TEXT_WIDTH(1) + 3), COLOR_RGBA32_RED);
            }
            if (currAddr == gSelectedAddress) {
                crash_screen_draw_rect((charX - 1), (charY - 1), (TEXT_WIDTH(2) + 1), (TEXT_WIDTH(1) + 3), COLOR_RGBA32_WHITE);
                color = COLOR_RGBA32_BLACK;
            }

            u8 byte = *(u8*)currAddr;

            if (sRamViewShowAsAscii) {
                crash_screen_draw_glyph(charX + TEXT_WIDTH(1), charY, byte, color);
            } else {
                // "XX"
                crash_screen_print(charX, charY, (STR_COLOR_PREFIX STR_HEX_BYTE), color, byte);
            }

            charX += (TEXT_WIDTH(2) + 1);
        }
    }

    u32 line2 = (line + RAM_VIEWER_NUM_ROWS);

    crash_screen_draw_divider(DIVIDER_Y(line2));

    // Scroll bar
    crash_screen_draw_scroll_bar(DIVIDER_Y(line), DIVIDER_Y(line2), RAM_VIEWER_SHOWN_SECTION, VALID_RAM_SIZE, (gScrollAddress - RAM_VIEWER_SCROLL_MIN), 4, COLOR_RGBA32_LIGHT_GRAY);

    osWritebackDCacheAll();
}


const enum ControlTypes ramViewerPageControls[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_CURSOR,
    CONT_DESC_JUMP_TO_ADDRESS,
    CONT_DESC_TOGGLE_ASCII,
    CONT_DESC_LIST_END,
};


void ram_viewer_input(void) {
    if (
        gCSDirectionFlags.pressed.up &&
        ((gSelectedAddress - RAM_VIEWER_STEP) >= VALID_RAM_START)
    ) {
        // Scroll up.
        gSelectedAddress -= RAM_VIEWER_STEP;
        gCSUpdateFB = TRUE;
    }
    if (
        gCSDirectionFlags.pressed.down &&
        ((gSelectedAddress + RAM_VIEWER_STEP) < VALID_RAM_END)
    ) {
        // Scroll down.
        gSelectedAddress += RAM_VIEWER_STEP;
        gCSUpdateFB = TRUE;
    }

    if (
        gCSDirectionFlags.pressed.left &&
        (((gSelectedAddress - 1) & BITMASK(4)) != 0xF) // Don't wrap.
    ) {
        gSelectedAddress--;
        gCSUpdateFB = TRUE;
    }
    if (
        gCSDirectionFlags.pressed.right &&
        (((gSelectedAddress + 1) & BITMASK(4)) != 0x0) // Don't wrap.
    ) {
        gSelectedAddress++;
        gCSUpdateFB = TRUE;
    }

    if (gPlayer1Controller->buttonPressed & A_BUTTON) { //! TODO: not if address select was just closed
        open_address_select(gSelectedAddress);
    }

    if (gPlayer1Controller->buttonPressed & B_BUTTON) {
        // Toggle whether the memory is printed as hex values or as ASCII chars.
        toggle_display_var(&sRamViewShowAsAscii);
    }
}
