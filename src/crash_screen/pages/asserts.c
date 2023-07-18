#include <ultra64.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen/crash_main.h"
#include "asserts.h"
#include <stdarg.h>
#include <string.h>


static u32 sAssertScrollIndex = 0;
static size_t sAssertPrintSize = 0;


const enum ControlTypes assertsContList[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_SCROLL_LIST,
    CONT_DESC_LIST_END,
};


void assert_init(void) {
    sAssertScrollIndex = 0;
    sAssertPrintSize = 0;
}

void print_assert(u32 x, u32 y) {
    crash_screen_print(x, y, "%s", __n64Assert_Message);
    sAssertPrintSize = gCSNumLinesPrinted;
}

void assert_draw(void) {
    u32 line = 2;

    gCSWordWrap = TRUE;

    if (__n64Assert_Filename != NULL) {
        // "FILE: [file name]"
        crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"FILE:%s", COLOR_RGBA32_CRASH_FILE_NAME, __n64Assert_Filename);
        line++;
        crash_screen_draw_divider(DIVIDER_Y(line));
        // "LINE: [line number]"
        crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"LINE:%d", COLOR_RGBA32_CRASH_AT, __n64Assert_LineNum);
        line++;
        crash_screen_draw_divider(DIVIDER_Y(line));
        // "MESSAGE:"
        crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"MESSAGE:", COLOR_RGBA32_CRASH_HEADER);
        line++;
        gCSScissorBox.y1 = TEXT_Y(line);
        // "[assert message]"
        print_assert(TEXT_X(0), TEXT_Y(line - sAssertScrollIndex));
        gCSScissorBox.y1 = SCISSOR_BOX_DEFAULT_Y1;

        // If the assert message is long enough to scroll, draw the scroll bar and dividers.
        if (sAssertPrintSize > ASSERTS_NUM_ROWS) {
            crash_screen_draw_divider(DIVIDER_Y(line));
            crash_screen_draw_scroll_bar((DIVIDER_Y(line) + 1), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y), ASSERTS_NUM_ROWS, sAssertPrintSize, sAssertScrollIndex, COLOR_RGBA32_LIGHT_GRAY, TRUE);
            crash_screen_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));
        }

        line++;
    } else {
        // "No failed assert to report."
        crash_screen_print(TEXT_X(0), TEXT_Y(line), "No failed assert to report.");
    }

    gCSWordWrap = FALSE;


    osWritebackDCacheAll();
}

void assert_input(void) {
    if (sAssertPrintSize > ASSERTS_NUM_ROWS) {
        if (gCSDirectionFlags.pressed.up) {
            // Scroll up.
            if (sAssertScrollIndex > 0) {
                sAssertScrollIndex--;
            }
        }
        if (gCSDirectionFlags.pressed.down) {
            // Scroll down.
            if (sAssertScrollIndex < (sAssertPrintSize - ASSERTS_NUM_ROWS)) {
                sAssertScrollIndex++;
            }
        }
    }

    // sAssertScrollIndex = clamp_view_to_selection(sAssertScrollIndex, sMapViewerSelectedIndex, MAP_VIEWER_NUM_ROWS, 1);
}
