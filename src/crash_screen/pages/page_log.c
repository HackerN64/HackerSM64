#include <ultra64.h>

#include <string.h>
#include <stdarg.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/crash_controls.h"
#include "crash_screen/crash_draw.h"
#include "crash_screen/crash_main.h"
#include "crash_screen/crash_print.h"

#include "page_log.h"

#include "game/assert.h"
#include "game/debug.h"
#include "game/puppyprint.h"


const enum ControlTypes logContList[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_SCROLL_LIST,
    CONT_DESC_LIST_END,
};

static u32 sLogSelectedIndex = 0;
static u32 sLogViewportIndex = 0;
static u32 sLogNumShownRows = LOG_BUFFER_SIZE;
static u32 sLogTotalRows    = LOG_BUFFER_SIZE;

void log_init(void) {
    sLogSelectedIndex = 0;
    sLogViewportIndex = 0;

    sLogNumShownRows = LOG_BUFFER_SIZE;
    sLogTotalRows    = LOG_BUFFER_SIZE;
}

// Draws the red background for the assert section.
static void draw_assert_highlight(u32 line) {
    //! Prints the assert message early, but with 0 alpha (skips framebuffer writes).
    //   This is a hacky way to get the amount of lines the wrapped assert text will be.
    //   This can't be done after the normal print because it would show up in front of the text.
    crash_screen_print(TEXT_X(0), TEXT_Y(line),
        STR_COLOR_PREFIX"MESSAGE:%s",
        COLOR_RGBA32_NONE, __n64Assert_Message
    );
    crash_screen_draw_rect(CRASH_SCREEN_X1, (DIVIDER_Y(line) + 1), CRASH_SCREEN_W, TEXT_HEIGHT(4 + gCSNumLinesPrinted), RGBA32_SET_ALPHA(COLOR_RGBA32_RED, 0x7F));
}

u32 print_assert_section(u32 line) {
    u32 charX = 0;

    draw_assert_highlight(line);

    // "ASSERT:"
    crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"ASSERT:", COLOR_RGBA32_CRASH_HEADER);
    line++;
    crash_screen_draw_divider(DIVIDER_Y(line));

    size_t lineStrStart = (CRASH_SCREEN_NUM_CHARS_X - STRLEN("LINE:0000"));
    // "FILE: [file name]"
    charX += crash_screen_print(TEXT_X(0), TEXT_Y(line),
        STR_COLOR_PREFIX"FILE:",
        COLOR_RGBA32_CRASH_HEADER
    );
    charX += crash_screen_print_scroll(TEXT_X(charX), TEXT_Y(line), (lineStrStart - charX),
        STR_COLOR_PREFIX"%s",
        COLOR_RGBA32_CRASH_FILE_NAME, __n64Assert_Filename
    );
    // "LINE:[line number]"
    crash_screen_print(TEXT_X(lineStrStart), TEXT_Y(line),
        STR_COLOR_PREFIX"LINE:"STR_COLOR_PREFIX"%d",
        COLOR_RGBA32_CRASH_HEADER,
        COLOR_RGBA32_CRASH_FILE_NAME, __n64Assert_LineNum
    );
    line++;

    // "COND:[condition]"
    if (__n64Assert_Condition != NULL) {
        charX = 0;
        charX += crash_screen_print(TEXT_X(charX), TEXT_Y(line),
            STR_COLOR_PREFIX"COND:", COLOR_RGBA32_CRASH_HEADER
        );
        charX += crash_screen_print_scroll(TEXT_X(charX), TEXT_Y(line),
            (CRASH_SCREEN_NUM_CHARS_X - charX),
            STR_COLOR_PREFIX"%s",
            COLOR_RGBA32_CRASH_AT, __n64Assert_Condition
        );
        line++;
    }

    // "MESSAGE:[message]"
    crash_screen_print(TEXT_X(0), TEXT_Y(line),
        STR_COLOR_PREFIX"MESSAGE:"STR_COLOR_PREFIX"%s",
        COLOR_RGBA32_CRASH_HEADER,
        gCSDefaultPrintColor, __n64Assert_Message
    );
    line += gCSNumLinesPrinted;

    osWritebackDCacheAll();

    return line;
}

#ifdef PUPPYPRINT_DEBUG
void draw_log_section(u32 line, u32 numLines) {
    // "PUPPYPRINT LOG:"
    crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"PUPPYPRINT LOG:", COLOR_RGBA32_CRASH_HEADER);
    line++;

    // Print entries:
    for (u32 y = 0; y < numLines; y++) {
        u32 printIndex = (sLogViewportIndex + y);
        char* entry = consoleLogTable[(LOG_BUFFER_SIZE - 1) - printIndex];

        if (entry[0] == CHAR_NULL) {
            break;
        }

        u32 charY = TEXT_Y(line + y);

        if (printIndex == sLogSelectedIndex) {
            crash_screen_draw_row_selection_box(charY);
        }

        crash_screen_print(TEXT_X(0), charY, "%i:\t%s", ((gConsoleLogLastIndex - 1) - printIndex), entry);
    }

    crash_screen_draw_divider(DIVIDER_Y(line));

    if (sLogTotalRows > sLogNumShownRows) {
        crash_screen_draw_scroll_bar((DIVIDER_Y(line) + 1), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y), sLogNumShownRows, sLogTotalRows, sLogViewportIndex, COLOR_RGBA32_CRASH_DIVIDER, TRUE);
        crash_screen_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));
    }

    osWritebackDCacheAll();
}
#endif

void log_draw(void) {
    u32 line = 2;

    gCSWordWrap = TRUE;

    if (__n64Assert_Message != NULL) {
        line = print_assert_section(line);
#ifdef PUPPYPRINT_DEBUG
        line++;
        crash_screen_draw_divider(DIVIDER_Y(line));
#else
    } else {
        crash_screen_print(TEXT_X(0), TEXT_Y(line), "No log or assert data.");
#endif
    }

#ifdef PUPPYPRINT_DEBUG
    sLogNumShownRows = ((CRASH_SCREEN_NUM_CHARS_Y - line) - 1);
    sLogTotalRows = MIN(gConsoleLogLastIndex, LOG_BUFFER_SIZE);

    draw_log_section(line, sLogNumShownRows);
#endif

    gCSWordWrap = FALSE;

    osWritebackDCacheAll();
}

void log_input(void) {
    if (gCSDirectionFlags.pressed.up) {
        // Scroll up.
        if (sLogSelectedIndex > 0) {
            sLogSelectedIndex--;
        }
    }
    if (gCSDirectionFlags.pressed.down) {
        // Scroll down.
        if (sLogSelectedIndex < (sLogTotalRows - 1)) {
            sLogSelectedIndex++;
        }
    }

    if (sLogTotalRows > sLogNumShownRows) {
        sLogViewportIndex = clamp_view_to_selection(sLogViewportIndex, sLogSelectedIndex, sLogNumShownRows, 1);
    }
}
