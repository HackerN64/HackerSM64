#include <ultra64.h>

#include <string.h>
#include <stdarg.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/crash_controls.h"
#include "crash_screen/crash_draw.h"
#include "crash_screen/crash_main.h"
#include "crash_screen/crash_pages.h"
#include "crash_screen/crash_print.h"
#include "crash_screen/crash_settings.h"

#include "page_logs.h"

#include "game/assert.h"
#include "game/debug.h"
#include "game/puppyprint.h"
#ifdef UNF
#include "usb/debug.h"
#endif // UNF


struct CSSetting cs_settings_group_page_logs[] = {
    [CS_OPT_HEADER_PAGE_LOG     ] = { .type = CS_OPT_TYPE_HEADER,  .name = "LOGS",                           .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_LOG_INDEX_NUMBERS   ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show index numbers",             .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_END_LOG             ] = { .type = CS_OPT_TYPE_END, },
};


const enum ControlTypes cs_cont_list_logs[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_HIDE_CRASH_SCREEN,
#ifdef UNF
    CONT_DESC_OS_PRINT,
#endif // UNF
    CONT_DESC_SCROLL_LIST,
    CONT_DESC_LIST_END,
};


static u32 sLogSelectedIndex = 0;
static u32 sLogViewportIndex = 0;
static u32 sLogNumShownRows = LOG_BUFFER_SIZE;
static u32 sLogTotalRows    = LOG_BUFFER_SIZE;

void page_logs_init(void) {
    sLogSelectedIndex = 0;
    sLogViewportIndex = 0;

    sLogNumShownRows = LOG_BUFFER_SIZE;
#ifdef PUPPYPRINT_DEBUG
    sLogTotalRows = MIN(gConsoleLogLastIndex, (u32)LOG_BUFFER_SIZE);
#else // !PUPPYPRINT_DEBUG
    sLogTotalRows = LOG_BUFFER_SIZE;
#endif // !PUPPYPRINT_DEBUG
}

#ifdef PUPPYPRINT_DEBUG
void draw_logs_section(u32 line, u32 numLines) {
    const _Bool showIndexNumbers = cs_get_setting_val(CS_OPT_GROUP_PAGE_LOGS, CS_OPT_LOG_INDEX_NUMBERS);

    // Print entries:
    for (u32 y = 0; y < numLines; y++) {
        u32 printIndex = (sLogViewportIndex + y);
        char* entry = consoleLogTable[(LOG_BUFFER_SIZE - 1) - printIndex];

        if (entry[0] == CHAR_NULL) {
            break;
        }

        u32 charY = TEXT_Y(line + y);

        if (printIndex == sLogSelectedIndex) {
            cs_draw_row_selection_box(charY);
        }

        if (showIndexNumbers) {
            cs_print(TEXT_X(0), charY, "%i:\t%s", ((gConsoleLogLastIndex - 1) - printIndex), entry);
        } else {
            cs_print(TEXT_X(0), charY, "%s", entry);
        }
    }

    cs_draw_divider(DIVIDER_Y(line));

    if (sLogTotalRows > sLogNumShownRows) {
        cs_draw_scroll_bar((DIVIDER_Y(line) + 1), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y), sLogNumShownRows, sLogTotalRows, sLogViewportIndex, COLOR_RGBA32_CRASH_DIVIDER, TRUE);
        cs_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));
    }

    osWritebackDCacheAll();
}
#endif // PUPPYPRINT_DEBUG

void page_logs_draw(void) {
    u32 line = 1;

#ifdef PUPPYPRINT_DEBUG
    sLogNumShownRows = ((CRASH_SCREEN_NUM_CHARS_Y - line) - 1);
    gCSWordWrap = TRUE;
    draw_logs_section(line, sLogNumShownRows);
    gCSWordWrap = FALSE;
#else // !PUPPYPRINT_DEBUG
    cs_print(TEXT_X(0), TEXT_Y(line), "No log data.");
#endif // !PUPPYPRINT_DEBUG

    osWritebackDCacheAll();
}

void page_logs_input(void) {
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
        sLogViewportIndex = cs_clamp_view_to_selection(sLogViewportIndex, sLogSelectedIndex, sLogNumShownRows, 1);
    }
}

void page_logs_print(void) {
#ifdef UNF
    debug_printf("\n");

 #ifdef PUPPYPRINT_DEBUG
    debug_printf("- PUPPYPRINT LOG:\n");
    for (u32 i = 0; i < sLogTotalRows; i++) {
        char* entry = consoleLogTable[(LOG_BUFFER_SIZE - 1) - i];

        if (entry[0] == CHAR_NULL) {
            break;
        }

        debug_printf("-- %i: %s\n", ((gConsoleLogLastIndex - 1) - i), entry);
    }
 #else // !PUPPYPRINT_DEBUG
    else {
        debug_printf("- No log or assert data.\n");
    }
 #endif // !PUPPYPRINT_DEBUG
#endif // UNF
}


struct CSPage gCSPage_logs = {
    .name         = "LOGS",
    .initFunc     = page_logs_init,
    .drawFunc     = page_logs_draw,
    .inputFunc    = page_logs_input,
    .printFunc    = page_logs_print,
    .contList     = cs_cont_list_logs,
    .settingsList = cs_settings_group_page_logs,
    .flags = {
        .initialized = FALSE,
        .crashed     = FALSE,
    },
};
