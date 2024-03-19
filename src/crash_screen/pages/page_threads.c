#include <ultra64.h>

#include <string.h>
#include <stdarg.h>

#include "types.h"
#include "sm64.h"

#include "PR/os_internal.h"

#include "crash_screen/util/registers.h"
#include "crash_screen/crash_controls.h"
#include "crash_screen/crash_descriptions.h"
#include "crash_screen/crash_draw.h"
#include "crash_screen/crash_main.h"
#include "crash_screen/crash_pages.h"
#include "crash_screen/crash_print.h"
#include "crash_screen/crash_settings.h"

#include "crash_screen/popups/popup_address_select.h"

#include "page_threads.h"

#include "game/assert.h"
#include "game/debug.h"
#include "game/puppyprint.h"
#ifdef UNF
#include "usb/usb.h"
#include "usb/debug.h"
#endif // UNF


struct CSSetting cs_settings_group_page_threads[] = {
    [CS_OPT_HEADER_PAGE_THREADS     ] = { .type = CS_OPT_TYPE_HEADER,  .name = "THREADS",                        .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_THREADS_SHOW_ADDRESSES  ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show thread addresses",          .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_END_THREADS             ] = { .type = CS_OPT_TYPE_END, },
};


const enum ControlTypes cs_cont_list_threads[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_PAGE_SELECT,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_HIDE_CRASH_SCREEN,
#ifdef UNF
    CONT_DESC_OS_PRINT,
#endif // UNF
    CONT_DESC_SCROLL_LIST,
    CONT_DESC_JUMP_TO_ADDRESS,
    CONT_DESC_SET_THREAD,
    CONT_DESC_LIST_END,
};


#define NUM_SHOWN_THREADS 10

OSThread* sThreadsSelectedThreadPtr = NULL;
static u32 sThreadsSelectedIndex = 0;
static u32 sThreadsViewportIndex = 0;
static u32 sThreadsNumShownRows = NUM_SHOWN_THREADS;
static u32 sThreadsTotalFoundThreads = 0;


void page_threads_init(void) {
    sThreadsSelectedIndex = 0;
    sThreadsViewportIndex = 0;

    sThreadsNumShownRows = NUM_SHOWN_THREADS;
    sThreadsTotalFoundThreads = 0;

    sThreadsSelectedThreadPtr = __osGetActiveQueue();
}

void draw_thread_state_icon(u32 x, u32 y, u16 state) {
    const s32 w = 6;
    const s32 h = 6;
    switch (state) {
        case OS_STATE_STOPPED:
            cs_draw_rect(x, y, (w - 1), (h - 1), COLOR_RGBA32_CRASH_NO);
            break;
        case OS_STATE_RUNNABLE:
            cs_draw_rect(x, y, 1, (h - 1), COLOR_RGBA32_VERY_LIGHT_YELLOW);
            cs_draw_triangle((x + 2), (y - 1), (w - 2), h, COLOR_RGBA32_VERY_LIGHT_YELLOW, CS_TRI_RIGHT);
            break;
        case OS_STATE_RUNNING:
            cs_draw_triangle(x, (y - 1), w, h, COLOR_RGBA32_CRASH_YES, CS_TRI_RIGHT);
            break;
        case OS_STATE_WAITING:
            cs_draw_rect(x, y, (w / 3), (h - 1), COLOR_RGBA32_GRAY);
            cs_draw_rect((x + (w / 3) + 1), y, (w / 3), (h - 1), COLOR_RGBA32_GRAY);
            break;
    }
}

RGBA32 cs_thread_draw_highlight(OSThread* thread, u32 y) {
    RGBA32 color = 0x00000000;

    if (thread == gCrashedThread) {
        color = COLOR_RGBA32_CRASH_PC_HIGHLIGHT;
    } else if (thread == __osRunningThread) {
        color = COLOR_RGBA32_CRASH_RUNNING_HIGHLIGHT;
    } else if (thread == gInspectThread) {
        color = COLOR_RGBA32_CRASH_INSPECT_HIGHLIGHT;
    }

    if (color) {
        cs_draw_row_box_2(y, color);
    }

    return color;
}

void page_threads_draw(void) {
    u32 line = 2;

    OSThread* queue = __osGetActiveQueue();
    OSThread* thread = queue;
    u32 i = 0;
    u32 threadIndex = 0;
    u32 y = TEXT_Y(0);
    while (thread->priority != OS_PRIORITY_THREADTAIL) {
        if (
            (threadIndex < sThreadsViewportIndex) ||
            (threadIndex > (sThreadsViewportIndex + sThreadsNumShownRows))
        ) {
            thread = thread->tlnext;
            threadIndex++;
            continue;
        }

        y = TEXT_Y(line + i);
        cs_thread_draw_highlight(thread, y);
        if (threadIndex == sThreadsSelectedIndex) {
            sThreadsSelectedThreadPtr = thread;
            cs_draw_row_selection_box_2(y);
        }

        // First line:

        const RGBA32 threadColor = COLOR_RGBA32_LIGHT_CYAN;
        size_t charAddrX = 0;
        _Bool showAddresses = cs_get_setting_val(CS_OPT_GROUP_PAGE_THREADS, CS_OPT_THREADS_SHOW_ADDRESSES);
        if (showAddresses) {
            charAddrX = cs_print(TEXT_X(0), y,
                (STR_HEX_WORD":"),
                (Address)thread
            );
        }

        size_t charX = charAddrX;
        charX += cs_print(TEXT_X(charAddrX), y,
            (STR_COLOR_PREFIX"thread %d"),
            threadColor, osGetThreadId(thread)
        );
        const char* threadName = get_thread_name(thread);
        if (threadName != NULL) {
            charX += cs_print(TEXT_X(charX), y,
                (STR_COLOR_PREFIX": %s"),
                threadColor, threadName
            );
        }

        i++;
        y = TEXT_Y(line + i);

        // Second line:

        draw_thread_state_icon(TEXT_X(showAddresses ? 0 : (CRASH_SCREEN_NUM_CHARS_X - 1)), y, thread->state);
        if (thread == gInspectThread) {
            cs_print(TEXT_X(showAddresses ? 1 : (CRASH_SCREEN_NUM_CHARS_X - (STRLEN("viewing") + 1))), y,
                STR_COLOR_PREFIX"viewing", COLOR_RGBA32_CRASH_THREAD
            );
        }

        charX = charAddrX;
        charX += cs_print(TEXT_X(charX), y,
            (STR_COLOR_PREFIX"pri:"STR_COLOR_PREFIX"%3d"),
            COLOR_RGBA32_LIGHT_GRAY,
            COLOR_RGBA32_CRASH_THREAD, osGetThreadPri(thread)
        );

        const char* stateName = get_thread_state_str(thread);
        if (stateName != NULL) {
            RGBA32 stateColor = COLOR_RGBA32_LIGHT_GRAY;
            switch (thread->state) {
                case OS_STATE_STOPPED:  stateColor = COLOR_RGBA32_CRASH_NO;          break;
                case OS_STATE_RUNNABLE: stateColor = COLOR_RGBA32_VERY_LIGHT_YELLOW; break;
                case OS_STATE_RUNNING:  stateColor = COLOR_RGBA32_CRASH_YES;         break;
                case OS_STATE_WAITING:  stateColor = COLOR_RGBA32_GRAY;              break;
            }
            charX += cs_print(TEXT_X(charX), y,
                (STR_COLOR_PREFIX" state:"STR_COLOR_PREFIX"%s"),
                    COLOR_RGBA32_LIGHT_GRAY,
                    stateColor, stateName
            );
            const char* flagsName = get_thread_flags_str(thread);
            if (flagsName != NULL) {
                // "(fault)" or "(cpu break)".
                charX += cs_print(TEXT_X(charX), y,
                    (STR_COLOR_PREFIX" (%s)"),
                    stateColor, flagsName
                );
            }
        }

        i++;
        cs_draw_divider_translucent(DIVIDER_Y(line + i));

        thread = thread->tlnext;
        threadIndex++;
    }

    sThreadsTotalFoundThreads = threadIndex;

    if (sThreadsTotalFoundThreads > sThreadsNumShownRows) {
        cs_draw_scroll_bar(
            (DIVIDER_Y(line) + 1), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y),
            sThreadsNumShownRows, sThreadsTotalFoundThreads,
            sThreadsViewportIndex,
            COLOR_RGBA32_CRASH_SCROLL_BAR, TRUE
        );

        cs_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));
    }

    line = 1;
    cs_print(TEXT_X(0), TEXT_Y(line++), "total threads found: %d", sThreadsTotalFoundThreads);
    cs_draw_divider(DIVIDER_Y(line));

    osWritebackDCacheAll();
}

void page_threads_input(void) {
    //! TODO: Button combo for this instead of just B?
    if (gCSCompositeController->buttonPressed & B_BUTTON) {
        if (sThreadsSelectedThreadPtr != NULL) {
            gInspectThread = sThreadsSelectedThreadPtr;
            cs_reinitialize_pages();
        }
    }

    if (gCSCompositeController->buttonPressed & A_BUTTON) {
        if (sThreadsSelectedThreadPtr != NULL) {
            open_address_select((Address)sThreadsSelectedThreadPtr);
        }
    }

    s32 change = 0;
    if (gCSDirectionFlags.pressed.up  ) change = -1; // Scroll up.
    if (gCSDirectionFlags.pressed.down) change = +1; // Scroll down.
    sThreadsSelectedIndex = WRAP(((s32)sThreadsSelectedIndex + change), 0, (s32)(sThreadsTotalFoundThreads - 1));

    if (sThreadsTotalFoundThreads > sThreadsNumShownRows) {
        sThreadsViewportIndex = cs_clamp_view_to_selection(sThreadsViewportIndex, sThreadsSelectedIndex, sThreadsNumShownRows, 1);
    }
}

void page_threads_print(void) {
#ifdef UNF
    osSyncPrintf("\n");

    //! TODO:
#endif // UNF
}


struct CSPage gCSPage_threads = {
    .name         = "THREADS",
    .initFunc     = page_threads_init,
    .drawFunc     = page_threads_draw,
    .inputFunc    = page_threads_input,
    .printFunc    = page_threads_print,
    .contList     = cs_cont_list_threads,
    .settingsList = cs_settings_group_page_threads,
    .flags = {
        .initialized = FALSE,
        .crashed     = FALSE,
    },
};
