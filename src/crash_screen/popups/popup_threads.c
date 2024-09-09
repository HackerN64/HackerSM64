#include <ultra64.h>

#include <PR/os_internal.h>

#include <string.h>
#include <stdarg.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/util/registers.h"
#include "crash_screen/cs_controls.h"
#include "crash_screen/cs_descriptions.h"
#include "crash_screen/cs_draw.h"
#include "crash_screen/cs_main.h"
#include "crash_screen/cs_pages.h"
#include "crash_screen/cs_print.h"
#include "crash_screen/cs_settings.h"
#include "popup_address.h"

#include "popup_threads.h"



OSThread* sSelectedThreadPtr = NULL;
static u32 sThreadsSelectedIndex = 0;
static u32 sThreadsViewportIndex = 0;
static u32 sNumShownThreadRows = NUM_SHOWN_SCROLL_THREADS;
static u32 sTotalFoundThreads = 0;


#define MAX_THREAD_SEARCH_ITERATIONS 32


u32 get_thread_index_in_queue(OSThread* findThread) {
    OSThread* queue = __osGetActiveQueue();
    OSThread* thread = queue;

    u32 threadIndex = 0;

    while (
        (thread != NULL) &&
        (thread->priority != OS_PRIORITY_THREADTAIL)
    ) {
        if (threadIndex > MAX_THREAD_SEARCH_ITERATIONS) {
            return 0;
        }

        if (thread == findThread) {
            return threadIndex;
        }

        thread = thread->tlnext;
        threadIndex++;
    }

    return 0;
}

void cs_popup_threads_init(void) {
    sThreadsViewportIndex = 0;

    sNumShownThreadRows = NUM_SHOWN_SCROLL_THREADS;
    sTotalFoundThreads = 0;

    sSelectedThreadPtr = gInspectThread;
    sThreadsSelectedIndex = get_thread_index_in_queue(sSelectedThreadPtr);
}

void cs_print_thread_info_line_1(ScreenCoord_u32 x, ScreenCoord_u32 y, CSTextCoord_u32 maxNumChars, OSThread* thread, _Bool align) {
    CSTextCoord_u32 charX = cs_print(x, y, "thread: "STR_COLOR_PREFIX"%d", COLOR_RGBA32_CRASH_THREAD_NAME, osGetThreadId(thread));
    if (align) {
        charX = STRLEN("thread: ##### ");    
    } else {
        charX++;
    }
    const char* threadName = get_thread_name(thread);
    if (threadName != NULL) {
        charX += cs_print_scroll((x + TEXT_WIDTH(charX)), y, (maxNumChars - charX), (align ? (STR_COLOR_PREFIX"%s") : (STR_COLOR_PREFIX"(%s)")), COLOR_RGBA32_CRASH_THREAD_NAME, threadName);
    }
}

void cs_print_thread_info_line_2(ScreenCoord_u32 x, ScreenCoord_u32 y, OSThread* thread) {
    CSTextCoord_u32 charX = 0;
    cs_print(x, y,
        ("pri:"STR_COLOR_PREFIX"%d"),
        COLOR_RGBA32_CRASH_THREAD_PRI, osGetThreadPri(thread)
    );
    charX += STRLEN("pri:### ");
    charX += cs_print((x + TEXT_WIDTH(charX)), y, "state:");
    RGBA32 stateColor = cs_draw_thread_state_icon((x + TEXT_WIDTH(charX++)), (y + 2), thread);
    const char* stateName = get_thread_state_str(thread);
    if (stateName != NULL) {
        charX += cs_print((x + TEXT_WIDTH(charX)), y,
            (STR_COLOR_PREFIX"%s"),
            stateColor, stateName
        );
        const char* flagsName = get_thread_flags_str(thread);
        if (flagsName != NULL) {
            // "(fault)" or "(break)".
            charX += cs_print((x + TEXT_WIDTH(charX)), y,
                (STR_COLOR_PREFIX" (%s)"),
                stateColor, flagsName
            );
        }
    }
}

void cs_print_thread_info(ScreenCoord_u32 x, ScreenCoord_u32 y, CSTextCoord_u32 maxNumChars, OSThread* thread) {
    CS_SET_DEFAULT_PRINT_COLOR_START(COLOR_RGBA32_LIGHT_GRAY);
    cs_print_thread_info_line_1(x, y, maxNumChars, thread, TRUE);
    y += TEXT_HEIGHT(1);
    cs_print_thread_info_line_2(x, y, thread);
    CS_SET_DEFAULT_PRINT_COLOR_END();
}

void cs_popup_threads_draw_list(ScreenCoord_u32 startY) {
    OSThread* queue = __osGetActiveQueue();
    OSThread* thread = queue;


    ScreenCoord_u32 y = startY;
    u32 threadIndex = 0;

    while (
        (thread != NULL) &&
        (thread->priority != OS_PRIORITY_THREADTAIL)
    ) {
        if (threadIndex > MAX_THREAD_SEARCH_ITERATIONS) {
            break;
        }
        if (
            (threadIndex >= sThreadsViewportIndex) &&
            (threadIndex < (sThreadsViewportIndex + sNumShownThreadRows))
        ) {
            // cs_thread_draw_highlight(thread, y);

            if (threadIndex == sThreadsSelectedIndex) {
                sSelectedThreadPtr = thread;
                cs_draw_row_box_thread(CS_POPUP_THREADS_BG_X1, y, COLOR_RGBA32_CRASH_SELECT_HIGHLIGHT);
            }

            cs_print_thread_info(TEXT_X(CS_POPUP_THREADS_TEXT_X1), y, CS_POPUP_THREADS_NUM_CHARS_X, thread);
            y += TEXT_HEIGHT(2);
            cs_draw_divider_translucent_impl((CS_POPUP_THREADS_BG_X1 + 1), (CS_POPUP_THREADS_BG_WIDTH - 2), (y - 2));
        }

        thread = thread->tlnext;
        threadIndex++;
    }

    sTotalFoundThreads = threadIndex;

    if (sTotalFoundThreads > sNumShownThreadRows) {
        cs_draw_scroll_bar_impl((CS_POPUP_THREADS_BG_X2 - 2),
            ((CS_POPUP_THREADS_BG_Y1 + TEXT_HEIGHT(2)) + 1), (CS_POPUP_THREADS_BG_Y2 - 1),
            sNumShownThreadRows, sTotalFoundThreads,
            sThreadsViewportIndex,
            COLOR_RGBA32_CRASH_SCROLL_BAR, TRUE
        );
    }

    cs_print(TEXT_X(STRLEN("THREAD REGISTERS    ")), TEXT_Y(0), "#th.found:%d", sTotalFoundThreads);

    osWritebackDCacheAll();

}

void cs_popup_threads_draw(void) {
    const ScreenCoord_s32 bgStartX = CS_POPUP_THREADS_BG_X1;
    const ScreenCoord_s32 bgStartY = CS_POPUP_THREADS_BG_Y1;
    const ScreenCoord_s32 bgW = CS_POPUP_THREADS_BG_WIDTH;
    const ScreenCoord_s32 bgH = CS_POPUP_THREADS_BG_HEIGHT;
    cs_draw_dark_rect(
        bgStartX, bgStartY,
        bgW, bgH,
        cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_POPUP_OPACITY)
    );

    OSThread* thread = gInspectThread;
    ScreenCoord_u32 threadY = CS_POPUP_THREADS_Y1;

    // Draw the currently selected thread at the top:
    // cs_thread_draw_highlight(thread, threadY);
    cs_print_thread_info(TEXT_X(CS_POPUP_THREADS_TEXT_X1), threadY, CS_POPUP_THREADS_NUM_CHARS_X, thread);

    threadY += TEXT_HEIGHT(2);
    cs_draw_divider_impl((bgStartX + 1), (bgW - 2), (threadY - 2), COLOR_RGBA32_CRASH_DIVIDER);
    cs_popup_threads_draw_list(threadY);

    cs_draw_outline(bgStartX, bgStartY, bgW, bgH, COLOR_RGBA32_CRASH_DIVIDER);

    osWritebackDCacheAll();
}

void cs_popup_threads_input(void) {
    u16 buttonPressed = gCSCompositeController->buttonPressed;

    if (sSelectedThreadPtr != NULL) {
        if (buttonPressed & START_BUTTON) {
            // Close the popup.
            cs_open_popup(CS_POPUP_NONE);
            gInspectThread = sSelectedThreadPtr;
            gSelectedAddress = gInspectThread->context.pc;
            cs_reinitialize_pages();
        }
        if (buttonPressed & A_BUTTON) {
            open_address_select((Address)sSelectedThreadPtr);
        }
    }
    if (buttonPressed & B_BUTTON) {
        // Close the popup.
        cs_open_popup(CS_POPUP_NONE);
    }

    s32 change = 0;
    if (gCSDirectionFlags.pressed.up  ) change = -1; // Scroll up.
    if (gCSDirectionFlags.pressed.down) change = +1; // Scroll down.
    sThreadsSelectedIndex = WRAP(((s32)sThreadsSelectedIndex + change), 0, (s32)(sTotalFoundThreads - 1));

    if (sTotalFoundThreads > sNumShownThreadRows) {
        sThreadsViewportIndex = cs_clamp_view_to_selection(sThreadsViewportIndex, sThreadsSelectedIndex, sNumShownThreadRows, 1);
    }
}

// Open the register inspect popup box.
void cs_open_threads(void) {
    cs_open_popup(CS_POPUP_THREADS);
    sSelectedThreadPtr = gInspectThread;
}

struct CSPopup gCSPopup_threads = {
    .name      = "THREADS",
    .initFunc  = cs_popup_threads_init,
    .drawFunc  = cs_popup_threads_draw,
    .inputFunc = cs_popup_threads_input,
    .flags = {
        .allowPageInput  = FALSE,
        .allowChangePage = FALSE,
    },
};
