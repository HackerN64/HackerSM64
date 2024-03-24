#include <ultra64.h>

#include <string.h>
#include <stdarg.h>

#include "types.h"
#include "sm64.h"

#include "PR/os_internal.h"

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

void cs_print_thread_info(CSTextCoord_u32 charStartX, CSScreenCoord_u32 y, OSThread* thread, _Bool showAddress, _Bool showViewing) {
    CSTextCoord_u32 charEndX = (charStartX + CS_POPUP_THREADS_NUM_CHARS_X);
    // First line:

    const RGBA32 threadColor = COLOR_RGBA32_LIGHT_CYAN;
    CSTextCoord_u32 charAddrX = charStartX;
    if (showAddress) {
        charAddrX += cs_print(TEXT_X(charStartX), y,
            (STR_HEX_WORD":"),
            (Address)thread
        );
    }

    CSTextCoord_u32 charX = charAddrX;
    cs_print(TEXT_X(charAddrX), y,
        (STR_COLOR_PREFIX"thread %d"),
        threadColor, osGetThreadId(thread)
    );
    charX += STRLEN("THREAD #####: ");
    const char* threadName = get_thread_name(thread);
    if (threadName != NULL) {
        charX += cs_print_scroll(TEXT_X(charX), y,
            (charEndX - charX),
            (STR_COLOR_PREFIX"%s"),
            COLOR_RGBA32_CRASH_VARIABLE, threadName
        );
    }

    y += TEXT_HEIGHT(1);
    // Second line:

    if (showViewing && (thread == gInspectThread)) {
        cs_print(TEXT_X(showAddress ? charStartX : (charEndX - (STRLEN("viewing") + 1))), y,
            STR_COLOR_PREFIX"viewing", COLOR_RGBA32_CRASH_THREAD
        );
    }

    charX = charAddrX;
    cs_print(TEXT_X(charX), y,
        (STR_COLOR_PREFIX"pri:"STR_COLOR_PREFIX"%d"),
        COLOR_RGBA32_LIGHT_GRAY,
        COLOR_RGBA32_CRASH_THREAD, osGetThreadPri(thread)
    );
    charX += STRLEN("pri:### ");
    charX += cs_print(TEXT_X(charX), y,
        (STR_COLOR_PREFIX"state:"),
        COLOR_RGBA32_LIGHT_GRAY
    );
    RGBA32 stateColor = cs_draw_thread_state_icon(TEXT_X(charX++), (y + 2), thread);
    const char* stateName = get_thread_state_str(thread);
    if (stateName != NULL) {
        charX += cs_print(TEXT_X(charX), y,
            (STR_COLOR_PREFIX"%s"),
            stateColor, stateName
        );
        const char* flagsName = get_thread_flags_str(thread);
        if (flagsName != NULL) {
            // "(fault)" or "(break)".
            charX += cs_print(TEXT_X(charX), y,
                (STR_COLOR_PREFIX" (%s)"),
                stateColor, flagsName
            );
        }
    }
}

void cs_popup_threads_draw_list(u32 startY) {
    _Bool exceededMax = FALSE;
    OSThread* queue = __osGetActiveQueue();
    OSThread* thread = queue;


    u32 y = startY;
    u32 threadIndex = 0;

    while (
        (thread != NULL) &&
        (thread->priority != OS_PRIORITY_THREADTAIL)
    ) {
        if (threadIndex > MAX_THREAD_SEARCH_ITERATIONS) {
            exceededMax = TRUE;
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

            cs_print_thread_info(CS_POPUP_THREADS_TEXT_X1, y, thread, FALSE, FALSE);
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

    if (exceededMax) {
        cs_print(TEXT_X(0), TEXT_Y(1), "num th.\nfound:\n>%d", MAX_THREAD_SEARCH_ITERATIONS);
    } else {
        cs_print(TEXT_X(0), TEXT_Y(1), "num th.\nfound:\n%d", sTotalFoundThreads);
    }

    osWritebackDCacheAll();

}

void cs_popup_threads_draw(void) {
    const CSScreenCoord_s32 bgStartX = CS_POPUP_THREADS_BG_X1;
    const CSScreenCoord_s32 bgStartY = CS_POPUP_THREADS_BG_Y1;
    const CSScreenCoord_s32 bgW = CS_POPUP_THREADS_BG_WIDTH;
    const CSScreenCoord_s32 bgH = CS_POPUP_THREADS_BG_HEIGHT;
    cs_draw_dark_rect(
        bgStartX, bgStartY,
        bgW, bgH,
        cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_POPUP_OPACITY)
    );

    OSThread* thread = gInspectThread;
    u32 threadY = CS_POPUP_THREADS_Y1;
    // cs_thread_draw_highlight(thread, threadY);
    cs_print_thread_info(CS_POPUP_THREADS_TEXT_X1, threadY, thread, FALSE, FALSE);
    
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
