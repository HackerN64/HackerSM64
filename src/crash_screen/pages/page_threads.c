// #include <ultra64.h>

// #include <string.h>
// #include <stdarg.h>

// #include "types.h"
// #include "sm64.h"

// #include "PR/os_internal.h"

// #include "crash_screen/util/registers.h"
// #include "crash_screen/cs_controls.h"
// #include "crash_screen/cs_descriptions.h"
// #include "crash_screen/cs_draw.h"
// #include "crash_screen/cs_main.h"
// #include "crash_screen/cs_pages.h"
// #include "crash_screen/cs_print.h"
// #include "crash_screen/cs_settings.h"

// #include "crash_screen/popups/popup_address.h"

// #include "page_threads.h"

// #include "game/assert.h"
// #include "game/debug.h"
// #include "game/puppyprint.h"
// #ifdef UNF
// #include "usb/usb.h"
// #include "usb/debug.h"
// #endif // UNF


// struct CSSetting cs_settings_group_page_threads[] = {
//     [CS_OPT_HEADER_PAGE_THREADS     ] = { .type = CS_OPT_TYPE_HEADER,  .name = "THREADS",                        .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
//     [CS_OPT_THREADS_SHOW_ADDRESSES  ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show thread addresses",          .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
//     [CS_OPT_END_THREADS             ] = { .type = CS_OPT_TYPE_END, },
// };


// const enum ControlTypes cs_cont_list_threads[] = {
//     CONT_DESC_SWITCH_PAGE,
//     CONT_DESC_PAGE_SELECT,
//     CONT_DESC_SHOW_CONTROLS,
//     CONT_DESC_HIDE_CRASH_SCREEN,
// #ifdef UNF
//     CONT_DESC_OS_PRINT,
// #endif // UNF
//     CONT_DESC_SCROLL_LIST,
//     CONT_DESC_JUMP_TO_ADDRESS,
//     CONT_DESC_SET_THREAD,
//     CONT_DESC_LIST_END,
// };


// #define NUM_SHOWN_SCROLL_THREADS 10

// OSThread* sThreadsSelectedThreadPtr = NULL;
// static u32 sThreadsSelectedIndex = 0;
// static u32 sThreadsViewportIndex = 0;
// static u32 sThreadsNumShownRows = NUM_SHOWN_SCROLL_THREADS;
// static u32 sThreadsTotalFoundThreads = 0;


// void page_threads_init(void) {
//     sThreadsSelectedIndex = 0;
//     sThreadsViewportIndex = 0;

//     sThreadsNumShownRows = NUM_SHOWN_SCROLL_THREADS;
//     sThreadsTotalFoundThreads = 0;

//     sThreadsSelectedThreadPtr = __osGetActiveQueue();
// }

// void page_threads_draw(void) {
//     u32 line = 2;

//     OSThread* queue = __osGetActiveQueue();
//     OSThread* thread = queue;
//     u32 i = 0;
//     u32 threadIndex = 0;
//     u32 y = TEXT_Y(0);

//     const u32 maxSearchIterations = 32;
//     _Bool err = FALSE;
//     while (thread->priority != OS_PRIORITY_THREADTAIL/* && thread->tlnext != queue*/) {
//         if (threadIndex > maxSearchIterations) {
//             err = TRUE;
//             break;
//         }
//         if (
//             (threadIndex < sThreadsViewportIndex) ||
//             (threadIndex > (sThreadsViewportIndex + sThreadsNumShownRows))
//         ) {
//             thread = thread->tlnext;
//             threadIndex++;
//             continue;
//         }

//         y = TEXT_Y(line + i);
//         cs_thread_draw_highlight(thread, y);
//         if (threadIndex == sThreadsSelectedIndex) {
//             sThreadsSelectedThreadPtr = thread;
//             cs_draw_row_selection_box_2(y);
//         }

//         cs_print_thread_info(0, y, thread, cs_get_setting_val(CS_OPT_GROUP_PAGE_THREADS, CS_OPT_THREADS_SHOW_ADDRESSES), TRUE);

//         i += 2;
//         cs_draw_divider_translucent(DIVIDER_Y(line + i));

//         thread = thread->tlnext;
//         threadIndex++;
//     }

//     sThreadsTotalFoundThreads = threadIndex;

//     if (sThreadsTotalFoundThreads > sThreadsNumShownRows) {
//         cs_draw_scroll_bar(
//             (DIVIDER_Y(line) + 1), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y),
//             sThreadsNumShownRows, sThreadsTotalFoundThreads,
//             sThreadsViewportIndex,
//             COLOR_RGBA32_CRASH_SCROLL_BAR, TRUE
//         );

//         cs_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));
//     }

//     line = 1;
//     if (err) {
//         cs_print(TEXT_X(0), TEXT_Y(line++), "ERROR: over %d threads found.", maxSearchIterations);
//     } else {
//         cs_print(TEXT_X(0), TEXT_Y(line++), "total threads found: %d", sThreadsTotalFoundThreads);
//     }
//     cs_draw_divider(DIVIDER_Y(line));

//     osWritebackDCacheAll();
// }

// void page_threads_input(void) {
//     //! TODO: Button combo for this instead of just B?
//     if (gCSCompositeController->buttonPressed & B_BUTTON) {
//         if (sThreadsSelectedThreadPtr != NULL) {
//             gInspectThread = sThreadsSelectedThreadPtr;
//             gSelectedAddress = gInspectThread->context.pc;
//             cs_reinitialize_pages();
//         }
//     }

//     if (gCSCompositeController->buttonPressed & A_BUTTON) {
//         if (sThreadsSelectedThreadPtr != NULL) {
//             open_address_select((Address)sThreadsSelectedThreadPtr);
//         }
//     }

//     s32 change = 0;
//     if (gCSDirectionFlags.pressed.up  ) change = -1; // Scroll up.
//     if (gCSDirectionFlags.pressed.down) change = +1; // Scroll down.
//     sThreadsSelectedIndex = WRAP(((s32)sThreadsSelectedIndex + change), 0, (s32)(sThreadsTotalFoundThreads - 1));

//     if (sThreadsTotalFoundThreads > sThreadsNumShownRows) {
//         sThreadsViewportIndex = cs_clamp_view_to_selection(sThreadsViewportIndex, sThreadsSelectedIndex, sThreadsNumShownRows, 1);
//     }
// }

// void page_threads_print(void) {
// #ifdef UNF
//     osSyncPrintf("\n");

//     //! TODO:
// #endif // UNF
// }


// struct CSPage gCSPage_threads = {
//     .name         = "THREADS",
//     .initFunc     = page_threads_init,
//     .drawFunc     = page_threads_draw,
//     .inputFunc    = page_threads_input,
//     .printFunc    = page_threads_print,
//     .contList     = cs_cont_list_threads,
//     .settingsList = cs_settings_group_page_threads,
//     .flags = {
//         .initialized = FALSE,
//         .crashed     = FALSE,
//     },
// };
