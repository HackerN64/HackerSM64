#include <ultra64.h>
#include <stdarg.h>
#include <string.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen/crash_screen.h"
#include "stack_trace.h"
#include "engine/colors.h"
#include "game/debug.h"
#include "game/game_init.h"


ALIGNED8 static struct FunctionInStack sAllFunctionStack[STACK_SIZE];
ALIGNED8 static struct FunctionInStack sKnownFunctionStack[STACK_SIZE];
static u32 sNumKnownFunctions = 0;
static u32 sNumShownFunctions = STACK_SIZE;

static _Bool sStackTraceSkipUnknowns = FALSE;
static _Bool sStackTraceShowFunctionNames = TRUE;

u32 gStackTraceIndex = 0;

const enum ControlTypes stackTracePageControls[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_SCROLL_LIST,
    CONT_DESC_TOGGLE_UNKNOWNS,
    CONT_DESC_TOGGLE_FUNCTIONS,
    CONT_DESC_LIST_END,
};


#ifdef INCLUDE_DEBUG_MAP
void fill_function_stack_trace(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;
    uintptr_t temp_sp = (tc->sp + 0x14); //! TODO: Explain why 0x14
    struct FunctionInStack *function = NULL;
    const char *fname;

    // Fill the stack buffer.
    for (u32 i = 0; i < STACK_SIZE; i++) {
        fname = find_function_in_stack(&temp_sp);

        function = &sAllFunctionStack[i];
        function->addr = temp_sp;
        function->name = fname;

        if (fname != NULL) {
            function = &sKnownFunctionStack[sNumKnownFunctions++];
            function->addr = temp_sp;
            function->name = fname;
        }
    }
}
#endif

// prints any function pointers it finds in the stack format:
// SP address: function name
void draw_stack_trace(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;
    uintptr_t temp_sp = (tc->sp + 0x14); //! TODO: Explain why 0x14

    u32 line = 1;

    crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_PAGE_NAME, gCrashScreenPages[PAGE_STACK_TRACE].name);
    line += crash_screen_print(TEXT_X(12), TEXT_Y(line), "%s "STR_HEX_WORD, "FROM", temp_sp);
    crash_screen_draw_divider(DIVIDER_Y(line));

#ifdef INCLUDE_DEBUG_MAP
    crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"%s:", COLOR_RGBA32_CRASH_AT, "CURRFUNC");
    uintptr_t pc = tc->pc;
    const char *fname = parse_map(&pc);
    if (fname == NULL) {
        line += crash_screen_print(TEXT_X(9), TEXT_Y(line), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_UNKNOWN, "UNKNOWN");
    } else {
        line += crash_screen_print_scroll(TEXT_X(9), TEXT_Y(line), (CRASH_SCREEN_NUM_CHARS_X - 9), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_FUNCTION_NAME, fname);
    }

    crash_screen_draw_divider(DIVIDER_Y(line));

    osWritebackDCacheAll();

    struct FunctionInStack *functionList = (sStackTraceSkipUnknowns ? sKnownFunctionStack : sAllFunctionStack);
    struct FunctionInStack *function = NULL;

    // Print
    for (u32 i = 0; i < STACK_TRACE_NUM_ROWS; i++) {
        u32 y = TEXT_Y(line + i);

        u32 currIndex = (gStackTraceIndex + i);

        if (currIndex >= sNumShownFunctions) {
            break;
        }

        function = &functionList[currIndex];

        if (function != NULL) {
            uintptr_t faddr = function->addr;
            const char *fname = function->name;

            crash_screen_print(TEXT_X(0), y, STR_HEX_WORD":", faddr);

            if (!sStackTraceSkipUnknowns && (fname == NULL)) {
                // Print unknown function
                crash_screen_print(TEXT_X(9), y, (STR_COLOR_PREFIX STR_HEX_WORD), COLOR_RGBA32_CRASH_UNKNOWN, *(uintptr_t*)faddr);
            } else {
                // Print known function
                if (sStackTraceShowFunctionNames) {
                    crash_screen_print_scroll(TEXT_X(9), y, (CRASH_SCREEN_NUM_CHARS_X - 9), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_FUNCTION_NAME_2, fname);
                } else {
                    crash_screen_print(TEXT_X(9), y, (STR_COLOR_PREFIX STR_HEX_WORD), COLOR_RGBA32_CRASH_FUNCTION_NAME_2, *(uintptr_t*)faddr);
                }
            }
        }
    }

    osWritebackDCacheAll();

    crash_screen_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));

    // Scroll Bar
    crash_screen_draw_scroll_bar(DIVIDER_Y(3), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y), STACK_TRACE_NUM_ROWS, sNumShownFunctions, gStackTraceIndex, 4, COLOR_RGBA32_LIGHT_GRAY);
#else
    osWritebackDCacheAll();

    crash_screen_print(TEXT_X(0), TEXT_Y(line), "STACK TRACE DISABLED");
#endif

    osWritebackDCacheAll();
}

void crash_screen_input_stack_trace(void) {
#ifdef INCLUDE_DEBUG_MAP
    if (gPlayer1Controller->buttonPressed & A_BUTTON) {
        // Toggle whether entries without a name are skipped.
        sStackTraceSkipUnknowns ^= TRUE;
        sNumShownFunctions = (sStackTraceSkipUnknowns ? sNumKnownFunctions : STACK_SIZE);
        gStackTraceIndex = 0;
        gCrashScreenUpdateFramebuffer = TRUE;
    }

    if (gPlayer1Controller->buttonPressed & B_BUTTON) {
        // Toggle whether to display function names.
        toggle_display_var(&sStackTraceShowFunctionNames);
    }

    if (gCrashScreenDirectionFlags.held.up) {
        // Scroll up.
        if (gStackTraceIndex > 0) {
            gStackTraceIndex--;
            gCrashScreenUpdateFramebuffer = TRUE;
        }
    }
    if (gCrashScreenDirectionFlags.held.down) {
        // Scroll down.
        if (gStackTraceIndex < (sNumShownFunctions - STACK_TRACE_NUM_ROWS)) {
            gStackTraceIndex++;
            gCrashScreenUpdateFramebuffer = TRUE;
        }
    }
#endif
}
