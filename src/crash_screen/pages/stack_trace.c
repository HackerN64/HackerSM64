#include <ultra64.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen/crash_screen.h"
#include "stack_trace.h"
#include "game/game_input.h"


ALIGNED8 static struct FunctionInStack sAllFunctionStack[STACK_TRACE_SIZE];
ALIGNED8 static struct FunctionInStack sKnownFunctionStack[STACK_TRACE_SIZE];
static u32 sNumKnownFunctions = 0;
static u32 sNumShownFunctions = STACK_TRACE_SIZE;

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
void fill_function_stack_trace(OSThread* thread) {
    __OSThreadContext* tc = &thread->context;
    uintptr_t temp_sp = (tc->sp + 0x14); //! TODO: Explain why 0x14
    struct FunctionInStack* function = NULL;
    const char* fname;

    // Fill the stack buffer.
    for (u32 i = 0; i < STACK_TRACE_SIZE; i++) {
        fname = find_function_in_stack(&temp_sp);

        function = &sAllFunctionStack[i];
        function->addr = temp_sp;
        function->name = fname;

        if (fname == NULL) {
            continue;
        }

        function = &sKnownFunctionStack[sNumKnownFunctions++];
        function->addr = temp_sp;
        function->name = fname;
    }
}
#endif

void stack_trace_init(void) {
    bzero(&sAllFunctionStack, sizeof(sAllFunctionStack));
    bzero(&sKnownFunctionStack, sizeof(sKnownFunctionStack));

    sNumKnownFunctions = 0;
    sNumShownFunctions = STACK_TRACE_SIZE;

    sStackTraceSkipUnknowns = FALSE;
    sStackTraceShowFunctionNames = TRUE;

    gStackTraceIndex = 0;

    fill_function_stack_trace(gActiveCSThreadInfo->crashedThread);
}

// prints any function pointers it finds in the stack format:
// SP address: function name
void stack_trace_draw(OSThread* thread) {
    __OSThreadContext* tc = &thread->context;
    uintptr_t temp_sp = (tc->sp + 0x14); //! TODO: Explain why 0x14

    u32 line = 1;

    // "FROM: [XXXXXXXX]"
    crash_screen_print(TEXT_X(12), TEXT_Y(line), "%s "STR_HEX_WORD, "FROM", temp_sp);

    line++;

#ifdef INCLUDE_DEBUG_MAP
    crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"%s:", COLOR_RGBA32_CRASH_AT, "CURRFUNC");
    uintptr_t pc = tc->pc;
    const char* fname = parse_map(&pc);
    if (fname == NULL) {
        // "UNKNOWN"
        crash_screen_print(TEXT_X(9), TEXT_Y(line), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_UNKNOWN, "UNKNOWN");
    } else {
        // "[function name]"
        crash_screen_print_scroll(TEXT_X(9), TEXT_Y(line), (CRASH_SCREEN_NUM_CHARS_X - 9), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_FUNCTION_NAME, fname);
    }

    line++;

    crash_screen_draw_divider(DIVIDER_Y(line));

    osWritebackDCacheAll();

    struct FunctionInStack* functionListStart = (sStackTraceSkipUnknowns ? sKnownFunctionStack : sAllFunctionStack);
    struct FunctionInStack* function = NULL;

    // Print
    for (u32 i = 0; i < STACK_TRACE_NUM_ROWS; i++) {
        u32 y = TEXT_Y(line + i);

        u32 currIndex = (gStackTraceIndex + i);

        if (currIndex >= sNumShownFunctions) {
            break;
        }

        function = &functionListStart[currIndex];

        if (function == NULL) {
            continue;
        }

        uintptr_t faddr = function->addr;
        const char* fname = function->name;

        // "[XXXXXXXX]:"
        crash_screen_print(TEXT_X(0), y, STR_HEX_WORD":", faddr);

        if (!sStackTraceSkipUnknowns && (fname == NULL)) {
            // Print unknown function
            // "[XXXXXXXX]"
            crash_screen_print(TEXT_X(9), y, (STR_COLOR_PREFIX STR_HEX_WORD), COLOR_RGBA32_CRASH_UNKNOWN, *(uintptr_t*)faddr);
        } else {
            // Print known function
            if (sStackTraceShowFunctionNames) {
                // "[function name]"
                crash_screen_print_scroll(TEXT_X(9), y, (CRASH_SCREEN_NUM_CHARS_X - 9), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_FUNCTION_NAME_2, fname);
            } else {
                // "[XXXXXXXX]"
                crash_screen_print(TEXT_X(9), y, (STR_COLOR_PREFIX STR_HEX_WORD), COLOR_RGBA32_CRASH_FUNCTION_NAME_2, *(uintptr_t*)faddr);
            }
        }
    }

    osWritebackDCacheAll();

    crash_screen_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));

    // Scroll Bar
    crash_screen_draw_scroll_bar(DIVIDER_Y(3), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y), STACK_TRACE_NUM_ROWS, sNumShownFunctions, gStackTraceIndex, 4, COLOR_RGBA32_LIGHT_GRAY);
#else
    osWritebackDCacheAll();

    // "STACK TRACE DISABLED"
    crash_screen_print(TEXT_X(0), TEXT_Y(line), "STACK TRACE DISABLED");
#endif

    osWritebackDCacheAll();
}

void stack_trace_input(void) {
#ifdef INCLUDE_DEBUG_MAP
    if (gPlayer1Controller->buttonPressed & A_BUTTON) {
        // Toggle whether entries without a name are skipped.
        sStackTraceSkipUnknowns ^= TRUE;
        sNumShownFunctions = (sStackTraceSkipUnknowns ? sNumKnownFunctions : STACK_TRACE_SIZE);
        gStackTraceIndex = 0;
        gCSUpdateFB = TRUE;
    }

    if (gPlayer1Controller->buttonPressed & B_BUTTON) {
        // Toggle whether to display function names.
        toggle_display_var(&sStackTraceShowFunctionNames);
    }

    if (gCSDirectionFlags.held.up) {
        // Scroll up.
        if (gStackTraceIndex > 0) {
            gStackTraceIndex--;
            gCSUpdateFB = TRUE;
        }
    }
    if (gCSDirectionFlags.held.down) {
        // Scroll down.
        if (gStackTraceIndex < (sNumShownFunctions - STACK_TRACE_NUM_ROWS)) {
            gStackTraceIndex++;
            gCSUpdateFB = TRUE;
        }
    }
#endif
}
