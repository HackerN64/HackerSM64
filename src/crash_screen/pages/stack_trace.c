#include <ultra64.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen/crash_screen.h"
#include "stack_trace.h"
#include "game/game_input.h"
#include "segment_symbols.h"


ALIGNED16 static struct FunctionInStack sFunctionStack[STACK_TRACE_BUFFER_SIZE];
static u32 sNumFoundFunctions = 0;

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
extern void __osCleanupThread(void);

void fill_function_stack_trace(void) {
    __OSThreadContext* tc = &gCrashedThread->context;
    struct FunctionInStack currInfo = {
        .stackAddr = 0,
        .curAddr = 0,
        .faddr = 0,
        .fname = NULL,
    };

    u64* sp = (u64*)(u32)tc->sp; // Stack pointer is already aligned.

    // Fill the stack buffer.
    while ((u8*)sp < _buffersSegmentBssEnd && sNumFoundFunctions < STACK_TRACE_BUFFER_SIZE) {
        currInfo.curAddr = (u32)(*sp);
        currInfo.faddr = currInfo.curAddr;
        currInfo.fname = parse_map(&currInfo.faddr);
        if (currInfo.fname != NULL) {
            currInfo.stackAddr = (u32)sp + sizeof(u32);
            sFunctionStack[sNumFoundFunctions++] = currInfo;
        }

        if (currInfo.faddr == (uintptr_t)__osCleanupThread) {
            break;
        }

        sp++;
    }
}
#endif

void stack_trace_init(void) {
    bzero(&sFunctionStack, sizeof(sFunctionStack));

    sNumFoundFunctions = 0;

    sStackTraceShowFunctionNames = TRUE;

    gStackTraceIndex = 0;

#ifdef INCLUDE_DEBUG_MAP
    fill_function_stack_trace();
#endif
}

void stack_trace_print_entries(u32 line, u32 numLines) {
    struct FunctionInStack* function = NULL;

    // Print
    for (u32 i = 0; i < numLines; i++) {
        u32 y = TEXT_Y(line + i);

        u32 currIndex = (gStackTraceIndex + i);

        if (currIndex >= sNumFoundFunctions) {
            break;
        }

        function = &sFunctionStack[currIndex];

        if (function == NULL) {
            continue;
        }

        // "[XXXXXXXX]:"
        const size_t addrStrSize = crash_screen_print(TEXT_X(0), y, STR_HEX_WORD":", function->stackAddr);

        if (function->fname == NULL) {
            // Print unknown function
            // "[XXXXXXXX]"
            crash_screen_print(TEXT_X(addrStrSize), y,
                (STR_COLOR_PREFIX STR_HEX_WORD),
                COLOR_RGBA32_CRASH_UNKNOWN, function->curAddr
            );
        } else {
            // Print known function
            if (sStackTraceShowFunctionNames) {
                // "[function name] + [offset]"
                const size_t offsetChars = STRLEN("+0000");
                crash_screen_print_scroll(TEXT_X(addrStrSize), y,
                    (CRASH_SCREEN_NUM_CHARS_X - (addrStrSize + offsetChars)),
                    STR_COLOR_PREFIX"%s",
                    COLOR_RGBA32_CRASH_FUNCTION_NAME_2, function->fname
                );
                crash_screen_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - offsetChars), y,
                    (STR_COLOR_PREFIX"+"STR_HEX_HALFWORD),
                    COLOR_RGBA32_CRASH_FUNCTION_NAME_2, (function->curAddr - function->faddr)
                );
            } else {
                // "[XXXXXXXX]"
                crash_screen_print(TEXT_X(addrStrSize), y,
                    (STR_COLOR_PREFIX STR_HEX_WORD),
                    COLOR_RGBA32_CRASH_FUNCTION_NAME_2, function->curAddr
                );
            }
        }
    }

    osWritebackDCacheAll();
}

// prints any function pointers it finds in the stack format:
// SP address: function name
void stack_trace_draw(void) {
    __OSThreadContext* tc = &gCrashedThread->context;
    uintptr_t temp_sp = (tc->sp + 0); //! TODO: Explain why 0x14

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

    stack_trace_print_entries(line, STACK_TRACE_NUM_ROWS);

    // Scroll Bar
    if (sNumFoundFunctions > STACK_TRACE_NUM_ROWS) {
        crash_screen_draw_scroll_bar(DIVIDER_Y(3), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y), STACK_TRACE_NUM_ROWS, sNumFoundFunctions, gStackTraceIndex, 4, COLOR_RGBA32_LIGHT_GRAY);

        crash_screen_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));
    }
#else
    osWritebackDCacheAll();

    // "STACK TRACE DISABLED"
    crash_screen_print(TEXT_X(0), TEXT_Y(line), "STACK TRACE DISABLED");
#endif

    osWritebackDCacheAll();
}

void stack_trace_input(void) {
#ifdef INCLUDE_DEBUG_MAP
    if (gPlayer1Controller->buttonPressed & B_BUTTON) {
        // Toggle whether to display function names.
        toggle_display_var(&sStackTraceShowFunctionNames);
    }

    if (sNumFoundFunctions > STACK_TRACE_NUM_ROWS) {
        if (gCSDirectionFlags.held.up) {
            // Scroll up.
            if (gStackTraceIndex > 0) {
                gStackTraceIndex--;
                gCSUpdateFB = TRUE;
            }
        }
        if (gCSDirectionFlags.held.down) {
            // Scroll down.
            if (gStackTraceIndex < (sNumFoundFunctions - STACK_TRACE_NUM_ROWS)) {
                gStackTraceIndex++;
                gCSUpdateFB = TRUE;
            }
        }
    }
#endif
}
