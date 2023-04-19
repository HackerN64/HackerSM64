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

static u32 sStackTraceSelectedIndex = 0;
static u32 sStackTraceViewportIndex = 0;

const enum ControlTypes stackTracePageControls[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_SCROLL_LIST,
    CONT_DESC_JUMP_TO_ADDRESS,
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

    u64* sp = (u64*)(uintptr_t)tc->sp; // Stack pointer is already aligned, so get the lower bits.

    // Fill the stack buffer.
    while ((u8*)sp < _buffersSegmentBssEnd && sNumFoundFunctions < STACK_TRACE_BUFFER_SIZE) {
        currInfo.curAddr = (uintptr_t)(*sp);
        currInfo.faddr = currInfo.curAddr;
        currInfo.fname = parse_map(&currInfo.faddr);
        if (currInfo.fname != NULL) {
            currInfo.curAddr -= (2 * sizeof(uintptr_t)); // Go back to the jump before the delay slot.
            //! TODO: If JAL command uses a different function than the previous entrie's faddr, use the one in the JAL command?
            //! TODO: handle duplicate entries caused by JALR RA, V0
            currInfo.stackAddr = (uintptr_t)sp + sizeof(uintptr_t);
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

    sStackTraceSelectedIndex = 0;
    sStackTraceViewportIndex = 0;

#ifdef INCLUDE_DEBUG_MAP
    fill_function_stack_trace();
#endif
}

void stack_trace_print_entries(u32 line, u32 numLines) {
    struct FunctionInStack* function = NULL;

    sStackTraceViewportIndex = clamp_view_to_selection(sStackTraceViewportIndex, sStackTraceSelectedIndex, STACK_TRACE_NUM_ROWS, 1);

    // Print
    for (u32 i = 0; i < numLines; i++) {
        u32 y = TEXT_Y(line + i);

        u32 currIndex = (sStackTraceViewportIndex + i);

        if (currIndex >= sNumFoundFunctions) {
            break;
        }

        if (currIndex == sStackTraceSelectedIndex) {
            crash_screen_draw_rect((TEXT_X(0) - 1), (y - 2), (CRASH_SCREEN_TEXT_W + 1), (TEXT_HEIGHT(1) + 1), COLOR_RGBA32_CRASH_SELECT);
        }

        function = &sFunctionStack[currIndex];

        if (function == NULL) {
            continue;
        }

        // "[stack address]:"
        const size_t addrStrSize = crash_screen_print(TEXT_X(0), y, STR_HEX_WORD":", function->stackAddr);

        if (function->fname == NULL) {
            // Print unknown function
            // "[function address]"
            crash_screen_print(TEXT_X(addrStrSize), y,
                (STR_COLOR_PREFIX STR_HEX_WORD),
                COLOR_RGBA32_CRASH_UNKNOWN, function->curAddr
            );
        } else {
            // Print known function
            if (sStackTraceShowFunctionNames) {
                // "[function name]"
                const size_t offsetChars = STRLEN("+0000");
                crash_screen_print_scroll(TEXT_X(addrStrSize), y,
                    (CRASH_SCREEN_NUM_CHARS_X - (addrStrSize + offsetChars)),
                    STR_COLOR_PREFIX"%s",
                    COLOR_RGBA32_CRASH_FUNCTION_NAME_2, function->fname
                );
                // "+[offset]"
                crash_screen_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - offsetChars), y,
                    (STR_COLOR_PREFIX"+"STR_HEX_HALFWORD),
                    COLOR_RGBA32_CRASH_FUNCTION_NAME_2, (function->curAddr - function->faddr)
                );
            } else {
                // "[function address]"
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

    u32 line = 1;

    // "FROM: [XXXXXXXX]"
    crash_screen_print(TEXT_X(12), TEXT_Y(line), "FROM "STR_HEX_WORD, (uintptr_t)tc->sp);

    line++;

#ifdef INCLUDE_DEBUG_MAP
    crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"CURRFUNC:", COLOR_RGBA32_CRASH_AT);
    uintptr_t pc = tc->pc;
    const char* fname = parse_map(&pc);
    if (fname == NULL) {
        // "UNKNOWN"
        crash_screen_print(TEXT_X(9), TEXT_Y(line), STR_COLOR_PREFIX"UNKNOWN", COLOR_RGBA32_CRASH_UNKNOWN);
    } else {
        // "[function name]"
        crash_screen_print_scroll(TEXT_X(9), TEXT_Y(line), (CRASH_SCREEN_NUM_CHARS_X - 9), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_FUNCTION_NAME, fname);
    }

    line++;

    stack_trace_print_entries(line, STACK_TRACE_NUM_ROWS);

    crash_screen_draw_divider(DIVIDER_Y(line));

    // Scroll Bar
    if (sNumFoundFunctions > STACK_TRACE_NUM_ROWS) {
        //! TODO: crash_screen_draw_scroll_bar(DIVIDER_Y(3), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y), STACK_TRACE_NUM_ROWS, sNumFoundFunctions, sStackTraceViewportIndex, 4, COLOR_RGBA32_LIGHT_GRAY);

        crash_screen_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));
    }
#else
    // "STACK TRACE DISABLED"
    crash_screen_print(TEXT_X(0), TEXT_Y(line), "STACK TRACE DISABLED");
#endif

    osWritebackDCacheAll();
}

void stack_trace_input(void) {
#ifdef INCLUDE_DEBUG_MAP
    u16 buttonPressed = gPlayer1Controller->buttonPressed;
    
    if (buttonPressed & A_BUTTON) {
        open_address_select(sFunctionStack[sStackTraceSelectedIndex].curAddr);
    }

    if (buttonPressed & B_BUTTON) {
        // Toggle whether to display function names.
        sStackTraceShowFunctionNames ^= TRUE;
    }

    if (gCSDirectionFlags.held.up) {
        // Scroll up.
        if (sStackTraceSelectedIndex > 0) {
            sStackTraceSelectedIndex--;
        }
    }
    if (gCSDirectionFlags.held.down) {
        // Scroll down.
        if (sStackTraceSelectedIndex < (sNumFoundFunctions - 1)) {
            sStackTraceSelectedIndex++;
        }
    }
#endif
}
