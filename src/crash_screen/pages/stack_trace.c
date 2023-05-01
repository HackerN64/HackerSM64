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

const enum ControlTypes stackTraceContList[] = {
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
        .curAddr   = 0,
        .faddr     = 0,
        .fname     = NULL,
    };

    Register* sp = (Register*)(Address)tc->sp; // Stack pointer is already aligned, so get the lower bits.

    // Loop through the stack buffer and find all the addresses that point to a function.
    while ((Byte*)sp < _buffersSegmentBssEnd && sNumFoundFunctions < STACK_TRACE_BUFFER_SIZE) {
        currInfo.curAddr = (Address)(*sp);

        if (is_in_code_segment(currInfo.curAddr)) {
            currInfo.faddr = currInfo.curAddr;
            currInfo.fname = parse_map(&currInfo.faddr);
            if (currInfo.fname != NULL) {
                //! TODO: If JAL command uses a different function than the previous entry's faddr, replace it with the one in the JAL command?
                //! TODO: handle duplicate entries caused by JALR RA, V0
                currInfo.stackAddr = (Address)sp + sizeof(Address);
                sFunctionStack[sNumFoundFunctions++] = currInfo;
            }

            if (currInfo.faddr == (Address)__osCleanupThread) {
                break;
            }
        }

        sp++;
    }
}
#endif

void stack_trace_init(void) {
    bzero(&sFunctionStack, sizeof(sFunctionStack));

    sStackTraceShowFunctionNames = TRUE;

    sStackTraceSelectedIndex = 0;
    sStackTraceViewportIndex = 0;

    sNumFoundFunctions = 0;

#ifdef INCLUDE_DEBUG_MAP
    // Include the current function at the top:
    __OSThreadContext* tc = &gCrashedThread->context;
    Address pc = tc->pc;
    const char* fname = parse_map(&pc);
    struct FunctionInStack currFunc = {
        .stackAddr = 0,
        .curAddr   = tc->pc,
        .faddr     = pc,
        .fname     = fname,
    };
    sFunctionStack[sNumFoundFunctions++] = currFunc;

    fill_function_stack_trace();
#endif
}

void stack_trace_print_entries(u32 line, u32 numLines) {
    u32 currIndex = sStackTraceViewportIndex;
    struct FunctionInStack* function = &sFunctionStack[currIndex];

    // Print
    for (u32 i = 0; i < numLines; i++) {
        if (currIndex >= sNumFoundFunctions) {
            break;
        }

        if (function == NULL) {
            break;
        }

        u32 y = TEXT_Y(line + i);

        if (currIndex == sStackTraceSelectedIndex) {
            crash_screen_draw_rect(
                (TEXT_X(0) - 1), (y - 2),
                (CRASH_SCREEN_TEXT_W + 1), (TEXT_HEIGHT(1) + 1),
                COLOR_RGBA32_CRASH_SELECT
            );
        }

        const size_t addrStrSize = STRLEN("00000000:");
        // "[stack address]:"
        if (currIndex == 0) {
            crash_screen_print(TEXT_X(0), y, STR_COLOR_PREFIX"CURRFUNC:", COLOR_RGBA32_CRASH_AT);
        } else {
            crash_screen_print(TEXT_X(0), y, STR_HEX_WORD":", function->stackAddr);
        }

        if (function->fname == NULL) {
            // Print unknown function
            // "[function address]"
            crash_screen_print(TEXT_X(addrStrSize), y,
                (STR_COLOR_PREFIX STR_HEX_WORD),
                COLOR_RGBA32_CRASH_UNKNOWN, function->curAddr
            );
        } else {
            const RGBA32 nameColor = ((currIndex == 0) ? COLOR_RGBA32_CRASH_FUNCTION_NAME : COLOR_RGBA32_CRASH_FUNCTION_NAME_2);
            // Print known function
            if (sStackTraceShowFunctionNames) {
                // "[function name]"
                const size_t offsetStrSize = STRLEN("+0000");
                crash_screen_print_map_name(TEXT_X(addrStrSize), y,
                    (CRASH_SCREEN_NUM_CHARS_X - (addrStrSize + offsetStrSize)),
                    nameColor, function->fname
                );
                // "+[offset]"
                crash_screen_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - offsetStrSize), y,
                    (STR_COLOR_PREFIX"+"STR_HEX_HALFWORD),
                    COLOR_RGBA32_CRASH_FUNCTION_NAME_2, (function->curAddr - function->faddr)
                );
            } else {
                // "[function address]"
                crash_screen_print(TEXT_X(addrStrSize), y,
                    (STR_COLOR_PREFIX STR_HEX_WORD),
                    nameColor, function->curAddr
                );
            }
        }

        currIndex++;
        function++;
    }

    osWritebackDCacheAll();
}

// prints any function pointers it finds in the stack format:
// SP address: function name
void stack_trace_draw(void) {
    __OSThreadContext* tc = &gCrashedThread->context;

    u32 line = 1;

    // "FROM: [XXXXXXXX]"
    crash_screen_print(TEXT_X(12), TEXT_Y(line), "FROM "STR_HEX_WORD, (Address)tc->sp);
    crash_screen_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - STRLEN("OFFSET")), TEXT_Y(line), STR_COLOR_PREFIX"OFFSET:", COLOR_RGBA32_CRASH_FUNCTION_NAME_2);

    line++;

#ifdef INCLUDE_DEBUG_MAP
    sStackTraceViewportIndex = clamp_view_to_selection(sStackTraceViewportIndex, sStackTraceSelectedIndex, STACK_TRACE_NUM_ROWS, 1);

    stack_trace_print_entries(line, STACK_TRACE_NUM_ROWS);

    // Draw the top line after the entries so the selection rectangle is behind it.
    crash_screen_draw_divider(DIVIDER_Y(line));

    // Scroll Bar
    if (sNumFoundFunctions > STACK_TRACE_NUM_ROWS) {
        crash_screen_draw_scroll_bar(
            (DIVIDER_Y(line) + 1), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y), 
            STACK_TRACE_NUM_ROWS, sNumFoundFunctions,
            sStackTraceViewportIndex,
            COLOR_RGBA32_LIGHT_GRAY, TRUE
        );

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

    if (gCSDirectionFlags.pressed.up) {
        // Scroll up.
        if (sStackTraceSelectedIndex > 0) {
            sStackTraceSelectedIndex--;
        }
    }
    if (gCSDirectionFlags.pressed.down) {
        // Scroll down.
        if (sStackTraceSelectedIndex < (sNumFoundFunctions - 1)) {
            sStackTraceSelectedIndex++;
        }
    }
#endif
}
