#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/address_select.h"
#include "crash_screen/crash_controls.h"
#include "crash_screen/crash_draw.h"
#include "crash_screen/crash_main.h"
#include "crash_screen/crash_pages.h"
#include "crash_screen/crash_print.h"
#include "crash_screen/crash_settings.h"
#include "crash_screen/map_parser.h"

#include "page_stack.h"

#include "segment_symbols.h"


struct CSSetting cs_settings_group_page_stack[] = {
    [CS_OPT_HEADER_PAGE_STACK   ] = { .type = CS_OPT_TYPE_HEADER,  .name = "STACK TRACE",                    .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_STACK_SHOW_ADDRESSES] = { .type = CS_OPT_TYPE_SETTING, .name = "Show addresses",                 .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_STACK_SHOW_OFFSETS  ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show offsets",                   .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_END_STACK           ] = { .type = CS_OPT_TYPE_END },
};


const enum ControlTypes stackTraceContList[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_SCROLL_LIST,
    CONT_DESC_JUMP_TO_ADDRESS,
#ifdef INCLUDE_DEBUG_MAP
    CONT_DESC_TOGGLE_FUNCTIONS,
#endif
    CONT_DESC_LIST_END,
};


ALIGNED16 static FunctionInStack sCSFunctionStackBuffer[STACK_TRACE_BUFFER_SIZE];
static u32 sCSNumFoundFunctions = 0;

static u32 sStackTraceSelectedIndex = 0;
static u32 sStackTraceViewportIndex = 0;

static void add_to_stack(FunctionInStack* func) {
    sCSFunctionStackBuffer[sCSNumFoundFunctions++] = *func;
}

extern void __osCleanupThread(void);

void fill_function_stack_trace(void) {
    bzero(&sCSFunctionStackBuffer, sizeof(sCSFunctionStackBuffer));
    sCSNumFoundFunctions = 0;

    // Include the current function at the top:
    __OSThreadContext* tc = &gCrashedThread->context;
    const MapSymbol* symbol = get_map_symbol(tc->pc, SYMBOL_SEARCH_BACKWARD);
    FunctionInStack currInfo = {
        .stackAddr = tc->sp,
        .curAddr   = tc->pc,
        .faddr     = (symbol ? symbol->addr : tc->pc),
        .fname     = get_map_symbol_name(symbol),
    };
    add_to_stack(&currInfo);

    Register* sp = (Register*)(Address)tc->sp; // Stack pointer is already aligned, so get the lower bits.

    // Loop through the stack buffer and find all the addresses that point to a function.
    while (sCSNumFoundFunctions < STACK_TRACE_BUFFER_SIZE) {
        currInfo.curAddr = (Address)(*sp); // Check the lower bits.

        if (is_in_code_segment(currInfo.curAddr)) {
            currInfo.faddr = currInfo.curAddr;
#ifdef INCLUDE_DEBUG_MAP
            symbol = get_map_symbol(currInfo.faddr, SYMBOL_SEARCH_BACKWARD);
            if (symbol != NULL) {
                currInfo.faddr = symbol->addr;
                currInfo.fname = get_map_symbol_name(symbol);
            }

            if (currInfo.fname != NULL)
#endif
            {
                //! TODO: If JAL command uses a different function than the previous entry's faddr, replace it with the one in the JAL command?
                //! TODO: handle duplicate entries caused by JALR RA, V0
                currInfo.stackAddr = (Address)sp + sizeof(Address);
                add_to_stack(&currInfo);
            }

            if (currInfo.faddr == (Address)__osCleanupThread) {
                break;
            }
        }

        sp++;
    }
}

void stack_trace_init(void) {
    sStackTraceSelectedIndex = 0;
    sStackTraceViewportIndex = 0;

    fill_function_stack_trace();
}

void stack_trace_print_entries(u32 line, u32 numLines) {
    const _Bool showAddressess   = cs_get_setting_val(CS_OPT_GROUP_PAGE_STACK, CS_OPT_STACK_SHOW_ADDRESSES);
#ifdef INCLUDE_DEBUG_MAP
    const _Bool showOffsets      = cs_get_setting_val(CS_OPT_GROUP_PAGE_STACK, CS_OPT_STACK_SHOW_OFFSETS  );
    const _Bool parseSymbolNames = cs_get_setting_val(CS_OPT_GROUP_GLOBAL,     CS_OPT_GLOBAL_SYMBOL_NAMES        );
#endif
    u32 currIndex = sStackTraceViewportIndex;
    FunctionInStack* function = &sCSFunctionStackBuffer[currIndex];

    // Print
    for (u32 i = 0; i < numLines; i++) {
        if (currIndex >= sCSNumFoundFunctions) {
            break;
        }

        if (function == NULL) {
            break;
        }

        u32 y = TEXT_Y(line + i);

        if (currIndex == sStackTraceSelectedIndex) {
            cs_draw_row_selection_box(y);
        }

        size_t charX = 0;

        if (showAddressess) {
            // "[stack address]:"
            charX += cs_print(TEXT_X(charX), y,
                STR_COLOR_PREFIX STR_HEX_WORD":",
                ((currIndex == 0) ? COLOR_RGBA32_CRASH_AT : COLOR_RGBA32_WHITE), function->stackAddr
            );
        }

#ifdef INCLUDE_DEBUG_MAP
        if (function->fname == NULL) {
            // Print unknown function.
            // "[function address]"
            cs_print(TEXT_X(charX), y,
                (STR_COLOR_PREFIX STR_HEX_WORD),
                COLOR_RGBA32_CRASH_UNKNOWN, function->curAddr
            );
        } else {
            // Print known function.
            if (parseSymbolNames) {
                size_t offsetStrSize = 0;
                if (showOffsets) {
                    offsetStrSize = STRLEN("+0000");
                    // "+[offset]"
                    cs_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - offsetStrSize), y,
                        (STR_COLOR_PREFIX"+"STR_HEX_HALFWORD),
                        COLOR_RGBA32_CRASH_OFFSET, (function->curAddr - function->faddr)
                    );
                }
                // "[function name]"
                cs_print_symbol_name_impl(TEXT_X(charX), y,
                    (CRASH_SCREEN_NUM_CHARS_X - (charX + offsetStrSize)),
                    COLOR_RGBA32_CRASH_FUNCTION_NAME, function->fname
                );
            } else {
                // "[function address]"
                cs_print(TEXT_X(charX), y,
                    (STR_COLOR_PREFIX STR_HEX_WORD),
                    COLOR_RGBA32_CRASH_FUNCTION_NAME, function->curAddr
                );
            }
        }
#else
        // "[function address]"
        cs_print(TEXT_X(charX), y,
            (STR_COLOR_PREFIX STR_HEX_WORD),
            COLOR_RGBA32_CRASH_FUNCTION_NAME, function->curAddr
        );
#endif

        currIndex++;
        function++;
    }

    osWritebackDCacheAll();
}

// prints any function pointers it finds in the stack format:
// SP address: function name
void stack_trace_draw(void) {
    u32 line = 1;

#ifdef INCLUDE_DEBUG_MAP
    if (cs_get_setting_val(CS_OPT_GROUP_PAGE_STACK, CS_OPT_STACK_SHOW_OFFSETS)) {
        // "OFFSET:"
        cs_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - STRLEN("OFFSET:")), TEXT_Y(line), STR_COLOR_PREFIX"OFFSET:", COLOR_RGBA32_CRASH_OFFSET);
    }
#endif

    line++;

    stack_trace_print_entries(line, STACK_TRACE_NUM_ROWS);

    // Draw the top line after the entries so the selection rectangle is behind it.
    cs_draw_divider(DIVIDER_Y(line));

    // Scroll Bar:
    if (sCSNumFoundFunctions > STACK_TRACE_NUM_ROWS) {
        cs_draw_scroll_bar(
            (DIVIDER_Y(line) + 1), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y),
            STACK_TRACE_NUM_ROWS, sCSNumFoundFunctions,
            sStackTraceViewportIndex,
            COLOR_RGBA32_CRASH_DIVIDER, TRUE
        );

        cs_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));
    }

    osWritebackDCacheAll();
}

void stack_trace_input(void) {
    u16 buttonPressed = gCSCompositeController->buttonPressed;

    if (buttonPressed & A_BUTTON) {
        open_address_select(sCSFunctionStackBuffer[sStackTraceSelectedIndex].curAddr);
    }

#ifdef INCLUDE_DEBUG_MAP
    if (buttonPressed & B_BUTTON) {
        // Toggle whether to display function names.
        cs_inc_setting(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_SYMBOL_NAMES, TRUE);
    }
#endif

    s32 change = 0;
    if (gCSDirectionFlags.pressed.up  ) change = -1; // Scroll up.
    if (gCSDirectionFlags.pressed.down) change = +1; // Scroll down.
    sStackTraceSelectedIndex = WRAP(((s32)sStackTraceSelectedIndex + change), 0, (s32)(sCSNumFoundFunctions - 1));

    sStackTraceViewportIndex = cs_clamp_view_to_selection(sStackTraceViewportIndex, sStackTraceSelectedIndex, STACK_TRACE_NUM_ROWS, 1);
}

struct CSPage gCSPage_stack = {
    .name         = "STACK TRACE",
    .initFunc     = stack_trace_init,
    .drawFunc     = stack_trace_draw,
    .inputFunc    = stack_trace_input,
    .contList     = stackTraceContList,
    .settingsList = cs_settings_group_page_stack,
    .flags = {
        .initialized = FALSE,
        .crashed     = FALSE,
        .printName   = TRUE,
    },
};
