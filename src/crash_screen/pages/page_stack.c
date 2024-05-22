#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/util/insn_disasm.h"
#include "crash_screen/util/map_parser.h"
#include "crash_screen/cs_controls.h"
#include "crash_screen/cs_descriptions.h"
#include "crash_screen/cs_draw.h"
#include "crash_screen/cs_main.h"
#include "crash_screen/cs_pages.h"
#include "crash_screen/cs_print.h"
#include "crash_screen/cs_settings.h"

#include "crash_screen/popups/popup_address.h"

#include "page_stack.h"

#include "segment_symbols.h"

#ifdef UNF
#include "usb/usb.h"
#include "usb/debug.h"
#endif // UNF


struct CSSetting cs_settings_group_page_stack[] = {
    [CS_OPT_HEADER_PAGE_STACK       ] = { .type = CS_OPT_TYPE_HEADER,  .name = "STACK TRACE",                    .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_STACK_SHOW_ADDRESSES    ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show addresses in stack",        .valNames = &gValNames_bool,          .val = FALSE,                     .defaultVal = FALSE,                     .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_STACK_SHOW_OFFSETS      ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show function offsets",          .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_END_STACK               ] = { .type = CS_OPT_TYPE_END, },
};


const enum ControlTypes cs_cont_list_stack[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_PAGE_SELECT,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_HIDE_CRASH_SCREEN,
#ifdef UNF
    CONT_DESC_OS_PRINT,
#endif // UNF
    CONT_DESC_SCROLL_LIST,
    CONT_DESC_JUMP_TO_ADDRESS,
#ifdef INCLUDE_DEBUG_MAP
    CONT_DESC_TOGGLE_FUNCTIONS,
#endif // INCLUDE_DEBUG_MAP
    CONT_DESC_LIST_END,
};


ALIGNED8 static FunctionInStack sStackTraceBuffer[STACK_TRACE_BUFFER_SIZE];
static u32 sStackTraceBufferEnd = 0;

static u32 sStackTraceSelectedIndex = 0;
static u32 sStackTraceViewportIndex = 0;
static u32 sStackTraceNumShownRows = STACK_TRACE_NUM_ROWS;

static const char* sStackTraceError = NULL;


// Jumps with links store the address of the instruction after the delay slot to $ra ($ra = $pc + 2).
#define LINK_SIZE (2 * sizeof(InsnData))

extern void __osCleanupThread(void);

static void set_stack_trace_error_mesg(const char* errorStr) {
    if (sStackTraceError == NULL) {
        sStackTraceError = errorStr;
    }
}

static void append_stack_entry_to_buffer(Address stackAddr, Address currAddr) {
    sStackTraceBuffer[sStackTraceBufferEnd++] = (FunctionInStack){
        .stackAddr = stackAddr,
        .currAddr  = currAddr,
    };
}

#define STACK_TRACE_MAX_INSN_SCAN (size_t)1024

//! TODO: Return should determine whether leaf or null symbol.
_Bool stacktrace_step(Address** sp, Address** ra, Address addr) {
    const MapSymbol* symbol = get_map_symbol(addr, SYMBOL_SEARCH_BACKWARD);
    if (symbol == NULL) {
        set_stack_trace_error_mesg("null symbol");
        return FALSE;
    }
    size_t stack_size = 0;
    size_t ra_offset = 0;

    // Get stack_size and ra_offset.
    InsnData* insnPtr = (InsnData*)symbol->addr; // Starting addr.

    const size_t numInsnsInFunc = MIN(STACK_TRACE_MAX_INSN_SCAN, (symbol->size / sizeof(InsnData)));
    for (size_t i = 0; i < numInsnsInFunc; i++) {
        InsnData insn = *insnPtr;
        u16 insnHi = (insn.raw >> 16);
        s16 imm = insn.immediate;

        //! TODO: Use defines/enums for insn check magic values:
        if (
            (stack_size == 0) &&
            ((insnHi == 0x27BD) || (insnHi == 0x67BD)) // [addiu/daddiu] $sp, $sp, 0xXXXX
        ) {
            if (imm < 0) {
                stack_size = -imm;
            }
        } else if (
            ((insnHi == 0xFFBF) || (insnHi == 0xAFBF)) // [sw/sd] $ra, 0xXXXX($sp)
        ) {
            ra_offset = imm;
        }

        // Exit the loop if both have been found.
        if ((stack_size != 0) && (ra_offset != 0)) {
            *ra = (Address*)((Address)*sp + ra_offset);
            *sp = (Address*)((Address)*sp + stack_size);
            return TRUE;
        }

        insnPtr++;
    }

    // No stack allocation found, so thit is a leaf function.
    return FALSE;
}

static const char* stack_error_leaf_not_at_top = "leaf func found not at top";

//! TODO: Failsafe in case the address of __osCleanupThread doesn't appear in the stack.
_Bool fill_function_stack_trace(void) {
    bzero(&sStackTraceBuffer, sizeof(sStackTraceBuffer));
    sStackTraceBufferEnd = 0;

    _Bool success = FALSE; 

    // Include the current function at the top:
    __OSThreadContext* tc = &gInspectThread->context;
    const Address epc  = GET_EPC(tc); // thread $pc
    const Address t_sp = tc->sp;      // thread $sp
    append_stack_entry_to_buffer((Address)t_sp, epc); // Set the first entry in the stack buffer to the function $EPC is in.

    Address* sp = (Address*)((Address)t_sp);

    if (IS_DEBUG_MAP_ENABLED()) {
        const Address t_ra = ((Address)tc->ra - LINK_SIZE); // thread $ra - link
        Address* ra = (Address*)t_ra;
        _Bool err = FALSE;

        if (!stacktrace_step(&sp, &ra, epc)) {
            // $EPC is in a leaf function, so get the next entry from $ra because it's not in the stack.
            if (stacktrace_step(&sp, &ra, t_ra)) {
                append_stack_entry_to_buffer(t_sp, t_ra);
            } else {
                set_stack_trace_error_mesg(stack_error_leaf_not_at_top);
                err = TRUE;
            }
        }

        if (!err) {
            // Loop through the stack buffer and find all the addresses that point to a function.
            while (sStackTraceBufferEnd < STACK_TRACE_BUFFER_SIZE) {
                if ((Address)*ra == (Address)__osCleanupThread) {
                    success = TRUE;
                    break;
                }
                Address raddr = ((Address)*ra - LINK_SIZE);
                append_stack_entry_to_buffer((Address)ra, raddr);
                if (!stacktrace_step(&sp, &ra, raddr)) {
                    set_stack_trace_error_mesg(stack_error_leaf_not_at_top);
                    break;
                }
            }
        }
    }

    // Fallback simpler stack trace functionality in case walking the stack fails or the debug map is disabled.
    // Just uses all aligned .text addresses in the stack.
    if (!success) {
        // Align sp to lower 32 of 64:
        Address* ra = (Address*)(ALIGNFLOOR((Address)sp, sizeof(Doubleword)) + sizeof(Address));

        // Loop through the stack buffer and find all the addresses that point to a function.
        while (sStackTraceBufferEnd < STACK_TRACE_BUFFER_SIZE) {
            if (*ra == (Address)__osCleanupThread) {
                success = TRUE;
                break;
            }
            Address raddr = ((Address)*ra - LINK_SIZE);
            if (addr_is_in_text_segment(raddr)) {
                append_stack_entry_to_buffer((Address)ra, raddr);
            }

            ra += 2;
        }
    }

    return success;
}

void page_stack_init(void) {
    sStackTraceSelectedIndex = 0;
    sStackTraceViewportIndex = 0;
    sStackTraceNumShownRows = STACK_TRACE_NUM_ROWS;

    sStackTraceError = NULL;

    if (!cs_get_current_page()->flags.crashed) {
        fill_function_stack_trace();
        if (sStackTraceError != NULL) {
            sStackTraceNumShownRows--;
        }
    }
}

void stack_trace_print_entries(CSTextCoord_u32 line, CSTextCoord_u32 numLines) {
    const _Bool showAddressess   = cs_get_setting_val(CS_OPT_GROUP_PAGE_STACK, CS_OPT_STACK_SHOW_ADDRESSES);
#ifdef INCLUDE_DEBUG_MAP
    const _Bool showOffsets      = cs_get_setting_val(CS_OPT_GROUP_PAGE_STACK, CS_OPT_STACK_SHOW_OFFSETS  );
    const _Bool parseSymbolNames = cs_get_setting_val(CS_OPT_GROUP_GLOBAL,     CS_OPT_GLOBAL_SYMBOL_NAMES );
#endif // INCLUDE_DEBUG_MAP
    u32 currIndex = sStackTraceViewportIndex;
    FunctionInStack* function = &sStackTraceBuffer[currIndex];

    // Print:
    for (CSTextCoord_u32 row = 0; row < numLines; row++) {
        if (currIndex >= sStackTraceBufferEnd) {
            break;
        }

        if (function == NULL) {
            break;
        }

        ScreenCoord_u32 y = TEXT_Y(line + row);

        if (currIndex == sStackTraceSelectedIndex) {
            cs_draw_row_selection_box(y);
        }

        CSTextCoord_u32 charX = 0;

        if (showAddressess) {
            // "[stack address]:"
            charX += cs_print(TEXT_X(charX), y,
                STR_COLOR_PREFIX STR_HEX_WORD":",
                ((currIndex == 0) ? COLOR_RGBA32_CRASH_AT : COLOR_RGBA32_WHITE),
                function->stackAddr
            );
        } else if (row > 0) {
            charX += cs_print(TEXT_X(charX), y,
                STR_COLOR_PREFIX"at ",
                COLOR_RGBA32_GRAY
            );
        }

        Address currAddr = function->currAddr;
        const MapSymbol* symbol = (IS_DEBUG_MAP_ENABLED() ? get_map_symbol(currAddr, SYMBOL_SEARCH_BINARY) : NULL); //! TODO: SYMBOL_SEARCH_BACKWARDS here slows down rendering.
        if (symbol == NULL) {
            // Print unknown function as just the address.
            cs_print(TEXT_X(charX), y,
                (STR_COLOR_PREFIX STR_HEX_WORD),
                (IS_DEBUG_MAP_ENABLED() ? COLOR_RGBA32_CRASH_FUNCTION_NAME : COLOR_RGBA32_CRASH_UNKNOWN), currAddr
            );
        } else {
            // Print known function.
            if (parseSymbolNames) {
                CSTextCoord_u32 offsetStrSize = 0;
                if (showOffsets) {
                    offsetStrSize = STRLEN("+0000");
                    // "+[offset]"
                    cs_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - offsetStrSize), y,
                        (STR_COLOR_PREFIX"+"STR_HEX_HALFWORD),
                        COLOR_RGBA32_CRASH_OFFSET, (currAddr - symbol->addr)
                    );
                }
                // "[function name]"
                cs_print_scroll(TEXT_X(charX), y,
                    (CRASH_SCREEN_NUM_CHARS_X - (charX + offsetStrSize)),
                    STR_COLOR_PREFIX"%s",
                    COLOR_RGBA32_CRASH_FUNCTION_NAME, get_map_symbol_name(symbol)
                );
                // cs_print_addr_location_info(TEXT_X(charX), y, (CRASH_SCREEN_NUM_CHARS_X - charX), currAddr, TRUE);
            } else {
                // "[address in function]"
                cs_print(TEXT_X(charX), y,
                    (STR_COLOR_PREFIX STR_HEX_WORD),
                    COLOR_RGBA32_CRASH_FUNCTION_NAME, currAddr
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
void page_stack_draw(void) {
    OSThread* thread = gInspectThread;
    CSTextCoord_u32 line = 1;
    CSTextCoord_u32 charX = 0;

    charX += cs_print(TEXT_X(charX), TEXT_Y(line++),
        (STR_COLOR_PREFIX"SP: "STR_COLOR_PREFIX STR_HEX_WORD" "STR_COLOR_PREFIX"in thread: "STR_COLOR_PREFIX"%d %s"),
        COLOR_RGBA32_CRASH_VARIABLE, COLOR_RGBA32_WHITE,
        (Address)thread->context.sp,
        COLOR_RGBA32_CRASH_HEADER, COLOR_RGBA32_CRASH_THREAD_NAME,
        thread->id, str_null_fallback(get_thread_name(thread), "unknown")
    );

    cs_draw_divider(DIVIDER_Y(line));
    charX = 0;
    if (cs_get_setting_val(CS_OPT_GROUP_PAGE_STACK, CS_OPT_STACK_SHOW_ADDRESSES)) {
        cs_print(TEXT_X(0), TEXT_Y(line), "IN STACK:");
        charX = STRLEN("00000000:");
    }
    cs_print(TEXT_X(charX), TEXT_Y(line), STR_COLOR_PREFIX"FUNCTION:", COLOR_RGBA32_CRASH_FUNCTION_NAME);

#ifdef INCLUDE_DEBUG_MAP
    if (
        IS_DEBUG_MAP_ENABLED() &&
        cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_SYMBOL_NAMES) &&
        cs_get_setting_val(CS_OPT_GROUP_PAGE_STACK, CS_OPT_STACK_SHOW_OFFSETS)
    ) {
        // "OFFSET:"
        cs_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - STRLEN("OFFSET:")), TEXT_Y(line), STR_COLOR_PREFIX"OFFSET:", COLOR_RGBA32_CRASH_OFFSET);
    }
#endif // INCLUDE_DEBUG_MAP

    line++;

    stack_trace_print_entries(line, sStackTraceNumShownRows);

    // Draw the top line after the entries so the selection rectangle is behind it.
    cs_draw_divider(DIVIDER_Y(line));

    // Scroll Bar:
    if (sStackTraceBufferEnd > sStackTraceNumShownRows) {
        cs_draw_scroll_bar(
            (DIVIDER_Y(line) + 1), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y),
            sStackTraceNumShownRows, sStackTraceBufferEnd,
            sStackTraceViewportIndex,
            COLOR_RGBA32_CRASH_SCROLL_BAR, TRUE
        );

        cs_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));
    }

    if (sStackTraceError != NULL) {
        cs_print(TEXT_X(0), TEXT_Y(CRASH_SCREEN_NUM_CHARS_Y - 1),
            STR_COLOR_PREFIX"stack error: %s",
            COLOR_RGBA32_CRASH_DESCRIPTION_MAIN, sStackTraceError
        );
    }

    osWritebackDCacheAll();
}

void page_stack_input(void) {
    u16 buttonPressed = gCSCompositeController->buttonPressed;

    if (buttonPressed & A_BUTTON) {
        open_address_select(sStackTraceBuffer[sStackTraceSelectedIndex].currAddr);
    }

    s32 change = 0;
    if (gCSDirectionFlags.pressed.up  ) change = -1; // Scroll up.
    if (gCSDirectionFlags.pressed.down) change = +1; // Scroll down.
    sStackTraceSelectedIndex = WRAP(((s32)sStackTraceSelectedIndex + change), 0, (s32)(sStackTraceBufferEnd - 1));

    sStackTraceViewportIndex = cs_clamp_view_to_selection(sStackTraceViewportIndex, sStackTraceSelectedIndex, sStackTraceNumShownRows, 1);
}

void page_stack_print(void) {
#ifdef UNF
    osSyncPrintf("\n");

    for (u32 i = 0; i < sStackTraceBufferEnd; i++) {
        FunctionInStack* function = &sStackTraceBuffer[i];

        if (function == NULL) {
            break;
        }

        Address currAddr = function->currAddr;
        const MapSymbol* symbol = get_map_symbol(currAddr, SYMBOL_SEARCH_BACKWARD);
        osSyncPrintf("- ["STR_HEX_WORD"]: "STR_HEX_WORD" +"STR_HEX_HALFWORD, function->stackAddr, symbol->addr, (currAddr - symbol->addr));
        if (IS_DEBUG_MAP_ENABLED()) {
            const char* fname = get_map_symbol_name(symbol);
            if (fname != NULL) {
                osSyncPrintf(" - %s", fname);
            }
        } else {
            osSyncPrintf("\n");
        }
    }
#endif // UNF
}


struct CSPage gCSPage_stack = {
    .name         = "STACK TRACE",
    .initFunc     = page_stack_init,
    .drawFunc     = page_stack_draw,
    .inputFunc    = page_stack_input,
    .printFunc    = page_stack_print,
    .contList     = cs_cont_list_stack,
    .settingsList = cs_settings_group_page_stack,
    .flags = {
        .initialized = FALSE,
        .crashed     = FALSE,
    },
};
