#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/util/insn_disasm.h"
#include "crash_screen/util/map_parser.h"
#include "crash_screen/util/memory_read.h"
#include "crash_screen/util/registers.h"
#include "crash_screen/crash_controls.h"
#include "crash_screen/crash_descriptions.h"
#include "crash_screen/crash_draw.h"
#include "crash_screen/crash_main.h"
#include "crash_screen/crash_pages.h"
#include "crash_screen/crash_print.h"
#include "crash_screen/crash_settings.h"

#include "page_home.h"

#include "page_disasm.h"

#include "game/asm.h"
#ifdef UNF
#include "usb/debug.h"
#endif // UNF


struct CSSetting cs_settings_group_page_home[] = {
    [CS_OPT_HEADER_PAGE_HOME        ] = { .type = CS_OPT_TYPE_HEADER,  .name = "HOME",                           .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_END_HOME                ] = { .type = CS_OPT_TYPE_END, },
};


const enum ControlTypes cs_cont_list_home[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_PAGE_SELECT,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_HIDE_CRASH_SCREEN,
#ifdef UNF
    CONT_DESC_OS_PRINT,
#endif // UNF
    CONT_DESC_CYCLE_FLOATS_MODE,
    CONT_DESC_LIST_END,
};


void page_home_init(void) {

}

void cs_print_fpe_cause(u32 x, u32 y, u32 fpcsr) {
    // "FPE:"
    size_t charX = cs_print(x, y,
        STR_COLOR_PREFIX"FPE:\t",
        COLOR_RGBA32_CRASH_DESCRIPTION
    );
    x += TEXT_WIDTH(charX);

    const char* fpcsrDesc = get_fpcsr_desc(fpcsr, TRUE);
    if (fpcsrDesc != NULL) {
        // "([float exception description])"
        cs_print(x, y, STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_DESCRIPTION, fpcsrDesc);
    }
}

void cs_print_crashed_thread(u32 x, u32 y) {
    // "THREAD: [thread id]"
    enum ThreadID threadID = gCrashedThread->id;
    size_t charX = cs_print(x, y, STR_COLOR_PREFIX"THREAD:\t%d",
        COLOR_RGBA32_CRASH_THREAD, threadID
    );
    if (threadID < NUM_THREADS) {
        const char* threadName = get_thread_name_from_id(threadID);

        if (threadName != NULL) {
            // "(thread name)"
            cs_print(TEXT_X(charX + STRLEN(" ")), y, STR_COLOR_PREFIX"(%s)",
                COLOR_RGBA32_CRASH_THREAD, threadName
            );
        }
    }
}

#ifdef INCLUDE_DEBUG_MAP
void cs_print_func(u32 x, u32 y, __OSThreadContext* tc) {
    const MapSymbol* symbol = get_map_symbol(tc->pc, SYMBOL_SEARCH_BACKWARD);
    // "FUNC: [function name]"
    size_t charX = cs_print(x, y,
        STR_COLOR_PREFIX"FUNC:\t",
        COLOR_RGBA32_CRASH_AT
    );
    cs_print_symbol_name(TEXT_X(charX), y, (CRASH_SCREEN_NUM_CHARS_X - charX), symbol);
}
#endif // INCLUDE_DEBUG_MAP

void cs_print_cause(u32 x, u32 y, __OSThreadContext* tc) {
    const char* desc = get_cause_desc(tc);
    if (desc != NULL) {
        // "CAUSE: ([exception cause description])"
        cs_print(x, y,
            STR_COLOR_PREFIX"CAUSE:\t%s",
            COLOR_RGBA32_CRASH_DESCRIPTION, desc
        );
    }
}

// Draw the assert info.
//! TODO: Scrollable long asserts.
u32 cs_draw_assert(u32 line) {
    u32 charX = 0;

    gCSWordWrap = TRUE;

    cs_draw_rect(CRASH_SCREEN_X1, (DIVIDER_Y(line) + 1), CRASH_SCREEN_W, (TEXT_HEIGHT((CRASH_SCREEN_NUM_CHARS_Y - 2) - line) - 1), RGBA32_SET_ALPHA(COLOR_RGBA32_RED, 0x3F));

    // "ASSERT:"
    cs_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"ASSERT:", COLOR_RGBA32_CRASH_HEADER);
    line++;
    cs_draw_divider(DIVIDER_Y(line));

    size_t lineStrStart = (CRASH_SCREEN_NUM_CHARS_X - STRLEN("LINE:0000"));
    // "FILE: [file name]"
    charX += cs_print(TEXT_X(0), TEXT_Y(line),
        STR_COLOR_PREFIX"FILE:",
        COLOR_RGBA32_CRASH_HEADER
    );
    charX += cs_print_scroll(TEXT_X(charX), TEXT_Y(line), (lineStrStart - charX),
        STR_COLOR_PREFIX"%s",
        COLOR_RGBA32_CRASH_FILE_NAME, __n64Assert_Filename
    );
    // "LINE:[line number]"
    cs_print(TEXT_X(lineStrStart), TEXT_Y(line),
        STR_COLOR_PREFIX"LINE:"STR_COLOR_PREFIX"%d",
        COLOR_RGBA32_CRASH_HEADER,
        COLOR_RGBA32_CRASH_FILE_NAME, __n64Assert_LineNum
    );
    line++;

    //! TODO: print the function that called __n64Assert.
#ifdef INCLUDE_DEBUG_MAP
    if (__assert_address) {
        const MapSymbol* symbol = get_map_symbol(__assert_address, SYMBOL_SEARCH_BACKWARD);
        // "FUNC:[function name]"
        size_t charX = cs_print(
            TEXT_X(0), TEXT_Y(line),
            STR_COLOR_PREFIX"FUNC:",
            COLOR_RGBA32_CRASH_HEADER
        );
        cs_print_symbol_name(TEXT_X(charX), TEXT_Y(line), (CRASH_SCREEN_NUM_CHARS_X - charX), symbol);
        line++;
    }
#endif // INCLUDE_DEBUG_MAP

    // "COND:[condition]"
    if (__n64Assert_Condition != NULL) {
        charX = 0;
        charX += cs_print(TEXT_X(charX), TEXT_Y(line),
            STR_COLOR_PREFIX"COND:", COLOR_RGBA32_CRASH_HEADER
        );
        charX += cs_print_scroll(TEXT_X(charX), TEXT_Y(line),
            (CRASH_SCREEN_NUM_CHARS_X - charX),
            STR_COLOR_PREFIX"%s",
            COLOR_RGBA32_CRASH_AT, __n64Assert_Condition
        );
        line++;
    }

    // "MESSAGE:[message]"
    cs_print(TEXT_X(0), TEXT_Y(line),
        STR_COLOR_PREFIX"MESSAGE:\n"STR_COLOR_PREFIX"%s",
        COLOR_RGBA32_CRASH_HEADER,
        gCSDefaultPrintColor, __n64Assert_Message
    );
    line += gCSNumLinesPrinted;

    gCSWordWrap = FALSE;

    osWritebackDCacheAll();

    return line;
}

void cs_draw_register_info_long(u32 charX, u32 line, RegisterId reg) {
    const RegisterInfo* regInfo = get_reg_info(reg.cop, reg.idx);
    Word data = get_reg_val(reg.cop, reg.idx);
    _Bool isCOP1 = (reg.cop == COP1);

    charX += cs_print(TEXT_X(charX), TEXT_Y(line), (STR_COLOR_PREFIX"%s: "),
        COLOR_RGBA32_CRASH_VARIABLE, (isCOP1 ? regInfo->name : regInfo->shortName)
    );
    if (isCOP1) { // Float.
        charX += cs_print_f32(TEXT_X(charX), TEXT_Y(line), (IEEE754_f32){ .asU32 = data, }, TRUE);
    } else {
        charX += cs_print(TEXT_X(charX), TEXT_Y(line), STR_HEX_WORD" ", data);

#ifdef INCLUDE_DEBUG_MAP
        const MapSymbol* symbol = get_map_symbol(data, SYMBOL_SEARCH_BACKWARD);
        if (symbol != NULL) {
            size_t offsetStrSize = STRLEN("+0000 ");
            size_t endX = CRASH_SCREEN_NUM_CHARS_X;
            cs_print_symbol_name(TEXT_X(charX), TEXT_Y(line), (endX - (charX + offsetStrSize)), symbol);
            cs_print(TEXT_X(endX - offsetStrSize), TEXT_Y(line),
                (STR_COLOR_PREFIX"+"STR_HEX_HALFWORD" "),
                COLOR_RGBA32_CRASH_OFFSET, (data - symbol->addr)
            );
        }
#endif // INCLUDE_DEBUG_MAP
    }
}

void page_home_draw(void) {
    __OSThreadContext* tc = &gCrashedThread->context;
    u32 cause = (tc->cause & CAUSE_EXCMASK);
    u32 line = 9;

    if (cause == EXC_SYSCALL) {
        // ASSERT:
        cs_draw_divider(DIVIDER_Y(line));
        line = cs_draw_assert(line);
    } else {
        Address addr = tc->pc;
        Word data = 0x00000000;
        if (try_read_data(&data, addr) && is_in_code_segment(addr)) {
            cs_print(TEXT_X(0), TEXT_Y(line++),
                STR_COLOR_PREFIX"crash location:",
                COLOR_RGBA32_CRASH_PAGE_NAME
            );

            cs_draw_register_info_long(1, line++, (RegisterId){ .cop = COP0, .idx = REG_COP0_EPC, });
            // line++;

            cs_print(TEXT_X(0), TEXT_Y(line++),
                STR_COLOR_PREFIX"instruction at crash:",
                COLOR_RGBA32_CRASH_PAGE_NAME
            );

            // // Draw a red selection rectangle.
            // cs_draw_rect((TEXT_X(0) - 1), (TEXT_Y(line) - 2), (CRASH_SCREEN_TEXT_W + 1), (TEXT_HEIGHT(1) + 1), COLOR_RGBA32_CRASH_PC_HIGHLIGHT);
            // // "<-- CRASH"
            // cs_print((CRASH_SCREEN_TEXT_X2 - TEXT_WIDTH(STRLEN("<-- CRASH"))), TEXT_Y(line), STR_COLOR_PREFIX"<-- CRASH", COLOR_RGBA32_CRASH_AT);

            // cs_draw_divider(DIVIDER_Y(line));
            print_as_insn(TEXT_X(1), TEXT_Y(line++), addr, data);
            // cs_draw_divider(DIVIDER_Y(line));

            cs_print(TEXT_X(0), TEXT_Y(line++), STR_COLOR_PREFIX"values before instruction:", COLOR_RGBA32_CRASH_PAGE_NAME);
            for (int i = 0; i < gSavedRegBufSize; i++) {
                cs_draw_register_info_long(1, line++, gSavedRegBuf[i]);
            }
        }
    }

    line = 2;

    const s32 centerX = (CRASH_SCREEN_NUM_CHARS_X / 2);
    size_t len = STRLEN("CRASH AT:");
    cs_print(TEXT_X(centerX - (len / 2)), TEXT_Y(line++), STR_COLOR_PREFIX"CRASH AT:", COLOR_RGBA32_RED);
    line++;

    cs_print_crashed_thread(TEXT_X(0), TEXT_Y(line++));
#ifdef INCLUDE_DEBUG_MAP
    cs_print_func(TEXT_X(0), TEXT_Y(line++), tc);
#endif // INCLUDE_DEBUG_MAP
    cs_print_cause(TEXT_X(0), TEXT_Y(line++), tc);

    if (cause == EXC_FPE) {
        cs_print_fpe_cause(TEXT_X(0), TEXT_Y(line), tc->fpcsr);
    }

    u32 endLine = (CRASH_SCREEN_NUM_CHARS_Y - 2);
    cs_draw_divider(DIVIDER_Y(endLine));
    cs_print(TEXT_X(0), TEXT_Y(endLine),
        STR_COLOR_PREFIX"see other pages for more info\npress [%s] for page-specific controls",
        COLOR_RGBA32_CRASH_HEADER, gCSControlDescriptions[CONT_DESC_SHOW_CONTROLS].control
    );
}

void page_home_input(void) {
    if (gCSCompositeController->buttonPressed & B_BUTTON) {
        // Cycle floats print mode.
        cs_inc_setting(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_FLOATS_FMT, 1);
    }
}

void page_home_print(void) {
#ifdef UNF
    debug_printf("\n");

    __OSThreadContext* tc = &gCrashedThread->context;

    // THREAD:
    enum ThreadID threadID = gCrashedThread->id;
    debug_printf("- THREAD:\t%d", threadID);
    if (threadID < NUM_THREADS) {
        const char* threadName = get_thread_name_from_id(threadID);

        if (threadName != NULL) {
            // "(thread name)"
            debug_printf(" (%s)", threadName);
        }
    }
    debug_printf("\n");

 #ifdef INCLUDE_DEBUG_MAP
    // FUNCTION:
    const MapSymbol* symbol = get_map_symbol(tc->pc, SYMBOL_SEARCH_BACKWARD);
    if (symbol != NULL) {
        debug_printf("- FUNC: %s\n", get_map_symbol_name(symbol));
    }
 #endif // INCLUDE_DEBUG_MAP

    // CAUSE:
    const char* desc = get_cause_desc(tc);
    if (desc != NULL) {
        debug_printf("- CAUSE: (%s)\n", desc);
    }

    // FPCSR:
    u32 fpcsr = tc->fpcsr;
    debug_printf("- FPCSR: "STR_HEX_WORD, fpcsr);
    const char* fpcsrDesc = get_fpcsr_desc(fpcsr, FALSE);
    if (fpcsrDesc != NULL) {
        debug_printf(" (%s)", fpcsrDesc);
    }
    debug_printf("\n");

    if (tc->cause == EXC_SYSCALL) {
        debug_printf("- ASSERT:\n");
        if (__n64Assert_Filename  != NULL) {
            debug_printf("-- FILE: %s in LINE: %d:\n", __n64Assert_Filename,__n64Assert_LineNum);
        }
 #ifdef INCLUDE_DEBUG_MAP
        if (__assert_address) {
            const MapSymbol* symbol = get_map_symbol(__assert_address, SYMBOL_SEARCH_BACKWARD);
            if (symbol != NULL) {
                const char* name = get_map_symbol_name(symbol);
                if (name != NULL) {
                    debug_printf("-- FUNC: %s\n", name);
                }
            }

        }
 #endif // INCLUDE_DEBUG_MAP
        if (__n64Assert_Condition != NULL) {
            debug_printf("-- CONDITION: %s\n", __n64Assert_Condition);
        }
        if (__n64Assert_Message   != NULL) {
            debug_printf("-- MESSAGE: %s\n", __n64Assert_Message);
        }
    }
    //! TODO: Disasm and registers.
#endif // UNF
}


struct CSPage gCSPage_home ={
    .name         = "CRASH SCREEN HOME",
    .initFunc     = page_home_init,
    .drawFunc     = page_home_draw,
    .inputFunc    = page_home_input,
    .printFunc    = page_home_print,
    .contList     = cs_cont_list_home,
    .settingsList = cs_settings_group_page_home,
    .flags = {
        .initialized = FALSE,
        .crashed     = FALSE,
    },
};
