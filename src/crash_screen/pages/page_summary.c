#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/util/insn_disasm.h"
#include "crash_screen/util/map_parser.h"
#include "crash_screen/util/memory_read.h"
#include "crash_screen/util/registers.h"
#include "crash_screen/cs_controls.h"
#include "crash_screen/cs_descriptions.h"
#include "crash_screen/cs_draw.h"
#include "crash_screen/cs_main.h"
#include "crash_screen/cs_pages.h"
#include "crash_screen/cs_print.h"
#include "crash_screen/cs_settings.h"
#include "crash_screen/popups/popup_threads.h"

#include "page_summary.h"

#include "page_disasm.h"

#include "game/asm.h"
#ifdef UNF
#include "usb/usb.h"
#include "usb/debug.h"
#endif // UNF


struct CSSetting cs_settings_group_page_summary[] = {
    [CS_OPT_HEADER_PAGE_SUMMARY     ] = { .type = CS_OPT_TYPE_HEADER,  .name = "SUMMARY",                           .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_END_SUMMARY             ] = { .type = CS_OPT_TYPE_END, },
};


const enum ControlTypes cs_cont_list_summary[] = {
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


void page_summary_init(void) {

}

extern void handle_dp_complete(void);
extern void alert_rcp_hung_up(void);

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
    if (__n64Assert_Filename != NULL) {
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
    }

    Address assertAddr = __assert_address;
    if (assertAddr) {
#ifdef INCLUDE_DEBUG_MAP
        // "FUNC:[function name]"
        size_t charX = cs_print(
            TEXT_X(0), TEXT_Y(line),
            STR_COLOR_PREFIX"FUNC:",
            COLOR_RGBA32_CRASH_HEADER
        );
        const MapSymbol* symbol = get_map_symbol(assertAddr, SYMBOL_SEARCH_BACKWARD);
        if (symbol != NULL) {
            charX += cs_print_addr_location_info(TEXT_X(charX), TEXT_Y(line), ((CRASH_SCREEN_NUM_CHARS_X - STRLEN(" +0000")) - charX), assertAddr, TRUE);
            cs_print(TEXT_X(charX + 1), TEXT_Y(line), (STR_COLOR_PREFIX"+"STR_HEX_HALFWORD), COLOR_RGBA32_CRASH_OFFSET, (assertAddr - symbol->addr));
            line++;
        }

        if (
            (symbol != NULL) && (
                (symbol->addr == (Address)handle_dp_complete) ||
                (symbol->addr == (Address)alert_rcp_hung_up)
            )
        )
#else
        if (assertAddr == ADDR_INSN_RCP_HANG) //! TODO: handle_dp_complete null SPTask.
#endif
        { //! TODO: Better way of checking for RCP crashes.
            CS_PRINT_DEFAULT_COLOR_START(COLOR_RGBA32_CRASH_HEADER);
            // RCP crash:
            cs_print(TEXT_X(0), TEXT_Y(line++), "DPC:\t"STR_COLOR_PREFIX STR_HEX_WORD" in ["STR_HEX_WORD"-"STR_HEX_WORD"]",
                COLOR_RGBA32_WHITE, IO_READ(DPC_CURRENT_REG), IO_READ(DPC_START_REG), IO_READ(DPC_END_REG)
            );
            cs_print(TEXT_X(0), TEXT_Y(line++), "STATUS:\t"STR_COLOR_PREFIX"DPC:"STR_HEX_WORD" SP:"STR_HEX_WORD,
                COLOR_RGBA32_WHITE, IO_READ(DPC_STATUS_REG), IO_READ(SP_STATUS_REG)
            );
            CS_PRINT_DEFAULT_COLOR_END();
        }
    }

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

    if (__n64Assert_Message != NULL) {
        // "MESSAGE:[message]"
        cs_print(TEXT_X(0), TEXT_Y(line),
            STR_COLOR_PREFIX"MESSAGE:\n"STR_COLOR_PREFIX"%s",
            COLOR_RGBA32_CRASH_HEADER,
            gCSDefaultPrintColor, __n64Assert_Message
        );
        line += gCSNumLinesPrinted;
    }
    gCSWordWrap = FALSE;

    osWritebackDCacheAll();

    return line;
}

void cs_draw_register_info_long(u32 charX, u32 line, RegisterId reg) {
    const RegisterInfo* regInfo = get_reg_info(reg.cop, reg.idx);
    Word data = get_reg_val(reg.cop, reg.idx);

    charX += cs_print(TEXT_X(charX), TEXT_Y(line), (STR_COLOR_PREFIX"%s: "),
        COLOR_RGBA32_CRASH_VARIABLE, ((reg.cop == COP1) ? regInfo->name : regInfo->shortName)
    );
    if (reg.out) {
        charX += cs_print(TEXT_X(charX), TEXT_Y(line), STR_COLOR_PREFIX"[output]", COLOR_RGBA32_LIGHT_GRAY);
    } else if (reg.flt) { // Float.
        charX += cs_print_f32(TEXT_X(charX), TEXT_Y(line), (IEEE754_f32){ .asU32 = data, }, cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_FLOATS_FMT), TRUE);
    } else {
        charX += cs_print(TEXT_X(charX), TEXT_Y(line), STR_HEX_WORD" ", data);

#ifdef INCLUDE_DEBUG_MAP
        const MapSymbol* symbol = get_map_symbol(data, SYMBOL_SEARCH_BACKWARD);
        if (symbol != NULL) {
            charX += cs_print_symbol_name(TEXT_X(charX), TEXT_Y(line), ((CRASH_SCREEN_NUM_CHARS_X - STRLEN(" +0000")) - charX), symbol, FALSE);
            cs_print(TEXT_X(charX + 1), TEXT_Y(line),
                (STR_COLOR_PREFIX"+"STR_HEX_HALFWORD),
                COLOR_RGBA32_CRASH_OFFSET, (data - symbol->addr)
            );
        }
#endif // INCLUDE_DEBUG_MAP
    }
}

enum CrashTypes {
    CRASH_TYPE_DEFAULT,
    CRASH_TYPE_ASSERT,
    CRASH_TYPE_IPC,
    CRASH_TYPE_II,
};

#define CRASH_SUMMARY_TITLE_TEXT "CRASH"

void page_summary_draw(void) {
    __OSThreadContext* tc = &gInspectThread->context;
    u32 cause = (tc->cause & CAUSE_EXCMASK);
    u32 line = 2;

    const s32 centerX = (CRASH_SCREEN_NUM_CHARS_X / 2);
    size_t len = STRLEN(CRASH_SUMMARY_TITLE_TEXT);
    cs_print(TEXT_X(centerX - (len / 2)), TEXT_Y(line++), (STR_COLOR_PREFIX CRASH_SUMMARY_TITLE_TEXT), COLOR_RGBA32_RED);

    Address epc = GET_EPC(tc);
    Word data = 0x00000000;
    const char* destFname = NULL;
    const char* insnAsStr = NULL;

    enum CrashTypes crashType = CRASH_TYPE_DEFAULT;

    if ((cause == EXC_SYSCALL) && (tc->pc == ADDR_INSN_ASSERT)) { // Crash is an assert.
        crashType = CRASH_TYPE_ASSERT;
    } else {
        if (!try_read_word_aligned(&data, epc)) { // PC is at an invalid memory location.
            crashType = CRASH_TYPE_IPC;
        } else {
            // Calculate this early so the register buffer can be checked.
            insnAsStr = cs_insn_to_string(epc, (InsnData)data, &destFname, TRUE);
            if ((cause == EXC_II) || !is_in_code_segment(epc)) { // PC is an invalid instruction.
                crashType = CRASH_TYPE_II;
            }
        }
    }

    // -- THREAD --
    cs_print(TEXT_X(0), TEXT_Y(line++), STR_COLOR_PREFIX"in thread:", COLOR_RGBA32_CRASH_PAGE_NAME);
    cs_draw_divider_translucent(DIVIDER_Y(line));
    // cs_print_thread_info(TEXT_X(1), TEXT_Y(line++), CRASH_SCREEN_NUM_CHARS_X, gInspectThread);
    cs_print_thread_info_line_1(TEXT_X(1), TEXT_Y(line++), CRASH_SCREEN_NUM_CHARS_X, gInspectThread, FALSE);
    // line += 2;
    line++;


    // -- CRASH DESCRIPTION --

    cs_print(TEXT_X(0), TEXT_Y(line++), STR_COLOR_PREFIX"caused by:", COLOR_RGBA32_CRASH_PAGE_NAME);
    cs_draw_divider_translucent(DIVIDER_Y(line));

    // First part of crash description:
    size_t charX = 0;
    const char* desc = get_cause_desc(tc, TRUE);
    if (desc != NULL) {
        charX += cs_print(
            TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"%s",
            COLOR_RGBA32_CRASH_DESCRIPTION, desc
        );
    }
    // Second part of crash description:
    switch (cause) {
        case EXC_CPU:
            // "COP:"
            cs_print(TEXT_X(1 + charX), TEXT_Y(line++),
                STR_COLOR_PREFIX" (cop%d)",
                COLOR_RGBA32_CRASH_DESCRIPTION,
                ((Reg_CP0_Cause){ .raw = cause, }).CE
            );
            break;
        case EXC_FPE:;
            const char* fpcsrDesc = get_fpcsr_desc(tc->fpcsr, TRUE);
            if (fpcsrDesc != NULL) {
                cs_print(
                    TEXT_X(2), TEXT_Y(line++), STR_COLOR_PREFIX"(%s)",
                    COLOR_RGBA32_CRASH_DESCRIPTION, fpcsrDesc
                );
            }
            break;
    }
    line++;

    if (crashType == CRASH_TYPE_ASSERT) {
        // -- ASSERT --
        cs_draw_divider(DIVIDER_Y(line));
        line = cs_draw_assert(line);
    } else {
        // -- PC --
        cs_print(TEXT_X(0), TEXT_Y(line++),
            STR_COLOR_PREFIX"crash location:",
            COLOR_RGBA32_CRASH_PAGE_NAME
        );
        cs_draw_divider_translucent(DIVIDER_Y(line));
        RegisterId regPC = {
            .cop = COP0,
            .idx = REG_CP0_EPC,
            .flt = FALSE,
            .out = FALSE,
        };
        cs_draw_register_info_long(1, line++, regPC);
        line++;
        if (crashType == CRASH_TYPE_IPC) {
            cs_print(TEXT_X(1), TEXT_Y(line++),
                STR_COLOR_PREFIX"Program counter at invalid memory location.",
                COLOR_RGBA32_CRASH_DESCRIPTION
            );
        } else {
            cs_print(TEXT_X(0), TEXT_Y(line++),
                STR_COLOR_PREFIX"instruction at crash:",
                COLOR_RGBA32_CRASH_PAGE_NAME
            );
            cs_draw_divider_translucent(DIVIDER_Y(line));
            print_insn(TEXT_X(1), TEXT_Y(line++), insnAsStr, destFname);
            line++;

            if (crashType == CRASH_TYPE_II) {
                cs_print(TEXT_X(0), TEXT_Y(line++), STR_COLOR_PREFIX"as binary:", COLOR_RGBA32_CRASH_PAGE_NAME);
                cs_draw_divider_translucent(DIVIDER_Y(line));
                print_data_as_binary(TEXT_X(1), TEXT_Y(line++), &data, sizeof(data), COLOR_RGBA32_WHITE);
            } else {
                // cs_print(TEXT_X(0), TEXT_Y(line++), STR_COLOR_PREFIX"instruction register values:", COLOR_RGBA32_CRASH_PAGE_NAME);
                // cs_draw_divider_translucent(DIVIDER_Y(line));
                for (int i = 0; i < gSavedRegBufSize; i++) {
                    cs_draw_register_info_long(1, line++, gSavedRegBuf[i]);
                }
            }
        }
    }

    // if (gLastCSSelectedAddress) {
    //     cs_print(TEXT_X(0), TEXT_Y(line++), "LAST SELECTED: %08X", gLastCSSelectedAddress);
    // }

    u32 endLine = (CRASH_SCREEN_NUM_CHARS_Y - 2);
    cs_draw_divider(DIVIDER_Y(endLine));
    cs_print(TEXT_X(0), TEXT_Y(endLine),
        STR_COLOR_PREFIX"see other pages for more info\npress [%s] for page-specific controls",
        COLOR_RGBA32_CRASH_HEADER, gCSControlDescriptions[CONT_DESC_SHOW_CONTROLS].control
    );
}

void page_summary_input(void) {
    if (gCSCompositeController->buttonPressed & B_BUTTON) {
        // Cycle floats print mode.
        cs_inc_setting(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_FLOATS_FMT, 1);
    }
}

void page_summary_print(void) {
#ifdef UNF
    osSyncPrintf("\n");

    OSThread* thread = gInspectThread;
    __OSThreadContext* tc = &thread->context;

    // THREAD:
    osSyncPrintf("- THREAD:\t%d", osGetThreadId(thread));
    const char* threadName = get_thread_name(thread);

    if (threadName != NULL) {
        // "(thread name)"
        osSyncPrintf(" (%s)", threadName);
    }
    osSyncPrintf("\n");

 #ifdef INCLUDE_DEBUG_MAP
    // FUNCTION:
    const MapSymbol* symbol = get_map_symbol(GET_EPC(tc), SYMBOL_SEARCH_BACKWARD);
    if (symbol != NULL) {
        osSyncPrintf("- FUNC: %s\n", get_map_symbol_name(symbol));
    }
 #endif // INCLUDE_DEBUG_MAP

    // CAUSE:
    const char* desc = get_cause_desc(tc, TRUE);
    if (desc != NULL) {
        osSyncPrintf("- CAUSE: %s\n", desc);
    }

    // FPE:
    const char* fpcsrDesc = get_fpcsr_desc(tc->fpcsr, FALSE);
    if (fpcsrDesc != NULL) {
        osSyncPrintf("- FPE: %s\n", fpcsrDesc);
    }

    if (tc->cause == EXC_SYSCALL) {
        osSyncPrintf("- ASSERT:\n");
        if (__n64Assert_Filename  != NULL) {
            osSyncPrintf("-- FILE: %s in LINE: %d:\n", __n64Assert_Filename,__n64Assert_LineNum);
        }
 #ifdef INCLUDE_DEBUG_MAP
        if (__assert_address) {
            const MapSymbol* symbol = get_map_symbol(__assert_address, SYMBOL_SEARCH_BACKWARD);
            if (symbol != NULL) {
                const char* name = get_map_symbol_name(symbol);
                if (name != NULL) {
                    osSyncPrintf("-- FUNC: %s\n", name);
                }
            }

        }
 #endif // INCLUDE_DEBUG_MAP
        if (__n64Assert_Condition != NULL) {
            osSyncPrintf("-- CONDITION: %s\n", __n64Assert_Condition);
        }
        if (__n64Assert_Message   != NULL) {
            osSyncPrintf("-- MESSAGE: %s\n", __n64Assert_Message);
        }
    } else {
        //! TODO: Disasm and registers.
    }
#endif // UNF
}


struct CSPage gCSPage_summary ={
    .name         = "SUMMARY",
    .initFunc     = page_summary_init,
    .drawFunc     = page_summary_draw,
    .inputFunc    = page_summary_input,
    .printFunc    = page_summary_print,
    .contList     = cs_cont_list_summary,
    .settingsList = cs_settings_group_page_summary,
    .flags = {
        .initialized = FALSE,
        .crashed     = FALSE,
    },
};
