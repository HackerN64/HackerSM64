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
    u32 x = TEXT_X(1);
    size_t maxNumChars = (CRASH_SCREEN_NUM_CHARS_X - 2);
    u32 charX = 0;

    Address assertAddr = __assert_address;
    if (assertAddr == 0x00000000) {
        __OSThreadContext* tc = &gInspectThread->context;
        assertAddr = GET_EPC(tc); // Will almost always be __n64Assert in this case.
    }
#ifdef INCLUDE_DEBUG_MAP
    cs_print_addr_location_info(x, TEXT_Y(line++), maxNumChars, assertAddr, TRUE);

    const MapSymbol* symbol = get_map_symbol(assertAddr, SYMBOL_SEARCH_BACKWARD);
    if (
        (symbol != NULL) && (
            (symbol->addr == (Address)handle_dp_complete) ||
            (symbol->addr == (Address)alert_rcp_hung_up)
        )
    )
#else // !INCLUDE_DEBUG_MAP
    if (assertAddr == ADDR_INSN_RCP_HANG) //! TODO: handle_dp_complete null SPTask.
#endif // !INCLUDE_DEBUG_MAP
    { //! TODO: Better way of checking for RCP crashes.
        CS_SET_DEFAULT_PRINT_COLOR_START(COLOR_RGBA32_CRASH_HEADER);
        // RCP crash:
        cs_print(x, TEXT_Y(line++), "DPC:\t"STR_COLOR_PREFIX STR_HEX_WORD" in ["STR_HEX_WORD"-"STR_HEX_WORD"]",
            COLOR_RGBA32_WHITE, IO_READ(DPC_CURRENT_REG), IO_READ(DPC_START_REG), IO_READ(DPC_END_REG)
        );
        cs_print(x, TEXT_Y(line++), "STATUS:\t"STR_COLOR_PREFIX"DPC:"STR_HEX_WORD" SP:"STR_HEX_WORD,
            COLOR_RGBA32_WHITE, IO_READ(DPC_STATUS_REG), IO_READ(SP_STATUS_REG)
        );
        CS_SET_DEFAULT_PRINT_COLOR_END();
    }

    if (__n64Assert_Filename != NULL) {
        size_t charX = cs_print_scroll(x, TEXT_Y(line),
            (maxNumChars - STRLEN("(line 0000)")),
            STR_COLOR_PREFIX"%s",
            COLOR_RGBA32_CRASH_FILE_NAME, __n64Assert_Filename
        );
        cs_print((x + TEXT_WIDTH(charX + 1)), TEXT_Y(line++),
            STR_COLOR_PREFIX"(line %d)",
            COLOR_RGBA32_CRASH_FILE_NAME, __n64Assert_LineNum
        );
        line++;
    }

    // "COND:[condition]"
    if (__n64Assert_Condition != NULL) {
        charX = cs_print(x, TEXT_Y(line++),
            STR_COLOR_PREFIX"CONDITION FAILED:", COLOR_RGBA32_CRASH_HEADER
        );
        charX += cs_print_scroll(x, TEXT_Y(line++), maxNumChars,
            STR_COLOR_PREFIX"%s",
            COLOR_RGBA32_CRASH_AT, __n64Assert_Condition
        );
        line++;
    }

    if (__n64Assert_Message != NULL) {
        gCSWordWrap = TRUE;

        // "MESSAGE:[message]"
        cs_print(x, TEXT_Y(line),
            STR_COLOR_PREFIX"MESSAGE:\n"STR_COLOR_PREFIX"%s",
            COLOR_RGBA32_CRASH_HEADER,
            gCSDefaultPrintColor, __n64Assert_Message
        );

        gCSWordWrap = FALSE;
    }

    osWritebackDCacheAll();

    return line;
}

void cs_draw_register_info_long(u32 charX, u32 line, RegisterId reg, size_t maxNumChars, _Bool showAddrIfPtr) {
    CS_SET_DEFAULT_PRINT_COLOR_START(COLOR_RGBA32_WHITE);
    const RegisterInfo* regInfo = get_reg_info(reg.cop, reg.idx);
    Word data = get_reg_val(reg.cop, reg.idx);

    charX += cs_print(TEXT_X(charX), TEXT_Y(line), (STR_COLOR_PREFIX"%s: "),
        COLOR_RGBA32_CRASH_VARIABLE, ((reg.cop == COP1) ? regInfo->name : regInfo->shortName)
    );
    if (reg.valInfo.out) {
        charX += cs_print(TEXT_X(charX), TEXT_Y(line), STR_COLOR_PREFIX"[output]", COLOR_RGBA32_LIGHT_GRAY);
    } else if (reg.valInfo.type == REG_VAL_TYPE_FLOAT) {
        charX += cs_print_f32(TEXT_X(charX), TEXT_Y(line), (IEEE754_f32){ .asU32 = data, }, cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_FLOATS_FMT), TRUE);
    } else {
        if (showAddrIfPtr) {
            charX += cs_print(TEXT_X(charX), TEXT_Y(line), (STR_HEX_WORD" "), data);
        }

        if (reg.valInfo.type == REG_VAL_TYPE_ADDR) {
            if (data == 0x00000000) {
                cs_print(TEXT_X(charX), TEXT_Y(line), STR_COLOR_PREFIX"NULL", COLOR_RGBA32_GRAY);
            }
        }
#ifdef INCLUDE_DEBUG_MAP
        const MapSymbol* symbol = get_map_symbol(data, SYMBOL_SEARCH_BACKWARD);
        if (symbol != NULL) {
            charX += cs_print_symbol_name(TEXT_X(charX), TEXT_Y(line), ((maxNumChars - STRLEN(" +0000")) - charX), symbol, FALSE);
            cs_print(TEXT_X(charX + 1), TEXT_Y(line),
                (STR_COLOR_PREFIX"+"STR_HEX_HALFWORD),
                COLOR_RGBA32_CRASH_OFFSET, (data - symbol->addr)
            );
        }
#endif // INCLUDE_DEBUG_MAP
    }
    CS_SET_DEFAULT_PRINT_COLOR_END();
}

enum CrashTypes {
    CRASH_TYPE_DEFAULT,
    CRASH_TYPE_ASSERT,
    CRASH_TYPE_IPC,
    CRASH_TYPE_II,
};

void draw_centered_title_text(u32 line, const char* text) {
    const s32 centerX = (CRASH_SCREEN_NUM_CHARS_X / 2);
    size_t len = strlen(text);
    cs_print(TEXT_X(centerX - (len / 2)), TEXT_Y(line), (STR_COLOR_PREFIX"%s"), COLOR_RGBA32_RED, text);
}


// CRASH DESCRIPTION
u32 draw_crash_cause_section(u32 line, RGBA32 descColor) {
    u32 x = 1;
    __OSThreadContext* tc = &gInspectThread->context;
    u32 cause = (tc->cause & CAUSE_EXCMASK);

    // First part of crash description:
    const char* desc = get_cause_desc(tc, TRUE);
    if (desc != NULL) {
        size_t charX = 0;
        charX += cs_print(
            TEXT_X(x), TEXT_Y(line), (STR_COLOR_PREFIX"%s"),
            descColor, desc
        );
        // Second part of crash description:
        switch (cause) {
            case EXC_CPU:
                // "COP:"
                cs_print(TEXT_X(x + charX), TEXT_Y(line),
                    STR_COLOR_PREFIX" (cop%d)",
                    descColor,
                    ((Reg_CP0_Cause){ .raw = cause, }).CE
                );
                break;
            case EXC_FPE:
                line++;
                const char* fpcsrDesc = get_fpcsr_desc(tc->fpcsr, TRUE);
                if (fpcsrDesc != NULL) {
                    cs_print(
                        TEXT_X(x), TEXT_Y(line), STR_COLOR_PREFIX"(%s)",
                        descColor, fpcsrDesc
                    );
                }
                break;
        }
    }

    return line;
}

void page_summary_draw(void) {
    __OSThreadContext* tc = &gInspectThread->context;
    u32 cause = (tc->cause & CAUSE_EXCMASK);

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

    u32 line = 2;
    draw_centered_title_text(line, ((crashType == CRASH_TYPE_ASSERT) ? "ASSERT:" : "CRASH:"));
    line += 2;

    size_t x = 1;
    size_t maxNumChars = (CRASH_SCREEN_NUM_CHARS_X - 2);

    // -- THREAD --
    CS_SET_DEFAULT_PRINT_COLOR_START(COLOR_RGBA32_CRASH_THREAD_NAME);
    size_t charX = cs_print(TEXT_X(x), TEXT_Y(line), "in ");
    cs_print_thread_info_line_1((TEXT_X(x) + TEXT_WIDTH(charX)), TEXT_Y(line++), maxNumChars, gInspectThread, FALSE);
    CS_SET_DEFAULT_PRINT_COLOR_END();

    if (crashType == CRASH_TYPE_ASSERT) {
        // -- ASSERT --
        cs_draw_assert(line);
    } else {
        line++;
        // -- PC --
        RegisterId regPC = {
            .cop = COP0,
            .idx = REG_CP0_EPC,
            .valInfo = {
                .type = REG_VAL_TYPE_ADDR,
                .dbl  = FALSE,
                .out  = FALSE,
            },
        };
        cs_draw_register_info_long(x, line++, regPC, maxNumChars, FALSE);
        if (crashType == CRASH_TYPE_IPC) {
            cs_print(TEXT_X(x), TEXT_Y(line++),
                STR_COLOR_PREFIX"Program counter at invalid memory location.",
                COLOR_RGBA32_CRASH_DESCRIPTION_MAIN
            );
        } else {
            cs_draw_divider_translucent(DIVIDER_Y(line));
            print_insn(TEXT_X(x), TEXT_Y(line++), insnAsStr, destFname);

            if (crashType == CRASH_TYPE_II) {
                print_data_as_binary(TEXT_X(x), TEXT_Y(line++), &data, sizeof(data), COLOR_RGBA32_WHITE);
                cs_draw_divider_translucent(DIVIDER_Y(line));
            } else {
                cs_draw_divider_translucent(DIVIDER_Y(line));
                for (int i = 0; i < gSavedRegBufSize; i++) {
                    cs_draw_register_info_long(x, line++, gSavedRegBuf[i], maxNumChars, TRUE);
                }
            }

            line++;
            draw_crash_cause_section(line, COLOR_RGBA32_CRASH_DESCRIPTION_MAIN);
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
    u16 buttonPressed = gCSCompositeController->buttonPressed;

    if (buttonPressed & B_BUTTON) {
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
