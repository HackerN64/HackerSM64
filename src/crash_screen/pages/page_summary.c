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
#include "game/area.h"
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

// Draw the assert info.
//! TODO: Scrollable long asserts.
CSTextCoord_u32 cs_draw_assert(CSTextCoord_u32 line) {
    ScreenCoord_u32 x = TEXT_X(1);
    CSTextCoord_u32 maxNumChars = (CRASH_SCREEN_NUM_CHARS_X - 2);
    CSTextCoord_u32 charX = 0;

    if (__assert_function != NULL) {
        cs_print(x, TEXT_Y(line++), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_FUNCTION_NAME, __assert_function);
    }

    if (__n64Assert_Filename != NULL) {
        charX = cs_print_scroll(x, TEXT_Y(line),
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
        // "MESSAGE:"
        cs_print(x, TEXT_Y(line++), STR_COLOR_PREFIX"MESSAGE:", COLOR_RGBA32_CRASH_HEADER);

        const char* message = __n64Assert_Message;

        enum AssertType {
            ASSERT_TYPE_DEFAULT,
            ASSERT_TYPE_LEVEL,
            ASSERT_TYPE_RCP,
            //! TODO: ASSERT_TYPE_AUDIO
            //! TODO: ASSERT_TYPE_CRASH_SCREEN
        } type = ASSERT_TYPE_DEFAULT;
        if (message[0] == CHAR_ASSERT_PREFIX) {
            switch (message[1]) {
                case CHAR_ASSERT_PREFIX_LEVEL: type = ASSERT_TYPE_LEVEL; break; // ASSERT_PREFIX_LEVEL
                case CHAR_ASSERT_PREFIX_RCP:   type = ASSERT_TYPE_RCP;   break; // ASSERT_PREFIX_RCP
            }
        }

        // Skip the first two characters of the message if they were used to set the assert type.
        if (type != ASSERT_TYPE_DEFAULT) {
            message += 2;
        }

        // "[message]"
        gCSWordWrap = TRUE;
        cs_print(x, TEXT_Y(line), "%s", message);
        line += gCSNumLinesPrinted;
        gCSWordWrap = FALSE;

        switch (type) {
            case ASSERT_TYPE_LEVEL:
                // line = (CRASH_SCREEN_NUM_CHARS_Y - 3 - 1);
                line++;
                CSTextCoord_u32 levelStrSize = cs_print(x, TEXT_Y(line), "Level %d %s",
                    gCurrLevelNum, get_level_name(gCurrLevelNum)
                );
                if (gCurrentArea != NULL) {
                    cs_print((x + TEXT_WIDTH(levelStrSize)), TEXT_Y(line), " (area %d)", gCurrentArea->index);
                }
                break;
            case ASSERT_TYPE_RCP:
                // line = (CRASH_SCREEN_NUM_CHARS_Y - 3 - 3);
                line++;
                //! TODO: Other RCP registers.
                cs_print(x, TEXT_Y(line), "DPC:\t0x%08X in 0x%08X-0x%08X\nSTAT:\tdp:0x%04X\tsp:0x%04X\nSP DMA:\tfull:%X busy:%X",
                    IO_READ(DPC_CURRENT_REG), IO_READ(DPC_START_REG), IO_READ(DPC_END_REG),
                    IO_READ(DPC_STATUS_REG ), IO_READ(SP_STATUS_REG),
                    IO_READ(SP_DMA_FULL_REG), IO_READ(SP_DMA_BUSY_REG)
                );
                break;
            default:
                break;
        }
    }

    osWritebackDCacheAll();

    return line;
}

void cs_draw_register_info_long(CSTextCoord_u32 charX, CSTextCoord_u32 line, RegisterId reg, CSTextCoord_u32 maxNumChars, _Bool showAddrIfPtr) {
    CS_SET_DEFAULT_PRINT_COLOR_START(COLOR_RGBA32_WHITE);
    const RegisterInfo* regInfo = get_reg_info(reg.src, reg.idx);
    Word data = get_reg_val(reg.src, reg.idx, TRUE);
    const char* name = ((reg.src == REGS_CPU) ? regInfo->shortName : regInfo->name);

    // FP is part of FPCSR.
    if (reg.valInfo.type == REG_VAL_TYPE_CONDBIT) {
        name = "FP";
        data = ((Reg_FPR_31)data).C;
    }

    charX += cs_print(TEXT_X(charX), TEXT_Y(line), (STR_COLOR_PREFIX"%s: "),
        COLOR_RGBA32_CRASH_VARIABLE, name
    );
    if (reg.valInfo.out) {
        charX += cs_print(TEXT_X(charX), TEXT_Y(line), STR_COLOR_PREFIX"[output]", COLOR_RGBA32_LIGHT_GRAY);
    } else if (reg.valInfo.type == REG_VAL_TYPE_FLOAT) {
        charX += cs_print_f32(TEXT_X(charX), TEXT_Y(line), (IEEE754_f32){ .asU32 = data, }, cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_FLOATS_FMT), TRUE);
    } else {
        if (showAddrIfPtr) {
            charX += cs_print(TEXT_X(charX), TEXT_Y(line), (STR_HEX_WORD" "), data);
        }

        charX += cs_print_addr_location_info(TEXT_X(charX), TEXT_Y(line), (maxNumChars - charX), data, (reg.valInfo.type == REG_VAL_TYPE_ADDR));
    }
    CS_SET_DEFAULT_PRINT_COLOR_END();
}

void draw_centered_title_text(CSTextCoord_u32 line, const char* text) {
    const CSTextCoord_s32 centerX = (CRASH_SCREEN_NUM_CHARS_X / 2);
    CSTextCoord_u32 len = strlen(text);
    cs_print(TEXT_X(centerX - (len / 2)), TEXT_Y(line), (STR_COLOR_PREFIX"%s"), COLOR_RGBA32_RED, text);
}


// Crash description:
CSTextCoord_u32 draw_crash_cause_section(CSTextCoord_u32 line) {
    CSTextCoord_u32 x = 1;
    __OSThreadContext* tc = &gInspectThread->context;

    // First part of crash description:
    const char* desc = get_cause_desc(tc, TRUE);
    if (desc != NULL) {
        CS_SET_DEFAULT_PRINT_COLOR_START(COLOR_RGBA32_CRASH_DESCRIPTION_MAIN);
        gCSWordWrap = TRUE;

        CSTextCoord_u32 charX = 0;
        charX += cs_print(TEXT_X(x), TEXT_Y(line), desc, tc->badvaddr);
        line += gCSNumLinesPrinted;

        u32 cause = (tc->cause & CAUSE_EXCMASK);
        // Second part of crash description:
        switch (cause) {
            case EXC_MOD:
            case EXC_RMISS:
            case EXC_WMISS:
            case EXC_RADE:
            case EXC_WADE:
                charX = cs_print(TEXT_X(x), TEXT_Y(line),
                    ("When %s: "STR_COLOR_PREFIX STR_HEX_WORD),
                    (((cause == EXC_RMISS) || (cause == EXC_RADE)) ? "reading from" : "writing to"),
                    COLOR_RGBA32_WHITE, tc->badvaddr
                ) + 2;
                cs_print_addr_location_info(TEXT_X(charX), TEXT_Y(line), ((CRASH_SCREEN_NUM_CHARS_X - 2) - charX), tc->badvaddr, TRUE);
                break;
            case EXC_CPU:
                line--;
                cs_print(TEXT_X(x + charX), TEXT_Y(line), " (cop%d)", ((Reg_CP0_Cause){ .raw = cause, }).CE);
                break;
            case EXC_FPE:
                const char* fpcsrDesc = get_fpcsr_desc(tc->fpcsr, TRUE);
                if (fpcsrDesc != NULL) {
                    cs_print(TEXT_X(x), TEXT_Y(line), "(%s)", fpcsrDesc);
                }
                break;
            case EXC_WATCH:
                Address watchedAddr = PHYSICAL_TO_VIRTUAL(gWatchLo & ~(WATCHLO_WTRAP | WATCHLO_RTRAP));
                charX = cs_print(TEXT_X(x), TEXT_Y(line),
                    ("Watched: "STR_COLOR_PREFIX STR_HEX_WORD),
                    COLOR_RGBA32_WHITE, watchedAddr
                ) + 2;
                cs_print_addr_location_info(TEXT_X(charX), TEXT_Y(line), ((CRASH_SCREEN_NUM_CHARS_X - 2) - charX), watchedAddr, TRUE);
                break;
        }
        gCSWordWrap = FALSE;
        CS_SET_DEFAULT_PRINT_COLOR_END();
    }


    return (line + 1);
}

void page_summary_draw(void) {
    __OSThreadContext* tc = &gInspectThread->context;
    u32 cause = (tc->cause & CAUSE_EXCMASK);

    Address epc = GET_EPC(tc);
    Word data = 0x00000000;
    const char* destFname = NULL;
    const char* insnAsStr = NULL;

    enum CrashTypes {
        CRASH_TYPE_DEFAULT,
        CRASH_TYPE_ASSERT,
        CRASH_TYPE_IPC,
        CRASH_TYPE_II,
    } crashType = CRASH_TYPE_DEFAULT;

    if ((cause == EXC_SYSCALL) && (tc->pc == ADDR_INSN_ASSERT)) { // Crash is an assert.
        crashType = CRASH_TYPE_ASSERT;
    } else {
        // Read this data early to fill gSavedRegBuf.
        if (!try_read_word_aligned(&data, epc)) { // PC is at an invalid memory location.
            crashType = CRASH_TYPE_IPC;
        } else {
            // Calculate this early so the register buffer can be checked.
            insnAsStr = cs_insn_to_string(epc, (InsnData)data, &destFname, TRUE);
            if ((cause == EXC_II) || !addr_is_in_text_segment(epc)) { // PC is an invalid instruction.
                crashType = CRASH_TYPE_II;
            }
        }
    }

    CSTextCoord_u32 line = 2;
    draw_centered_title_text(line, ((crashType == CRASH_TYPE_ASSERT) ? "ASSERT:" : "CRASH:"));
    line += 2;

    CSTextCoord_u32 x = 1;
    CSTextCoord_u32 maxNumChars = (CRASH_SCREEN_NUM_CHARS_X - 2);

    // -- THREAD --
    CS_SET_DEFAULT_PRINT_COLOR_START(COLOR_RGBA32_CRASH_THREAD_NAME);
    CSTextCoord_u32 charX = cs_print(TEXT_X(x), TEXT_Y(line), "in ");
    cs_print_thread_info_line_1((TEXT_X(x) + TEXT_WIDTH(charX)), TEXT_Y(line++), maxNumChars, gInspectThread, FALSE);
    CS_SET_DEFAULT_PRINT_COLOR_END();

    if (crashType == CRASH_TYPE_ASSERT) {
        // -- ASSERT --
        cs_draw_assert(line);
    } else {
        cs_print_addr_location_info(TEXT_X(x), TEXT_Y(line++), maxNumChars, epc, TRUE);
        if (crashType == CRASH_TYPE_IPC) {
            cs_print(TEXT_X(x), TEXT_Y(line++),
                STR_COLOR_PREFIX"Program counter at invalid memory location:\n"STR_COLOR_PREFIX"PC: %08X",
                COLOR_RGBA32_CRASH_DESCRIPTION_MAIN, COLOR_RGBA32_WHITE, epc
            );
        } else {
            line = draw_crash_cause_section(line);
            line++;
            cs_print(TEXT_X(x), TEXT_Y(line++), STR_COLOR_PREFIX"DISASM:", COLOR_RGBA32_CRASH_HEADER);
            // cs_draw_divider_translucent(DIVIDER_Y(line));
            print_insn(TEXT_X(x), TEXT_Y(line++), insnAsStr, destFname);

            if (crashType == CRASH_TYPE_II) {
                print_data_as_binary(TEXT_X(x), TEXT_Y(line++), &data, sizeof(data), COLOR_RGBA32_WHITE);
                // cs_draw_divider_translucent(DIVIDER_Y(line));
            } else {
                // cs_draw_divider_translucent(DIVIDER_Y(line));
                for (int i = 0; i < gSavedRegBufSize; i++) {
                    cs_draw_register_info_long(x, line++, gSavedRegBuf[i], maxNumChars, TRUE);
                }

                line++;
                cs_print(TEXT_X(x), TEXT_Y(line++), STR_COLOR_PREFIX"PSEUDO:", COLOR_RGBA32_CRASH_HEADER);
                cs_print_scroll(TEXT_X(x), TEXT_Y(line++), (CRASH_SCREEN_NUM_CHARS_X - 2), "%s", cs_insn_to_pseudo_c((InsnData)data));
            }
        }
    }

    cs_draw_footer_instructions(TRUE);
}

void page_summary_input(void) {
    // u16 buttonPressed = gCSCompositeController->buttonPressed;

    // if (buttonPressed & B_BUTTON) {
    //     // Cycle floats print mode.
    //     cs_inc_setting(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_FLOATS_FMT, 1);
    // }
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

    if (IS_DEBUG_MAP_ENABLED()) {
        // FUNCTION:
        const MapSymbol* symbol = get_map_symbol(GET_EPC(tc), SYMBOL_SEARCH_BACKWARD);
        if (symbol != NULL) {
            osSyncPrintf("- FUNC: %s\n", get_map_symbol_name(symbol));
        }
    }

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
            osSyncPrintf("-- FILE: %s in LINE: %d:\n", __n64Assert_Filename, __n64Assert_LineNum);
        }
        if (__assert_function != NULL) {
            osSyncPrintf("-- FUNC: %s\n", __assert_function);
        }
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
