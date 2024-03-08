#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/address_select.h"
#include "crash_screen/crash_controls.h"
#include "crash_screen/crash_draw.h"
#include "crash_screen/crash_main.h"
#include "crash_screen/crash_pages.h"
#include "crash_screen/crash_print.h"
#include "crash_screen/crash_settings.h"
#include "crash_screen/insn_disasm.h"
#include "crash_screen/map_parser.h"
#include "crash_screen/memory_read.h"
#include "crash_screen/registers.h"

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
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_HIDE_CRASH_SCREEN,
#ifdef UNF
    CONT_DESC_OS_PRINT,
#endif // UNF
    CONT_DESC_LIST_END,
};


static const ThreadIDName sThreadIDNames[] = {
    { .threadID = THREAD_0,                   .name = "0",              },
    { .threadID = THREAD_1_IDLE,              .name = "idle",           },
    { .threadID = THREAD_2,                   .name = "2",              },
    { .threadID = THREAD_3_MAIN,              .name = "main",           },
    { .threadID = THREAD_4_SOUND,             .name = "sound",          },
    { .threadID = THREAD_5_GAME_LOOP,         .name = "game loop",      },
    { .threadID = THREAD_6_RUMBLE,            .name = "rumble",         },
    { .threadID = THREAD_7_HVQM,              .name = "HVQM",           },
    { .threadID = THREAD_8_TIMEKEEPER,        .name = "timekeeper",     },
    { .threadID = THREAD_9_DA_COUNTER,        .name = "DA counter",     },
    { .threadID = THREAD_1000_CRASH_SCREEN_0, .name = "Crash Screen 0", },
    { .threadID = THREAD_1001_CRASH_SCREEN_1, .name = "Crash Screen 1", },
    { .threadID = THREAD_1002_CRASH_SCREEN_2, .name = "Crash Screen 2", },
};

static const char* sCauseDesc[18] = {
    /*EXC_INT       */ "Interrupt",
    /*EXC_MOD       */ "TLB modification",
    /*EXC_RMISS     */ "TLB exception on load or inst.",
    /*EXC_WMISS     */ "TLB exception on store",
    /*EXC_RADE      */ "Address error on load or inst.",
    /*EXC_WADE      */ "Address error on store",
    /*EXC_IBE       */ "Bus error on inst.",
    /*EXC_DBE       */ "Bus error on data",
    /*EXC_SYSCALL   */ "Failed Assert: See below",
    /*EXC_BREAK     */ "Breakpoint exception",
    /*EXC_II        */ "Reserved instruction",
    /*EXC_CPU       */ "Coprocessor unusable",
    /*EXC_OV        */ "Arithmetic overflow",
    /*EXC_TRAP      */ "Trap exception",
    /*EXC_VCEI      */ "Virtual coherency on inst.",
    /*EXC_FPE       */ "Floating point exception",
    /*EXC_WATCH     */ "Watchpoint exception",
    /*EXC_VCED      */ "Virtual coherency on data",
};

static const char* sFpcsrDesc[6] = {
    /*FPCSR_CE      */ "Unimplemented operation",
    /*FPCSR_CV      */ "Invalid operation",
    /*FPCSR_CZ      */ "Division by zero",
    /*FPCSR_CO      */ "Overflow",
    /*FPCSR_CU      */ "Underflow",
    /*FPCSR_CI      */ "Inexact operation",
};


// Returns a CAUSE description from 'sCauseDesc'.
static const char* get_cause_desc(__OSThreadContext* tc) {
    u32 cause = tc->cause;
    // Make the last two cause case indexes sequential for array access.
    switch (cause) {
        case EXC_WATCH: cause = 16; break; // 23 -> 16
        case EXC_VCED:  cause = 17; break; // 31 -> 17
        default:        cause = ((cause & CAUSE_EXCMASK) >> CAUSE_EXCSHIFT); break;
    }

    if (cause < ARRAY_COUNT(sCauseDesc)) {
        return sCauseDesc[cause];
    }

    return NULL;
}

// Returns a FPCSR description from 'sFpcsrDesc'.
static const char* get_fpcsr_desc(u32 fpcsr) {
    u32 bit = BIT(17);

    for (u32 i = 0; i < ARRAY_COUNT(sFpcsrDesc); i++) {
        if (fpcsr & bit) {
            return sFpcsrDesc[i];
        }

        bit >>= 1;
    }

    return NULL;
}

// Returns a thread name from 'sThreadIDNames'.
static const char* get_thread_name_from_id(enum ThreadID threadID) {
    const ThreadIDName* threadIDName = &sThreadIDNames[0];

    for (int i = 0; i < ARRAY_COUNT(sThreadIDNames); i++) {
        if (threadIDName->threadID == threadID) {
            return threadIDName->name;
        }

        threadIDName++;
    }

    return NULL;
}


void page_home_init(void) {

}

void cs_print_fpcsr(u32 x, u32 y, u32 fpcsr) {
    // "FPCSR:[XXXXXXXX]"
    size_t fpcsrSize = cs_print(x, y,
        STR_COLOR_PREFIX"FPCSR: "STR_COLOR_PREFIX STR_HEX_WORD" ",
        COLOR_RGBA32_CRASH_VARIABLE,
        COLOR_RGBA32_WHITE, fpcsr
    );
    x += TEXT_WIDTH(fpcsrSize);

    const char* fpcsrDesc = get_fpcsr_desc(fpcsr);
    if (fpcsrDesc != NULL) {
        // "([float exception description])"
        cs_print(x, y, STR_COLOR_PREFIX"(%s)", COLOR_RGBA32_CRASH_DESCRIPTION, fpcsrDesc);
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

void cs_draw_register_info_long(u32 x, u32 line, UNUSED __OSThreadContext* tc, const RegisterInfo* reg) {
    Word data = get_reg_val_from_info(reg);
    size_t charX = x + cs_print(TEXT_X(x), TEXT_Y(line), STR_COLOR_PREFIX"%s: "STR_COLOR_PREFIX STR_HEX_WORD" ",
        COLOR_RGBA32_CRASH_VARIABLE, reg->shortName,
        COLOR_RGBA32_WHITE, data
    );
#ifdef INCLUDE_DEBUG_MAP
    const MapSymbol* symbol = get_map_symbol(data, SYMBOL_SEARCH_BACKWARD);
    if (symbol != NULL) {
        size_t offsetStrSize = STRLEN("+0000");
        size_t endX = CRASH_SCREEN_NUM_CHARS_X;
        cs_print_symbol_name(TEXT_X(charX), TEXT_Y(line), (endX - (charX + offsetStrSize)), symbol);
        cs_print(TEXT_X(endX - offsetStrSize), TEXT_Y(line),
            (STR_COLOR_PREFIX"+"STR_HEX_HALFWORD),
            COLOR_RGBA32_CRASH_OFFSET, (data - symbol->addr)
        );
    }
#endif // INCLUDE_DEBUG_MAP
}

void draw_relevant_registers(u32 x, u32 line, __OSThreadContext* tc) {
    for (int i = 0; i < REG_BUFFER_SIZE; i++) {
        const RegisterInfo* reg = gSavedRegBuf[i];
        if (reg == NULL) {
            break;
        }

        cs_draw_register_info_long(x, line++, tc, reg);
    }
}

void page_home_draw(void) {
    __OSThreadContext* tc = &gCrashedThread->context;
    u32 line = 1;

    line++;

    const s32 centerX = (CRASH_SCREEN_NUM_CHARS_X / 2);
    size_t len = STRLEN("CRASH AT:");
    cs_print(TEXT_X(centerX - (len / 2)), TEXT_Y(line++), STR_COLOR_PREFIX"CRASH AT:", COLOR_RGBA32_RED);
    line++;

    cs_print_crashed_thread(TEXT_X(0), TEXT_Y(line++));
#ifdef INCLUDE_DEBUG_MAP
    cs_print_func(TEXT_X(0), TEXT_Y(line++), tc);
#endif // INCLUDE_DEBUG_MAP
    cs_print_cause(TEXT_X(0), TEXT_Y(line++), tc);

    line++;
    // cs_draw_divider(DIVIDER_Y(line));
    line++;

    switch (tc->cause) {
        case EXC_SYSCALL:
            // ASSERT:
            cs_draw_divider(DIVIDER_Y(line));
            line = cs_draw_assert(line);
            break;
        case EXC_FPE:
            cs_print_fpcsr(TEXT_X(0), TEXT_Y(line++), tc->fpcsr);
            break;
        default:;
            Address addr = tc->pc;
            Word data = 0x00000000;
            if (try_read_data(&data, addr) && is_in_code_segment(addr)) {
                cs_print(TEXT_X(0), TEXT_Y(line++),
                    STR_COLOR_PREFIX"crash location:",
                    COLOR_RGBA32_CRASH_PAGE_NAME
                );

                cs_draw_register_info_long(1, line++, tc, get_reg_info(COP0, REG_COP0_EPC));
                // line++;

                cs_print(TEXT_X(0), TEXT_Y(line++),
                    STR_COLOR_PREFIX"asm at crash:",
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
                draw_relevant_registers(1, line, tc);

            }
            break;
    }
    
    line = (CRASH_SCREEN_NUM_CHARS_Y - 2);
    cs_draw_divider(DIVIDER_Y(line));
    cs_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"see other pages for more info\npress [%s] for page-specific controls", COLOR_RGBA32_CRASH_HEADER, gCSControlDescriptions[CONT_DESC_SHOW_CONTROLS].control);
}

void page_home_input(void) {

}

void page_home_print(void) {
#ifdef UNF
    debug_printf("\n");

    __OSThreadContext* tc = &gCrashedThread->context;

    // THREAD:
    enum ThreadID threadID = gCrashedThread->id;
    debug_printf("- THREAD: %d", threadID);
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
    const char* fpcsrDesc = get_fpcsr_desc(fpcsr);
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
