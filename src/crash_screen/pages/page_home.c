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
#include "crash_screen/map_parser.h"

#include "page_home.h"

#include "game/asm.h"
#ifdef UNF
#include "usb/debug.h"
#endif // UNF


struct CSSetting cs_settings_group_page_home[] = {
    [CS_OPT_HEADER_PAGE_HOME    ] = { .type = CS_OPT_TYPE_HEADER,  .name = "HOME",                           .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_END_HOME            ] = { .type = CS_OPT_TYPE_END, },
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
static const char* get_cause_desc(u32 cause) {
    // Make the last two cause case indexes sequential for array access.
    switch (cause) {
        case EXC_WATCH: cause = 16; break; // 23 -> 16
        case EXC_VCED:  cause = 17; break; // 31 -> 17
        default:        cause = ((cause >> CAUSE_EXCSHIFT) & BITMASK(5)); break;
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


// Draws the red background for the assert section.
static void draw_assert_highlight(u32 line) {
    // //! Prints the assert message early, but with 0 alpha (skips framebuffer writes).
    // //   This is a hacky way to get the amount of lines the wrapped assert text will be.
    // //   This can't be done after the normal print because it would show up in front of the text.
    // cs_print(TEXT_X(0), TEXT_Y(line),
    //     STR_COLOR_PREFIX"MESSAGE:%s",
    //     COLOR_RGBA32_NONE, __n64Assert_Message
    // );
    // cs_draw_rect(CRASH_SCREEN_X1, (DIVIDER_Y(line) + 1), CRASH_SCREEN_W, TEXT_HEIGHT(5 + gCSNumLinesPrinted), RGBA32_SET_ALPHA(COLOR_RGBA32_RED, 0x3F));
    cs_draw_rect(CRASH_SCREEN_X1, (DIVIDER_Y(line) + 1), CRASH_SCREEN_W, TEXT_HEIGHT(CRASH_SCREEN_NUM_CHARS_Y - line), RGBA32_SET_ALPHA(COLOR_RGBA32_RED, 0x3F));
}

void cs_print_crashed_thread(u32 x, u32 y) {
    // "THREAD: [thread id]"
    enum ThreadID threadID = gCrashedThread->id;
    size_t charX = cs_print(TEXT_X(x), TEXT_Y(y), STR_COLOR_PREFIX"THREAD:\t%d",
        COLOR_RGBA32_CRASH_THREAD, threadID
    );
    if (threadID < NUM_THREADS) {
        const char* threadName = get_thread_name_from_id(threadID);

        if (threadName != NULL) {
            // "(thread name)"
            cs_print(TEXT_X(charX + STRLEN(" ")), TEXT_Y(y), STR_COLOR_PREFIX"(%s)",
                COLOR_RGBA32_CRASH_THREAD, threadName
            );
        }
    }
}

#ifdef INCLUDE_DEBUG_MAP
void cs_print_func(u32 x, u32 y, __OSThreadContext* tc) {
    const MapSymbol* symbol = get_map_symbol(tc->pc, SYMBOL_SEARCH_BACKWARD);
    // "FUNC: [function name]"
    size_t charX = cs_print(TEXT_X(x), TEXT_Y(y),
        STR_COLOR_PREFIX"FUNC:\t",
        COLOR_RGBA32_CRASH_AT
    );
    cs_print_symbol_name(TEXT_X(charX), TEXT_Y(y), (CRASH_SCREEN_NUM_CHARS_X - charX), symbol);
}
#endif // INCLUDE_DEBUG_MAP

void cs_print_cause(u32 x, u32 y, __OSThreadContext* tc) {
    const char* desc = get_cause_desc(tc->cause);
    if (desc != NULL) {
        // "CAUSE: ([exception cause description])"
        cs_print(TEXT_X(x), TEXT_Y(y),
            STR_COLOR_PREFIX"CAUSE:\t%s",
            COLOR_RGBA32_CRASH_DESCRIPTION, desc
        );
    }
}

u32 print_assert_section(u32 line) {
    u32 charX = 0;

    gCSWordWrap = TRUE;

    draw_assert_highlight(line);

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
// #ifdef INCLUDE_DEBUG_MAP
//     Address RAddr = 0;
//     ASM_GET_RA(RAddr);
//     const MapSymbol* symbol = get_map_symbol(RAddr, SYMBOL_SEARCH_BACKWARD);
//     if (symbol != NULL) {
//         const char* name = get_map_symbol_name(symbol);
//         if (name != NULL) {
//             // "FUNC:[function name]"
//             cs_print(TEXT_X(0), TEXT_Y(line),
//                 STR_COLOR_PREFIX"FUNC:"STR_COLOR_PREFIX"%s",
//                 COLOR_RGBA32_CRASH_HEADER,
//                 COLOR_RGBA32_CRASH_FUNCTION_NAME, name
//             );
//             line++;
//         }
//     }
// #endif // INCLUDE_DEBUG_MAP

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

void page_home_draw(void) {
    __OSThreadContext* tc = &gCrashedThread->context;
    u32 line = 1;

    // "START:controls"
    cs_print(TEXT_X(0), TEXT_Y(line),
        STR_COLOR_PREFIX"press %s for page controls",
        COLOR_RGBA32_CRASH_HEADER, gCSControlDescriptions[CONT_DESC_SHOW_CONTROLS].control
    );
    line++;

    cs_draw_divider(DIVIDER_Y(line));
    line++;

    const s32 centerX = (CRASH_SCREEN_NUM_CHARS_X / 2);
    size_t len = STRLEN("CRASH AT:");
    cs_print(TEXT_X(centerX - (len / 2)), TEXT_Y(line++), STR_COLOR_PREFIX"CRASH AT:", COLOR_RGBA32_RED);
    line++;

    cs_print_crashed_thread(0, line++);
#ifdef INCLUDE_DEBUG_MAP
    cs_print_func(0, line++, tc);
#endif // INCLUDE_DEBUG_MAP
    cs_print_cause(0, line++, tc);

    cs_print_fpcsr(TEXT_X(0), TEXT_Y(line), tc->fpcsr);
    line++;
    line++;

    if (__n64Assert_Message != NULL) {
        line++;
        cs_draw_divider(DIVIDER_Y(line));
        line = print_assert_section(line);
        // line++;
        // cs_draw_divider(DIVIDER_Y(line));
    }

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
    const char* desc = get_cause_desc(tc->cause);
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

    if (__n64Assert_Message != NULL) {
        debug_printf(
            "- ASSERT: \n-- FILE: %s in LINE: %d\n-- CONDITION: %s\n-- MESSAGE: \"%s\"\n",
            __n64Assert_Filename,
            __n64Assert_LineNum,
            __n64Assert_Condition,
            __n64Assert_Message
        );
    }
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
