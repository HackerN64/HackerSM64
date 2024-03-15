#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/util/map_parser.h"
#include "crash_screen/util/memory_read.h"
#include "crash_screen/util/registers.h"
#include "crash_screen/crash_controls.h"
#include "crash_screen/crash_draw.h"
#include "crash_screen/crash_descriptions.h"
#include "crash_screen/crash_main.h"
#include "crash_screen/crash_pages.h"
#include "crash_screen/crash_print.h"
#include "crash_screen/crash_settings.h"

#include "page_registers.h"

#ifdef UNF
#include "usb/usb.h"
#include "usb/debug.h"
#endif // UNF


struct CSSetting cs_settings_group_page_registers[] = {
    [CS_OPT_HEADER_PAGE_REGISTERS   ] = { .type = CS_OPT_TYPE_HEADER,  .name = "REGISTERS",                        .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
#ifdef INCLUDE_DEBUG_MAP
    [CS_OPT_REGISTERS_PARSE_REG     ] = { .type = CS_OPT_TYPE_SETTING, .name = "Parse register addr names",      .valNames = &gValNames_bool,          .val = FALSE,                     .defaultVal = FALSE,                     .lowerBound = FALSE,                 .upperBound = TRUE,                       },
#endif // INCLUDE_DEBUG_MAP
    [CS_OPT_END_REGISTERS           ] = { .type = CS_OPT_TYPE_END, },
};


const enum ControlTypes cs_cont_list_registers[] = {
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


#define REG_LIST_TERMINATOR (u32)-1
#define LIST_REG(_cop, _idx) {  \
    .cop = _cop,                \
    .idx = _idx,                \
    .flt = FALSE,               \
    .out = FALSE,               \
}
#define LIST_REG_END() { .raw = REG_LIST_TERMINATOR, }
static const RegisterId sRegList[32 + 1] = {
    LIST_REG(COP0, REG_COP0_EPC), LIST_REG(COP0, REG_COP0_SR), LIST_REG(COP0, REG_COP0_BADVADDR),
    LIST_REG(CPU, REG_CPU_AT), LIST_REG(CPU, REG_CPU_V0), LIST_REG(CPU, REG_CPU_V1),
    LIST_REG(CPU, REG_CPU_A0), LIST_REG(CPU, REG_CPU_A1), LIST_REG(CPU, REG_CPU_A2),
    LIST_REG(CPU, REG_CPU_A3), LIST_REG(CPU, REG_CPU_T0), LIST_REG(CPU, REG_CPU_T1),
    LIST_REG(CPU, REG_CPU_T2), LIST_REG(CPU, REG_CPU_T3), LIST_REG(CPU, REG_CPU_T4),
    LIST_REG(CPU, REG_CPU_T5), LIST_REG(CPU, REG_CPU_T6), LIST_REG(CPU, REG_CPU_T7),
    LIST_REG(CPU, REG_CPU_S0), LIST_REG(CPU, REG_CPU_S1), LIST_REG(CPU, REG_CPU_S2),
    LIST_REG(CPU, REG_CPU_S3), LIST_REG(CPU, REG_CPU_S4), LIST_REG(CPU, REG_CPU_S5),
    LIST_REG(CPU, REG_CPU_S6), LIST_REG(CPU, REG_CPU_S7), LIST_REG(CPU, REG_CPU_T8),
    LIST_REG(CPU, REG_CPU_T9), LIST_REG(CPU, REG_CPU_GP), LIST_REG(CPU, REG_CPU_SP),
    LIST_REG(CPU, REG_CPU_FP), LIST_REG(CPU, REG_CPU_RA), LIST_REG_END(),
};


void page_registers_init(void) {

}

void cs_registers_print_thread(u32 x, u32 y) {
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

// Print a fixed-point register.
void cs_registers_print_reg(u32 x, u32 y, const char* name, Word val) {
    const MapSymbol* symbol = NULL;

    // "[register name]:"
    size_t charX = cs_print(x, y,
        (" "STR_COLOR_PREFIX"%s:"),
        COLOR_RGBA32_CRASH_VARIABLE, name
    );

#ifdef INCLUDE_DEBUG_MAP
    if (cs_get_setting_val(CS_OPT_GROUP_PAGE_REGISTERS, CS_OPT_REGISTERS_PARSE_REG)) {
        symbol = get_map_symbol(val, SYMBOL_SEARCH_BACKWARD);
    }
#endif // INCLUDE_DEBUG_MAP

    if (symbol != NULL) {
        // "[symbol name]"
        cs_print_symbol_name((x + TEXT_WIDTH(charX)), y, 10, symbol);
    } else {
        // "[XXXXXXXX]"
        cs_print((x + TEXT_WIDTH(charX + STRLEN(" "))), y,
            STR_COLOR_PREFIX STR_HEX_WORD,
            COLOR_RGBA32_WHITE, val
        );
    }
}

// Print important fixed-point registers.
u32 cs_registers_print_registers(u32 line) {
    const size_t columnWidth = 15;
    const u32 columns = 3;
    const u32 rows = (ARRAY_COUNT(sRegList) / columns);
    const RegisterId* reg = sRegList;

    for (u32 y = 0; y < rows; y++) {
        for (u32 x = 0; x < columns; x++) {
            if (reg->raw == REG_LIST_TERMINATOR) {
                break;
            }

            const RegisterInfo* regInfo = get_reg_info(reg->cop, reg->idx);

            if (regInfo != NULL) {
                cs_registers_print_reg(TEXT_X(x * columnWidth), TEXT_Y(line + y), regInfo->shortName, get_reg_val(reg->cop, reg->idx));
            }

            reg++;
        }
    }

    return (line + rows);
}

void cs_print_fpcsr(u32 x, u32 y, u32 fpcsr) {
    // "FPCSR:[XXXXXXXX]"
    size_t fpcsrSize = cs_print(x, y,
        STR_COLOR_PREFIX"FPCSR: "STR_COLOR_PREFIX STR_HEX_WORD" ",
        COLOR_RGBA32_CRASH_VARIABLE,
        COLOR_RGBA32_WHITE, fpcsr
    );
    x += TEXT_WIDTH(fpcsrSize);

    const char* fpcsrDesc = get_fpcsr_desc(fpcsr, FALSE);
    if (fpcsrDesc != NULL) {
        // "([float exception description])"
        cs_print(x, y, STR_COLOR_PREFIX"(%s)", COLOR_RGBA32_CRASH_DESCRIPTION, fpcsrDesc);
    }
}

// Print a floating-point register.
void cs_registers_print_float_reg(u32 x, u32 y, u32 regNum) {
    const RegisterInfo* regInfo = get_reg_info(COP1, regNum);

    if (regInfo == NULL) {
        return;
    }

    // "[register name]:"
    size_t charX = cs_print(x, y, STR_COLOR_PREFIX"%s:", COLOR_RGBA32_CRASH_VARIABLE, regInfo->name);
    x += TEXT_WIDTH(charX);

    IEEE754_f32 val = {
        .asU32 = (u32)get_reg_val(COP1, regNum),
    };

    cs_print_f32(x, y, val, FALSE);
}

void cs_registers_print_float_registers(u32 line, __OSThreadContext* tc) {
    const size_t columnWidth = 15;
    u32 regNum = 0;
    __OSfp* osfp = &tc->fp0;

    // cs_registers_print_fpcsr(TEXT_X(0), TEXT_Y(line), tc->fpcsr);
    cs_print_fpcsr(TEXT_X(0), TEXT_Y(line++), tc->fpcsr);

    osWritebackDCacheAll();

    for (u32 y = 0; y < 6; y++) {
        for (u32 x = 0; x < 3; x++) {
            if (regNum >= COP1_NUM_REGISTERS) {
                return;
            }

            cs_registers_print_float_reg(TEXT_X(x * columnWidth), TEXT_Y(line + y), regNum);

            regNum += 2;
            osfp++;
        }
    }
}

void page_registers_draw(void) {
    __OSThreadContext* tc = &gCrashedThread->context;
    u32 line = 2;

    cs_print(TEXT_X(0), TEXT_Y(line++), STR_COLOR_PREFIX"REGISTERS IN:",
        COLOR_RGBA32_CRASH_THREAD
    );
    cs_registers_print_thread(TEXT_X(0), TEXT_Y(line++));
    
    line++;

    line = cs_registers_print_registers(line);

    osWritebackDCacheAll();

    cs_registers_print_float_registers(line, tc);
}

void page_registers_input(void) {
    if (gCSCompositeController->buttonPressed & B_BUTTON) {
        // Cycle floats print mode.
        cs_inc_setting(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_FLOATS_FMT, 1);
    }
}

void page_registers_print(void) {
#ifdef UNF
    osSyncPrintf("\n");

    __OSThreadContext* tc = &gCrashedThread->context;

    osSyncPrintf("- REGISTERS IN:\n");
    // THREAD:
    enum ThreadID threadID = gCrashedThread->id;
    osSyncPrintf("- THREAD:\t%d", threadID);
    if (threadID < NUM_THREADS) {
        const char* threadName = get_thread_name_from_id(threadID);

        if (threadName != NULL) {
            // "(thread name)"
            osSyncPrintf(" (%s)", threadName);
        }
    }
    osSyncPrintf("\n");

    // Thread registers:
    const u32 columns = 3;
    const u32 rows = (ARRAY_COUNT(sRegList) / columns);
    const RegisterId* reg = sRegList;
    for (u32 y = 0; y < rows; y++) {
        osSyncPrintf("- ");
        for (u32 x = 0; x < columns; x++) {
            if (reg->raw == REG_LIST_TERMINATOR) {
                break;
            }

            const RegisterInfo* regInfo = get_reg_info(reg->cop, reg->idx);

            if (regInfo != NULL) {
                osSyncPrintf("%s "STR_HEX_PREFIX STR_HEX_LONG" ", regInfo->shortName, get_reg_val(reg->cop, reg->idx));
            }

            reg++;
        }
        osSyncPrintf("\n");
    }

    // Float registers:
    u32 regNum = 0;
    __OSfp* osfp = &tc->fp0;
    for (u32 i = 0; i < 8; i++) {
        osSyncPrintf("- ");
        for (u32 j = 0; j < 2; j++) {
            if (regNum >= COP1_NUM_REGISTERS) {
                break;
            }

            osSyncPrintf("d%02d "STR_HEX_DECIMAL"\t", regNum, get_reg_val(COP1, regNum));

            regNum += 2;
            osfp++;
        }
        osSyncPrintf("\n");
    }
#endif // UNF
}

struct CSPage gCSPage_registers ={
    .name         = "REGISTERS",
    .initFunc     = page_registers_init,
    .drawFunc     = page_registers_draw,
    .inputFunc    = page_registers_input,
    .printFunc    = page_registers_print,
    .contList     = cs_cont_list_registers,
    .settingsList = cs_settings_group_page_registers,
    .flags = {
        .initialized = FALSE,
        .crashed     = FALSE,
    },
};
