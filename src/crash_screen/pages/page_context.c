#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/crash_controls.h"
#include "crash_screen/crash_draw.h"
#include "crash_screen/crash_main.h"
#include "crash_screen/crash_pages.h"
#include "crash_screen/crash_print.h"
#include "crash_screen/crash_settings.h"
#include "crash_screen/map_parser.h"
#include "crash_screen/memory_read.h"

#include "page_context.h"

#ifdef UNF
#include "usb/debug.h"
#endif // UNF


struct CSSetting cs_settings_group_page_context[] = {
    [CS_OPT_HEADER_PAGE_CONTEXT ] = { .type = CS_OPT_TYPE_HEADER,  .name = "CONTEXT",                        .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
#ifdef INCLUDE_DEBUG_MAP
    [CS_OPT_CONTEXT_PARSE_REG   ] = { .type = CS_OPT_TYPE_SETTING, .name = "Parse register addr names",      .valNames = &gValNames_bool,          .val = FALSE,                     .defaultVal = FALSE,                     .lowerBound = FALSE,                 .upperBound = TRUE,                       },
#endif // INCLUDE_DEBUG_MAP
    [CS_OPT_CONTEXT_FLOATS_FMT  ] = { .type = CS_OPT_TYPE_SETTING, .name = "Floats print format",            .valNames = &gValNames_print_num_fmt, .val = PRINT_NUM_FMT_DEC,         .defaultVal = PRINT_NUM_FMT_DEC,         .lowerBound = PRINT_NUM_FMT_HEX,     .upperBound = PRINT_NUM_FMT_SCI,          },
    [CS_OPT_END_CONTEXT         ] = { .type = CS_OPT_TYPE_END, },
};


const enum ControlTypes cs_cont_list_context[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_HIDE_CRASH_SCREEN,
#ifdef UNF
    CONT_DESC_OS_PRINT,
#endif // UNF
    CONT_DESC_CYCLE_FLOATS_MODE,
    CONT_DESC_LIST_END,
};


// Main list of registers to print.
#define LIST_REG(field, regName) {                          \
    .offset = __builtin_offsetof(__OSThreadContext, field), \
    .size = sizeof_member(__OSThreadContext, field),        \
    .name = (regName),                                      \
}
static const OSThreadContextRegister sRegList[32 + 1] = {
    LIST_REG(pc, "PC"), LIST_REG(sr, "SR"), LIST_REG(badvaddr, "VA"),
    LIST_REG(at, "AT"), LIST_REG(v0, "V0"), LIST_REG(v1, "V1"),
    LIST_REG(a0, "A0"), LIST_REG(a1, "A1"), LIST_REG(a2, "A2"),
    LIST_REG(a3, "A3"), LIST_REG(t0, "T0"), LIST_REG(t1, "T1"),
    LIST_REG(t2, "T2"), LIST_REG(t3, "T3"), LIST_REG(t4, "T4"),
    LIST_REG(t5, "T5"), LIST_REG(t6, "T6"), LIST_REG(t7, "T7"),
    LIST_REG(s0, "S0"), LIST_REG(s1, "S1"), LIST_REG(s2, "S2"),
    LIST_REG(s3, "S3"), LIST_REG(s4, "S4"), LIST_REG(s5, "S5"),
    LIST_REG(s6, "S6"), LIST_REG(s7, "S7"), LIST_REG(t8, "T8"),
    LIST_REG(t9, "T9"), LIST_REG(gp, "GP"), LIST_REG(sp, "SP"),
    LIST_REG(s8, "S8"), LIST_REG(ra, "RA"),
    {
        .offset = (Address)-1,
        .size = sizeof(Address),
        .name = "MM",
    },
};


void page_context_init(void) {

}

// Print a fixed-point register.
void cs_context_print_reg(u32 x, u32 y, const char* name, Word val) {
    const MapSymbol* symbol = NULL;

    // "[register name]:"
    size_t charX = cs_print(x, y,
        " "STR_COLOR_PREFIX"%s:",
        COLOR_RGBA32_CRASH_VARIABLE, name
    );

#ifdef INCLUDE_DEBUG_MAP
    if (cs_get_setting_val(CS_OPT_GROUP_PAGE_CONTEXT, CS_OPT_CONTEXT_PARSE_REG)) {
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

// Gets the value of a OSThreadContextRegister register.
u64 get_thread_register_val(__OSThreadContext* tc, const OSThreadContextRegister* reg) {
    Address addr = ((Address)tc + reg->offset);

    // Special case for "MM".
    if (reg->offset == (Address)-1) {
        addr = tc->pc;
    }

    return ((reg->size == sizeof(u64)) ? *(u64*)addr : *(u32*)addr);
}

// Print important fixed-point registers.
u32 cs_context_print_registers(u32 line, __OSThreadContext* tc) {
    const size_t columnWidth = 15;
    const u32 columns = 3;
    const u32 rows = (ARRAY_COUNT(sRegList) / columns);
    const OSThreadContextRegister* reg = sRegList;

    for (u32 y = 0; y < rows; y++) {
        for (u32 x = 0; x < columns; x++) {
            cs_context_print_reg(TEXT_X(x * columnWidth), TEXT_Y(line + y), reg->name, (u32)get_thread_register_val(tc, reg));

            reg++;
        }
    }

    return (line + rows);
}

// Print a floating-point register.
void cs_context_print_float_reg(u32 x, u32 y, u32 regNum, f32* data) {
    // "[register name]:"
    size_t charX = cs_print(x, y, STR_COLOR_PREFIX"F%02d:", COLOR_RGBA32_CRASH_VARIABLE, regNum);
    x += TEXT_WIDTH(charX);

    IEEE754_f32 val = {
        .asF32 = *data,
    };

    char prefix = '\0';

    if (val.mantissa != 0) {
        if (val.exponent == 0x00) {
            prefix = 'D'; // Denormalized value.
        } else if (val.exponent == 0xFF) {
            prefix = 'N'; // NaN.
        }
    }

    if (prefix != '\0') {
        // "[prefix][XXXXXXXX]"
        cs_print(x, y, "%c"STR_HEX_WORD, prefix, val.asU32);
    } else {
        const enum CSPrintNumberFormats floatsFormat = cs_get_setting_val(CS_OPT_GROUP_PAGE_CONTEXT, CS_OPT_CONTEXT_FLOATS_FMT);

        switch (floatsFormat) {
            case PRINT_NUM_FMT_HEX: cs_print(x, y, " "STR_HEX_WORD, val.asU32); break; // "[XXXXXXXX]"
            default:
            case PRINT_NUM_FMT_DEC: cs_print(x, y, "% g",           val.asF32); break; // "[±][exponent]"
            case PRINT_NUM_FMT_SCI: cs_print(x, y, "% .3e",         val.asF32); break; // "[scientific notation]"
        }
    }
}

void cs_context_print_float_registers(u32 line, __OSThreadContext* tc) {
    const size_t columnWidth = 15;
    u32 regNum = 0;
    __OSfp* osfp = &tc->fp0;

    // cs_context_print_fpcsr(TEXT_X(0), TEXT_Y(line), tc->fpcsr);
    line++;

    osWritebackDCacheAll();

    for (u32 y = 0; y < 6; y++) {
        for (u32 x = 0; x < 3; x++) {
            if (regNum > 30) {
                return;
            }

            cs_context_print_float_reg(TEXT_X(x * columnWidth), TEXT_Y(line + y), regNum, &osfp->f.f_even);

            osfp++;
            regNum += 2;
        }
    }
}

void page_context_draw(void) {
    __OSThreadContext* tc = &gCrashedThread->context;
    u32 line = 1;
    line++;

    line = cs_context_print_registers(line, tc);

    line++;

    osWritebackDCacheAll();

    cs_context_print_float_registers(line, tc);
}

void page_context_input(void) {
    if (gCSCompositeController->buttonPressed & B_BUTTON) {
        // Cycle floats print mode.
        cs_inc_setting(CS_OPT_GROUP_PAGE_CONTEXT, CS_OPT_CONTEXT_FLOATS_FMT, 1);
    }
}

void page_context_print(void) {
#ifdef UNF
    debug_printf("\n");

    __OSThreadContext* tc = &gCrashedThread->context;

    // Thread registers:
    const u32 columns = 3;
    const u32 rows = (ARRAY_COUNT(sRegList) / columns);
    const OSThreadContextRegister* reg = sRegList;
    for (u32 y = 0; y < rows; y++) {
        debug_printf("- ");
        for (u32 x = 0; x < columns; x++) {
            debug_printf("%s "STR_HEX_PREFIX STR_HEX_LONG" ", reg->name, get_thread_register_val(tc, reg));
            reg++;
        }
        debug_printf("\n");
    }

    // Float registers:
    u32 regNum = 0;
    __OSfp* osfp = &tc->fp0;
    for (u32 i = 0; i < 8; i++) {
        debug_printf("- ");
        for (u32 j = 0; j < 2; j++) {
            if (regNum > 30) break;
            debug_printf("d%02d "STR_HEX_DECIMAL"\t", regNum, osfp->f.f_even);
            regNum += 2;
            osfp++;
        }
        debug_printf("\n");
    }
#endif // UNF
}

struct CSPage gCSPage_context ={
    .name         = "REGISTERS",
    .initFunc     = page_context_init,
    .drawFunc     = page_context_draw,
    .inputFunc    = page_context_input,
    .printFunc    = page_context_print,
    .contList     = cs_cont_list_context,
    .settingsList = cs_settings_group_page_context,
    .flags = {
        .initialized = FALSE,
        .crashed     = FALSE,
    },
};
