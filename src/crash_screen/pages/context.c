#include <ultra64.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen/crash_main.h"
#include "context.h"


static const char* sCauseDesc[18] = {
    /*EXC_INT       */ "Interrupt",
    /*EXC_MOD       */ "TLB modification",
    /*EXC_RMISS     */ "TLB exception on load or inst.",
    /*EXC_WMISS     */ "TLB exception on store",
    /*EXC_RADE      */ "Address error on load or inst.",
    /*EXC_WADE      */ "Address error on store",
    /*EXC_IBE       */ "Bus error on inst.",
    /*EXC_DBE       */ "Bus error on data",
    /*EXC_SYSCALL   */ "Failed Assert: See Assert Page",
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

static const char* sRegNames[29] = { //! TODO: Combine this with sCPURegisterNames
    "AT", "V0", "V1",
    "A0", "A1", "A2",
    "A3", "T0", "T1",
    "T2", "T3", "T4",
    "T5", "T6", "T7",
    "S0", "S1", "S2",
    "S3", "S4", "S5",
    "S6", "S7", "T8",
    "T9", "GP", "SP",
    "S8", "RA",
};


// Print a fixed-point register.
void crash_screen_print_reg(u32 x, u32 y, const char* name, Address addr) {
    // "[register name]: [XXXXXXXX]"
    crash_screen_print(x, y,
        " "STR_COLOR_PREFIX"%s: "STR_COLOR_PREFIX STR_HEX_WORD,
        COLOR_RGBA32_CRASH_REGISTER, name,
        COLOR_RGBA32_WHITE, addr
    );
}

// Print important fixed-point registers.
void crash_screen_print_registers(__OSThreadContext* tc) {
    u32 regNum = 0;
    Register* reg = &tc->at;

    crash_screen_print_reg(TEXT_X(0 * 15), TEXT_Y(3), "PC", tc->pc);
    crash_screen_print_reg(TEXT_X(1 * 15), TEXT_Y(3), "SR", tc->sr);
    crash_screen_print_reg(TEXT_X(2 * 15), TEXT_Y(3), "VA", tc->badvaddr);

    Word data = 0;
    if (try_read_data(&data, tc->pc)) {
        crash_screen_print_reg(TEXT_X(2 * 15), TEXT_Y(13), "MM", data); // The raw data of the asm code that crashed.
    }

    osWritebackDCacheAll();

    for (u32 y = 0; y < 10; y++) {
        for (u32 x = 0; x < 3; x++) {
            if (regNum >= ARRAY_COUNT(sRegNames)) {
                return;
            }

            crash_screen_print_reg(TEXT_X(x * 15), TEXT_Y(4 + y), sRegNames[regNum], *(reg + regNum));

            regNum++;
        }
    }
}

void crash_screen_print_fpcsr(u32 x, u32 y, u32 fpcsr) {
    u32 bit = BIT(17);

    // "FPCSR:[XXXXXXXX]"
    size_t fpcsrSize = crash_screen_print(x, y,
        STR_COLOR_PREFIX"FPCSR:"STR_COLOR_PREFIX STR_HEX_WORD" ",
        COLOR_RGBA32_CRASH_REGISTER,
        COLOR_RGBA32_WHITE, fpcsr
    );
    x += TEXT_WIDTH(fpcsrSize);

    for (u32 i = 0; i < ARRAY_COUNT(sFpcsrDesc); i++) {
        if (fpcsr & bit) {
            // "([float exception description])"
            crash_screen_print(x, y, STR_COLOR_PREFIX"(%s)", COLOR_RGBA32_CRASH_DESCRIPTION, sFpcsrDesc[i]);
            return;
        }

        bit >>= 1;
    }
}

// Print a floating-point register.
void crash_screen_print_float_reg(u32 x, u32 y, u32 regNum, f32* data) {
    // "[register name]:"
    size_t charX = crash_screen_print(x, y, STR_COLOR_PREFIX"F%02d:", COLOR_RGBA32_CRASH_REGISTER, regNum);
    x += TEXT_WIDTH(charX);

    IEEE754_f32 val = { .asF32 = *data };

    char prefix = '\0';

    if (val.mantissa != 0) {
        if (val.exponent == 0x00) {
            prefix = 'D'; // Denormalized value
        } else if (val.exponent == 0xFF) {
            prefix = 'N'; // NaN
        }
    }

    if (prefix == '\0') {
        // "[±][exponent]"
        crash_screen_print(x, y, "% g", val.asF32);
    } else {
        // "[XXXXXXXX]"
        crash_screen_print(x, y, "%c"STR_HEX_WORD, prefix, val.asU32);
    }
}

void crash_screen_print_float_registers(__OSThreadContext* tc) {
    u32 regNum = 0;
    __OSfp* osfp = &tc->fp0;

    crash_screen_print_fpcsr(TEXT_X(0), (TEXT_Y(14) + 5), tc->fpcsr);

    osWritebackDCacheAll();

    for (u32 y = 0; y < 6; y++) {
        for (u32 x = 0; x < 3; x++) {
            if (regNum > 30) {
                return;
            }

            crash_screen_print_float_reg(TEXT_X(x * 15), TEXT_Y(16 + y), regNum, &osfp->f.f_even);

            osfp++;
            regNum += 2;
        }
    }
}

void crash_context_draw(void) {
    __OSThreadContext* tc = &gCrashedThread->context;

    u32 cause = ((tc->cause >> CAUSE_EXCSHIFT) & BITMASK(5));
    // Make the last two cause case indexes sequential for array access.
    if (cause == (EXC_WATCH >> CAUSE_EXCSHIFT)) cause = 16;
    if (cause == (EXC_VCED  >> CAUSE_EXCSHIFT)) cause = 17;

    u32 line = 1;

    // "THREAD:[thread id] ([exception cause description])"
    crash_screen_print(TEXT_X(0), TEXT_Y(line),
        STR_COLOR_PREFIX"THREAD:%d "STR_COLOR_PREFIX"(%s)",
        COLOR_RGBA32_CRASH_THREAD, gCrashedThread->id,
        COLOR_RGBA32_CRASH_DESCRIPTION, sCauseDesc[cause]
    );

    line++;

    osWritebackDCacheAll();

#ifdef INCLUDE_DEBUG_MAP
    const struct MapSymbol* symbol = get_map_symbol(tc->pc, SYMBOL_SEARCH_BACKWARD);
    // "CRASH IN:"
    size_t charX = crash_screen_print(TEXT_X(0), TEXT_Y(line),
        STR_COLOR_PREFIX"CRASH IN: ",
        COLOR_RGBA32_CRASH_AT
    );
    crash_screen_print_symbol_name(TEXT_X(charX), TEXT_Y(line), (CRASH_SCREEN_NUM_CHARS_X - charX), symbol);
#endif

    crash_screen_print_registers(tc);

    osWritebackDCacheAll();

    crash_screen_print_float_registers(tc);
}
