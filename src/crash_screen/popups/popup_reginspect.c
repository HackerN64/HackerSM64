#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/util/map_parser.h"
#include "crash_screen/util/memory_read.h"
#include "crash_screen/util/registers.h"
#include "crash_screen/cs_controls.h"
#include "crash_screen/cs_draw.h"
#include "crash_screen/cs_descriptions.h"
#include "crash_screen/cs_main.h"
#include "crash_screen/cs_pages.h"
#include "crash_screen/cs_print.h"
#include "crash_screen/cs_settings.h"

#include "popup_address_select.h"

#include "popup_reginspect.h"


RegisterId gInspectedRegister = {
    // Default to R0.
    .cop = CPU,
    .idx = REG_CPU_R0,
    .flt = FALSE,
    .out = FALSE,
};

Address sInspectedRegisterPtrAddr = 0x00000000;


void cs_popup_reginspect_init(void) {
    sInspectedRegisterPtrAddr = 0x00000000;
}

void cs_popup_reginspect_draw_reg_value(u32 x, u32 y, RegisterId regId, uint64_t val64) {
    const RegisterInfo* regInfo = get_reg_info(regId.cop, regId.idx);
    const _Bool is64Bit = (regInfo->size == sizeof(uint64_t));
    const uint32_t val32 = (uint32_t)val64;

    // Print as hex:
    if (is64Bit) {
        cs_print(x, y, (" "STR_HEX_PREFIX STR_HEX_LONG), val64);
    } else {
        cs_print(x, y, (" "STR_HEX_PREFIX STR_HEX_WORD), val32);
    }

    // Print as other floatint point formats:
    if (regId.flt) {
        y += TEXT_HEIGHT(1);
        const IEEE754_f64 flt64 = { .asU64 = val64, };
        const IEEE754_f32 flt32 = { .asU32 = val32, };
        enum FloatErrorType fltErrType = (is64Bit ? validate_f64(flt64) : validate_f32(flt32));
        if (fltErrType != FLT_ERR_NONE) {
            cs_print(x, y, ((fltErrType == FLT_ERR_DENORM) ? "denormalized" : "NaN"));
        } else {
            cs_print(x, y, "% g", (is64Bit ? flt64.asF64 : flt32.asF32));
            y += TEXT_HEIGHT(1);
            cs_print(x, y, "% e", (is64Bit ? flt64.asF64 : flt32.asF32));
        }
    }
}

_Bool cs_print_reg_info_CPU_V(u32 line, UNUSED uint64_t val) {
    u32 v1 = get_reg_val(CPU, REG_CPU_V1);
    if (v1 > 1) {
        u32 v0 = get_reg_val(CPU, REG_CPU_V0);
        HiLo64 combinedV = {
            .hi = v0,
            .lo = v1,
        };
        cs_print(TEXT_X(2), TEXT_Y(line), (" "STR_HEX_PREFIX STR_HEX_LONG), combinedV.raw);
        return TRUE;
    }

    return FALSE;
}

_Bool cs_print_reg_info_C0_SR(u32 line, uint64_t val) {
    const RGBA32 infoColor = COLOR_RGBA32_GRAY;
    const Reg_CP0_Status s = {
        .raw = (uint32_t)val,
    };
    cs_print(TEXT_X(2), TEXT_Y(line++), "cop1 useable:\t\t"STR_COLOR_PREFIX"%d", infoColor, s.CU_.CP1); // The other coprocessor bits are ignored by the N64.
#undef RP // PR/region.h moment
    cs_print(TEXT_X(2), TEXT_Y(line++), "low power mode:\t\t"STR_COLOR_PREFIX"%d", infoColor, s.RP);
    cs_print(TEXT_X(2), TEXT_Y(line++), "extra fpr:\t\t\t"STR_COLOR_PREFIX"%d", infoColor, s.FR);
    const char* endian[] = {
        [0] = "big",
        [1] = "little",
    };
    cs_print(TEXT_X(2), TEXT_Y(line++), "endian:\t\t\t\t"STR_COLOR_PREFIX"%s", infoColor, endian[s.RE]);
    cs_print(TEXT_X(2), TEXT_Y(line++), "diagnostic bits:\t"STR_COLOR_PREFIX STR_HEX_PREFIX"%03X", infoColor, s.DS); //! TODO: list this properly
    cs_print(TEXT_X(2), TEXT_Y(line++), "interrupt mask:\t\t"STR_COLOR_PREFIX STR_HEX_PREFIX"%02X", infoColor, s.IM);
    const char* bit_mode[] = {
        [0] = "32-bit",
        [1] = "64-bit",
    };
    cs_print(TEXT_X(2), TEXT_Y(line++), "kernel:\t\t\t\t"STR_COLOR_PREFIX"%s", infoColor, bit_mode[s.KX]);
    cs_print(TEXT_X(2), TEXT_Y(line++), "supervisor:\t\t\t"STR_COLOR_PREFIX"%s", infoColor, bit_mode[s.SX]);
    cs_print(TEXT_X(2), TEXT_Y(line++), "user:\t\t\t\t"STR_COLOR_PREFIX"%s", infoColor, bit_mode[s.UX]);
    const char* exec_mode[] = {
        [0b00] = "kernel",
        [0b01] = "supervisor",
        [0b10] = "user",
    };
    cs_print(TEXT_X(2), TEXT_Y(line++), "exec mode:\t\t\t"STR_COLOR_PREFIX"%s", infoColor, exec_mode[s.KSU]);
    cs_print(TEXT_X(2), TEXT_Y(line++), "error:\t\t\t\t"STR_COLOR_PREFIX"%d", infoColor, s.ERL);
    cs_print(TEXT_X(2), TEXT_Y(line++), "exception:\t\t\t"STR_COLOR_PREFIX"%d", infoColor, s.EXL);
    cs_print(TEXT_X(2), TEXT_Y(line++), "global interrupts:\t"STR_COLOR_PREFIX"%d", infoColor, s.IE);

    return TRUE;
}

_Bool cs_print_reg_info_C0_CAUSE(u32 line, uint64_t val) {
    const RGBA32 infoColor = COLOR_RGBA32_GRAY;
    const Reg_CP0_Cause c = {
        .raw = (uint32_t)val,
    };

    cs_print(TEXT_X(2), TEXT_Y(line++), "in branch delay slot:\t"STR_COLOR_PREFIX"%d", infoColor, c.BD);
    cs_print(TEXT_X(2), TEXT_Y(line++), "interrupts pending:\t\t"STR_COLOR_PREFIX STR_HEX_PREFIX"%02X", infoColor, c.IP);
    cs_print(TEXT_X(2), TEXT_Y(line++), "exc code:\t\t\t\t"STR_COLOR_PREFIX"%d", infoColor, c.Exc_Code);
    if (c.Exc_Code == (EXC_CPU >> CAUSE_EXCSHIFT)) {
        cs_print(TEXT_X(2), TEXT_Y(line++), "unusable coprocessor:\t"STR_COLOR_PREFIX"cop%d", infoColor, c.CE);
    }
    const char* causeDesc = get_cause_desc(&gInspectThread->context, FALSE);
    if (causeDesc != NULL) {
        cs_print(TEXT_X(4), TEXT_Y(line++), STR_COLOR_PREFIX"(%s)", infoColor, causeDesc);
    }

    return TRUE;
}

_Bool cs_print_reg_info_FPR_CSR(u32 line, uint64_t val) {
    const RGBA32 infoColor = COLOR_RGBA32_GRAY;
    const Reg_FPR_31 f = {
        .raw = (uint32_t)val,
    };
    cs_print(TEXT_X(2), TEXT_Y(line++), "flush denorms to zero:\t"STR_COLOR_PREFIX"%d", infoColor, f.FS);
    cs_print(TEXT_X(2), TEXT_Y(line++), "condition bit:\t\t\t"STR_COLOR_PREFIX"%d", infoColor, f.C);
    const char* round_mode[] = {
        [0b00] = "nearest",
        [0b01] = "zero",
        [0b10] = "+inf",
        [0b11] = "-inf",
    };
    cs_print(TEXT_X(2), TEXT_Y(line++), "rounding mode:\t\t\t"STR_COLOR_PREFIX"to %s", infoColor, round_mode[f.RM]);
    line++;
    cs_print(TEXT_X(2), TEXT_Y(line++), "exception bits:");
    cs_print(TEXT_X(4), TEXT_Y(line++), "E:unimpl   | V:invalid   | Z:div0");
    cs_print(TEXT_X(4), TEXT_Y(line++), "O:overflow | U:underflow | I:inexact");
    line++;
    cs_print(TEXT_X(12), TEXT_Y(line++), "       E V Z O U I");
    cs_print(TEXT_X(12), TEXT_Y(line++), "cause: "STR_COLOR_PREFIX"%d %d %d %d %d %d", infoColor, f.cause_.E, f.cause_.V,  f.cause_.Z,  f.cause_.O,  f.cause_.U,  f.cause_.I );
    cs_print(TEXT_X(12), TEXT_Y(line++), "enable:  "STR_COLOR_PREFIX"%d %d %d %d %d",  infoColor,             f.enable_.V, f.enable_.Z, f.enable_.O, f.enable_.U, f.enable_.I);
    cs_print(TEXT_X(12), TEXT_Y(line++), "flags:   "STR_COLOR_PREFIX"%d %d %d %d %d",  infoColor,             f.flag_.V,   f.flag_.Z,   f.flag_.O,   f.flag_.U,   f.flag_.I  );

    return TRUE;
}

typedef struct RegInspectExtraInfo {
    /*0x00*/ const enum Coprocessors cop;
    /*0x04*/ const int idx;
    /*0x08*/ _Bool (*func)(u32 line, uint64_t val);
    /*0x0C*/ const char* title;
} RegInspectExtraInfo; /*0x10*/

const RegInspectExtraInfo sRegInspectExtraInfoFuncs[] = {
    { .cop = CPU,  .idx = REG_CPU_V0,             .func = cs_print_reg_info_CPU_V,    .title = "64-bit combined value of $v0 and $v1", },
    { .cop = CPU,  .idx = REG_CPU_V1,             .func = cs_print_reg_info_CPU_V,    .title = "64-bit combined value of $v0 and $v1", },
    { .cop = COP0, .idx = REG_CP0_SR,             .func = cs_print_reg_info_C0_SR,    .title = "decoded info", },
    { .cop = COP0, .idx = REG_CP0_CAUSE,          .func = cs_print_reg_info_C0_CAUSE, .title = "decoded info", },
    { .cop = FCR,  .idx = REG_FCR_CONTROL_STATUS, .func = cs_print_reg_info_FPR_CSR,  .title = "decoded info", },
};

// Register popup box draw function.
void cs_popup_reginspect_draw(void) {
    const s32 bgStartX = (CRASH_SCREEN_X1 + (TEXT_WIDTH(1) / 2));
    const s32 bgStartY = (CRASH_SCREEN_Y1 + (TEXT_HEIGHT(1) / 2));
    const s32 bgW = (CRASH_SCREEN_W - TEXT_WIDTH(1));
    const s32 bgH = (CRASH_SCREEN_H - TEXT_HEIGHT(1));
    cs_draw_dark_rect(
        bgStartX, bgStartY,
        bgW, bgH,
        cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_POPUP_OPACITY)
    );

    RegisterId regId = gInspectedRegister;

    enum Coprocessors cop = regId.cop;
    int idx = regId.idx;

    const RegisterInfo* regInfo = get_reg_info(cop, idx);
    if (regInfo == NULL) {
        return;
    }
    _Bool is64Bit = (regInfo->size == sizeof(uint64_t));

    u32 line = 1;
    const RGBA32 descColor = COLOR_RGBA32_CRASH_PAGE_NAME;
    // "[register] REGISTER".
    cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"register on thread %d (%s):", COLOR_RGBA32_CRASH_PAGE_NAME,
        gInspectThread->id, get_thread_name(gInspectThread)
    );
    size_t charX = cs_print(TEXT_X(2), TEXT_Y(line), STR_COLOR_PREFIX"\"$%s\"", COLOR_RGBA32_CRASH_VARIABLE, regInfo->name);
    const char* copName = get_coprocessor_name(cop);
    if (copName != NULL) {
        cs_print(TEXT_X(2 + charX), TEXT_Y(line), STR_COLOR_PREFIX" in %s", COLOR_RGBA32_CRASH_VARIABLE, copName);
    }
    line++;
    const char* regDesc = get_reg_desc(cop, idx);
    if (regDesc != NULL) {
        cs_print(TEXT_X(2), TEXT_Y(line++), STR_COLOR_PREFIX"(%s)", COLOR_RGBA32_CRASH_VARIABLE, regDesc);
    }
    line++;

    cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"%d-bit value:", descColor, (is64Bit ? 64 : 32));
    uint64_t value = get_reg_val(cop, idx);
    cs_popup_reginspect_draw_reg_value(TEXT_X(2), TEXT_Y(line++), regId, value);

    line++;
    _Bool hasExInfo = FALSE;
    const RegInspectExtraInfo* exInfo = &sRegInspectExtraInfoFuncs[0];
    for (int i = 0; i < ARRAY_COUNT(sRegInspectExtraInfoFuncs); i++) {
        if (exInfo->cop == cop && exInfo->idx == idx) {
            hasExInfo = exInfo->func(line, value);
            break;
        }
        exInfo++;
    }
    line--;

    if (hasExInfo) {
        cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"%s:", COLOR_RGBA32_CRASH_PAGE_NAME, exInfo->title);
        sInspectedRegisterPtrAddr = 0x00000000;
    } else {
#ifdef INCLUDE_DEBUG_MAP
        u32 val32 = (uint32_t)value;
        const MapSymbol* symbol = get_map_symbol(val32, SYMBOL_SEARCH_BACKWARD);
        if (symbol != NULL) {
            cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"pointer to:", COLOR_RGBA32_CRASH_PAGE_NAME);
            cs_print_symbol_name(TEXT_X(2), TEXT_Y(line++), (CRASH_SCREEN_NUM_CHARS_X - 4), symbol, FALSE);
            cs_print(TEXT_X(2), TEXT_Y(line++),
                (STR_COLOR_PREFIX"+"STR_HEX_HALFWORD" "),
                COLOR_RGBA32_CRASH_OFFSET, (val32 - symbol->addr)
            );
        }
#endif // INCLUDE_DEBUG_MAP
        u32 data = 0;
        if (try_read_word_aligned(&data, val32)) {
            sInspectedRegisterPtrAddr = val32;
            cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"data at pointer:", COLOR_RGBA32_CRASH_PAGE_NAME);
            if (is_in_code_segment(val32)) {
                print_as_insn(TEXT_X(2), TEXT_Y(line++), val32, data);
            } else {
                cs_print(TEXT_X(2), TEXT_Y(line++), STR_HEX_PREFIX STR_HEX_WORD, data);
                print_as_binary(TEXT_X(2), TEXT_Y(line++), data, COLOR_RGBA32_WHITE);
            }
        } else {
            sInspectedRegisterPtrAddr = 0x00000000;
        }
    }

    cs_draw_outline(bgStartX, bgStartY, bgW, bgH, COLOR_RGBA32_CRASH_DIVIDER);

    osWritebackDCacheAll();
}

void cs_popup_reginspect_input(void) {
    //! TODO: If register is address, goto address select. (return to this if cancelled).
    u16 buttonPressed = gCSCompositeController->buttonPressed;

    if (buttonPressed & A_BUTTON) {
        //! TODO: Option to jump to register's location in inspected thread's __OSThreadContext.
        if (sInspectedRegisterPtrAddr != 0x00000000) {
            open_address_select(sInspectedRegisterPtrAddr);
        }
    }
    if (buttonPressed & (B_BUTTON | START_BUTTON)) {
        // Close the popup without jumping.
        cs_open_popup(CS_POPUP_NONE);
    }
}

// Open the register inspect popup box.
void cs_open_reginspect(RegisterId regId) {
    cs_open_popup(CS_POPUP_REGINSPECT);
    gInspectedRegister = regId;
}

struct CSPopup gCSPopup_reginspect = {
    .name      = "REGISTER",
    .initFunc  = cs_popup_reginspect_init,
    .drawFunc  = cs_popup_reginspect_draw,
    .inputFunc = cs_popup_reginspect_input,
    .flags = {
        .allowPageInput  = FALSE,
        .allowChangePage = FALSE,
    },
};
