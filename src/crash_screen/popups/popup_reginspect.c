#include <ultra64.h>

#include "types.h"
#include "sm64.h"

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

#include "popup_address.h"

#include "popup_reginspect.h"


RegisterId gInspectedRegister = {
    // Default to R0.
    .cop         = CPU,
    .idx         = REG_CPU_R0,
    .valInfo.raw = 0,
};

Address sInspectedRegisterPtrAddr = 0x00000000;


void cs_popup_reginspect_init(void) {
    sInspectedRegisterPtrAddr = 0x00000000;
}

CSTextCoord_u32 cs_popup_reginspect_draw_reg_value(CSTextCoord_u32 line, RegisterId regId, const Doubleword val64, _Bool is64Bit) {
    cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"%d-bit value:", COLOR_RGBA32_CRASH_PAGE_NAME, (is64Bit ? 64 : 32));

    const Word val32 = (Word)val64;

    CS_SET_DEFAULT_PRINT_COLOR_START(COLOR_RGBA32_CRASH_HEADER);
    const RGBA32 valColor = COLOR_RGBA32_WHITE;

    // Print as hex:
    if (is64Bit) {
        cs_print(TEXT_X(2), TEXT_Y(line++), ("hex: "STR_COLOR_PREFIX STR_HEX_PREFIX STR_HEX_LONG), valColor, val64);
    } else {
        cs_print(TEXT_X(2), TEXT_Y(line++), ("hex: "STR_COLOR_PREFIX STR_HEX_PREFIX STR_HEX_WORD), valColor, val32);
    }

    // Print as other floatint point formats:
    if (regId.valInfo.type == REG_VAL_TYPE_FLOAT) {
        //! TODO: Combine this with cs_print_f32.
        const IEEE754_f64 flt64 = { .asU64 = val64, };
        const IEEE754_f32 flt32 = { .asU32 = val32, };
        enum FloatErrorType fltErrType = (is64Bit ? validate_f64(flt64) : validate_f32(flt32));
        if (fltErrType != FLT_ERR_NONE) {
            cs_print(TEXT_X(2), TEXT_Y(line++), ((fltErrType == FLT_ERR_DENORM) ? "denormalized" : "NaN"));
        } else {
            cs_print(TEXT_X(2), TEXT_Y(line++), ("dec:"STR_COLOR_PREFIX"% g"), valColor, (is64Bit ? flt64.asF64 : flt32.asF32));
            cs_print(TEXT_X(2), TEXT_Y(line++), ("sci:"STR_COLOR_PREFIX"% e"), valColor, (is64Bit ? flt64.asF64 : flt32.asF32));
        }
    }

    CS_SET_DEFAULT_PRINT_COLOR_END();

    return line;
}

_Bool cs_print_reg_info_C0_SR(CSTextCoord_u32 line, Doubleword val) {
    const ScreenCoord_u32 x = TEXT_X(2);
    const RGBA32 infoColor = COLOR_RGBA32_GRAY;
    const Reg_CP0_Status s = {
        .raw = (Word)val,
    };

    cs_print(x, TEXT_Y(line++), "cop1 useable:\t\t"STR_COLOR_PREFIX"%d", infoColor, s.CU_.CP1); // The other coprocessor bits are ignored by the N64.
#undef RP // PR/region.h moment
    cs_print(x, TEXT_Y(line++), "low power mode:\t\t"STR_COLOR_PREFIX"%d", infoColor, s.RP);
    cs_print(x, TEXT_Y(line++), "extra fpr:\t\t\t"STR_COLOR_PREFIX"%d", infoColor, s.FR);
    const char* endian[] = {
        [0] = "big",
        [1] = "little",
    };
    cs_print(x, TEXT_Y(line++), "endian:\t\t\t\t"STR_COLOR_PREFIX"%s", infoColor, endian[s.RE]);
    //! TODO: list these two properly:
    cs_print(x, TEXT_Y(line++), "diagnostic bits:\t"STR_COLOR_PREFIX STR_HEX_PREFIX"%03X", infoColor, s.DS);
    cs_print(x, TEXT_Y(line++), "interrupt mask:\t\t"STR_COLOR_PREFIX STR_HEX_PREFIX"%02X", infoColor, s.IM);
    cs_print(x, TEXT_Y(line++), "kernel:\t\t\t\t"STR_COLOR_PREFIX"%d-bit",   infoColor, (s.KX ? 64 : 32));
    cs_print(x, TEXT_Y(line++), "supervisor:\t\t\t"STR_COLOR_PREFIX"%d-bit", infoColor, (s.SX ? 64 : 32));
    cs_print(x, TEXT_Y(line++), "user:\t\t\t\t"STR_COLOR_PREFIX"%d-bit",     infoColor, (s.UX ? 64 : 32));
    const char* exec_mode[] = {
        [0b00] = "kernel",
        [0b01] = "supervisor",
        [0b10] = "user",
    };
    cs_print(x, TEXT_Y(line++), "exec mode:\t\t\t"STR_COLOR_PREFIX"%s", infoColor, exec_mode[s.KSU]);
    cs_print(x, TEXT_Y(line++), "error:\t\t\t\t"STR_COLOR_PREFIX"%d", infoColor, s.ERL);
    cs_print(x, TEXT_Y(line++), "exception:\t\t\t"STR_COLOR_PREFIX"%d", infoColor, s.EXL);
    cs_print(x, TEXT_Y(line++), "global interrupts:\t"STR_COLOR_PREFIX"%d", infoColor, s.IE);

    return TRUE;
}

_Bool cs_print_reg_info_C0_CAUSE(CSTextCoord_u32 line, Doubleword val) {
    const ScreenCoord_u32 x1 = TEXT_X(2);
    const ScreenCoord_u32 x2 = TEXT_X(4);
    const RGBA32 infoColor = COLOR_RGBA32_GRAY;
    const Reg_CP0_Cause c = {
        .raw = (Word)val,
    };

    cs_print(x1, TEXT_Y(line++), "is branch delay slot:\t"STR_COLOR_PREFIX"%d", infoColor, c.BD);
    line++;
    cs_print(x1, TEXT_Y(line++), "interrupts pending:");
    cs_print(x2, TEXT_Y(line++), "Timer:\t\t\t\t  "STR_COLOR_PREFIX"%d", infoColor, c.IP_.Timer);
    cs_print(x2, TEXT_Y(line++), "Indy RDB:\t\t\t  "STR_COLOR_PREFIX"R:%d/W:%d", infoColor, c.IP_External_.Indy_R, c.IP_External_.Indy_W);
    cs_print(x2, TEXT_Y(line++), "Reset (NMI):\t\t  "STR_COLOR_PREFIX"%d", infoColor, c.IP_External_.Reset);
    cs_print(x2, TEXT_Y(line++), "Cartridge:\t\t\t  "STR_COLOR_PREFIX"%d", infoColor, c.IP_External_.Cartridge);
    cs_print(x2, TEXT_Y(line++), "MIPS:\t\t\t\t  "STR_COLOR_PREFIX"%d", infoColor, c.IP_External_.MI);
    cs_print(x2, TEXT_Y(line++), "Software:\t\t\t  "STR_COLOR_PREFIX"%d", infoColor, c.IP_.Software);
    line++;
    cs_print(x1, TEXT_Y(line++), "exc code:\t\t\t\t"STR_COLOR_PREFIX"%d", infoColor, c.Exc_Code);
    CSTextCoord_u32 charX = 4;
    const char* causeDesc = get_cause_desc(&gInspectThread->context, FALSE);
    if (causeDesc != NULL) {
        charX += cs_print(TEXT_X(charX), TEXT_Y(line), STR_COLOR_PREFIX"%s", infoColor, causeDesc);
    }
    if (c.Exc_Code == (EXC_CPU >> CAUSE_EXCSHIFT)) {
        cs_print(TEXT_X(charX), TEXT_Y(line++), " "STR_COLOR_PREFIX"(cop%d)", infoColor, c.CE);
    }

    return TRUE;
}

_Bool cs_print_reg_info_FPR_CSR(CSTextCoord_u32 line, Doubleword val) {
    const ScreenCoord_u32 x1 = TEXT_X(2);
    const ScreenCoord_u32 x2 = TEXT_X(4);
    const ScreenCoord_u32 x3 = TEXT_X(12);
    const RGBA32 infoColor = COLOR_RGBA32_GRAY;
    const Reg_FPR_31 f = {
        .raw = (Word)val,
    };

    cs_print(TEXT_X(2), TEXT_Y(line++), "flush denorms to zero:\t"STR_COLOR_PREFIX"%d", infoColor, f.FS);
    cs_print(TEXT_X(2), TEXT_Y(line++), "condition bit:\t\t\t"STR_COLOR_PREFIX"%d", infoColor, f.C);
    const char* round_mode[] = {
        [0b00] = "nearest",
        [0b01] = "zero",
        [0b10] = "+inf",
        [0b11] = "-inf",
    };
    cs_print(x1, TEXT_Y(line++), "rounding mode:\t\t\t"STR_COLOR_PREFIX"to %s", infoColor, round_mode[f.RM]);
    line++;
    cs_print(x1, TEXT_Y(line++), "exception bits:");
    cs_print(x2, TEXT_Y(line++), "E:unimpl   | V:invalid   | Z:div0");
    cs_print(x2, TEXT_Y(line++), "O:overflow | U:underflow | I:inexact");
    line++;
    cs_print(x3, TEXT_Y(line++), "       E V Z O U I");
    cs_print(x3, TEXT_Y(line++), "cause: "STR_COLOR_PREFIX"%d %d %d %d %d %d", infoColor, f.cause_.E, f.cause_.V,  f.cause_.Z,  f.cause_.O,  f.cause_.U,  f.cause_.I );
    cs_print(x3, TEXT_Y(line++), "enable:  "STR_COLOR_PREFIX"%d %d %d %d %d",  infoColor,             f.enable_.V, f.enable_.Z, f.enable_.O, f.enable_.U, f.enable_.I);
    cs_print(x3, TEXT_Y(line++), "flags:   "STR_COLOR_PREFIX"%d %d %d %d %d",  infoColor,             f.flag_.V,   f.flag_.Z,   f.flag_.O,   f.flag_.U,   f.flag_.I  );

    return TRUE;
}

_Bool cs_print_reg_info_SPC_RCP(CSTextCoord_u32 line, Doubleword val) {
    const ScreenCoord_u32 x1 = TEXT_X(2);
    const RGBA32 infoColor = COLOR_RGBA32_GRAY;
    cs_print(x1, TEXT_Y(line++), "interrupts:\t\t"STR_COLOR_PREFIX STR_HEX_PREFIX"%02X", infoColor, (u32)(val & (RCP_IMASK >> RCP_IMASKSHIFT)));

    return TRUE;
}

void cs_reginspect_pointer(CSTextCoord_u32 line, Word val32) {
    if (IS_DEBUG_MAP_INCLUDED()) {
        const MapSymbol* symbol = get_map_symbol(val32, SYMBOL_SEARCH_BACKWARD);
        if (symbol != NULL) {
            cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"pointer to:", COLOR_RGBA32_CRASH_PAGE_NAME);
            cs_print_symbol_name(TEXT_X(2), TEXT_Y(line++), (CRASH_SCREEN_NUM_CHARS_X - 4), symbol, FALSE);
            cs_print(TEXT_X(2), TEXT_Y(line++),
                (STR_COLOR_PREFIX"+"STR_HEX_HALFWORD" "),
                COLOR_RGBA32_CRASH_OFFSET, (val32 - symbol->addr)
            );
        }
    }

    Word dataAtAddr = 0x00000000;
    if (is_valid_ram_addr(val32) && try_read_word_aligned(&dataAtAddr, val32)) {
        sInspectedRegisterPtrAddr = val32;
        cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"data at dereferenced pointer:", COLOR_RGBA32_CRASH_PAGE_NAME);
        if (addr_is_in_text_segment(val32)) {
            format_and_print_insn(TEXT_X(2), TEXT_Y(line++), val32, dataAtAddr);
        } else {
            CS_SET_DEFAULT_PRINT_COLOR_START(COLOR_RGBA32_CRASH_HEADER);
            const RGBA32 valColor = COLOR_RGBA32_WHITE;
            cs_print(TEXT_X(2), TEXT_Y(line++), ("hex: "STR_COLOR_PREFIX STR_HEX_PREFIX STR_HEX_WORD), valColor, dataAtAddr);
            CSTextCoord_u32 charX = cs_print(TEXT_X(2), TEXT_Y(line), "bin: ");
            print_data_as_binary(TEXT_X(2 + charX), TEXT_Y(line++), &dataAtAddr, sizeof(dataAtAddr), valColor);
            CS_SET_DEFAULT_PRINT_COLOR_END();
        }
    } else {
        sInspectedRegisterPtrAddr = 0x00000000;
    }
}

typedef struct RegInspectExtraInfo {
    /*0x00*/ const enum Coprocessors cop;
    /*0x01*/ const s8 idx;
    /*0x03*/ const u8 pad[2];
    /*0x04*/ _Bool (*func)(CSTextCoord_u32 line, Doubleword val);
} RegInspectExtraInfo; /*0x08*/
const RegInspectExtraInfo sRegInspectExtraInfoFuncs[] = {
    { .cop = COP0, .idx = REG_CP0_SR,             .func = cs_print_reg_info_C0_SR,    },
    { .cop = COP0, .idx = REG_CP0_CAUSE,          .func = cs_print_reg_info_C0_CAUSE, },
    { .cop = FCR,  .idx = REG_FCR_CONTROL_STATUS, .func = cs_print_reg_info_FPR_CSR,  },
    { .cop = SPC,  .idx = REG_SPC_RCP,            .func = cs_print_reg_info_SPC_RCP,  },
};

// Register popup box draw function.
void cs_popup_reginspect_draw(void) {
    const ScreenCoord_s32 bgStartX = (CRASH_SCREEN_X1 + (TEXT_WIDTH(1) / 2));
    const ScreenCoord_s32 bgStartY = (CRASH_SCREEN_Y1 + (TEXT_HEIGHT(1) / 2));
    const ScreenCoord_s32 bgW = (CRASH_SCREEN_W - TEXT_WIDTH(1));
    const ScreenCoord_s32 bgH = (CRASH_SCREEN_H - TEXT_HEIGHT(1));
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
    _Bool is64Bit = (regInfo->size == sizeof(Doubleword));
    _Bool isCOP1 = (regId.cop == COP1); //! TODO: Split hi and lo bits for FGR and label them even/odd + combined.
    Doubleword value = get_reg_val(cop, idx);
    if (isCOP1 && ((regId.idx & 0x1) == 0)) {
        Word cop1OddBits = get_reg_val(COP1, (regId.idx + 1));
        if (cop1OddBits != 0) {
            value = ((HiLo64){ .hi = cop1OddBits, .lo = (Word)value, }).raw;
            is64Bit = TRUE;
        }
    }

    CSTextCoord_u32 line = 1;
    // "[register] REGISTER".
    cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"register on thread %d (%s):", COLOR_RGBA32_CRASH_PAGE_NAME,
        gInspectThread->id, get_thread_name(gInspectThread)
    );
    CSTextCoord_u32 charX = cs_print(TEXT_X(2), TEXT_Y(line), STR_COLOR_PREFIX"\"$%s\"", COLOR_RGBA32_CRASH_VARIABLE, regInfo->name);
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

    // 64 bit value if it exists:
    if (is64Bit) {
        line = cs_popup_reginspect_draw_reg_value(line, regId, value, is64Bit);
    }

    // 32 bit value:
    line = cs_popup_reginspect_draw_reg_value(line, regId, value, FALSE);

    // line++;
    _Bool hasExInfo = FALSE;
    const RegInspectExtraInfo* exInfo = &sRegInspectExtraInfoFuncs[0];
    for (int i = 0; i < ARRAY_COUNT(sRegInspectExtraInfoFuncs); i++) {
        if ((exInfo->cop == cop) && (exInfo->idx == idx)) {
            cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"decoded bits:", COLOR_RGBA32_CRASH_PAGE_NAME);
            hasExInfo = exInfo->func(line, value);
            sInspectedRegisterPtrAddr = 0x00000000;
            break;
        }
        exInfo++;
    }

    if (!hasExInfo) {
        cs_reginspect_pointer(line++, (Word)value);
    }

    cs_draw_outline(bgStartX, bgStartY, bgW, bgH, COLOR_RGBA32_CRASH_DIVIDER);

    osWritebackDCacheAll();
}

void cs_popup_reginspect_input(void) {
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
