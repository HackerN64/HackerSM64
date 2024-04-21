#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/util/map_parser.h"
#include "crash_screen/util/memory_read.h"
#include "crash_screen/util/registers.h"
#include "crash_screen/util/floats.h"
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
    .src         = REGS_CPU,
    .idx         = REG_CPU_R0,
    .valInfo.raw = 0,
};

Address sInspectedRegisterPtrAddr = 0x00000000;


void cs_popup_reginspect_init(void) {
    sInspectedRegisterPtrAddr = 0x00000000;
}

static const char* sStrTruth[] = {
    [FALSE] = "false",
    [TRUE ] = "true",
};
static const char* sStrYesNo[] = {
    [FALSE] = "no",
    [TRUE ] = "yes",
};
static const char* sStrEnable[] = {
    [FALSE] = "disabled",
    [TRUE ] = "enabled",
};
static const char* sStrOnOff[] = {
    [FALSE] = "off",
    [TRUE ] = "on",
};
static const char* sStrEndian[] = {
    [0] = "big",
    [1] = "little",
};
static const char* sStrBitMode[] = {
    [0] = "32-bit",
    [1] = "64-bit",
};
static const char* sStr_C0_SR_ExecMode[] = {
    [0b00] = "kernel",
    [0b01] = "supervisor",
    [0b10] = "user",
};
static const char* sStr_C0_SR_BEV[] = {
    [0b0] = "normal",
    [0b1] = "bootstrap",
};
static const char* sStr_FCR31_RoundingMode[] = {
    [0b00] = "nearest",
    [0b01] = "zero",
    [0b10] = "+inf",
    [0b11] = "-inf",
};

enum PACKED RegBitsInfoStringLists {
    REG_BITS_INFO_STR_TRUTH,
    REG_BITS_INFO_STR_YES_NO,
    REG_BITS_INFO_STR_ENABLE,
    REG_BITS_INFO_STR_ON_OFF,
    REG_BITS_INFO_STR_ENDIAN,
    REG_BITS_INFO_STR_BIT_MODE,
    REG_BITS_INFO_STR_C0_SR_EXEC_MODE,
    REG_BITS_INFO_STR_C0_SR_BEV,
    REG_BITS_INFO_STR_FCR31_ROUNDING_MODE,
};

const char** sRegBitsInfoStrings[] = {
    [REG_BITS_INFO_STR_TRUTH   ] = sStrTruth,
    [REG_BITS_INFO_STR_YES_NO  ] = sStrYesNo,
    [REG_BITS_INFO_STR_ENABLE  ] = sStrEnable,
    [REG_BITS_INFO_STR_ON_OFF  ] = sStrOnOff,
    [REG_BITS_INFO_STR_ENDIAN  ] = sStrEndian,
    [REG_BITS_INFO_STR_BIT_MODE] = sStrBitMode,
    [REG_BITS_INFO_STR_C0_SR_EXEC_MODE] = sStr_C0_SR_ExecMode,
    [REG_BITS_INFO_STR_C0_SR_BEV      ] = sStr_C0_SR_BEV,
    [REG_BITS_INFO_STR_FCR31_ROUNDING_MODE] = sStr_FCR31_RoundingMode,
};

static char sRegBitsInfoFuncBuffer[8];

enum PACKED RegBitsInfoFuncs {
    REG_BITS_INFO_FUNC_READWRITE,
};

void regbits_str_readwrite(char* buf, u32 bits) {
    char* p = buf;
    bits &= 0b11;
    p += sprintf(p, "R:%d/W:%d", (bits >> 1), (bits & 0b1));
}

typedef void (*RegBitsInfoFunc)(char* buf, u32 bits);
RegBitsInfoFunc sRegBitsInfoFuncs[] = {
    [REG_BITS_INFO_FUNC_READWRITE] = regbits_str_readwrite,
};

enum PACKED RegBitsType {
    REG_BITS_TYPE_NONE,
    REG_BITS_TYPE_HEX,
    REG_BITS_TYPE_DEC,
    REG_BITS_TYPE_STR,
    REG_BITS_TYPE_FUNC,
};
typedef struct RegBitsInfo {
    /*0x00*/ const char* name;
    /*0x04*/ const u8 maskSize;
    /*0x05*/ const u8 shiftSize;
    /*0x06*/ const enum RegBitsType type;
    /*0x07*/ union {
                const u8 numDigits;                     // REG_BITS_TYPE_HEX/REG_BITS_TYPE_DEC
                const enum RegBitsInfoStringLists list; // REG_BITS_TYPE_STR
                const enum RegBitsInfoFuncs func;       // REG_BITS_TYPE_FUNC
            };
} RegBitsInfo; /*0x08*/
#define REG_BITS_INFO_HEX(_name, _mask, _numDigits) {   \
    .name      = _name,                                 \
    .maskSize  = POPCOUNT(_mask),                       \
    .shiftSize = CTZ(_mask),                            \
    .type      = REG_BITS_TYPE_HEX,                     \
    .numDigits = _numDigits,                            \
}
#define REG_BITS_INFO_DEC(_name, _mask, _numDigits) {   \
    .name      = _name,                                 \
    .maskSize  = POPCOUNT(_mask),                       \
    .shiftSize = CTZ(_mask),                            \
    .type      = REG_BITS_TYPE_DEC,                     \
    .numDigits = _numDigits,                            \
}
#define REG_BITS_INFO_STR(_name, _mask, _list) {        \
    .name      = _name,                                 \
    .maskSize  = POPCOUNT(_mask),                       \
    .shiftSize = CTZ(_mask),                            \
    .type      = REG_BITS_TYPE_STR,                     \
    .list      = _list,                                 \
}
#define REG_BITS_INFO_FUNC(_name, _mask, _func) {       \
    .name      = _name,                                 \
    .maskSize  = POPCOUNT(_mask),                       \
    .shiftSize = CTZ(_mask),                            \
    .type      = REG_BITS_TYPE_FUNC,                    \
    .func      = _func,                                 \
}
#define REG_BITS_INFO_NONE(_name)   { .name = _name, .type = REG_BITS_TYPE_NONE, }
#define REG_BITS_INFO_GAP()         REG_BITS_INFO_NONE("")
#define REG_BITS_INFO_END()         REG_BITS_INFO_NONE(NULL)

const RegBitsInfo regBits_C0_SR[] = {
    REG_BITS_INFO_STR("cop1",       SR_CU1,      REG_BITS_INFO_STR_ENABLE),
    REG_BITS_INFO_STR("low power",  SR_RP,       REG_BITS_INFO_STR_ON_OFF),
    REG_BITS_INFO_STR("extra fpr",  SR_FR,       REG_BITS_INFO_STR_ENABLE),
    REG_BITS_INFO_STR("endian",     SR_RE,       REG_BITS_INFO_STR_ENDIAN),
    REG_BITS_INFO_HEX("intr mask",  SR_IMASK,    2),

    REG_BITS_INFO_STR("kernel",     SR_KX,       REG_BITS_INFO_STR_BIT_MODE),
    REG_BITS_INFO_STR("supervisor", SR_SX,       REG_BITS_INFO_STR_BIT_MODE),
    REG_BITS_INFO_STR("user",       SR_UX,       REG_BITS_INFO_STR_BIT_MODE),
    REG_BITS_INFO_STR("exec mode",  SR_KSU_MASK, REG_BITS_INFO_STR_C0_SR_EXEC_MODE),
    REG_BITS_INFO_STR("error",      SR_ERL,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_INFO_STR("exception",  SR_EXL,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_INFO_STR("glbl intr",  SR_IE,       REG_BITS_INFO_STR_ENABLE),

    REG_BITS_INFO_STR("insn trace", SR_ITS,      REG_BITS_INFO_STR_ENABLE),
    REG_BITS_INFO_STR("bs exc vec", SR_BEV,      REG_BITS_INFO_STR_C0_SR_BEV),
    REG_BITS_INFO_STR("tlb down",   SR_TS,       REG_BITS_INFO_STR_YES_NO),
    REG_BITS_INFO_STR("soft reset", SR_SR,       REG_BITS_INFO_STR_YES_NO),
    REG_BITS_INFO_STR("cp0 cond",   SR_CH,       REG_BITS_INFO_STR_TRUTH),

    REG_BITS_INFO_END(),
};

CSTextCoord_u32 cs_print_reg_info_list(CSTextCoord_u32 line, CSTextCoord_u32 descW, CSTextCoord_u32 infoW, Word val, const RegBitsInfo* list) {
    const RGBA32 infoColor = COLOR_RGBA32_GRAY;
    ScreenCoord_u32 x = TEXT_X(2);
    CSTextCoord_u32 currLine = line;

    descW += STRLEN(": ");

    const RegBitsInfo* info = &list[0];
    while (info->name != NULL) {
        if (currLine >= (CRASH_SCREEN_NUM_CHARS_Y - 1)) {
            currLine = line;
            x += TEXT_WIDTH(descW + infoW + STRLEN(" "));
        }

        ScreenCoord_u32 y = TEXT_Y(currLine);
        const char* name = info->name;
        if (name[0] != '\0') {
            cs_print(x, y, "%s:", info->name);
        }
        enum RegBitsType type = info->type;
        if (type != REG_BITS_TYPE_NONE) {
            ScreenCoord_u32 x2 = (x + TEXT_WIDTH(descW));
            Word mask = BITMASK(info->maskSize);
            Word bits = ((val >> info->shiftSize) & mask);

            switch (type) {
                case REG_BITS_TYPE_HEX:
                    cs_print(x2, y, (STR_COLOR_PREFIX STR_HEX_PREFIX"%0*X"), infoColor, info->numDigits, bits);
                    break;
                case REG_BITS_TYPE_DEC:
                    cs_print(x2, y, (STR_COLOR_PREFIX"%0*d"), infoColor, info->numDigits, bits);
                    break;
                case REG_BITS_TYPE_STR:
                    cs_print(x2, y, (STR_COLOR_PREFIX"%s"), infoColor, sRegBitsInfoStrings[info->list][bits]);
                    break;
                case REG_BITS_TYPE_FUNC:
                    bzero(sRegBitsInfoFuncBuffer, sizeof(sRegBitsInfoFuncBuffer));
                    sRegBitsInfoFuncs[info->func](sRegBitsInfoFuncBuffer, bits);
                    cs_print(x2, y, (STR_COLOR_PREFIX"%s"), infoColor, sRegBitsInfoFuncBuffer);
                    break;
                default:
                    break;
            }
        }
        currLine++;
        info++;
    }

    return currLine;
}

void cs_print_reg_info_C0_SR(CSTextCoord_u32 line, Doubleword val) {
    cs_print_reg_info_list(line, STRLEN("xxxxxxxxxx"), STRLEN("xxxxxxxx"), (Word)val, regBits_C0_SR);
}

const RegBitsInfo regBits_C0_CAUSE[] = {
    REG_BITS_INFO_STR("is branch delay slot", CAUSE_BD, REG_BITS_INFO_STR_YES_NO),
    REG_BITS_INFO_GAP(),
    REG_BITS_INFO_NONE("interrupts pending"),
    REG_BITS_INFO_DEC( "  Timer",       CAUSE_IP8, 1),
    REG_BITS_INFO_FUNC("  Indy RDB",    (CAUSE_IP7 | CAUSE_IP6), REG_BITS_INFO_FUNC_READWRITE),
    REG_BITS_INFO_DEC( "  Reset (NMI)", CAUSE_IP5, 1),
    REG_BITS_INFO_DEC( "  Cartridge",   CAUSE_IP4, 1),
    REG_BITS_INFO_DEC( "  MIPS",        CAUSE_IP3, 1),
    REG_BITS_INFO_DEC( "  Software",    (CAUSE_SW2 | CAUSE_SW1), 1),
    REG_BITS_INFO_GAP(),
    REG_BITS_INFO_DEC("exc code", CAUSE_EXCMASK, 2),

    REG_BITS_INFO_END(),
};

void cs_print_reg_info_C0_CAUSE(CSTextCoord_u32 line, Doubleword val) {
    line = cs_print_reg_info_list(line, STRLEN("is branch delay slot"), 0, (Word)val, regBits_C0_CAUSE);

    const RGBA32 infoColor = COLOR_RGBA32_GRAY;
    const Reg_CP0_Cause c = {
        .raw = (Word)val,
    };

    CSTextCoord_u32 charX = 4;
    const char* causeDesc = get_cause_desc(&gInspectThread->context, FALSE);
    if (causeDesc != NULL) {
        charX += cs_print(TEXT_X(charX), TEXT_Y(line), STR_COLOR_PREFIX"%s", infoColor, causeDesc);
    }
    if (c.Exc_Code == (EXC_CPU >> CAUSE_EXCSHIFT)) {
        cs_print(TEXT_X(charX), TEXT_Y(line++), " "STR_COLOR_PREFIX"(cop%d)", infoColor, c.CE);
    }
}

const RegBitsInfo regBits_FPR_CSR[] = {
    REG_BITS_INFO_STR("flush denorms to zero", FPCSR_FS,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_INFO_STR("condition bit",         FPCSR_C,       REG_BITS_INFO_STR_TRUTH),
    REG_BITS_INFO_STR("rounding mode",         FPCSR_RM_MASK, REG_BITS_INFO_STR_FCR31_ROUNDING_MODE),
    REG_BITS_INFO_END(),
};

void cs_print_reg_info_FPR_CSR(CSTextCoord_u32 line, Doubleword val) {
    const ScreenCoord_u32 x1 = TEXT_X(2);
    const ScreenCoord_u32 x2 = TEXT_X(4);
    const ScreenCoord_u32 x3 = TEXT_X(12);
    const RGBA32 infoColor = COLOR_RGBA32_GRAY;
    const Reg_FPR_31 f = {
        .raw = (Word)val,
    };

    line = cs_print_reg_info_list(line, STRLEN("flush denorms to zero: "), 0, (Word)val, regBits_FPR_CSR);
    line++;
    cs_print(x1, TEXT_Y(line++), "exception bits:");
    cs_print(x2, TEXT_Y(line++), "E:unimpl   | V:invalid   | Z:div0");
    cs_print(x2, TEXT_Y(line++), "O:overflow | U:underflow | I:inexact");
    line++;
    cs_print(x3, TEXT_Y(line++), "       E V Z O U I");
    cs_print(x3, TEXT_Y(line++), "cause: "STR_COLOR_PREFIX"%d %d %d %d %d %d", infoColor, f.cause_.E, f.cause_.V,  f.cause_.Z,  f.cause_.O,  f.cause_.U,  f.cause_.I );
    cs_print(x3, TEXT_Y(line++), "enable:  "STR_COLOR_PREFIX"%d %d %d %d %d",  infoColor,             f.enable_.V, f.enable_.Z, f.enable_.O, f.enable_.U, f.enable_.I);
    cs_print(x3, TEXT_Y(line++), "flags:   "STR_COLOR_PREFIX"%d %d %d %d %d",  infoColor,             f.flag_.V,   f.flag_.Z,   f.flag_.O,   f.flag_.U,   f.flag_.I  );
}

const RegBitsInfo regBits_SPC_RCP[] = {
    REG_BITS_INFO_HEX("interrupt mask", (RCP_IMASK >> RCP_IMASKSHIFT), 2),
    REG_BITS_INFO_END(),
};

void cs_print_reg_info_SPC_RCP(CSTextCoord_u32 line, Doubleword val) {
    cs_print_reg_info_list(line, STRLEN("interrupt mask"), 0, (Word)val, regBits_SPC_RCP);
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
    if (try_read_word_aligned(&dataAtAddr, val32)) {
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
    /*0x00*/ const enum RegisterSources src;
    /*0x01*/ const s8 idx;
    /*0x03*/ const u8 pad[2];
    /*0x04*/ void (*func)(CSTextCoord_u32 line, Doubleword val);
} RegInspectExtraInfo; /*0x08*/
const RegInspectExtraInfo sRegInspectExtraInfoFuncs[] = {
    { .src = REGS_CP0, .idx = REG_CP0_SR,             .func = cs_print_reg_info_C0_SR,    },
    { .src = REGS_CP0, .idx = REG_CP0_CAUSE,          .func = cs_print_reg_info_C0_CAUSE, },
    { .src = REGS_FCR, .idx = REG_FCR_CONTROL_STATUS, .func = cs_print_reg_info_FPR_CSR,  },
    { .src = REGS_SPC, .idx = REG_SPC_RCP,            .func = cs_print_reg_info_SPC_RCP,  },
};

CSTextCoord_u32 cs_popup_reginspect_draw_reg_value(CSTextCoord_u32 line, RegisterId regId, const Doubleword val64, _Bool is64Bit) {
    cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"%d-bit value:", COLOR_RGBA32_CRASH_PAGE_NAME, (is64Bit ? 64 : 32));

    Word val32 = (Word)val64;

    CS_SET_DEFAULT_PRINT_COLOR_START(COLOR_RGBA32_CRASH_HEADER);
    const RGBA32 valColor = COLOR_RGBA32_WHITE;

    // Print as hex:
    if (is64Bit) {
        cs_print(TEXT_X(2), TEXT_Y(line++), ("hex: "STR_COLOR_PREFIX STR_HEX_PREFIX STR_HEX_LONG), valColor, val64);
    } else {
        cs_print(TEXT_X(2), TEXT_Y(line++), ("hex: "STR_COLOR_PREFIX STR_HEX_PREFIX STR_HEX_WORD), valColor, val32);
    }
    CSTextCoord_u32 charX = cs_print(TEXT_X(2), TEXT_Y(line), "bin: ");
    if (is64Bit) {
        Word valHi = ((HiLo64){ .raw = val64 }).hi;
        print_data_as_binary(TEXT_X(2 + charX), TEXT_Y(line++), &valHi, sizeof(valHi), valColor);
    }
    print_data_as_binary(TEXT_X(2 + charX), TEXT_Y(line++), &val32, sizeof(val32), valColor);

    // Print as other floatint point formats:
    if (regId.valInfo.type == REG_VAL_TYPE_FLOAT) {
        //! TODO: Combine this with cs_print_f32.
        const IEEE754_f64 flt64 = { .asU64 = val64, };
        const IEEE754_f32 flt32 = { .asU32 = val32, };
        enum FloatErrorType fltErrType = (is64Bit ? validate_f64(flt64) : validate_f32(flt32));
        if (fltErrType != FLT_ERR_NONE) {
            cs_print(TEXT_X(2), TEXT_Y(line++), (STR_COLOR_PREFIX"%s"), COLOR_RGBA32_LIGHT_RED, ((fltErrType == FLT_ERR_DENORM) ? "denormalized" : "NaN"));
        } else {
            cs_print(TEXT_X(2), TEXT_Y(line++), ("dec:"STR_COLOR_PREFIX"% g"), valColor, (is64Bit ? flt64.asF64 : flt32.asF32));
            cs_print(TEXT_X(2), TEXT_Y(line++), ("sci:"STR_COLOR_PREFIX"% e"), valColor, (is64Bit ? flt64.asF64 : flt32.asF32));
        }
    }

    CS_SET_DEFAULT_PRINT_COLOR_END();

    return line;
}

void reginspect_draw_for_thread(RegisterId regId) {
    enum RegisterSources src = regId.src;
    int idx = regId.idx;

    const RegisterSource* regSrc = get_reg_src(src);
    const RegisterInfo* regInfo = get_reg_info(src, idx);
    if (regInfo == NULL) {
        return;
    }
    _Bool is64Bit = (regInfo->is64bit);
    _Bool isCP1 = (regId.src == REGS_CP1); //! TODO: Split hi and lo bits for FGR and label them even/odd + combined.
    _Bool isFCR = (regId.src == REGS_FCR);
    Doubleword value = get_reg_val(src, idx);
    Word val32 = (Word)value;
    if (isCP1 && ((regId.idx & 0x1) == 0)) {
        Word cop1OddBits = get_reg_val(REGS_CP1, (regId.idx + 1));
        if (cop1OddBits != 0) {
            value = ((HiLo64){ .hi = cop1OddBits, .lo = val32, }).raw;
            is64Bit = TRUE;
        }
    }

    CSTextCoord_u32 line = 1;
    // "[register] REGISTER".
    cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"register on thread %d (%s):", COLOR_RGBA32_CRASH_PAGE_NAME,
        gInspectThread->id, get_thread_name(gInspectThread)
    );
    CSTextCoord_u32 charX = cs_print(TEXT_X(2), TEXT_Y(line), STR_COLOR_PREFIX"\"$%s\"", COLOR_RGBA32_CRASH_VARIABLE, regInfo->name);
    const char* copName = regSrc->name;
    if (copName != NULL) {
        cs_print(TEXT_X(2 + charX), TEXT_Y(line), STR_COLOR_PREFIX" in %s", COLOR_RGBA32_CRASH_VARIABLE, copName);
    }
    line++;
    const char* regDesc = get_reg_desc(src, idx);
    if (regDesc != NULL) {
        cs_print(TEXT_X(2), TEXT_Y(line++), STR_COLOR_PREFIX"(%s)", COLOR_RGBA32_CRASH_VARIABLE, regDesc);
    }
    line++;

    // 64 bit value if it exists:
    if (is64Bit) {
        line = cs_popup_reginspect_draw_reg_value(line, regId, value, is64Bit);
    }

    if (!is64Bit || isCP1) {
        // 32 bit value:
        line = cs_popup_reginspect_draw_reg_value(line, regId, value, FALSE);
    }

    // line++;
    _Bool hasExInfo = FALSE;
    const RegInspectExtraInfo* exInfo = &sRegInspectExtraInfoFuncs[0];
    for (int i = 0; i < ARRAY_COUNT(sRegInspectExtraInfoFuncs); i++) {
        if ((exInfo->src == src) && (exInfo->idx == idx)) {
            cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"decoded bits:", COLOR_RGBA32_CRASH_PAGE_NAME);
            exInfo->func(line, value);
            hasExInfo = TRUE;
            sInspectedRegisterPtrAddr = 0x00000000;
            break;
        }
        exInfo++;
    }

    if (!isCP1 && !isFCR && !hasExInfo && is_valid_ram_addr(val32)) {
        cs_reginspect_pointer(line++, val32);
    }
}

void reginspect_draw_for_interface(UNUSED RegisterId regId) {
    // InterfaceReg* regInfo = get_interface_reg_info(regId.src, regId.idx);
    // Word data = 0x00000000;
    // if (try_read_word_aligned(&data, regInfo->addr)) {

    // }
}

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

    if (regId.src >= REGS_INTERFACES_START) {
        reginspect_draw_for_interface(regId);
    } else {
        reginspect_draw_for_thread(regId);
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
        .allowPageInput  = TRUE,
        .allowChangePage = FALSE,
    },
};
