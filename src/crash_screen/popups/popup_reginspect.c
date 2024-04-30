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
static const char* sStrEnabled[] = {
    [FALSE] = "disabled",
    [TRUE ] = "enabled",
};
static const char* sStrOnOff[] = {
    [FALSE] = "off",
    [TRUE ] = "on",
};
static const char* sStrSuccess[] = {
    [FALSE] = "fail",
    [TRUE ] = "success",
};
static const char* sStrEndian[] = {
    [0b0] = "little",
    [0b1] = "big",
};
static const char* sStrBitMode[] = {
    [0b0] = "32-bit",
    [0b1] = "64-bit",
};
static const char* sStr_32_64[] = {
    [0b0] = "32",
    [0b1] = "64",
};
static const char* sStr_64_128[] = {
    [0b0] = "64",
    [0b1] = "128",
};
static const char* sStrAuto[] = {
    [0b0] = "manual",
    [0b1] = "auto",
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
static const char* sStr_C0_CONFIG_EM[] = {
    [0b0] = "parity",
    [0b1] = "ecc",
};
static const char* sStr_C0_CONFIG_EB[] = {
    [0b0] = "subblock",
    [0b1] = "sequent",
};
static const char* sStr_FCR31_RoundingMode[] = {
    [0b00] = "nearest",
    [0b01] = "zero",
    [0b10] = "+inf",
    [0b11] = "-inf",
};
static const char* sStr_RDRAM_VERSION[] = {
    [0b0000] = "unknown",
    [0b0001] = "extended",
    [0b0010] = "concurrent",
    [0b0011] = "unknown",
    //! TODO: Can this potentially read out of bounds?
};
static const char* sStr_RDRAM_TYPE[] = {
    [0b0000] = "rdram device",
    //! TODO: Can this potentially read out of bounds?
};
static const char* sStr_XBUS_DMEM[] = {
    [0b0] = "xbus",
    [0b1] = "dmem",
};
static const char* sStr_VI_AA_MODE[] = {
    [0b00] = "aa=1,resample=1,ex=always",
    [0b01] = "aa=1,resample=1,ex=needed",
    [0b10] = "aa=0,resample=1",
    [0b11] = "aa=0,resample=0",
};
static const char* sStr_VI_TYPE[] = {
    [0b00] = "blank",
    [0b01] = "reserved",
    [VI_CTRL_TYPE_16] = "rgba16",
    [VI_CTRL_TYPE_32] = "rgba32",
};
static const char* sStr_RI_MODE_OP[] = {
    [0b00] = "continuous",
    [0b01] = "every 4 BusClk cycles",
    [0b10] = "pre-instruction",
    [0b11] = "unknown",
};
const char** sRegBitsInfoStrings[] = {
    [REG_BITS_INFO_STR_TRUTH   ] = sStrTruth,
    [REG_BITS_INFO_STR_YES_NO  ] = sStrYesNo,
    [REG_BITS_INFO_STR_ENABLED ] = sStrEnabled,
    [REG_BITS_INFO_STR_ON_OFF  ] = sStrOnOff,
    [REG_BITS_INFO_STR_SUCCESS ] = sStrSuccess,
    [REG_BITS_INFO_STR_ENDIAN  ] = sStrEndian,
    [REG_BITS_INFO_STR_BIT_MODE] = sStrBitMode,
    [REG_BITS_INFO_STR_32_64   ] = sStr_32_64,
    [REG_BITS_INFO_STR_64_128  ] = sStr_64_128,
    [REG_BITS_INFO_STR_AUTO    ] = sStrAuto,

    [REG_BITS_INFO_STR_C0_SR_EXEC_MODE] = sStr_C0_SR_ExecMode,
    [REG_BITS_INFO_STR_C0_SR_BEV      ] = sStr_C0_SR_BEV,
    [REG_BITS_INFO_STR_C0_CONFIG_EM   ] = sStr_C0_CONFIG_EM,
    [REG_BITS_INFO_STR_C0_CONFIG_EB   ] = sStr_C0_CONFIG_EB,
    [REG_BITS_INFO_STR_FCR31_ROUNDING_MODE] = sStr_FCR31_RoundingMode,

    [REG_BITS_INFO_STR_RDRAM_VERSION] = sStr_RDRAM_VERSION,
    [REG_BITS_INFO_STR_RDRAM_TYPE   ] = sStr_RDRAM_TYPE,
    [REG_BITS_INFO_STR_XBUS_DMEM    ] = sStr_XBUS_DMEM,
    [REG_BITS_INFO_STR_VI_AA_MODE   ] = sStr_VI_AA_MODE,
    [REG_BITS_INFO_STR_VI_TYPE      ] = sStr_VI_TYPE,
    [REG_BITS_INFO_STR_RI_MODE_OP   ] = sStr_RI_MODE_OP,
};




typedef struct IdNamePairList {
    const IdNamePair* list;
    size_t numEntries;
} IdNamePairList;
#define DEF_BITS_PAIR_LIST(_name, ...)          \
    static const IdNamePair _##_name[] = {      \
        __VA_ARGS__                             \
        ID_LIST_END(),                          \
    };                                          \
    static const IdNamePairList _name = {       \
        .list = _##_name,                       \
        .numEntries = ARRAY_COUNT(_##_name),    \
    }
DEF_BITS_PAIR_LIST(sPairList_sys_clock_ratio,
    { .id = CONFIG_EC_1_1, .name = "1:1",   },
    { .id = CONFIG_EC_3_2, .name = "1.5:1", },
    { .id = CONFIG_EC_2_1, .name = "2:1",   },
    { .id = CONFIG_EC_3_1, .name = "3:1",   },
);
DEF_BITS_PAIR_LIST(sPairList_trans_data_pattern,
    { .id = 0, .name = "cold rst", },
    { .id = 6, .name = "2dw/6c",     },
);
DEF_BITS_PAIR_LIST(sPairList_config_k0,
    { .id = CONFIG_UNCACHED,     .name = "uncached",   },
    { .id = CONFIG_NONCOHRNT,    .name = "non-coh.",   },
    { .id = CONFIG_COHRNT_EXLWR, .name = "coh. exlwr", },
);
const IdNamePairList* sRegBitsInfoIdStringPairs[] = {
    [REG_BITS_INFO_LIST_C0_CONFIG_SYS_CLOCK_RATIO   ] = &sPairList_sys_clock_ratio,
    [REG_BITS_INFO_LIST_C0_CONFIG_TRANS_DATA_PATTERN] = &sPairList_trans_data_pattern,
    [REG_BITS_INFO_LIST_C0_CONFIG_K0                ] = &sPairList_config_k0,
};


static char sRegBitsInfoFuncBuffer[CRASH_SCREEN_NUM_CHARS_X];
void regbits_str_readwrite(char* buf, Word bits) {
    char* p = buf;
    p += sprintf(p, "R:%d/W:%d", (bits >> 1), (bits & 0b1));
}
void regbits_str_C0_cause(char* buf, Word bits) {
    char* p = buf;
    p += sprintf(p, "%s", get_cause_desc_simple(bits));
    if (((bits & CAUSE_EXCMASK) >> CAUSE_EXCSHIFT) == EXC_CPU) {
        p += sprintf(p, " (cop%d)", ((bits & CAUSE_CEMASK) >> CAUSE_CESHIFT));//c.CE);
    }
}
void regbits_str_RDRAM_MODE_CCValue(char* buf, Word bits) {
    char* p = buf;
    p += sprintf(p, "%c %c %c %c %c",
        ('0' + BITFLAG_BOOL(bits, RDRAM_MODE_C5_MASK)),
        ('0' + BITFLAG_BOOL(bits, RDRAM_MODE_C4_MASK)),
        ('0' + BITFLAG_BOOL(bits, RDRAM_MODE_C3_MASK)),
        ('0' + BITFLAG_BOOL(bits, RDRAM_MODE_C2_MASK)),
        ('0' + BITFLAG_BOOL(bits, RDRAM_MODE_C1_MASK)),
        ('0' + BITFLAG_BOOL(bits, RDRAM_MODE_C0_MASK))
    );
}
typedef void (*RegBitsInfoFunc)(char* buf, Word bits);
RegBitsInfoFunc sRegBitsInfoFuncs[] = {
    [REG_BITS_INFO_FUNC_READWRITE         ] = regbits_str_readwrite,
    [REG_BITS_INFO_FUNC_C0_CAUSE          ] = regbits_str_C0_cause,
    [REG_BITS_INFO_FUNC_RDRAM_MODE_CCVALUE] = regbits_str_RDRAM_MODE_CCValue,
};


CSTextCoord_u32 cs_print_reg_info_list(CSTextCoord_u32 line, Word val, const RegBitsInfo* list) {
    const RGBA32 infoColor = COLOR_RGBA32_GRAY;
    ScreenCoord_u32 x = TEXT_X(2);
    CSTextCoord_u32 currLine = line;
    CSTextCoord_u32 descW = ((CRASH_SCREEN_NUM_CHARS_X - 2) / 2);
    CSTextCoord_u32 infoW = 0;

    const RegBitsInfo* info = &list[0];
    while ((info != NULL) && (info->type != REG_BITS_TYPE_END)) {
        //! TODO: This wraps twice (going offscreen) when using REG_BITS_CMD_WRAP().
        // if (currLine >= (CRASH_SCREEN_NUM_CHARS_Y - 1)) {
        //     currLine = line;
        //     x += TEXT_WIDTH(descW + infoW + STRLEN(" "));
        // }

        ScreenCoord_u32 y = TEXT_Y(currLine);
        CSTextCoord_u32 linesPrinted = 1;
        enum RegBitsType type = info->type;
        _Bool hasInfo = (type != REG_BITS_TYPE_NONE);
        const char* name = info->name;
        if (name != NULL) {
            // cs_print(x, y, "%s%c", name, (hasInfo ? ':' : ' '));
            cs_print(x, y, "%s:", name);
            linesPrinted = gCSNumLinesPrinted;
        }
        if (hasInfo) {
            ScreenCoord_u32 x2 = (x + TEXT_WIDTH(descW));
            u8 maskSize = info->maskSize;
            Word mask = BITMASK(maskSize);
            u8 shiftSize = info->shiftSize;
            Word bits = ((val >> shiftSize) & mask);

            switch (type) {
                case REG_BITS_TYPE_BIN: // Print as binary (arg = spacing between chars):
                    //! TODO: combine this with cs_print_as_binary:
                    ScreenCoord_u32 x3 = x2;
                    for (int i = 0; i < maskSize; i++) {
                        char c = (((bits >> ((maskSize - 1) - i)) & 0b1) ? '1' : '0');
                        cs_draw_glyph(x3, y, c, infoColor);
                        x3 += TEXT_WIDTH(info->spacing);
                    }
                    break;
                case REG_BITS_TYPE_HEX: // Print as hexadecimal:
                    cs_print(x2, y, (STR_COLOR_PREFIX STR_HEX_PREFIX"%0*X"), infoColor, info->numDigits, bits);
                    break;
                case REG_BITS_TYPE_DEC: // Print as decimal:
                    cs_print(x2, y, (STR_COLOR_PREFIX"%0*d"), infoColor, info->numDigits, bits);
                    break;
                case REG_BITS_TYPE_STR: // Print from string array:
                    cs_print(x2, y, (STR_COLOR_PREFIX"%s"), infoColor, sRegBitsInfoStrings[info->list][bits]);
                    break;
                case REG_BITS_TYPE_BSTR: // Print from string array (inverted boolean):
                    cs_print(x2, y, (STR_COLOR_PREFIX"%s"), infoColor, sRegBitsInfoStrings[info->list][!bits]);
                    break;
                case REG_BITS_TYPE_ISTR:; // Print from id/string pair array:
                    const IdNamePairList* list = sRegBitsInfoIdStringPairs[info->iList];
                    cs_print(x2, y, (STR_COLOR_PREFIX"%s"), infoColor,
                        str_null_fallback(get_name_from_id_list_impl(bits, list->list, list->numEntries), "?")
                    );
                    break;
                case REG_BITS_TYPE_FUNC: // Print from formatting function:
                    bzero(sRegBitsInfoFuncBuffer, sizeof(sRegBitsInfoFuncBuffer));
                    sRegBitsInfoFuncs[info->func](sRegBitsInfoFuncBuffer, bits);
                    cs_print(x2, y, (STR_COLOR_PREFIX"%s"), infoColor, sRegBitsInfoFuncBuffer);
                    break;
                case REG_BITS_TYPE_SETX: // Set x position of data:
                    linesPrinted = 0;
                    descW = info->xPos;
                    break;
                case REG_BITS_TYPE_SETW: // Set max width of data:
                    linesPrinted = 0;
                    infoW = info->width;
                    break;
                case REG_BITS_TYPE_WRAP: // Wrap the list:
                    linesPrinted = 0;
                    currLine = line;
                    x += TEXT_WIDTH(descW + infoW + STRLEN(" "));
                    break;
                default:
                    break;
            }
        }
        currLine += linesPrinted;
        info++;
    }

    return currLine;
}

#include "reginspect_bits.inc.c"

void cs_reginspect_pointer(CSTextCoord_u32 line, Word val32, _Bool sureAddr) {
    const MapSymbol* symbol = get_map_symbol(val32, SYMBOL_SEARCH_BACKWARD); // Returns NULL if debug map is not included.
    _Bool inSymbol = (symbol != NULL);

    if (inSymbol || sureAddr) {
        const CSTextCoord_u32 maxWidth = (CRASH_SCREEN_NUM_CHARS_X - 4);

        cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"pointer to:", COLOR_RGBA32_CRASH_PAGE_NAME);

        if (inSymbol) {
            cs_print_symbol_name(TEXT_X(2), TEXT_Y(line++), maxWidth, symbol, FALSE);
            cs_print(TEXT_X(2), TEXT_Y(line++),
                (STR_COLOR_PREFIX"+"STR_HEX_HALFWORD" "),
                COLOR_RGBA32_CRASH_OFFSET, (val32 - symbol->addr)
            );
        } else if (sureAddr) {
            cs_print_addr_location_info(TEXT_X(2), TEXT_Y(line++), maxWidth, val32, TRUE);
        }

        sInspectedRegisterPtrAddr = val32;
    }

    Word dataAtAddr = 0x00000000;
    _Bool validAddr = try_read_word_aligned(&dataAtAddr, val32);

    if ((val32 != (Address)NULL) && (validAddr || sureAddr)) {
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
    }
}

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
        Word valHi = HI_OF_64(val64);
        print_data_as_binary(TEXT_X(2 + charX), TEXT_Y(line++), &valHi, sizeof(valHi), valColor);
    }
    print_data_as_binary(TEXT_X(2 + charX), TEXT_Y(line++), &val32, sizeof(val32), valColor);

    // Print as other floatint point formats:
    if (regId.valInfo.type == REG_VAL_TYPE_FLOAT) {
        //! TODO: Combine this with cs_print_f32.
        const IEEE754_f64 flt64 = { .asU64 = val64, };
        const IEEE754_f32 flt32 = { .asU32 = val32, };
        FloatError fltErrType = (is64Bit ? validate_f64(flt64) : validate_f32(flt32));
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

void reginspect_draw_contents(RegisterId regId) {
    enum RegisterSources src = regId.src;
    int idx = regId.idx;

    const RegisterSource* regSrc = get_reg_src(src);
    const RegisterInfo* regInfo = get_reg_info(src, idx);
    if (regInfo == NULL) {
        return;
    }
    _Bool isInterface = (regId.src >= REGS_INTERFACES_START);
    _Bool is64Bit = (!isInterface && regInfo->is64bit);
    _Bool isCP1 = (regId.src == REGS_CP1);
    _Bool isFCR = (regId.src == REGS_FCR);
    _Bool checkThread = regId.valInfo.thr;
    Doubleword value = get_reg_val(src, idx, checkThread);
    Word val32 = (Word)value;
    if (isCP1 && ((regId.idx & 0x1) == 0)) {
        //! TODO: Split hi and lo bits for FGR and label them even/odd + combined.
        Word cop1OddBits = get_reg_val(REGS_CP1, (regId.idx + 1), checkThread);
        if (cop1OddBits != 0) {
            value = HI_LO_64(cop1OddBits, val32);
            is64Bit = TRUE;
        }
    }

    CSTextCoord_u32 line = 1;
    gCSWordWrap = TRUE;
    cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"register:", COLOR_RGBA32_CRASH_PAGE_NAME);
    const char* regDesc = str_null_fallback(get_reg_desc(src, idx), "");
    if (isInterface) {
        cs_print(TEXT_X(2), TEXT_Y(line), STR_COLOR_PREFIX"%s_%s_REG\n (%s)\n in %s - %s", COLOR_RGBA32_VSC_DEFINE, //! TODO: Change this (colon makes it look like a subsection)
            regSrc->name, regInfo->name, regDesc, regSrc->name, regSrc->desc
        );
        line += gCSNumLinesPrinted;
    } else {
        cs_print(TEXT_X(2), TEXT_Y(line), STR_COLOR_PREFIX"\"$%s\" (%s)\n in %s - %s", COLOR_RGBA32_CRASH_VARIABLE,
            regInfo->name, regDesc, regSrc->name, regSrc->desc
        );
        line += gCSNumLinesPrinted;
        if (checkThread && (regInfo->offset != REGINFO_NULL_OFFSET)) {
            cs_print(TEXT_X(2), TEXT_Y(line), STR_COLOR_PREFIX" on thread %d (%s)", COLOR_RGBA32_CRASH_VARIABLE,
                gInspectThread->id, get_thread_name(gInspectThread)
            );
            line += gCSNumLinesPrinted;
        }
    }
    gCSWordWrap = FALSE;

    // 64 bit value if it exists.
    if (is64Bit) {
        line = cs_popup_reginspect_draw_reg_value(line, regId, value, TRUE);
    }

    // 32 bit value. Always print for CP1 registers.
    if (!is64Bit || isCP1) {
        line = cs_popup_reginspect_draw_reg_value(line, regId, value, FALSE);
    }

    // line++;
    _Bool hasExInfo = FALSE;
    const RegInspectExtraInfo* exInfo = &sRegInspectExtraInfoFuncs[0];
    for (int i = 0; i < ARRAY_COUNT(sRegInspectExtraInfoFuncs); i++) {
        if ((exInfo->src == src) && (exInfo->idx == idx)) {
            cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"decoded bits:", COLOR_RGBA32_CRASH_PAGE_NAME);
            cs_print_reg_info_list(line, val32, exInfo->list);
            hasExInfo = TRUE;
            sInspectedRegisterPtrAddr = 0x00000000;
            break;
        }
        exInfo++;
    }

    sInspectedRegisterPtrAddr = 0x00000000; // Reset this in case it doesn't get set later in cs_reginspect_pointer().
    _Bool sureAddr = regInfo->sureAddr;
    if (!isCP1 && !isFCR && !hasExInfo && (sureAddr || is_valid_ram_addr(val32))) {
        cs_reginspect_pointer(line++, val32, sureAddr);
    }
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

    reginspect_draw_contents(gInspectedRegister);

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
