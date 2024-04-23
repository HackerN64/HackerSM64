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
static const char* sStrAuto[] = {
    [0] = "manual",
    [1] = "auto",
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
    [0] = "xbus",
    [1] = "dmem",
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

enum PACKED RegBitsInfoStringLists {
    REG_BITS_INFO_STR_TRUTH,
    REG_BITS_INFO_STR_YES_NO,
    REG_BITS_INFO_STR_ENABLE,
    REG_BITS_INFO_STR_ON_OFF,
    REG_BITS_INFO_STR_ENDIAN,
    REG_BITS_INFO_STR_BIT_MODE,
    REG_BITS_INFO_STR_AUTO,
    REG_BITS_INFO_STR_C0_SR_EXEC_MODE,
    REG_BITS_INFO_STR_C0_SR_BEV,
    REG_BITS_INFO_STR_FCR31_ROUNDING_MODE,

    REG_BITS_INFO_STR_RDRAM_VERSION,
    REG_BITS_INFO_STR_RDRAM_TYPE,
    REG_BITS_INFO_STR_XBUS_DMEM,
    REG_BITS_INFO_STR_VI_AA_MODE,
    REG_BITS_INFO_STR_VI_TYPE,
    REG_BITS_INFO_STR_RI_MODE_OP,
};

const char** sRegBitsInfoStrings[] = {
    [REG_BITS_INFO_STR_TRUTH   ] = sStrTruth,
    [REG_BITS_INFO_STR_YES_NO  ] = sStrYesNo,
    [REG_BITS_INFO_STR_ENABLE  ] = sStrEnable,
    [REG_BITS_INFO_STR_ON_OFF  ] = sStrOnOff,
    [REG_BITS_INFO_STR_ENDIAN  ] = sStrEndian,
    [REG_BITS_INFO_STR_BIT_MODE] = sStrBitMode,
    [REG_BITS_INFO_STR_AUTO    ] = sStrAuto,

    [REG_BITS_INFO_STR_C0_SR_EXEC_MODE] = sStr_C0_SR_ExecMode,
    [REG_BITS_INFO_STR_C0_SR_BEV      ] = sStr_C0_SR_BEV,
    [REG_BITS_INFO_STR_FCR31_ROUNDING_MODE] = sStr_FCR31_RoundingMode,

    [REG_BITS_INFO_STR_RDRAM_VERSION] = sStr_RDRAM_VERSION,
    [REG_BITS_INFO_STR_RDRAM_TYPE   ] = sStr_RDRAM_TYPE,
    [REG_BITS_INFO_STR_XBUS_DMEM    ] = sStr_XBUS_DMEM,
    [REG_BITS_INFO_STR_VI_AA_MODE   ] = sStr_VI_AA_MODE,
    [REG_BITS_INFO_STR_VI_TYPE      ] = sStr_VI_TYPE,
    [REG_BITS_INFO_STR_RI_MODE_OP   ] = sStr_RI_MODE_OP,
};

static char sRegBitsInfoFuncBuffer[CRASH_SCREEN_NUM_CHARS_X];

enum PACKED RegBitsInfoFuncs {
    REG_BITS_INFO_FUNC_READWRITE,
    REG_BITS_INFO_FUNC_CAUSE,
    REG_BITS_INFO_FUNC_RDRAM_MODE_CCVALUE,
};

void regbits_str_readwrite(char* buf, Word bits) {
    char* p = buf;
    p += sprintf(p, "R:%d/W:%d", (bits >> 1), (bits & 0b1));
}

void regbits_str_cause(char* buf, Word bits) {
    char* p = buf;
    p += sprintf(p, "%s", get_cause_desc_simple(bits));
    if (((bits & CAUSE_EXCMASK) >> CAUSE_EXCSHIFT) == EXC_CPU) {
        p += sprintf(p, " (cop%d)", ((bits & CAUSE_CEMASK) >> CAUSE_CESHIFT));//c.CE);
    }
}

#define RDRAM_MODE_CE_MASK 0x80000000
#define RDRAM_MODE_X2_MASK 0x40000000
#define RDRAM_MODE_PL_MASK 0x20000000
#define RDRAM_MODE_SV_MASK 0x10000000 // always 0
#define RDRAM_MODE_SK_MASK 0x08000000 // always 0
#define RDRAM_MODE_AS_MASK 0x04000000 // always 1
#define RDRAM_MODE_DE_MASK 0x02000000
#define RDRAM_MODE_LE_MASK 0x01000000
#define RDRAM_MODE_AD_MASK 0x00080000
#define RDRAM_MODE_C5_MASK 0x00800000
#define RDRAM_MODE_C4_MASK 0x00008000
#define RDRAM_MODE_C3_MASK 0x00000080
#define RDRAM_MODE_C2_MASK 0x00400000
#define RDRAM_MODE_C1_MASK 0x00004000
#define RDRAM_MODE_C0_MASK 0x00000040
#define RDRAM_MODE_CC_MASK (RDRAM_MODE_C5_MASK | RDRAM_MODE_C4_MASK | RDRAM_MODE_C3_MASK | RDRAM_MODE_C2_MASK | RDRAM_MODE_C1_MASK | RDRAM_MODE_C0_MASK) // 0x00C0C0C0

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
    [REG_BITS_INFO_FUNC_READWRITE] = regbits_str_readwrite,
    [REG_BITS_INFO_FUNC_CAUSE    ] = regbits_str_cause,

    [REG_BITS_INFO_FUNC_RDRAM_MODE_CCVALUE] = regbits_str_RDRAM_MODE_CCValue,
};

enum PACKED RegBitsType {
    REG_BITS_TYPE_END = -1,
    REG_BITS_TYPE_NONE,
    REG_BITS_TYPE_BIN,
    REG_BITS_TYPE_HEX,
    REG_BITS_TYPE_DEC,
    REG_BITS_TYPE_STR,
    REG_BITS_TYPE_FUNC,
    REG_BITS_TYPE_SETX, // Set info start X.
    REG_BITS_TYPE_SETW, // Set info width (for wrapping).
    REG_BITS_TYPE_WRAP,
};
typedef struct RegBitsInfo {
    /*0x00*/ const char* name;
    /*0x04*/ const u8 maskSize;
    /*0x05*/ const u8 shiftSize;
    /*0x06*/ const enum RegBitsType type;
    /*0x07*/ union {
                const u8 arg;
                const u8 spacing;                       // REG_BITS_TYPE_BIN
                const u8 numDigits;                     // REG_BITS_TYPE_HEX/REG_BITS_TYPE_DEC
                const enum RegBitsInfoStringLists list; // REG_BITS_TYPE_STR
                const enum RegBitsInfoFuncs func;       // REG_BITS_TYPE_FUNC
                const u8 xPos;                          // REG_BITS_TYPE_SETX
                const u8 width;                         // REG_BITS_TYPE_SETW
            };
} RegBitsInfo; /*0x08*/
#define REG_BITS_INFO(_name, _mask, _type, _arg) {  \
    .name      = _name,                             \
    .maskSize  = POPCOUNT(_mask),                   \
    .shiftSize = CTZ(_mask),                        \
    .type      = _type,                             \
    .arg       = _arg,                              \
}
#define REG_BITS_CMD_BIN(_name, _mask, _spacing)   REG_BITS_INFO(_name, _mask, REG_BITS_TYPE_BIN,  _spacing)
#define REG_BITS_CMD_HEX(_name, _mask, _numDigits) REG_BITS_INFO(_name, _mask, REG_BITS_TYPE_HEX,  _numDigits)
#define REG_BITS_CMD_DEC(_name, _mask, _numDigits) REG_BITS_INFO(_name, _mask, REG_BITS_TYPE_DEC,  _numDigits)
#define REG_BITS_CMD_STR(_name, _mask, _list)      REG_BITS_INFO(_name, _mask, REG_BITS_TYPE_STR,  _list)
#define REG_BITS_CMD_FUNC(_name, _mask, _func)     REG_BITS_INFO(_name, _mask, REG_BITS_TYPE_FUNC, _func)
#define REG_BITS_CMD_SETX(_x)                      REG_BITS_INFO(NULL,  0,     REG_BITS_TYPE_SETX, _x)
#define REG_BITS_CMD_SETW(_width)                  REG_BITS_INFO(NULL,  0,     REG_BITS_TYPE_SETW, _width)
#define REG_BITS_CMD_WRAP()                        REG_BITS_INFO(NULL,  0,     REG_BITS_TYPE_WRAP, 0)
#define REG_BITS_CMD_NONE(_name)                   REG_BITS_INFO(_name, 0,     REG_BITS_TYPE_NONE, 0)
#define REG_BITS_CMD_GAP()                         REG_BITS_INFO(NULL,  0,     REG_BITS_TYPE_NONE, 0)
#define REG_BITS_CMD_END()                         REG_BITS_INFO(NULL,  0,     REG_BITS_TYPE_END,  0)


CSTextCoord_u32 cs_print_reg_info_list(CSTextCoord_u32 line, Word val, const RegBitsInfo* list) {
    const RGBA32 infoColor = COLOR_RGBA32_GRAY;
    ScreenCoord_u32 x = TEXT_X(2);
    CSTextCoord_u32 currLine = line;
    CSTextCoord_u32 descW = ((CRASH_SCREEN_NUM_CHARS_X - 2) / 2);
    CSTextCoord_u32 infoW = 0;

    const RegBitsInfo* info = &list[0];
    while ((info != NULL) && (info->type != REG_BITS_TYPE_END)) {
        if (currLine >= (CRASH_SCREEN_NUM_CHARS_Y - 1)) {
            currLine = line;
            x += TEXT_WIDTH(descW + infoW + STRLEN(" "));
        }

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
                case REG_BITS_TYPE_BIN:
                    //! TODO: combine this with cs_print_as_binary:
                    ScreenCoord_u32 x3 = x2;
                    for (int i = 0; i < maskSize; i++) {
                        char c = (((bits >> ((maskSize - 1) - i)) & 0b1) ? '1' : '0');
                        cs_draw_glyph(x3, y, c, infoColor);
                        x3 += TEXT_WIDTH(info->spacing);
                    }
                    break;
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
                case REG_BITS_TYPE_SETX:
                    linesPrinted = 0;
                    descW = info->xPos;
                    break;
                case REG_BITS_TYPE_SETW:
                    linesPrinted = 0;
                    infoW = info->width;
                    break;
                case REG_BITS_TYPE_WRAP:
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

const RegBitsInfo regBits_C0_SR[] = {
    REG_BITS_CMD_SETX(STRLEN("xxxxxxxxxx: ")),
    REG_BITS_CMD_SETW(STRLEN("xxxxxxxx")),
    REG_BITS_CMD_STR("cop1",       SR_CU1,      REG_BITS_INFO_STR_ENABLE),
    REG_BITS_CMD_STR("low power",  SR_RP,       REG_BITS_INFO_STR_ON_OFF),
    REG_BITS_CMD_STR("extra fpr",  SR_FR,       REG_BITS_INFO_STR_ENABLE),
    REG_BITS_CMD_STR("endian",     SR_RE,       REG_BITS_INFO_STR_ENDIAN),
    REG_BITS_CMD_HEX("intr mask",  SR_IMASK,    2),

    REG_BITS_CMD_STR("kernel",     SR_KX,       REG_BITS_INFO_STR_BIT_MODE),
    REG_BITS_CMD_STR("supervisor", SR_SX,       REG_BITS_INFO_STR_BIT_MODE),
    REG_BITS_CMD_STR("user",       SR_UX,       REG_BITS_INFO_STR_BIT_MODE),
    REG_BITS_CMD_STR("exec mode",  SR_KSU_MASK, REG_BITS_INFO_STR_C0_SR_EXEC_MODE),
    REG_BITS_CMD_STR("error",      SR_ERL,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("exception",  SR_EXL,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("glbl intr",  SR_IE,       REG_BITS_INFO_STR_ENABLE),

    REG_BITS_CMD_STR("insn trace", SR_ITS,      REG_BITS_INFO_STR_ENABLE),
    REG_BITS_CMD_STR("bs exc vec", SR_BEV,      REG_BITS_INFO_STR_C0_SR_BEV),
    REG_BITS_CMD_STR("tlb down",   SR_TS,       REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("soft reset", SR_SR,       REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("cp0 cond",   SR_CH,       REG_BITS_INFO_STR_TRUTH),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_C0_CAUSE[] = {
    REG_BITS_CMD_SETX(STRLEN("is branch delay slot: ")),
    REG_BITS_CMD_STR("is branch delay slot", CAUSE_BD, REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_GAP(),
    REG_BITS_CMD_NONE("interrupts pending"),
    REG_BITS_CMD_DEC( "  Timer",       CAUSE_IP8, 1),
    REG_BITS_CMD_FUNC("  Indy RDB",    (CAUSE_IP7 | CAUSE_IP6), REG_BITS_INFO_FUNC_READWRITE),
    REG_BITS_CMD_DEC( "  Reset (NMI)", CAUSE_IP5, 1),
    REG_BITS_CMD_DEC( "  Cartridge",   CAUSE_IP4, 1),
    REG_BITS_CMD_DEC( "  MIPS",        CAUSE_IP3, 1),
    REG_BITS_CMD_DEC( "  Software",    (CAUSE_SW2 | CAUSE_SW1), 1),
    REG_BITS_CMD_GAP(),
    REG_BITS_CMD_DEC("exc code", CAUSE_EXCMASK, 2),
    REG_BITS_CMD_SETX(2),
    REG_BITS_CMD_FUNC(NULL, BITMASK(32), REG_BITS_INFO_FUNC_CAUSE),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_FPR_CSR[] = {
    REG_BITS_CMD_SETX(STRLEN("flush denorms to zero: ")),
    REG_BITS_CMD_STR("flush denorms to zero", FPCSR_FS,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("condition bit",         FPCSR_C,       REG_BITS_INFO_STR_TRUTH),
    REG_BITS_CMD_STR("rounding mode",         FPCSR_RM_MASK, REG_BITS_INFO_STR_FCR31_ROUNDING_MODE),
    REG_BITS_CMD_NONE("\nexception bits:\n  E:unimpl   | V:invalid   | Z:div0\n  O:overflow | U:underflow | I:inexact\n\n                 E V Z O U I"),
    REG_BITS_CMD_SETX(17),
    REG_BITS_CMD_BIN("          cause",  (FPCSR_CE | FPCSR_CV | FPCSR_CZ | FPCSR_CO | FPCSR_CU | FPCSR_CI), 2),
    REG_BITS_CMD_SETX(19),
    REG_BITS_CMD_BIN("          enable",            (FPCSR_EV | FPCSR_EZ | FPCSR_EO | FPCSR_EU | FPCSR_EI), 2),
    REG_BITS_CMD_BIN("          flags",             (FPCSR_FV | FPCSR_FZ | FPCSR_FO | FPCSR_FU | FPCSR_FI), 2),
    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_SPC_RCP[] = {
    REG_BITS_CMD_SETX(STRLEN("interrupt mask: ")),
    REG_BITS_CMD_HEX("interrupt mask", (RCP_IMASK >> RCP_IMASKSHIFT), 2),

    REG_BITS_CMD_END(),
};

#define RDRAM_CONFIG_COLUMN_BITS    0xF0000000
#define RDRAM_CONFIG_BN             0x04000000
#define RDRAM_CONFIG_EN             0x01000000
#define RDRAM_CONFIG_BANK_BITS      0x00F00000
#define RDRAM_CONFIG_ROW_BITS       0x000F0000
#define RDRAM_CONFIG_VERSION        0x000000F0
#define RDRAM_CONFIG_TYPE           0x0000000F

const RegBitsInfo regBits_RDRAM_CONFIG[] = {
    REG_BITS_CMD_STR("9 bits per byte", RDRAM_CONFIG_BN,          REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("Low latency",     RDRAM_CONFIG_EN,          REG_BITS_INFO_STR_ENABLE),
    REG_BITS_CMD_DEC("column bits",     RDRAM_CONFIG_COLUMN_BITS, 2),
    REG_BITS_CMD_DEC("bank bits",       RDRAM_CONFIG_BANK_BITS,   2),
    REG_BITS_CMD_DEC("row bits",        RDRAM_CONFIG_ROW_BITS,    2),
    REG_BITS_CMD_STR("version",         RDRAM_CONFIG_VERSION,     REG_BITS_INFO_STR_RDRAM_VERSION),
    REG_BITS_CMD_STR("device type",     RDRAM_CONFIG_TYPE,        REG_BITS_INFO_STR_RDRAM_TYPE),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_RDRAM_MODE[] = {
    REG_BITS_CMD_SETX(STRLEN("Select PowerDown latency: ")),
    REG_BITS_CMD_STR("CCEnable",                 RDRAM_MODE_CE_MASK, REG_BITS_INFO_STR_AUTO),
    REG_BITS_CMD_DEC("CCMult",                   RDRAM_MODE_X2_MASK, 1),
    REG_BITS_CMD_DEC("Select PowerDown Latency", RDRAM_MODE_PL_MASK, 1),
    REG_BITS_CMD_STR("RDRAM device",             RDRAM_MODE_DE_MASK, REG_BITS_INFO_STR_ENABLE),
    REG_BITS_CMD_STR("PowerDown",                RDRAM_MODE_LE_MASK, REG_BITS_INFO_STR_ENABLE),
    REG_BITS_CMD_DEC("AckDis",                   RDRAM_MODE_AD_MASK, 1),
    REG_BITS_CMD_FUNC("CCValue",                 BITMASK(32),        REG_BITS_INFO_FUNC_RDRAM_MODE_CCVALUE),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_SP_STATUS[] = {
    REG_BITS_CMD_SETX(STRLEN("intr. on break: ")),
    REG_BITS_CMD_SETW(STRLEN("yes")),
    REG_BITS_CMD_STR("halt",           SP_STATUS_HALT,       REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("broke",          SP_STATUS_BROKE,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("dma busy",       SP_STATUS_DMA_BUSY,   REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("dma full",       SP_STATUS_DMA_FULL,   REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("io full",        SP_STATUS_IO_FULL,    REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("single step",    SP_STATUS_SSTEP,      REG_BITS_INFO_STR_ON_OFF),
    REG_BITS_CMD_STR("intr. on break", SP_STATUS_INTR_BREAK, REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_WRAP(),
    REG_BITS_CMD_SETX(STRLEN("sigN (xxx signal): ")),
    REG_BITS_CMD_DEC("sig0 (yield)",      SP_STATUS_YIELD,     1),
    REG_BITS_CMD_DEC("sig1 (yielded)",    SP_STATUS_YIELDED,   1),
    REG_BITS_CMD_DEC("sig2 (task done)",  SP_STATUS_TASKDONE,  1),
    REG_BITS_CMD_DEC("sig3 (rsp signal)", SP_STATUS_RSPSIGNAL, 1),
    REG_BITS_CMD_DEC("sig4 (cpu signal)", SP_STATUS_CPUSIGNAL, 1),
    REG_BITS_CMD_DEC("sig5",              SP_STATUS_SIG5,      1),
    REG_BITS_CMD_DEC("sig6",              SP_STATUS_SIG6,      1),
    REG_BITS_CMD_DEC("sig7",              SP_STATUS_SIG7,      1),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_DPC_STATUS[] = {
    REG_BITS_CMD_SETX(STRLEN("transfer src: ")),
    REG_BITS_CMD_STR("start valid",  DPC_STATUS_START_VALID,   REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("end valid",    DPC_STATUS_END_VALID,     REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("dma busy",     DPC_STATUS_DMA_BUSY,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("cbuf ready",   DPC_STATUS_CBUF_READY,    REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("cmd busy",     DPC_STATUS_CMD_BUSY,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("pipe busy",    DPC_STATUS_PIPE_BUSY,     REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("tmem busy",    DPC_STATUS_TMEM_BUSY,     REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("start gclk",   DPC_STATUS_START_GCLK,    REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("flush",        DPC_STATUS_FLUSH,         REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("freeze",       DPC_STATUS_FREEZE,        REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("transfer src", DPC_STATUS_XBUS_DMEM_DMA, REG_BITS_INFO_STR_XBUS_DMEM),

    REG_BITS_CMD_END(),
};

#define VI_CTRL_PIXEL_ADVANCE_MASK  0x0F000
#define VI_CTRL_KILL_WE             0x00800
#define VI_CTRL_TEST_MODE           0x00080
#define VI_CTRL_VBUS_CLOCK_ENABLE   0x00020 //! TODO: Warning to never set this bit.
#define VI_CTRL_TYPE_MASK           0x00003

const RegBitsInfo regBits_VI_CONTROL[] = {
    REG_BITS_CMD_SETX(STRLEN("dither filter: ")),
    REG_BITS_CMD_STR("dither filter", VI_CTRL_DITHER_FILTER_ON,   REG_BITS_INFO_STR_ENABLE),
    REG_BITS_CMD_BIN("pixel advance", VI_CTRL_PIXEL_ADVANCE_MASK, 1),
    REG_BITS_CMD_STR("kill we",       VI_CTRL_KILL_WE,            REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("aa mode",       VI_CTRL_ANTIALIAS_MASK,     REG_BITS_INFO_STR_VI_AA_MODE),
    REG_BITS_CMD_STR("test mode",     VI_CTRL_TEST_MODE,          REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("serrate",       VI_CTRL_SERRATE_ON,         REG_BITS_INFO_STR_ON_OFF),
    REG_BITS_CMD_STR("vbus clock",    VI_CTRL_VBUS_CLOCK_ENABLE,  REG_BITS_INFO_STR_ENABLE), //! TODO: Warning to never set this bit.
    REG_BITS_CMD_STR("divot",         VI_CTRL_DIVOT_ON,           REG_BITS_INFO_STR_ON_OFF),
    REG_BITS_CMD_STR("gamma",         VI_CTRL_GAMMA_ON,           REG_BITS_INFO_STR_ON_OFF),
    REG_BITS_CMD_STR("gamma dither",  VI_CTRL_GAMMA_DITHER_ON,    REG_BITS_INFO_STR_ON_OFF),
    REG_BITS_CMD_STR("type",          VI_CTRL_TYPE_MASK,          REG_BITS_INFO_STR_VI_TYPE),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_AI_CONTROL[] = {
    REG_BITS_CMD_SETX(STRLEN("dma: ")),
    REG_BITS_CMD_STR("dma", AI_CONTROL_DMA_ON, REG_BITS_INFO_STR_ON_OFF),

    REG_BITS_CMD_END(),
};

#define AI_STATUS_ENABLED   0x03000000
#define AI_STATUS_WC        0x00080000
#define AI_STATUS_BC        0x00010000
#define AI_STATUS_COUNT     0x00007FFE
#define AI_STATUS_FULL2     0x00000001

const RegBitsInfo regBits_AI_STATUS[] = {
    REG_BITS_CMD_SETX(STRLEN("word clock: ")),
    REG_BITS_CMD_STR("fifo full",  AI_STATUS_FIFO_FULL,         REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("dma busy",   AI_STATUS_DMA_BUSY,          REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("enabled",    BIT(CTZ(AI_STATUS_ENABLED)), REG_BITS_INFO_STR_ENABLE),
    REG_BITS_CMD_DEC("word clock", AI_STATUS_WC,    1),
    REG_BITS_CMD_DEC("bit clock",  AI_STATUS_BC,    1),
    REG_BITS_CMD_DEC("count",      AI_STATUS_COUNT, 5),
    //! TODO: AI_STATUS_FULL2?

    REG_BITS_CMD_END(),
};

#define PI_STATUS_INTR 0x08

const RegBitsInfo regBits_PI_STATUS[] = {
    REG_BITS_CMD_SETX(STRLEN("intr. (dma completed): ")),
    REG_BITS_CMD_STR("intr. (dma completed)", PI_STATUS_INTR,     REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("dma error",             PI_STATUS_ERROR,    REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("io busy",               PI_STATUS_IO_BUSY,  REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("dma busy",              PI_STATUS_DMA_BUSY, REG_BITS_INFO_STR_YES_NO),

    REG_BITS_CMD_END(),
};

#define RI_MODE_STOP_R  0x08
#define RI_MODE_STOP_T  0x04
#define RI_MODE_OP_MODE 0x03

const RegBitsInfo regBits_RI_MODE[] = {
    REG_BITS_CMD_SETX(STRLEN("op mode: ")),
    REG_BITS_CMD_STR("stop r",  RI_MODE_STOP_R,  REG_BITS_INFO_STR_ENABLE),
    REG_BITS_CMD_STR("stop t",  RI_MODE_STOP_T,  REG_BITS_INFO_STR_ENABLE),
    REG_BITS_CMD_STR("op mode", RI_MODE_OP_MODE, REG_BITS_INFO_STR_RI_MODE_OP),

    REG_BITS_CMD_END(),
};

#define RI_CONFIG_AUTO  0x40
#define RI_CONFIG_CC    0x3F

const RegBitsInfo regBits_RI_CONFIG[] = {
    REG_BITS_CMD_SETX(STRLEN("auto cc: ")),
    REG_BITS_CMD_STR("auto cc", RI_CONFIG_AUTO, REG_BITS_INFO_STR_ENABLE),
    REG_BITS_CMD_DEC("cc",      RI_CONFIG_CC,   2),

    REG_BITS_CMD_END(),
};

#define RI_REFRESH_MULTIBANK    0x00780000
#define RI_REFRESH_OPT          0x00040000
#define RI_REFRESH_EN           0x00020000
#define RI_REFRESH_BANK         0x00010000
#define RI_REFRESH_DIRTY        0x0000FF00
#define RI_REFRESH_CLEAN        0x000000FF

const RegBitsInfo regBits_RI_REFRESH[] = {
    REG_BITS_CMD_SETX(STRLEN("dirty refresh delay: ")),
    REG_BITS_CMD_BIN("multibank",           RI_REFRESH_MULTIBANK, 2),
    REG_BITS_CMD_STR("optimize",            RI_REFRESH_OPT,       REG_BITS_INFO_STR_ENABLE),
    REG_BITS_CMD_STR("automatic refresh",   RI_REFRESH_EN,        REG_BITS_INFO_STR_ENABLE),
    REG_BITS_CMD_DEC("bank",                RI_REFRESH_BANK,      1),
    REG_BITS_CMD_DEC("dirty refresh delay", RI_REFRESH_DIRTY,     3),
    REG_BITS_CMD_DEC("clean refresh delay", RI_REFRESH_CLEAN,     3),

    REG_BITS_CMD_END(),
};

#define SI_STATUS_DMA_STATE_MASK    0x0F00
#define SI_STATUS_PCH_STATE_MASK    0x00F0
#define SI_STATUS_READ_PENDING      0x0004

const RegBitsInfo regBits_SI_STATUS[] = {
    REG_BITS_CMD_SETX(STRLEN("pif channel state: ")),
    REG_BITS_CMD_STR("interrupt",         SI_STATUS_INTERRUPT,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_BIN("dma state",         SI_STATUS_DMA_STATE_MASK, 2),
    REG_BITS_CMD_BIN("pif channel state", SI_STATUS_PCH_STATE_MASK, 2),
    REG_BITS_CMD_STR("dma error",         SI_STATUS_DMA_ERROR,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("read pending",      SI_STATUS_READ_PENDING,   REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("io busy",           SI_STATUS_RD_BUSY,        REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("dma busy",          SI_STATUS_DMA_BUSY,       REG_BITS_INFO_STR_YES_NO),

    REG_BITS_CMD_END(),
};

typedef struct RegInspectExtraInfo {
    /*0x00*/ const enum RegisterSources src;
    /*0x01*/ const s8 idx;
    /*0x03*/ const u8 pad[2];
    /*0x04*/ const RegBitsInfo* list;
} RegInspectExtraInfo; /*0x08*/
const RegInspectExtraInfo sRegInspectExtraInfoFuncs[] = {
    { .src = REGS_CP0,   .idx = REG_CP0_SR,             .list = regBits_C0_SR,    },
    { .src = REGS_CP0,   .idx = REG_CP0_CAUSE,          .list = regBits_C0_CAUSE, },
    //! TODO: CP0 $Config, $Context, etc.
    { .src = REGS_FCR,   .idx = REG_FCR_CONTROL_STATUS, .list = regBits_FPR_CSR,  },
    { .src = REGS_SPC,   .idx = REG_SPC_RCP,            .list = regBits_SPC_RCP,  },

    { .src = REGS_RDRAM, .idx = REGID_RDRAM_CONFIG,     .list = regBits_RDRAM_CONFIG, },
    { .src = REGS_RDRAM, .idx = REGID_RDRAM_MODE,       .list = regBits_RDRAM_MODE,   },
    { .src = REGS_SP,    .idx = REGID_SP_STATUS,        .list = regBits_SP_STATUS,    },
    { .src = REGS_DPC,   .idx = REGID_DPC_STATUS,       .list = regBits_DPC_STATUS,   },
    { .src = REGS_VI,    .idx = REGID_VI_CONTROL,       .list = regBits_VI_CONTROL,   },
    { .src = REGS_AI,    .idx = REGID_AI_CONTROL,       .list = regBits_AI_CONTROL,   },
    { .src = REGS_AI,    .idx = REGID_AI_STATUS,        .list = regBits_AI_STATUS,    },
    { .src = REGS_PI,    .idx = REGID_PI_STATUS,        .list = regBits_PI_STATUS,    },
    { .src = REGS_RI,    .idx = REGID_RI_MODE,          .list = regBits_RI_MODE,      },
    { .src = REGS_RI,    .idx = REGID_RI_CONFIG,        .list = regBits_RI_CONFIG,    },
    { .src = REGS_RI,    .idx = REGID_RI_REFRESH,       .list = regBits_RI_REFRESH,   },
    { .src = REGS_SI,    .idx = REGID_SI_STATUS,        .list = regBits_SI_STATUS,    },
};

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
    Doubleword value = get_reg_val(src, idx);
    Word val32 = (Word)value;
    if (isCP1 && ((regId.idx & 0x1) == 0)) {
        //! TODO: Split hi and lo bits for FGR and label them even/odd + combined.
        Word cop1OddBits = get_reg_val(REGS_CP1, (regId.idx + 1));
        if (cop1OddBits != 0) {
            value = ((HiLo64){ .hi = cop1OddBits, .lo = val32, }).raw;
            is64Bit = TRUE;
        }
    }

    CSTextCoord_u32 line = 1;
    RGBA32 color = COLOR_RGBA32_WHITE;
    if (isInterface) {
        cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"register on %s (%s):", COLOR_RGBA32_CRASH_PAGE_NAME,
            regSrc->desc, regSrc->name
        );
        color = COLOR_RGBA32_VSC_DEFINE;
        cs_print(TEXT_X(2), TEXT_Y(line++), STR_COLOR_PREFIX"%s %s REG", color, regSrc->name, regInfo->name);
    } else {
        cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"register on thread %d (%s):", COLOR_RGBA32_CRASH_PAGE_NAME,
            gInspectThread->id, get_thread_name(gInspectThread)
        );
        color = COLOR_RGBA32_CRASH_VARIABLE;
        cs_print(TEXT_X(2), TEXT_Y(line++), STR_COLOR_PREFIX"\"$%s\" in %s", color, regInfo->name, regSrc->name);
    }
    const char* regDesc = get_reg_desc(src, idx);
    if (regDesc != NULL) {
        gCSWordWrap = TRUE;
        cs_print(TEXT_X(2), TEXT_Y(line), STR_COLOR_PREFIX"(%s)", color, regDesc);
        gCSWordWrap = FALSE;
        line += gCSNumLinesPrinted;
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
            cs_print_reg_info_list(line, val32, exInfo->list);
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

    reginspect_draw_contents(regId);

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
