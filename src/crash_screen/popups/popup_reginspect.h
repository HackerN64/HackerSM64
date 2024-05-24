#pragma once

#include <ultra64.h>

#include "types.h"


typedef enum PACKED RegBitsInfoStringLists {
    REG_BITS_INFO_STR_SET,
    REG_BITS_INFO_STR_TRUTH,
    REG_BITS_INFO_STR_YES_NO,
    REG_BITS_INFO_STR_ENABLED,
    REG_BITS_INFO_STR_ON_OFF,
    REG_BITS_INFO_STR_SUCCESS,
    REG_BITS_INFO_STR_ENDIAN,
    REG_BITS_INFO_STR_BIT_MODE,
    REG_BITS_INFO_STR_32_64,
    REG_BITS_INFO_STR_64_128,
    REG_BITS_INFO_STR_AUTO,

    REG_BITS_INFO_STR_C0_SR_EXEC_MODE,
    REG_BITS_INFO_STR_C0_SR_BEV,
    REG_BITS_INFO_STR_C0_CONFIG_EM,
    REG_BITS_INFO_STR_C0_CONFIG_EB,
    REG_BITS_INFO_STR_FCR31_ROUNDING_MODE,

    REG_BITS_INFO_STR_RDRAM_VERSION,
    REG_BITS_INFO_STR_RDRAM_TYPE,
    REG_BITS_INFO_STR_BANK,
    REG_BITS_INFO_STR_XBUS_DMEM,
    REG_BITS_INFO_STR_VI_AA_MODE,
    REG_BITS_INFO_STR_VI_TYPE,
    REG_BITS_INFO_STR_RI_MODE_OP,
} RegBitsInfoStringLists;
typedef enum PACKED RegBitsInfoIdStringPairs {
    REG_BITS_INFO_LIST_C0_CONFIG_SYS_CLOCK_RATIO,
    REG_BITS_INFO_LIST_C0_CONFIG_TRANS_DATA_PATTERN,
    REG_BITS_INFO_LIST_C0_CONFIG_K0,
} RegBitsInfoIdStringPairs;
typedef enum PACKED RegBitsInfoFuncs {
    REG_BITS_INFO_FUNC_READWRITE,
    REG_BITS_INFO_FUNC_C0_CAUSE,
    REG_BITS_INFO_FUNC_RDRAM_MODE_CCVALUE,
} RegBitsInfoFuncs;


typedef enum PACKED RegBitsType {
    REG_BITS_TYPE_END = -1,
    REG_BITS_TYPE_NONE,
    REG_BITS_TYPE_BIN,
    REG_BITS_TYPE_HEX,
    REG_BITS_TYPE_DEC,
    REG_BITS_TYPE_STR,  // String array.
    REG_BITS_TYPE_BSTR, // Invert boolean string array.
    REG_BITS_TYPE_ISTR,
    REG_BITS_TYPE_FUNC,
    REG_BITS_TYPE_ADDR,
    // non-print:
    REG_BITS_TYPE_SETX, // Set info start X.
    REG_BITS_TYPE_SETW, // Set info width (for wrapping).
    REG_BITS_TYPE_WRAP,
} RegBitsType;
typedef struct RegBitsInfo {
    /*0x00*/ const char* name;
    /*0x04*/ const u8 maskSize;
    /*0x05*/ const u8 shiftSize;
    /*0x06*/ RegBitsType type;
    /*0x07*/ union {
                u8 arg;                         // Generic arg for macro.
                u8 spacing;                     // REG_BITS_TYPE_BIN
                u8 numDigits;                   // REG_BITS_TYPE_HEX/REG_BITS_TYPE_DEC
                RegBitsInfoStringLists list;    // REG_BITS_TYPE_STR
                RegBitsInfoIdStringPairs iList; // RET_BITS_TYPE_ISTR
                RegBitsInfoFuncs func;          // REG_BITS_TYPE_FUNC
                _Bool isVirtual;                // REG_BITS_TYPE_ADDR
                u8 xPos;                        // REG_BITS_TYPE_SETX
                u8 width;                       // REG_BITS_TYPE_SETW
            };
} RegBitsInfo; /*0x08*/
#define SIZEOF_REG_BITS_INFO sizeof(RegBitsInfo)
#define REG_BITS_CMD(_name, _mask, _type, _arg) {   \
    .name      = _name,                             \
    .maskSize  = POPCOUNT(_mask),                   \
    .shiftSize = CTZ(_mask),                        \
    .type      = _type,                             \
    .arg       = _arg,                              \
}
#define REG_BITS_CMD_BIN(_name,  _mask, _spacing)   REG_BITS_CMD(_name, _mask, REG_BITS_TYPE_BIN,  _spacing)
#define REG_BITS_CMD_HEX(_name,  _mask, _numDigits) REG_BITS_CMD(_name, _mask, REG_BITS_TYPE_HEX,  _numDigits)
#define REG_BITS_CMD_DEC(_name,  _mask, _numDigits) REG_BITS_CMD(_name, _mask, REG_BITS_TYPE_DEC,  _numDigits)
#define REG_BITS_CMD_STR(_name,  _mask, _list)      REG_BITS_CMD(_name, _mask, REG_BITS_TYPE_STR,  _list)
#define REG_BITS_CMD_BSTR(_name, _mask, _list)      REG_BITS_CMD(_name, _mask, REG_BITS_TYPE_BSTR, _list)
#define REG_BITS_CMD_ISTR(_name, _mask, _list)      REG_BITS_CMD(_name, _mask, REG_BITS_TYPE_ISTR, _list)
#define REG_BITS_CMD_FUNC(_name, _mask, _func)      REG_BITS_CMD(_name, _mask, REG_BITS_TYPE_FUNC, _func)
#define REG_BITS_CMD_SETX(_x)                       REG_BITS_CMD(NULL,  0,     REG_BITS_TYPE_SETX, _x)
#define REG_BITS_CMD_SETW(_width)                   REG_BITS_CMD(NULL,  0,     REG_BITS_TYPE_SETW, _width)
#define REG_BITS_CMD_WRAP()                         REG_BITS_CMD(NULL,  0,     REG_BITS_TYPE_WRAP, 0)
#define REG_BITS_CMD_NONE(_name)                    REG_BITS_CMD(_name, 0,     REG_BITS_TYPE_NONE, 0)
#define REG_BITS_CMD_GAP()                          REG_BITS_CMD(NULL,  0,     REG_BITS_TYPE_NONE, 0)
#define REG_BITS_CMD_END()                          REG_BITS_CMD(NULL,  0,     REG_BITS_TYPE_END,  0)


extern RegisterId gInspectedRegister;


extern struct CSPopup gCSPopup_reginspect;


void cs_open_reginspect(RegisterId regId);
