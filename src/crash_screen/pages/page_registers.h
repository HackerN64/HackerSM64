#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/crash_settings.h"


enum CSSettingsGroup_page_registers {
    CS_OPT_HEADER_PAGE_REGISTERS,
#ifdef INCLUDE_DEBUG_MAP
    CS_OPT_REGISTERS_PARSE_REG,
#endif // INCLUDE_DEBUG_MAP
    CS_OPT_END_REGISTERS,
};


typedef struct {
    /*0x00*/ const Address offset;
    /*0x04*/ const u8 size;
    /*0x05*/ const char name[3];
} OSThreadContextRegister; /*0x08*/


extern struct CSSetting cs_settings_group_page_registers[];
extern struct CSPage gCSPage_registers;
