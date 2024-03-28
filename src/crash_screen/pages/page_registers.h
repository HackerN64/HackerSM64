#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/util/registers.h"
#include "crash_screen/cs_settings.h"


enum CSSettingsGroup_page_registers {
    CS_OPT_HEADER_PAGE_REGISTERS,
#ifdef INCLUDE_DEBUG_MAP
    CS_OPT_REGISTERS_PARSE_REG,
#endif // INCLUDE_DEBUG_MAP
    CS_OPT_END_REGISTERS,
};


extern struct CSSetting cs_settings_group_page_registers[];
extern struct CSPage gCSPage_registers;
