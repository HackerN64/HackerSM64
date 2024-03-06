#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/crash_settings.h"


enum CSSettingsGroup_page_context {
    CS_OPT_HEADER_PAGE_CONTEXT,
#ifdef INCLUDE_DEBUG_MAP
    CS_OPT_CONTEXT_PARSE_REG,
#endif // INCLUDE_DEBUG_MAP
    CS_OPT_CONTEXT_FLOATS_FMT,
    CS_OPT_END_CONTEXT,
};


typedef struct {
    /*0x00*/ const Address offset;
    /*0x04*/ const u8 size;
    /*0x05*/ const char name[3];
} OSThreadContextRegister; /*0x08*/


extern struct CSSetting cs_settings_group_page_context[];
extern struct CSPage gCSPage_context;
