#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/crash_settings.h"


enum CSSettingsGroup_page_context {
    CS_OPT_HEADER_PAGE_CONTEXT,
#ifdef INCLUDE_DEBUG_MAP
    CS_OPT_CONTEXT_PARSE_REG,
#endif
    CS_OPT_CONTEXT_FLOATS_FMT,
    CS_OPT_END_CONTEXT,
};


extern struct CSSetting cs_settings_group_page_context[];
extern struct CSPage gCSPage_context;
