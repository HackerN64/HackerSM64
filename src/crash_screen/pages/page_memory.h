#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/crash_settings.h"


enum CSSettingsGroup_page_memory {
    CS_OPT_HEADER_PAGE_MEMORY,
#ifdef INCLUDE_DEBUG_MAP
    CS_OPT_MEMORY_SHOW_SYMBOL,
#endif
    CS_OPT_MEMORY_AS_ASCII,
    CS_OPT_END_MEMORY,
};


// RAM Viewer constants
#define RAM_VIEWER_STEP (ssize_t)(4 * sizeof(Word))


extern struct CSSetting cs_settings_group_page_memory[];
extern struct CSPage gCSPage_memory;
