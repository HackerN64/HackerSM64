#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/cs_settings.h"


enum CSSettingsGroup_page_memory {
    CS_OPT_HEADER_PAGE_MEMORY,
    CS_OPT_MEMORY_SHOW_RANGE,
#ifdef INCLUDE_DEBUG_MAP
    CS_OPT_MEMORY_SHOW_SYMBOL,
#endif // INCLUDE_DEBUG_MAP
    CS_OPT_MEMORY_DISPLAY_MODE,
    CS_OPT_END_MEMORY,
};


// RAM Viewer constants
#define PAGE_MEMORY_STEP (ssize_t)(4 * sizeof(Word))


extern struct CSSetting cs_settings_group_page_memory[];
extern struct CSPage gCSPage_memory;
