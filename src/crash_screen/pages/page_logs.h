#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/cs_settings.h"


#ifdef PUPPYPRINT_DEBUG

enum CSSettingsGroup_page_log {
    CS_OPT_HEADER_PAGE_LOG,
    CS_OPT_LOG_INDEX_NUMBERS,
    CS_OPT_END_LOG,
};


extern struct CSSetting cs_settings_group_page_logs[];
extern struct CSPage gCSPage_logs;

#endif // PUPPYPRINT_DEBUG
