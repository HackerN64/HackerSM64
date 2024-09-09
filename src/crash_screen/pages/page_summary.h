#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/cs_settings.h"


enum CSSettingsGroup_page_summary {
    CS_OPT_HEADER_PAGE_SUMMARY,
    CS_OPT_END_SUMMARY,
};


extern struct CSSetting cs_settings_group_page_summary[];
extern struct CSPage gCSPage_summary;
