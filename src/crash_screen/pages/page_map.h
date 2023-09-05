#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/crash_settings.h"


enum CSSettingsGroup_page_map {
    CS_OPT_HEADER_PAGE_MAP,
    CS_OPT_MAP_SHOW_ADDRESSES,
    CS_OPT_MAP_SHOW_TYPES,
    CS_OPT_MAP_SHOW_SIZES,
    CS_OPT_END_MAP,
};


// Number of rows to print:
#define MAP_VIEWER_NUM_ROWS 20


extern struct CSSetting cs_settings_group_page_map[];
extern struct CSPage gCSPage_map;
