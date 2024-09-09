#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/cs_settings.h"


enum CSSettingsGroup_page_segments {
    CS_OPT_HEADER_PAGE_SEGMENTS,
    CS_OPT_SEGMENTS_SHOW_ADDRESSES,
    CS_OPT_END_SEGMENTS,
};


// Number of rows to print:
#define NUM_SHOWN_SEGMENTS 10
#define NUM_SHOWN_SEGMENTS_ROWS (NUM_SHOWN_SEGMENTS * 2)


extern struct CSSetting cs_settings_group_page_segments[];
extern struct CSPage gCSPage_segments;
