#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/cs_settings.h"


enum CSSettingsGroup_page_interfaces {
    CS_OPT_HEADER_PAGE_INTERFACES,
    CS_OPT_INTERFACES_SHOW_ADDRESSES,
    CS_OPT_END_INTERFACES,
};


extern struct CSSetting cs_settings_group_page_interfaces[];
extern struct CSPage gCSPage_interfaces;
