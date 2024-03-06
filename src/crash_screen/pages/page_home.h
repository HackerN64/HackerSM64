#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/crash_settings.h"


enum CSSettingsGroup_page_home {
    CS_OPT_HEADER_PAGE_HOME,
    CS_OPT_END_HOME,
};


typedef struct ThreadIDName {
    /*0x00*/ enum ThreadID threadID;
    /*0x04*/ const char* name;
} ThreadIDName; /*0x08*/


extern struct CSSetting cs_settings_group_page_home[];
extern struct CSPage gCSPage_home;
