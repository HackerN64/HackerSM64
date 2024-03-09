#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/crash_settings.h"

#include "game/emutest.h"


enum CSSettingsGroup_page_about {
    CS_OPT_HEADER_PAGE_ABOUT,
    CS_OPT_END_ABOUT,
};


typedef struct EmulatorName {
    /*0x00*/ const enum Emulator bits;
    /*0x04*/ const char* name;
} EmulatorName; /*0x08*/


extern struct CSSetting cs_settings_group_page_about[];
extern struct CSPage gCSPage_about;
