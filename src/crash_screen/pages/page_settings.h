#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/cs_settings.h"


#define SETTINGS_NUM_ROWS 21

#define VALUE_NAME_SIZE 10


enum CSSettingsGroup_buttons {
    CS_OPT_BUTTON_EXPAND_ALL,
    CS_OPT_BUTTON_COLLAPSE_ALL,
    CS_OPT_BUTTON_RESET_TO_DEFAULTS,
    CS_OPT_END_BUTTON,
};


typedef struct CSSettingDisplay {
    /*0x00*/ u8 groupID;
    /*0x01*/ u8 settingID;
} CSSettingDisplay; /*0x02*/


extern struct CSSetting cs_settings_group_buttons[];
extern struct CSPage gCSPage_settings;
