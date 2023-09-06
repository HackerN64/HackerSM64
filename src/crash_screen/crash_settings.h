#pragma once

#include <ultra64.h>

#include "types.h"


#define SECTION_EXPANDED_DEFAULT FALSE
#define MAX_OPTS_PER_GROUP 8
#define GROUP_HEADER_INDEX 0


typedef s16 SettingsType;
typedef _Bool (*CSSettingsFunc)(int groupID, int settingID);


enum CSPrintNumberFormats {
    PRINT_NUM_FMT_HEX, // 0xFFFFFFFF
    PRINT_NUM_FMT_DEC, // 0.0
    PRINT_NUM_FMT_SCI, // XeX
};

enum CSSettingsGroups {
    CS_OPT_GROUP_BUTTONS,
    CS_OPT_GROUP_GLOBAL,
    CS_OPT_GROUP_CONTROLS,
    CS_OPT_GROUP_PAGE_CONTEXT,
    CS_OPT_GROUP_PAGE_LOG,
    CS_OPT_GROUP_PAGE_STACK,
#ifdef INCLUDE_DEBUG_MAP
    CS_OPT_GROUP_PAGE_MAP,
#endif
    CS_OPT_GROUP_PAGE_MEMORY,
    CS_OPT_GROUP_PAGE_DISASM,
    NUM_CS_OPT_GROUPS,
};

enum CSSettingsGroup_global {
    CS_OPT_HEADER_GLOBAL,
    CS_OPT_GLOBAL_DRAW_SCREENSHOT,
#ifdef INCLUDE_DEBUG_MAP
    CS_OPT_GLOBAL_SYMBOL_NAMES,
#endif
    CS_OPT_GLOBAL_PRINT_SCROLL_SPEED,
    CS_OPT_END_GLOBAL,
};

enum CSSettingsEntryType {
    CS_OPT_TYPE_END,
    CS_OPT_TYPE_SETTING,
    CS_OPT_TYPE_HEADER,
    CS_OPT_TYPE_BUTTON,
};


typedef struct CSSettingsGroup {
    /*0x00*/ const char* name;
    /*0x04*/ struct CSSetting* list;
} CSSettingsGroup; /*0x08*/

//! TODO: shown flag + enabled flag + callback func(old, new) + draw func(x, y, text, opt)
typedef struct CSSetting {
    /*0x00*/ enum CSSettingsEntryType type;
    /*0x04*/ const char* name;
    /*0x08*/ const char* (*valNames)[];
    /*0x0C*/ SettingsType val;
    /*0x0E*/ SettingsType defaultVal;
    /*0x10*/ SettingsType lowerBound;
    /*0x12*/ SettingsType upperBound;
} CSSetting; /*0x14*/


extern const char* gValNames_bool[];
extern const char* gValNames_print_num_fmt[];


CSSettingsGroup* cs_get_settings_group(int groupID);
CSSetting* cs_get_setting(int groupID, int settingID);
SettingsType cs_get_setting_val(int groupID, int settingID);
void cs_inc_setting(int groupID, int settingID, SettingsType inc);
_Bool cs_settings_group_has_header(int groupID);

_Bool cs_setting_func_reset(int groupID, int settingID);
_Bool cs_setting_func_is_non_default(int groupID, int settingID);

_Bool cs_settings_check_for_header_state(_Bool expand);
void cs_settings_set_all_headers(_Bool expand);

_Bool cs_settings_apply_func_to_all_in_group(CSSettingsFunc func, int groupID);
_Bool cs_settings_apply_func_to_all(CSSettingsFunc func);
