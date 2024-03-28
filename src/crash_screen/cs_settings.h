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

    // CS_OPT_GROUP_PAGE_SUMMARY,
    CS_OPT_GROUP_PAGE_STACK,
    CS_OPT_GROUP_PAGE_REGISTERS,
    CS_OPT_GROUP_PAGE_DISASM,
    CS_OPT_GROUP_PAGE_MEMORY,
#ifdef INCLUDE_DEBUG_MAP
    CS_OPT_GROUP_PAGE_MAP,
#endif // INCLUDE_DEBUG_MAP
#ifdef PUPPYPRINT_DEBUG
    CS_OPT_GROUP_PAGE_LOGS,
#endif // PUPPYPRINT_DEBUG
    // CS_OPT_GROUP_PAGE_SETTINGS,
    // CS_OPT_GROUP_PAGE_ABOUT,

    NUM_CS_OPT_GROUPS,
};

enum CSSettingsGroup_global {
    CS_OPT_HEADER_GLOBAL,
    CS_OPT_GLOBAL_DRAW_SCREENSHOT,
    CS_OPT_GLOBAL_DRAW_LR_ARROWS,
#ifdef INCLUDE_DEBUG_MAP
    CS_OPT_GLOBAL_SYMBOL_NAMES,
    CS_OPT_ADDRESS_SELECT_SYMBOL,
#endif // INCLUDE_DEBUG_MAP
    CS_OPT_GLOBAL_PRINT_SCROLL_SPEED,
    CS_OPT_GLOBAL_FLOATS_FMT,
    CS_OPT_GLOBAL_BG_OPACITY,
    CS_OPT_GLOBAL_POPUP_OPACITY,
    CS_OPT_END_GLOBAL,
};

enum CSSettingsEntryType {
    CS_OPT_TYPE_END,
    CS_OPT_TYPE_SETTING,
    CS_OPT_TYPE_HEADER,
};


//! TODO: shown flag + enabled flag + callback func(old, new) + draw func(x, y, text, opt).
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
