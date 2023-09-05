#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "crash_controls.h"
#include "crash_main.h"
#include "crash_pages.h"

#include "crash_settings.h"

#include "engine/math_util.h"


const char* gValNames_bool[] = {
    [FALSE] = "FALSE",
    [TRUE ] = "TRUE",
};

const char* gValNames_print_num_fmt[] = {
    [PRINT_NUM_FMT_HEX] = "HEX",
    [PRINT_NUM_FMT_DEC] = "DECIMAL",
    [PRINT_NUM_FMT_SCI] = "SCIENTIFIC",
};

#define SHOW_FUNC_NAMES_DEFAULT TRUE

CSSetting cs_settings_group_global[] = {
    [CS_OPT_HEADER_GLOBAL               ] = { .type = CS_OPT_TYPE_HEADER,  .name = "GLOBAL",                         .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_GLOBAL_DRAW_SCREENSHOT      ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show screenshot background",     .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
#ifdef INCLUDE_DEBUG_MAP
    [CS_OPT_GLOBAL_SYMBOL_NAMES         ] = { .type = CS_OPT_TYPE_SETTING, .name = "Print symbol names",             .valNames = &gValNames_bool,          .val = SHOW_FUNC_NAMES_DEFAULT,   .defaultVal = SHOW_FUNC_NAMES_DEFAULT,   .lowerBound = FALSE,                 .upperBound = TRUE,                       },
#endif
    [CS_OPT_GLOBAL_PRINT_SCROLL_SPEED   ] = { .type = CS_OPT_TYPE_SETTING, .name = "Text scroll speed",              .valNames = NULL,                     .val = 2,                         .defaultVal = 2,                         .lowerBound = 0,                     .upperBound = 5,                          },
    [CS_OPT_END_GLOBAL                  ] = { .type = CS_OPT_TYPE_END },
};

// Groups:
CSSettingsGroup gCSSettingsGroups[NUM_CS_OPT_GROUPS] = {
    [CS_OPT_GROUP_BUTTONS     ] = { .name = "BUTTONS",  .list = cs_settings_group_buttons,     },
    [CS_OPT_GROUP_GLOBAL      ] = { .name = "GLOBAL",   .list = cs_settings_group_global,      },
    [CS_OPT_GROUP_CONTROLS    ] = { .name = "CONTROLS", .list = cs_settings_group_controls,    },
    [CS_OPT_GROUP_PAGE_CONTEXT] = { .name = "CONTEXT",  .list = cs_settings_group_page_context,},
    [CS_OPT_GROUP_PAGE_LOG    ] = { .name = "LOG",      .list = cs_settings_group_page_log,    },
    [CS_OPT_GROUP_PAGE_STACK  ] = { .name = "STACK",    .list = cs_settings_group_page_stack,  },
#ifdef INCLUDE_DEBUG_MAP
    [CS_OPT_GROUP_PAGE_MAP    ] = { .name = "MAP",      .list = cs_settings_group_page_map,    },
#endif
    [CS_OPT_GROUP_PAGE_MEMORY ] = { .name = "RAM VIEW", .list = cs_settings_group_page_memory, },
    [CS_OPT_GROUP_PAGE_DISASM ] = { .name = "DISASM",   .list = cs_settings_group_page_disasm, },
};

CSSettingsGroup* get_settings_group(int groupID) {
    return &gCSSettingsGroups[groupID];
}

CSSetting* get_setting(int groupID, int settingID) {
    CSSettingsGroup* group = get_settings_group(groupID);

    return &group->list[settingID];
}

SettingsType get_setting_val(int groupID, int settingID) {
    CSSetting* setting = get_setting(groupID, settingID);

    return ((setting != NULL) ? setting->val : 0);
}

// Increment/wrap value.
void crash_screen_inc_setting(int groupID, int settingID, SettingsType inc) {
    CSSetting* setting = get_setting(groupID, settingID);

    if (
        (setting != NULL) &&
        (
            (setting->type == CS_OPT_TYPE_HEADER ) ||
            (setting->type == CS_OPT_TYPE_SETTING)
        )
    ) {
        setting->val = WRAP((setting->val + inc), setting->lowerBound, setting->upperBound);
    }
}

_Bool group_has_header(int groupID) {
    CSSettingsGroup* group = get_settings_group(groupID);

    return ((group != NULL) && (group->list != NULL) && (group->list[GROUP_HEADER_INDEX].type == CS_OPT_TYPE_HEADER));
}

// -- Settings functions --

// Reset a specific setting to its default.
_Bool settings_func_reset(int groupID, int settingID) {
    CSSetting* setting = get_setting(groupID, settingID);

    if ((setting != NULL) && (setting->type == CS_OPT_TYPE_SETTING)) {
        setting->val = setting->defaultVal;
    }

    return FALSE;
}

_Bool settings_func_is_non_default(int groupID, int settingID) {
    CSSetting* setting = get_setting(groupID, settingID);

    if ((setting != NULL) && (setting->type == CS_OPT_TYPE_SETTING)) {
        return (setting->val != setting->defaultVal);
    }

    return FALSE;
}

_Bool crash_screen_settings_apply_to_all_in_group(CSSettingsFunc func, int groupID) {
    int settingID = group_has_header(groupID); // Skip header.

    while (TRUE) {
        CSSetting* setting = get_setting(groupID, settingID);

        if ((setting == NULL) || (setting->type == CS_OPT_TYPE_END)) {
            break;
        }

        if (setting->type == CS_OPT_TYPE_SETTING) {
            if (func(groupID, settingID)) {
                return TRUE;
            }
        }

        settingID++;
    }

    return FALSE;
}

_Bool crash_screen_settings_apply_to_all(CSSettingsFunc func) {
    for (int groupID = 0; groupID < NUM_CS_OPT_GROUPS; groupID++) {
        if (crash_screen_settings_apply_to_all_in_group(func, groupID)) {
            return TRUE;
        }
    }

    return FALSE;
}

_Bool crash_screen_settings_check_for_header_state(_Bool expand) {
    for (int groupID = 0; groupID < NUM_CS_OPT_GROUPS; groupID++) {
        if (group_has_header(groupID) && (get_setting_val(groupID, GROUP_HEADER_INDEX) == expand)) {
            return TRUE;
        }
    }

    return FALSE;
}

void crash_screen_settings_set_all_headers(_Bool expand) {
    for (int groupID = 0; groupID < NUM_CS_OPT_GROUPS; groupID++) {
        if (group_has_header(groupID)) {
            CSSetting* setting = get_setting(groupID, GROUP_HEADER_INDEX);

            setting->val = expand;
        }
    }
}
