#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "cs_controls.h"
#include "cs_main.h"
#include "cs_pages.h"

#include "cs_settings.h"

#include "cs_draw.h"

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
    [CS_OPT_HEADER_GLOBAL            ] = { .type = CS_OPT_TYPE_HEADER,  .name = "GLOBAL",                .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,             .upperBound = TRUE,               },
    [CS_OPT_GLOBAL_DRAW_SCREENSHOT   ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show game screenshot",  .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,             .upperBound = TRUE,               },
    [CS_OPT_GLOBAL_DRAW_LR_ARROWS    ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show L/R arrows",       .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,             .upperBound = TRUE,               },
#ifdef INCLUDE_DEBUG_MAP        
    [CS_OPT_GLOBAL_SYMBOL_NAMES      ] = { .type = CS_OPT_TYPE_SETTING, .name = "Print symbol names",    .valNames = &gValNames_bool,          .val = SHOW_FUNC_NAMES_DEFAULT,   .defaultVal = SHOW_FUNC_NAMES_DEFAULT,   .lowerBound = FALSE,             .upperBound = TRUE,               },
    [CS_OPT_ADDRESS_SELECT_SYMBOL    ] = { .type = CS_OPT_TYPE_SETTING, .name = "Address select symbol", .valNames = &gValNames_bool,          .val = SHOW_FUNC_NAMES_DEFAULT,   .defaultVal = SHOW_FUNC_NAMES_DEFAULT,   .lowerBound = FALSE,             .upperBound = TRUE,               },
#endif // INCLUDE_DEBUG_MAP
    [CS_OPT_GLOBAL_PRINT_SCROLL_SPEED] = { .type = CS_OPT_TYPE_SETTING, .name = "Text scroll speed",     .valNames = NULL,                     .val = CRASH_SCREEN_LETTER_WIDTH, .defaultVal = CRASH_SCREEN_LETTER_WIDTH, .lowerBound = 0,                 .upperBound = 127,                },
    [CS_OPT_GLOBAL_FLOATS_FMT        ] = { .type = CS_OPT_TYPE_SETTING, .name = "Floats print format",   .valNames = &gValNames_print_num_fmt, .val = PRINT_NUM_FMT_DEC,         .defaultVal = PRINT_NUM_FMT_DEC,         .lowerBound = PRINT_NUM_FMT_HEX, .upperBound = PRINT_NUM_FMT_SCI,  },
    [CS_OPT_GLOBAL_BG_OPACITY        ] = { .type = CS_OPT_TYPE_SETTING, .name = "Background opacity",    .valNames = NULL,                     .val = CS_DARKEN_SEVEN_EIGHTHS,   .defaultVal = CS_DARKEN_SEVEN_EIGHTHS,   .lowerBound = CS_DARKEN_NONE,    .upperBound = CS_DARKEN_TO_BLACK, },
    [CS_OPT_GLOBAL_POPUP_OPACITY     ] = { .type = CS_OPT_TYPE_SETTING, .name = "Popup opacity",         .valNames = NULL,                     .val = CS_DARKEN_SEVEN_EIGHTHS,   .defaultVal = CS_DARKEN_SEVEN_EIGHTHS,   .lowerBound = CS_DARKEN_NONE,    .upperBound = CS_DARKEN_TO_BLACK, },
    [CS_OPT_END_GLOBAL               ] = { .type = CS_OPT_TYPE_END, },
};

// Groups:
CSSetting* gCSSettingsGroups[NUM_CS_OPT_GROUPS] = {
    [CS_OPT_GROUP_BUTTONS       ] = cs_settings_group_buttons,
    [CS_OPT_GROUP_GLOBAL        ] = cs_settings_group_global,
    [CS_OPT_GROUP_CONTROLS      ] = cs_settings_group_controls,

    //! TODO: Automatically do this from gCSPages:
    // [CS_OPT_GROUP_PAGE_SUMMARY  ] = cs_settings_group_page_summary,
    [CS_OPT_GROUP_PAGE_STACK    ] = cs_settings_group_page_stack,
    [CS_OPT_GROUP_PAGE_REGISTERS] = cs_settings_group_page_registers,
    [CS_OPT_GROUP_PAGE_DISASM   ] = cs_settings_group_page_disasm,
    [CS_OPT_GROUP_PAGE_MEMORY   ] = cs_settings_group_page_memory,
#ifdef INCLUDE_DEBUG_MAP
    [CS_OPT_GROUP_PAGE_MAP      ] = cs_settings_group_page_map,
#endif // INCLUDE_DEBUG_MAP
    [CS_OPT_GROUP_PAGE_SEGMENTS ] = cs_settings_group_page_segments,
    // [CS_OPT_GORUP_PAGE_INTERFACES] = cs_Settings_group_page_interfaces,
#ifdef PUPPYPRINT_DEBUG
    [CS_OPT_GROUP_PAGE_LOGS     ] = cs_settings_group_page_logs,
#endif // PUPPYPRINT_DEBUG
    // [CS_OPT_GROUP_PAGE_SETTINGS ] = cs_settings_group_page_settings,
    // [CS_OPT_GROUP_PAGE_ABOUT    ] = cs_settings_group_page_about,
};


// Gets the settings group pointer from a groupID.
static CSSetting* cs_get_settings_group(int groupID) {
    return gCSSettingsGroups[groupID];
}

// Gets the setting pointer from a groupID and settingID.
CSSetting* cs_get_setting(int groupID, int settingID) {
    CSSetting* group = cs_get_settings_group(groupID);

    return &group[settingID];
}

// Gets the value of a setting from a groupID and settingID.
SettingsType cs_get_setting_val(int groupID, int settingID) {
    CSSetting* setting = cs_get_setting(groupID, settingID);

    return ((setting != NULL) ? setting->val : 0);
}

// Increment/wrap the value of a setting by 'inc'.
void cs_inc_setting(int groupID, int settingID, SettingsType inc) {
    CSSetting* setting = cs_get_setting(groupID, settingID);

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

// Checks whether a specific settings group has a header.
_Bool cs_settings_group_has_header(int groupID) {
    CSSetting* group = cs_get_settings_group(groupID);

    return ((group != NULL) && (group[GROUP_HEADER_INDEX].type == CS_OPT_TYPE_HEADER));
}

// -- CSSettingsFunc --

// Resets a specific setting to its default value.
_Bool cs_setting_func_reset(int groupID, int settingID) {
    CSSetting* setting = cs_get_setting(groupID, settingID);

    if ((setting != NULL) && (setting->type == CS_OPT_TYPE_SETTING)) {
        setting->val = setting->defaultVal;
    }

    return FALSE;
}

// Checks whether a specific setting has been changed from its default value.
_Bool cs_setting_func_is_non_default(int groupID, int settingID) {
    CSSetting* setting = cs_get_setting(groupID, settingID);

    if ((setting != NULL) && (setting->type == CS_OPT_TYPE_SETTING)) {
        return (setting->val != setting->defaultVal);
    }

    return FALSE;
}

// -- End CSSettingsFunc --

// Applies a function to all settings in a specific group.
_Bool cs_settings_apply_func_to_all_in_group(CSSettingsFunc func, int groupID) {
    int settingID = cs_settings_group_has_header(groupID); // Skip header.

    while (TRUE) {
        CSSetting* setting = cs_get_setting(groupID, settingID);

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

// Applies a function to all settings.
_Bool cs_settings_apply_func_to_all(CSSettingsFunc func) {
    for (int groupID = 0; groupID < NUM_CS_OPT_GROUPS; groupID++) {
        if (cs_settings_apply_func_to_all_in_group(func, groupID)) {
            return TRUE;
        }
    }

    return FALSE;
}

// Check all settings headers for a specific expand state.
_Bool cs_settings_check_for_header_state(_Bool expand) {
    for (int groupID = 0; groupID < NUM_CS_OPT_GROUPS; groupID++) {
        if (cs_settings_group_has_header(groupID) && (cs_get_setting_val(groupID, GROUP_HEADER_INDEX) == expand)) {
            return TRUE;
        }
    }

    return FALSE;
}

// Sets the expand state of all settings headers.
void cs_settings_set_all_headers(_Bool expand) {
    for (int groupID = 0; groupID < NUM_CS_OPT_GROUPS; groupID++) {
        if (cs_settings_group_has_header(groupID)) {
            CSSetting* setting = cs_get_setting(groupID, GROUP_HEADER_INDEX);

            setting->val = expand;
        }
    }
}
