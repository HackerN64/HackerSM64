#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/crash_controls.h"
#include "crash_screen/crash_draw.h"
#include "crash_screen/crash_pages.h"
#include "crash_screen/crash_print.h"
#include "crash_screen/crash_settings.h"

#include "page_settings.h"


struct CSSetting cs_settings_group_buttons[] = {
    [CS_OPT_BUTTON_EXPAND_ALL           ] = { .type = CS_OPT_TYPE_SETTING, .name = "expand all",                     .valNames = &gValNames_bool,          .val = FALSE,                     .defaultVal = FALSE,                     .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_BUTTON_COLLAPSE_ALL         ] = { .type = CS_OPT_TYPE_SETTING, .name = "collapse all",                   .valNames = &gValNames_bool,          .val = FALSE,                     .defaultVal = FALSE,                     .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_BUTTON_RESET_TO_DEFAULTS    ] = { .type = CS_OPT_TYPE_SETTING, .name = "reset all settings to defaults", .valNames = &gValNames_bool,          .val = FALSE,                     .defaultVal = FALSE,                     .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_END_BUTTON                  ] = { .type = CS_OPT_TYPE_END },
};


const enum ControlTypes cs_cont_list_settings[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_SCROLL_LIST,
    CONT_DESC_CHANGE_SETTING,
    CONT_DESC_RESET_SETTING,
    CONT_DESC_LIST_END,
};


u32 sSettingsSelectedIndex = 0;
static u32 sSettingsViewportIndex = 0;

CSSettingDisplay gCSDisplayedSettings[NUM_CS_OPT_GROUPS * MAX_OPTS_PER_GROUP];
static u32 sNumDisplayedSettings = 0;


static void append_to_displayed_settings(int groupID, int settingID) {
    gCSDisplayedSettings[sNumDisplayedSettings++] = (CSSettingDisplay){
        .groupID   = groupID,
        .settingID = settingID,
    };
}

void update_displayed_settings(void) {
    bzero(&gCSDisplayedSettings, sizeof(gCSDisplayedSettings));

    sNumDisplayedSettings = 0;
    _Bool sectionShown = TRUE;

    // Loop through all groups and add their contents to gCSDisplayedSettings.
    for (int groupID = 0; groupID < NUM_CS_OPT_GROUPS; groupID++) {
        int settingID = 0;

        // Header.
        if (cs_settings_group_has_header(groupID)) {
            CSSettingsGroup* group = cs_get_settings_group(groupID);

            sectionShown = group->list[settingID].val;

            append_to_displayed_settings(groupID, settingID);

            settingID++;
        }

        // Settings entries.
        while (sectionShown) {
            CSSetting* setting = cs_get_setting(groupID, settingID);

            if ((setting == NULL) || (setting->type == CS_OPT_TYPE_END)) {
                break;
            }

            append_to_displayed_settings(groupID, settingID);

            settingID++;
        }
    }
}

void settings_init(void) {
    sSettingsSelectedIndex = 0;
    sSettingsViewportIndex = 0;

    update_displayed_settings();
}

extern const char* gValNames_bool[];

void print_settings_list(u32 line, u32 numLines) {
    u32 currViewIndex = sSettingsViewportIndex;
    const u32 section_indent = STRLEN("> ");

    // Print
    for (u32 i = 0; i < numLines; i++, currViewIndex++) {
        CSSettingDisplay* settingDisplay = &gCSDisplayedSettings[currViewIndex];
        s16 groupID   = settingDisplay->groupID;
        s16 settingID = settingDisplay->settingID;
        const CSSetting* setting = cs_get_setting(groupID, settingID);

        if (currViewIndex >= sNumDisplayedSettings) {
            break;
        }

        if (setting == NULL) {
            break;
        }

        u32 y = TEXT_Y(line + i);

        if (currViewIndex == sSettingsSelectedIndex) {
            cs_draw_row_selection_box(y);
        }

        // Print button options differently:
        if (groupID == CS_OPT_GROUP_BUTTONS) {
            RGBA32 buttonColor = COLOR_RGBA32_CRASH_SETTINGS_DESCRIPTION;
            _Bool buttonCond = FALSE;

            switch (currViewIndex) {
                case CS_OPT_BUTTON_RESET_TO_DEFAULTS:
                    buttonColor = COLOR_RGBA32_CRASH_NO;
                    buttonCond = cs_settings_apply_func_to_all(cs_setting_func_is_non_default);
                    break;
                case CS_OPT_BUTTON_EXPAND_ALL:
                case CS_OPT_BUTTON_COLLAPSE_ALL:
                    buttonCond = cs_settings_check_for_header_state(currViewIndex == CS_OPT_BUTTON_COLLAPSE_ALL);
                    break;
                default:
                    break;
            }

            s32 centeredDefaultsStartX = TEXT_X((CRASH_SCREEN_NUM_CHARS_X / 2) - ((STRLEN("<") + strlen(setting->name) + STRLEN(">")) / 2));

            // "<[button name]>"
            if (buttonCond) {
                cs_print(
                    centeredDefaultsStartX, y,
                    STR_COLOR_PREFIX"<"STR_COLOR_PREFIX"%s"STR_COLOR_PREFIX">",
                    COLOR_RGBA32_CRASH_SELECT_ARROW,
                    buttonColor, setting->name,
                    COLOR_RGBA32_CRASH_SELECT_ARROW
                );
            } else {
                cs_print(
                    centeredDefaultsStartX, y,
                    STR_COLOR_PREFIX"<%s>",
                    COLOR_RGBA32_CRASH_SETTINGS_DISABLED, setting->name
                );
            }
        } else if (setting->type == CS_OPT_TYPE_HEADER) { // Header entry.
            cs_draw_triangle(TEXT_X(0), y, TEXT_WIDTH(1), TEXT_WIDTH(1), COLOR_RGBA32_CRASH_PAGE_NAME, (setting->val ? CS_TRI_DOWN : CS_TRI_RIGHT));
            cs_print(
                TEXT_X(section_indent), y,
                STR_COLOR_PREFIX"%s",
                COLOR_RGBA32_CRASH_PAGE_NAME, setting->name
            );
            // Translucent divider.
            cs_draw_divider_translucent(DIVIDER_Y((line + i) + 1));
        } else { // Normal setting
            // Maximum description print size.
            u32 charX = (CRASH_SCREEN_NUM_CHARS_X - (STRLEN("*<") + VALUE_NAME_SIZE + STRLEN(">")));

            cs_print_scroll(
                TEXT_X(section_indent), y, (charX - section_indent),
                STR_COLOR_PREFIX"%s",
                COLOR_RGBA32_CRASH_SETTINGS_DESCRIPTION, setting->name
            );

            // "<"
            charX += cs_print(TEXT_X(charX), y,
                (STR_COLOR_PREFIX"<"),
                COLOR_RGBA32_CRASH_SELECT_ARROW
            );

            // Print the current setting.
            if (setting->valNames != NULL) {
                RGBA32 nameColor = COLOR_RGBA32_CRASH_SETTINGS_NAMED;

                // Booleans color.
                if (setting->valNames == &gValNames_bool) {
                    nameColor = (setting->val ? COLOR_RGBA32_CRASH_YES : COLOR_RGBA32_CRASH_NO);
                }

                // "[setting value (string)]"
                cs_print(TEXT_X(charX), y,
                    (STR_COLOR_PREFIX"%s"),
                    nameColor, (*setting->valNames)[setting->val]
                );
            } else {
                // "[setting value (number)]"
                cs_print(TEXT_X(charX), y,
                    (STR_COLOR_PREFIX"%-d"),
                    COLOR_RGBA32_CRASH_SETTINGS_NUMERIC, setting->val
                );
            }
            charX += VALUE_NAME_SIZE;

            // ">"
            charX += cs_print(TEXT_X(charX), y,
                (STR_COLOR_PREFIX">"),
                COLOR_RGBA32_CRASH_SELECT_ARROW
            );

            // Print an asterisk if the setting has been changed from the default value.
            if (setting->val != setting->defaultVal) {
                // "*"
                cs_print(TEXT_X(charX), y,
                    (STR_COLOR_PREFIX"*"),
                    COLOR_RGBA32_CRASH_SETTINGS_DESCRIPTION
                );
            }
        }
    }
}

void settings_draw(void) {
    osWritebackDCacheAll();

    u32 line = 2;

    print_settings_list(line, SETTINGS_NUM_ROWS);

    // Draw this line again so the selection box doesn't get drawn in front of it.
    cs_draw_divider(DIVIDER_Y(line));

    // Scroll Bar:
    if (sNumDisplayedSettings > SETTINGS_NUM_ROWS) {
        cs_draw_scroll_bar(
            (DIVIDER_Y(line) + 1), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y),
            SETTINGS_NUM_ROWS, sNumDisplayedSettings,
            sSettingsViewportIndex,
            COLOR_RGBA32_CRASH_DIVIDER, TRUE
        );
        cs_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));
    }
}

void settings_input(void) {
    CSSettingDisplay* settingDisplay = &gCSDisplayedSettings[sSettingsSelectedIndex];
    s16 groupID   = settingDisplay->groupID;
    s16 settingID = settingDisplay->settingID;
    CSSetting* setting = cs_get_setting(groupID, settingID);
    u16 buttonPressed = gCSCompositeController->buttonPressed;

    // Handle buttons group differently.
    if (groupID == CS_OPT_GROUP_BUTTONS) {
        switch (settingID) {
            case CS_OPT_BUTTON_RESET_TO_DEFAULTS:
                if (buttonPressed & (A_BUTTON | B_BUTTON)) {
                    cs_settings_apply_func_to_all(cs_setting_func_reset);
                }
                break;
            case CS_OPT_BUTTON_EXPAND_ALL:
            case CS_OPT_BUTTON_COLLAPSE_ALL:
                if (buttonPressed & (A_BUTTON | B_BUTTON)) {
                    cs_settings_set_all_headers(settingID == CS_OPT_BUTTON_EXPAND_ALL);
                }
                break;
            default:
                break;
        }
    } else {
        if ((gCSCompositeController->buttonDown & (A_BUTTON | B_BUTTON)) == (A_BUTTON | B_BUTTON)) { // Reset combo
            if (setting->type == CS_OPT_TYPE_HEADER) {
                // Resetting a header resets the whole section.
                cs_settings_apply_func_to_all_in_group(cs_setting_func_reset, groupID);
            } else {
                cs_setting_func_reset(groupID, settingID);
            }
        } else {
            if (gCSDirectionFlags.pressed.left  || (buttonPressed & B_BUTTON)) cs_inc_setting(groupID, settingID, -1); // Decrement + wrap.
            if (gCSDirectionFlags.pressed.right || (buttonPressed & A_BUTTON)) cs_inc_setting(groupID, settingID, +1); // Increment + wrap.
        }
    }

    update_displayed_settings();

    s32 change = 0;
    if (gCSDirectionFlags.pressed.up  ) change = -1; // Scroll up.
    if (gCSDirectionFlags.pressed.down) change = +1; // Scroll down.
    sSettingsSelectedIndex = WRAP(((s32)sSettingsSelectedIndex + change), 0, ((s32)sNumDisplayedSettings - 1));

    sSettingsViewportIndex = cs_clamp_view_to_selection(sSettingsViewportIndex, sSettingsSelectedIndex, SETTINGS_NUM_ROWS, 1);

    u32 lastViewportIndex = MAX(((s32)sNumDisplayedSettings - SETTINGS_NUM_ROWS), 0);
    if (sSettingsViewportIndex > lastViewportIndex) {
        sSettingsViewportIndex = lastViewportIndex;
    }
}


struct CSPage gCSPage_settings = {
    .name         = "SETTINGS",
    .initFunc     = settings_init,
    .drawFunc     = settings_draw,
    .inputFunc    = settings_input,
    .contList     = cs_cont_list_settings,
    .settingsList = NULL,
    .flags = {
        .initialized = FALSE,
        .crashed     = FALSE,
        .printName   = TRUE,
    },
};
