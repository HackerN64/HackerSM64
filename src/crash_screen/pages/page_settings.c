#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/crash_controls.h"
#include "crash_screen/crash_draw.h"
#include "crash_screen/crash_print.h"
#include "crash_screen/crash_settings.h"

#include "page_settings.h"


const enum ControlTypes settingsContList[] = {
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
static u32 sSettngsTotalShownAmount = NUM_CS_OPTS;

enum CSSettings gCSDisplayedSettingIDs[NUM_CS_OPTS];


void update_displayed_settings(void) {
    bzero(&gCSDisplayedSettingIDs, sizeof(gCSDisplayedSettingIDs));

    int dstSettingIndex = 0;
    _Bool sectionShown = TRUE;

    for (int srcSettingIndex = 0; srcSettingIndex < NUM_CS_OPTS; srcSettingIndex++) {
        _Bool show = sectionShown;

        // Header entry:
        if (crash_screen_setting_is_header(srcSettingIndex)) {
            // Set whether the next section is shown:
            sectionShown = gCSSettings[srcSettingIndex].val;
            show = TRUE;
        }

        // If section is not collapsed:
        if (show) {
            gCSDisplayedSettingIDs[dstSettingIndex] = srcSettingIndex;
            dstSettingIndex++;
        }
    }

    sSettngsTotalShownAmount = dstSettingIndex;
}

void settings_init(void) {
    sSettingsSelectedIndex = 0;
    sSettingsViewportIndex = 0;

    update_displayed_settings();
}

extern const char* sValNames_bool[];

void print_settings_list(u32 line, u32 numLines) {
    u32 currViewIndex = sSettingsViewportIndex;
    const u32 section_indent = STRLEN("> ");

    // Print
    for (u32 i = 0; i < numLines; i++) {
        u32 currSettingIndex = gCSDisplayedSettingIDs[currViewIndex];
        const struct CSSettingsEntry* setting = &gCSSettings[currSettingIndex];

        if (currViewIndex >= sSettngsTotalShownAmount) {
            break;
        }

        if (setting == NULL) {
            break;
        }

        u32 y = TEXT_Y(line + i);

        if (currViewIndex == sSettingsSelectedIndex) {
            crash_screen_draw_row_selection_box(y);
        }

        // Print the "RESET TO DEFAULTS" and header options differently:
        if (currViewIndex == CS_OPT_RESET_TO_DEFAULTS) { // Reset all to defaults entry.
            s32 centeredDefaultsStartX = TEXT_X((CRASH_SCREEN_NUM_CHARS_X / 2) - (STRLEN("<RESET ALL TO DEFAULTS>") / 2));
            // "<[setting name]>"
            if (crash_screen_check_for_changed_settings()) {
                crash_screen_print(
                    centeredDefaultsStartX, y,
                    STR_COLOR_PREFIX"<"STR_COLOR_PREFIX"%s"STR_COLOR_PREFIX">",
                    COLOR_RGBA32_CRASH_SELECT_ARROW,
                    COLOR_RGBA32_CRASH_NO, setting->name,
                    COLOR_RGBA32_CRASH_SELECT_ARROW
                );
            } else {
                crash_screen_print(
                    centeredDefaultsStartX, y,
                    STR_COLOR_PREFIX"<%s>",
                    COLOR_RGBA32_CRASH_SETTINGS_DISABLED, setting->name
                );
            }
        } else if (crash_screen_setting_is_header(currSettingIndex)) { // Header entry.
            crash_screen_draw_triangle(TEXT_X(0), y, TEXT_WIDTH(1), TEXT_WIDTH(1), COLOR_RGBA32_CRASH_PAGE_NAME, (setting->val ? CS_TRI_DOWN : CS_TRI_RIGHT));
            crash_screen_print(
                TEXT_X(section_indent), y,
                STR_COLOR_PREFIX"%s",
                COLOR_RGBA32_CRASH_PAGE_NAME, setting->name
            );
            // Translucent divider.
            crash_screen_draw_divider_translucent(DIVIDER_Y((line + i) + 1));
        } else { // Setting entry.
            // Print an asterisk if the setting has been changed from the default value.
            if (setting->val != setting->defaultVal) {
                // "*"
                crash_screen_print(TEXT_X(0), y,
                    (STR_COLOR_PREFIX"*"),
                    COLOR_RGBA32_CRASH_SETTINGS_DESCRIPTION
                );
            }

            // Maximum description print size.
            u32 charX = (CRASH_SCREEN_NUM_CHARS_X - (STRLEN("*<") + VALUE_NAME_SIZE + STRLEN(">")));

            crash_screen_print_scroll(
                TEXT_X(section_indent), y, (charX - section_indent),
                STR_COLOR_PREFIX"%s",
                COLOR_RGBA32_CRASH_SETTINGS_DESCRIPTION, setting->name
            );

            // "<"
            charX += crash_screen_print(TEXT_X(charX), y,
                (STR_COLOR_PREFIX"<"),
                COLOR_RGBA32_CRASH_SELECT_ARROW
            );

            // Print the current setting.
            if (setting->valNames != NULL) {
                RGBA32 nameColor = COLOR_RGBA32_CRASH_SETTINGS_NAMED;

                // Booleans color.
                if (setting->valNames == &sValNames_bool) {
                    nameColor = (setting->val ? COLOR_RGBA32_CRASH_YES : COLOR_RGBA32_CRASH_NO);
                }

                // "[setting value (string)]"
                crash_screen_print(TEXT_X(charX), y,
                    (STR_COLOR_PREFIX"%s"),
                    nameColor, (*setting->valNames)[setting->val]
                );
            } else {
                // "[setting value (number)]"
                crash_screen_print(TEXT_X(charX), y,
                    (STR_COLOR_PREFIX"%-d"),
                    COLOR_RGBA32_CRASH_SETTINGS_NUMERIC, setting->val
                );
            }
            charX += VALUE_NAME_SIZE;

            // ">"
            crash_screen_print(TEXT_X(charX), y,
                (STR_COLOR_PREFIX">"),
                COLOR_RGBA32_CRASH_SELECT_ARROW
            );
        }

        currViewIndex++;
    }
}

void settings_draw(void) {
    osWritebackDCacheAll();

    u32 line = 2;

    print_settings_list(line, SETTINGS_NUM_ROWS);

    // Draw this line again so the selection box doesn't get drawn in front of it.
    crash_screen_draw_divider(DIVIDER_Y(line));

    // Scroll Bar:
    if (sSettngsTotalShownAmount > SETTINGS_NUM_ROWS) {
        crash_screen_draw_scroll_bar(
            (DIVIDER_Y(line) + 1), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y),
            SETTINGS_NUM_ROWS, sSettngsTotalShownAmount,
            sSettingsViewportIndex,
            COLOR_RGBA32_CRASH_DIVIDER, TRUE
        );
        crash_screen_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));
    }
}

void settings_input(void) {
    u32 selectedSettingIndex = gCSDisplayedSettingIDs[sSettingsSelectedIndex];
    u16 buttonPressed = gCSCompositeController->buttonPressed;

    // Handle the reset to defaults entry differently.
    if (selectedSettingIndex == CS_OPT_RESET_TO_DEFAULTS) {
        if (buttonPressed & (A_BUTTON | B_BUTTON)) {
            crash_screen_reset_all_settings();
        }
    } else {
        if ((gCSCompositeController->buttonDown & (A_BUTTON | B_BUTTON)) == (A_BUTTON | B_BUTTON)) {
            if (crash_screen_setting_is_header(selectedSettingIndex)) {
                // Resetting a header resets the whole section.
                crash_screen_reset_settings_section(selectedSettingIndex);
            } else {
                crash_screen_reset_setting(selectedSettingIndex);
            }
        } else {
            if (gCSDirectionFlags.pressed.left  || (buttonPressed & B_BUTTON)) crash_screen_inc_setting(selectedSettingIndex, -1); // Decrement + wrap.
            if (gCSDirectionFlags.pressed.right || (buttonPressed & A_BUTTON)) crash_screen_inc_setting(selectedSettingIndex, +1); // Increment + wrap.
        }
    }

    update_displayed_settings();

    s32 change = 0;
    if (gCSDirectionFlags.pressed.up  ) change = -1; // Scroll up.
    if (gCSDirectionFlags.pressed.down) change = +1; // Scroll down.
    sSettingsSelectedIndex = WRAP(((s32)sSettingsSelectedIndex + change), 0, ((s32)sSettngsTotalShownAmount - 1));

    sSettingsViewportIndex = clamp_view_to_selection(sSettingsViewportIndex, sSettingsSelectedIndex, SETTINGS_NUM_ROWS, 1);

    u32 lastViewportIndex = MAX(((s32)sSettngsTotalShownAmount - SETTINGS_NUM_ROWS), 0);
    if (sSettingsViewportIndex > lastViewportIndex) {
        sSettingsViewportIndex = lastViewportIndex;
    }
}
