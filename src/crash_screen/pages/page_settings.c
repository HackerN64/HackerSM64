#include <ultra64.h>
#include <string.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen/crash_main.h"
#include "page_settings.h"
#include "game/input.h"


u32 sSettingsSelectedIndex = 0;
static u32 sSettingsViewportIndex = 0;


const enum ControlTypes settingsContList[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_SCROLL_LIST,
    CONT_DESC_LIST_END,
};

void settings_init(void) {
    sSettingsSelectedIndex = 0;
    sSettingsViewportIndex = 0;
}

extern const char* sValNames_bool[];

void print_settings_list(u32 line, u32 numLines) {
    u32 currIndex = sSettingsViewportIndex;
    const struct CSSettingsEntry* setting = &gCSSettings[currIndex];

    // Print
    for (u32 i = 0; i < numLines; i++) {
        if (currIndex >= NUM_CS_OPTS) {
            break;
        }

        if (setting == NULL) {
            break;
        }

        u32 y = TEXT_Y(line + i);

        if (currIndex == sSettingsSelectedIndex) {
            crash_screen_draw_row_selection_box(y);
        }

        // "[setting name]"
        u32 settingDescEndCharX = (CRASH_SCREEN_NUM_CHARS_X - (VALUE_NAME_SIZE + 1));
        crash_screen_print_scroll(TEXT_X(0), y, settingDescEndCharX,
            STR_COLOR_PREFIX"%s",
            COLOR_RGBA32_CRASH_SETTINGS_DESCRIPTION, setting->name
        );

        // Left arrow.
        crash_screen_print(TEXT_X(settingDescEndCharX), y,
            (STR_COLOR_PREFIX"%c"),
            COLOR_RGBA32_CRASH_SELECT_ARROW, '<'
        );
        // Right arrow.
        crash_screen_print(TEXT_X(settingDescEndCharX + VALUE_NAME_SIZE), y,
            (STR_COLOR_PREFIX"%c"),
            COLOR_RGBA32_CRASH_SELECT_ARROW, '>'
        );

        u32 x = TEXT_X(settingDescEndCharX + 1);

        const char* name = NULL;

        if (setting->valNames != NULL) {
            name = setting->valNames[setting->val];
        }

        if (name != NULL) {
            RGBA32 nameColor = COLOR_RGBA32_CRASH_SETTINGS_NAMED;

            if (setting->valNames == sValNames_bool) {
                nameColor = ((setting->val) ? COLOR_RGBA32_CRASH_YES : COLOR_RGBA32_CRASH_NO);
            }

            // "[setting value (string)]"
            crash_screen_print(x, y,
                (STR_COLOR_PREFIX"%s"),
                nameColor, name
            );
        } else {
            // "[setting value (number)]"
            crash_screen_print(x, y,
                (STR_COLOR_PREFIX"%-d"),
                COLOR_RGBA32_CRASH_SETTINGS_NUMERIC, setting->val
            );
        }

        currIndex++;
        setting++;
    }
}

void settings_draw(void) {
    osWritebackDCacheAll();

    print_settings_list(2, SETTINGS_NUM_ROWS);

    // Draw this line again so the selection box doesn't get drawn in front of it.
    crash_screen_draw_divider(DIVIDER_Y(2));

    // Scroll Bar:
    if (NUM_CS_OPTS > SETTINGS_NUM_ROWS) {
        crash_screen_draw_scroll_bar(
            (DIVIDER_Y(2) + 1), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y),
            SETTINGS_NUM_ROWS, NUM_CS_OPTS,
            sSettingsViewportIndex,
            COLOR_RGBA32_CRASH_DIVIDER, TRUE
        );
        crash_screen_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));
    }
}

void settings_input(void) {
    u32 currIndex = sSettingsSelectedIndex;
    struct CSSettingsEntry* setting = &gCSSettings[currIndex];
    u16 buttonPressed = gPlayer1Controller->buttonPressed;

    if (gCSDirectionFlags.pressed.left  || (buttonPressed & B_BUTTON)) {
        // Decrement/wrap value.
        setting->val--;

        if (setting->val < setting->lowerBound) {
            setting->val = setting->upperBound;
        }
    }
    if (gCSDirectionFlags.pressed.right || (buttonPressed & A_BUTTON)) {
        // Increment/wrap value.
        setting->val++;

        if (setting->val > setting->upperBound) {
            setting->val = setting->lowerBound;
        }
    }

    if (gCSDirectionFlags.pressed.up) {
        // Scroll up.
        if (sSettingsSelectedIndex > 0) {
            sSettingsSelectedIndex--;
        }
    }
    if (gCSDirectionFlags.pressed.down) {
        // Scroll down.
        if (sSettingsSelectedIndex < (NUM_CS_OPTS - 1)) {
            sSettingsSelectedIndex++;
        }
    }

    sSettingsViewportIndex = clamp_view_to_selection(sSettingsViewportIndex, sSettingsSelectedIndex, SETTINGS_NUM_ROWS, 1);
}
