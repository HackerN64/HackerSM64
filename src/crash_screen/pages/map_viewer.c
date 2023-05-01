#include <ultra64.h>
#include <string.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen/crash_screen.h"
#include "map_viewer.h"
#include "game/game_input.h"


u32 sMapViewerSelectedIndex = 0;
static u32 sMapViewerViewportIndex = 0;


const enum ControlTypes mapViewerContList[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_SCROLL_LIST,
    CONT_DESC_JUMP_TO_ADDRESS,
    CONT_DESC_LIST_END,
};


void map_viewer_init(void) {
    sMapViewerSelectedIndex = 0;
    sMapViewerViewportIndex = 0;
}

void map_viewer_print_entries(u32 line, u32 numLines) {
    u32 currIndex = sMapViewerViewportIndex;
    const struct MapEntry* entry = &gMapEntries[currIndex];

    // Print
    for (u32 i = 0; i < numLines; i++) {
        if (currIndex >= gNumMapEntries) {
            break;
        }

        if (entry == NULL) {
            break;
        }

        u32 y = TEXT_Y(line + i);

        if (currIndex == sMapViewerSelectedIndex) {
            crash_screen_draw_rect(
                (TEXT_X(0) - 1), (y - 2),
                (CRASH_SCREEN_TEXT_W + 1), (TEXT_HEIGHT(1) + 1),
                COLOR_RGBA32_CRASH_SELECT
            );
        }

        const size_t sizeStrSize = STRLEN("00000");
        // "[stack address]:"
        const size_t addrStrSize = crash_screen_print(TEXT_X(0), y, STR_HEX_WORD":", entry->addr);
        // "[name from map data]"
        crash_screen_print_map_name(TEXT_X(addrStrSize), y,
            (CRASH_SCREEN_NUM_CHARS_X - (addrStrSize + sizeStrSize + 1)),
            (is_in_code_segment(entry->addr) ? COLOR_RGBA32_CRASH_FUNCTION_NAME : COLOR_RGBA32_VERY_LIGHT_CYAN), get_map_entry_name(entry)
        );
        // "[size]"
        crash_screen_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - sizeStrSize), y,
            (STR_COLOR_PREFIX "%-X"),
            COLOR_RGBA32_CRASH_FUNCTION_NAME_2, entry->size
        );

        currIndex++;
        entry++;
    }

    osWritebackDCacheAll();
}

void map_viewer_draw(void) {
    u32 line = 1;
#ifdef INCLUDE_DEBUG_MAP
    sMapViewerViewportIndex = clamp_view_to_selection(sMapViewerViewportIndex, sMapViewerSelectedIndex, MAP_VIEWER_NUM_ROWS, 1);

    crash_screen_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - STRLEN("SIZE")), TEXT_Y(line), STR_COLOR_PREFIX"SIZE:", COLOR_RGBA32_CRASH_FUNCTION_NAME_2);

    line++;

    map_viewer_print_entries(line, MAP_VIEWER_NUM_ROWS);

    // Draw the line after the entries so the selection box is behind it.
    crash_screen_draw_divider(DIVIDER_Y(line));

    // Scroll Bar
    if (gNumMapEntries > MAP_VIEWER_NUM_ROWS) {
        crash_screen_draw_scroll_bar((DIVIDER_Y(line) + 1), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y), MAP_VIEWER_NUM_ROWS, gNumMapEntries, sMapViewerViewportIndex, COLOR_RGBA32_LIGHT_GRAY, TRUE);

        crash_screen_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));
    }
#else
    line++;

    // "MAP VIEWER DISABLED"
    crash_screen_print(TEXT_X(0), TEXT_Y(line), "MAP VIEWER DISABLED");
#endif

    osWritebackDCacheAll();
}

void map_viewer_input(void) {
    u16 buttonPressed = gPlayer1Controller->buttonPressed;

    if (buttonPressed & A_BUTTON) {
        open_address_select(gMapEntries[sMapViewerSelectedIndex].addr);
    }

    if (gCSDirectionFlags.pressed.up) {
        // Scroll up.
        if (sMapViewerSelectedIndex > 0) {
            sMapViewerSelectedIndex--;
        }
    }
    if (gCSDirectionFlags.pressed.down) {
        // Scroll down.
        if (sMapViewerSelectedIndex < (gNumMapEntries - 1)) {
            sMapViewerSelectedIndex++;
        }
    }
}
