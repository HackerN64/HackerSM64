#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/address_select.h"
#include "crash_screen/crash_controls.h"
#include "crash_screen/crash_draw.h"
#include "crash_screen/crash_main.h"
#include "crash_screen/crash_print.h"
#include "crash_screen/map_parser.h"

#include "page_map.h"


const enum ControlTypes mapViewerContList[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_SCROLL_LIST,
    CONT_DESC_JUMP_TO_ADDRESS,
    CONT_DESC_LIST_END,
};


u32 sMapViewerSelectedIndex = 0;
static u32 sMapViewerViewportIndex = 0;


void map_view_init(void) {
    s32 newIndex = get_symbol_index_from_addr_backward(gSelectedAddress);
    sMapViewerSelectedIndex = (newIndex != -1) ? newIndex : 0;
    sMapViewerViewportIndex = clamp_view_to_selection(sMapViewerViewportIndex, sMapViewerSelectedIndex, MAP_VIEWER_NUM_ROWS, 1);
}

void map_viewer_print_entries(u32 line, u32 numLines) {
    u32 currIndex = sMapViewerViewportIndex;
    const struct MapSymbol* symbol = &gMapSymbols[currIndex];

    // Print.
    for (u32 i = 0; i < numLines; i++) {
        if (currIndex >= gNumMapSymbols) {
            break;
        }

        if (symbol == NULL) {
            break;
        }

        u32 y = TEXT_Y(line + i);

        if (currIndex == sMapViewerSelectedIndex) {
            crash_screen_draw_row_selection_box(y);
        }

        const size_t typeStrSize = STRLEN("T ");
        const size_t sizeStrSize = STRLEN("00000");
        // "[stack address]:"
        const size_t addrStrSize = crash_screen_print(TEXT_X(0), y, STR_HEX_WORD":", symbol->addr);
        // "[name from map data]"
        crash_screen_print_symbol_name(TEXT_X(addrStrSize), y, (CRASH_SCREEN_NUM_CHARS_X - (addrStrSize + typeStrSize + sizeStrSize)), symbol);

        // "[type]"
        crash_screen_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - (typeStrSize + sizeStrSize)), y,
            (STR_COLOR_PREFIX"%c"),
            COLOR_RGBA32_CRASH_MAP_SYMBOL_TYPE, symbol->type
        );

        // Print size:
        u32 x = TEXT_X(CRASH_SCREEN_NUM_CHARS_X - sizeStrSize);

        if (symbol->errc == 'S') {
            // Size too large
            // "?"
            crash_screen_print(x, y,
                (STR_COLOR_PREFIX"%c"),
                COLOR_RGBA32_CRASH_UNKNOWN, '?'
            );
        } else {
            // "[size]"
            crash_screen_print(x, y,
                (STR_COLOR_PREFIX"%-X"),
                COLOR_RGBA32_CRASH_OFFSET, symbol->size
            );
        }

        currIndex++;
        symbol++;
    }

    osWritebackDCacheAll();
}

void map_view_draw(void) {
    u32 line = 1;

    const size_t typeStrSize = STRLEN("TYPE:");
    const size_t sizeStrSize = STRLEN("SIZE:");

    // "TYPE:"
    crash_screen_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - (typeStrSize + sizeStrSize)), TEXT_Y(line), STR_COLOR_PREFIX"TYPE:", COLOR_RGBA32_CRASH_MAP_SYMBOL_TYPE);

    // "SIZE:"
    crash_screen_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - sizeStrSize), TEXT_Y(line), STR_COLOR_PREFIX"SIZE:", COLOR_RGBA32_CRASH_OFFSET);

    line++;

    map_viewer_print_entries(line, MAP_VIEWER_NUM_ROWS);

    // Draw the line after the entries so the selection box is behind it.
    crash_screen_draw_divider(DIVIDER_Y(line));

    // Scroll Bar:
    if (gNumMapSymbols > MAP_VIEWER_NUM_ROWS) {
        crash_screen_draw_scroll_bar((DIVIDER_Y(line) + 1), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y), MAP_VIEWER_NUM_ROWS, gNumMapSymbols, sMapViewerViewportIndex, COLOR_RGBA32_CRASH_DIVIDER, TRUE);

        crash_screen_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));
    }

    osWritebackDCacheAll();
}

void map_view_input(void) {
    if (gCSSwitchedPage) {
        s32 targetIndex = get_symbol_index_from_addr_backward(gSelectedAddress);

        if (targetIndex != -1) {
            sMapViewerSelectedIndex = targetIndex;
        }
    }

    if (gCSCompositeController->buttonPressed & A_BUTTON) {
        open_address_select(gMapSymbols[sMapViewerSelectedIndex].addr);
    }

    if (gCSDirectionFlags.pressed.up) {
        // Scroll up.
        if (sMapViewerSelectedIndex > 0) {
            sMapViewerSelectedIndex--;
        }
    }
    if (gCSDirectionFlags.pressed.down) {
        // Scroll down.
        if (sMapViewerSelectedIndex < (gNumMapSymbols - 1)) {
            sMapViewerSelectedIndex++;
        }
    }

    sMapViewerViewportIndex = clamp_view_to_selection(sMapViewerViewportIndex, sMapViewerSelectedIndex, MAP_VIEWER_NUM_ROWS, 1);
}
