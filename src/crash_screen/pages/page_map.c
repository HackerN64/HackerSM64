#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/util/map_parser.h"
#include "crash_screen/cs_controls.h"
#include "crash_screen/cs_draw.h"
#include "crash_screen/cs_descriptions.h"
#include "crash_screen/cs_main.h"
#include "crash_screen/cs_pages.h"
#include "crash_screen/cs_print.h"
#include "crash_screen/cs_settings.h"

#include "crash_screen/popups/popup_address.h"

#include "page_map.h"

#ifdef UNF
#include "usb/usb.h"
#include "usb/debug.h"
#endif // UNF


#ifdef INCLUDE_DEBUG_MAP

struct CSSetting cs_settings_group_page_map[] = {
    [CS_OPT_HEADER_PAGE_MAP         ] = { .type = CS_OPT_TYPE_HEADER,  .name = "MAP SYMBOLS",                    .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_MAP_SHOW_ADDRESSES      ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show symbol addresses",          .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_MAP_SHOW_TYPES          ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show symbol types",              .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_MAP_SHOW_SIZES          ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show symbol sizes",              .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_END_MAP                 ] = { .type = CS_OPT_TYPE_END, },
};


const enum ControlTypes cs_cont_list_map[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_PAGE_SELECT,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_HIDE_CRASH_SCREEN,
#ifdef UNF
    CONT_DESC_OS_PRINT,
#endif // UNF
    CONT_DESC_SCROLL_LIST,
    CONT_DESC_JUMP_TO_ADDRESS,
    CONT_DESC_LIST_END,
};


MapSymbolIndex sMapViewerSelectedIndex = 0;
static MapSymbolIndex sMapViewerViewportIndex = 0;


void page_map_init(void) {
    s32 newIndex = get_symbol_index_from_addr_backward(gInspectThread->context.pc);
    sMapViewerSelectedIndex = (newIndex != -1) ? newIndex : 0;
    sMapViewerViewportIndex = cs_clamp_view_to_selection(sMapViewerViewportIndex, sMapViewerSelectedIndex, MAP_VIEWER_NUM_ROWS, 1);
}

void map_viewer_print_entries(CSTextCoord_u32 line, CSTextCoord_u32 numLines) {
    _Bool showAddresses = cs_get_setting_val(CS_OPT_GROUP_PAGE_MAP, CS_OPT_MAP_SHOW_ADDRESSES);
    _Bool showTypes     = cs_get_setting_val(CS_OPT_GROUP_PAGE_MAP, CS_OPT_MAP_SHOW_TYPES    );
    _Bool showSizes     = cs_get_setting_val(CS_OPT_GROUP_PAGE_MAP, CS_OPT_MAP_SHOW_SIZES    );
    MapSymbolIndex currIndex = sMapViewerViewportIndex;
    const MapSymbol* symbol = &gMapSymbols[currIndex];

    // Print.
    for (CSTextCoord_u32 row = 0; row < numLines; row++) {
        if (currIndex >= gNumMapSymbols) {
            break;
        }

        if (symbol == NULL) {
            break;
        }

        ScreenCoord_u32 y = TEXT_Y(line + row);

        if (currIndex == sMapViewerSelectedIndex) {
            cs_draw_row_selection_box(y);
        }

        CSTextCoord_u32 addrStrSize = 0;
        if (showAddresses) {
            // "[stack address]:"
            addrStrSize += cs_print(TEXT_X(addrStrSize), y, STR_HEX_WORD":", symbol->addr);
        }

        CSTextCoord_u32 sizeStrSize = 0;
        if (showSizes) {
            sizeStrSize = STRLEN("00000");

            // "[size]"
            cs_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - sizeStrSize), y,
                (STR_COLOR_PREFIX"%-X"STR_COLOR_PREFIX"%c"),
                COLOR_RGBA32_CRASH_OFFSET, symbol->size,
                COLOR_RGBA32_CRASH_UNKNOWN, (symbol->errc == 'S') ? '?' : '\0'
            );
        }

        CSTextCoord_u32 typeStrSize = 0;
        if (showTypes) {
            typeStrSize = STRLEN("T") + showSizes; // Add a space if both types and sizes are shown.
            // "[type]"
            cs_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - (typeStrSize + sizeStrSize)), y,
                (STR_COLOR_PREFIX"%c"),
                COLOR_RGBA32_CRASH_MAP_SYMBOL_TYPE, symbol->type
            );
        }

        // "[name from map data]"
        cs_print_symbol_name(TEXT_X(addrStrSize), y, (CRASH_SCREEN_NUM_CHARS_X - (addrStrSize + typeStrSize + sizeStrSize)), symbol, TRUE);

        currIndex++;
        symbol++;
    }

    osWritebackDCacheAll();
}

void page_map_draw(void) {
    CSTextCoord_u32 line = 1;
    CSTextCoord_u32 charX = 0;
    _Bool showAddr = cs_get_setting_val(CS_OPT_GROUP_PAGE_MAP, CS_OPT_MAP_SHOW_ADDRESSES);
    _Bool showType = cs_get_setting_val(CS_OPT_GROUP_PAGE_MAP, CS_OPT_MAP_SHOW_TYPES);
    _Bool showSize = cs_get_setting_val(CS_OPT_GROUP_PAGE_MAP, CS_OPT_MAP_SHOW_SIZES);

    if (showAddr) {
        // "ADDRESS:"
        cs_print(TEXT_X(0), TEXT_Y(line), "ADDRESS:");
        charX = STRLEN("00000000:");
    }

    cs_print(TEXT_X(charX), TEXT_Y(line), STR_COLOR_PREFIX"NAME:", COLOR_RGBA32_CRASH_FUNCTION_NAME);

    CSTextCoord_u32 sizeStrSize = 0;
    if (showSize) {
        sizeStrSize = STRLEN("SIZE:");
        // "SIZE:"
        cs_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - sizeStrSize), TEXT_Y(line), STR_COLOR_PREFIX"SIZE:", COLOR_RGBA32_CRASH_OFFSET);
    }

    CSTextCoord_u32 typeStrSize = 0;
    if (showType) {
        typeStrSize = STRLEN("TYPE:");
        // "TYPE:"
        cs_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - (typeStrSize + sizeStrSize)), TEXT_Y(line), STR_COLOR_PREFIX"TYPE:", COLOR_RGBA32_CRASH_MAP_SYMBOL_TYPE);
    }

    line++;

    map_viewer_print_entries(line, MAP_VIEWER_NUM_ROWS);

    // Draw the line after the entries so the selection box is behind it.
    cs_draw_divider(DIVIDER_Y(line));

    // Scroll Bar:
    if (gNumMapSymbols > MAP_VIEWER_NUM_ROWS) {
        cs_draw_scroll_bar(
            (DIVIDER_Y(line) + 1), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y),
            MAP_VIEWER_NUM_ROWS, gNumMapSymbols,
            sMapViewerViewportIndex,
            COLOR_RGBA32_CRASH_SCROLL_BAR, TRUE
        );

        cs_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));
    }

    osWritebackDCacheAll();
}

void page_map_input(void) {
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

    sMapViewerViewportIndex = cs_clamp_view_to_selection(sMapViewerViewportIndex, sMapViewerSelectedIndex, MAP_VIEWER_NUM_ROWS, 1);
}

void page_map_print(void) {
#ifdef UNF
    osSyncPrintf("\n");

    MapSymbolIndex currIndex = sMapViewerViewportIndex;
    osSyncPrintf("SECTION: ["STR_HEX_WORD"-"STR_HEX_WORD"]\n", gMapSymbols[currIndex].addr, gMapSymbols[currIndex + (MAP_VIEWER_NUM_ROWS - 1)].addr);
    const MapSymbol* symbol = &gMapSymbols[currIndex];

    for (MapSymbolIndex i = 0; i < MAP_VIEWER_NUM_ROWS; i++) {
        if (currIndex >= gNumMapSymbols) {
            break;
        }

        if (symbol == NULL) {
            break;
        }

        const char* name = get_map_symbol_name(symbol);
        if (name != NULL) {
            osSyncPrintf("- ["STR_HEX_WORD"]: %s [TYPE: \'%c\'] (SIZE: "STR_HEX_PREFIX"%X)\n", symbol->addr, name, symbol->type, symbol->size);
        }

        currIndex++;
        symbol++;
    }
#endif // UNF
}


struct CSPage gCSPage_map = {
    .name         = "MAP SYMBOLS",
    .initFunc     = page_map_init,
    .drawFunc     = page_map_draw,
    .inputFunc    = page_map_input,
    .printFunc    = page_map_print,
    .contList     = cs_cont_list_map,
    .settingsList = cs_settings_group_page_map,
    .flags = {
        .initialized = FALSE,
        .crashed     = FALSE,
    },
};

#endif // INCLUDE_DEBUG_MAP
