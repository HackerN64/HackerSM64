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

#include "page_segments.h"

#ifdef UNF
#include "usb/usb.h"
#include "usb/debug.h"
#endif // UNF

struct CSSetting cs_settings_group_page_segments[] = {
    [CS_OPT_HEADER_PAGE_SEGMENTS    ] = { .type = CS_OPT_TYPE_HEADER,  .name = "SEGMENTS",                       .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_SEGMENTS_SHOW_ADDRESSES ] = { .type = CS_OPT_TYPE_SETTING, .name = "Show segment addresses",         .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_END_SEGMENTS            ] = { .type = CS_OPT_TYPE_END, },
};


const enum ControlTypes cs_cont_list_segments[] = {
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


u32 sSegmentsSelectedIndex = 0;
static u32 sSegmentsViewportIndex = 0;


void page_segments_init(void) {
    sSegmentsSelectedIndex = 0;
    sSegmentsViewportIndex = 0;
}

void draw_segments_list(CSTextCoord_u32 line) {
    _Bool showAddresses = cs_get_setting_val(CS_OPT_GROUP_PAGE_SEGMENTS, CS_OPT_SEGMENTS_SHOW_ADDRESSES);
    u32 currIndex = sSegmentsViewportIndex;

    for (CSTextCoord_u32 row = 0; row < (NUM_SHOWN_SEGMENTS * 2); row += 2) {
        ScreenCoord_u32 y = TEXT_Y(line + row);

        if (currIndex == sSegmentsSelectedIndex) {
            cs_draw_row_selection_box_2(y);
        }

        cs_draw_divider_translucent(DIVIDER_Y(line + row));

        Address start = (uintptr_t)get_segment_base_addr(currIndex);
        size_t size = get_segment_size(currIndex);
        Address romStart = sSegmentROMTable[currIndex];
        _Bool isLoaded = (romStart != (Address)NULL);

        // Line 1:
        CSTextCoord_u32 textEnd = CRASH_SCREEN_NUM_CHARS_X;
        if (showAddresses && isLoaded) {
            textEnd -= STRLEN("00000000-00000000");
            cs_print(TEXT_X(textEnd), y, (STR_HEX_WORD" "STR_COLOR_PREFIX STR_HEX_WORD), start, COLOR_RGBA32_CRASH_HEADER, romStart);
        }
        const RGBA32 segColor = COLOR_RGBA32_LIGHT_CYAN;
        CSTextCoord_u32 textStart = cs_print(TEXT_X(0), y, STR_COLOR_PREFIX"seg%02d:", segColor, currIndex);
        cs_print_scroll(TEXT_X(textStart), y, (textEnd - textStart), STR_COLOR_PREFIX"%s", segColor, get_segment_name(currIndex));

        y += TEXT_HEIGHT(1);

        // Line 2:
        if (showAddresses && isLoaded) {
            const RGBA32 romColor = COLOR_RGBA32_LIGHT_BLUE;
            CSTextCoord_u32 romStrStart = cs_print(TEXT_X(1), y, STR_COLOR_PREFIX"loaded:", COLOR_RGBA32_CRASH_HEADER);
            cs_print_scroll(TEXT_X(1 + romStrStart), y, (textEnd - (1 + romStrStart)), STR_COLOR_PREFIX"%s",
                romColor, get_segment_sub_name(currIndex)
            );
            cs_print(TEXT_X(textEnd + 4), y, (STR_COLOR_PREFIX"size:"STR_HEX_PREFIX"%X"), COLOR_RGBA32_GRAY, size);
        } else {
            cs_print(TEXT_X(1), y, STR_COLOR_PREFIX"unloaded", COLOR_RGBA32_GRAY);
        }

        currIndex++;
    }
}

void page_segments_draw(void) {
    u32 line = 1;

    cs_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"ID:", COLOR_RGBA32_LIGHT_CYAN);
    if (cs_get_setting_val(CS_OPT_GROUP_PAGE_SEGMENTS, CS_OPT_SEGMENTS_SHOW_ADDRESSES)) {
        // cs_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - STRLEN("START-00000000")), TEXT_Y(line), STR_COLOR_PREFIX"START-END:", COLOR_RGBA32_CRASH_HEADER);
        cs_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - STRLEN("00000000 00000000")), TEXT_Y(line), "RAM:     "STR_COLOR_PREFIX"ROM:", COLOR_RGBA32_CRASH_HEADER);
    }
    line++;

    draw_segments_list(line);

    // Draw the line after the entries so the selection box is behind it.
    cs_draw_divider(DIVIDER_Y(line));

    // Scroll Bar:
    cs_draw_scroll_bar(
        (DIVIDER_Y(line) + 1), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y),
        NUM_SHOWN_SEGMENTS, 32,
        sSegmentsViewportIndex,
        COLOR_RGBA32_CRASH_SCROLL_BAR, TRUE
    );

    cs_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));

    osWritebackDCacheAll();

}
void page_segments_input(void) {
    if (gCSCompositeController->buttonPressed & A_BUTTON) {
        if (sSegmentROMTable[sSegmentsSelectedIndex] != (Address)NULL) {
            open_address_select((Address)get_segment_base_addr(sSegmentsSelectedIndex));
        }
    }

    s32 change = 0;
    if (gCSDirectionFlags.pressed.up  ) change = -1; // Scroll up.
    if (gCSDirectionFlags.pressed.down) change = +1; // Scroll down.
    sSegmentsSelectedIndex = WRAP(((s32)sSegmentsSelectedIndex + change), 0, (s32)(32 - 1));

    sSegmentsViewportIndex = cs_clamp_view_to_selection(sSegmentsViewportIndex, sSegmentsSelectedIndex, NUM_SHOWN_SEGMENTS, 1);
}

void page_segments_print(void) {
#ifdef UNF
    osSyncPrintf("\n");

    for (int i = 0; i < 32; i++) {
        Address start = (uintptr_t)get_segment_base_addr(i);
        Address end = (start + get_segment_size(i));
        Address romStart = sSegmentROMTable[i];
        osSyncPrintf("["STR_HEX_WORD"-"STR_HEX_WORD"]: seg%02d %s\t\tloaded: "STR_HEX_WORD" %s\n", start, end, i, get_segment_name(i), romStart, get_segment_sub_name(i));
    }
#endif // UNF
}

struct CSPage gCSPage_segments = {
    .name         = "SEGMENTS",
    .initFunc     = page_segments_init,
    .drawFunc     = page_segments_draw,
    .inputFunc    = page_segments_input,
    .printFunc    = page_segments_print,
    .contList     = cs_cont_list_segments,
    .settingsList = cs_settings_group_page_segments,
    .flags = {
        .initialized = FALSE,
        .crashed     = FALSE,
    },
};
