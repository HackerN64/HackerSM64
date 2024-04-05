#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/cs_controls.h"
#include "crash_screen/cs_descriptions.h"
#include "crash_screen/cs_draw.h"
#include "crash_screen/cs_main.h"
#include "crash_screen/cs_pages.h"
#include "crash_screen/cs_print.h"
#include "crash_screen/cs_settings.h"

#include "popup_page_select.h"


// Page select popup box draw function.
void cs_popup_pages_draw(void) {
    const ScreenCoord_s32 w = 32; // TEXT_WIDTH(32);
    const ScreenCoord_s32 h = 16; // TEXT_HEIGHT(16);
    const ScreenCoord_s32 startX = ((CRASH_SCREEN_NUM_CHARS_X / 2) - (w / 2));
    const ScreenCoord_s32 startY = ((CRASH_SCREEN_NUM_CHARS_Y / 2) - (h / 2));
    const ScreenCoord_s32 bgStartX = (TEXT_X(startX) - (TEXT_WIDTH(1) / 2));
    const ScreenCoord_s32 bgStartY = (TEXT_Y(startY) - (TEXT_HEIGHT(1) / 2));
    const ScreenCoord_s32 bgW = (TEXT_WIDTH(w) + TEXT_WIDTH(1));
    const ScreenCoord_s32 bgH = (TEXT_HEIGHT(h) + (TEXT_HEIGHT(1)));
    cs_draw_dark_rect(
        bgStartX, bgStartY,
        bgW, bgH,
        cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_POPUP_OPACITY)
    );

    CSPopup* currPopup = cs_get_current_popup();

    // "PAGE SELECT:"
    cs_print(TEXT_X((CRASH_SCREEN_NUM_CHARS_X / 2) - (STRLEN("PAGE SELECT:") / 2)), TEXT_Y(startY),
        STR_COLOR_PREFIX"%s:",
        COLOR_RGBA32_CRASH_PAGE_NAME, currPopup->name
    );

    CSTextCoord_u32 line = (startY + 2);
    //! TODO: Scrollable if the list is long enough.
    for (enum CSPages pageID = 0; pageID < CS_NUM_PAGES; pageID++) {
        CSPage* page = gCSPages[pageID];

        if ((page == NULL) || (page->name == NULL)) {
            break;
        }

        if (pageID == gCSPageID) {
            cs_draw_row_selection_box_impl(bgStartX, TEXT_Y((startY + 2) + pageID), bgW, TEXT_HEIGHT(1), COLOR_RGBA32_CRASH_SELECT_HIGHLIGHT);
        }

        cs_draw_divider_translucent_impl(bgStartX, bgW, DIVIDER_Y(line));
        cs_print(TEXT_X(startX + 1), TEXT_Y(line), STR_COLOR_PREFIX"<%02d>: %s", ((page->flags.crashed) ? COLOR_RGBA32_CRASH_AT : COLOR_RGBA32_CRASH_PAGE_NAME), (pageID + 1), page->name);

        line++;
    }

    cs_draw_divider_translucent_impl(bgStartX, bgW, DIVIDER_Y(line));

    cs_draw_outline(bgStartX, bgStartY, bgW, bgH, COLOR_RGBA32_CRASH_DIVIDER);

    osWritebackDCacheAll();
}

void cs_popup_pages_input(void) {
    enum CSPages prevPage = gCSPageID;

    u16 buttonPressed = gCSCompositeController->buttonPressed;

    if (buttonPressed & (A_BUTTON | B_BUTTON | START_BUTTON)) {
        // Close the popup without jumping.
        cs_open_popup(CS_POPUP_NONE);
        return;
    }

    s32 change = 0;
    if (gCSDirectionFlags.pressed.up  ) change = -1; // Scroll up.
    if (gCSDirectionFlags.pressed.down) change = +1; // Scroll down.
    //! TODO: combine this with the normal L/R functionality.
    gCSPageID = WRAP(((s32)gCSPageID + change), CS_FIRST_PAGE, (CS_NUM_PAGES - 1));

    if (gCSPageID != prevPage) {
        // Reset certain values when the page is changed.
        gCSSwitchedPage = TRUE;
        CSPage* page = cs_get_current_page();
        if ((page->initFunc != NULL) && !page->flags.initialized) {
            page->initFunc();
            page->flags.initialized = TRUE;
        }
    }
}

struct CSPopup gCSPopup_pages = {
    .name      = "PAGE SELECT",
    .initFunc  = NULL,
    .drawFunc  = cs_popup_pages_draw,
    .inputFunc = cs_popup_pages_input,
    .flags = {
        .allowPageInput  = FALSE,
        .allowChangePage = TRUE,
    },
};
