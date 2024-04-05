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

#include "popup_page_controls.h"


// Controls popup box draw function.
//! TODO: Allow changing page-specific settings from here.
void cs_popup_controls_draw(void) {
    const ScreenCoord_s32 bgStartX = (CRASH_SCREEN_X1 + (TEXT_WIDTH(1) / 2));
    const ScreenCoord_s32 bgStartY = (CRASH_SCREEN_Y1 + (TEXT_HEIGHT(1) / 2));
    const ScreenCoord_s32 bgW = (CRASH_SCREEN_W - TEXT_WIDTH(1));
    const ScreenCoord_s32 bgH = (CRASH_SCREEN_H - TEXT_HEIGHT(1));
    cs_draw_dark_rect(
        bgStartX, bgStartY,
        bgW, bgH,
        cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_POPUP_OPACITY)
    );
    CSPage* page = cs_get_current_page();

    // "[page name] PAGE CONTROLS"
    cs_print(TEXT_X(1), TEXT_Y(1), STR_COLOR_PREFIX"%s PAGE CONTROLS", COLOR_RGBA32_CRASH_PAGE_NAME, page->name);

    const enum ControlTypes* list = page->contList;

    if (list != NULL) {
        const ControlType* desc = NULL;

        CSTextCoord_u32 line = 3;

        while (*list != CONT_DESC_LIST_END) {
            desc = &gCSControlDescriptions[*list++];
            // [control]
            //   [description]
            cs_print(TEXT_X(2), TEXT_Y(line), "%s:\n "STR_COLOR_PREFIX"%s", desc->control, COLOR_RGBA32_CRASH_CONTROLS_DESCRIPTION, desc->description);
            line += 2;
        }
    }

    cs_draw_outline(bgStartX, bgStartY, bgW, bgH, COLOR_RGBA32_CRASH_DIVIDER);

    osWritebackDCacheAll();
}

//! TODO: Scrolling if list of controls is too long.
void cs_popup_controls_input(void) {
    u16 buttonPressed = gCSCompositeController->buttonPressed;

    if (buttonPressed & (A_BUTTON | B_BUTTON | START_BUTTON)) {
        // Close the popup without jumping.
        cs_open_popup(CS_POPUP_NONE);
    }
}

struct CSPopup gCSPopup_controls = {
    .name      = "CONTROLS",
    .initFunc  = NULL,
    .drawFunc  = cs_popup_controls_draw,
    .inputFunc = cs_popup_controls_input,
    .flags = {
        .allowPageInput  = FALSE,
        .allowChangePage = TRUE,
    },
};
