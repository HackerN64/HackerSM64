#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "address_select.h"
#include "crash_main.h"
#include "crash_controls.h"
#include "crash_settings.h"

#include "crash_pages.h"

#include "pages/page_context.h"
#include "pages/page_log.h"
#include "pages/page_stack.h"
#include "pages/page_map.h"
#include "pages/page_memory.h"
#include "pages/page_disasm.h"
#include "pages/page_settings.h"


// -- Pages --

CSPage* gCSPages[NUM_PAGES] = {
    [PAGE_CONTEXT    ] = &gCSPage_context,
    [PAGE_LOG        ] = &gCSPage_log,
    [PAGE_STACK_TRACE] = &gCSPage_stack,
#ifdef INCLUDE_DEBUG_MAP
    [PAGE_MAP_VIEWER ] = &gCSPage_map,
#endif
    [PAGE_RAM_VIEWER ] = &gCSPage_memory,
    [PAGE_DISASM     ] = &gCSPage_disasm,
    [PAGE_SETTINGS   ] = &gCSPage_settings,
};
enum CSPages gCSPageID = CRASH_SCREEN_START_PAGE;
_Bool gCSSwitchedPage = FALSE;


/**
 * @brief Change the current page.
 *
 * @param[in] pageID The page ID to switch to.
 */
void cs_set_page(enum CSPages pageID) {
    if ((gCSPageID != pageID) && !gCSPages[pageID]->flags.crashed) {
        gCSPageID       = pageID;
        gCSSwitchedPage = TRUE;
    }
}


// -- Popups --

struct CSPopup gCSPopup_none = {
    .name      = "",
    .initFunc  = NULL,
    .drawFunc  = NULL,
    .inputFunc = NULL,
};

CSPopup* gCSPopups[NUM_CS_POPUPS] = {
    [CS_POPUP_NONE          ] = &gCSPopup_none,
    [CS_POPUP_CONTROLS      ] = &gCSPopup_controls,
    [CS_POPUP_ADDRESS_SELECT] = &gCSPopup_address_select,
};
enum CSPopups gCSPopupID = CS_POPUP_NONE;
_Bool gCSSwitchedPopup = FALSE;


/**
 * @brief Change the current popup.
 *
 * @param popupID The popup ID to switch to.
 */
void cs_open_popup(enum CSPopups popupID) {
    if (gCSPopupID != popupID) {
        gCSPopupID = popupID;
        gCSSwitchedPopup = TRUE;
    }
}
