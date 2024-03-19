#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "crash_main.h"
#include "crash_controls.h"
#include "crash_settings.h"

#include "crash_pages.h"

#include "pages/page_home.h"
#include "pages/page_registers.h"
#include "pages/page_threads.h"
#include "pages/page_logs.h"
#include "pages/page_stack.h"
#ifdef INCLUDE_DEBUG_MAP
#include "pages/page_map.h"
#endif // INCLUDE_DEBUG_MAP
#include "pages/page_memory.h"
#include "pages/page_disasm.h"
#include "pages/page_settings.h"
#include "pages/page_about.h"

#include "popups/popup_address_select.h"
#include "popups/popup_page_controls.h"
#include "popups/popup_page_select.h"
#include "popups/popup_register.h"


// -- Pages --

CSPage* gCSPages[CS_NUM_PAGES] = {
    [CS_PAGE_HOME       ] = &gCSPage_home,
    [CS_PAGE_STACK      ] = &gCSPage_stack,
    [CS_PAGE_THREADS    ] = &gCSPage_threads,
    [CS_PAGE_REGISTERS  ] = &gCSPage_registers,
    [CS_PAGE_DISASM     ] = &gCSPage_disasm,
    [CS_PAGE_MEMORY     ] = &gCSPage_memory,
#ifdef INCLUDE_DEBUG_MAP
    [CS_PAGE_MAP        ] = &gCSPage_map,
#endif // INCLUDE_DEBUG_MAP
#ifdef PUPPYPRINT_DEBUG
    [CS_PAGE_LOGS       ] = &gCSPage_logs,
#endif // PUPPYPRINT_DEBUG
    [CS_PAGE_SETTINGS   ] = &gCSPage_settings,
    [CS_PAGE_ABOUT      ] = &gCSPage_about,
};
enum CSPages gCSPageID = CRASH_SCREEN_START_PAGE; // Current page ID.
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

/**
 * @brief Gets a pointer to the current page's info.
 *
 * @return CSPage* A pointer to the current page's info.
 */
CSPage* cs_get_current_page(void) {
    return gCSPages[gCSPageID];
}

/**
 * @brief Reinitialize all of the crash screen's pages.
 */
void cs_reinitialize_pages(void) {
    for (int pageID = 0; pageID < ARRAY_COUNT(gCSPages); pageID++) {
        gCSPages[pageID]->flags.initialized = FALSE;
    }
}


// -- Popups --

CSPopup* gCSPopups[NUM_CS_POPUPS] = {
    [CS_POPUP_NONE          ] = NULL,
    [CS_POPUP_CONTROLS      ] = &gCSPopup_controls,
    [CS_POPUP_PAGES         ] = &gCSPopup_pages,
    [CS_POPUP_REGISTER      ] = &gCSPopup_register,
    [CS_POPUP_ADDRESS_SELECT] = &gCSPopup_address_select,
};
enum CSPopups gCSPopupID = CS_POPUP_NONE; // Current open popup ID. CS_POPUP_NONE means no popup is open.
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

/**
 * @brief Gets a pointer to the current popup's info.
 * 
 * @return CSPopup* A pointer to the current popup's info.
 */
CSPopup* cs_get_current_popup(void) {
    return gCSPopups[gCSPopupID];
}
