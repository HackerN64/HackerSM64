#include <ultra64.h>

#include "types.h"
#include "sm64.h"

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


// Change the current page.
void crash_screen_set_page(enum CSPages pageID) {
    if (!gCSPages[pageID]->flags.crashed) {
        gCSPageID = pageID;
        gCSSwitchedPage = TRUE;
    }
}
