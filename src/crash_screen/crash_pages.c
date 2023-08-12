#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "crash_pages.h"
#include "crash_main.h"
#include "crash_controls.h"

#include "pages/page_context.h"
#include "pages/page_assert.h"
#include "pages/page_log.h"
#include "pages/page_stack.h"
#include "pages/page_map.h"
#include "pages/page_memory.h"
#include "pages/page_disasm.h"
#include "pages/page_settings.h"


struct CSPage gCSPages[NUM_PAGES] = {
    [PAGE_CONTEXT    ] = { .initFunc = context_init,     .drawFunc = context_draw,     .inputFunc = context_input,     .contList = contextContList,    .name = "CONTEXT",     .flags = { .initialized = FALSE, .crashed = FALSE, .printName = FALSE, }, },
    [PAGE_ASSERTS    ] = { .initFunc = assert_init,      .drawFunc = assert_draw,      .inputFunc = assert_input,      .contList = assertsContList,    .name = "ASSERTS",     .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
#ifdef PUPPYPRINT_DEBUG
    [PAGE_LOG        ] = { .initFunc = log_init,         .drawFunc = log_draw,         .inputFunc = log_input,         .contList = defaultContList,    .name = "LOG",         .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
#endif
    [PAGE_STACK_TRACE] = { .initFunc = stack_trace_init, .drawFunc = stack_trace_draw, .inputFunc = stack_trace_input, .contList = stackTraceContList, .name = "STACK TRACE", .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
#ifdef INCLUDE_DEBUG_MAP
    [PAGE_MAP_VIEWER ] = { .initFunc = map_view_init,    .drawFunc = map_view_draw,    .inputFunc = map_view_input,    .contList = mapViewerContList,  .name = "MAP VIEW",    .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
#endif
    [PAGE_RAM_VIEWER ] = { .initFunc = ram_view_init,    .drawFunc = ram_view_draw,    .inputFunc = ram_view_input,    .contList = ramViewerContList,  .name = "RAM VIEW",    .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
    [PAGE_DISASM     ] = { .initFunc = disasm_init,      .drawFunc = disasm_draw,      .inputFunc = disasm_input,      .contList = disasmContList,     .name = "DISASM",      .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
    [PAGE_SETTINGS   ] = { .initFunc = settings_init,    .drawFunc = settings_draw,    .inputFunc = settings_input,    .contList = settingsContList,   .name = "SETTINGS",    .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
};


enum CrashScreenPages gCSPageID = FIRST_PAGE;


// Change the current page.
void crash_screen_set_page(enum CrashScreenPages page) {
    if (!gCSPages[gCSPageID].flags.crashed) {
        gCSPageID = page;
        gCSSwitchedPage = TRUE;
    }
}
