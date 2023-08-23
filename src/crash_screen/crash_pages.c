#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "crash_pages.h"
#include "crash_main.h"
#include "crash_controls.h"

#include "pages/page_context.h"
#include "pages/page_log.h"
#include "pages/page_stack.h"
#include "pages/page_map.h"
#include "pages/page_memory.h"
#include "pages/page_disasm.h"
#include "pages/page_settings.h"


CSPage gCSPages[NUM_PAGES] = {
    [PAGE_CONTEXT    ] = { .name = "CONTEXT",     .initFunc = context_init,     .drawFunc = context_draw,     .inputFunc = context_input,     .contList = contextContList,    .flags = { .initialized = FALSE, .crashed = FALSE, .printName = FALSE, }, },
    [PAGE_LOG        ] = { .name = "LOG",         .initFunc = log_init,         .drawFunc = log_draw,         .inputFunc = log_input,         .contList = logContList,        .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
    [PAGE_STACK_TRACE] = { .name = "STACK TRACE", .initFunc = stack_trace_init, .drawFunc = stack_trace_draw, .inputFunc = stack_trace_input, .contList = stackTraceContList, .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
#ifdef INCLUDE_DEBUG_MAP
    [PAGE_MAP_VIEWER ] = { .name = "MAP VIEW",    .initFunc = map_view_init,    .drawFunc = map_view_draw,    .inputFunc = map_view_input,    .contList = mapViewerContList,  .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
#endif
    [PAGE_RAM_VIEWER ] = { .name = "RAM VIEW",    .initFunc = ram_view_init,    .drawFunc = ram_view_draw,    .inputFunc = ram_view_input,    .contList = ramViewerContList,  .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
    [PAGE_DISASM     ] = { .name = "DISASM",      .initFunc = disasm_init,      .drawFunc = disasm_draw,      .inputFunc = disasm_input,      .contList = disasmContList,     .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
    [PAGE_SETTINGS   ] = { .name = "SETTINGS",    .initFunc = settings_init,    .drawFunc = settings_draw,    .inputFunc = settings_input,    .contList = settingsContList,   .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
};


enum CrashScreenPages gCSPageID = FIRST_PAGE;


// Change the current page.
void crash_screen_set_page(enum CrashScreenPages page) {
    if (!gCSPages[gCSPageID].flags.crashed) {
        gCSPageID = page;
        gCSSwitchedPage = TRUE;
    }
}
