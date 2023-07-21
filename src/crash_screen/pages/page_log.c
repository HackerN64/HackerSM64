#include <ultra64.h>
#include <stdarg.h>
#include <string.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen/crash_main.h"
#include "page_log.h"
#include "engine/colors.h"
#include "game/debug.h"
#include "game/puppyprint.h"


#ifdef PUPPYPRINT_DEBUG //! TODO: Make this scrollable if the log is long enough.

void draw_log_entries(u32 line) {
    u32 entryIndex = (LOG_BUFFER_SIZE - 1);

    for (u32 i = 0; i < LOG_NUM_ROWS; i++) {

        char* entry = consoleLogTable[entryIndex];

        if (entry[0] == CHAR_NULL) {
            break;
        }

        crash_screen_print(TEXT_X(0), TEXT_Y(line + i), "%i: %s", (gConsoleLogLastIndex - i), entry);

        entryIndex--;
    }
}

void log_draw(void) {
    osWritebackDCacheAll();

    draw_log_entries(2);
}

#endif
