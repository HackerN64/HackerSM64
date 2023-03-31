#include <ultra64.h>
#include <stdarg.h>
#include <string.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen/crash_screen.h"
#include "log.h"
#include "engine/colors.h"
#include "game/debug.h"
#include "game/puppyprint.h"


#ifdef PUPPYPRINT_DEBUG //! TODO: Make this scrollable
void draw_crash_log(UNUSED OSThread *thread) {
    u32 i;

    crash_screen_print(TEXT_X(0), TEXT_Y(1), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_PAGE_NAME, gCrashScreenPages[PAGE_LOG].name);
    crash_screen_draw_divider(DIVIDER_Y(2));

    osWritebackDCacheAll();

    for (i = 0; i < LOG_BUFFER_SIZE; i++) {
        crash_screen_print(TEXT_X(0), TEXT_Y(1 + (LOG_BUFFER_SIZE - i)), consoleLogTable[i]);
    }
}
#endif
