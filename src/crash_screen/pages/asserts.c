#include <ultra64.h>
#include <stdarg.h>
#include <string.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen/crash_screen.h"
#include "asserts.h"
#include "engine/colors.h"
#include "game/debug.h"


void draw_assert(UNUSED OSThread *thread) {
    u32 line = 1;

    line += crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_PAGE_NAME, gCrashScreenPages[PAGE_ASSERTS].name);

    crash_screen_draw_divider(DIVIDER_Y(line));

    if (__n64Assert_Filename != NULL) {
        line += crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"%s:%s", COLOR_RGBA32_CRASH_FILE_NAME, "FILE", __n64Assert_Filename);
        crash_screen_draw_divider(DIVIDER_Y(line));
        line += crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"%s:%d", COLOR_RGBA32_CRASH_AT, "LINE", __n64Assert_LineNum);
        crash_screen_draw_divider(DIVIDER_Y(line));
        line += crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"%s:", COLOR_RGBA32_CRASH_HEADER, "MESSAGE");
        line += crash_screen_print(TEXT_X(0), (TEXT_Y(line) + 5), "%s", __n64Assert_Message);
    } else {
        crash_screen_print(TEXT_X(0), TEXT_Y(line), "no failed assert to report.");
    }

    osWritebackDCacheAll();
}
