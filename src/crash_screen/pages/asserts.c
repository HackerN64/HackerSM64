#include <ultra64.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen/crash_screen.h"
#include "asserts.h"


void assert_draw(void) { //! TODO: Make this scrollable if the message long enough.
    u32 line = 2;

    if (__n64Assert_Filename != NULL) {
        // "FILE: [file name]"
        crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"%s:%s", COLOR_RGBA32_CRASH_FILE_NAME, "FILE", __n64Assert_Filename);
        line++;
        crash_screen_draw_divider(DIVIDER_Y(line));
        // "LINE: [line number]"
        crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"%s:%d", COLOR_RGBA32_CRASH_AT, "LINE", __n64Assert_LineNum);
        line++;
        crash_screen_draw_divider(DIVIDER_Y(line));
        // "MESSAGE:"
        crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"%s:", COLOR_RGBA32_CRASH_HEADER, "MESSAGE");
        line++;
        // "[assert message]"
        crash_screen_print(TEXT_X(0), (TEXT_Y(line) + 5), "%s", __n64Assert_Message);
        line++;
    } else {
        // "No failed assert to report."
        crash_screen_print(TEXT_X(0), TEXT_Y(line), "No failed assert to report.");
    }

    osWritebackDCacheAll();
}
