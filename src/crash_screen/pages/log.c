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


#ifdef PUPPYPRINT_DEBUG //! TODO: Make this scrollable if it's long enough

void puppyprint_log_draw(void) {
    osWritebackDCacheAll();

    for (u32 i = 0; i < LOG_BUFFER_SIZE; i++) {
        // "[log entry]"
        crash_screen_print(TEXT_X(0), TEXT_Y(1 + (LOG_BUFFER_SIZE - i)), consoleLogTable[i]);
    }
}

#endif
