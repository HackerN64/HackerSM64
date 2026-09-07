#ifndef CRASH_SCREEN_H
#define CRASH_SCREEN_H

#include <ultra64.h>
#include "config/config_debug.h"
#include "game/main.h"

#define CRASH_SCREEN_MAX_PATH 256

// Configurable Defines
#define CRASH_SCREEN_X_KERNING 6
#define CRASH_SCREEN_GLYPH_WIDTH 8
#define CRASH_SCREEN_GLYPH_HEIGHT 12
#define CRASH_SCREEN_FONT_ROWS 16

// Margins for crash screen prints
#define CRASH_SCREEN_LEFT_MARGIN 16
#define  CRASH_SCREEN_TOP_MARGIN 16

// Tab width ('\t') for crash screen prints, in characters.
//  Note that we only treat tabs as a wide space.
#define CRASH_SCREEN_TAB_WIDTH_CHARS 4

#define CRASH_SCREEN_NUM_CFBs (2)

// Where to stop drawing the Header rectangle and where to start drawing the Body rectangle.
#define CRASH_SCREEN_RECT_BOUNDARY_Y CRASH_SCREEN_TOP_MARGIN + CRASH_SCREEN_GLYPH_HEIGHT + 5

enum CrashPages {
    CRASH_SCREEN_PAGE_SIMPLE,
    CRASH_SCREEN_PAGE_CONTEXT,
    CRASH_SCREEN_PAGE_STACKTRACE,
#ifdef PUPPYPRINT_DEBUG
    CRASH_SCREEN_PAGE_LOG,
#endif
    CRASH_SCREEN_PAGE_DISASM,
    CRASH_SCREEN_PAGE_ASSERTS,
    CRASH_SCREEN_PAGE_COUNT
};

struct FaultInfo {
    OSThread thread;
    u64 stack[THREAD2_STACK / sizeof(u64)];
    OSMesgQueue mesgQueue;
    OSMesg mesg;
    u16 *framebuffer;
    u16 width;
    u16 height;
};

// Exports for the debugger/ system
char *crash_screen_ellide_string(char *str, u32 truncateLength);

// Exports for the rest of the game
void crash_screen_init(void);

#endif // CRASH_SCREEN_H
