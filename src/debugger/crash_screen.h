#ifndef CRASH_SCREEN_H
#define CRASH_SCREEN_H

// Configurable Defines
#define X_KERNING 6
#define GLYPH_WIDTH 8
#define GLYPH_HEIGHT 12
#define FONT_ROWS 16
#define LEFT_MARGIN 16 // for crash screen prints

enum CrashPages {
    PAGE_SIMPLE,
    PAGE_CONTEXT,
#ifdef PUPPYPRINT_DEBUG
    PAGE_LOG,
#endif
    PAGE_STACKTRACE,
    PAGE_DISASM,
    PAGE_ASSERTS,
    PAGE_COUNT
};

void crash_screen_init(void);

#endif // CRASH_SCREEN_H
