#ifndef CRASH_SCREEN_H
#define CRASH_SCREEN_H

// Configurable Defines
#define X_KERNING 6
#define GLYPH_WIDTH 8
#define GLYPH_HEIGHT 12
#define FONT_ROWS 16

// Margins for crash screen prints
#define LEFT_MARGIN 16
#define  TOP_MARGIN 16

// Tab width ('\t') for crash screen prints, in characters.
//  Note that we only treat tabs as a wide space.
#define TAB_WIDTH_CHARS 4

// Where to stop drawing the Header rectangle and where to start drawing the Body rectangle.
#define RECT_BOUNDARY_Y TOP_MARGIN + GLYPH_HEIGHT + 5

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
