#ifndef PRINT_H
#define PRINT_H

#include <PR/ultratypes.h>

#define TEXRECT_MIN_X 10
#define TEXRECT_MAX_X 300
#define TEXRECT_MIN_Y 5
#define TEXRECT_MAX_Y 220

enum PrintfGlyphs {
    GLYPH_SPACE           = 0x20,
    GLYPH_U               = 0x55,
    GLYPH_EXCLAMATION_PNT = 0x21,
    GLYPH_TWO_EXCLAMATION = 0x0, // UTF-8
    GLYPH_QUESTION_MARK   = 0x3F,
    GLYPH_AMPERSAND       = 0x26,
    GLYPH_PERCENT         = 0x25,
    GLYPH_MINUS           = 0x2D,
    GLYPH_MULTIPLY        = 0x0, // UTF-8
    GLYPH_COIN            = 0x0, // UTF-8
    GLYPH_RED_COIN        = 0x0, // UTF-8
    GLYPH_SILVER_COIN     = 0x0, // UTF-8
    GLYPH_MARIO_HEAD      = 0x0, // UTF-8
    GLYPH_STAR            = 0x0, // UTF-8
    GLYPH_PERIOD          = 0x2E,
    GLYPH_BETA_KEY        = 0x0, // UTF-8
    GLYPH_APOSTROPHE      = 0x27,
    GLYPH_DOUBLE_QUOTE    = 0x22,
    GLYPH_UMLAUT          = 0x0, // UTF-8
};

void print_text_fmt_int(s32 x, s32 y, const char *str, s32 n);
void print_text(s32 x, s32 y, const char *str);
void print_text_centered(s32 x, s32 y, const char *str);
void render_text_labels(void);

#endif // PRINT_H
