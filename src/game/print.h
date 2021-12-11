#ifndef PRINT_H
#define PRINT_H

#include <PR/ultratypes.h>

#define TEXRECT_MIN_X 10
#define TEXRECT_MAX_X 300
#define TEXRECT_MIN_Y 5
#define TEXRECT_MAX_Y 220

enum PrintfGlyphs {
    GLYPH_SPACE           = -1,
    GLYPH_U               = 30,
    GLYPH_EXCLAMATION_PNT = 36,
    GLYPH_TWO_EXCLAMATION = 37,
    GLYPH_QUESTION_MARK   = 38,
    GLYPH_AMPERSAND       = 39,
    GLYPH_PERCENT         = 40,
    GLYPH_MINUS           = 47,
    GLYPH_MULTIPLY        = 48,
    GLYPH_COIN            = 49,
    GLYPH_RED_COIN        = 50,
    GLYPH_SILVER_COIN     = 51,
    GLYPH_MARIO_HEAD      = 52,
    GLYPH_STAR            = 53,
    GLYPH_PERIOD          = 54,
    GLYPH_BETA_KEY        = 55,
    GLYPH_APOSTROPHE      = 56,
    GLYPH_DOUBLE_QUOTE    = 57,
    GLYPH_UMLAUT          = 58,
};

void print_text_fmt_int(s32 x, s32 y, const char *str, s32 n);
void print_text(s32 x, s32 y, const char *str);
void print_text_centered(s32 x, s32 y, const char *str);
void render_text_labels(void);

#endif // PRINT_H
