#ifndef PRINT_H
#define PRINT_H

#include <PR/ultratypes.h>

#define TEXRECT_MIN_X 10
#define TEXRECT_MAX_X 300
#define TEXRECT_MIN_Y 5
#define TEXRECT_MAX_Y 220

enum PrintfGlyphs {
    GLYPH_SPACE           = ' ',
    GLYPH_U               = 'U',
    GLYPH_EXCLAMATION_PNT = '!',
    GLYPH_TWO_EXCLAMATION = 0x0, // UTF-8
    GLYPH_QUESTION_MARK   = '?',
    GLYPH_AMPERSAND       = '&',
    GLYPH_PERCENT         = '%',
    GLYPH_MINUS           = '-',
    GLYPH_MULTIPLY        = 0x0, // UTF-8
    GLYPH_COIN            = 0x0, // UTF-8
    GLYPH_RED_COIN        = 0x0, // UTF-8
    GLYPH_SILVER_COIN     = 0x0, // UTF-8
    GLYPH_MARIO_HEAD      = 0x0, // UTF-8
    GLYPH_STAR            = 0x0, // UTF-8
    GLYPH_PERIOD          = '.',
    GLYPH_BETA_KEY        = 0x0, // UTF-8
    GLYPH_APOSTROPHE      = '\'',
    GLYPH_DOUBLE_QUOTE    = '"',
    GLYPH_UMLAUT          = 0x0, // UTF-8
};

void print_text_fmt_int(s32 x, s32 y, const char *str, s32 n);
void print_text(s32 x, s32 y, const char *str);
void print_text_centered(s32 x, s32 y, const char *str);
void render_text_labels(void);

#endif // PRINT_H
