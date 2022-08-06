#pragma once

#include <ultra64.h>

#include "types.h"
#include "engine/colors.h"


// Crash screen font image properties.
#define CRASH_SCREEN_FONT_CHAR_WIDTH     5
#define CRASH_SCREEN_FONT_CHAR_HEIGHT    7
#define CRASH_SCREEN_FONT_CHARS_PER_ROW  6
#define CRASH_SCREEN_FONT_NUM_ROWS      43

// Spacing between chars.
#define CRASH_SCREEN_CHAR_SPACING_X      1
#define CRASH_SCREEN_CHAR_SPACING_Y      3

// The amount of space each char uses.
#define CRASH_SCREEN_LETTER_WIDTH       (CRASH_SCREEN_FONT_CHAR_WIDTH  + CRASH_SCREEN_CHAR_SPACING_X) //  6
#define CRASH_SCREEN_ROW_HEIGHT         (CRASH_SCREEN_FONT_CHAR_HEIGHT + CRASH_SCREEN_CHAR_SPACING_Y) // 10

// Width and height of crash screen.
#define CRASH_SCREEN_W 270
#define CRASH_SCREEN_H 222

// Number of chars that can fit in the crash screen.
#define CRASH_SCREEN_NUM_CHARS_X ((CRASH_SCREEN_W - 1) / CRASH_SCREEN_LETTER_WIDTH) // 44
#define CRASH_SCREEN_NUM_CHARS_Y ((CRASH_SCREEN_H - 1) / CRASH_SCREEN_ROW_HEIGHT)   // 22

// Macros for string size.
#define TEXT_WIDTH(numChars)  ((numChars) * CRASH_SCREEN_LETTER_WIDTH) // n *  6
#define TEXT_HEIGHT(numChars) ((numChars) * CRASH_SCREEN_ROW_HEIGHT  ) // n * 10

// Width and height of the text grid.
#define CRASH_SCREEN_TEXT_W TEXT_WIDTH( CRASH_SCREEN_NUM_CHARS_X) // 264
#define CRASH_SCREEN_TEXT_H TEXT_HEIGHT(CRASH_SCREEN_NUM_CHARS_Y) // 220

// Number of pixels between the text and the edge of the crash screen.
#define CRASH_SCREEN_TEXT_MARGIN_X ((CRASH_SCREEN_W - CRASH_SCREEN_TEXT_W) / 2) // 3
#define CRASH_SCREEN_TEXT_MARGIN_Y ((CRASH_SCREEN_H - CRASH_SCREEN_TEXT_H) / 2) // 1

// Top left corner of crash screen (round up).
#define CRASH_SCREEN_X1 (((SCREEN_WIDTH  - CRASH_SCREEN_W) / 2) - 0) // 25
#define CRASH_SCREEN_Y1 (((SCREEN_HEIGHT - CRASH_SCREEN_H) / 2) - 1) //  8

// Bottom right corner of crash screen.
#define CRASH_SCREEN_X2 (CRASH_SCREEN_X1 + CRASH_SCREEN_W) // 295
#define CRASH_SCREEN_Y2 (CRASH_SCREEN_Y1 + CRASH_SCREEN_H) // 230

// Top left corner of the text grid.
#define CRASH_SCREEN_TEXT_X1 (CRASH_SCREEN_X1 + CRASH_SCREEN_TEXT_MARGIN_X + 0) // 28
#define CRASH_SCREEN_TEXT_Y1 (CRASH_SCREEN_Y1 + CRASH_SCREEN_TEXT_MARGIN_Y + 1) // 10

// Bottom right corner of the text grid.
#define CRASH_SCREEN_TEXT_X2 (CRASH_SCREEN_TEXT_X1 + CRASH_SCREEN_TEXT_W) // 292
#define CRASH_SCREEN_TEXT_Y2 (CRASH_SCREEN_TEXT_Y1 + CRASH_SCREEN_TEXT_H) // 230

// Macros to convert a position on the text grid to screen coords.
#define TEXT_X(numChars) (CRASH_SCREEN_TEXT_X1 + TEXT_WIDTH(numChars) ) // 28 + (n *  6)
#define TEXT_Y(numChars) (CRASH_SCREEN_TEXT_Y1 + TEXT_HEIGHT(numChars)) // 10 + (n * 10)

// Returns the Y coordinate between the Y position on the text grid, and the space above it.
#define DIVIDER_Y(numChars) (TEXT_Y(numChars) - 2)

// The size of one row of the font image.
typedef u32 FontRow;

// Size of the crashed crash screen image.
#define SRC_IMG_SIZE (SCREEN_SIZE * sizeof(Texture) / sizeof(RGBA32))

void crash_screen_draw_dark_rect(u32 startX, u32 startY, u32 w, u32 h, u32 darken);
void crash_screen_draw_rect(u32 startX, u32 startY, u32 w, u32 h, RGBA32 color);
void crash_screen_draw_vertical_triangle(u32 startX, u32 startY, u32 w, u32 h, RGBA32 color, s8 flip);
void crash_screen_draw_horizontal_triangle(u32 startX, u32 startY, u32 w, u32 h, RGBA32 color, s8 flip);
void crash_screen_draw_line(u32 x1, u32 y1, u32 x2, u32 y2, RGBA32 color);
void crash_screen_draw_glyph(u32 startX, u32 startY, unsigned char glyph, RGBA32 color);
void crash_screen_take_screenshot(RGBA16 *dst);
void crash_screen_reset_framebuffer(s32 sDrawBackground);
void crash_screen_update_framebuffer(void);

#ifdef CRASH_SCREEN_CRASH_SCREEN
void draw_crashed_image_i4(void);
#endif

ALWAYS_INLINE void crash_screen_draw_divider(u32 y) {
    crash_screen_draw_rect(CRASH_SCREEN_X1, y, CRASH_SCREEN_W, 1, COLOR_RGBA32_LIGHT_GRAY);
}
