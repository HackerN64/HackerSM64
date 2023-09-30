#pragma once

#include <ultra64.h>

#include "types.h"

#include "engine/colors.h"


// The size of one row of the font image.
typedef u32 CSFontRow;


// Crash screen font image properties.
#define CRASH_SCREEN_FONT_CHAR_WIDTH     5
#define CRASH_SCREEN_FONT_CHAR_HEIGHT    7
#define CRASH_SCREEN_FONT_CHARS_PER_ROW (SIZEOF_BITS(CSFontRow) / CRASH_SCREEN_FONT_CHAR_WIDTH) // 32 bits per row.
#define CRASH_SCREEN_FONT_NUM_ROWS      ((255 / CRASH_SCREEN_FONT_CHARS_PER_ROW) + 1) // Round up.

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

// Default bounds for the scissor box.
#define SCISSOR_BOX_DEFAULT_X1 CRASH_SCREEN_X1
#define SCISSOR_BOX_DEFAULT_Y1 CRASH_SCREEN_Y1
#define SCISSOR_BOX_DEFAULT_X2 (CRASH_SCREEN_X1 + CRASH_SCREEN_W)
#define SCISSOR_BOX_DEFAULT_Y2 (CRASH_SCREEN_Y1 + CRASH_SCREEN_H)

// Top left corner of the text grid.
#define CRASH_SCREEN_TEXT_X1 ((CRASH_SCREEN_X1 + CRASH_SCREEN_TEXT_MARGIN_X) + 0) // 28
#define CRASH_SCREEN_TEXT_Y1 ((CRASH_SCREEN_Y1 + CRASH_SCREEN_TEXT_MARGIN_Y) + 1) // 10

// Bottom right corner of the text grid.
#define CRASH_SCREEN_TEXT_X2 (CRASH_SCREEN_TEXT_X1 + CRASH_SCREEN_TEXT_W) // 292
#define CRASH_SCREEN_TEXT_Y2 (CRASH_SCREEN_TEXT_Y1 + CRASH_SCREEN_TEXT_H) // 230

// Macros to convert a position on the text grid to screen coords.
#define TEXT_X(numChars) (CRASH_SCREEN_TEXT_X1 + TEXT_WIDTH(numChars) ) // 28 + (n *  6)
#define TEXT_Y(numChars) (CRASH_SCREEN_TEXT_Y1 + TEXT_HEIGHT(numChars)) // 10 + (n * 10)

// Returns the Y coordinate between the Y position on the text grid, and the space above it.
#define DIVIDER_Y(numChars) (TEXT_Y(numChars) - 2)

// Get a pointer to the framebuffer as a specific type:
#define FB_PTR_AS(type) (type*)PHYSICAL_TO_VIRTUAL(gFramebuffers[sRenderingFramebuffer])


// For cs_draw_dark_rect.
enum CSDrawDarkRectDarken {
    CS_DARKEN_NONE,
    CS_DARKEN_HALF,
    CS_DARKEN_THREE_QUARTERS,
    CS_DARKEN_SEVEN_EIGHTHS,
    CS_DARKEN_FIFTEEN_SIXTEENTHS,
    CS_DARKEN_TO_BLACK,
};

// For cs_draw_triangle.
enum CSDrawTriangleDirection {
    CS_TRI_UP,
    CS_TRI_DOWN,
    CS_TRI_LEFT,
    CS_TRI_RIGHT,
};


typedef struct CSScissorBox {
    s32 x1;
    s32 y1;
    s32 x2;
    s32 y2;
} CSScissorBox;

extern struct CSScissorBox gCSScissorBox;


void cs_set_scissor_box(s32 x1, s32 y1, s32 x2, s32 y2);
void cs_reset_scissor_box(void);
void cs_draw_dark_rect(s32 startX, s32 startY, s32 w, s32 h, u32 darken);
void cs_draw_rect(s32 startX, s32 startY, s32 w, s32 h, RGBA32 color);
void cs_draw_diamond(s32 startX, s32 startY, s32 w, s32 h, RGBA32 color);
void cs_draw_triangle(s32 startX, s32 startY, s32 w, s32 h, RGBA32 color, enum CSDrawTriangleDirection direction);
// void cs_draw_line(u32 x1, u32 y1, u32 x2, u32 y2, RGBA32 color);
void cs_draw_glyph(u32 startX, u32 startY, uchar glyph, RGBA32 color);
void cs_take_screenshot_of_game(RGBA16* dst, size_t size);
void cs_draw_scroll_bar(u32 topY, u32 bottomY, u32 numVisibleEntries, u32 numTotalEntries, u32 topVisibleEntry, RGBA32 color, _Bool drawBg);
void cs_draw_main(void);

ALWAYS_INLINE void cs_draw_divider(u32 y) {
    cs_draw_rect(CRASH_SCREEN_X1, y, CRASH_SCREEN_W, 1, COLOR_RGBA32_CRASH_DIVIDER);
}

ALWAYS_INLINE void cs_draw_divider_translucent(u32 y) {
    cs_draw_rect(CRASH_SCREEN_X1, y, CRASH_SCREEN_W, 1, RGBA32_SET_ALPHA(COLOR_RGBA32_CRASH_DIVIDER, 0x7F));
}

ALWAYS_INLINE void cs_draw_row_selection_box(s32 y) {
    cs_draw_rect(
        (TEXT_X(0) - 1), (y - 2),
        (CRASH_SCREEN_TEXT_W + 1), (TEXT_HEIGHT(1) + 1),
        COLOR_RGBA32_CRASH_SELECT_HIGHLIGHT
    );
}
