#pragma once

#include <ultra64.h>

#include "types.h"

#include "engine/colors.h"


// The size of one row of the font image.
typedef u32 CSFontRow;

// Used to keep track of screen coords vs. text coords.
typedef u32 ScreenCoord_u32;
typedef s32 ScreenCoord_s32;
typedef u16 ScreenCoord_u16;
typedef s16 ScreenCoord_s16;
typedef u32 CSTextCoord_u32;
typedef s32 CSTextCoord_s32;


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
#define TEXT_WIDTH(_numChars)  ((_numChars) * CRASH_SCREEN_LETTER_WIDTH) // n *  6
#define TEXT_HEIGHT(_numChars) ((_numChars) * CRASH_SCREEN_ROW_HEIGHT  ) // n * 10

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
#define TEXT_X(_numChars) (CRASH_SCREEN_TEXT_X1 + TEXT_WIDTH(_numChars) ) // 28 + (n *  6)
#define TEXT_Y(_numChars) (CRASH_SCREEN_TEXT_Y1 + TEXT_HEIGHT(_numChars)) // 10 + (n * 10)

// Returns the Y coordinate between the Y position on the text grid, and the space above it.
#define DIVIDER_Y(_numChars) (TEXT_Y(_numChars) - 2)

// Get a pointer to the framebuffer as a specific type:
#define FB_PTR_AS(_type) (_type*)(Address)(gFramebuffers[sRenderingFramebuffer])


// For gCSDarkenAlphas (bit indices in RGBA16 color component).
enum CSDrawDarkRectDarken {
    CS_DARKEN_NONE,
    CS_DARKEN_HALF,
    CS_DARKEN_THREE_QUARTERS,
    CS_DARKEN_SEVEN_EIGHTHS,
    CS_DARKEN_FIFTEEN_SIXTEENTHS,
    CS_DARKEN_TO_BLACK,
    CS_DARKEN_LIMIT,
};

extern const Alpha gCSDarkenAlphas[CS_DARKEN_LIMIT];

// For cs_draw_triangle.
enum CSDrawTriangleDirection {
    CS_TRI_DOWN,
    CS_TRI_UP,
    CS_TRI_RIGHT,
    CS_TRI_LEFT,
};


typedef struct CSScissorBox {
    /*0x00*/ ScreenCoord_s16 x1;
    /*0x02*/ ScreenCoord_s16 y1;
    /*0x04*/ ScreenCoord_s16 x2;
    /*0x06*/ ScreenCoord_s16 y2;
} CSScissorBox; /*0x08*/

extern struct CSScissorBox gCSScissorBox;


void cs_set_scissor_box(ScreenCoord_s16 x1, ScreenCoord_s16 y1, ScreenCoord_s16 x2, ScreenCoord_s16 y2);
void cs_reset_scissor_box(void);

#define CS_SCISSOR_BOX_START(_x, _y, _w, _h) \
    CSScissorBox __tempScissorBox = gCSScissorBox; \
    cs_set_scissor_box((_x), (_y), (_w), (_h));

#define CS_SCISSOR_BOX_END() \
    gCSScissorBox = __tempScissorBox;

void cs_draw_rect(ScreenCoord_s32 startX, ScreenCoord_s32 startY, ScreenCoord_s32 w, ScreenCoord_s32 h, RGBA32 color);
ALWAYS_INLINE void cs_draw_dark_rect(ScreenCoord_s32 startX, ScreenCoord_u32 startY, ScreenCoord_s32 w, ScreenCoord_s32 h, u16 darken) {
    cs_draw_rect(startX, startY, w, h, RGBA32_SET_ALPHA(COLOR_RGBA32_BLACK, gCSDarkenAlphas[darken]));
}
void cs_draw_outline(ScreenCoord_s32 startX, ScreenCoord_s32 startY, ScreenCoord_s32 w, ScreenCoord_s32 h, RGBA32 color);
void cs_draw_diamond(ScreenCoord_s32 startX, ScreenCoord_s32 startY, ScreenCoord_s32 w, ScreenCoord_s32 h, RGBA32 color);
void cs_draw_triangle(ScreenCoord_s32 startX, ScreenCoord_s32 startY, ScreenCoord_s32 w, ScreenCoord_s32 h, RGBA32 color, enum CSDrawTriangleDirection direction);
// void cs_draw_line(ScreenCoord_u32 x1, ScreenCoord_u32 y1, ScreenCoord_u32 x2, ScreenCoord_u32 y2, RGBA32 color);
void cs_draw_glyph(ScreenCoord_u32 startX, ScreenCoord_u32 startY, uchar glyph, RGBA32 color);
// void cs_draw_texture(ScreenCoord_s32 startX, ScreenCoord_s32 startY, ScreenCoord_s32 w, ScreenCoord_s32 h, RGBA16* texture);
void cs_take_screenshot_of_game(RGBA16* dst, size_t size);
void cs_draw_scroll_bar_impl(ScreenCoord_u32 x, ScreenCoord_u32 topY, ScreenCoord_u32 bottomY, u32 numVisibleEntries, u32 numTotalEntries, u32 topVisibleEntry, RGBA32 color, _Bool drawBg);
ALWAYS_INLINE void cs_draw_scroll_bar(ScreenCoord_u32 topY, ScreenCoord_u32 bottomY, u32 numVisibleEntries, u32 numTotalEntries, u32 topVisibleEntry, RGBA32 color, _Bool drawBg) {
    cs_draw_scroll_bar_impl((CRASH_SCREEN_X2 - 1), topY, bottomY, numVisibleEntries, numTotalEntries, topVisibleEntry, color, drawBg);
}
// RGBA32 cs_thread_draw_highlight(OSThread* thread, ScreenCoord_u32 y);
RGBA32 cs_draw_thread_state_icon(ScreenCoord_u32 x, ScreenCoord_u32 y, OSThread* thread);
void cs_draw_main(void);


ALWAYS_INLINE void cs_draw_divider_impl(ScreenCoord_s32 startX, ScreenCoord_s32 width, ScreenCoord_u32 startY, RGBA32 color) {
    cs_draw_rect(startX, startY, width, 1, color);
}

ALWAYS_INLINE void cs_draw_divider(ScreenCoord_u32 startY) {
    cs_draw_divider_impl(CRASH_SCREEN_X1, CRASH_SCREEN_W, startY, COLOR_RGBA32_CRASH_DIVIDER);
}

ALWAYS_INLINE void cs_draw_divider_translucent_impl(ScreenCoord_s32 startX, ScreenCoord_s32 width, ScreenCoord_u32 startY) {
    cs_draw_divider_impl(startX, width, startY, COLOR_RGBA32_CRASH_DIVIDER_TRANSLUCENT);
}

ALWAYS_INLINE void cs_draw_divider_translucent(ScreenCoord_u32 startY) {
    cs_draw_divider_translucent_impl(CRASH_SCREEN_X1, CRASH_SCREEN_W, startY);
}

ALWAYS_INLINE void cs_draw_row_selection_box_impl(ScreenCoord_s32 x, ScreenCoord_s32 y, ScreenCoord_s32 w, ScreenCoord_s32 h, RGBA32 color) {
    cs_draw_rect(
        (x - 1), (y - 2),
        (w + 1), (h + 1),
        color
    );
}
ALWAYS_INLINE void cs_draw_row_box_1(ScreenCoord_u32 y, RGBA32 color) {
    cs_draw_row_selection_box_impl(TEXT_X(0), y,
        CRASH_SCREEN_TEXT_W, TEXT_HEIGHT(1),
        color
    );
}
ALWAYS_INLINE void cs_draw_row_box_2(ScreenCoord_u32 y, RGBA32 color) {
    cs_draw_row_selection_box_impl(TEXT_X(0), (y + 1),
        CRASH_SCREEN_TEXT_W, (TEXT_HEIGHT(2) - 2),
        color
    );
}
ALWAYS_INLINE void cs_draw_row_box_w(ScreenCoord_u32 x, ScreenCoord_u32 width, ScreenCoord_u32 y, RGBA32 color) {
    cs_draw_row_selection_box_impl(x, (y + 1),
        width, (TEXT_HEIGHT(2) - 2),
        color
    );
}

ALWAYS_INLINE void cs_draw_row_selection_box(ScreenCoord_u32 y) {
    cs_draw_row_box_1(y, COLOR_RGBA32_CRASH_SELECT_HIGHLIGHT);
}
ALWAYS_INLINE void cs_draw_row_selection_box_2(ScreenCoord_u32 y) {
    cs_draw_row_box_2(y, COLOR_RGBA32_CRASH_SELECT_HIGHLIGHT);
}
ALWAYS_INLINE void cs_draw_row_crash_box(ScreenCoord_u32 y) {
    cs_draw_row_box_1(y, COLOR_RGBA32_CRASH_PC_HIGHLIGHT);
}
ALWAYS_INLINE void cs_draw_row_crash_box_2(ScreenCoord_u32 y) {
    cs_draw_row_box_2(y, COLOR_RGBA32_CRASH_PC_HIGHLIGHT);
}
