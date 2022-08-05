#pragma once

#include <ultra64.h>

#include "types.h"
#include "engine/colors.h"

void crash_screen_draw_dark_rect(u32 startX, u32 startY, u32 w, u32 h, u32 darken);
void crash_screen_draw_rect(u32 startX, u32 startY, u32 w, u32 h, RGBA32 color);
void crash_screen_draw_vertical_triangle(u32 startX, u32 startY, u32 w, u32 h, RGBA32 color, s8 flip);
void crash_screen_draw_horizontal_triangle(u32 startX, u32 startY, u32 w, u32 h, RGBA32 color, s8 flip);
void crash_screen_draw_line(u32 x1, u32 y1, u32 x2, u32 y2, RGBA32 color);
void crash_screen_draw_glyph(u32 startX, u32 startY, unsigned char glyph, RGBA32 color);
void crash_screen_take_screenshot(RGBA16 *dst);
void reset_crash_screen_framebuffer(s32 sDrawBackground);
void update_crash_screen_framebuffer(void);

#ifdef CRASH_SCREEN_CRASH_SCREEN
void draw_crashed_image_i4(void);
#endif

ALWAYS_INLINE void crash_screen_draw_divider(u32 y) {
    crash_screen_draw_rect(CRASH_SCREEN_X1, y, CRASH_SCREEN_W, 1, COLOR_RGBA32_LIGHT_GRAY);
}
