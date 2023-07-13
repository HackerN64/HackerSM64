#include <ultra64.h>
#include <string.h>

#include "types.h"
#include "sm64.h"
#include "crash_screen.h"
#include "crash_draw.h"
#include "crash_print.h"
#include "buffers/framebuffers.h"
#include "buffers/zbuffer.h"
#include "game/game_init.h"


_Bool gCSDrawCrashScreen     = TRUE;
_Bool gCSDrawSavedScreenshot = TRUE;


// Crash screen font. Each row of the image fits in one u32 pointer.
ALIGNED32 static const Texture gCSFont[CRASH_SCREEN_FONT_CHAR_HEIGHT * CRASH_SCREEN_FONT_NUM_ROWS * sizeof(CSFontRow)] = {
    #include "textures/crash_screen/crash_screen_font.custom.ia1.inc.c"
};

static RGBA16* get_rendering_fb_pixel(u32 x, u32 y) {
    return (FB_PTR_AS(RGBA16) + (SCREEN_WIDTH * y) + x);
}

static void apply_color(RGBA16* dst, RGBA16 newColor, Alpha alpha) {
    if (alpha == MSK_RGBA32_A) {
        *dst = newColor;
    } else {
        *dst = rgba16_blend(*dst, newColor, alpha);
    }
}

// Darkens a rectangular area. This is faster than the color blending done by
// crash_screen_draw_rect, so it's used for the large background rectangle.
// 0  - does nothing
// 1  - darken by 1/2
// 2  - darken by 3/4
// 3  - darken by 7/8
// 4  - darken by 15/16
// 5+ - darken to black
void crash_screen_draw_dark_rect(u32 startX, u32 startY, u32 w, u32 h, u32 darken) {
    if (darken == CS_DARKEN_NONE) {
        return;
    }

    const RGBA16Component componentMask = (MSK_RGBA16_C & ~BITMASK(darken));
    RGBA16 mask = GPACK_RGBA5551(0, 0, 0, 0);
    for (u32 i = SIZ_RGBA16_A; i < (SIZ_RGBA16_C * 3); i += SIZ_RGBA16_C) {
        mask |= (componentMask << i);
    }

    RGBA16* dst = get_rendering_fb_pixel(startX, startY);

    for (u32 y = 0; y < h; y++) {
        for (u32 x = 0; x < w; x++) {
            *dst = (((*dst & mask) >> darken) | MSK_RGBA16_A);
            dst++;
        }

        dst += (SCREEN_WIDTH - w);
    }
}

// Draws a rectangle.
void crash_screen_draw_rect(u32 startX, u32 startY, u32 w, u32 h, RGBA32 color) {
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);

    RGBA16* dst = get_rendering_fb_pixel(startX, startY);

    for (u32 y = 0; y < h; y++) {
        for (u32 x = 0; x < w; x++) {
            apply_color(dst, newColor, alpha);
            dst++;
        }

        dst += (SCREEN_WIDTH - w);
    }
}

// Draws a triangle pointing upwards or downwards. Flip: FALSE = point up, TRUE = point down.
void crash_screen_draw_vertical_triangle(u32 startX, u32 startY, u32 w, u32 h, RGBA32 color, _Bool flip) {
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);
    const f32 middle = (w / 2.0f);
    f32 d = 0.0f;
    f32 t = (middle / (f32)h);
    if (flip) {
        d = (middle - t);
        t = -t;
    }

    RGBA16* dst = get_rendering_fb_pixel(startX, startY);

    for (u32 y = 0; y < h; y++) {
        for (u32 x = 0; x < w; x++) {
            if (absf(middle - x) < d) {
                apply_color(dst, newColor, alpha);
            }
            dst++;
        }

        d += t;
        dst += (SCREEN_WIDTH - w);
    }
}

// Draws a triangle pointing left or right.
void crash_screen_draw_horizontal_triangle(u32 startX, u32 startY, u32 w, u32 h, RGBA32 color, _Bool flip) {
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);
    const f32 middle = (h / 2.0f);
    const f32 t = ((f32)w / middle);
    f32 x1 = w;

    RGBA16* dst = get_rendering_fb_pixel(startX, startY);
    RGBA16* start = dst;

    for (u32 y = 0; y < h; y++) {
        for (u32 x = x1; x < w; x++) {
            apply_color(dst, newColor, alpha);
            dst++;
        }
        x1 -= (y < middle) ? t : -t;
        dst = start + (SCREEN_WIDTH * y);
        if (flip) {
            dst += (u32)x1;
        }
    }
}

// Draws a line from one point on the screen to another.
void crash_screen_draw_line(u32 x1, u32 y1, u32 x2, u32 y2, RGBA32 color) {
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);

    RGBA16* dst;

    // Swap the points so that the second point is after the first.
    if (x1 > x2) SWAP(x1, x2);
    if (y1 > y2) SWAP(y1, y2);

    const f32 slope = (f32)(y2 - y1) / (x2 - x1);

    f32 x = x1;
    f32 y;
    while (x <= x2) {
        y = ((slope * (x - x1)) + y1);
        dst = get_rendering_fb_pixel(x, y);
        apply_color(dst, newColor, alpha);
        x++;
    }
}

void crash_screen_draw_glyph(u32 startX, u32 startY, uchar glyph, RGBA32 color) {
    if (glyph == CHAR_NULL) { // Null
        color = COLOR_RGBA32_GRAY;
    }
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);
    CSFontRow startBit = ((CSFontRow)BIT(SIZEOF_BITS(CSFontRow) - 1) >> ((glyph % CRASH_SCREEN_FONT_CHARS_PER_ROW) * CRASH_SCREEN_FONT_CHAR_WIDTH));
    CSFontRow bit;
    CSFontRow rowMask;

    const CSFontRow* src = &((CSFontRow*)gCSFont)[(glyph / CRASH_SCREEN_FONT_CHARS_PER_ROW) * CRASH_SCREEN_FONT_CHAR_HEIGHT];
    RGBA16* dst = get_rendering_fb_pixel(startX, startY);

    for (u32 y = 0; y < CRASH_SCREEN_FONT_CHAR_HEIGHT; y++) {
        bit = startBit;
        rowMask = *src++;

        for (u32 x = 0; x < CRASH_SCREEN_FONT_CHAR_WIDTH; x++) {
            if (bit & rowMask) {
                apply_color(dst, newColor, alpha);
            }
            dst++;
            bit >>= 1;
        }

        dst += (SCREEN_WIDTH - CRASH_SCREEN_FONT_CHAR_WIDTH);
    }
}

// Copy the framebuffer data from gFramebuffers one frame at a time, forcing alpha to true to turn off broken anti-aliasing.
void crash_screen_take_screenshot(RGBA16* dst) {
    u32* src = FB_PTR_AS(u32);
    u32* ptr = (u32*)dst;
    const u32 mask = ((MSK_RGBA16_A << SIZEOF_BITS(RGBA16)) | MSK_RGBA16_A);

    for (size_t size = 0; size < FRAMEBUFFER_SIZE; size += sizeof(u32)) {
        *ptr++ = (*src++ | mask);
    }
}

void crash_screen_reset_framebuffer(_Bool drawBackground) {
    if (drawBackground) {
        bcopy(gZBuffer, FB_PTR_AS(void), FRAMEBUFFER_SIZE);
    } else {
        crash_screen_draw_dark_rect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, CS_DARKEN_TO_BLACK);
    }

    osWritebackDCacheAll();
}

void crash_screen_update_framebuffer(void) {
    osWritebackDCacheAll();

    OSMesgQueue* queue = &gActiveCSThreadInfo->mesgQueue;
    OSMesg* mesg = &gActiveCSThreadInfo->mesg;

    osViBlack(FALSE);
    osRecvMesg(queue, mesg, OS_MESG_BLOCK);
    osViSwapBuffer(FB_PTR_AS(void));
    osRecvMesg(queue, mesg, OS_MESG_BLOCK);

    if (++sRenderingFramebuffer == 3) {
        sRenderingFramebuffer = 0;
    }

    osWritebackDCacheAll();
}

void crash_screen_draw_scroll_bar(u32 topY, u32 bottomY, u32 numVisibleEntries, u32 numTotalEntries, u32 topVisibleEntry, RGBA32 color, _Bool drawBg) {
    const u32 x = (CRASH_SCREEN_X2 - 1);

    // The total height of the area the scroll bar can move.
    u32 scrollableHeight = (bottomY - topY);

    if (drawBg) {
        // Draw the background scroll bar
        const Alpha bgAlpha = (RGBA32_A(color) / 2);
        crash_screen_draw_rect(x, topY, 1, scrollableHeight, ((color & ~0xFF) | bgAlpha));
    }

    u32 bottomVisibleEntry = (topVisibleEntry + numVisibleEntries);

    u32 barTop    = (scrollableHeight * ((f32)   topVisibleEntry / (f32)numTotalEntries));
    u32 barBottom = (scrollableHeight * ((f32)bottomVisibleEntry / (f32)numTotalEntries));

    u32 barHeight = (barBottom - barTop);
    if (barHeight < 1) {
        barHeight = 1;
    }

    crash_screen_draw_rect(x, (topY + barTop), 1, barHeight, color);
}

// Draw the header.
void print_crash_screen_header(void) {
    u32 line = 0;
    // "HackerSM64 vX.X.X"
    crash_screen_print(TEXT_X(0), TEXT_Y(line),
        STR_COLOR_PREFIX"HackerSM64 v%s",
        COLOR_RGBA32_CRASH_HEADER,
        HACKERSM64_VERSION
    );

    // "START:controls"
    crash_screen_print(TEXT_X(19), TEXT_Y(line),
        STR_COLOR_PREFIX"%s:controls",
        COLOR_RGBA32_CRASH_HEADER, gCSControlDescriptions[CONT_DESC_SHOW_CONTROLS].control
    );

    // "<Page:XX>"
    crash_screen_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - STRLEN("<Page:XX>")), TEXT_Y(line),
        STR_COLOR_PREFIX"<Page:%02d>",
        COLOR_RGBA32_CRASH_HEADER, (gCSPageID + 1)
    );

    line++;

    crash_screen_draw_divider(DIVIDER_Y(line));

    struct CSPage* page = &gCSPages[gCSPageID];

    if (page->flags.printName) {
        crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_PAGE_NAME, page->name);

        line++;

        crash_screen_draw_divider(DIVIDER_Y(line));
    }

    osWritebackDCacheAll();
}

void crash_screen_draw_main(void) {
    crash_screen_reset_framebuffer(gCSDrawSavedScreenshot);

    if (gCSDrawCrashScreen) {
        if (gCSDrawSavedScreenshot) {
            // Draw the transparent background.
            crash_screen_draw_dark_rect(
                CRASH_SCREEN_X1, CRASH_SCREEN_Y1,
                CRASH_SCREEN_W,  CRASH_SCREEN_H,
                CS_DARKEN_THREE_QUARTERS
            );
        }

        print_crash_screen_header();

        struct CSPage* page = &gCSPages[gCSPageID];

        // Run the page-specific draw function.
        if (page->drawFunc == NULL) {
            crash_screen_print(TEXT_X(0), TEXT_Y(2), STR_COLOR_PREFIX"%s",
                COLOR_RGBA32_CRASH_PAGE_NAME, "THIS PAGE DOESN'T EXIST"
            );
        } else if (page->flags.crashed) {
            crash_screen_print(TEXT_X(0), TEXT_Y(2), STR_COLOR_PREFIX"%s",
                COLOR_RGBA32_CRASH_AT, "THIS PAGE HAS CRASHED"
            );
        } else {
            page->drawFunc();
        }

        if (gAddressSelectMenuOpen) {
            draw_address_select();
        }

        if (gCSDrawControls) {
            draw_controls_box();
        }
    }

    crash_screen_update_framebuffer();
}
