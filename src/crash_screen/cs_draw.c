#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "cs_controls.h"
#include "cs_main.h"
#include "cs_pages.h"
#include "cs_print.h"
#include "cs_settings.h"

#include "cs_draw.h"

#include "buffers/framebuffers.h"
#include "buffers/zbuffer.h"
#include "game/game_init.h"


// Crash screen font. Each row of the image fits in one u32 pointer.
ALIGNED32 static const Texture gCSFont[CRASH_SCREEN_FONT_CHAR_HEIGHT * CRASH_SCREEN_FONT_NUM_ROWS * sizeof(CSFontRow)] = {
    #include "textures/crash_screen/crash_screen_font.custom.ia1.inc.c"
};

// The rectangular area that can be drawn in. Almost everything is culled outside of the box.
ALIGNED16 CSScissorBox gCSScissorBox = {
    .x1 = SCISSOR_BOX_DEFAULT_X1,
    .y1 = SCISSOR_BOX_DEFAULT_Y1,
    .x2 = SCISSOR_BOX_DEFAULT_X2,
    .y2 = SCISSOR_BOX_DEFAULT_Y2,
};

// Sets the scissor box.
void cs_set_scissor_box(CSScreenCoord_s32 x1, CSScreenCoord_s32 y1, CSScreenCoord_s32 x2, CSScreenCoord_s32 y2) {
    gCSScissorBox.x1 = x1;
    gCSScissorBox.y1 = y1;
    gCSScissorBox.x2 = x2;
    gCSScissorBox.y2 = y2;
}

// Resets the scissor box to the defaults.
void cs_reset_scissor_box(void) {
    cs_set_scissor_box(
        SCISSOR_BOX_DEFAULT_X1,
        SCISSOR_BOX_DEFAULT_Y1,
        SCISSOR_BOX_DEFAULT_X2,
        SCISSOR_BOX_DEFAULT_Y2
    );
}

// Checks whether the pixel at the given screen coordinates are within the scissor box.
static _Bool cs_is_in_scissor_box(s32 x, s32 y) {
    return (
        (x >= gCSScissorBox.x1) &&
        (y >= gCSScissorBox.y1) &&
        (x <  gCSScissorBox.x2) &&
        (y <  gCSScissorBox.y2)
    );
}

// Get a pointer to the framebuffer pixel at the given screen coordinates.
static RGBA16* get_rendering_fb_pixel(CSScreenCoord_u32 x, CSScreenCoord_u32 y) {
    return (FB_PTR_AS(RGBA16) + (SCREEN_WIDTH * y) + x);
}

/**
 * A fast lerp function between two RGBA16 colors.
 */
static RGBA16 cs_rgba16_blend(RGBA16 a, RGBA16 b, Alpha fac) {
    RGBA16 ds, d = MSK_RGBA16_A;
    RGBA16 s = (MSK_RGBA16_C << SIZ_RGBA16_A);

    for (s32 i = 0; i < 3; i++) {
        ds = (a & s); // Current color component of 'a'.
        d |= ((((fac * ((b & s) - ds)) >> 8) + ds) & s);
        s <<= SIZ_RGBA16_C;
    }

    return d;
}

// Apply color to an RGBA16 pixel with alpha blending.
static void apply_color(RGBA16* dst, RGBA16 newColor, Alpha alpha) {
    if (alpha == MSK_RGBA32_A) {
        *dst = newColor;
    } else {
        *dst = cs_rgba16_blend(*dst, newColor, alpha);
    }
}

// Darkens a rectangular area. This is faster than the color blending done by
// cs_draw_rect, so it's used for the large background rectangle.
// 0  - does nothing
// 1  - darken by 1/2
// 2  - darken by 3/4
// 3  - darken by 7/8
// 4  - darken by 15/16
// 5+ - darken to black
void cs_draw_dark_rect(CSScreenCoord_s32 startX, CSScreenCoord_s32 startY, CSScreenCoord_s32 w, CSScreenCoord_s32 h, u32 darken) {
    if (darken == CS_DARKEN_NONE) {
        return;
    }

    const RGBA16Component componentMask = (MSK_RGBA16_C & ~BITMASK(darken));
    RGBA16 mask = GPACK_RGBA5551(0, 0, 0, 0);
    for (u32 i = SIZ_RGBA16_A; i < (SIZ_RGBA16_C * 3); i += SIZ_RGBA16_C) {
        mask |= (componentMask << i);
    }

    RGBA16* dst = get_rendering_fb_pixel(startX, startY);

    for (CSScreenCoord_s32 y = 0; y < h; y++) {
        for (CSScreenCoord_s32 x = 0; x < w; x++) {
            if (cs_is_in_scissor_box((startX + x), (startY + y))) {
                *dst = (((*dst & mask) >> darken) | MSK_RGBA16_A);
            }
            dst++;
        }
        dst += (SCREEN_WIDTH - w);
    }
}

// Draws a rectangle.
void cs_draw_rect(CSScreenCoord_s32 startX, CSScreenCoord_s32 startY, CSScreenCoord_s32 w, CSScreenCoord_s32 h, RGBA32 color) {
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);

    RGBA16* dst = get_rendering_fb_pixel(startX, startY);

    for (CSScreenCoord_s32 y = 0; y < h; y++) {
        for (CSScreenCoord_s32 x = 0; x < w; x++) {
            if (cs_is_in_scissor_box((startX + x), (startY + y))) {
                apply_color(dst, newColor, alpha);
            }
            dst++;
        }
        dst += (SCREEN_WIDTH - w);
    }
}

// Draws an empty box.
void cs_draw_outline(CSScreenCoord_s32 startX, CSScreenCoord_s32 startY, CSScreenCoord_s32 w, CSScreenCoord_s32 h, RGBA32 color) {
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);

    RGBA16* dst = get_rendering_fb_pixel(startX, startY);

    for (CSScreenCoord_s32 y = 0; y < h; y++) {
        for (CSScreenCoord_s32 x = 0; x < w; x++) {
            if (
                cs_is_in_scissor_box((startX + x), (startY + y)) &&
                ((y == 0) || (y == (h - 1)) || (x == 0) || (x == (w - 1)))
            ) {
                apply_color(dst, newColor, alpha);
            }
            dst++;
        }
        dst += (SCREEN_WIDTH - w);
    }
}

// Draws a diamond shape.
void cs_draw_diamond(CSScreenCoord_s32 startX, CSScreenCoord_s32 startY, CSScreenCoord_s32 w, CSScreenCoord_s32 h, RGBA32 color) {
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);

    RGBA16* dst = get_rendering_fb_pixel(startX, startY);

    CSScreenCoord_s32 middleX = (w / 2.0f);
    CSScreenCoord_s32 middleY = (h / 2.0f);

    f32 dx = ((f32)w / (f32)h);
    f32 d = 0.0f;

    for (CSScreenCoord_s32 y = 0; y < h; y++) {
        for (CSScreenCoord_s32 x = 0; x < w; x++) {
            if (absi(x - middleX) < d) {
                if (cs_is_in_scissor_box((startX + x), (startY + y))) {
                    apply_color(dst, newColor, alpha);
                }
            }
            dst++;
        }
        d += ((y < middleY) ? dx : -dx);
        dst += (SCREEN_WIDTH - w);
    }
}

// Draws a diamond then crops it using gCSScissorBox so that it looks like a triangle.
void cs_draw_triangle(CSScreenCoord_s32 startX, CSScreenCoord_s32 startY, CSScreenCoord_s32 w, CSScreenCoord_s32 h, RGBA32 color, enum CSDrawTriangleDirection direction) {
    CSScissorBox temp = gCSScissorBox;

    cs_set_scissor_box(startX, startY, (startX + w), (startY + h));

    switch (direction) {
        case CS_TRI_DOWN:
            startY -= (h + 1); // Flip vertically.
            FALL_THROUGH;
        case CS_TRI_UP:
            h *= 2;
            break;
        case CS_TRI_RIGHT:
            startX -= (w + 1); // Flip horizontally.
            FALL_THROUGH;
        case CS_TRI_LEFT: 
            w *= 2;
            break;
    }

    cs_draw_diamond(startX, startY, w, h, color);

    gCSScissorBox = temp;
}

// // Draws a line between any two points.
// void cs_draw_line(CSScreenCoord_u32 x1, CSScreenCoord_u32 y1, CSScreenCoord_u32 x2, CSScreenCoord_u32 y2, RGBA32 color) {
//     const Alpha alpha = RGBA32_A(color);
//     if (alpha == 0x00) {
//         return;
//     }
//     const RGBA16 newColor = RGBA32_TO_RGBA16(color);

//     RGBA16* dst;

//     // Swap the points so that the second point is after the first.
//     if (x1 > x2) SWAP(x1, x2);
//     if (y1 > y2) SWAP(y1, y2);

//     const f32 slope = (f32)(y2 - y1) / (x2 - x1);

//     f32 x = x1;
//     f32 y;
//     while (x <= x2) {
//         y = ((slope * (x - x1)) + y1);
//         if (cs_is_in_scissor_box(x, y)) {
//             dst = get_rendering_fb_pixel(x, y);
//             apply_color(dst, newColor, alpha);
//         }
//         x++;
//     }
// }

// Draw a single character from gCSFont to the framebuffer.
void cs_draw_glyph(CSScreenCoord_u32 startX, CSScreenCoord_u32 startY, uchar glyph, RGBA32 color) {
    if (glyph == CHAR_NULL) { // Null
        color = COLOR_RGBA32_CRASH_NULL_CHAR;
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

    for (CSScreenCoord_u32 y = 0; y < CRASH_SCREEN_FONT_CHAR_HEIGHT; y++) {
        bit = startBit;
        rowMask = *src++;

        for (CSScreenCoord_u32 x = 0; x < CRASH_SCREEN_FONT_CHAR_WIDTH; x++) {
            if ((bit & rowMask) && cs_is_in_scissor_box((startX + x), (startY + y))) {
                apply_color(dst, newColor, alpha);
            }
            dst++;
            bit >>= 1;
        }

        dst += (SCREEN_WIDTH - CRASH_SCREEN_FONT_CHAR_WIDTH);
    }
}

// void cs_draw_texture(CSScreenCoord_s32 startX, CSScreenCoord_s32 startY, CSScreenCoord_s32 w, CSScreenCoord_s32 h, RGBA16* texture) {
//     if (texture == NULL) {
//         return;
//     }

//     RGBA16* src = texture;
//     RGBA16* dst = get_rendering_fb_pixel(startX, startY);

//     for (CSScreenCoord_s32 y = 0; y < h; y++) {
//         for (CSScreenCoord_s32 x = 0; x < w; x++) {
//             if (cs_is_in_scissor_box((startX + x), (startY + y))) {
//                 apply_color(dst, *src, MSK_RGBA32_A);
//             }
//             src++;
//             dst++;
//         }
//         dst += (SCREEN_WIDTH - w);
//     }
// }

// Copy the framebuffer data from gFramebuffers one frame at a time, forcing alpha to true to turn off broken anti-aliasing.
void cs_take_screenshot_of_game(RGBA16* dst, size_t size) {
    u32* src = FB_PTR_AS(u32);
    u32* ptr = (u32*)dst;
    const u32 mask = ((MSK_RGBA16_A << SIZEOF_BITS(RGBA16)) | MSK_RGBA16_A);

    for (size_t s = 0; s < size; s += sizeof(u32)) {
        *ptr++ = (*src++ | mask);
    }
}

// Set the entire framebuffer either to the saved screenshot or to black.
void cs_reset_framebuffer(_Bool drawBackground) {
    if (drawBackground) {
        bcopy(gZBuffer, FB_PTR_AS(void), sizeof(gZBuffer));
    } else {
        cs_draw_dark_rect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, CS_DARKEN_TO_BLACK);
    }

    osWritebackDCacheAll();
}

// Cycle through the 3 framebuffers.
//! TODO: Instant input?
void cs_update_framebuffer(void) {
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

// Draw a scroll bar at the right edge of the crash screen.
void cs_draw_scroll_bar_impl(CSScreenCoord_u32 x, CSScreenCoord_u32 topY, CSScreenCoord_u32 bottomY, CSScreenCoord_u32 numVisibleEntries, CSScreenCoord_u32 numTotalEntries, CSScreenCoord_u32 topVisibleEntry, RGBA32 color, _Bool drawBg) {
    // The total height of the area the scroll bar can move.
    CSScreenCoord_u32 scrollableHeight = (bottomY - topY);

    if (drawBg) {
        // Draw the background scroll bar
        const Alpha bgAlpha = (RGBA32_A(color) / 2);
        cs_draw_rect(x, topY, 1, scrollableHeight, RGBA32_SET_ALPHA(color, bgAlpha));
    }

    CSScreenCoord_u32 bottomVisibleEntry = (topVisibleEntry + numVisibleEntries);

    CSScreenCoord_u32 barTop    = (scrollableHeight * ((f32)   topVisibleEntry / (f32)numTotalEntries));
    CSScreenCoord_u32 barBottom = (scrollableHeight * ((f32)bottomVisibleEntry / (f32)numTotalEntries));

    CSScreenCoord_u32 barHeight = (barBottom - barTop);
    if (barHeight < 1) {
        barHeight = 1;
    }

    cs_draw_rect(x, (topY + barTop), 1, barHeight, color);
}

// RGBA32 cs_thread_draw_highlight(OSThread* thread, CSScreenCoord_u32 y) {
//     RGBA32 color = 0x00000000;

//     if (thread == gCrashedThread) {
//         color = COLOR_RGBA32_CRASH_PC_HIGHLIGHT;
//     } else if (thread == __osRunningThread) {
//         color = COLOR_RGBA32_CRASH_RUNNING_HIGHLIGHT;
//     } else if (thread == gInspectThread) {
//         color = COLOR_RGBA32_CRASH_INSPECT_HIGHLIGHT;
//     }

//     if (color) {
//         cs_draw_row_box_thread(CS_POPUP_THREADS_BG_X1, y, color);
//     }

//     return color;
// }

RGBA32 cs_draw_thread_state_icon(CSScreenCoord_u32 x, CSScreenCoord_u32 y, OSThread* thread) {
    const s32 w = 6;
    const s32 h = 6;

    u16 state = thread->state;
    u16 flags = thread->flags;

    switch (state) {
        case OS_STATE_STOPPED:
            switch (flags) {
                case OS_FLAG_CPU_BREAK:
                    cs_draw_outline(x, y, (w - 1), (h - 1), COLOR_RGBA32_CRASH_NO);
                    break;
                case OS_FLAG_FAULT:
                    cs_draw_rect(x, y, (w - 1), (h - 1), COLOR_RGBA32_CRASH_NO);
                    break;
                default:
                    cs_draw_glyph(x, y, 'x', COLOR_RGBA32_CRASH_NO);
                    break;
            }
            return COLOR_RGBA32_CRASH_NO;
        case OS_STATE_RUNNABLE:
            cs_draw_rect(x, y, 1, (h - 1), COLOR_RGBA32_VERY_LIGHT_YELLOW);
            cs_draw_triangle((x + 2), (y - 1), (w - 2), h, COLOR_RGBA32_VERY_LIGHT_YELLOW, CS_TRI_RIGHT);
            return COLOR_RGBA32_VERY_LIGHT_YELLOW;
        case OS_STATE_RUNNING:
            cs_draw_triangle(x, (y - 1), w, h, COLOR_RGBA32_CRASH_YES, CS_TRI_RIGHT);
            return COLOR_RGBA32_CRASH_YES;
        case OS_STATE_WAITING:
            cs_draw_rect(x, y, (w / 3), (h - 1), COLOR_RGBA32_GRAY);
            cs_draw_rect((x + (w / 3) + 1), y, (w / 3), (h - 1), COLOR_RGBA32_GRAY);
            return COLOR_RGBA32_GRAY;
    }

    return COLOR_RGBA32_WHITE;
}

// Page header draw function.
CSTextCoord_u32 cs_page_header_draw(void) {
    CSTextCoord_u32 line = 0;

    CSPage* page = cs_get_current_page();
    cs_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_PAGE_NAME, page->name);

    // "<Page:##>"
    cs_print(TEXT_X(CRASH_SCREEN_NUM_CHARS_X - STRLEN("<Page:##>")), TEXT_Y(line),
        STR_COLOR_PREFIX"<Page:%02d>",
        COLOR_RGBA32_CRASH_HEADER, (gCSPageID + 1)
    );

    line++;

    osWritebackDCacheAll();

    return line;
}

static const Alpha sCSBackgroundAlphas[CS_DARKEN_LIMIT] = {
    [CS_DARKEN_NONE              ] = 0x00,
    [CS_DARKEN_HALF              ] = 0x7F,
    [CS_DARKEN_THREE_QUARTERS    ] = 0xBF,
    [CS_DARKEN_SEVEN_EIGHTHS     ] = 0xDF,
    [CS_DARKEN_FIFTEEN_SIXTEENTHS] = 0xEF,
    [CS_DARKEN_TO_BLACK          ] = 0xFF,
};

// Draws the 'L' and 'R' triangles.
void cs_draw_LR_triangles(void) {
    cs_set_scissor_box(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT);

    const CSScreenCoord_s32 triWidth = 14; // Width of the triangles.
    const CSScreenCoord_s32 triHeight = (triWidth * 2); // Height of the triangles.

    const CSScreenCoord_s32 yPos = SCREEN_CENTER_Y; // Height of the triangles.

    const CSScreenCoord_s32 triSeparation = 2; // Separation between the edge of the triangle and the edge of the crash screen.
    const CSScreenCoord_s32 txtSeparation = 1; // Separation between the edge of the text and the edge of the triangle.

    const CSScreenCoord_s32 triY = (yPos - (triHeight / 2)); // Center triangles vertically.
    const CSScreenCoord_s32 txtY = ((yPos - (TEXT_HEIGHT(1) / 2)) + 2); // Center text vertically

    u16 buttonDown    = gCSCompositeController->buttonDown;
    u16 buttonPressed = gCSCompositeController->buttonPressed;

    CSScreenCoord_s32 L_shift = ((BITFLAG_BOOL(buttonDown, L_TRIG) + BITFLAG_BOOL(buttonPressed, L_TRIG)));
    CSScreenCoord_s32 R_shift = ((BITFLAG_BOOL(buttonDown, R_TRIG) + BITFLAG_BOOL(buttonPressed, R_TRIG)));

    const RGBA32 bgColor = RGBA32_SET_ALPHA(COLOR_RGBA32_NONE, sCSBackgroundAlphas[cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_BG_OPACITY)]);

    // 'L' triangle:
    const CSScreenCoord_s32 L_edge = ((CRASH_SCREEN_X1 - triSeparation) - L_shift); // Edge of the 'L' triangle.
    cs_draw_triangle((L_edge - triWidth), triY, triWidth, triHeight, bgColor, CS_TRI_LEFT);
    cs_draw_glyph(((L_edge - TEXT_WIDTH(1)) - txtSeparation), txtY, (STR_L)[0], COLOR_RGBA32_CRASH_HEADER);
    // 'R' triangle:
    const s32 R_edge = ((CRASH_SCREEN_X2 + triSeparation) + R_shift); // Edge of the 'R' triangle.
    cs_draw_triangle(R_edge, triY, triWidth, triHeight, bgColor, CS_TRI_RIGHT);
    cs_draw_glyph(((R_edge + 1) + txtSeparation), txtY, (STR_R)[0], COLOR_RGBA32_CRASH_HEADER);

    cs_reset_scissor_box();
}

// Crash screen main draw function.
void cs_draw_main(void) {
    const _Bool drawScreenshot = cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_DRAW_SCREENSHOT);

    // Draw the background screenshot of the game.
    cs_set_scissor_box(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT);
    cs_reset_framebuffer(drawScreenshot);
    cs_reset_scissor_box();

    // If Z is not held, draw the crash screen.
    if (!(gCSCompositeController->buttonDown & Z_TRIG)) {
        if (drawScreenshot) {
            // Draw the transparent background.
            cs_draw_dark_rect(
                CRASH_SCREEN_X1, CRASH_SCREEN_Y1,
                CRASH_SCREEN_W,  CRASH_SCREEN_H,
                cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_BG_OPACITY)
            );
        }

        // Draw the L/R triangles.
        if (can_switch_page() && cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_DRAW_LR_ARROWS)) {
            cs_draw_LR_triangles();
        }

        // Draw the page header.
        CSTextCoord_u32 line = cs_page_header_draw();

        CSPage* page = cs_get_current_page();

        // Run the page-specific draw function.
        if (page->drawFunc == NULL) {
            cs_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"%s",
                COLOR_RGBA32_CRASH_PAGE_NAME, "THIS PAGE DOESN'T EXIST"
            );
        } else if (page->flags.crashed) {
            cs_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"%s",
                COLOR_RGBA32_CRASH_AT, "THIS PAGE HAS CRASHED\n\nPRESS A+B+START TO ATTEMPT TO REVIVE PAGE"
            );
        } else {
            page->drawFunc();
        }

        cs_draw_divider(DIVIDER_Y(line));

        CSPopup* popup = cs_get_current_popup();
        if (popup != NULL) {
            if (popup->drawFunc != NULL) {
                popup->drawFunc();
            }
        }
    }

    cs_update_framebuffer();
}
