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
void cs_set_scissor_box(ScreenCoord_s16 x1, ScreenCoord_s16 y1, ScreenCoord_s16 x2, ScreenCoord_s16 y2) {
    gCSScissorBox.x1 = x1;
    gCSScissorBox.y1 = y1;
    gCSScissorBox.x2 = x2;
    gCSScissorBox.y2 = y2;
}

// Sets the scissor box, cut by the previous scissor box.
void cs_add_scissor_box(ScreenCoord_s16 x1, ScreenCoord_s16 y1, ScreenCoord_s16 x2, ScreenCoord_s16 y2) {
    gCSScissorBox.x1 = MAX(gCSScissorBox.x1, x1);
    gCSScissorBox.y1 = MAX(gCSScissorBox.y1, y1);
    gCSScissorBox.x2 = MIN(gCSScissorBox.x2, x2);
    gCSScissorBox.y2 = MIN(gCSScissorBox.y2, y2);
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
ALWAYS_INLINE static RGBA16* get_rendering_fb_pixel(ScreenCoord_u32 x, ScreenCoord_u32 y) {
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

const Alpha gCSDarkenAlphas[CS_DARKEN_LIMIT] = {
    [CS_DARKEN_NONE              ] = 0x00, // 000/000
    [CS_DARKEN_HALF              ] = 0x7F, // 127/128
    [CS_DARKEN_THREE_QUARTERS    ] = 0xBF, // 191/192
    [CS_DARKEN_SEVEN_EIGHTHS     ] = 0xDF, // 223/224
    [CS_DARKEN_FIFTEEN_SIXTEENTHS] = 0xEF, // 239/240
    [CS_DARKEN_TO_BLACK          ] = 0xFF, // 255/256
};

// Apply color to an RGBA16 pixel with alpha blending.
static void apply_rgba16_color(RGBA16* dst, RGBA16 newColor, Alpha alpha) {
    if (alpha == MSK_RGBA32_A) {
        *dst = newColor;
    } else {
        *dst = cs_rgba16_blend(*dst, newColor, alpha);
    }
}

// For specific translucent black colors (see gCSDarkenAlphas), an even faster blending method can be used.
static u16 cs_get_darken_rgba16(RGBA16 color, Alpha alpha) {
    if ((color | MSK_RGBA16_A) != COLOR_RGBA16_BLACK) {
        return CS_DARKEN_NONE;
    }

    for (u16 darken = CS_DARKEN_NONE; darken < CS_DARKEN_LIMIT; darken++) {
        if (alpha == gCSDarkenAlphas[darken]) {
            return darken;
        }
    }

    return CS_DARKEN_NONE;
}
// Generate a mask for each component of RGBA16 to later mask the color and shift to the right:
// eg:                         RRRRRGGGGGBBBBBA
//  0: 0x00 alpha -> ((color & 1111111111111110) >> 0) (does nothing)
//  1: 0x7F alpha -> ((color & 1111011110111100) >> 1) (darken by 1/2)
//  2: 0xBF alpha -> ((color & 1110011100111000) >> 2) (darken by 3/4)
//  3: 0xDF alpha -> ((color & 1100011000110000) >> 3) (darken by 7/8)
//  4: 0xEF alpha -> ((color & 1000010000100000) >> 4) (darken by 15/16)
//  5: 0xFF alpha -> ((color & 0000000000000000) >> 5) (darken to black)
static RGBA16 generate_darken_rgba16_mask(u16 darken) {
    return ASSEMBLE_RGBA16_GRAYSCALE((MSK_RGBA16_C & ~BITMASK(darken)), 0);
}
static void apply_rgba16_darken(RGBA16* dst, RGBA16 mask, u16 darken) {
    *dst = (((*dst & mask) >> darken) | MSK_RGBA16_A);
}

static void apply_rgba16_color_or_darken(RGBA16* dst, RGBA16 newColor, Alpha alpha, RGBA16 mask, u16 darken) {
    if (darken != CS_DARKEN_NONE) {
        apply_rgba16_darken(dst, mask, darken);
    } else {
        apply_rgba16_color(dst, newColor, alpha);
    }
}

// Draws a rectangle.
void cs_draw_rect(ScreenCoord_s32 startX, ScreenCoord_s32 startY, ScreenCoord_s32 w, ScreenCoord_s32 h, RGBA32 color) {
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);

    const u16 darken = cs_get_darken_rgba16(newColor, alpha);
    const RGBA16 mask = generate_darken_rgba16_mask(darken);

    // Scale the rectangle to fit inside the scissor box:
    ScreenCoord_s32 x1 = startX;
    ScreenCoord_s32 y1 = startY;
    ScreenCoord_s32 x2 = (startX + w);
    ScreenCoord_s32 y2 = (startY + h);
    if (x1 <  gCSScissorBox.x1) x1 = gCSScissorBox.x1;
    if (y1 <  gCSScissorBox.y1) y1 = gCSScissorBox.y1;
    if (x2 >= gCSScissorBox.x2) x2 = gCSScissorBox.x2;
    if (y2 >= gCSScissorBox.y2) y2 = gCSScissorBox.y2;
    startX = x1;
    startY = y1;
    w = (x2 - x1);
    h = (y2 - y1);

    RGBA16* dst = get_rendering_fb_pixel(startX, startY);

    for (ScreenCoord_s32 y = 0; y < h; y++) {
        for (ScreenCoord_s32 x = 0; x < w; x++) {
            apply_rgba16_color_or_darken(dst, newColor, alpha, mask, darken);
            dst++;
        }
        dst += (SCREEN_WIDTH - w);
    }
}

// Draws an empty box.
void cs_draw_outline(ScreenCoord_s32 startX, ScreenCoord_s32 startY, ScreenCoord_s32 w, ScreenCoord_s32 h, RGBA32 color) {
    ScreenCoord_s32 x1 = startX;
    ScreenCoord_s32 y1 = startY;
    ScreenCoord_s32 x2 = startX + (w - 1);
    ScreenCoord_s32 y2 = startY + (h - 1);

    cs_draw_rect(x1, y1,       w, 1,       color); // Top line.
    cs_draw_rect(x1, y2,       w, 1,       color); // Bottom line.
    cs_draw_rect(x1, (y1 + 1), 1, (h - 2), color); // Left line.
    cs_draw_rect(x2, (y1 + 1), 1, (h - 2), color); // Right line.
}

// Draws a diamond shape.
void cs_draw_diamond(ScreenCoord_s32 startX, ScreenCoord_s32 startY, ScreenCoord_s32 w, ScreenCoord_s32 h, RGBA32 color) {
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);
    const u16 darken = cs_get_darken_rgba16(newColor, alpha);
    const RGBA16 mask = generate_darken_rgba16_mask(darken);

    RGBA16* dst = get_rendering_fb_pixel(startX, startY);

    const ScreenCoord_s32 middleX = (w / 2.0f);
    const ScreenCoord_s32 middleY = (h / 2.0f);

    f32 dx = ((f32)w / (f32)h);
    f32 d = 0.0f;

    for (ScreenCoord_s32 y = 0; y < h; y++) {
        for (ScreenCoord_s32 x = 0; x < w; x++) {
            if (absi(x - middleX) < d) {
                if (cs_is_in_scissor_box((startX + x), (startY + y))) {
                    apply_rgba16_color_or_darken(dst, newColor, alpha, mask, darken);
                }
            }
            dst++;
        }
        d += ((y < middleY) ? dx : -dx);
        dst += (SCREEN_WIDTH - w);
    }
}

// Draws a diamond then crops it using gCSScissorBox so that it looks like a triangle.
void cs_draw_triangle(ScreenCoord_s32 startX, ScreenCoord_s32 startY, ScreenCoord_s32 w, ScreenCoord_s32 h, RGBA32 color, enum CSDrawTriangleDirection direction) {
    CS_SCISSOR_BOX_START(startX, startY, (startX + w), (startY + h));

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

    CS_SCISSOR_BOX_END();
}

//! TODO:
// // Draws a line between any two points.
// void cs_draw_line(ScreenCoord_u32 x1, ScreenCoord_u32 y1, ScreenCoord_u32 x2, ScreenCoord_u32 y2, RGBA32 color) {
//     const Alpha alpha = RGBA32_A(color);
//     if (alpha == 0x00) {
//         return;
//     }
//     const RGBA16 newColor = RGBA32_TO_RGBA16(color);
//     const u16 darken = cs_get_darken_rgba16(newColor, alpha);
//     const RGBA16 mask = generate_darken_rgba16_mask(darken);

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
//             apply_rgba16_color_or_darken(dst, newColor, alpha, mask, darken);
//         }
//         x++;
//     }
// }

// Draw a single character from gCSFont to the framebuffer.
void cs_draw_glyph(ScreenCoord_u32 startX, ScreenCoord_u32 startY, uchar glyph, RGBA32 color) {
    if (glyph == CHAR_NULL) { // Null
        color = COLOR_RGBA32_CRASH_NULL_CHAR;
    }
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);
    const u16 darken = cs_get_darken_rgba16(newColor, alpha);
    const RGBA16 mask = generate_darken_rgba16_mask(darken);

    CSFontRow startBit = ((CSFontRow)BIT(SIZEOF_BITS(CSFontRow) - 1) >> ((glyph % CRASH_SCREEN_FONT_CHARS_PER_ROW) * CRASH_SCREEN_FONT_CHAR_WIDTH));
    CSFontRow bit;
    CSFontRow rowMask;

    const CSFontRow* src = &((CSFontRow*)gCSFont)[(glyph / CRASH_SCREEN_FONT_CHARS_PER_ROW) * CRASH_SCREEN_FONT_CHAR_HEIGHT];
    RGBA16* dst = get_rendering_fb_pixel(startX, startY);

    for (ScreenCoord_u32 y = 0; y < CRASH_SCREEN_FONT_CHAR_HEIGHT; y++) {
        bit = startBit;
        rowMask = *src++;

        for (ScreenCoord_u32 x = 0; x < CRASH_SCREEN_FONT_CHAR_WIDTH; x++) {
            if ((bit & rowMask) && cs_is_in_scissor_box((startX + x), (startY + y))) {
                apply_rgba16_color_or_darken(dst, newColor, alpha, mask, darken);
            }
            dst++;
            bit >>= 1;
        }
        dst += (SCREEN_WIDTH - CRASH_SCREEN_FONT_CHAR_WIDTH);
    }
}

UNUSED void cs_draw_custom_5x5_glyph(ScreenCoord_u32 startX, ScreenCoord_u32 startY, CSCustom5x5Glyph glyph, RGBA32 color) {
    const ScreenCoord_u32 w = 5;
    const ScreenCoord_u32 h = 5;
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);
    const u16 darken = cs_get_darken_rgba16(newColor, alpha);
    const RGBA16 mask = generate_darken_rgba16_mask(darken);
    RGBA16* dst = get_rendering_fb_pixel(startX, startY);

    u32 bit = BIT((w * h) - 1);
    for (ScreenCoord_u32 y = 0; y < h; y++) {
        for (ScreenCoord_u32 x = 0; x < w; x++) {
            if (glyph.raw & bit) {
                apply_rgba16_color_or_darken(dst, newColor, alpha, mask, darken);
            }
            dst++;
            bit >>= 1;
        }
        dst += (SCREEN_WIDTH - w);
    }
};

//! TODO:
// void cs_draw_texture(ScreenCoord_s32 startX, ScreenCoord_s32 startY, ScreenCoord_s32 w, ScreenCoord_s32 h, RGBA16* texture) {
//     if (texture == NULL) {
//         return;
//     }

//     RGBA16* src = texture;
//     RGBA16* dst = get_rendering_fb_pixel(startX, startY);

//     for (ScreenCoord_s32 y = 0; y < h; y++) {
//         for (ScreenCoord_s32 x = 0; x < w; x++) {
//             if (cs_is_in_scissor_box((startX + x), (startY + y))) {
//                 apply_rgba16_color(dst, *src, MSK_RGBA32_A);
//             }
//             src++;
//             dst++;
//         }
//         dst += (SCREEN_WIDTH - w);
//     }
// }

// Copy the framebuffer data from gFramebuffers one frame at a time, forcing alpha to true to turn off broken anti-aliasing.
void cs_take_screenshot_of_game(RGBA16* dst, size_t size) {
    RGBA16FILL* src = FB_PTR_AS(RGBA16FILL);
    RGBA16FILL* ptr = (RGBA16FILL*)dst;
    const RGBA16FILL mask = ((MSK_RGBA16_A << SIZEOF_BITS(RGBA16)) | MSK_RGBA16_A);

    for (size_t s = 0; s < size; s += sizeof(RGBA16FILL)) {
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

// // Emulators that the Instant Input patch should not be applied to
// #define INSTANT_INPUT_BLACKLIST (EMU_CONSOLE | EMU_WIIVC | EMU_ARES | EMU_SIMPLE64 | EMU_CEN64)

#define CRASH_SCREEN_NUM_FRAMEBUFFERS 3

// Cycle through the 3 framebuffers.
//! TODO: Instant input?
//! TODO: Fix flickering on emulators.
void cs_update_framebuffer(void) {
    osWritebackDCacheAll();

    OSMesgQueue* queue = &gActiveCSThreadInfo->mesgQueue;
    OSMesg* mesg = &gActiveCSThreadInfo->mesg;

    osViBlack(FALSE);
    osRecvMesg(queue, mesg, OS_MESG_BLOCK);
    osViSwapBuffer(FB_PTR_AS(void));
    osRecvMesg(queue, mesg, OS_MESG_BLOCK);

    // if (gEmulator & INSTANT_INPUT_BLACKLIST) {
        if (++sRenderingFramebuffer == CRASH_SCREEN_NUM_FRAMEBUFFERS) {
            sRenderingFramebuffer = 0;
        }
    // }

    osWritebackDCacheAll();
}

// Draw a scroll bar at the right edge of the crash screen.
void cs_draw_scroll_bar_impl(ScreenCoord_u32 x, ScreenCoord_u32 topY, ScreenCoord_u32 bottomY, u32 numVisibleEntries, u32 numTotalEntries, u32 topVisibleEntry, RGBA32 color, _Bool drawBg) {
    // The total height of the area the scroll bar can move.
    ScreenCoord_u32 scrollableHeight = (bottomY - topY);

    if (drawBg) {
        // Draw the background scroll bar
        cs_draw_rect(x, topY, 1, scrollableHeight, color);
        cs_draw_dark_rect(x, topY, 1, scrollableHeight, CS_DARKEN_HALF);
    }

    u32 bottomVisibleEntry = (topVisibleEntry + numVisibleEntries);

    ScreenCoord_u32 barTop    = (scrollableHeight * ((f32)   topVisibleEntry / (f32)numTotalEntries));
    ScreenCoord_u32 barBottom = (scrollableHeight * ((f32)bottomVisibleEntry / (f32)numTotalEntries));

    ScreenCoord_u32 barHeight = (barBottom - barTop);
    if (barHeight < 1) {
        barHeight = 1;
    }

    cs_draw_rect(x, (topY + barTop), 1, barHeight, color);
}

//! TODO: If `cs_draw_custom_5x5_glyph` gets used somewhere else and is therefore included in the rom, also use it here to save space.
RGBA32 cs_draw_thread_state_icon(ScreenCoord_u32 x, ScreenCoord_u32 y, OSThread* thread) {
    const s32 w = (CRASH_SCREEN_FONT_CHAR_WIDTH + 1);
    const s32 h = (CRASH_SCREEN_FONT_CHAR_WIDTH + 1);

    RGBA32 color = COLOR_RGBA32_WHITE;

    switch (thread->state) {
        case OS_STATE_STOPPED:
            color = COLOR_RGBA32_CRASH_NO;
            switch (thread->flags) {
                case OS_FLAG_CPU_BREAK:
                    cs_draw_outline(x, y, (w - 1), (h - 1), color);
                    break;
                case OS_FLAG_FAULT:
                    cs_draw_rect(x, y, (w - 1), (h - 1), color);
                    break;
                default:
                    cs_draw_glyph(x, y, 'x', color);
                    break;
            }
            break;
        case OS_STATE_RUNNABLE:
            color = COLOR_RGBA32_VERY_LIGHT_YELLOW;
            cs_draw_rect(x, y, 1, (h - 1), color);
            cs_draw_triangle((x + 2), (y - 1), (w - 2), h, color, CS_TRI_RIGHT);
            break;
        case OS_STATE_RUNNING:
            color = COLOR_RGBA32_CRASH_YES;
            cs_draw_triangle(x, (y - 1), w, h, color, CS_TRI_RIGHT);
            break;
        case OS_STATE_WAITING:
            color = COLOR_RGBA32_GRAY;
            cs_draw_rect(x, y, (w / 3), (h - 1), color);
            cs_draw_rect((x + (w / 3) + 1), y, (w / 3), (h - 1), color);
            break;
    }

    return color;
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

// Draws the 'L' and 'R' triangles.
void cs_draw_LR_triangles(void) {
    cs_set_scissor_box(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT);

    const ScreenCoord_s32 triWidth = 14; // Width of the triangles.
    const ScreenCoord_s32 triHeight = (triWidth * 2); // Height of the triangles.

    const ScreenCoord_s32 yPos = SCREEN_CENTER_Y; // Height of the triangles.

    const ScreenCoord_s32 triSeparation = 2; // Separation between the edge of the triangle and the edge of the crash screen.
    const ScreenCoord_s32 txtSeparation = 1; // Separation between the edge of the text and the edge of the triangle.

    const ScreenCoord_s32 triY = (yPos - (triHeight / 2)); // Center triangles vertically.
    const ScreenCoord_s32 txtY = ((yPos - (TEXT_HEIGHT(1) / 2)) + 2); // Center text vertically

    u16 buttonDown    = gCSCompositeController->buttonDown;
    u16 buttonPressed = gCSCompositeController->buttonPressed;

    ScreenCoord_s32 L_shift = ((BITFLAG_BOOL(buttonDown, L_TRIG) + BITFLAG_BOOL(buttonPressed, L_TRIG)));
    ScreenCoord_s32 R_shift = ((BITFLAG_BOOL(buttonDown, R_TRIG) + BITFLAG_BOOL(buttonPressed, R_TRIG)));

    const RGBA32 bgColor = RGBA32_SET_ALPHA(COLOR_RGBA32_BLACK, gCSDarkenAlphas[cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_BG_OPACITY)]);

    // 'L' triangle:
    const ScreenCoord_s32 L_edge = ((CRASH_SCREEN_X1 - triSeparation) - L_shift); // Edge of the 'L' triangle.
    cs_draw_triangle((L_edge - triWidth), triY, triWidth, triHeight, bgColor, CS_TRI_LEFT);
    cs_draw_glyph(((L_edge - TEXT_WIDTH(1)) - txtSeparation), txtY, (STR_L)[0], COLOR_RGBA32_CRASH_HEADER);
    // 'R' triangle:
    const s32 R_edge = ((CRASH_SCREEN_X2 + triSeparation) + R_shift); // Edge of the 'R' triangle.
    cs_draw_triangle(R_edge, triY, triWidth, triHeight, bgColor, CS_TRI_RIGHT);
    cs_draw_glyph(((R_edge + 1) + txtSeparation), txtY, (STR_R)[0], COLOR_RGBA32_CRASH_HEADER);

    cs_reset_scissor_box();
}

u32 gCSFrameCounter = 0;

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

        if (!CS_IS_DEFAULT_PRINT_COLOR_DEFAULT()) {
            gCSDefaultPrintColor = CS_DEFAULT_PRINT_COLOR;
        }

        gCSFrameCounter++;
    }

    cs_update_framebuffer();
}
