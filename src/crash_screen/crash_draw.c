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


// Crash screen font. Each row of the image fits in one u32 pointer.
ALIGNED32 static const FontRow gCrashScreenFont[CRASH_SCREEN_FONT_CHAR_HEIGHT * CRASH_SCREEN_FONT_NUM_ROWS] = {
    #include "textures/crash_screen/crash_screen_font.custom.ia1.inc.c"
};

static RGBA16* crash_screen_get_framebuffer_pixel_ptr(u32 x, u32 y) {
    return (gFramebuffers[sRenderingFramebuffer] + (SCREEN_WIDTH * y) + x);
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
    if (darken == 0) {
        return;
    }

    const RGBA16Component componentMask = (MSK_RGBA16_C & ~BITMASK(darken));
    RGBA16 mask = GPACK_RGBA5551(0, 0, 0, 0);
    for (u32 i = SIZ_RGBA16_A; i < (SIZ_RGBA16_C * 3); i += SIZ_RGBA16_C) {
        mask |= (componentMask << i);
    }

    RGBA16* dst = crash_screen_get_framebuffer_pixel_ptr(startX, startY);

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
    // const _Bool opaque = (alpha == MSK_RGBA32_A);
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);

    RGBA16* dst = crash_screen_get_framebuffer_pixel_ptr(startX, startY);

    for (u32 y = 0; y < h; y++) {
        for (u32 x = 0; x < w; x++) {
            apply_color(dst, newColor, alpha);
            dst++;
        }

        dst += (SCREEN_WIDTH - w);
    }
}

// Draws a triangle pointing upwards or downwards.
void crash_screen_draw_vertical_triangle(u32 startX, u32 startY, u32 w, u32 h, RGBA32 color, _Bool flip) {
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    // const _Bool opaque = (alpha == MSK_RGBA32_A);
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);
    const f32 middle = (w / 2.0f);
    f32 d = 0.0f;
    f32 t = (middle / (f32)h);
    if (flip) {
        d = (middle - t);
        t = -t;
    }

    RGBA16* dst = crash_screen_get_framebuffer_pixel_ptr(startX, startY);

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
    // const _Bool opaque = (alpha == MSK_RGBA32_A);
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);
    const f32 middle = (h / 2.0f);
    const f32 t = ((f32)w / middle);
    f32 x1 = w;

    RGBA16* dst = crash_screen_get_framebuffer_pixel_ptr(startX, startY);
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
    // const _Bool opaque = (alpha == MSK_RGBA32_A);
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
        dst = crash_screen_get_framebuffer_pixel_ptr(x, y);
        apply_color(dst, newColor, alpha);
        x++;
    }
}

void crash_screen_draw_glyph(u32 startX, u32 startY, unsigned char glyph, RGBA32 color) {
    if (glyph == CHAR_NULL) { // Null
        color = COLOR_RGBA32_GRAY;
    }
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    // const _Bool opaque = (alpha == MSK_RGBA32_A);
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);
    const FontRow startBit = ((FontRow)BIT(31) >> ((glyph % CRASH_SCREEN_FONT_CHARS_PER_ROW) * CRASH_SCREEN_FONT_CHAR_WIDTH));
    FontRow bit;
    FontRow rowMask;

    const FontRow* src = &gCrashScreenFont[(glyph / CRASH_SCREEN_FONT_CHARS_PER_ROW) * CRASH_SCREEN_FONT_CHAR_HEIGHT];
    RGBA16* dst = crash_screen_get_framebuffer_pixel_ptr(startX, startY);

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

// Copy the framebuffer data from gFramebuffers one frame at a time, forcing alpha to true to disable broken anti-aliasing.
void crash_screen_take_screenshot(RGBA16* dst) {
    u32* src = (u32*)gFramebuffers[sRenderingFramebuffer];
    u32* ptr = (u32*)dst;
    const u32 mask = ((MSK_RGBA16_A << 16) | MSK_RGBA16_A);

    for (size_t size = 0; size < FRAMEBUFFER_SIZE; size += sizeof(u32)) {
        *ptr++ = (*src++ | mask);
    }
}

void crash_screen_reset_framebuffer(_Bool drawBackground) {
    if (drawBackground) {
        bcopy(gZBuffer, (void*)PHYSICAL_TO_VIRTUAL(gFramebuffers[sRenderingFramebuffer]), FRAMEBUFFER_SIZE);
    } else {
        crash_screen_draw_dark_rect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, 5);
    }

    osWritebackDCacheAll();
}

void crash_screen_update_framebuffer(struct CrashScreen* crashScreen) {
    osWritebackDCacheAll();

    osViBlack(FALSE);
    osRecvMesg(&crashScreen->mesgQueue, &crashScreen->mesg, OS_MESG_BLOCK);
    osViSwapBuffer((void*)PHYSICAL_TO_VIRTUAL(gFramebuffers[sRenderingFramebuffer]));
    osRecvMesg(&crashScreen->mesgQueue, &crashScreen->mesg, OS_MESG_BLOCK);

    if (++sRenderingFramebuffer == 3) {
        sRenderingFramebuffer = 0;
    }

    osWritebackDCacheAll();
}

#ifdef CRASH_SCREEN_CRASH_SCREEN
extern u8 _crash_screen_crash_screenSegmentRomStart[];
extern u8 _crash_screen_crash_screenSegmentRomEnd[];
extern void dma_read(u8* dest, u8* srcStart, u8* srcEnd);

void draw_crashed_image_i4(void) {
    Texture srcColor;
    Color color; // I4 color
    RGBA16* fb_u16 = gFramebuffers[sRenderingFramebuffer];

    u8* segStart = _crash_screen_crash_screenSegmentRomStart;
    u8* segEnd = _crash_screen_crash_screenSegmentRomEnd;
    size_t size = (uintptr_t) (segEnd - segStart);
    Texture* fb_u8 = (u8*)((uintptr_t) fb_u16 + (SCREEN_SIZE * sizeof(RGBA16*)) - size);

    // Make sure the source image is the correct size.
    if (size != SRC_IMG_SIZE) {
        return;
    }

    // DMA the data directly onto the framebuffer.
    dma_read(fb_u8, segStart, segEnd);

    const s32 diff = (SIZ_RGBA16_C - SIZ_I4); // 1
    const s32 shiftUpper = (SIZ_I4 * 1); // 0
    const s32 shiftLower = (SIZ_I4 * 0); // 0
    const s32 diffShiftUpper = (diff - shiftUpper); // -3
    const s32 diffShiftLower = (diff - shiftLower); //  1

    // Copy and convert the image data from the framebuffer to itself.
    for (u32 i = 0; i < SRC_IMG_SIZE; i++) {
        srcColor = *fb_u8++;

        // Convert upper 4 bits to RGBA16
        color = (srcColor & (MSK_I4 << shiftUpper));
        *fb_u16++ = (
            SSHIFTL(color, (IDX_RGBA16_R + diffShiftUpper)) | // color <<  8
            SSHIFTL(color, (IDX_RGBA16_G + diffShiftUpper)) | // color <<  3
            SSHIFTL(color, (IDX_RGBA16_B + diffShiftUpper)) | // color >>  2
            MSK_RGBA16_A
        );

        // Convert lower 4 bits to RGBA16
        color = (srcColor & (MSK_I4 << shiftLower));
        *fb_u16++ = (
            SSHIFTL(color, (IDX_RGBA16_R + diffShiftLower)) | // color << 12
            SSHIFTL(color, (IDX_RGBA16_G + diffShiftLower)) | // color <<  7
            SSHIFTL(color, (IDX_RGBA16_B + diffShiftLower)) | // color <<  2
            MSK_RGBA16_A
        );
    }
}
#endif // CRASH_SCREEN_CRASH_SCREEN
