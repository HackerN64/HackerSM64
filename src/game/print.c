#include <ultra64.h>

#include "config.h"
#include "game_init.h"
#include "memory.h"
#include "print.h"
#include "segment2.h"
#include "ingame_menu.h"

/**
 * This file handles printing and formatting the colorful text that
 * appears when printing things such as "PRESS START".
 */

struct TextLabel {
    u32 x;
    u32 y;
    char buffer[50];
};

/**
 * Stores the text to be rendered on screen
 * and how they are to be rendered.
 */
struct TextLabel *sTextLabels[52];
s16 sTextLabelsCount = 0;

/**
 * Takes a number, formats the number, and prints it
 * at the given X & Y coordinates.
 */
void print_text_fmt_int(s32 x, s32 y, const char *str, s32 n) {
    char buffer[50];
    sprintf(buffer, str, n);
    print_text(x, y, str);
}

/**
 * Prints text in the colorful lettering at given X, Y coordinates.
 */
void print_text(s32 x, s32 y, const char *str) {
    char c = 0;
    s32 length = 0;
    s32 srcIndex = 0;

    // Don't continue if there is no memory to do so.
    if ((sTextLabels[sTextLabelsCount] = mem_pool_alloc(gEffectsMemoryPool,
                                                        sizeof(struct TextLabel))) == NULL) {
        return;
    }

    sTextLabels[sTextLabelsCount]->x = x;
    sTextLabels[sTextLabelsCount]->y = y;

    strcpy(sTextLabels[sTextLabelsCount]->buffer, str);

    sTextLabelsCount++;
}

/**
 * Prints text in the colorful lettering centered at given X, Y coordinates.
 */
void print_text_centered(s32 x, s32 y, const char *str) {
    print_text(x - strlen(str) * 6, y, str);
}

/**
 * Adds an individual glyph to be rendered.
 */
u8 add_glyph_texture(s8 glyphIndex) {
    const struct AsciiCharLUTEntry *glyphs = segmented_to_virtual(main_hud_lut);

    gDPPipeSync(gDisplayListHead++);
    gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, glyphs[glyphIndex].texture);
    gSPDisplayList(gDisplayListHead++, dl_hud_img_load_tex_block);

    return glyphs[glyphIndex].kerning;
}

u8 add_utf8_glyph_texture(struct UnicodeCharLUTEntry *utf8Entry) {
    gDPPipeSync(gDisplayListHead++);
    gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, utf8Entry->texture);
    gSPDisplayList(gDisplayListHead++, dl_hud_img_load_tex_block);
    return utf8Entry->kerning;
}

#ifndef WIDESCREEN
/**
 * Clips textrect into the boundaries defined.
 */
void clip_to_bounds(s32 *x, s32 *y) {
    if (*x < TEXRECT_MIN_X) {
        *x = TEXRECT_MIN_X;
    }

    if (*x > TEXRECT_MAX_X) {
        *x = TEXRECT_MAX_X;
    }

    if (*y < TEXRECT_MIN_Y) {
        *y = TEXRECT_MIN_Y;
    }

    if (*y > TEXRECT_MAX_Y) {
        *y = TEXRECT_MAX_Y;
    }
}
#endif

/**
 * Renders the glyph that's set at the given position.
 */
void render_textrect(s32 x, s32 y) {
    s32 rectBaseX = x;
    s32 rectBaseY = 224 - y;
    s32 rectX;
    s32 rectY;

#ifndef WIDESCREEN
    // For widescreen we must allow drawing outside the usual area
    clip_to_bounds(&rectBaseX, &rectBaseY);
#endif
    rectX = rectBaseX;
    rectY = rectBaseY;
    gSPTextureRectangle(gDisplayListHead++, rectX << 2, rectY << 2, (rectX + 15) << 2,
                        (rectY + 15) << 2, G_TX_RENDERTILE, 0, 0, 4 << 10, 1 << 10);
}

/**
 * Renders the text in sTextLabels on screen at the proper locations by iterating
 * a for loop.
 */
void render_text_labels(void) {
    s32 i;
    s32 j;
    s8 glyphIndex;
    Mtx *mtx;
    u8 curOffset;
    u8 kerning;

    if (sTextLabelsCount == 0) {
        return;
    }

    mtx = alloc_display_list(sizeof(*mtx));

    if (mtx == NULL) {
        sTextLabelsCount = 0;
        return;
    }

    guOrtho(mtx, 0.0f, SCREEN_WIDTH, 0.0f, SCREEN_HEIGHT, -10.0f, 10.0f, 1.0f);
    gSPPerspNormalize((Gfx *) (gDisplayListHead++), 0xFFFF);
    gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(mtx), G_MTX_PROJECTION | G_MTX_LOAD | G_MTX_NOPUSH);
    gSPDisplayList(gDisplayListHead++, dl_hud_img_begin);

    for (i = 0; i < sTextLabelsCount; i++) {
        j = 0;
        curOffset = 0;
        while (sTextLabels[i]->buffer[j] != 0) {
            if (sTextLabels[i]->buffer[j] != ' ') {
#ifdef VERSION_EU
                // Beta Key was removed by EU, so glyph slot reused.
                // This produces a colorful Ü.
                if (glyphIndex == GLYPH_BETA_KEY) {
                    add_glyph_texture(GLYPH_U);
                    render_textrect(sTextLabels[i]->x, sTextLabels[i]->y, j);

                    add_glyph_texture(GLYPH_UMLAUT);
                    render_textrect(sTextLabels[i]->x, sTextLabels[i]->y + 3, j);
                } else {
                    add_glyph_texture(glyphIndex);
                    render_textrect(sTextLabels[i]->x, sTextLabels[i]->y, j);
                }
#else
                // Lookup correct glyph
                if (!(sTextLabels[i]->buffer[j] & 0x80)) {
                    kerning = add_glyph_texture(sTextLabels[i]->buffer[j] - ' ');
                } else {
                    struct UnicodeCharLUTEntry *utf8Entry = utf8_lookup(&main_hud_utf8_lut, sTextLabels[i]->buffer, &j);
                    kerning = add_utf8_glyph_texture(utf8Entry);
                }
                // Handle custom offsets for ' and " glyphs
                if (sTextLabels[i]->buffer[j] == '\'') {
                    render_textrect(sTextLabels[i]->x + curOffset - 2, sTextLabels[i]->y + 7);
                } else if (sTextLabels[i]->buffer[j] == '"') {
                    render_textrect(sTextLabels[i]->x + curOffset + 1, sTextLabels[i]->y + 7);
                } else {
                    render_textrect(sTextLabels[i]->x + curOffset, sTextLabels[i]->y);
                }
                curOffset += kerning;
#endif          
            } else {
                curOffset += 12;
            }
            j++;
        }

        mem_pool_free(gEffectsMemoryPool, sTextLabels[i]);
    }

    gSPDisplayList(gDisplayListHead++, dl_hud_img_end);

    sTextLabelsCount = 0;
}
