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

// The maximum number of characters in a single text label.
#define MAX_TEXT_LABEL_SIZE 50

struct TextLabel {
    u32 x;
    u32 y;
    char buffer[MAX_TEXT_LABEL_SIZE];
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
void print_text_fmt_int(s32 x, s32 y, char *str, s32 n) {
    char buffer[MAX_TEXT_LABEL_SIZE];
    sprintf(buffer, str, n);
    print_text(x, y, buffer);
}

/**
 * Prints text in the colorful lettering at given X, Y coordinates.
 */
void print_text(s32 x, s32 y, char *str) {
    // Don't continue if there is no memory to do so.
    if ((sTextLabels[sTextLabelsCount] = mem_pool_alloc(gEffectsMemoryPool,
                                                        sizeof(struct TextLabel))) == NULL) {
        return;
    }

    sTextLabels[sTextLabelsCount]->x = x;
    sTextLabels[sTextLabelsCount]->y = y;

    u32 i = 0;
    while (str[i] != '\0') {
        sTextLabels[sTextLabelsCount]->buffer[i] = str[i];
        i++;
    }
    sTextLabels[sTextLabelsCount]->buffer[i] = '\0';

    sTextLabelsCount++;
}

/**
 * Prints text in the colorful lettering, allowing for text alignment.
 */
void print_text_aligned(s32 x, s32 y, char *str, u32 alignment) {
    s32 strLength = get_string_width(str, main_hud_lut, &main_hud_utf8_lut);
    if (alignment == TEXT_ALIGN_RIGHT) {
        x -= strLength;
    } else if (alignment == TEXT_ALIGN_CENTER) {
        x -= strLength / 2;
    }
    print_text(x, y, str);
}

/**
 * Renders the text in sTextLabels on screen at the proper locations by iterating
 * a for loop.
 */
void render_text_labels(void) {
    s32 i;
    Mtx *mtx;

    if (sTextLabelsCount == 0) {
        return;
    }

    mtx = alloc_display_list(sizeof(*mtx));

    if (mtx == NULL) {
        sTextLabelsCount = 0;
        return;
    }

    guOrtho(mtx, 0.0f, SCREEN_WIDTH, 0.0f, SCREEN_HEIGHT, -10.0f, 10.0f, 1.0f);
    gSPPerspNormalize(gDisplayListHead++, 0xFFFF);
    gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(mtx), G_MTX_PROJECTION | G_MTX_LOAD | G_MTX_NOPUSH);
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);

    for (i = 0; i < sTextLabelsCount; i++) {
        print_hud_lut_string(sTextLabels[i]->x, 224 - sTextLabels[i]->y, sTextLabels[i]->buffer);
        mem_pool_free(gEffectsMemoryPool, sTextLabels[i]);
    }

    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);

    sTextLabelsCount = 0;
}
