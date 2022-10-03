#include <ultra64.h>

#include "actors/common1.h"
#include "area.h"
#include "audio/external.h"
#include "camera.h"
#include "course_table.h"
#include "dialog_ids.h"
#include "engine/math_util.h"
#include "eu_translation.h"
#include "segment_symbols.h"
#include "game_init.h"
#include "gfx_dimensions.h"
#include "ingame_menu.h"
#include "level_update.h"
#include "levels/castle_grounds/header.h"
#include "memory.h"
#include "print.h"
#include "save_file.h"
#include "segment2.h"
#include "segment7.h"
#include "seq_ids.h"
#include "sm64.h"
#include "types.h"
#include "config.h"
#include "puppycam2.h"
#include "main.h"

#ifdef VERSION_EU
#undef LANGUAGE_FUNCTION
#define LANGUAGE_FUNCTION gInGameLanguage
#endif

u16 gDialogColorFadeTimer;
s8 gLastDialogLineNum;
s32 gDialogVariable;
u16 gDialogTextAlpha;
s8 gRedCoinsCollected;

#if MULTILANG
#define seg2_course_name_table course_name_table_eu_en
#define seg2_act_name_table act_name_table_eu_en
#define seg2_dialog_table dialog_table_eu_en
#endif

s16 gInGameLanguage = LANGUAGE_ENGLISH;
s16 gLoadedLanguage = LANGUAGE_ENGLISH;

void *languageTable[][3] = {
    {&seg2_dialog_table, &seg2_course_name_table, &seg2_act_name_table}, // In EU, this is just mirroring English.
#if MULTILANG
    {&dialog_table_eu_en, &course_name_table_eu_en, &act_name_table_eu_en},
    {&dialog_table_eu_fr, &course_name_table_eu_fr, &act_name_table_eu_fr},
    {&dialog_table_eu_de, &course_name_table_eu_de, &act_name_table_eu_de},
#endif
};

extern u8 gLastCompletedCourseNum;
extern u8 gLastCompletedStarNum;

enum DialogBoxState {
    DIALOG_STATE_OPENING,
    DIALOG_STATE_VERTICAL,
    DIALOG_STATE_HORIZONTAL,
    DIALOG_STATE_CLOSING
};

enum DialogBoxPageState {
    DIALOG_PAGE_STATE_NONE,
    DIALOG_PAGE_STATE_SCROLL,
    DIALOG_PAGE_STATE_END
};

enum DialogBoxType {
    DIALOG_TYPE_ROTATE, // used in NPCs and level messages
    DIALOG_TYPE_ZOOM    // used in signposts and wall signs and etc
};

#define DEFAULT_DIALOG_BOX_ANGLE 90.0f
#define DEFAULT_DIALOG_BOX_SCALE 19.0f

s8 gDialogBoxState = DIALOG_STATE_OPENING;
f32 gDialogBoxOpenTimer = DEFAULT_DIALOG_BOX_ANGLE;
f32 gDialogBoxScale = DEFAULT_DIALOG_BOX_SCALE;
s16 gDialogScrollOffsetY = 0;
s8 gDialogBoxType = DIALOG_TYPE_ROTATE;
s16 gDialogID = DIALOG_NONE;
s16 gLastDialogPageStrPos = 0;
s16 gDialogTextPos = 0;
s8 gDialogLineNum = 1;
s8 gLastDialogResponse = 0;
u8 gMenuHoldKeyIndex = 0;
u8 gMenuHoldKeyTimer = 0;
s32 gDialogResponse = DIALOG_RESPONSE_NONE;


void create_dl_identity_matrix(void) {
    Mtx *matrix = (Mtx *) alloc_display_list(sizeof(Mtx));

    if (matrix == NULL) {
        return;
    }

#ifndef GBI_FLOATS
    matrix->m[0][0] = 0x00010000;    matrix->m[1][0] = 0x00000000;    matrix->m[2][0] = 0x00000000;    matrix->m[3][0] = 0x00000000;
    matrix->m[0][1] = 0x00000000;    matrix->m[1][1] = 0x00010000;    matrix->m[2][1] = 0x00000000;    matrix->m[3][1] = 0x00000000;
    matrix->m[0][2] = 0x00000001;    matrix->m[1][2] = 0x00000000;    matrix->m[2][2] = 0x00000000;    matrix->m[3][2] = 0x00000000;
    matrix->m[0][3] = 0x00000000;    matrix->m[1][3] = 0x00000001;    matrix->m[2][3] = 0x00000000;    matrix->m[3][3] = 0x00000000;
#else
    guMtxIdent(matrix);
#endif

    gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(matrix), G_MTX_MODELVIEW | G_MTX_LOAD | G_MTX_NOPUSH);
    gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(matrix), G_MTX_PROJECTION | G_MTX_LOAD | G_MTX_NOPUSH);
}

void create_dl_translation_matrix(s8 pushOp, f32 x, f32 y, f32 z) {
    Mtx *matrix = (Mtx *) alloc_display_list(sizeof(Mtx));

    if (matrix == NULL) {
        return;
    }

    guTranslate(matrix, x, y, z);

    if (pushOp == MENU_MTX_PUSH) {
        gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(matrix), G_MTX_MODELVIEW | G_MTX_MUL | G_MTX_PUSH);
    }

    if (pushOp == MENU_MTX_NOPUSH) {
        gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(matrix), G_MTX_MODELVIEW | G_MTX_MUL | G_MTX_NOPUSH);
    }
}

void create_dl_rotation_matrix(s8 pushOp, f32 a, f32 x, f32 y, f32 z) {
    Mtx *matrix = (Mtx *) alloc_display_list(sizeof(Mtx));

    if (matrix == NULL) {
        return;
    }

    guRotate(matrix, a, x, y, z);

    if (pushOp == MENU_MTX_PUSH) {
        gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(matrix), G_MTX_MODELVIEW | G_MTX_MUL | G_MTX_PUSH);
    }

    if (pushOp == MENU_MTX_NOPUSH) {
        gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(matrix), G_MTX_MODELVIEW | G_MTX_MUL | G_MTX_NOPUSH);
    }
}

void create_dl_scale_matrix(s8 pushOp, f32 x, f32 y, f32 z) {
    Mtx *matrix = (Mtx *) alloc_display_list(sizeof(Mtx));

    if (matrix == NULL) {
        return;
    }

    guScale(matrix, x, y, z);

    if (pushOp == MENU_MTX_PUSH) {
        gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(matrix), G_MTX_MODELVIEW | G_MTX_MUL | G_MTX_PUSH);
    }

    if (pushOp == MENU_MTX_NOPUSH) {
        gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(matrix), G_MTX_MODELVIEW | G_MTX_MUL | G_MTX_NOPUSH);
    }
}

void create_dl_ortho_matrix(void) {
    Mtx *matrix = (Mtx *) alloc_display_list(sizeof(Mtx));

    if (matrix == NULL) {
        return;
    }

    create_dl_identity_matrix();

    guOrtho(matrix, 0.0f, SCREEN_WIDTH, 0.0f, SCREEN_HEIGHT, -10.0f, 10.0f, 1.0f);

    // Should produce G_RDPHALF_1 in Fast3D
    gSPPerspNormalize(gDisplayListHead++, 0xFFFF);

    gSPMatrix(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(matrix), G_MTX_PROJECTION | G_MTX_MUL | G_MTX_NOPUSH);
}

// Determine the UTF8 character to render, given a string and the current position in the string.
// Return the struct of the relevant character, and increment the position in the string by either 1 or 2.
struct Utf8CharLUTEntry *utf8_lookup(struct Utf8LUT *lut, char *str, s32 *strPos) {
    u32 codepoint;
    struct Utf8CharLUTEntry *usedLUT;
    u32 length;

    lut = segmented_to_virtual(lut);
    if (!(str[*strPos] & 0x20)) {
        codepoint = ((str[*strPos] & 0x1F) << 6) | (str[++(*strPos)] & 0x3F);
        usedLUT = segmented_to_virtual(lut->lut2Bytes);
        length = lut->length2Bytes;
    } else if (!(str[*strPos] & 0x10)) {
        codepoint = ((str[*strPos] & 0xF) << 12) | ((str[++(*strPos)] & 0x3F) << 6) | (str[++(*strPos)] & 0x3F);
        usedLUT = segmented_to_virtual(lut->lut3Bytes);
        length = lut->length3Bytes;
    } else {
        codepoint = ((str[*strPos] & 0x7) << 18) | ((str[++(*strPos)] & 0x3F) << 12) | ((str[++(*strPos)] & 0x3F) << 6) | (str[++(*strPos)] & 0x3F);
        usedLUT = segmented_to_virtual(lut->lut4Bytes);
        length = lut->length4Bytes;
    }

    s32 start = 0;
    s32 end = length - 1;
    s32 mid = (start + end) / 2;

    while (start <= end) {
        if (usedLUT[mid].codepoint == codepoint) {
            return &usedLUT[mid];
        }

        if (usedLUT[mid].codepoint > codepoint) {
            end = mid - 1;
        } else {
            start = mid + 1;
        }

        mid = (start + end) / 2;
    }
    return segmented_to_virtual(lut->missingChar);
}

s32 get_string_length(char *str, struct AsciiCharLUTEntry *asciiLut, struct Utf8LUT *utf8LUT) {
    s32 length = 0;
    s32 strPos = 0;

    asciiLut = segmented_to_virtual(asciiLut);

    while (str[strPos] != '\0') {
        if (str[strPos] & 0x80) {
            length += utf8_lookup(utf8LUT, str, &strPos)->kerning;
        } else {
            length += asciiLut[str[strPos] - ' '].kerning;
        }
        strPos++;
    }

    return length;
}

u8 render_generic_ascii_char(char c) {
    struct AsciiCharLUTEntry *fontLUT = segmented_to_virtual(main_font_lut);
    const Texture *texture = fontLUT[c - ' '].texture;

    if (texture == NULL) {
        return 0;
    }

    gDPPipeSync(gDisplayListHead++);
    gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_IA, G_IM_SIZ_16b, 1, texture);
    gSPDisplayList(gDisplayListHead++, dl_ia_text_tex_settings);

    return fontLUT[c - ' '].kerning;
}

u8 render_generic_unicode_char(char *str, s32 *strPos) {
    struct Utf8CharLUTEntry *utf8Entry = utf8_lookup(&main_font_utf8_lut, str, strPos);

    gDPPipeSync(gDisplayListHead++);
    gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_IA, G_IM_SIZ_16b, 1, utf8Entry->texture);
    gSPDisplayList(gDisplayListHead++, dl_ia_text_tex_settings);

    if (utf8Entry->flags & GENERIC_TEXT_DIACRITIC_MASK) {
        struct DiacriticLUTEntry *diacriticLUT = segmented_to_virtual(&main_font_diacritic_lut);
        struct DiacriticLUTEntry *diacritic = &diacriticLUT[(utf8Entry->flags & GENERIC_TEXT_DIACRITIC_MASK) - 1];
        
        if (diacritic->xOffset | diacritic->yOffset) {
            create_dl_translation_matrix(MENU_MTX_PUSH, diacritic->xOffset, diacritic->yOffset, 0.0f);
        }

        gDPPipeSync(gDisplayListHead++);
        gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_IA, G_IM_SIZ_16b, 1, diacritic->texture);
        gSPDisplayList(gDisplayListHead++, dl_ia_text_tex_settings);

        if (diacritic->xOffset | diacritic->yOffset) {
            gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
        }
    }

    return utf8Entry->kerning;  
}

#define MAX_STRING_WIDTH 16

/**
 * Prints a generic white string.
 * In JP/EU a IA1 texture is used but in US a IA4 texture is used.
 */
void print_generic_string(s16 x, s16 y, char *str) {
    s32 strPos = 0;
    u8 lineNum = 1;

    s16 colorLoop;
    ColorRGBA rgbaColors = { 0x00, 0x00, 0x00, 0x00 };
    u8 customColor = 0;
    u8 diffTmp     = 0;
    u8 kerning     = 0;

    create_dl_translation_matrix(MENU_MTX_PUSH, x, y, 0.0f);

    while (str[strPos] != '\0') {
        switch (str[strPos]) {
            /**case DIALOG_CHAR_COLOR:
                customColor = 1;
                strPos++;
                for (colorLoop = (strPos + 8); strPos < colorLoop; ++strPos) {
                    diffTmp = 0;
                    if ((str[strPos] >= 0x24)
                     && (str[strPos] <= 0x29)) {
                        diffTmp = 0x1A;
                    } else if (str[strPos] >= 0x10) {
                        customColor = 2;
                        strPos = (colorLoop - 8);
                        for (diffTmp = 0; diffTmp < 8; ++diffTmp) {
                            if (str[strPos + diffTmp] != 0x9F) {
                                break;
                            }
                        }
                        if (diffTmp == 8) {
                            strPos += diffTmp;
                        }
                        break;
                    }
                    if (((8 - (colorLoop - strPos)) % 2) == 0) {
                        rgbaColors[(8 - (colorLoop - strPos)) / 2] = (((str[strPos] - diffTmp) & 0x0F) << 4);
                    } else {
                        rgbaColors[(8 - (colorLoop - strPos)) / 2] += ((str[strPos] - diffTmp) & 0x0F);
                    }
                }
                strPos--;
                if (customColor == 1) {
                    gDPSetEnvColor(gDisplayListHead++, rgbaColors[0],
                                                    rgbaColors[1],
                                                    rgbaColors[2],
                                                    rgbaColors[3]);
                } else if (customColor == 2) {
                    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, 255); // TODO: Is it possible to retrieve the original color that was set before print_generic_string was called?
                    customColor = 0;
                }
                break;**/
            case '\n':
                gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
                create_dl_translation_matrix(MENU_MTX_PUSH, x, y - (lineNum * MAX_STRING_WIDTH), 0.0f);
                lineNum++;
                break;
            case '/':
                create_dl_translation_matrix(MENU_MTX_NOPUSH, SPACE_KERNING(segmented_to_virtual(main_font_lut)) * 2, 0.0f, 0.0f);
                break;
            case ' ':
                create_dl_translation_matrix(MENU_MTX_NOPUSH, SPACE_KERNING(segmented_to_virtual(main_font_lut)), 0.0f, 0.0f);
                break;
            default:
                if (!(str[strPos] & 0x80)) {
                    kerning = render_generic_ascii_char(str[strPos]);
                } else {
                    kerning = render_generic_unicode_char(str, &strPos);
                }
                create_dl_translation_matrix(MENU_MTX_NOPUSH, kerning, 0.0f, 0.0f);
                break;
        }

        strPos++;
    }

    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
}

/**
 * Prints a hud string in the colorful font.
 */
void print_hud_lut_string(s16 x, s16 y, char *str) {
    s32 strPos = 0;
    struct AsciiCharLUTEntry *hudLUT = segmented_to_virtual(main_hud_lut); // 0-9 A-Z HUD Color Font
    u32 curX = x;
    u32 curY = y;
    u32 renderX, renderY;
    struct Utf8CharLUTEntry *utf8Entry;

    u32 xStride; // X separation

    /**if (hudLUT == HUD_TEXT_MODE_1CYCLE) {
        xStride = 16;
    } else { // HUD_TEXT_MODE_1CYCLE
        xStride = 12; //? Shindou uses this.
    }**/

    while (str[strPos] != 0x00) {
        switch (str[strPos]) {
            case ' ':
                curX += SPACE_KERNING(hudLUT);
                break;
            default:
                gDPPipeSync(gDisplayListHead++);

                if (!(str[strPos] & 0x80)) {
                    const Texture *tex = hudLUT[str[strPos] - ' '].texture;
                    gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, tex);
                    xStride = hudLUT[str[strPos] - ' '].kerning;
                } else {
                    utf8Entry = utf8_lookup(&main_hud_utf8_lut, str, &strPos);
                    if ((utf8Entry->flags & GENERIC_TEXT_DIACRITIC_MASK) == TEXT_DIACRITIC_UMLAUT_UPPERCASE) {
                        renderX = curX;
                        renderY = curY - 4;
                        gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, &texture_hud_char_umlaut);
                        gSPDisplayList(gDisplayListHead++, dl_rgba16_load_tex_block);
                        gSPTextureRectangle(gDisplayListHead++, renderX << 2, renderY << 2, (renderX + 16) << 2,
                                    (renderY + 16) << 2, G_TX_RENDERTILE, 0, 0, 1 << 10, 1 << 10);
                        gDPPipeSync(gDisplayListHead++);
                    }
                    gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, utf8Entry->texture);
                    xStride = utf8Entry->kerning;
                }

                renderX = curX;
                renderY = curY;
                if (str[strPos] == '\'') {
                    renderX -= 2;
                    renderY -= 7;
                } else if (str[strPos] == '"') {
                    renderX += 1;
                    renderY -= 7;
                } else if (str[strPos] == ',') {
                    renderX -= 4;
                    renderY += 7;
                } else if (str[strPos] == '.') {
                    renderX -= 2;
                    renderY += 1;
                }

                gSPDisplayList(gDisplayListHead++, dl_rgba16_load_tex_block);
                gSPTextureRectangle(gDisplayListHead++, renderX << 2, renderY << 2, (renderX + 16) << 2,
                                    (renderY + 16) << 2, G_TX_RENDERTILE, 0, 0, 1 << 10, 1 << 10);

                curX += xStride;
        }
        strPos++;
    }
}

/**
 * Prints a menu white string in the smaller font.
 * Only available in the file select and star select menus.
 */
void print_menu_generic_string(s16 x, s16 y, char *str) {
    s32 strPos = 0;
    u32 curX = x;
    u32 curY = y;
    u32 kerning;
    struct AsciiCharLUTEntry *fontLUT = segmented_to_virtual(menu_font_lut);

    while (str[strPos] != '\0') {
        switch (str[strPos]) {
            case ' ':
                curX += SPACE_KERNING(fontLUT);
                break;
            default:
                if (str[strPos] & 0x80) {
                    struct Utf8CharLUTEntry *utf8Entry = utf8_lookup(&menu_font_utf8_lut, str, &strPos);
                    gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_IA, G_IM_SIZ_8b, 1, utf8Entry->texture);
                    kerning = utf8Entry->kerning;
                } else {
                    gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_IA, G_IM_SIZ_8b, 1, fontLUT[str[strPos] - ' '].texture);
                    kerning = fontLUT[str[strPos] - ' '].kerning;
                }
                gDPLoadSync(gDisplayListHead++);
                gDPLoadBlock(gDisplayListHead++, G_TX_LOADTILE, 0, 0, 8 * 8 - 1, CALC_DXT(8, G_IM_SIZ_8b_BYTES));
                gSPTextureRectangle(gDisplayListHead++, curX << 2, curY << 2, (curX + 8) << 2,
                                    (curY + 8) << 2, G_TX_RENDERTILE, 0, 0, 1 << 10, 1 << 10);

                curX += kerning;
        }
        strPos++;
    }
}

/**
 * Variants of the above that allow for text alignment.
 */
void print_generic_string_aligned(s16 x, s16 y, char *str, u32 alignment) {
    s32 strLength = get_string_length(str, main_font_lut, &main_font_utf8_lut);
    if (alignment == TEXT_ALIGN_RIGHT) {
        x -= strLength;
    } else if (alignment == TEXT_ALIGN_CENTER) {
        x -= strLength / 2;
    }
    print_generic_string(x, y, str);
}

void print_hud_lut_string_aligned(s16 x, s16 y, char *str, u32 alignment) {
    s32 strLength = get_string_length(str, main_hud_lut, &main_hud_utf8_lut);
    if (alignment == TEXT_ALIGN_RIGHT) {
        x -= strLength;
    } else if (alignment == TEXT_ALIGN_CENTER) {
        x -= strLength / 2;
    }
    print_hud_lut_string(x, y, str);
}

void print_menu_generic_string_aligned(s16 x, s16 y, char *str, u32 alignment) {
    s32 strLength = get_string_length(str, menu_font_lut, &menu_font_utf8_lut);
    if (alignment == TEXT_ALIGN_RIGHT) {
        x -= strLength;
    } else if (alignment == TEXT_ALIGN_CENTER) {
        x -= strLength / 2;
    }
    print_menu_generic_string(x, y, str);
}

/**
 * Prints a string in the green credits font.
 */
void print_credits_string(s16 x, s16 y, const char *str) {
    s32 strPos = 0;
    struct AsciiCharLUTEntry *fontLUT = segmented_to_virtual(main_credits_font_lut);
    u32 curX = x;
    u32 curY = y;

    gDPSetTile(gDisplayListHead++, G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0,
                G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD);
    gDPTileSync(gDisplayListHead++);
    gDPSetTile(gDisplayListHead++, G_IM_FMT_RGBA, G_IM_SIZ_16b, 2, 0, G_TX_RENDERTILE, 0,
                G_TX_CLAMP, 3, G_TX_NOLOD, G_TX_CLAMP, 3, G_TX_NOLOD);
    gDPSetTileSize(gDisplayListHead++, G_TX_RENDERTILE, 0, 0, (8 - 1) << G_TEXTURE_IMAGE_FRAC, (8 - 1) << G_TEXTURE_IMAGE_FRAC);

    while (str[strPos] != '\0') {
        switch (str[strPos]) {
            case ' ':
                curX += SPACE_KERNING(main_credits_font_lut);
                break;
            default:
                gDPPipeSync(gDisplayListHead++);
                gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, fontLUT[str[strPos] - ' '].texture);
                gDPLoadSync(gDisplayListHead++);
                gDPLoadBlock(gDisplayListHead++, G_TX_LOADTILE, 0, 0, 8 * 8 - 1, CALC_DXT(8, G_IM_SIZ_16b_BYTES));
                gSPTextureRectangle(gDisplayListHead++, curX << 2, curY << 2, (curX + 8) << 2,
                                    (curY + 8) << 2, G_TX_RENDERTILE, 0, 0, 1 << 10, 1 << 10);
                curX += fontLUT[str[strPos] - ' '].kerning;
                break;
        }
        strPos++;
    }
}

void handle_menu_scrolling(s8 scrollDirection, s8 *currentIndex, s8 minIndex, s8 maxIndex) {
    u8 index = 0;

    if (scrollDirection == MENU_SCROLL_VERTICAL) {
        if ((gPlayer3Controller->rawStickY >  60) || (gPlayer3Controller->buttonDown & (U_CBUTTONS | U_JPAD))) index++;
        if ((gPlayer3Controller->rawStickY < -60) || (gPlayer3Controller->buttonDown & (D_CBUTTONS | D_JPAD))) index += 2;
    } else if (scrollDirection == MENU_SCROLL_HORIZONTAL) {
        if ((gPlayer3Controller->rawStickX >  60) || (gPlayer3Controller->buttonDown & (R_CBUTTONS | R_JPAD))) index += 2;
        if ((gPlayer3Controller->rawStickX < -60) || (gPlayer3Controller->buttonDown & (L_CBUTTONS | L_JPAD))) index++;
    }

    if (((index ^ gMenuHoldKeyIndex) & index) == 2) {
        if (*currentIndex != maxIndex) {
            play_sound(SOUND_MENU_CHANGE_SELECT, gGlobalSoundSource);
            (*currentIndex)++;
        }
    }

    if (((index ^ gMenuHoldKeyIndex) & index) == 1) {
        if (*currentIndex != minIndex) {
            play_sound(SOUND_MENU_CHANGE_SELECT, gGlobalSoundSource);
            (*currentIndex)--;
        }
    }

    if (gMenuHoldKeyTimer == 10) {
        gMenuHoldKeyTimer = 8;
        gMenuHoldKeyIndex = 0;
    } else {
        gMenuHoldKeyTimer++;
        gMenuHoldKeyIndex = index;
    }

    if ((index & 3) == 0) {
        gMenuHoldKeyTimer = 0;
    }
}

s32 get_string_width(char *str) {
    s16 strPos = 0;
    s16 width = 0;
    struct AsciiCharLUTEntry *fontLUT = segmented_to_virtual(main_font_lut);

    while (str[strPos] != '\0') {
        width += fontLUT[str[strPos] - ' '].kerning;
        strPos++;
    }
    return width;
}

s32 get_str_x_pos_from_center(s16 centerPos, char *str, UNUSED f32 scale) {
    return (s16)(centerPos - (s16)(get_string_width(str) / 2.0f));
}

void print_hud_my_score_coins(s32 useCourseCoinScore, s8 fileIndex, s8 courseIndex, s16 x, s16 y) {
    char strNumCoins[10];
    s16 numCoins;

    if (!useCourseCoinScore) {
        numCoins = (u16)(save_file_get_max_coin_score(courseIndex) & 0xFFFF);
    } else {
        numCoins = save_file_get_course_coin_score(fileIndex, courseIndex);
    }

    if (numCoins != 0) {
        sprintf(strNumCoins, "✪×%d", numCoins);
        print_hud_lut_string(x, y, strNumCoins);
    }
}

void print_hud_my_score_stars(s8 fileIndex, s8 courseIndex, s16 x, s16 y) {
    char strStarCount[10];
    s16 starCount = save_file_get_course_star_count(fileIndex, courseIndex);

    if (starCount != 0) {
        sprintf(strStarCount, "★×%d", starCount);
        print_hud_lut_string(x, y, strStarCount);
    }
}

s32 get_dialog_id(void) {
    return gDialogID;
}

void create_dialog_box(s16 dialog) {
    if (gDialogID == DIALOG_NONE) {
        gDialogID = dialog;
        gDialogBoxType = DIALOG_TYPE_ROTATE;
    }
}

void create_dialog_box_with_var(s16 dialog, s32 dialogVar) {
    if (gDialogID == DIALOG_NONE) {
        gDialogID = dialog;
        gDialogVariable = dialogVar;
        gDialogBoxType = DIALOG_TYPE_ROTATE;
    }
}

void create_dialog_inverted_box(s16 dialog) {
    if (gDialogID == DIALOG_NONE) {
        gDialogID = dialog;
        gDialogBoxType = DIALOG_TYPE_ZOOM;
    }
}

void create_dialog_box_with_response(s16 dialog) {
    if (gDialogID == DIALOG_NONE) {
        gDialogID = dialog;
        gDialogBoxType = DIALOG_TYPE_ROTATE;
        gLastDialogResponse = 1;
    }
}

void reset_dialog_render_state(void) {
    level_set_transition(0, NULL);

    gDialogBoxScale = 19.0f;
    gDialogBoxOpenTimer = 90.0f;
    gDialogBoxState = DIALOG_STATE_OPENING;
    gDialogID = DIALOG_NONE;
    gDialogTextPos = 0;
    gLastDialogResponse = 0;
    gLastDialogPageStrPos = 0;
    gDialogResponse = DIALOG_RESPONSE_NONE;
}

void render_dialog_box_type(struct DialogEntry *dialog, s8 linesPerBox) {
    create_dl_translation_matrix(MENU_MTX_NOPUSH, dialog->leftOffset, dialog->width, 0);

    switch (gDialogBoxType) {
        case DIALOG_TYPE_ROTATE: // Renders a dialog black box with zoom and rotation
            if ((gDialogBoxState == DIALOG_STATE_OPENING)
             || (gDialogBoxState == DIALOG_STATE_CLOSING)) {
                create_dl_scale_matrix(MENU_MTX_NOPUSH, (1.0f / gDialogBoxScale), (1.0f / gDialogBoxScale), 1.0f);
                // convert the speed into angle
                create_dl_rotation_matrix(MENU_MTX_NOPUSH, (gDialogBoxOpenTimer * 4.0f), 0, 0, 1.0f);
            }
            gDPSetEnvColor(gDisplayListHead++, 0, 0, 0, 150);
            break;
        case DIALOG_TYPE_ZOOM: // Renders a dialog white box with zoom
            if (gDialogBoxState == DIALOG_STATE_OPENING || gDialogBoxState == DIALOG_STATE_CLOSING) {
                create_dl_translation_matrix(MENU_MTX_NOPUSH, (65.0f - (65.0f / gDialogBoxScale)), ((40.0f / gDialogBoxScale) - 40), 0);
                create_dl_scale_matrix(MENU_MTX_NOPUSH, (1.0f / gDialogBoxScale), (1.0f / gDialogBoxScale), 1.0f);
            }
            gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, 150);
            break;
    }

    create_dl_translation_matrix(MENU_MTX_PUSH, -7.0f, 5.0f, 0);
    create_dl_scale_matrix(MENU_MTX_NOPUSH, 1.1f, (((f32) linesPerBox / 5.0f) + 0.1f), 1.0f);

    gSPDisplayList(gDisplayListHead++, dl_draw_text_bg_box);
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
}

void change_and_flash_dialog_text_color_lines(s8 colorMode, s8 lineNum, u8 *customColor) {
    u8 colorFade;

    if (colorMode == 1) {
        if (lineNum == 1) {
            gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, 255);
        } else {
            if (lineNum == gDialogLineNum) {
                colorFade = (gSineTable[gDialogColorFadeTimer >> 4] * 50.0f) + 200.0f;
                gDPSetEnvColor(gDisplayListHead++, colorFade, colorFade, colorFade, 255);
            } else {
                gDPSetEnvColor(gDisplayListHead++, 200, 200, 200, 255);
            }
        }
    } else {
        switch (gDialogBoxType) {
            case DIALOG_TYPE_ROTATE:
                if (*customColor == 2) {
                    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, 255);
                    *customColor = 0;
                }
                break;
            case DIALOG_TYPE_ZOOM:
                gDPSetEnvColor(gDisplayListHead++, 0, 0, 0, 255);
                break;
        }
    }
}

void handle_dialog_scroll_page_state(s8 lineNum, s8 totalLines, s8 *pageState, s8 *xMatrix, s16 *linePos) {
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);

    if (lineNum == totalLines) {
        *pageState = DIALOG_PAGE_STATE_SCROLL;
        return;
    }
    create_dl_translation_matrix(MENU_MTX_PUSH, 0.0f, 2 - (lineNum * 16), 0);

    *linePos = 0;
    *xMatrix = 1;
}

void render_star_count_dialog_text(s8 *xMatrix, s16 *linePos) {
    s8 tensDigit = gDialogVariable / 10;
    s8 onesDigit = gDialogVariable - (tensDigit * 10); // remainder
    u8 kerning;

    if (tensDigit != 0) {
        if (*xMatrix != 1) {
            create_dl_translation_matrix(MENU_MTX_NOPUSH, (f32)(5 * *xMatrix), 0, 0);
        }

        kerning = render_generic_ascii_char(tensDigit);
        create_dl_translation_matrix(MENU_MTX_NOPUSH, kerning, 0, 0);
        *xMatrix = 1;
        (*linePos)++;
    }

    if (*xMatrix != 1) {
        create_dl_translation_matrix(MENU_MTX_NOPUSH, (f32)5 * (*xMatrix - 1), 0, 0);
    }

    kerning = render_generic_ascii_char(onesDigit);
    create_dl_translation_matrix(MENU_MTX_NOPUSH, kerning, 0, 0);
    (*linePos)++;
    *xMatrix = 1;
}

u32 ensure_nonnegative(s16 value) {
    return ((value < 0) ? 0 : value);
}

void handle_dialog_text_and_pages(s8 colorMode, struct DialogEntry *dialog, s8 lowerBound) {
    u8 strChar;
    s16 colorLoop;
    ColorRGBA rgbaColors = { 0x00, 0x00, 0x00, 0x00 };
    u8 customColor = 0;
    u8 diffTmp = 0;
    char *str = segmented_to_virtual(dialog->str);
    s8 lineNum = 1;
    s8 totalLines;
    s8 pageState = DIALOG_PAGE_STATE_NONE;
    s8 xMatrix = 1;
    s8 linesPerBox = dialog->linesPerBox;
    s32 strIdx;
    s16 linePos = 0;
    u8 kerning;

    if (gDialogBoxState == DIALOG_STATE_HORIZONTAL) {
        // If scrolling, consider the number of lines for both
        // the current page and the page being scrolled to.
        totalLines = linesPerBox * 2 + 1;
    } else {
        totalLines = linesPerBox + 1;
    }

    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);
    strIdx = gDialogTextPos;

    if (gDialogBoxState == DIALOG_STATE_HORIZONTAL) {
        create_dl_translation_matrix(MENU_MTX_NOPUSH, 0, (f32) gDialogScrollOffsetY, 0);
    }

    create_dl_translation_matrix(MENU_MTX_PUSH, 0.f, 2 - lineNum * 16, 0);

    while (pageState == DIALOG_PAGE_STATE_NONE) {
        if (customColor == 1) {
            gDPSetEnvColor(gDisplayListHead++, rgbaColors[0], rgbaColors[1], rgbaColors[2], rgbaColors[3]);
        } else {
            change_and_flash_dialog_text_color_lines(colorMode, lineNum, &customColor);
        }
        strChar = str[strIdx];

        switch (strChar) {
            case '\0':
                pageState = DIALOG_PAGE_STATE_END;
                gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
                break;
            /**case DIALOG_CHAR_COLOR:
                customColor = 1;
                strIdx++;
                for (colorLoop = (strIdx + 8); strIdx < colorLoop; ++strIdx) {
                    diffTmp = 0;
                    if ((str[strIdx] >= 0x24) && (str[strIdx] <= 0x29)) {
                        diffTmp = 0x1A;
                    } else if (str[strIdx] >= 0x10) {
                        customColor = 2;
                        strIdx = (colorLoop - 8);
                        for (diffTmp = 0; diffTmp < 8; ++diffTmp) {
                            if (str[strIdx + diffTmp] != 0x9F) {
                                break;
                            }
                        }
                        if (diffTmp == 8) {
                            strIdx += diffTmp;
                        }
                        break;
                    }
                    if (((8 - (colorLoop - strIdx)) % 2) == 0) {
                        rgbaColors[(8 - (colorLoop - strIdx)) / 2] = (((str[strIdx] - diffTmp) & 0x0F) << 4);
                    } else {
                        rgbaColors[(8 - (colorLoop - strIdx)) / 2] += ((str[strIdx] - diffTmp) & 0x0F);
                    }
                }
                strIdx--;
                break;**/
            case '\n':
                lineNum++;
                handle_dialog_scroll_page_state(lineNum, totalLines, &pageState, &xMatrix, &linePos);
                break;
            case ' ':
                xMatrix++;
                linePos++;
                break;
            case '/':
                xMatrix += 2;
                linePos += 2;
                break;
            case 0x100: // come back to this
                render_star_count_dialog_text(&xMatrix, &linePos);
                break;
            default: // any other character
                if ((lineNum >= lowerBound) && (lineNum <= (lowerBound + linesPerBox))) {
                    if (linePos || xMatrix != 1) {
                        create_dl_translation_matrix(
                            MENU_MTX_NOPUSH, (f32)(5 * (xMatrix - 1)), 0, 0);
                    }

                    if (!(strChar & 0x80)) {
                        kerning = render_generic_ascii_char(strChar);
                    } else {
                        kerning = render_generic_unicode_char(str, &strIdx);
                    }
                    create_dl_translation_matrix(MENU_MTX_NOPUSH, kerning, 0, 0);
                    xMatrix = 1;
                    linePos++;
                }
        }


        strIdx++;
    }

    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);

    if (gDialogBoxState == DIALOG_STATE_VERTICAL) {
        if (pageState == DIALOG_PAGE_STATE_END) {
            gLastDialogPageStrPos = -1;
        } else {
            gLastDialogPageStrPos = strIdx;
        }
    }

    gLastDialogLineNum = lineNum;
}

void render_dialog_triangle_choice(void) {
    if (gDialogBoxState == DIALOG_STATE_VERTICAL) {
        handle_menu_scrolling(MENU_SCROLL_HORIZONTAL, &gDialogLineNum, 1, 2);
    }

    create_dl_translation_matrix(MENU_MTX_NOPUSH, (gDialogLineNum * 56) - 47, 2 - (gLastDialogLineNum * 16), 0);

    if (gDialogBoxType == DIALOG_TYPE_ROTATE) {
        gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, 255);
    } else {
        gDPSetEnvColor(gDisplayListHead++, 0, 0, 0, 255);
    }

    gSPDisplayList(gDisplayListHead++, dl_draw_triangle);
}

void render_dialog_triangle_next(s8 linesPerBox) {
    s32 globalTimer = gGlobalTimer;

    if (globalTimer & 0x8) {
        return;
    }

    create_dl_translation_matrix(MENU_MTX_PUSH, 118.f, (linesPerBox * -16) + 5, 0);
    create_dl_scale_matrix(MENU_MTX_NOPUSH, 0.8f, 0.8f, 1.0f);
    create_dl_rotation_matrix(MENU_MTX_NOPUSH, -DEFAULT_DIALOG_BOX_ANGLE, 0, 0, 1.0f);

    if (gDialogBoxType == DIALOG_TYPE_ROTATE) { // White Text
        gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, 255);
    } else { // Black Text
        gDPSetEnvColor(gDisplayListHead++, 0, 0, 0, 255);
    }

    gSPDisplayList(gDisplayListHead++, dl_draw_triangle);
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
}

void handle_special_dialog_text(s16 dialogID) { // dialog ID tables, in order
    // King Bob-omb (Start), Whomp (Start), King Bob-omb (throw him out), Eyerock (Start), Wiggler (Start)
    s16 dialogBossStart[] = { DIALOG_017, DIALOG_114, DIALOG_128, DIALOG_117, DIALOG_150 };
    // Koopa the Quick (BoB), Koopa the Quick (THI), Penguin Race, Fat Penguin Race (120 stars)
    s16 dialogRaceSound[] = { DIALOG_005, DIALOG_009, DIALOG_055, DIALOG_164             };
    // Red Switch, Green Switch, Blue Switch, 100 coins star, Bowser Red Coin Star
    s16 dialogStarSound[] = { DIALOG_010, DIALOG_011, DIALOG_012, DIALOG_013, DIALOG_014 };
    // King Bob-omb (Start), Whomp (Defeated), King Bob-omb (Defeated, missing in JP), Eyerock (Defeated), Wiggler (Defeated)
    s16 dialogBossStop[]  = { DIALOG_017, DIALOG_115, DIALOG_116, DIALOG_118, DIALOG_152 };
    s16 i;

    for (i = 0; i < (s16) ARRAY_COUNT(dialogBossStart); i++) {
        if (dialogBossStart[i] == dialogID) {
            seq_player_unlower_volume(SEQ_PLAYER_LEVEL, 60);
            play_music(SEQ_PLAYER_LEVEL, SEQUENCE_ARGS(4, SEQ_EVENT_BOSS), 0);
            return;
        }
    }

    for (i = 0; i < (s16) ARRAY_COUNT(dialogRaceSound); i++) {
        if (dialogRaceSound[i] == dialogID && gDialogLineNum == 1) {
            play_race_fanfare();
            return;
        }
    }

    for (i = 0; i < (s16) ARRAY_COUNT(dialogStarSound); i++) {
        if (dialogStarSound[i] == dialogID && gDialogLineNum == 1) {
            play_sound(SOUND_MENU_STAR_SOUND, gGlobalSoundSource);
            return;
        }
    }

    for (i = 0; i < (s16) ARRAY_COUNT(dialogBossStop); i++) {
        if (dialogBossStop[i] == dialogID) {
            seq_player_fade_out(SEQ_PLAYER_LEVEL, 1);
            return;
        }
    }
}

s16 gMenuMode = MENU_MODE_NONE;

char *gEndCutsceneStringsEn[] = {
    LANGUAGE_TEXT(
        "Mario!",
        "Mario!",
        "Mario!",
        "マリオ！！"),
    LANGUAGE_TEXT(
        "The power of the Stars is restored to the castle...",
        "Grâce aux étoiles, le château a retrouvé ses pouvoirs...",
        "Die Macht der Sterne ruht wieder sicher im Schloss...",
        "おしろにスターが　もどったのね"),
    LANGUAGE_TEXT(
        "...and it's all thanks to you!",
        "...et ceci grâce à toi!",
        "...und alles dank Deiner Hilfe!",
        "みんな　あなたのおかげだわ！"),
    LANGUAGE_TEXT(
        "Thank you, Mario!",
        "Merci, Mario!",
        "Vielen Dank, Mario!",
        "ありがとう　マリオ"),
    LANGUAGE_TEXT(
        "We have to do something special for you...",
        "Tu mérites une récompense...",
        "Wir haben eine Überraschung für Dich...",
        "なにか　おれいをしなくちゃ・・"),
    LANGUAGE_TEXT(
        "Listen, everybody,",
        "Venez les amis...",
        "Hört alle her...",
        "さあ　みんな"),
    LANGUAGE_TEXT(
        "let's bake a delicious cake...",
        "Allons préparer un délicieux gâteau...",
        "Laßt uns einen leckeren Kuchen backen...",
        "おいしいケーキを　やきましょう"),
    LANGUAGE_TEXT(
        "...for Mario...",
        "...pour Mario...",
        "...für Mario...",
        "マリオの　ために・・・"),
    LANGUAGE_TEXT(
        "Mario!",
        "Mario!",
        "Mario!",
        "マリオ！！"),
    NULL,
};

u16 gCutsceneMsgFade        =  0;
s16 gCutsceneMsgIndex       = -1;
s16 gCutsceneMsgDuration    = -1;
s16 gCutsceneMsgTimer       =  0;
s8  gDialogCameraAngleIndex = CAM_SELECTION_MARIO;
s8  gDialogCourseActNum     =  1;

void render_dialog_entries(void) {
    s8 lowerBound = 0;
    void **dialogTable = segmented_to_virtual(languageTable[gInGameLanguage][0]);
    struct DialogEntry *dialog = segmented_to_virtual(dialogTable[gDialogID]);

    // if the dialog entry is invalid, set the ID to DIALOG_NONE.
    if (segmented_to_virtual(NULL) == dialog) {
        gDialogID = DIALOG_NONE;
        return;
    }

    switch (gDialogBoxState) {
        case DIALOG_STATE_OPENING:
            if (gDialogBoxOpenTimer == DEFAULT_DIALOG_BOX_ANGLE) {
                play_dialog_sound(dialog->voice);
                play_sound(SOUND_MENU_MESSAGE_APPEAR, gGlobalSoundSource);
            }

            if (gDialogBoxType == DIALOG_TYPE_ROTATE) {
                gDialogBoxOpenTimer -= 7.5f;
                gDialogBoxScale -= 1.5f;
            } else {
                gDialogBoxOpenTimer -= 10.0f;
                gDialogBoxScale -= 2.0f;
            }

            if (gDialogBoxOpenTimer == 0.0f) {
                gDialogBoxState = DIALOG_STATE_VERTICAL;
                gDialogLineNum = 1;
            }
            lowerBound = 1;
            break;

        case DIALOG_STATE_VERTICAL:
            gDialogBoxOpenTimer = 0.0f;

            if (gPlayer3Controller->buttonPressed & (A_BUTTON | B_BUTTON | START_BUTTON | D_CBUTTONS | R_CBUTTONS | D_JPAD | R_JPAD)) {
                if (gLastDialogPageStrPos == -1) {
                    handle_special_dialog_text(gDialogID);
                    gDialogBoxState = DIALOG_STATE_CLOSING;
                } else {
                    gDialogBoxState = DIALOG_STATE_HORIZONTAL;
                    play_sound(SOUND_MENU_MESSAGE_NEXT_PAGE, gGlobalSoundSource);
                }
            }
            lowerBound = 1;
            break;
        case DIALOG_STATE_HORIZONTAL: // scrolling
            gDialogScrollOffsetY += (dialog->linesPerBox * 2);

            if (gDialogScrollOffsetY >= dialog->linesPerBox * 16) {
                gDialogTextPos = gLastDialogPageStrPos;
                gDialogBoxState = DIALOG_STATE_VERTICAL;
                gDialogScrollOffsetY = 0;
            }
            lowerBound = (gDialogScrollOffsetY / 16) + 1;
            break;

        case DIALOG_STATE_CLOSING:
            if (gDialogBoxOpenTimer == 20.0f) {
                level_set_transition(0, NULL);
                play_sound(SOUND_MENU_MESSAGE_DISAPPEAR, gGlobalSoundSource);

                gDialogResponse = gDialogLineNum;
            }

            gDialogBoxOpenTimer = gDialogBoxOpenTimer + 10.0f;
            gDialogBoxScale = gDialogBoxScale + 2.0f;

            if (gDialogBoxOpenTimer == DEFAULT_DIALOG_BOX_ANGLE) {
                gDialogBoxState = DIALOG_STATE_OPENING;
                gDialogID = DIALOG_NONE;
                gDialogTextPos = 0;
                gLastDialogResponse = 0;
                gLastDialogPageStrPos = 0;
                gDialogResponse = DIALOG_RESPONSE_NONE;
            }
            lowerBound = 1;
            break;
    }

    render_dialog_box_type(dialog, dialog->linesPerBox);

    gDPSetScissor(gDisplayListHead++, G_SC_NON_INTERLACE,
                  // Horizontal scissoring isn't really required and can potentially mess up widescreen enhancements.
                  0,
                  ensure_nonnegative(SCREEN_HEIGHT - dialog->width - 3),
                  SCREEN_WIDTH,
                  ensure_nonnegative(SCREEN_HEIGHT + (dialog->linesPerBox * 16) - dialog->width));
    handle_dialog_text_and_pages(0, dialog, lowerBound);

    if (gLastDialogPageStrPos == -1 && gLastDialogResponse == 1) {
        render_dialog_triangle_choice();
    }
    gDPSetScissor(gDisplayListHead++, G_SC_NON_INTERLACE, 2, 2, SCREEN_WIDTH - gBorderHeight / 2, SCREEN_HEIGHT - gBorderHeight / 2);
    if (gLastDialogPageStrPos != -1 && gDialogBoxState == DIALOG_STATE_VERTICAL) {
        render_dialog_triangle_next(dialog->linesPerBox);
    }
}

// Calls a gMenuMode value defined by render_menus_and_dialogs cases
void set_menu_mode(s16 mode) {
    if (gMenuMode == MENU_MODE_NONE) {
        gMenuMode = mode;
    }
}

void reset_cutscene_msg_fade(void) {
    gCutsceneMsgFade = 0;
}

void dl_rgba16_begin_cutscene_msg_fade(void) {
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gCutsceneMsgFade);
}

void dl_rgba16_stop_cutscene_msg_fade(void) {
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);

    if (gCutsceneMsgFade < 250) {
        gCutsceneMsgFade += 25;
    } else {
        gCutsceneMsgFade = 255;
    }
}

void set_cutscene_message(s16 msgIndex, s16 msgDuration) {
    // is message done printing?
    if (gCutsceneMsgIndex == -1) {
        gCutsceneMsgIndex = msgIndex;
        gCutsceneMsgDuration = msgDuration;
        gCutsceneMsgTimer = 0;
        gCutsceneMsgFade = 0;
    }
}

void do_cutscene_handler(void) {
    // is a cutscene playing? do not perform this handler's actions if so.
    if (gCutsceneMsgIndex == -1) {
        return;
    }
    
    create_dl_ortho_matrix();

    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gCutsceneMsgFade);

    print_generic_string_aligned(SCREEN_WIDTH/2, 13, gEndCutsceneStringsEn[gCutsceneMsgIndex], TEXT_ALIGN_CENTER);

    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);

    // if the timing variable is less than 5, increment
    // the fade until we are at full opacity.
    if (gCutsceneMsgTimer < 5) {
        gCutsceneMsgFade += 50;
    }

    // if the cutscene frame length + the fade-in counter is
    // less than the timer, it means we have exceeded the
    // time that the message is supposed to remain on
    // screen. if (message_duration = 50) and (msg_timer = 55)
    // then after the first 5 frames, the message will remain
    // on screen for another 50 frames until it starts fading.
    if (gCutsceneMsgDuration + 5 < gCutsceneMsgTimer) {
        gCutsceneMsgFade -= 50;
    }

    // like the first check, it takes 5 frames to fade out, so
    // perform a + 10 to account for the earlier check (10-5=5).
    if (gCutsceneMsgDuration + 10 < gCutsceneMsgTimer) {
        gCutsceneMsgIndex = -1;
        gCutsceneMsgFade = 0;
        gCutsceneMsgTimer = 0;
        return;
    }

    gCutsceneMsgTimer++;
}

#define PEACH_MESSAGE_TIMER 250

// "Dear Mario" message handler
void print_peach_letter_message(void) {
#ifdef VERSION_EU
    void **dialogTable;
    gInGameLanguage = eu_get_language();
    switch (gInGameLanguage) {
        default:
        case LANGUAGE_ENGLISH: dialogTable = segmented_to_virtual(dialog_table_eu_en); break;
        case LANGUAGE_FRENCH:  dialogTable = segmented_to_virtual(dialog_table_eu_fr); break;
        case LANGUAGE_GERMAN:  dialogTable = segmented_to_virtual(dialog_table_eu_de); break;
    }
#else
    void **dialogTable = segmented_to_virtual(seg2_dialog_table);
#endif
    struct DialogEntry *dialog = segmented_to_virtual(dialogTable[gDialogID]);
    char *str = segmented_to_virtual(dialog->str);

    create_dl_translation_matrix(MENU_MTX_PUSH, 97.0f, 118.0f, 0);

    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gCutsceneMsgFade);
    gSPDisplayList(gDisplayListHead++, castle_grounds_seg7_dl_0700EA58);
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 20, 20, 20, gCutsceneMsgFade);

    print_generic_string(38, 142, str);
#ifdef VERSION_JP
    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, 255);
#else
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, 255);
    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
    gDPSetEnvColor(gDisplayListHead++, 200, 80, 120, gCutsceneMsgFade);
    gSPDisplayList(gDisplayListHead++, castle_grounds_seg7_us_dl_0700F2E8);
#endif

    // at the start/end of message, reset the fade.
    if (gCutsceneMsgTimer == 0) {
        gCutsceneMsgFade = 0;
    }

    // we're less than 20 increments, so increase the fade.
    if (gCutsceneMsgTimer < 20) {
        gCutsceneMsgFade += 10;
    }

    // we're after PEACH_MESSAGE_TIMER increments, so decrease the fade.
    if (gCutsceneMsgTimer > PEACH_MESSAGE_TIMER) {
        gCutsceneMsgFade -= 10;
    }

    // 20 increments after the start of the decrease, we're
    // back where we are, so reset everything at the end.
    if (gCutsceneMsgTimer > (PEACH_MESSAGE_TIMER + 20)) {
        gCutsceneMsgIndex = -1;
        gCutsceneMsgFade = 0; //! uselessly reset since the next execution will just set it to 0 again.
        gDialogID = DIALOG_NONE;
        gCutsceneMsgTimer = 0;
        return; // return to avoid incrementing the timer
    }

    gCutsceneMsgTimer++;
}

/**
 * Renders the cannon reticle when Mario is inside a cannon.
 * Formed by four triangles.
 */
void render_hud_cannon_reticle(void) {
    create_dl_translation_matrix(MENU_MTX_PUSH, 160.0f, 120.0f, 0);

    gDPSetEnvColor(gDisplayListHead++, 50, 50, 50, 180);
    create_dl_translation_matrix(MENU_MTX_PUSH, -20.0f, -8.0f, 0);
    gSPDisplayList(gDisplayListHead++, dl_draw_triangle);
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);

    create_dl_translation_matrix(MENU_MTX_PUSH, 20.0f, 8.0f, 0);
    create_dl_rotation_matrix(MENU_MTX_NOPUSH, 180.0f, 0, 0, 1.0f);
    gSPDisplayList(gDisplayListHead++, dl_draw_triangle);
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);

    create_dl_translation_matrix(MENU_MTX_PUSH, 8.0f, -20.0f, 0);
    create_dl_rotation_matrix(MENU_MTX_NOPUSH, DEFAULT_DIALOG_BOX_ANGLE, 0, 0, 1.0f);
    gSPDisplayList(gDisplayListHead++, dl_draw_triangle);
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);

    create_dl_translation_matrix(MENU_MTX_PUSH, -8.0f, 20.0f, 0);
    create_dl_rotation_matrix(MENU_MTX_NOPUSH, -DEFAULT_DIALOG_BOX_ANGLE, 0, 0, 1.0f);
    gSPDisplayList(gDisplayListHead++, dl_draw_triangle);
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);

    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
}

void reset_red_coins_collected(void) {
    gRedCoinsCollected = 0;
}

void change_dialog_camera_angle(void) {
    if (cam_select_alt_mode(0) == CAM_SELECTION_MARIO) {
        gDialogCameraAngleIndex = CAM_SELECTION_MARIO;
    } else {
        gDialogCameraAngleIndex = CAM_SELECTION_FIXED;
    }
}

void shade_screen(void) {
    create_dl_translation_matrix(MENU_MTX_PUSH, GFX_DIMENSIONS_FROM_LEFT_EDGE(0), SCREEN_HEIGHT, 0);

    // This is a bit weird. It reuses the dialog text box (width 130, height -80),
    // so scale to at least fit the screen.
#ifdef WIDESCREEN
    create_dl_scale_matrix(MENU_MTX_NOPUSH,
                           GFX_DIMENSIONS_ASPECT_RATIO * SCREEN_HEIGHT / 130.0f, 3.0f, 1.0f);
#else
    create_dl_scale_matrix(MENU_MTX_NOPUSH, 2.6f, 3.4f, 1.0f);
#endif

    gDPSetEnvColor(gDisplayListHead++, 0, 0, 0, 110);
    gSPDisplayList(gDisplayListHead++, dl_draw_text_bg_box);
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
}

void print_animated_red_coin(s16 x, s16 y) {
    s32 globalTimer = gGlobalTimer;

    create_dl_translation_matrix(MENU_MTX_PUSH, x, y, 0);
    create_dl_scale_matrix(MENU_MTX_NOPUSH, 0.2f, 0.2f, 1.0f);
    gDPSetRenderMode(gDisplayListHead++, G_RM_TEX_EDGE, G_RM_TEX_EDGE2);

#ifdef IA8_30FPS_COINS
    switch (globalTimer & 0x7) {
        case 0: gSPDisplayList(gDisplayListHead++, coin_seg3_dl_red_0     ); break;
        case 1: gSPDisplayList(gDisplayListHead++, coin_seg3_dl_red_22_5  ); break;
        case 2: gSPDisplayList(gDisplayListHead++, coin_seg3_dl_red_45    ); break;
        case 3: gSPDisplayList(gDisplayListHead++, coin_seg3_dl_red_67_5  ); break;
        case 4: gSPDisplayList(gDisplayListHead++, coin_seg3_dl_red_90    ); break;
        case 5: gSPDisplayList(gDisplayListHead++, coin_seg3_dl_red_67_5_r); break;
        case 6: gSPDisplayList(gDisplayListHead++, coin_seg3_dl_red_45_r  ); break;
        case 7: gSPDisplayList(gDisplayListHead++, coin_seg3_dl_red_22_5_r); break;
    }
#else
    switch (globalTimer & 0x6) {
        case 0: gSPDisplayList(gDisplayListHead++, coin_seg3_dl_red_front     ); break;
        case 2: gSPDisplayList(gDisplayListHead++, coin_seg3_dl_red_tilt_right); break;
        case 4: gSPDisplayList(gDisplayListHead++, coin_seg3_dl_red_side      ); break;
        case 6: gSPDisplayList(gDisplayListHead++, coin_seg3_dl_red_tilt_left ); break;
    }
#endif

    gDPSetRenderMode(gDisplayListHead++, G_RM_AA_ZB_OPA_SURF, G_RM_AA_ZB_OPA_SURF2);
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
}

void render_pause_red_coins(void) {
    s8 x;

    for (x = 0; x < gRedCoinsCollected; x++) {
        print_animated_red_coin(GFX_DIMENSIONS_FROM_RIGHT_EDGE(30) - x * 20, 16);
    }
}

/// By default, not needed as puppycamera has an option, but should you wish to revert that, you are legally allowed.

#if defined(WIDE) && !defined(PUPPYCAM)
void render_widescreen_setting(void) {
    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    if (!gConfig.widescreen) {
        print_generic_string(10, 20, "ASPECT RATIO: 4:3\nPRESS L TO SWITCH");
    } else {
        print_generic_string(10, 20, "ASPECT RATIO: 16:9\nPRESS L TO SWITCH");
    }
    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
    if (gPlayer1Controller->buttonPressed & L_TRIG){
        gConfig.widescreen ^= 1;
        save_file_set_widescreen_mode(gConfig.widescreen);
    }
}
#endif

char *textCourseX = LANGUAGE_TEXT(
    "COURSE %d",
    "NIVEAU %d",
    "KURS %d",
    "コース%d");

char *textMyScore = LANGUAGE_TEXT(
    "MY SCORE",
    "MON SCORE",
    "LEISTUNG",
    "マイスコア");

#define PAUSE_MENU_LEFT_X  106
#define PAUSE_MENU_RIGHT_X 117

#define PAUSE_MENU_COURSE_Y 157
#define PAUSE_MENU_ACT_Y 140
#define PAUSE_MENU_MY_SCORE_Y 121

void render_pause_my_score_coins(void) {
    char str[20];

    void **courseNameTbl = segmented_to_virtual(languageTable[gInGameLanguage][1]);
    void    **actNameTbl = segmented_to_virtual(languageTable[gInGameLanguage][2]);

    u8 courseIndex = COURSE_NUM_TO_INDEX(gCurrCourseNum);
    u8 starFlags = save_file_get_star_flags(gCurrSaveFileNum - 1, COURSE_NUM_TO_INDEX(gCurrCourseNum));

    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);

    if (courseIndex <= COURSE_NUM_TO_INDEX(COURSE_STAGES_MAX)) {
        print_hud_my_score_coins(1, gCurrSaveFileNum - 1, courseIndex, PAUSE_MENU_RIGHT_X + 61, PAUSE_MENU_MY_SCORE_Y - 18);
        print_hud_my_score_stars(   gCurrSaveFileNum - 1, courseIndex, PAUSE_MENU_RIGHT_X,      PAUSE_MENU_MY_SCORE_Y - 18);
    }

    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);
    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);

    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);

    char *courseName = segmented_to_virtual(courseNameTbl[courseIndex]);

    if (courseIndex <= COURSE_NUM_TO_INDEX(COURSE_STAGES_MAX)) {
        sprintf(str, LANGUAGE_ARRAY(textCourseX), gCurrCourseNum);
        print_generic_string_aligned(PAUSE_MENU_LEFT_X, PAUSE_MENU_COURSE_Y, str, TEXT_ALIGN_RIGHT);

        char *actName = segmented_to_virtual(actNameTbl[COURSE_NUM_TO_INDEX(gCurrCourseNum) * 6 + gDialogCourseActNum - 1]);

        if (starFlags & (1 << (gDialogCourseActNum - 1))) {
            print_generic_string_aligned(PAUSE_MENU_LEFT_X, PAUSE_MENU_ACT_Y, "★", TEXT_ALIGN_RIGHT);
        } else {
            print_generic_string_aligned(PAUSE_MENU_LEFT_X, PAUSE_MENU_ACT_Y, "☆", TEXT_ALIGN_RIGHT);
        }

        print_generic_string(PAUSE_MENU_RIGHT_X, PAUSE_MENU_ACT_Y,    actName);
        print_generic_string(PAUSE_MENU_RIGHT_X, PAUSE_MENU_COURSE_Y, &courseName[3]);

        if (save_file_get_course_star_count(gCurrSaveFileNum - 1, courseIndex) != 0) {
            print_generic_string_aligned(PAUSE_MENU_LEFT_X + 3, PAUSE_MENU_MY_SCORE_Y, LANGUAGE_ARRAY(textMyScore), TEXT_ALIGN_RIGHT);
        }
    } else {
        print_generic_string_aligned(SCREEN_WIDTH/2, PAUSE_MENU_COURSE_Y, &courseName[3], TEXT_ALIGN_CENTER);
    }

    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
}

char *textLakituMario = LANGUAGE_TEXT(
    "LAKITU ↔ MARIO",
    "LAKITU ↔ MARIO",
    "LAKITU ↔ MARIO",
    "ジュゲム↔マリオ");

char *textLakituStop = LANGUAGE_TEXT(
    "LAKITU ↔ STOP",
    "LAKITU ↔ STOP",
    "LAKITU ↔ STOP",
    "ジュゲム↔ストップ");

char *textNormalUpClose = LANGUAGE_TEXT(
    "(NORMAL)(UP-CLOSE)",
    "(NORMAL)(GROS-PLAN)",
    "(NORMAL)(WEIT-ZOOM)",
    "（おすすめ）（リアル）");

char *textNormalFixed = LANGUAGE_TEXT(
    "(NORMAL)(FIXED)",
    "(NORMAL)(FIXE)",
    "(NORMAL)(STATIV)",
    "（おすすめ）（とまる）");

void render_pause_camera_options(s16 x, s16 y, s8 *index, s16 xIndex) {
    handle_menu_scrolling(MENU_SCROLL_HORIZONTAL, index, 1, 2);

    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);

    print_generic_string_aligned(x + 54,  y,      LANGUAGE_ARRAY(textLakituMario),   TEXT_ALIGN_CENTER);
    print_generic_string_aligned(x + 54,  y - 15, LANGUAGE_ARRAY(textNormalUpClose), TEXT_ALIGN_CENTER);
    print_generic_string_aligned(x + 160, y,      LANGUAGE_ARRAY(textLakituStop),    TEXT_ALIGN_CENTER);
    print_generic_string_aligned(x + 160, y - 15, LANGUAGE_ARRAY(textNormalFixed),   TEXT_ALIGN_CENTER);

    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
    create_dl_translation_matrix(MENU_MTX_PUSH, ((*index - 1) * xIndex) + x, y, 0);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    gSPDisplayList(gDisplayListHead++, dl_draw_triangle);
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);

    switch (*index) {
        case CAM_SELECTION_MARIO:
            cam_select_alt_mode(CAM_SELECTION_MARIO);
            break;
        case CAM_SELECTION_FIXED:
            cam_select_alt_mode(CAM_SELECTION_FIXED);
            break;
    }
}

char *textContinue = LANGUAGE_TEXT(
    "CONTINUE",
    "CONTINUER",
    "WEITER",
    "つづけて　マリオする？");

char *textExitCourse = LANGUAGE_TEXT(
    "EXIT COURSE",
    "QUITTER NIVEAU",
    "KURS VERLASSEN",
    "コースからでる？");

char *textCameraAngleR = LANGUAGE_TEXT(
    "SET CAMERA ANGLE WITH Ⓡ",
    "RÉGLAGE CAMÉRA AVEC Ⓡ",
    "KAMERA MIT Ⓡ VERSTELLEN",
    "Ｒボタンのカメラきりかえ");

void render_pause_course_options(s16 x, s16 y, s8 *index, s16 yIndex) {
    handle_menu_scrolling(MENU_SCROLL_VERTICAL, index, 1, 3);

    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);

    print_generic_string(x, y,      LANGUAGE_ARRAY(textContinue));
    print_generic_string(x, y - 15, LANGUAGE_ARRAY(textExitCourse));

    if (*index != MENU_OPT_CAMERA_ANGLE_R) {
        print_generic_string(x, y - 31, LANGUAGE_ARRAY(textCameraAngleR));
        gSPDisplayList(gDisplayListHead++, dl_ia_text_end);

        create_dl_translation_matrix(MENU_MTX_PUSH, x - 14, (y - ((*index - 1) * yIndex)), 0);

        gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
        gSPDisplayList(gDisplayListHead++, dl_draw_triangle);
        gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
    }

    if (*index == MENU_OPT_CAMERA_ANGLE_R) {
        render_pause_camera_options(x - 52, y - 38, &gDialogCameraAngleIndex, 110);
    }
}

void render_pause_castle_menu_box(s16 x, s16 y) {
    create_dl_translation_matrix(MENU_MTX_PUSH, x - 78, y - 32, 0);
    create_dl_scale_matrix(MENU_MTX_NOPUSH, 1.2f, 0.8f, 1.0f);
    gDPSetEnvColor(gDisplayListHead++, 0, 0, 0, 105);
    gSPDisplayList(gDisplayListHead++, dl_draw_text_bg_box);
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);

    create_dl_translation_matrix(MENU_MTX_PUSH, x + 6, y - 28, 0);
    create_dl_rotation_matrix(MENU_MTX_NOPUSH, DEFAULT_DIALOG_BOX_ANGLE, 0, 0, 1.0f);
    gDPPipeSync(gDisplayListHead++);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    gSPDisplayList(gDisplayListHead++, dl_draw_triangle);
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);

    create_dl_translation_matrix(MENU_MTX_PUSH, x - 9, y - 101, 0);
    create_dl_rotation_matrix(MENU_MTX_NOPUSH, 270.0f, 0, 0, 1.0f);
    gSPDisplayList(gDisplayListHead++, dl_draw_triangle);
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
}

void highlight_last_course_complete_stars(void) {
    u8 doneCourseIndex;

    if (gLastCompletedCourseNum == COURSE_NONE) {
        doneCourseIndex = COURSE_NUM_TO_INDEX(COURSE_MIN);
    } else {
        doneCourseIndex = COURSE_NUM_TO_INDEX(gLastCompletedCourseNum);

        if (doneCourseIndex >= COURSE_NUM_TO_INDEX(COURSE_BONUS_STAGES)) {
            doneCourseIndex = COURSE_NUM_TO_INDEX(COURSE_BONUS_STAGES);
        }
    }

    gDialogLineNum = doneCourseIndex;
}

void print_hud_pause_colorful_str(void) {
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);

    print_hud_lut_string_aligned(SCREEN_WIDTH/2, 81, "PAUSE", TEXT_ALIGN_CENTER);

    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);
}

void render_pause_castle_course_stars(s16 x, s16 y, s16 fileIndex, s16 courseIndex) {
    s16 hasStar = 0;

    char str[30];
    char *entries[6];

    u8 starFlags = save_file_get_star_flags(fileIndex, courseIndex);
    u16 starCount = save_file_get_course_star_count(fileIndex, courseIndex);

    u16 nextStar = 0;

    if (starFlags & STAR_FLAG_ACT_100_COINS) {
        starCount--;
        print_generic_string(x + 98, y, "★");
    }

    while (hasStar != starCount) {
        if (starFlags & (1 << nextStar)) {
            entries[nextStar] = "★";
            hasStar++;
        } else {
            entries[nextStar] = "☆";
        }
        nextStar++;
    }

    if (starCount == nextStar && starCount != 6) {
        entries[nextStar] = "☆";
        nextStar++;
    }
    while (nextStar < 6) {
        entries[nextStar] = "";
        nextStar++;
    }

    sprintf(str, "%s %s %s %s %s %s", entries[0], entries[1], entries[2], entries[3], entries[4], entries[5]);
    print_generic_string(x + 23, y + 18, str);
}

void render_pause_castle_main_strings(s16 x, s16 y) {
    void **courseNameTbl = segmented_to_virtual(languageTable[gInGameLanguage][1]);

    void *courseName;

    char strVal[8];
    s16 prevCourseIndex = gDialogLineNum;


    handle_menu_scrolling(
        MENU_SCROLL_VERTICAL, &gDialogLineNum,
        COURSE_NUM_TO_INDEX(COURSE_MIN) - 1, COURSE_NUM_TO_INDEX(COURSE_BONUS_STAGES) + 1
    );

    if (gDialogLineNum == COURSE_NUM_TO_INDEX(COURSE_BONUS_STAGES) + 1) {
        gDialogLineNum = COURSE_NUM_TO_INDEX(COURSE_MIN); // Exceeded max, set to min
    }

    if (gDialogLineNum == COURSE_NUM_TO_INDEX(COURSE_MIN) - 1) {
        gDialogLineNum = COURSE_NUM_TO_INDEX(COURSE_BONUS_STAGES); // Exceeded min, set to max
    }

    if (gDialogLineNum != COURSE_NUM_TO_INDEX(COURSE_BONUS_STAGES)) {
        while (save_file_get_course_star_count(gCurrSaveFileNum - 1, gDialogLineNum) == 0) {
            if (gDialogLineNum >= prevCourseIndex) {
                gDialogLineNum++;
            } else {
                gDialogLineNum--;
            }

            if (gDialogLineNum == COURSE_NUM_TO_INDEX(COURSE_STAGES_MAX) + 1
             || gDialogLineNum == COURSE_NUM_TO_INDEX(COURSE_MIN) - 1) {
                gDialogLineNum = COURSE_NUM_TO_INDEX(COURSE_BONUS_STAGES);
                break;
            }
        }
    }

    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);

    if (gDialogLineNum <= COURSE_NUM_TO_INDEX(COURSE_STAGES_MAX)) { // Main courses
        courseName = segmented_to_virtual(courseNameTbl[gDialogLineNum]);
        render_pause_castle_course_stars(x, y, gCurrSaveFileNum - 1, gDialogLineNum);
        sprintf(strVal, "✪× %d", save_file_get_course_coin_score(gCurrSaveFileNum - 1, gDialogLineNum));
        print_generic_string(x + 43, y, strVal);
    } else { // Castle secret stars
        courseName = segmented_to_virtual(courseNameTbl[COURSE_MAX]);
        sprintf(strVal, "★× %d", save_file_get_total_star_count(gCurrSaveFileNum - 1,
                                                             COURSE_NUM_TO_INDEX(COURSE_BONUS_STAGES),
                                                             COURSE_NUM_TO_INDEX(COURSE_MAX)));
        print_generic_string(x + 49, y + 18, strVal);
    }

    print_generic_string(x, y + 35, courseName);

    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
}

s8 gCourseCompleteCoinsEqual = FALSE;
s32 gCourseDoneMenuTimer = 0;
s32 gCourseCompleteCoins = 0;
s8 gHudFlash = HUD_FLASH_NONE;

s32 render_pause_courses_and_castle(void) {
    s16 index;

#ifdef PUPPYCAM
    puppycam_check_pause_buttons();
    if (!gPCOptionOpen) {
#endif
    switch (gDialogBoxState) {
        case DIALOG_STATE_OPENING:
            gDialogLineNum = MENU_OPT_DEFAULT;
            gDialogTextAlpha = 0;
            level_set_transition(-1, NULL);
            play_sound(SOUND_MENU_PAUSE_OPEN, gGlobalSoundSource);

            if (gCurrCourseNum >= COURSE_MIN
             && gCurrCourseNum <= COURSE_MAX) {
                change_dialog_camera_angle();
                gDialogBoxState = DIALOG_STATE_VERTICAL;
            } else {
                highlight_last_course_complete_stars();
                gDialogBoxState = DIALOG_STATE_HORIZONTAL;
            }
            break;

        case DIALOG_STATE_VERTICAL:
            shade_screen();
            render_pause_my_score_coins();
            render_pause_red_coins();
#ifndef DISABLE_EXIT_COURSE
#ifdef EXIT_COURSE_WHILE_MOVING
            if ((gMarioStates[0].action & (ACT_FLAG_SWIMMING | ACT_FLAG_METAL_WATER | ACT_FLAG_PAUSE_EXIT))
             || (gMarioStates[0].pos[1] <= gMarioStates[0].floorHeight)) {
#else
            if (gMarioStates[0].action & ACT_FLAG_PAUSE_EXIT) {
#endif
                render_pause_course_options(109, 91, &gDialogLineNum, 15);
            }
#endif

            if (gPlayer3Controller->buttonPressed & (A_BUTTON | START_BUTTON)) {
                level_set_transition(0, NULL);
                play_sound(SOUND_MENU_PAUSE_CLOSE, gGlobalSoundSource);
                gDialogBoxState = DIALOG_STATE_OPENING;
                gMenuMode = MENU_MODE_NONE;

                if (gDialogLineNum == MENU_OPT_EXIT_COURSE) {
                    index = gDialogLineNum;
                } else { // MENU_OPT_CONTINUE or MENU_OPT_CAMERA_ANGLE_R
                    index = MENU_OPT_DEFAULT;
                }

                return index;
            }
            break;

        case DIALOG_STATE_HORIZONTAL:
            shade_screen();
            print_hud_pause_colorful_str();
            render_pause_castle_menu_box(160, 143);
            render_pause_castle_main_strings(95, 55);

            if (gPlayer3Controller->buttonPressed & (A_BUTTON | START_BUTTON | Z_TRIG)) {
                level_set_transition(0, NULL);
                play_sound(SOUND_MENU_PAUSE_CLOSE, gGlobalSoundSource);
                gMenuMode = MENU_MODE_NONE;
                gDialogBoxState = DIALOG_STATE_OPENING;

                return MENU_OPT_DEFAULT;
            }
            break;
    }
#if defined(WIDE) && !defined(PUPPYCAM)
        render_widescreen_setting();
#endif
    if (gDialogTextAlpha < 250) {
        gDialogTextAlpha += 25;
    }
#ifdef PUPPYCAM
    } else {
        shade_screen();
        puppycam_display_options();
    }

    puppycam_render_option_text();
#endif
    return MENU_OPT_NONE;
}

enum HUDCourseCompleteStringIDs {
    HUD_PRINT_HISCORE,
    HUD_PRINT_CONGRATULATIONS
};

char *textHudHiScore = LANGUAGE_TEXT(
    "HI SCORE",
    "MEILLEUR SCORE",
    "BESTLEISTUNG",
    "HISCORE");

char *textCongratulations = LANGUAGE_TEXT(
    "CONGRATULATIONS",
    "FELICITATIONS",
    "GRATULATION",
    "CONGRATULATIONS");

void print_hud_course_complete_string(s8 str) {
    u8 colorFade = sins(gDialogColorFadeTimer) * 50.0f + 200.0f;

    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);
    gDPSetEnvColor(gDisplayListHead++, colorFade, colorFade, colorFade, 255);

    if (str == HUD_PRINT_HISCORE) {
        print_hud_lut_string_aligned(SCREEN_WIDTH/2, 36, LANGUAGE_ARRAY(textHudHiScore),      TEXT_ALIGN_CENTER);
    } else { // HUD_PRINT_CONGRATULATIONS
        print_hud_lut_string_aligned(SCREEN_WIDTH/2, 67, LANGUAGE_ARRAY(textCongratulations), TEXT_ALIGN_CENTER);
    }

    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);
}

void print_hud_course_complete_coins(s16 x, s16 y) {
    char courseCompleteCoinsStr[10];

    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, 255);

    sprintf(courseCompleteCoinsStr, "✪×%d", gCourseCompleteCoins);
    print_hud_lut_string(x, y, courseCompleteCoinsStr);

    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);

    if (gCourseCompleteCoins >= gHudDisplay.coins) {
        gCourseCompleteCoinsEqual = TRUE;
        gCourseCompleteCoins = gHudDisplay.coins;

        if (gGotFileCoinHiScore) {
            print_hud_course_complete_string(HUD_PRINT_HISCORE);
        }
    } else {
        if ((gCourseDoneMenuTimer & 1) || gHudDisplay.coins > 70) {
            gCourseCompleteCoins++;
            play_sound(SOUND_MENU_YOSHI_GAIN_LIVES, gGlobalSoundSource);

#ifndef DISABLE_LIVES
            if (gCourseCompleteCoins && ((gCourseCompleteCoins % 50) == 0)) {
                play_sound(SOUND_GENERAL_COLLECT_1UP, gGlobalSoundSource);
                gMarioState->numLives++;
            }
#endif
        }

        if ((gHudDisplay.coins == gCourseCompleteCoins) && gGotFileCoinHiScore) {
            play_sound(SOUND_MENU_HIGH_SCORE, gGlobalSoundSource);
        }
    }
}

void play_star_fanfare_and_flash_hud(s32 arg, u8 starNum) {
    if (gHudDisplay.coins == gCourseCompleteCoins && (gCurrCourseStarFlags & starNum) == 0 && gHudFlash == HUD_FLASH_NONE) {
        play_star_fanfare();
        gHudFlash = arg;
    }
}

char *textClear = LANGUAGE_TEXT(
    "CLEAR",
    "CLEAR",
    "CLEAR",
    "クリア！");

#define COURSE_COMPLETE_COURSE_X 63
#define COURSE_COMPLETE_COURSE_Y 167
#define COURSE_COMPLETE_ACT_X    74
#define COURSE_COMPLETE_ACT_Y    147

#define CONGRATULATIONS_COURSE_X 69
#define CONGRATULATIONS_COURSE_Y 132

#define COURSE_COMPLETE_COINS_X 118
#define COURSE_COMPLETE_COINS_Y 103
#define CONGRATULATIONS_COINS_Y 111

void render_course_complete_lvl_info_and_hud_str(void) {
    char *name;

    char str[20];

    void **actNameTbl    = segmented_to_virtual(languageTable[gInGameLanguage][2]);
    void **courseNameTbl = segmented_to_virtual(languageTable[gInGameLanguage][1]);

    if (gLastCompletedCourseNum <= COURSE_STAGES_MAX) { // Main courses
        print_hud_course_complete_coins(COURSE_COMPLETE_COINS_X, COURSE_COMPLETE_COINS_Y);
        play_star_fanfare_and_flash_hud(HUD_FLASH_STARS, (1 << (gLastCompletedStarNum - 1)));

        if (gLastCompletedStarNum == 7) {
            name = segmented_to_virtual(actNameTbl[COURSE_STAGES_MAX * 6 + 1]);
        } else {
            name = segmented_to_virtual(actNameTbl[COURSE_NUM_TO_INDEX(gLastCompletedCourseNum) * 6 + gLastCompletedStarNum - 1]);
        }

        // Print course number
        gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);

        sprintf(str, LANGUAGE_ARRAY(textCourseX), gLastCompletedCourseNum);
        gDPSetEnvColor(gDisplayListHead++, 0, 0, 0, gDialogTextAlpha);
        print_generic_string(COURSE_COMPLETE_COURSE_X + 2,  COURSE_COMPLETE_COURSE_Y - 2, str);

        gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
        print_generic_string(COURSE_COMPLETE_COURSE_X,      COURSE_COMPLETE_COURSE_Y, str);

        gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
    } else if (gLastCompletedCourseNum == COURSE_BITDW || gLastCompletedCourseNum == COURSE_BITFS) { // Bowser courses
        u32 clearX;
        name = segmented_to_virtual(courseNameTbl[COURSE_NUM_TO_INDEX(gLastCompletedCourseNum)]);
        clearX = get_string_width(name) + 79;

        // Print course name and clear text
        gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);

        gDPSetEnvColor(gDisplayListHead++, 0, 0, 0, gDialogTextAlpha);
        print_generic_string(CONGRATULATIONS_COURSE_X + 2, CONGRATULATIONS_COURSE_Y - 2, name);
        print_generic_string(clearX                   + 2, CONGRATULATIONS_COURSE_Y - 2, textClear);
        gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
        print_generic_string(CONGRATULATIONS_COURSE_X,     CONGRATULATIONS_COURSE_Y,     name);
        print_generic_string(clearX,                       CONGRATULATIONS_COURSE_Y,     textClear);
        gSPDisplayList(gDisplayListHead++, dl_ia_text_end);

        print_hud_course_complete_string(HUD_PRINT_CONGRATULATIONS);
        print_hud_course_complete_coins(COURSE_COMPLETE_COINS_X, CONGRATULATIONS_COINS_Y);
        play_star_fanfare_and_flash_hud(HUD_FLASH_KEYS, 0);
        return;
    } else { // Castle secret stars
        name = segmented_to_virtual(actNameTbl[COURSE_STAGES_MAX * 6]);

        print_hud_course_complete_coins(COURSE_COMPLETE_COINS_X, COURSE_COMPLETE_COINS_Y);
        play_star_fanfare_and_flash_hud(HUD_FLASH_STARS, 1 << (gLastCompletedStarNum - 1));
    }

    // Print star glyph
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);

    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    print_hud_lut_string(COURSE_COMPLETE_ACT_X - 19, SCREEN_HEIGHT - COURSE_COMPLETE_ACT_Y - 16, "★");

    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);

    // Print act name and catch text
    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);

    gDPSetEnvColor(gDisplayListHead++, 0, 0, 0, gDialogTextAlpha);
    print_generic_string(COURSE_COMPLETE_ACT_X + 2, COURSE_COMPLETE_ACT_Y - 2, name);

    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    print_generic_string(COURSE_COMPLETE_ACT_X,     COURSE_COMPLETE_ACT_Y,     name);

    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
}

char *textSaveAndContinue = LANGUAGE_TEXT(
    "SAVE & CONTINUE",
    "SAUVEGARDER & CONTINUER",
    "SPEICHERN & WEITER",
    "セーブしてつづける？");

char *textSaveAndQuit = LANGUAGE_TEXT(
    "SAVE & QUIT",
    "SAUVEGARDER & QUITTER",
    "SPEICHERN & ENDE",
    "セーブしておわる？");

char *textContinueWithoutSave = LANGUAGE_TEXT(
    "CONTINUE, DON'T SAVE",
    "CONTINUER SANS SAUVEGARDER",
    "WEITER OHNE ZU SPEICHERN",
    "セーブしないでつづける？");

void render_save_confirmation(s16 x, s16 y, s8 *index, s16 yPos) {
    handle_menu_scrolling(MENU_SCROLL_VERTICAL, index, 1, 3);

    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);

    print_generic_string(x + 12, y,      LANGUAGE_ARRAY(textSaveAndContinue));
    print_generic_string(x + 12, y - 20, LANGUAGE_ARRAY(textSaveAndQuit));
    print_generic_string(x + 12, y - 40, LANGUAGE_ARRAY(textContinueWithoutSave));

    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);

    create_dl_translation_matrix(MENU_MTX_PUSH, x, y - ((*index - 1) * yPos), 0);

    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    gSPDisplayList(gDisplayListHead++, dl_draw_triangle);

    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
}

s32 render_course_complete_screen(void) {
    switch (gDialogBoxState) {
        case DIALOG_STATE_OPENING:
            render_course_complete_lvl_info_and_hud_str();
            if (gCourseDoneMenuTimer > 100 && gCourseCompleteCoinsEqual) {
#ifdef SAVE_NUM_LIVES
                save_file_set_num_lives(gMarioState->numLives);
#endif
                gDialogBoxState = DIALOG_STATE_VERTICAL;
                level_set_transition(-1, NULL);
                gDialogTextAlpha = 0;
                gDialogLineNum = MENU_OPT_DEFAULT;
            }
            break;

        case DIALOG_STATE_VERTICAL:
            shade_screen();
            render_course_complete_lvl_info_and_hud_str();
            render_save_confirmation(100, 86, &gDialogLineNum, 20);

            if (gCourseDoneMenuTimer > 110 && (gPlayer3Controller->buttonPressed & (A_BUTTON | START_BUTTON))) {
                level_set_transition(0, NULL);
                play_sound(SOUND_MENU_STAR_SOUND, gGlobalSoundSource);
                gDialogBoxState = DIALOG_STATE_OPENING;
                gMenuMode = MENU_MODE_NONE;
                gCourseDoneMenuTimer = 0;
                gCourseCompleteCoins = 0;
                gCourseCompleteCoinsEqual = FALSE;
                gHudFlash = HUD_FLASH_NONE;

                return gDialogLineNum;
            }
            break;
    }

    if (gDialogTextAlpha < 250) {
        gDialogTextAlpha += 25;
    }

    gCourseDoneMenuTimer++;

    return MENU_OPT_NONE;
}

s32 render_menus_and_dialogs(void) {
    s32 mode = MENU_OPT_NONE;

    create_dl_ortho_matrix();

    if (gMenuMode != MENU_MODE_NONE) {
        switch (gMenuMode) {
            case MENU_MODE_UNUSED_0:
                mode = render_pause_courses_and_castle();
                break;
            case MENU_MODE_RENDER_PAUSE_SCREEN:
                mode = render_pause_courses_and_castle();
                break;
            case MENU_MODE_RENDER_COURSE_COMPLETE_SCREEN:
                mode = render_course_complete_screen();
                break;
            case MENU_MODE_UNUSED_3:
                mode = render_course_complete_screen();
                break;
        }

        gDialogColorFadeTimer = (s16) gDialogColorFadeTimer + 0x1000;
    } else if (gDialogID != DIALOG_NONE) {
        // The Peach "Dear Mario" message needs to be repositioned separately
        if (gDialogID == DIALOG_020) {
            print_peach_letter_message();
            return mode;
        }

        render_dialog_entries();
        gDialogColorFadeTimer = (s16) gDialogColorFadeTimer + 0x1000;
    }

    return mode;
}
