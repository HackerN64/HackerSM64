#include <ultra64.h>

#include "actors/common1.h"
#include "area.h"
#include "audio/external.h"
#include "camera.h"
#include "course_table.h"
#include "dialog_ids.h"
#include "engine/math_util.h"
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

u16 gDialogColorFadeTimer;
s8 gLastDialogLineNum;
DialogVariable gDialogVariable;
u16 gDialogTextAlpha;
s8 gRedCoinsCollected;

// The language to display the game's text in.
u8 gInGameLanguage = LANGUAGE_ENGLISH;

#ifdef MULTILANG
// Determines which languages are available for the language selector.
const u8 gDefinedLanguages[] = DEFINE_LANGUAGE_ARRAY(
    LANGUAGE_ENGLISH,
    LANGUAGE_FRENCH,
    LANGUAGE_GERMAN,
    LANGUAGE_JAPANESE,
    LANGUAGE_SPANISH
);

extern u8 dialog_table_en[];
extern u8 course_name_table_en[];
extern u8 act_name_table_en[];

#ifdef ENABLE_FRENCH
extern u8 dialog_table_fr[];
extern u8 course_name_table_fr[];
extern u8 act_name_table_fr[];
#endif

#ifdef ENABLE_GERMAN
extern u8 dialog_table_de[];
extern u8 course_name_table_de[];
extern u8 act_name_table_de[];
#endif

#ifdef ENABLE_JAPANESE
extern u8 dialog_table_jp[];
extern u8 course_name_table_jp[];
extern u8 act_name_table_jp[];
#endif

#ifdef ENABLE_SPANISH
extern u8 dialog_table_es[];
extern u8 course_name_table_es[];
extern u8 act_name_table_es[];
#endif
#else // !MULTILANG
#define dialog_table_en      seg2_dialog_table
#define course_name_table_en seg2_course_name_table
#define act_name_table_en    seg2_act_name_table
#endif // !MULTILANG

#define LANGUAGE_TABLES(dialog, course_name, act_name) { (u8 *)(dialog), (u8 *)(course_name), (u8 *)(act_name) }

// The language table for the game's dialogs, level names and act names.
// #ifdef MULTILANG
const struct LanguageTables gLanguageTables[] = DEFINE_LANGUAGE_ARRAY(
    LANGUAGE_TABLES(dialog_table_en, course_name_table_en, act_name_table_en),
    LANGUAGE_TABLES(dialog_table_fr, course_name_table_fr, act_name_table_fr),
    LANGUAGE_TABLES(dialog_table_de, course_name_table_de, act_name_table_de),
    LANGUAGE_TABLES(dialog_table_jp, course_name_table_jp, act_name_table_jp),
    LANGUAGE_TABLES(dialog_table_es, course_name_table_es, act_name_table_es)
);

extern u8 gLastCompletedCourseNum;
extern u8 gLastCompletedStarNum;

enum DialogBoxState {
    DIALOG_STATE_OPENING,
    DIALOG_STATE_VERTICAL,
    DIALOG_STATE_HORIZONTAL,
    DIALOG_STATE_CLOSING
};

enum DialogBoxType {
    DIALOG_TYPE_ROTATE, // used in NPCs and level messages
    DIALOG_TYPE_ZOOM    // used in signposts and wall signs and etc
};

#define DEFAULT_DIALOG_BOX_ANGLE 90.0f
#define DEFAULT_DIALOG_BOX_SCALE 19.0f

s8 gDialogBoxState = DIALOG_STATE_OPENING;
s8 gDialogBoxType = DIALOG_TYPE_ROTATE;
s8 gDialogLineNum = 1;
s8 gDialogHasResponse = FALSE;
u8 gMenuHoldKeyIndex = 0;
u8 gMenuHoldKeyTimer = 0;
s16 gDialogScrollOffsetY = 0;
s16 gDialogID = DIALOG_NONE;
s16 gLastDialogPageStrPos = 0;
s16 gDialogTextPos = 0;
f32 gDialogBoxOpenTimer = DEFAULT_DIALOG_BOX_ANGLE;
f32 gDialogBoxScale = DEFAULT_DIALOG_BOX_SCALE;
s32 gDialogResponse = DIALOG_RESPONSE_NONE;
ColorRGBA gDialogColorByEnd;
ColorRGBA gDialogCarryoverColor;

static ColorRGBA sActiveTextColor;

static u8 sGenericFontLineHeight = 0;
static u8 sGenericFontLineAlignment = TEXT_ALIGN_LEFT;

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

/**
 * Determine which UTF-8 character to render, given a string and the current position in the string.
 * Returns the table entry of the relevant character.
 * Also increments the string position by the correct amount to reach the next character.
 */
struct Utf8CharLUTEntry *utf8_lookup(struct Utf8LUT *lut, char *str, s32 *strPos) {
    u32 codepoint;
    struct Utf8CharLUTEntry *usedLUT;
    u32 length;

    lut = segmented_to_virtual(lut);
    if (!(str[*strPos] & 0x20)) {
        codepoint = ((str[*strPos] & 0x1F) << 6) | (str[*strPos + 1] & 0x3F);
        *strPos += 1;

        usedLUT = segmented_to_virtual(lut->lut2Bytes);
        length = lut->length2Bytes;
    } else if (!(str[*strPos] & 0x10)) {
        codepoint = ((str[*strPos] & 0xF) << 12) | ((str[*strPos + 1] & 0x3F) << 6) | (str[*strPos + 2] & 0x3F);
        *strPos += 2;

        usedLUT = segmented_to_virtual(lut->lut3Bytes);
        length = lut->length3Bytes;
    } else {
        codepoint = ((str[*strPos] & 0x7) << 18) | ((str[*strPos + 1] & 0x3F) << 12) | ((str[*strPos + 2] & 0x3F) << 6) | (str[*strPos + 3] & 0x3F);
        *strPos += 3;

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

/**
 * Convert a character in the range 0-9 or A-F to an integer value. Returns -1 if the character is
 * not a valid hex digit.
 */
static s32 hex_char_to_value(char c) {
    if (c >= '0' && c <= '9') {
        return c - '0';
    }
    if (c >= 'A' && c <= 'F') {
        return c - 'A' + 10;
    }
    if (c >= 'a' && c <= 'f') {
        return c - 'a' + 10;
    }
    return -1;
}

/**
 * Convert two hexadecimal text characters into an integer. Returns -1 if either of the two characters
 * aren't hexadecimal values.
 */
static s32 hex_pair_to_value(char *str, s32 strPos) {
    s32 firstDigit = hex_char_to_value(str[strPos]);
    s32 secondDigit = hex_char_to_value(str[strPos + 1]);

    if (firstDigit == -1 || secondDigit == -1) {
        return -1;
    }

    return (firstDigit << 4) | secondDigit;
}

/**
 * Determine if the characters following a color command are a valid hex color code.
 */
static s32 is_color_code_valid(char *str, s32 strPos) {
    for (u32 i = 0; i < sizeof(gDialogCarryoverColor) * 2; i++) {
        if (hex_char_to_value(str[strPos + i]) == -1) {
            if (i < sizeof(ColorRGB) * 2) return FALSE;
            // allow alpha ignore
            if (str[strPos + i] != CHAR_VALUE_IGNORE[0]) return FALSE;
        }
    }
    return TRUE;
}

/**
 * Set text color for string to print, needed for color reset commands to function properly.
 * This should always be called instead of gDPSetEnvColor prior to any print_generic_string call or variant.
 */
void set_text_color(u32 r, u32 g, u32 b) {
    gDPSetEnvColor(gDisplayListHead++, r, g, b, gDialogTextAlpha);

    sActiveTextColor[0] = r;
    sActiveTextColor[1] = g;
    sActiveTextColor[2] = b;
    sActiveTextColor[3] = gDialogTextAlpha;
}

/**
 * Get the exact width of the line of a string of any font in pixels, using the given ASCII and UTF-8 tables.
 */
s32 get_string_width(char *str, struct AsciiCharLUTEntry *asciiLut, struct Utf8LUT *utf8LUT) {
    char c;
    s32 width = 0;
    s32 maxWidth = 0;
    s32 strPos = -1;

    s32 isGenericFont = (asciiLut == main_font_lut);
    asciiLut = segmented_to_virtual(asciiLut);

    while ((c = str[++strPos]) != '\0' && c != '\n') {
        // Handle color codes and tabs if using generic font
        if (isGenericFont) {
            switch (c) {
                case '\t':
                    width += 4 * SPACE_KERNING(asciiLut);
                    continue;
                case HEX(CONTROL_CHAR_COL):
                    if (is_color_code_valid(str, strPos + 1)) {
                        strPos += sizeof(gDialogCarryoverColor) * 2;
                    }
                    continue;
                case HEX(CONTROL_CHAR_RESET):
                    continue;
            }
        }

        if (c & 0x80) {
            width += utf8_lookup(utf8LUT, str, &strPos)->kerning;
        } else {
            width += asciiLut[ASCII_LUT_INDEX(c)].kerning;
        }
    }

    return MAX(width, maxWidth);
}

/**
 * Get the value to shift the X position of a string by, given a specific alignment.
 */
static s32 get_alignment_x_offset(char *str, u32 alignment, struct AsciiCharLUTEntry *asciiLut, struct Utf8LUT *utf8LUT) {
    if (alignment == TEXT_ALIGN_LEFT) {
        return 0;
    }
    s32 width = get_string_width(str, asciiLut, utf8LUT);
    if (alignment == TEXT_ALIGN_CENTER) {
        return -width / 2;
    }
    // TEXT_ALIGN_RIGHT
    return -width;
}

/**
 * Takes a value and writes the string representation of the number into a buffer.
 * If the language is set to Japanese, the number is written in full-width digits.
 */
void format_int_to_string(char *buf, s32 value) {
#ifdef ENABLE_JAPANESE
    if (gInGameLanguage == LANGUAGE_JAPANESE) {
        u8 digits[10];
        s32 numDigits = 0;
        // Minus sign
        if (value < 0) {
            buf[0] = '-';
            buf++;
            value = -value;
        }
        // Copy each digit of the number into an array, in reverse order.
        do {
            digits[numDigits++] = value % 10;
            value /= 10;
        } while (value != 0);
        for (s32 i = 0; i < numDigits; i++) {
            // The UTF-8 encoding of "０" is 0xEF, 0xBC, 0x90
            buf[i * 3]     = 0xEF;
            buf[i * 3 + 1] = 0xBC;
            buf[i * 3 + 2] = 0x90 + digits[numDigits - i - 1];
        }
        buf[numDigits * 3] = '\0';
        return;
    }
#endif

    sprintf(buf, "%d", value);
}

/**
 * Unpacks a packed I1 character texture into a usable IA8 texture when TEXT_FLAG_PACKED is used.
 * By default this is used for all the Japanese characters.
 */
static u8 *alloc_ia8_text_from_i1(u16 *in, s16 width, s16 height) {
    s32 inPos;
    u16 bitMask;
    u8 *out;
    s16 outPos = 0;

    out = alloc_display_list((u32) width * (u32) height);

    if (out == NULL) {
        return NULL;
    }

    for (inPos = 0; inPos < (width * height) / 16; inPos++) {
        bitMask = 0x8000;

        while (bitMask != 0) {
            if (in[inPos] & bitMask) {
                out[outPos] = 0xFF;
            } else {
                out[outPos] = 0x00;
            }

            bitMask /= 2;
            outPos++;
        }
    }

    return out;
}

/**
 * Renders a single ASCII character in the generic font.
 */
static u32 render_generic_ascii_char(char c) {
    struct AsciiCharLUTEntry *fontLUT = segmented_to_virtual(main_font_lut);
    const Texture *texture = fontLUT[ASCII_LUT_INDEX(c)].texture;

    if (texture != NULL) {
        gDPPipeSync(gDisplayListHead++);
        gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_IA, G_IM_SIZ_16b, 1, texture);
        gSPDisplayList(gDisplayListHead++, dl_ia_text_tex_settings);
    }

    return fontLUT[ASCII_LUT_INDEX(c)].kerning;
}

/**
 * Renders a single UTF-8 character in the generic font.
 */
static u32 render_generic_unicode_char(char *str, s32 *strPos) {
    struct Utf8CharLUTEntry *utf8Entry = utf8_lookup(&main_font_utf8_lut, str, strPos);

    if (utf8Entry->texture == NULL) {
        return utf8Entry->kerning;
    }

    gDPPipeSync(gDisplayListHead++);

    if (utf8Entry->flags & TEXT_FLAG_PACKED) {
        void *unpackedTexture = alloc_ia8_text_from_i1(segmented_to_virtual(utf8Entry->texture), 8, 16);
        gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_IA, G_IM_SIZ_8b, 1, unpackedTexture);
        gSPDisplayList(gDisplayListHead++, dl_ia_text_tex_settings_packed);
    } else {
        gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_IA, G_IM_SIZ_16b, 1, utf8Entry->texture);
        gSPDisplayList(gDisplayListHead++, dl_ia_text_tex_settings);
    }

    if (utf8Entry->flags & TEXT_DIACRITIC_MASK) {
        struct DiacriticLUTEntry *diacriticLUT = segmented_to_virtual(&main_font_diacritic_lut);
        struct DiacriticLUTEntry *diacritic = &diacriticLUT[utf8Entry->flags & TEXT_DIACRITIC_MASK];
        
        if (diacritic->xOffset | diacritic->yOffset) {
            create_dl_translation_matrix(MENU_MTX_PUSH, diacritic->xOffset, diacritic->yOffset, 0.0f);
        }

        s32 fakeStrPos = 0;
        render_generic_unicode_char(segmented_to_virtual(diacritic->str), &fakeStrPos);

        if (diacritic->xOffset | diacritic->yOffset) {
            gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
        }
    }

    return utf8Entry->kerning;
}

// Constants that control how the dialog box renders, the box is taller in Japanese.
#define DIALOG_LINE_HEIGHT_EN 16
#define DIALOG_LINE_HEIGHT_JP 20
#define BOX_TRANS_X_EN -7.f
#define BOX_TRANS_X_JP -5.f
#define BOX_TRANS_Y_EN  5.f
#define BOX_TRANS_Y_JP  2.f
#define BOX_SCALE_EN    5.f
#define BOX_SCALE_JP    4.f

#ifdef ENABLE_JAPANESE
#define DIALOG_LINE_HEIGHT ((gInGameLanguage == LANGUAGE_JAPANESE) ? DIALOG_LINE_HEIGHT_JP : DIALOG_LINE_HEIGHT_EN)
#define BOX_TRANS_X ((gInGameLanguage == LANGUAGE_JAPANESE) ? BOX_TRANS_X_JP : BOX_TRANS_X_EN)
#define BOX_TRANS_Y ((gInGameLanguage == LANGUAGE_JAPANESE) ? BOX_TRANS_Y_JP : BOX_TRANS_Y_EN)
#define BOX_SCALE   ((gInGameLanguage == LANGUAGE_JAPANESE) ? BOX_SCALE_JP   : BOX_SCALE_EN)
#else
#define DIALOG_LINE_HEIGHT DIALOG_LINE_HEIGHT_EN
#define BOX_TRANS_X BOX_TRANS_X_EN
#define BOX_TRANS_Y BOX_TRANS_Y_EN
#define BOX_SCALE   BOX_SCALE_EN
#endif

/**
 * Prints a generic white string. Used for both dialog entries and regular prints.
 * Only prints a total of maxLines lines of text. If maxLines is -1, it will print
 * until the end of the string.
 * 
 * Uses the global variables sGenericFontLineHeight and sGenericFontLineAlignment 
 * to control printing.
 */
static s32 render_main_font_text(s16 x, s16 y, char *str, s32 maxLines) {
    char c;
    s8 kerning = 0;
    u8 queuedSpaces = 0; // Optimization to only have one translation matrix if there are multiple spaces in a row.
    u8 validColor;
    s32 strPos = 0;
    s32 lineNum = 1;
    s32 alignmentXOffset = get_alignment_x_offset(str, sGenericFontLineAlignment, main_font_lut, &main_font_utf8_lut);

    ColorRGBA color;
    bcopy(gDialogCarryoverColor, color, sizeof(color));

    create_dl_translation_matrix(MENU_MTX_PUSH, x, y, 0.0f);

    while ((c = str[strPos]) != '\0') {
        // Handle text alignment if needed
        if (alignmentXOffset != 0) {
            create_dl_translation_matrix(MENU_MTX_NOPUSH, alignmentXOffset, 0.0f, 0.0f);
            alignmentXOffset = 0;
        }
        switch (c) {
            // Newline
            case '\n':
                gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
                if (lineNum == maxLines) {
                    if (gDialogBoxState == DIALOG_STATE_VERTICAL) {
                        bcopy(color, gDialogColorByEnd, sizeof(gDialogColorByEnd));
                    }
                    return strPos + 1;
                }

                create_dl_translation_matrix(MENU_MTX_PUSH, x, y - (lineNum * sGenericFontLineHeight), 0.0f);
                lineNum++;
                // Can skip any queued spaces
                queuedSpaces = 0;
                // Calculate alignment of new line
                alignmentXOffset = get_alignment_x_offset(&str[strPos + 1], sGenericFontLineAlignment, main_font_lut, &main_font_utf8_lut);
                break;

            // Space
            case ' ':
                queuedSpaces++;
                break;

            // Tab
            case '\t':
                queuedSpaces += 4;
                break;

            // %d or %s: Display value of dialog variable
            case '%':
                // Resolve queued spaces
                if (queuedSpaces != 0) {
                    create_dl_translation_matrix(MENU_MTX_NOPUSH, queuedSpaces * SPACE_KERNING(segmented_to_virtual(main_font_lut)), 0.0f, 0.0f);
                    queuedSpaces = 0;
                }

                // %d: Display dialog var as a decimal integer.
                if (str[strPos + 1] == 'd') {
                    char dialogVarText[32];
                    strPos++;
                    format_int_to_string(dialogVarText, gDialogVariable.asInt);
                    render_main_font_text(0, 0, dialogVarText, -1);
                    kerning = get_string_width(dialogVarText, main_font_lut, &main_font_utf8_lut);
                    create_dl_translation_matrix(MENU_MTX_NOPUSH, kerning, 0.0f, 0.0f);
                    break;
                // %s: Display dialog var as a pointer to a string.
                } else if (str[strPos + 1] == 's') {
                    strPos++;
                    render_main_font_text(0, 0, gDialogVariable.asStr, -1);
                    kerning = get_string_width(gDialogVariable.asStr, main_font_lut, &main_font_utf8_lut);
                    create_dl_translation_matrix(MENU_MTX_NOPUSH, kerning, 0.0f, 0.0f);
                    break;
                // %%: Special case, print only a single %.
                } else if (str[strPos + 1] == '%') {
                    strPos++;
                }

                // If the character following the % is not 'd' or 's', print the % as a normal character.
                goto render_character;

            // Color set control character
            case HEX(CONTROL_CHAR_COL): // '\033'
                validColor = TRUE;

                for (u32 i = 0; i < sizeof(color); i++) {
                    if (i == sizeof(ColorRGB)) { // check if alpha should be ignored (if alpha is "--")
                        if (str[strPos + i * 2 + 1] == CHAR_VALUE_IGNORE[0] && str[strPos + i * 2 + 2] == CHAR_VALUE_IGNORE[0]) {
                            color[i] = (u8) gDialogTextAlpha;
                            continue;
                        }
                    }

                    // If the sequence following the control char is not a valid RGBA32 color, interpret it as normal text.
                    // NOTE: Invalid color combinations can also cause color rendering issues, but those are considered user error.
                    s32 val = hex_pair_to_value(str, strPos + i * 2 + 1);
                    if (val == -1) {
                        validColor = FALSE;
                        break;
                    }

                    color[i] = val;
                }

                if (validColor) {
                    gDPSetEnvColor(gDisplayListHead++, color[0], color[1], color[2], color[3]);
                    strPos += sizeof(color) * 2;
                }
                break;
            
            // Color reset control character
            case HEX(CONTROL_CHAR_RESET): // '\034'
                bcopy(sActiveTextColor, color, sizeof(color));
                gDPSetEnvColor(gDisplayListHead++, color[0], color[1], color[2], color[3]);
                break;

            // Normal character rendering
            default:
render_character:
                // Resolve queued spaces
                if (queuedSpaces != 0) {
                    create_dl_translation_matrix(MENU_MTX_NOPUSH, queuedSpaces * SPACE_KERNING(segmented_to_virtual(main_font_lut)), 0.0f, 0.0f);
                    queuedSpaces = 0;
                }

                if (!(c & 0x80)) {
                    kerning = render_generic_ascii_char(c);
                } else {
                    kerning = render_generic_unicode_char(str, &strPos);
                }

                create_dl_translation_matrix(MENU_MTX_NOPUSH, kerning, 0.0f, 0.0f);
                break;
        }

        strPos++;
    }

    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
    gLastDialogLineNum = lineNum; // Used for rendering the choice triangle during dialog boxes
    return -1;
}

/**
 * Prints a generic white string.
 */
void print_generic_string(s16 x, s16 y, char *str) {
    print_generic_string_aligned(x, y, str, TEXT_ALIGN_LEFT);
}

/**
 * Prints a hud string in the colorful font.
 */
void print_hud_lut_string(s16 x, s16 y, char *str) {
    s32 strPos = 0;
    char c;
    struct AsciiCharLUTEntry *hudLUT = segmented_to_virtual(main_hud_lut); // 0-9 A-Z HUD Color Font
    u32 curX = x;
    u32 curY = y;
    u32 renderX, renderY;
    struct Utf8CharLUTEntry *utf8Entry;
    const Texture *texture;
    u32 kerning;

    if (str == NULL) {
        return;
    }

    while ((c = str[strPos]) != '\0') {
        gDPPipeSync(gDisplayListHead++);

        if (!(c & 0x80)) {
            texture = hudLUT[ASCII_LUT_INDEX(c)].texture;
            kerning = hudLUT[ASCII_LUT_INDEX(c)].kerning;
        } else {
            utf8Entry = utf8_lookup(&main_hud_utf8_lut, str, &strPos);
            if ((utf8Entry->flags & TEXT_DIACRITIC_MASK) == TEXT_DIACRITIC_UMLAUT_UPPERCASE) {
                renderX = curX;
                renderY = curY - 4;
                gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, &texture_hud_char_umlaut);
                gSPDisplayList(gDisplayListHead++, dl_rgba16_load_tex_block);
                gSPTextureRectangle(gDisplayListHead++, renderX << 2, renderY << 2, (renderX + 16) << 2,
                            (renderY + 16) << 2, G_TX_RENDERTILE, 0, 0, 1 << 10, 1 << 10);
                gDPPipeSync(gDisplayListHead++);
            }
            texture = utf8Entry->texture;
            kerning = utf8Entry->kerning;
        }

        if (texture != NULL) {
            gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture);

            renderX = curX;
            renderY = curY;
            if (c == '\'') {
                renderX -= 2;
                renderY -= 7;
            } else if (c == '"') {
                renderX += 1;
                renderY -= 7;
            } else if (c == ',') {
                renderX -= 4;
                renderY += 7;
            } else if (c == '.') {
                renderX -= 2;
                renderY += 1;
            }

            gSPDisplayList(gDisplayListHead++, dl_rgba16_load_tex_block);
            gSPTextureRectangle(gDisplayListHead++, renderX << 2, renderY << 2, (renderX + 16) << 2,
                                (renderY + 16) << 2, G_TX_RENDERTILE, 0, 0, 1 << 10, 1 << 10);
        }

        curX += kerning;
        strPos++;
    }
}

/**
 * Renders a single ASCII character in the menu font.
 */
static u32 render_menu_ascii_char(char c, u32 curX, u32 curY) {
    struct AsciiCharLUTEntry *fontLUT = segmented_to_virtual(menu_font_lut);
    const Texture *texture = fontLUT[ASCII_LUT_INDEX(c)].texture;

    if (texture != NULL) {
        gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_IA, G_IM_SIZ_8b, 1, texture);
        gDPLoadSync(gDisplayListHead++);
        gDPLoadBlock(gDisplayListHead++, G_TX_LOADTILE, 0, 0, 8 * 8 - 1, CALC_DXT(8, G_IM_SIZ_8b_BYTES));
        gSPTextureRectangle(gDisplayListHead++, curX << 2, curY << 2, (curX + 8) << 2,
                            (curY + 8) << 2, G_TX_RENDERTILE, 0, 0, 1 << 10, 1 << 10);
    }

    return fontLUT[ASCII_LUT_INDEX(c)].kerning;
}

/**
 * Renders a single UTF-8 character in the menu font.
 */
static u32 render_menu_unicode_char(char *str, s32 *strPos, u32 curX, u32 curY) {
    struct Utf8CharLUTEntry *utf8Entry = utf8_lookup(&menu_font_utf8_lut, str, strPos);

    if (utf8Entry->texture == NULL) {
        return utf8Entry->kerning;
    }

    if (utf8Entry->flags & TEXT_DIACRITIC_MASK) {
        struct DiacriticLUTEntry *diacriticLUT = segmented_to_virtual(&menu_font_diacritic_lut);
        struct DiacriticLUTEntry *diacritic = &diacriticLUT[utf8Entry->flags & TEXT_DIACRITIC_MASK];

        s32 fakeStrPos = 0;
        render_menu_unicode_char(segmented_to_virtual(diacritic->str), &fakeStrPos, curX + diacritic->xOffset, curY - diacritic->yOffset);
    }

    gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_IA, G_IM_SIZ_8b, 1, utf8Entry->texture);
    gDPLoadSync(gDisplayListHead++);
    gDPLoadBlock(gDisplayListHead++, G_TX_LOADTILE, 0, 0, 8 * 8 - 1, CALC_DXT(8, G_IM_SIZ_8b_BYTES));
    gSPTextureRectangle(gDisplayListHead++, curX << 2, curY << 2, (curX + 8) << 2,
                        (curY + 8) << 2, G_TX_RENDERTILE, 0, 0, 1 << 10, 1 << 10);

    return utf8Entry->kerning;
}

/**
 * Prints a menu white string in the smaller font.
 * Only available in the file select and star select menus.
 */
void print_menu_generic_string(s16 x, s16 y, char *str) {
    s32 strPos = 0;
    char c;
    u32 curX = x;
    u32 curY = y;
    u32 kerning;

    while ((c = str[strPos]) != '\0') {
        if (c & 0x80) {
            kerning = render_menu_unicode_char(str, &strPos, curX, curY);
        } else {
            kerning = render_menu_ascii_char(c, curX, curY);
        }

        curX += kerning;
        strPos++;
    }
}

/**
 * Prints a string in the green credits font.
 */
void print_credits_string(s16 x, s16 y, char *str) {
    s32 strPos = 0;
    char c;
    struct AsciiCharLUTEntry *fontLUT = segmented_to_virtual(main_credits_font_lut);
    u32 curX = x;
    u32 curY = y;

    gDPSetTile(gDisplayListHead++, G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0,
                G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD);
    gDPTileSync(gDisplayListHead++);
    gDPSetTile(gDisplayListHead++, G_IM_FMT_RGBA, G_IM_SIZ_16b, 2, 0, G_TX_RENDERTILE, 0,
                G_TX_CLAMP, 3, G_TX_NOLOD, G_TX_CLAMP, 3, G_TX_NOLOD);
    gDPSetTileSize(gDisplayListHead++, G_TX_RENDERTILE, 0, 0, (8 - 1) << G_TEXTURE_IMAGE_FRAC, (8 - 1) << G_TEXTURE_IMAGE_FRAC);

    while ((c = str[strPos]) != '\0') {
        if (fontLUT[ASCII_LUT_INDEX(c)].texture != NULL) {
            gDPPipeSync(gDisplayListHead++);
            gDPSetTextureImage(gDisplayListHead++, G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, fontLUT[ASCII_LUT_INDEX(c)].texture);
            gDPLoadSync(gDisplayListHead++);
            gDPLoadBlock(gDisplayListHead++, G_TX_LOADTILE, 0, 0, 8 * 8 - 1, CALC_DXT(8, G_IM_SIZ_16b_BYTES));
            gSPTextureRectangle(gDisplayListHead++, curX << 2, curY << 2, (curX + 8) << 2,
                                (curY + 8) << 2, G_TX_RENDERTILE, 0, 0, 1 << 10, 1 << 10);
        }

        curX += fontLUT[ASCII_LUT_INDEX(c)].kerning;
        strPos++;
    }
}

/**
 * Variants of the above that allow for text alignment.
 */
void print_generic_string_aligned(s16 x, s16 y, char *str, u32 alignment) {
    sGenericFontLineHeight = DIALOG_LINE_HEIGHT_EN;
    sGenericFontLineAlignment = alignment;
    render_main_font_text(x, y, str, -1);
}

void print_hud_lut_string_aligned(s16 x, s16 y, char *str, u32 alignment) {
    x += get_alignment_x_offset(str, alignment, main_hud_lut, &main_hud_utf8_lut);
    print_hud_lut_string(x, y, str);
}

void print_menu_generic_string_aligned(s16 x, s16 y, char *str, u32 alignment) {
    x += get_alignment_x_offset(str, alignment, menu_font_lut, &menu_font_utf8_lut);
    print_menu_generic_string(x, y, str);
}

void print_credits_string_aligned(s16 x, s16 y, char *str, u32 alignment) {
    x += get_alignment_x_offset(str, alignment, main_credits_font_lut, NULL);
    print_credits_string(x, y, str);
}

void handle_menu_scrolling(s8 scrollDirection, s8 *currentIndex, s8 minIndex, s8 maxIndex) {
    u8 index = 0;

    if (scrollDirection == MENU_SCROLL_VERTICAL) {
        if (gPlayer1Controller->rawStickY >  60) index++;
        if (gPlayer1Controller->rawStickY < -60) index += 2;
    } else if (scrollDirection == MENU_SCROLL_HORIZONTAL) {
        if (gPlayer1Controller->rawStickX >  60) index += 2;
        if (gPlayer1Controller->rawStickX < -60) index++;
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

/**
 * Initialise a dialog box.
 */
void create_dialog_box(s16 dialog) {
    if (gDialogID == DIALOG_NONE) {
        gDialogID = dialog;
        gDialogBoxType = DIALOG_TYPE_ROTATE;
    }
}

/**
 * Initialise a dialog box with an integer variable to be displayed with %d.
 */
void create_dialog_box_with_int_var(s16 dialog, s32 dialogVar) {
    DialogVariable var = { .asInt = dialogVar };
    create_dialog_box_with_var(dialog, var);
}

/**
 * Initialise a dialog box with a string variable to be displayed with %s.
 */
void create_dialog_box_with_str_var(s16 dialog, char *dialogVar) {
    DialogVariable var = { .asStr = dialogVar };
    create_dialog_box_with_var(dialog, var);
}

/**
 * Initialise a dialog box with a general variable.
 */
void create_dialog_box_with_var(s16 dialog, DialogVariable dialogVar) {
    if (gDialogID == DIALOG_NONE) {
        gDialogID = dialog;
        gDialogVariable = dialogVar;
        gDialogBoxType = DIALOG_TYPE_ROTATE;
    }
}

/**
 * Initialise a dialog box with black text on a white background instead of white text on black.
 */
void create_dialog_inverted_box(s16 dialog) {
    if (gDialogID == DIALOG_NONE) {
        gDialogID = dialog;
        gDialogBoxType = DIALOG_TYPE_ZOOM;
    }
}

/**
 * Initialise a dialog box that asks for a response.
 */
void create_dialog_box_with_response(s16 dialog) {
    if (gDialogID == DIALOG_NONE) {
        gDialogID = dialog;
        gDialogBoxType = DIALOG_TYPE_ROTATE;
        gDialogHasResponse = TRUE;
    }
}

void reset_dialog_render_state(void) {
    level_set_transition(0, NULL);

    gDialogBoxScale = 19.0f;
    gDialogBoxOpenTimer = 90.0f;
    gDialogBoxState = DIALOG_STATE_OPENING;
    gDialogID = DIALOG_NONE;
    gDialogTextPos = 0;
    gDialogHasResponse = FALSE;
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

    create_dl_translation_matrix(MENU_MTX_PUSH, BOX_TRANS_X, BOX_TRANS_Y, 0);
    create_dl_scale_matrix(MENU_MTX_NOPUSH, 1.1f, (((f32) linesPerBox / BOX_SCALE) + 0.1f), 1.0f);

    gSPDisplayList(gDisplayListHead++, dl_draw_text_bg_box);
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
}

u32 ensure_nonnegative(s16 value) {
    return ((value < 0) ? 0 : value);
}

static void handle_dialog_text_and_pages(struct DialogEntry *dialog) {
    char *str = segmented_to_virtual(dialog->str);
    s8 totalLines;
    s32 printResult;
    s16 yPos = 2 - DIALOG_LINE_HEIGHT;

    if (gDialogBoxState == DIALOG_STATE_HORIZONTAL) {
        // If scrolling, consider the number of lines for both
        // the current page and the page being scrolled to.
        totalLines = dialog->linesPerBox * 2;
        yPos += gDialogScrollOffsetY;
    } else {
        totalLines = dialog->linesPerBox;
    }

    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);

    gDialogTextAlpha = 255;
    if (gDialogBoxType == DIALOG_TYPE_ZOOM) {
        set_text_color(0, 0, 0);
    } else {
        set_text_color(255, 255, 255);
    }

    if (gDialogBoxState == DIALOG_STATE_OPENING) {
        bcopy(sActiveTextColor, gDialogCarryoverColor, sizeof(gDialogCarryoverColor));
    } else {
        // Deliberately not using set_text_color here, as gDialogCarryoverColor isn't
        // representative of the text color at the start of the dialog prompt
        gDPSetEnvColor(gDisplayListHead++, gDialogCarryoverColor[0], gDialogCarryoverColor[1], gDialogCarryoverColor[2], gDialogCarryoverColor[3]);
    }

    sGenericFontLineHeight = DIALOG_LINE_HEIGHT;
    sGenericFontLineAlignment = TEXT_ALIGN_LEFT;
    printResult = render_main_font_text(0, yPos, str + gDialogTextPos, totalLines);

    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);

    if (gDialogBoxState == DIALOG_STATE_VERTICAL) {
        if (printResult == -1) { // Reached end of dialog box
            gLastDialogPageStrPos = -1;
        } else {
            gLastDialogPageStrPos = gDialogTextPos + printResult;
        }
    }
}

void render_dialog_triangle_choice(void) {
    if (gDialogBoxState == DIALOG_STATE_VERTICAL) {
        handle_menu_scrolling(MENU_SCROLL_HORIZONTAL, &gDialogLineNum, 1, 2);
    }

    create_dl_translation_matrix(MENU_MTX_NOPUSH, (gDialogLineNum * 56) - 47, 2 - (gLastDialogLineNum * DIALOG_LINE_HEIGHT), 0);

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

    create_dl_translation_matrix(MENU_MTX_PUSH, 118.f, (linesPerBox * -DIALOG_LINE_HEIGHT) + 5, 0);
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

// King Bob-omb (Start), Whomp (Start), King Bob-omb (throw him out), Eyerock (Start), Wiggler (Start)
static s16 sDialogBossStart[] = { DIALOG_017, DIALOG_114, DIALOG_128, DIALOG_117, DIALOG_150 };
// Koopa the Quick (BoB), Koopa the Quick (THI), Penguin Race, Fat Penguin Race (120 stars)
static s16 sDialogRaceSound[] = { DIALOG_005, DIALOG_009, DIALOG_055, DIALOG_164             };
// Red Switch, Green Switch, Blue Switch, 100 coins star, Bowser Red Coin Star
static s16 sDialogStarSound[] = { DIALOG_010, DIALOG_011, DIALOG_012, DIALOG_013, DIALOG_014 };
// King Bob-omb (Start), Whomp (Defeated), King Bob-omb (Defeated, missing in JP), Eyerock (Defeated), Wiggler (Defeated)
static s16 sDialogBossStop[]  = { DIALOG_017, DIALOG_115, DIALOG_116, DIALOG_118, DIALOG_152 };

void handle_special_dialog_text(s16 dialogID) { // dialog ID tables, in order
    s16 i;

    for (i = 0; i < (s16) ARRAY_COUNT(sDialogBossStart); i++) {
        if (sDialogBossStart[i] == dialogID) {
            seq_player_unlower_volume(SEQ_PLAYER_LEVEL, 60);
            play_music(SEQ_PLAYER_LEVEL, SEQUENCE_ARGS(4, SEQ_EVENT_BOSS), 0);
            return;
        }
    }

    for (i = 0; i < (s16) ARRAY_COUNT(sDialogRaceSound); i++) {
        if (sDialogRaceSound[i] == dialogID && gDialogLineNum == 1) {
            play_race_fanfare();
            return;
        }
    }

    for (i = 0; i < (s16) ARRAY_COUNT(sDialogStarSound); i++) {
        if (sDialogStarSound[i] == dialogID && gDialogLineNum == 1) {
            play_sound(SOUND_MENU_STAR_SOUND, gGlobalSoundSource);
            return;
        }
    }

    for (i = 0; i < (s16) ARRAY_COUNT(sDialogBossStop); i++) {
        if (sDialogBossStop[i] == dialogID) {
            seq_player_fade_out(SEQ_PLAYER_LEVEL, 1);
            return;
        }
    }
}

s16 gMenuMode = MENU_MODE_NONE;

LangArray textEndCutscene1 = DEFINE_LANGUAGE_ARRAY(
    "Mario!",
    "Mario!",
    "Mario!",
    "マリオ！！",
    "¡Mario!");

LangArray textEndCutscene2 = DEFINE_LANGUAGE_ARRAY(
    "The power of the Stars is restored to the castle...",
    "Grâce aux étoiles, le château a retrouvé ses pouvoirs...",
    "Die Macht der Sterne ruht wieder sicher im Schloss...",
    "おしろにスターが もどったのね",
    "El poder de las estrellas ha vuelto al castillo...");

LangArray textEndCutscene3 = DEFINE_LANGUAGE_ARRAY(
    "...and it's all thanks to you!",
    "...et ceci grâce à toi!",
    "...und alles dank Deiner Hilfe!",
    "みんな あなたのおかげだわ！",
    "¡y es todo gracias a ti!");

LangArray textEndCutscene4 = DEFINE_LANGUAGE_ARRAY(
    "Thank you, Mario!",
    "Merci, Mario!",
    "Vielen Dank, Mario!",
    "ありがとう マリオ",
    "¡Gracias, Mario!");

LangArray textEndCutscene5 = DEFINE_LANGUAGE_ARRAY(
    "We have to do something special for you...",
    "Tu mérites une récompense...",
    "Wir haben eine Überraschung für Dich...",
    "なにか おれいをしなくちゃ・・",
    "Tenemos que prepararte algo especial...");

LangArray textEndCutscene6 = DEFINE_LANGUAGE_ARRAY(
    "Listen, everybody,",
    "Venez les amis...",
    "Hört alle her...",
    "さあ みんな",
    "Escuchadme todos:");

LangArray textEndCutscene7 = DEFINE_LANGUAGE_ARRAY(
    "let's bake a delicious cake...",
    "Allons préparer un délicieux gâteau...",
    "Laßt uns einen leckeren Kuchen backen...",
    "おいしいケーキを やきましょう",
    "Hagamos una deliciosa tarta...");

LangArray textEndCutscene8 = DEFINE_LANGUAGE_ARRAY(
    "...for Mario...",
    "...pour Mario...",
    "...für Mario...",
    "マリオの ために・・・",
    "...para Mario...");

LangArray textEndCutscene9 = DEFINE_LANGUAGE_ARRAY(
    "Mario!",
    "Mario!",
    "Mario!",
    "マリオ！！",
    "¡Mario!");

LangArray *gEndCutsceneStringsEn[] = {
    &textEndCutscene1,
    &textEndCutscene2,
    &textEndCutscene3,
    &textEndCutscene4,
    &textEndCutscene5,
    &textEndCutscene6,
    &textEndCutscene7,
    &textEndCutscene8,
    &textEndCutscene9
};

u16 gCutsceneMsgFade        =  0;
s16 gCutsceneMsgIndex       = -1;
s16 gCutsceneMsgDuration    = -1;
s16 gCutsceneMsgTimer       =  0;
s8  gDialogCameraAngleIndex = CAM_SELECTION_MARIO;
s8  gDialogCourseActNum     =  1;

void render_dialog_entries(void) {
    void **dialogTable = segmented_to_virtual(gLanguageTables[gInGameLanguage].dialog_table);
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
            break;

        case DIALOG_STATE_VERTICAL:
            gDialogBoxOpenTimer = 0.0f;

            if (gPlayer1Controller->buttonPressed & (A_BUTTON | B_BUTTON)) {
                if (gLastDialogPageStrPos == -1) {
                    handle_special_dialog_text(gDialogID);
                    gDialogBoxState = DIALOG_STATE_CLOSING;
                } else {
                    gDialogBoxState = DIALOG_STATE_HORIZONTAL;
                    play_sound(SOUND_MENU_MESSAGE_NEXT_PAGE, gGlobalSoundSource);
                }
            }
            break;
        case DIALOG_STATE_HORIZONTAL: // scrolling
            gDialogScrollOffsetY += (dialog->linesPerBox * 2);

            if (gDialogScrollOffsetY >= dialog->linesPerBox * DIALOG_LINE_HEIGHT) {
                gDialogTextPos = gLastDialogPageStrPos;
                gDialogBoxState = DIALOG_STATE_VERTICAL;
                gDialogScrollOffsetY = 0;

                bcopy(gDialogColorByEnd, gDialogCarryoverColor, sizeof(gDialogCarryoverColor));
            }
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
                gDialogHasResponse = FALSE;
                gLastDialogPageStrPos = 0;
                gDialogResponse = DIALOG_RESPONSE_NONE;
            }
            break;
    }

    render_dialog_box_type(dialog, dialog->linesPerBox);

    gDPSetScissor(gDisplayListHead++, G_SC_NON_INTERLACE,
                  // Horizontal scissoring isn't really required and can potentially mess up widescreen enhancements.
                  0,
                  ensure_nonnegative(SCREEN_HEIGHT - dialog->width - 3),
                  SCREEN_WIDTH,
                  ensure_nonnegative(SCREEN_HEIGHT + (dialog->linesPerBox * DIALOG_LINE_HEIGHT) - dialog->width));
    handle_dialog_text_and_pages(dialog);

    if (gLastDialogPageStrPos == -1 && gDialogHasResponse) {
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
    gDialogTextAlpha = gCutsceneMsgFade;
    set_text_color(255, 255, 255);

    print_generic_string_aligned(SCREEN_CENTER_X, 13, LANG_ARRAY(*gEndCutsceneStringsEn[gCutsceneMsgIndex]), TEXT_ALIGN_CENTER);

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
    void **dialogTable = segmented_to_virtual(gLanguageTables[gInGameLanguage].dialog_table);
    struct DialogEntry *dialog = segmented_to_virtual(dialogTable[gDialogID]);
    char *str = segmented_to_virtual(dialog->str);

    create_dl_translation_matrix(MENU_MTX_PUSH, 97.0f, 118.0f, 0);

    gDialogTextAlpha = gCutsceneMsgFade;
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    gSPDisplayList(gDisplayListHead++, castle_grounds_seg7_dl_0700EA58);
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);

    set_text_color(20, 20, 20);
    print_generic_string(38, 142, str);

    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
    gDPSetEnvColor(gDisplayListHead++, 200, 80, 120, gDialogTextAlpha);
    gSPDisplayList(gDisplayListHead++, castle_grounds_seg7_us_dl_0700F2E8);

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
    Gfx* dlHead = gDisplayListHead;

    gSPDisplayList(dlHead++, dl_shade_screen_begin);
    gDPFillRectangle(dlHead++, GFX_DIMENSIONS_RECT_FROM_LEFT_EDGE(0), gBorderHeight,
        (GFX_DIMENSIONS_RECT_FROM_RIGHT_EDGE(0) - 1), ((SCREEN_HEIGHT - gBorderHeight) - 1));
    gSPDisplayList(dlHead++, dl_shade_screen_end);

    gDisplayListHead = dlHead;
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

    if (gRedCoinsCollected <= 9) {
        for (x = 0; x < gRedCoinsCollected; x++) {
            print_animated_red_coin(GFX_DIMENSIONS_FROM_RIGHT_EDGE(30) - x * 20, 16);
        }
    } else {
        gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);
        gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, 255);

        char str[10];
        sprintf(str, "×%d", gRedCoinsCollected);
        print_hud_lut_string(GFX_DIMENSIONS_FROM_RIGHT_EDGE(108), SCREEN_HEIGHT - 32, str);

        gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);

        print_animated_red_coin(GFX_DIMENSIONS_FROM_RIGHT_EDGE(116), 16);
    }
}

LangArray textCurrRatio43 = DEFINE_LANGUAGE_ARRAY(
    "ASPECT RATIO: 4:3\nPRESS L TO SWITCH",
    "RATIO D'ASPECT: 4:3\nAPPUYEZ SUR L POUR CHANGER",
    "SEITENVERHÄLTNIS: 4:3\nDRÜCKE L ZUM WECHSELN",
    "アスペクトひ: ４:３\nＬボタンできりかえ",
    "RELACIÓN DE ASPECTO: 4:3\nPULSA L PARA CAMBIAR");

LangArray textCurrRatio169 = DEFINE_LANGUAGE_ARRAY(
    "ASPECT RATIO: 16:9\nPRESS L TO SWITCH",
    "RATIO D'ASPECT: 16:9\nAPPUYEZ SUR L POUR CHANGER",
    "SEITENVERHÄLTNIS: 16:9\nDRÜCKE L ZUM WECHSELN",
    "アスペクトひ: １６:９\nＬボタンできりかえ",
    "RELACIÓN DE ASPECTO: 16:9\nPULSA L PARA CAMBIAR");

/// By default, not needed as puppycamera has an option, but should you wish to revert that, you are legally allowed.
#if defined(WIDE) && !defined(PUPPYCAM)
void render_widescreen_setting(void) {
    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);
    set_text_color(255, 255, 255);
    if (!gConfig.widescreen) {
        print_generic_string(10, 24, LANG_ARRAY(textCurrRatio43));
    } else {
        print_generic_string(10, 24, LANG_ARRAY(textCurrRatio169));
    }
    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
    if (gPlayer1Controller->buttonPressed & L_TRIG){
        gConfig.widescreen ^= 1;
        save_file_set_widescreen_mode(gConfig.widescreen);
    }
}
#endif

LangArray textCourseX = DEFINE_LANGUAGE_ARRAY(
    "COURSE %s",
    "NIVEAU %s",
    "KURS %s",
    "コース%s",
    "NIVEL %s");

LangArray textMyScore = DEFINE_LANGUAGE_ARRAY(
    "MY SCORE",
    "MON SCORE",
    "LEISTUNG",
    "マイスコア",
    "MI RÉCORD");

#define PAUSE_MENU_LEFT_X  106
#define PAUSE_MENU_RIGHT_X 117

#define PAUSE_MENU_COURSE_Y 157
#define PAUSE_MENU_ACT_Y 140
#define PAUSE_MENU_MY_SCORE_Y 121

void render_pause_my_score_coins(void) {
    char str[20];

    void **courseNameTbl = segmented_to_virtual(gLanguageTables[gInGameLanguage].course_name_table);
    void    **actNameTbl = segmented_to_virtual(gLanguageTables[gInGameLanguage].act_name_table);

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

    set_text_color(255, 255, 255);

    char *courseName = segmented_to_virtual(courseNameTbl[courseIndex]);

    if (courseIndex <= COURSE_NUM_TO_INDEX(COURSE_STAGES_MAX)) {
        char courseNumText[8];
        format_int_to_string(courseNumText, gCurrCourseNum);
        sprintf(str, LANG_ARRAY(textCourseX), courseNumText);
        print_generic_string_aligned(PAUSE_MENU_LEFT_X, PAUSE_MENU_COURSE_Y, str, TEXT_ALIGN_RIGHT);

        char *actName = segmented_to_virtual(actNameTbl[COURSE_NUM_TO_INDEX(gCurrCourseNum) * 6 + gDialogCourseActNum - 1]);

        if (starFlags & (1 << (gDialogCourseActNum - 1))) {
            print_generic_string_aligned(PAUSE_MENU_LEFT_X, PAUSE_MENU_ACT_Y, "★", TEXT_ALIGN_RIGHT);
        } else {
            print_generic_string_aligned(PAUSE_MENU_LEFT_X, PAUSE_MENU_ACT_Y, "☆", TEXT_ALIGN_RIGHT);
        }

        print_generic_string(PAUSE_MENU_RIGHT_X, PAUSE_MENU_ACT_Y,    actName);
        print_generic_string(PAUSE_MENU_RIGHT_X, PAUSE_MENU_COURSE_Y, courseName);

        if (save_file_get_course_star_count(gCurrSaveFileNum - 1, courseIndex) != 0) {
            print_generic_string_aligned(PAUSE_MENU_LEFT_X + 3, PAUSE_MENU_MY_SCORE_Y, LANG_ARRAY(textMyScore), TEXT_ALIGN_RIGHT);
        }
    } else {
        print_generic_string_aligned(SCREEN_CENTER_X, PAUSE_MENU_COURSE_Y, courseName, TEXT_ALIGN_CENTER);
    }

    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
}

LangArray textLakituMario = DEFINE_LANGUAGE_ARRAY(
    "LAKITU ↔ MARIO",
    "LAKITU ↔ MARIO",
    "LAKITU ↔ MARIO",
    "ジュゲム↔マリオ",
    "LAKITU ↔ MARIO");

LangArray textLakituStop = DEFINE_LANGUAGE_ARRAY(
    "LAKITU ↔ STOP",
    "LAKITU ↔ STOP",
    "LAKITU ↔ STOP",
    "ジュゲム↔ストップ",
    "LAKITU ↔ FIJA");

LangArray textNormalUpClose = DEFINE_LANGUAGE_ARRAY(
    "(NORMAL)(UP-CLOSE)",
    "(NORMAL)(GROS-PLAN)",
    "(NORMAL)(WEIT-ZOOM)",
    "（おすすめ）（リアル）",
    "(NORMAL)(CERCA)");

LangArray textNormalFixed = DEFINE_LANGUAGE_ARRAY(
    "(NORMAL)(FIXED)",
    "(NORMAL)(FIXE)",
    "(NORMAL)(STATIV)",
    "（おすすめ）（とまる）",
    "(NORMAL)(FIJA)");

void render_pause_camera_options(s16 x, s16 y, s8 *index, s16 xIndex) {
    handle_menu_scrolling(MENU_SCROLL_HORIZONTAL, index, 1, 2);

    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);

    set_text_color(255, 255, 255);
    print_generic_string_aligned(x + 54,  y,      LANG_ARRAY(textLakituMario),   TEXT_ALIGN_CENTER);
    print_generic_string_aligned(x + 54,  y - 15, LANG_ARRAY(textNormalUpClose), TEXT_ALIGN_CENTER);
    print_generic_string_aligned(x + 160, y,      LANG_ARRAY(textLakituStop),    TEXT_ALIGN_CENTER);
    print_generic_string_aligned(x + 160, y - 15, LANG_ARRAY(textNormalFixed),   TEXT_ALIGN_CENTER);

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

LangArray textContinue = DEFINE_LANGUAGE_ARRAY(
    "CONTINUE",
    "CONTINUER",
    "WEITER",
    "つづけて マリオする？",
    "CONTINUAR");

LangArray textExitCourse = DEFINE_LANGUAGE_ARRAY(
    "EXIT COURSE",
    "QUITTER NIVEAU",
    "KURS VERLASSEN",
    "コースからでる？",
    "SALIR DEL NIVEL");

LangArray textCameraAngleR = DEFINE_LANGUAGE_ARRAY(
    "SET CAMERA ANGLE WITH Ⓡ",
    "RÉGLAGE CAMÉRA AVEC Ⓡ",
    "KAMERA MIT Ⓡ VERSTELLEN",
    "Ｒボタンのカメラきりかえ",
    "MODO DE CÁMARA CON Ⓡ");

void render_pause_course_options(s16 x, s16 y, s8 *index, s16 yIndex) {
    handle_menu_scrolling(MENU_SCROLL_VERTICAL, index, 1, 3);

    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);

    set_text_color(255, 255, 255);
    print_generic_string(x, y,      LANG_ARRAY(textContinue));
    print_generic_string(x, y - 15, LANG_ARRAY(textExitCourse));

    if (*index != MENU_OPT_CAMERA_ANGLE_R) {
        print_generic_string(x, y - 31, LANG_ARRAY(textCameraAngleR));
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

LangArray textPause = DEFINE_LANGUAGE_ARRAY(
    "PAUSE",
    "PAUSE",
    "PAUSE",
    "PAUSE",
    "PAUSA");

void print_hud_pause_colorful_str(void) {
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);

    print_hud_lut_string_aligned(SCREEN_CENTER_X, 81, LANG_ARRAY(textPause), TEXT_ALIGN_CENTER);

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

LangArray textCoinX = DEFINE_LANGUAGE_ARRAY(
    "✪× %s",
    "✪× %s",
    "✪× %s",
    "✪ｘ%s",
    "✪× %s");

LangArray textStarX = DEFINE_LANGUAGE_ARRAY(
    "★× %s",
    "★× %s",
    "★× %s",
    "★ｘ%s",
    "★× %s");

void render_pause_castle_main_strings(s16 x, s16 y) {
    void **courseNameTbl = segmented_to_virtual(gLanguageTables[gInGameLanguage].course_name_table);

    void *courseName;

    char str[8];
    char countText[10];
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

    set_text_color(255, 255, 255);
    if (gDialogLineNum <= COURSE_NUM_TO_INDEX(COURSE_STAGES_MAX)) { // Main courses
        courseName = segmented_to_virtual(courseNameTbl[gDialogLineNum]);
        print_generic_string(x - 50, y + 35, courseName);

        render_pause_castle_course_stars(x - 65, y, gCurrSaveFileNum - 1, gDialogLineNum);

        format_int_to_string(countText, save_file_get_course_coin_score(gCurrSaveFileNum - 1, gDialogLineNum));
        sprintf(str, LANG_ARRAY(textCoinX), countText);
        print_generic_string(x - 22, y, str);

        format_int_to_string(str, gDialogLineNum + 1);
        print_generic_string_aligned(x - 55, y + 35, str, TEXT_ALIGN_RIGHT);
    } else { // Castle secret stars
        courseName = segmented_to_virtual(courseNameTbl[COURSE_MAX]);
        print_generic_string_aligned(x, y + 35, courseName, TEXT_ALIGN_CENTER);

        format_int_to_string(countText, save_file_get_total_star_count(gCurrSaveFileNum - 1,
                                                             COURSE_NUM_TO_INDEX(COURSE_BONUS_STAGES),
                                                             COURSE_NUM_TO_INDEX(COURSE_MAX)));
        sprintf(str, LANG_ARRAY(textStarX), countText);
        print_generic_string_aligned(x, y + 18, str, TEXT_ALIGN_CENTER);
    }

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

            if (gPlayer1Controller->buttonPressed & (A_BUTTON | START_BUTTON)) {
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
            render_pause_castle_main_strings(SCREEN_CENTER_X, 55);

            if (gPlayer1Controller->buttonPressed & (A_BUTTON | START_BUTTON)) {
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
    gDialogTextAlpha += 25;
    if (gDialogTextAlpha > 250) {
        gDialogTextAlpha = 250;
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

LangArray textHudHiScore = DEFINE_LANGUAGE_ARRAY(
    "HI SCORE",
    "MEILLEUR SCORE",
    "BESTLEISTUNG",
    "HISCORE",
    "NUEVO RECORD");

LangArray textCongratulations = DEFINE_LANGUAGE_ARRAY(
    "CONGRATULATIONS",
    "FELICITATIONS",
    "GRATULATION",
    "CONGRATULATIONS",
    "FELICIDADES");

void print_hud_course_complete_string(s8 str) {
    u8 colorFade = sins(gDialogColorFadeTimer) * 50.0f + 200.0f;

    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);
    gDPSetEnvColor(gDisplayListHead++, colorFade, colorFade, colorFade, 255);

    if (str == HUD_PRINT_HISCORE) {
        print_hud_lut_string_aligned(SCREEN_CENTER_X, 36, LANG_ARRAY(textHudHiScore),      TEXT_ALIGN_CENTER);
    } else { // HUD_PRINT_CONGRATULATIONS
        print_hud_lut_string_aligned(SCREEN_CENTER_X, 67, LANG_ARRAY(textCongratulations), TEXT_ALIGN_CENTER);
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

#ifdef ENABLE_LIVES
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

LangArray textClear = DEFINE_LANGUAGE_ARRAY(
    "CLEAR",
    "CLEAR",
    "CLEAR",
    "クリア！",
    "HECHO");

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
    char courseNumText[8];

    void **actNameTbl    = segmented_to_virtual(gLanguageTables[gInGameLanguage].act_name_table);
    void **courseNameTbl = segmented_to_virtual(gLanguageTables[gInGameLanguage].course_name_table);

    if ((gLastCompletedCourseNum != COURSE_NONE) && (gLastCompletedCourseNum <= COURSE_STAGES_MAX)) { // Main courses
        print_hud_course_complete_coins(COURSE_COMPLETE_COINS_X, COURSE_COMPLETE_COINS_Y);
        play_star_fanfare_and_flash_hud(HUD_FLASH_STARS, (1 << (gLastCompletedStarNum - 1)));

        if (gLastCompletedStarNum == 7) {
            name = segmented_to_virtual(actNameTbl[COURSE_STAGES_MAX * 6 + 1]);
        } else {
            name = segmented_to_virtual(actNameTbl[COURSE_NUM_TO_INDEX(gLastCompletedCourseNum) * 6 + gLastCompletedStarNum - 1]);
        }

        // Print course number
        gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);

        format_int_to_string(courseNumText, gLastCompletedCourseNum);
        sprintf(str, LANG_ARRAY(textCourseX), courseNumText);
        set_text_color(0, 0, 0);
        print_generic_string(COURSE_COMPLETE_COURSE_X + 2,  COURSE_COMPLETE_COURSE_Y - 2, str);

        set_text_color(255, 255, 255);
        print_generic_string(COURSE_COMPLETE_COURSE_X,      COURSE_COMPLETE_COURSE_Y, str);

        gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
    } else if (gLastCompletedCourseNum == COURSE_BITDW || gLastCompletedCourseNum == COURSE_BITFS) { // Bowser courses
        name = segmented_to_virtual(courseNameTbl[COURSE_NUM_TO_INDEX(gLastCompletedCourseNum)]);
        u32 clearX = get_string_width(name, main_font_lut, &main_font_utf8_lut) + COURSE_COMPLETE_COURSE_X + 16;

        // Print course name and clear text
        gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);

        set_text_color(0, 0, 0);
        print_generic_string(CONGRATULATIONS_COURSE_X + 2, CONGRATULATIONS_COURSE_Y - 2, name);
        print_generic_string(clearX                   + 2, CONGRATULATIONS_COURSE_Y - 2, LANG_ARRAY(textClear));
        set_text_color(255, 255, 255);
        print_generic_string(CONGRATULATIONS_COURSE_X,     CONGRATULATIONS_COURSE_Y,     name);
        print_generic_string(clearX,                       CONGRATULATIONS_COURSE_Y,     LANG_ARRAY(textClear));
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

    set_text_color(0, 0, 0);
    print_generic_string(COURSE_COMPLETE_ACT_X + 2, COURSE_COMPLETE_ACT_Y - 2, name);

    set_text_color(255, 255, 255);
    print_generic_string(COURSE_COMPLETE_ACT_X,     COURSE_COMPLETE_ACT_Y,     name);

    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
}

LangArray textSaveAndContinue = DEFINE_LANGUAGE_ARRAY(
    "SAVE & CONTINUE",
    "SAUVEGARDER & CONTINUER",
    "SPEICHERN & WEITER",
    "セーブしてつづける？",
    "GUARDAR Y CONTINUAR");

LangArray textSaveAndQuit = DEFINE_LANGUAGE_ARRAY(
    "SAVE & QUIT",
    "SAUVEGARDER & QUITTER",
    "SPEICHERN & ENDE",
    "セーブしておわる？",
    "GUARDAR Y SALIR");

LangArray textContinueWithoutSave = DEFINE_LANGUAGE_ARRAY(
    "CONTINUE, DON'T SAVE",
    "CONTINUER SANS SAUVEGARDER",
    "WEITER OHNE ZU SPEICHERN",
    "セーブしないでつづける？",
    "CONTINUAR SIN GUARDAR");

void render_save_confirmation(s16 x, s16 y, s8 *index, s16 yPos) {
    handle_menu_scrolling(MENU_SCROLL_VERTICAL, index, 1, 3);

    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);

    set_text_color(255, 255, 255);
    print_generic_string(x + 12, y,      LANG_ARRAY(textSaveAndContinue));
    print_generic_string(x + 12, y - 20, LANG_ARRAY(textSaveAndQuit));
    print_generic_string(x + 12, y - 40, LANG_ARRAY(textContinueWithoutSave));

    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);

    create_dl_translation_matrix(MENU_MTX_PUSH, x, y - ((*index - 1) * yPos), 0);

    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    gSPDisplayList(gDisplayListHead++, dl_draw_triangle);

    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
}

// Save option strings are longer in French, so render them further to the left.
#define SAVE_CONFIRMATION_X_EN  100
#define SAVE_CONFIRMATION_X_FR  80  

#ifdef ENABLE_FRENCH
#define SAVE_CONFIRMATION_X ((gInGameLanguage == LANGUAGE_FRENCH) ? SAVE_CONFIRMATION_X_FR : SAVE_CONFIRMATION_X_EN)
#else
#define SAVE_CONFIRMATION_X SAVE_CONFIRMATION_X_EN
#endif

s32 render_course_complete_screen(void) {
    switch (gDialogBoxState) {
        case DIALOG_STATE_OPENING:
            render_course_complete_lvl_info_and_hud_str();
            if (gCourseDoneMenuTimer > 100 && gCourseCompleteCoinsEqual) {
                gDialogBoxState = DIALOG_STATE_VERTICAL;
                level_set_transition(-1, NULL);
                gDialogTextAlpha = 0;
                gDialogLineNum = MENU_OPT_DEFAULT;
            }
            break;

        case DIALOG_STATE_VERTICAL:
            shade_screen();
            render_course_complete_lvl_info_and_hud_str();
            render_save_confirmation(SAVE_CONFIRMATION_X, 86, &gDialogLineNum, 20);

            if (gCourseDoneMenuTimer > 110 && (gPlayer1Controller->buttonPressed & (A_BUTTON | START_BUTTON))) {
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

    gDialogTextAlpha += 25;
    if (gDialogTextAlpha > 250) {
        gDialogTextAlpha = 250;
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
