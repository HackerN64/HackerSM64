#ifndef INGAME_MENU_H
#define INGAME_MENU_H

#include <PR/ultratypes.h>
#include "types.h"
#include "dialog_ids.h"
#include "course_table.h"

// General string macros
#define HEX(s)       GLUE2(0x, s)
#define HEX_STR(s)   STRING2(GLUE2(\x, s))

// Control characters (Must be hexadecimal without the
// '0x' prefix, possible values range from 00-1F)
#define CONTROL_CHAR_TERMINATOR 00 // '\0' (RESERVED!)
#define CONTROL_CHAR_TAB        09 // '\t' (RESERVED!)
#define CONTROL_CHAR_NEWLINE    0A // '\n' (RESERVED!)
#define CONTROL_CHAR_COL        1B // '\033'
#define CONTROL_CHAR_RESET      1C // '\034'

// Additional control macros
#define CHAR_VALUE_IGNORE "-"


/********************************** BEGIN USER DIALOG CONTROL **********************************/


/**
 * COL_RGB("XXXXXX"), COL_RGBA("XXXXXXXX")
 *
 * Set color of text to an RGB value.
 * e.g. COL_RGB("00FF00") will set the color to green.
 * Will use gDialogTextAlpha as the alpha value if alpha is not specified.
 * 
 * Example: "normal text " COL_RGBA("FF00007F") "transparent red text"
 */
#define COL_RGB(color) \
    HEX_STR(CONTROL_CHAR_COL) color CHAR_VALUE_IGNORE CHAR_VALUE_IGNORE
#define COL_RGBA(color) \
    HEX_STR(CONTROL_CHAR_COL) color

/**
 * COL_RESET
 *
 * Reset the text color back to its default text color.
 * 
 * Example: "colored text " COL_RESET "normal text"
 */
#define COL_RESET \
    HEX_STR(CONTROL_CHAR_RESET)


/*********************************** END USER DIALOG CONTROL ***********************************/


enum MenuMtxPushOp {
    MENU_MTX_NONE,
    MENU_MTX_PUSH,
    MENU_MTX_NOPUSH,
};

enum MenuScrollAxis {
    MENU_SCROLL_NONE,
    MENU_SCROLL_VERTICAL,
    MENU_SCROLL_HORIZONTAL,
};

enum MenuMode {
    MENU_MODE_NONE = -1,
    MENU_MODE_UNUSED_0,
    MENU_MODE_RENDER_PAUSE_SCREEN,
    MENU_MODE_RENDER_COURSE_COMPLETE_SCREEN,
    MENU_MODE_UNUSED_3
};

enum HUDFlashModes {
    HUD_FLASH_NONE,
    HUD_FLASH_STARS,
    HUD_FLASH_KEYS
};

extern s8 gHudFlash;

extern s8 gDialogCourseActNum;
extern u8 gInGameLanguage;

struct LanguageTables {
    /* 0x00 */ u8 *dialog_table;
    /* 0x01 */ u8 *course_name_table;
    /* 0x02 */ u8 *act_name_table;
}; /* 0x03 */

extern const struct LanguageTables gLanguageTables[];

struct AsciiCharLUTEntry {
    const Texture *texture;
    const s8 kerning;
};

// Convert an ASCII char to the index in the ASCII LUT. ASCII LUTs start at the space character.
#define ASCII_LUT_INDEX(c) ((c) - ' ')

// Macro to quickly get the kerning of the space character from an ASCII LUT.
#define SPACE_KERNING(lut) (((struct AsciiCharLUTEntry *)(lut))[ASCII_LUT_INDEX(' ')].kerning)

struct Utf8CharLUTEntry {
    u32 codepoint;
    s8 kerning;
    u16 flags; // used for diacritics and packed textures
    Texture *texture;
};

struct Utf8LUT {
    struct Utf8CharLUTEntry *lut2Bytes;
    struct Utf8CharLUTEntry *lut3Bytes;
    struct Utf8CharLUTEntry *lut4Bytes;
    u16 length2Bytes; // set these with the ARRAY_COUNT macro
    u16 length3Bytes;
    u16 length4Bytes;
    struct Utf8CharLUTEntry *missingChar;
};

struct DiacriticLUTEntry {
    s8 xOffset;
    s8 yOffset;
    char *str;
};

enum TextDiacriticMarks {
    TEXT_DIACRITIC_NONE,
    TEXT_DIACRITIC_CIRCUMFLEX,
    TEXT_DIACRITIC_CIRCUMFLEX_UPPERCASE,
    TEXT_DIACRITIC_ACUTE,
    TEXT_DIACRITIC_ACUTE_UPPERCASE,
    TEXT_DIACRITIC_GRAVE,
    TEXT_DIACRITIC_GRAVE_UPPERCASE,
    TEXT_DIACRITIC_TILDE,
    TEXT_DIACRITIC_TILDE_UPPERCASE,
    TEXT_DIACRITIC_UMLAUT,
    TEXT_DIACRITIC_UMLAUT_UPPERCASE,
    TEXT_DIACRITIC_CEDILLA,
#ifdef JAPANESE_CHARACTERS
    TEXT_DIACRITIC_DAKUTEN,
    TEXT_DIACRITIC_HANDAKUTEN,
#endif
};

enum TextAlignments {
    TEXT_ALIGN_LEFT,
    TEXT_ALIGN_CENTER,
    TEXT_ALIGN_RIGHT,
};

#define TEXT_FLAG_PACKED 0x8000
#define TEXT_DIACRITIC_MASK 0x00FF
// bits 0x0100 through 0x4000 are free for use, and the mask can be reduced if necessary

struct DialogEntry {
    /*0x00*/ s32 voice;
    /*0x04*/ s8 linesPerBox;
    /*0x06*/ s16 leftOffset;
    /*0x08*/ s16 width;
    /*0x0C*/ const char *str;
};

// gDialogResponse
enum DialogResponseDefines {
    DIALOG_RESPONSE_NONE,
    DIALOG_RESPONSE_YES,
    DIALOG_RESPONSE_NO,
    DIALOG_RESPONSE_NOT_DEFINED,
    DIALOG_RESPONSE_MAXIMUM = 32
};

// Types and defines for handling language arrays
#ifdef MULTILANG
extern const u8 gDefinedLanguages[];

enum MultilangLanguages {
    LANGUAGE_ENGLISH,
#ifdef ENABLE_FRENCH
    LANGUAGE_FRENCH,
#endif
#ifdef ENABLE_GERMAN
    LANGUAGE_GERMAN,
#endif
#ifdef ENABLE_JAPANESE
    LANGUAGE_JAPANESE,
#endif
#ifdef ENABLE_SPANISH
    LANGUAGE_SPANISH,
#endif
    LANGUAGE_COUNT,
    NUM_DEFINED_LANGUAGES = (LANGUAGE_COUNT - 1),
};

#ifdef ENABLE_FRENCH
#define LANG_ARRAY_COND_FRENCH(...) [LANGUAGE_FRENCH] = __VA_ARGS__,
#else
#define LANG_ARRAY_COND_FRENCH(...)
#endif
#ifdef ENABLE_GERMAN
#define LANG_ARRAY_COND_GERMAN(...) [LANGUAGE_GERMAN] = __VA_ARGS__,
#else
#define LANG_ARRAY_COND_GERMAN(...)
#endif
#ifdef ENABLE_JAPANESE
#define LANG_ARRAY_COND_JAPANESE(...) [LANGUAGE_JAPANESE] = __VA_ARGS__,
#else
#define LANG_ARRAY_COND_JAPANESE(...)
#endif
#ifdef ENABLE_SPANISH
#define LANG_ARRAY_COND_SPANISH(...) [LANGUAGE_SPANISH] = __VA_ARGS__,
#else
#define LANG_ARRAY_COND_SPANISH(...)
#endif

typedef char * LangArray[LANGUAGE_COUNT];
#define LANG_ARRAY(cmd) ((cmd)[gInGameLanguage])
#define DEFINE_LANGUAGE_ARRAY(english, french, german, japanese, spanish) { \
    [LANGUAGE_ENGLISH] = english,                                           \
    LANG_ARRAY_COND_FRENCH(french)                                          \
    LANG_ARRAY_COND_GERMAN(german)                                          \
    LANG_ARRAY_COND_JAPANESE(japanese)                                      \
    LANG_ARRAY_COND_SPANISH(spanish)                                        \
}

#else

// If multilang is off, ignore all other languages and only include English.
#define LANGUAGE_ENGLISH 0

typedef char * LangArray;
#define LANG_ARRAY(cmd) (cmd)
#define DEFINE_LANGUAGE_ARRAY(english, french, german, japanese, spanish) english

#endif

//! NOTE: These are just to assist with VSCode autocomplete and should not be considered functional.
#undef DEFINE_DIALOG
#define DEFINE_DIALOG(id, voice, linesPerBox, leftOffset, bottomOffset, dialogText)

#undef COURSE_ACTS
#define COURSE_ACTS(id, name, a, b, c, d, e, f)

#undef SECRET_STAR
#define SECRET_STAR(id, name)

#undef CASTLE_SECRET_STARS
#define CASTLE_SECRET_STARS(str)

#undef EXTRA_TEXT
#define EXTRA_TEXT(id, str)

typedef union {
    s32 asInt;
    char *asStr;
} DialogVariable;

extern s32 gDialogResponse;
extern u16 gDialogColorFadeTimer;
extern s8  gLastDialogLineNum;
extern DialogVariable gDialogVariable;
extern u16 gDialogTextAlpha;
extern s8  gRedCoinsCollected;

void create_dl_identity_matrix(void);
void create_dl_translation_matrix(s8 pushOp, f32 x, f32 y, f32 z);
void create_dl_ortho_matrix(void);
void create_dl_scale_matrix(s8 pushOp, f32 x, f32 y, f32 z);

void set_text_color(u32 r, u32 g, u32 b);
s32 get_string_width(char *str, struct AsciiCharLUTEntry *asciiLut, struct Utf8LUT *utf8LUT);
void format_int_to_string(char *buf, s32 value);
void print_generic_string(s16 x, s16 y, char *str);
void print_hud_lut_string(s16 x, s16 y, char *str);
void print_menu_generic_string(s16 x, s16 y, char *str);
void print_credits_string(s16 x, s16 y, char *str);
void print_generic_string_aligned(s16 x, s16 y, char *str, u32 alignment);
void print_hud_lut_string_aligned(s16 x, s16 y, char *str, u32 alignment);
void print_menu_generic_string_aligned(s16 x, s16 y, char *str, u32 alignment);
void print_credits_string_aligned(s16 x, s16 y, char *str, u32 alignment);

void handle_menu_scrolling(s8 scrollDirection, s8 *currentIndex, s8 minIndex, s8 maxIndex);
void print_hud_my_score_coins(s32 useCourseCoinScore, s8 fileIndex, s8 courseIndex, s16 x, s16 y);
s32 get_dialog_id(void);
void create_dialog_box(s16 dialog);
void create_dialog_box_with_int_var(s16 dialog, s32 dialogVar);
void create_dialog_box_with_str_var(s16 dialog, char *dialogVar);
void create_dialog_box_with_var(s16 dialog, DialogVariable dialogVar);
void create_dialog_inverted_box(s16 dialog);
void create_dialog_box_with_response(s16 dialog);
void reset_dialog_render_state(void);
void set_menu_mode(s16 mode);
void reset_cutscene_msg_fade(void);
void dl_rgba16_begin_cutscene_msg_fade(void);
void dl_rgba16_stop_cutscene_msg_fade(void);
void set_cutscene_message(s16 msgIndex, s16 msgDuration);
void do_cutscene_handler(void);
void render_hud_cannon_reticle(void);
void reset_red_coins_collected(void);
s32 render_menus_and_dialogs(void);

#endif // INGAME_MENU_H
