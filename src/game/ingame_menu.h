#ifndef INGAME_MENU_H
#define INGAME_MENU_H

#include <PR/ultratypes.h>

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
extern s16 gInGameLanguage;

struct AsciiCharLUTEntry {
    const Texture *texture;
    const s8 kerning;
};

#define SPACE_KERNING(lut) (((struct AsciiCharLUTEntry *)(lut))[0].kerning)

struct Utf8CharLUTEntry {
    const u32 codepoint;
    const s8 kerning;
    const u16 flags; // used for diacritics and packed textures
    const Texture *texture;
};

struct Utf8LUT {
    const struct Utf8CharLUTEntry *lut2Bytes;
    const struct Utf8CharLUTEntry *lut3Bytes;
    const struct Utf8CharLUTEntry *lut4Bytes;
    const u16 length2Bytes;
    const u16 length3Bytes;
    const u16 length4Bytes;
    const struct Utf8CharLUTEntry *missingChar;
};

struct DiacriticLUTEntry {
    const s8 xOffset;
    const s8 yOffset;
    const Texture *texture;
};

enum GenericTextDiacriticMarks {
    TEXT_DIACRITIC_NONE,
    TEXT_DIACRITIC_GRAVE,
    TEXT_DIACRITIC_GRAVE_UPPERCASE,
    TEXT_DIACRITIC_ACUTE,
    TEXT_DIACRITIC_ACUTE_UPPERCASE,
    TEXT_DIACRITIC_CIRCUMFLEX,
    TEXT_DIACRITIC_CIRCUMFLEX_UPPERCASE,
    TEXT_DIACRITIC_TILDE,
    TEXT_DIACRITIC_TILDE_UPPERCASE,
    TEXT_DIACRITIC_UMLAUT,
    TEXT_DIACRITIC_UMLAUT_UPPERCASE,
};

enum TextAlignments {
    TEXT_ALIGN_LEFT,
    TEXT_ALIGN_CENTER,
    TEXT_ALIGN_RIGHT,
};

#define GENERIC_TEXT_PACKED 0x8000
#define GENERIC_TEXT_DIACRITIC_MASK 0x7FFF

struct DialogEntry {
    /*0x00*/ s32 voice;
    /*0x04*/ s8 linesPerBox;
    /*0x06*/ s16 leftOffset;
    /*0x08*/ s16 width;
    /*0x0C*/ const u8 *str;
};

// gDialogResponse
enum DialogResponseDefines {
    DIALOG_RESPONSE_NONE,
    DIALOG_RESPONSE_YES,
    DIALOG_RESPONSE_NO,
    DIALOG_RESPONSE_NOT_DEFINED,
    DIALOG_RESPONSE_MAXIMUM = 32
};

// Macro to create an array of all 4 languages' versions of a string.
#define LANGUAGE_TEXT(english, french, german, japanese) english

extern s32 gDialogResponse;
extern u16 gDialogColorFadeTimer;
extern s8  gLastDialogLineNum;
extern s32 gDialogVariable;
extern u16 gDialogTextAlpha;
extern s8  gRedCoinsCollected;

void create_dl_identity_matrix(void);
void create_dl_translation_matrix(s8 pushOp, f32 x, f32 y, f32 z);
void create_dl_ortho_matrix(void);
void create_dl_scale_matrix(s8 pushOp, f32 x, f32 y, f32 z);

s32 get_string_length(char *str, struct AsciiCharLUTEntry *asciiLut, struct Utf8LUT *utf8LUT);
void print_generic_string(s16 x, s16 y, char *str);
void print_hud_lut_string(s16 x, s16 y, char *str);
void print_menu_generic_string(s16 x, s16 y, char *str);
void print_generic_string_aligned(s16 x, s16 y, char *str, u32 alignment);
void print_hud_lut_string_aligned(s16 x, s16 y, char *str, u32 alignment);
void print_menu_generic_string_aligned(s16 x, s16 y, char *str, u32 alignment);
void print_credits_string(s16 x, s16 y, const char *str);

void handle_menu_scrolling(s8 scrollDirection, s8 *currentIndex, s8 minIndex, s8 maxIndex);
s32 get_str_x_pos_from_center(s16 centerPos, char *str, f32 scale);
void print_hud_my_score_coins(s32 useCourseCoinScore, s8 fileIndex, s8 courseIndex, s16 x, s16 y);
s32 get_dialog_id(void);
void create_dialog_box(s16 dialog);
void create_dialog_box_with_var(s16 dialog, s32 dialogVar);
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
