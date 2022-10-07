#ifndef SEGMENT7_H
#define SEGMENT7_H

#include <PR/ultratypes.h>
#include <PR/gbi.h>

// from main menu segment 7
extern Gfx dl_menu_idle_hand[];
extern Gfx dl_menu_grabbing_hand[];
extern Texture menu_hud_lut[];
extern struct AsciiCharLUTEntry menu_font_lut[];
extern struct Utf8LUT menu_font_utf8_lut;
extern Texture texture_menu_font_char_umlaut[];
extern Gfx dl_menu_ia8_text_begin[];
extern Gfx dl_menu_ia8_text_end[];
extern Gfx dl_menu_rgba16_wood_course[];
#ifdef MULTILANG
extern Gfx dl_menu_rgba16_wood_course_end[];
extern Gfx dl_menu_texture_course_upper[];
extern Gfx dl_menu_texture_niveau_upper[];
extern Gfx dl_menu_texture_kurs_upper[];

extern const u8 course_strings_en_table[];
extern const u8 course_strings_fr_table[];
extern const u8 course_strings_de_table[];
extern const u8 *const course_strings_language_table[];
#endif

// from intro_segment7
extern Gfx *intro_seg7_dl_main_logo;
extern Gfx *intro_seg7_dl_copyright_trademark;
extern f32 intro_seg7_table_scale_1[];
extern f32 intro_seg7_table_scale_2[];

#endif // SEGMENT7_H
