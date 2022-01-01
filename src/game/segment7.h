#ifndef SEGMENT7_H
#define SEGMENT7_H

#include <PR/ultratypes.h>
#include <PR/gbi.h>

// from main menu segment 7
extern Gfx dl_menu_idle_hand[];
extern Gfx dl_menu_grabbing_hand[];
extern Texture menu_hud_lut[];
extern Texture menu_font_lut[];
extern Gfx dl_menu_ia8_text_begin[];
extern Gfx dl_menu_ia8_text_end[];
extern Gfx dl_menu_rgba16_wood_course[];
#ifdef VERSION_EU
extern Gfx dl_menu_rgba16_wood_course_end[];
extern Gfx dl_menu_texture_course_upper[];
extern Gfx dl_menu_texture_niveau_upper[];
extern Gfx dl_menu_texture_kurs_upper[];

extern const Gfx eu_course_strings_en_table[];
extern const Gfx eu_course_strings_fr_table[];
extern const Gfx eu_course_strings_de_table[];
#endif

// from intro_segment7
extern Gfx *intro_seg7_dl_main_logo;
extern Gfx *intro_seg7_dl_copyright_trademark;
extern f32 intro_seg7_table_scale_1[];
extern f32 intro_seg7_table_scale_2[];

#endif // SEGMENT7_H
