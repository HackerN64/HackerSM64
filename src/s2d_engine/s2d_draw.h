#ifndef S2D_DRAW_H
#define S2D_DRAW_H
#include <ultra64.h>

extern float myScale;
extern int myDegrees;
extern int drop_shadow;
extern int drop_x, drop_y;

extern uObjMtx final_mtx, rot_mtx;

extern int s2d_red, s2d_green, s2d_blue, s2d_alpha;

extern void s2d_rdp_init(void);

extern void setup_font_texture(int idx);

extern void mtx_pipeline(uObjMtx *m, int x, int y);

extern void draw_s2d_glyph(char c, int x, int y, uObjMtx *mt);
extern void draw_s2d_shadow(char c, int x, int y, uObjMtx *ds);

#endif