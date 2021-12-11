#include "config.h"
#include <ultra64.h>
#include "mtx.h"
#include "debug.h"

float myScale = 1.0f;
int myDegrees = 0;
uObjMtx final_mtx, rot_mtx;
int s2d_red = 255, s2d_green = 255, s2d_blue = 255, s2d_alpha = 255;
int drop_shadow = FALSE;
int drop_x = 0;
int drop_y = 0;

Gfx s2d_text_init_dl[] = {
    gsDPPipeSync(),
    gsDPSetTexturePersp(G_TP_NONE),
    gsDPSetTextureLOD(G_TL_TILE),
    gsDPSetTextureLUT(G_TT_NONE),
    gsDPSetTextureConvert(G_TC_FILT),
    gsDPSetAlphaCompare(G_AC_THRESHOLD),
    gsDPSetBlendColor(0, 0, 0, 0x01),

    gsDPSetCombineLERP(
        0, 0, 0, ENVIRONMENT,
        0, 0, 0, TEXEL0,
        0, 0, 0, ENVIRONMENT,
        0, 0, 0, TEXEL0
    ),

    gsSPEndDisplayList(),
};

void s2d_rdp_init(void) {
        gDPPipeSync(gdl_head++);
        gDPSetTextureFilter(gdl_head++, G_TF_POINT);
        gDPSetCycleType(gdl_head++, G_CYC_1CYCLE);
        gDPSetRenderMode(gdl_head++, G_RM_XLU_SPRITE, G_RM_XLU_SPRITE2);
    if (init_cond) {
        gSPDisplayList(gdl_head++, s2d_text_init_dl);
        gSPObjRenderMode(gdl_head++, G_OBJRM_XLU);
    }
}

void setup_s2d_texture(int idx) {
    gDPPipeSync(gdl_head++);
    gDPSetEnvColor(gdl_head++, s2d_red, s2d_green, s2d_blue, s2d_alpha);
    gSPObjLoadTxtr(gdl_head++, &s2d_tex[idx]);
}

void mtx_pipeline(uObjMtx *m, int x, int y) {
    gDPPipeSync(gdl_head++);

    mat2_ident(m, 1.0f / myScale);
    mat2_translate(m, x, y);

    gSPObjSubMatrix(gdl_head++, &m->m.X);
}

#define CLAMP_0(x) ((x < 0) ? 0 : x)

void draw_s2d_dropshadow(char c, int x, int y, uObjMtx *ds) {
    if (mtx_cond) mtx_pipeline(ds, x, y);
    if (tex_cond) setup_s2d_texture(c);

    if (s2d_red != 0
        && s2d_green != 0
        && s2d_blue != 0
        ) {
        gDPPipeSync(gdl_head++);
        gDPSetEnvColor(gdl_head++,
                   CLAMP_0(s2d_red - 100),
                   CLAMP_0(s2d_green - 100),
                   CLAMP_0(s2d_blue - 100),
                   s2d_alpha);
        if (spr_cond) gSPObjRectangleR(gdl_head++, &s2d_font);
        gDPSetEnvColor(gdl_head++, s2d_red, s2d_green, s2d_blue, s2d_alpha);
    }
}

void draw_s2d_glyph(char c, int x, int y, uObjMtx *mt) {
    if (mtx_cond) mtx_pipeline(mt, x, y);
    if (tex_cond) setup_s2d_texture(c);

    if (spr_cond) gSPObjRectangleR(gdl_head++, &s2d_font);
}

