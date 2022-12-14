#include "config.h"

#include <ultra64.h>
#include "mtx.h"
#include "debug.h"

#include "s2d_draw.h"

#define s(sprite) ((uObjSprite *)seg2virt(&sprite))
#define t(texture) ((uObjTxtr *)seg2virt(&texture))

void f3d_rdp_init(void) {
    gDPPipeSync(gdl_head++);
    gDPSetCycleType(gdl_head++, G_CYC_1CYCLE);
    gDPSetRenderMode(gdl_head++, G_RM_XLU_SPRITE, G_RM_XLU_SPRITE2);
    gDPSetTextureLUT(gdl_head++, G_TT_NONE);
    gDPSetTexturePersp(gdl_head++, G_TP_NONE);
    gDPSetCombineLERP(gdl_head++,
        0, 0, 0, ENVIRONMENT,
        0, 0, 0, TEXEL0,
        0, 0, 0, ENVIRONMENT,
        0, 0, 0, TEXEL0
    );
}

void setup_f3d_texture(int idx) {
    gDPPipeSync(gdl_head++);


    gDPSetEnvColor(gdl_head++, s2d_red, s2d_green, s2d_blue, s2d_alpha);

    gDPLoadTextureBlock(
        gdl_head++,
        t(s2d_tex[idx])->block.image,
        s(s2d_font)->s.imageFmt,
        G_IM_SIZ_8b,
        s(s2d_font)->s.imageW >> 5,
        s(s2d_font)->s.imageH >> 5,
        s(s2d_font)->s.imagePal,
        G_TX_WRAP,
        G_TX_WRAP,
        G_TX_NOMASK,
        G_TX_NOMASK,
        G_TX_NOLOD,
        G_TX_NOLOD);
    gDPLoadSync(gdl_head++);
}

#define CLAMP_0(x) ((x < 0) ? 0 : x)

void texrect(int x, int y, float scale) {
    #define qs510(n) ((s16)((n)*0x0400))
    gSPScisTextureRectangle (
        gdl_head++,
        x << 2,
        y << 2,
        (u32)(x + ((f32) ((s(s2d_font)->s.imageW >> 5) - 1) * scale)) << 2,
        (u32)(y + ((f32) ((s(s2d_font)->s.imageH >> 5) - 1) * scale)) << 2,
        G_TX_RENDERTILE,
        0,
        0,
        qs510(1.0f / myScale), qs510(1.0f / myScale));
}

void draw_f3d_dropshadow(char c, int x, int y, uObjMtx *ds) {
    setup_f3d_texture(c);

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
        texrect(x, y, myScale);
        gDPPipeSync(gdl_head++);
        gDPSetEnvColor(gdl_head++, s2d_red, s2d_green, s2d_blue, s2d_alpha);
    }
}

void draw_f3d_glyph(char c, int x, int y, uObjMtx *mt) {
    setup_f3d_texture(c);

    texrect(x, y, myScale);
    gDPPipeSync(gdl_head++);
}

#undef s
#undef t
