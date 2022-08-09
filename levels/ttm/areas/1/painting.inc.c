#include "game/paintings.h"

// 0x07012308 - 0x07012388
static const Vtx ttm_seg7_vertex_slide_painting[] = {
    {{{     0,      0,      0}, 0, {   -32,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{   614,      0,      0}, 0, {  2012,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{   614,    307,      0}, 0, {  2012,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{     0,    307,      0}, 0, {   -32,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{     0,    307,      0}, 0, {   -32,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{   614,    307,      0}, 0, {  2012,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{   614,    614,      0}, 0, {  2012,    -28}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{     0,    614,      0}, 0, {   -32,    -28}, {0x00, 0x00, 0x7f, 0xff}}},
};

// 0x07012388 - 0x070123A0
const Gfx ttm_seg7_sub_dl_slide_painting_bottom[] = {
    gsSP2Triangles( 0,  1,  2, 0x0,  0,  2,  3, 0x0),
    gsSPEndDisplayList(),
};

// 0x070123A0 - 0x070123B8
const Gfx ttm_seg7_sub_dl_slide_painting_top[] = {
    gsSP2Triangles( 4,  5,  6, 0x0,  4,  6,  7, 0x0),
    gsSPEndDisplayList(),
};

// 0x070123B8 - 0x07012410
const Gfx ttm_seg7_sub_dl_slide_painting_normal_begin[] = {
    gsDPPipeSync(),
    gsSPSetGeometryMode(G_LIGHTING | G_SHADING_SMOOTH),
    gsDPSetCombineMode(G_CC_MODULATERGB, G_CC_MODULATERGB),
    gsSPLightColor(LIGHT_1, 0xffffffff),
    gsSPLightColor(LIGHT_2, 0x505050ff),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 16, 0, G_TX_RENDERTILE, 0, G_TX_CLAMP, 5, G_TX_NOLOD, G_TX_CLAMP, 6, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (64 - 1) << G_TEXTURE_IMAGE_FRAC, (32 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
    gsSPEndDisplayList(),
};

// 0x07012410 - 0x07012430
const Gfx ttm_seg7_sub_dl_slide_painting_normal_end[] = {
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPEndDisplayList(),
};

// 0x07012430 - 0x07012450
static const Gfx ttm_seg7_painting_dl_slide_normal_ripple[] = {
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 16, 0, G_TX_RENDERTILE, 0, G_TX_CLAMP, 5, G_TX_NOLOD, G_TX_CLAMP, 6, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (64 - 1) << G_TEXTURE_IMAGE_FRAC, (32 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPEndDisplayList(),
};

// 0x07012E98 - 0x07012EF8
static const Gfx ttm_seg7_painting_dl_slide_normal[] = {
    gsSPDisplayList(ttm_seg7_sub_dl_slide_painting_normal_begin),
    gsSPVertex(ttm_seg7_vertex_slide_painting, 8, 0),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, ttm_seg7_texture_07004000),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(ttm_seg7_sub_dl_slide_painting_bottom),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, ttm_seg7_texture_07003000),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(ttm_seg7_sub_dl_slide_painting_top),
    gsSPDisplayList(ttm_seg7_sub_dl_slide_painting_normal_end),
    gsSPEndDisplayList(),
};

// 0x07012EF8 - 0x07012F78
ALIGNED8 static const Texture *const ttm_seg7_painting_textures_slide[] = {
    ttm_seg7_texture_07004000,
    ttm_seg7_texture_07003000,
};

// 0x07012F00 (PaintingData)
const struct Painting ttm_slide_painting = {
    /* id */ PAINTING_ID_TTM_SLIDE,
    /* Image Count */ 2,
    /* Texture Type */ PAINTING_IMAGE,
    /*                      passive     entry */
    /* Ripple Magnitude */    20.0f,    80.0f,
    /* Ripple Decay */      0.9608f,  0.9524f,
    /* Ripple Rate */         0.24f,    0.14f,
    /* Ripple Dispersion */   40.0f,    30.0f,
    /* Normal DList */ ttm_seg7_painting_dl_slide_normal,
    /* Textures */     ttm_seg7_painting_textures_slide,
    /* Texture w, h */ 64, 32,
    /* Ripple DList */ ttm_seg7_painting_dl_slide_normal_ripple,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Alpha */ 0xFF,
    /* Unused*/ 0,
    /* Size */  460.8f, 460.8f,
};
