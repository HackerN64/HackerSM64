// Small Water splash

// 0x040220C8
static const Vtx small_water_splash_seg4_vertex[] = {
    {{{   -32,      0,     32}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xc8}}},
    {{{    32,      0,     32}, 0, { 62<<5,      0}, {0xff, 0xff, 0xff, 0xc8}}},
    {{{    32,      0,    -32}, 0, { 62<<5,  62<<5}, {0xff, 0xff, 0xff, 0xc8}}},
    {{{   -32,      0,    -32}, 0, {     0,  62<<5}, {0xff, 0xff, 0xff, 0xc8}}},
};

// 0x04022148
ALIGNED8 static const Texture small_water_splash_seg4_texture_0[] = {
#include "actors/small_water_splash/small_water_splash_0.ia16.inc.c"
};

// 0x04022948
ALIGNED8 static const Texture small_water_splash_seg4_texture_1[] = {
#include "actors/small_water_splash/small_water_splash_1.ia16.inc.c"
};

// 0x04023148
ALIGNED8 static const Texture small_water_splash_seg4_texture_2[] = {
#include "actors/small_water_splash/small_water_splash_2.ia16.inc.c"
};

// 0x04023948
ALIGNED8 static const Texture small_water_splash_seg4_texture_3[] = {
#include "actors/small_water_splash/small_water_splash_3.ia16.inc.c"
};

// 0x04024148
ALIGNED8 static const Texture small_water_splash_seg4_texture_4[] = {
#include "actors/small_water_splash/small_water_splash_4.ia16.inc.c"
};

// 0x04024948
ALIGNED8 static const Texture small_water_splash_seg4_texture_5[] = {
#include "actors/small_water_splash/small_water_splash_5.ia16.inc.c"
};

// 0x04025148 - 0x04025190
const Gfx small_water_splash_seg4_sub_dl_start[] = {
    gsSPClearGeometryMode(G_LIGHTING),
    gsDPSetCombineMode(G_CC_MODULATEIA, G_CC_MODULATEIA),
    gsSPTexture(0x8000, 0x8000, 0, G_TX_RENDERTILE, G_ON),
    gsDPSetTile(G_IM_FMT_IA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_CLAMP, 5, G_TX_NOLOD, G_TX_CLAMP, 5, G_TX_NOLOD),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsDPSetTile(G_IM_FMT_IA, G_IM_SIZ_16b, 8, 0, G_TX_RENDERTILE, 0, G_TX_CLAMP, 5, G_TX_NOLOD, G_TX_CLAMP, 5, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (32 - 1) << G_TEXTURE_IMAGE_FRAC, (32 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPEndDisplayList(),
};

// 0x04025190 - 0x040251C8
const Gfx small_water_splash_seg4_sub_dl_end[] = {
    gsSP2Triangles( 0,  1,  2, 0x0,  0,  2,  3, 0x0),
    gsDPPipeSync(),
    gsSPTexture(0x0001, 0x0001, 0, G_TX_RENDERTILE, G_OFF),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPSetGeometryMode(G_LIGHTING),
    gsSPEndDisplayList(),
};

// 0x040251C8 - 0x040251E0
const Gfx small_water_splash_seg4_sub_dl_model[] = {
    gsSPDisplayList(small_water_splash_seg4_sub_dl_start),
    gsSPVertex(small_water_splash_seg4_vertex, 4, 0),
    gsSPBranchList(small_water_splash_seg4_sub_dl_end),
};

// 0x040251F8 - 0x04025210
const Gfx small_water_splash_seg4_dl_0[] = {
    gsDPPipeSync(),
    gsDPSetTextureImage(G_IM_FMT_IA, G_IM_SIZ_16b, 1, small_water_splash_seg4_texture_0),
    gsSPBranchList(small_water_splash_seg4_sub_dl_model),
};

// 0x04025210 - 0x04025228
const Gfx small_water_splash_seg4_dl_1[] = {
    gsDPPipeSync(),
    gsDPSetTextureImage(G_IM_FMT_IA, G_IM_SIZ_16b, 1, small_water_splash_seg4_texture_1),
    gsSPBranchList(small_water_splash_seg4_sub_dl_model),
};

// 0x04025228 - 0x04025240
const Gfx small_water_splash_seg4_dl_2[] = {
    gsDPPipeSync(),
    gsDPSetTextureImage(G_IM_FMT_IA, G_IM_SIZ_16b, 1, small_water_splash_seg4_texture_2),
    gsSPBranchList(small_water_splash_seg4_sub_dl_model),
};

// 0x04025240 - 0x04025258
const Gfx small_water_splash_seg4_dl_3[] = {
    gsDPPipeSync(),
    gsDPSetTextureImage(G_IM_FMT_IA, G_IM_SIZ_16b, 1, small_water_splash_seg4_texture_3),
    gsSPBranchList(small_water_splash_seg4_sub_dl_model),
};

// 0x04025258 - 0x04025270
const Gfx small_water_splash_seg4_dl_4[] = {
    gsDPPipeSync(),
    gsDPSetTextureImage(G_IM_FMT_IA, G_IM_SIZ_16b, 1, small_water_splash_seg4_texture_4),
    gsSPBranchList(small_water_splash_seg4_sub_dl_model),
};

// 0x04025270 - 0x04025288
const Gfx small_water_splash_seg4_dl_5[] = {
    gsDPPipeSync(),
    gsDPSetTextureImage(G_IM_FMT_IA, G_IM_SIZ_16b, 1, small_water_splash_seg4_texture_5),
    gsSPBranchList(small_water_splash_seg4_sub_dl_model),
};
