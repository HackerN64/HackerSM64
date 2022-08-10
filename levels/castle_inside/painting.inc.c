#include "game/paintings.h"

// 0x07021818 - 0x07021898
static const Vtx inside_castle_seg7_vertex_painting_textured[] = {
    {{{            0,                    0,      0}, 0, {   -32,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{PAINTING_SIZE,                    0,      0}, 0, {  2012,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{PAINTING_SIZE,  (PAINTING_SIZE / 2),      0}, 0, {  2012,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{            0,  (PAINTING_SIZE / 2),      0}, 0, {   -32,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{            0,  (PAINTING_SIZE / 2),      0}, 0, {   -32,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{PAINTING_SIZE,  (PAINTING_SIZE / 2),      0}, 0, {  2012,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{PAINTING_SIZE,        PAINTING_SIZE,      0}, 0, {  2012,    -28}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{            0,        PAINTING_SIZE,      0}, 0, {   -32,    -28}, {0x00, 0x00, 0x7f, 0xff}}},
};

// 0x07021898 - 0x07021918
static const Vtx inside_castle_seg7_vertex_painting_sl[] = {
    {{{            0,                    0,      0}, 0, {   -32,    992}, {0xdd, 0xdd, 0xdd, 0xff}}},
    {{{PAINTING_SIZE,                    0,      0}, 0, {  2012,    992}, {0xdd, 0xdd, 0xdd, 0xff}}},
    {{{PAINTING_SIZE,  (PAINTING_SIZE / 2),      0}, 0, {  2012,      0}, {0xdd, 0xdd, 0xdd, 0xff}}},
    {{{            0,  (PAINTING_SIZE / 2),      0}, 0, {   -32,      0}, {0xdd, 0xdd, 0xdd, 0xff}}},
    {{{            0,  (PAINTING_SIZE / 2),      0}, 0, {   -32,    992}, {0xdd, 0xdd, 0xdd, 0xff}}},
    {{{PAINTING_SIZE,  (PAINTING_SIZE / 2),      0}, 0, {  2012,    992}, {0xdd, 0xdd, 0xdd, 0xff}}},
    {{{PAINTING_SIZE,        PAINTING_SIZE,      0}, 0, {  2012,    -28}, {0xdd, 0xdd, 0xdd, 0xff}}},
    {{{            0,        PAINTING_SIZE,      0}, 0, {   -32,    -28}, {0xdd, 0xdd, 0xdd, 0xff}}},
};

// 0x07021918 - 0x07021998
static const Vtx inside_castle_seg7_vertex_painting_ccm_fake_1[] = {
    {{{        -3046, -(PAINTING_SIZE / 2),  -3724}, 0, {   -32,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{        -2742, -(PAINTING_SIZE / 2),  -4258}, 0, {  2012,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{        -2742,                    0,  -4258}, 0, {  2012,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{        -3046,                    0,  -3724}, 0, {   -32,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{        -3046,                    0,  -3724}, 0, {   -32,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{        -2742,                    0,  -4258}, 0, {  2012,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{        -2742,  (PAINTING_SIZE / 2),  -4258}, 0, {  2012,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{        -3046,  (PAINTING_SIZE / 2),  -3724}, 0, {   -32,      0}, {0x00, 0x00, 0x7f, 0xff}}},
};

// 0x07021998 - 0x07021A18
static const Vtx inside_castle_seg7_vertex_painting_ccm_fake_2[] = {
    {{{        -1866, -(PAINTING_SIZE / 2),  -4258}, 0, {   -32,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{        -1562, -(PAINTING_SIZE / 2),  -3724}, 0, {  2012,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{        -1562,                    0,  -3724}, 0, {  2012,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{        -1866,                    0,  -4258}, 0, {   -32,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{        -1866,                    0,  -4258}, 0, {   -32,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{        -1562,                    0,  -3724}, 0, {  2012,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{        -1562,  (PAINTING_SIZE / 2),  -3724}, 0, {  2012,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{        -1866,  (PAINTING_SIZE / 2),  -4258}, 0, {   -32,      0}, {0x00, 0x00, 0x7f, 0xff}}},
};

// 0x07021A18 - 0x07021A30
static const Gfx inside_castle_seg7_sub_dl_painting_bottom[] = {
    gsSP2Triangles( 0,  1,  2, 0x0,  0,  2,  3, 0x0),
    gsSPEndDisplayList(),
};

// 0x07021A30 - 0x07021A48
static const Gfx inside_castle_seg7_sub_dl_painting_top[] = {
    gsSP2Triangles( 4,  5,  6, 0x0,  4,  6,  7, 0x0),
    gsSPEndDisplayList(),
};

// 0x07021A48 - 0x07021AA0
static const Gfx inside_castle_seg7_dl_painting_texture_begin[] = {
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

// 0x07021AA0 - 0x07021AC0
static const Gfx inside_castle_seg7_dl_painting_texture_end[] = {
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPEndDisplayList(),
};

// 0x07022598 - 0x070225D8
static const Vtx inside_castle_seg7_vertex_hmc[] = {
    {{{            0,             0,      0}, 0, {     0,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{PAINTING_SIZE,             0,      0}, 0, {     0,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{PAINTING_SIZE, PAINTING_SIZE,      0}, 0, {     0,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{            0, PAINTING_SIZE,      0}, 0, {     0,      0}, {0x00, 0x00, 0x7f, 0xff}}},
};

// 0x07023050 - 0x070230B0
static const Gfx inside_castle_seg7_painting_dl_bob[] = {
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_begin),
    gsSPVertex(inside_castle_seg7_vertex_painting_textured, 8, 0),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0700B800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_bottom),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0700A800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_top),
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_end),
    gsSPEndDisplayList(),
};

// 0x070230B0 - 0x07023110
static const Gfx inside_castle_seg7_painting_dl_ccm[] = {
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_begin),
    gsSPVertex(inside_castle_seg7_vertex_painting_textured, 8, 0),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0700D800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_bottom),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0700C800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_top),
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_end),
    gsSPEndDisplayList(),
};

// 0x07023110 - 0x07023170
static const Gfx inside_castle_seg7_painting_dl_wf[] = {
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_begin),
    gsSPVertex(inside_castle_seg7_vertex_painting_textured, 8, 0),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0700F800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_bottom),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0700E800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_top),
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_end),
    gsSPEndDisplayList(),
};

// 0x07023170 - 0x070231D0
static const Gfx inside_castle_seg7_painting_dl_jrb[] = {
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_begin),
    gsSPVertex(inside_castle_seg7_vertex_painting_textured, 8, 0),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_07011800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_bottom),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_07010800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_top),
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_end),
    gsSPEndDisplayList(),
};

// 0x070231D0 - 0x07023230
static const Gfx inside_castle_seg7_painting_dl_lll[] = {
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_begin),
    gsSPVertex(inside_castle_seg7_vertex_painting_textured, 8, 0),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_07012800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_bottom),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_07013800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_top),
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_end),
    gsSPEndDisplayList(),
};

// 0x07023230 - 0x07023290
static const Gfx inside_castle_seg7_painting_dl_ssl[] = {
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_begin),
    gsSPVertex(inside_castle_seg7_vertex_painting_textured, 8, 0),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_07015800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_bottom),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_07014800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_top),
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_end),
    gsSPEndDisplayList(),
};

// 0x07023290 - 0x070232F0
static const Gfx inside_castle_seg7_painting_dl_wdw[] = {
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_begin),
    gsSPVertex(inside_castle_seg7_vertex_painting_textured, 8, 0),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_07018800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_bottom),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_07017800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_top),
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_end),
    gsSPEndDisplayList(),
};

// 0x070232F0 - 0x07023350
static const Gfx inside_castle_seg7_painting_dl_thi[] = {
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_begin),
    gsSPVertex(inside_castle_seg7_vertex_painting_textured, 8, 0),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0701A800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_bottom),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_07019800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_top),
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_end),
    gsSPEndDisplayList(),
};

// 0x07023350 - 0x070233B0
static const Gfx inside_castle_seg7_painting_dl_ttm[] = {
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_begin),
    gsSPVertex(inside_castle_seg7_vertex_painting_textured, 8, 0),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0701C800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_bottom),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0701B800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_top),
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_end),
    gsSPEndDisplayList(),
};

// 0x070233B0 - 0x07023410
static const Gfx inside_castle_seg7_painting_dl_ttc[] = {
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_begin),
    gsSPVertex(inside_castle_seg7_vertex_painting_textured, 8, 0),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0701E800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_bottom),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0701D800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_top),
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_end),
    gsSPEndDisplayList(),
};

// 0x07023410 - 0x070234C0
static const Gfx inside_castle_seg7_painting_dl_sl[] = {
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_MODULATERGB, G_CC_MODULATERGB),
    gsSPClearGeometryMode(G_LIGHTING),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 16, 0, G_TX_RENDERTILE, 0, G_TX_CLAMP, 5, G_TX_NOLOD, G_TX_CLAMP, 6, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (64 - 1) << G_TEXTURE_IMAGE_FRAC, (32 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
    gsSPVertex(inside_castle_seg7_vertex_painting_sl, 8, 0),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_07020800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_bottom),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0701F800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_top),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPSetGeometryMode(G_LIGHTING),
    gsSPEndDisplayList(),
};

// 0x070234C0 - 0x07023520
const Gfx inside_castle_seg7_dl_ccm_fake_painting_1[] = {
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_begin),
    gsSPVertex(inside_castle_seg7_vertex_painting_ccm_fake_1, 8, 0),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0700D800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_bottom),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0700C800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_top),
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_end),
    gsSPEndDisplayList(),
};

// 0x07023520 - 0x07023580
const Gfx inside_castle_seg7_dl_ccm_fake_painting_2[] = {
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_begin),
    gsSPVertex(inside_castle_seg7_vertex_painting_ccm_fake_2, 8, 0),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0700D800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_bottom),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0700C800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_top),
    gsSPDisplayList(inside_castle_seg7_dl_painting_texture_end),
    gsSPEndDisplayList(),
};

// 0x07023580 - 0x070235B8
static const Gfx inside_castle_seg7_painting_dl_hmc[] = {
    gsDPPipeSync(),
    gsSPLightColor(LIGHT_1, 0x6464ffff),
    gsSPLightColor(LIGHT_2, 0x404080ff),
    gsSPVertex(inside_castle_seg7_vertex_hmc, 4, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  0,  2,  3, 0x0),
    gsSPEndDisplayList(),
};

// 0x070235B8 - 0x070235C0
static const Gfx inside_castle_seg7_painting_dl_ddd[] = {
    gsSPBranchList(inside_castle_seg7_painting_dl_hmc),
};

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_bob[] = {
    inside_castle_seg7_texture_0700B800,
    inside_castle_seg7_texture_0700A800,
};

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_ccm[] = {
    inside_castle_seg7_texture_0700D800,
    inside_castle_seg7_texture_0700C800,
};

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_wf[] = {
    inside_castle_seg7_texture_0700F800,
    inside_castle_seg7_texture_0700E800,
};

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_jrb[] = {
    inside_castle_seg7_texture_07011800,
    inside_castle_seg7_texture_07010800,
};

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_lll[] = {
    inside_castle_seg7_texture_07012800,
    inside_castle_seg7_texture_07013800,
};

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_ssl[] = {
    inside_castle_seg7_texture_07015800,
    inside_castle_seg7_texture_07014800,
};

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_hmc_env[] = {
    inside_castle_seg7_texture_07016800,
};

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_ddd_env[] = {
    inside_castle_seg7_texture_07017000,
};

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_wdw[] = {
    inside_castle_seg7_texture_07018800,
    inside_castle_seg7_texture_07017800,
};

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_thi[] = {
    inside_castle_seg7_texture_0701A800,
    inside_castle_seg7_texture_07019800,
};

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_ttm[] = {
    inside_castle_seg7_texture_0701C800,
    inside_castle_seg7_texture_0701B800,
};

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_ttc[] = {
    inside_castle_seg7_texture_0701E800,
    inside_castle_seg7_texture_0701D800,
};

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_sl[] = {
    inside_castle_seg7_texture_07020800,
    inside_castle_seg7_texture_0701F800,
};

// 0x07023620 - 0x07023698
const struct Painting bob_painting = {
    /* ID */ PAINTING_ID_CASTLE_BOB,
    /* Image Count */ 2,
    /* Alpha */ 0xFF,

    /* Texture Type */ PAINTING_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Ripple Animation */ RIPPLE_ANIM_PROXIMITY,
    /* Normal DList */ inside_castle_seg7_painting_dl_bob,
    /* Textures */     inside_castle_seg7_painting_textures_bob,
    /* Texture w, h */ 64, 32,    /* Size */  614.0f, 614.0f,
};

// 0x07023698 - 0x07023710
const struct Painting ccm_painting = {
    /* ID */ PAINTING_ID_CASTLE_CCM,
    /* Image Count */ 2,
    /* Alpha */ 0xFF,
    /* Texture Type */ PAINTING_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Ripple Animation */ RIPPLE_ANIM_PROXIMITY,
    /* Normal DList */ inside_castle_seg7_painting_dl_ccm,
    /* Textures */     inside_castle_seg7_painting_textures_ccm,
    /* Texture w, h */ 64, 32,
    /* Size */  614.0f, 614.0f,
};

// 0x07023710 - 0x07023788
const struct Painting wf_painting = {
    /* ID */ PAINTING_ID_CASTLE_WF,
    /* Image Count */ 2,
    /* Alpha */ 0xFF,
    /* Texture Type */ PAINTING_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Ripple Animation */ RIPPLE_ANIM_PROXIMITY,
    /* Normal DList */ inside_castle_seg7_painting_dl_wf,
    /* Textures */     inside_castle_seg7_painting_textures_wf,
    /* Texture w, h */ 64, 32,
    /* Size */  614.0f, 614.0f,
};

// 0x07023788 - 0x07023800
const struct Painting jrb_painting = {
    /* ID */ PAINTING_ID_CASTLE_JRB,
    /* Image Count */ 2,
    /* Alpha */ 0xFF,
    /* Texture Type */ PAINTING_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Ripple Animation */ RIPPLE_ANIM_PROXIMITY,
    /* Normal DList */ inside_castle_seg7_painting_dl_jrb,
    /* Textures */     inside_castle_seg7_painting_textures_jrb,
    /* Texture w, h */ 64, 32,
    /* Size */  614.0f, 614.0f,
};

// 0x07023800 - 0x07023878
const struct Painting lll_painting = {
    /* ID */ PAINTING_ID_CASTLE_LLL,
    /* Image Count */ 2,
    /* Alpha */ 0xFF,
    /* Texture Type */ PAINTING_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Ripple Animation */ RIPPLE_ANIM_PROXIMITY,
    /* Normal DList */ inside_castle_seg7_painting_dl_lll,
    /* Textures */     inside_castle_seg7_painting_textures_lll,
    /* Texture w, h */ 64, 32,
    /* Size */  614.0f, 614.0f,
};

// 0x07023878 - 0x070238F0
const struct Painting ssl_painting = {
    /* ID */ PAINTING_ID_CASTLE_SSL,
    /* Image Count */ 2,
    /* Alpha */ 0xFF,
    /* Texture Type */ PAINTING_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Ripple Animation */ RIPPLE_ANIM_PROXIMITY,
    /* Normal DList */ inside_castle_seg7_painting_dl_ssl,
    /* Textures */     inside_castle_seg7_painting_textures_ssl,
    /* Texture w, h */ 64, 32,
    /* Size */  614.0f, 614.0f,
};

// 0x070238F0 - 0x07023968
const struct Painting hmc_painting = {
    /* ID */ PAINTING_ID_CASTLE_HMC,
    /* Image Count */ 1,
    /* Alpha */ 0xFF,
    /* Texture Type */ PAINTING_ENV_MAP,
    /* Ripple Trigger */ RIPPLE_TRIGGER_CONTINUOUS,
    /* Ripple Animation */ RIPPLE_ANIM_CONTINUOUS,
    /* Normal DList */ inside_castle_seg7_painting_dl_hmc,
    /* Textures */     inside_castle_seg7_painting_textures_hmc_env,
    /* Texture w, h */ 32, 32,
    /* Size */  768.0f, 768.0f,
};

// 0x07023968 - 0x070239E0
const struct Painting ddd_painting = {
    /* ID */ PAINTING_ID_CASTLE_DDD,
    /* Image Count */ 1,
    /* Alpha */ 0xB4,
    /* Texture Type */ PAINTING_ENV_MAP,
    /* Ripple Trigger */ RIPPLE_TRIGGER_CONTINUOUS,
    /* Ripple Animation */ RIPPLE_ANIM_CONTINUOUS,
    /* Normal DList */ inside_castle_seg7_painting_dl_ddd,
    /* Textures */     inside_castle_seg7_painting_textures_ddd_env,
    /* Texture w, h */ 32, 32,
    /* Size */  819.2f, 819.2f,
};

// 0x070239E0 - 0x07023A58
const struct Painting wdw_painting = {
    /* ID */ PAINTING_ID_CASTLE_WDW,
    /* Image Count */ 2,
    /* Alpha */ 0xFF,
    /* Texture Type */ PAINTING_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Ripple Animation */ RIPPLE_ANIM_PROXIMITY,
    /* Normal DList */ inside_castle_seg7_painting_dl_wdw,
    /* Textures */     inside_castle_seg7_painting_textures_wdw,
    /* Texture w, h */ 64, 32,
    /* Size */  614.0f, 614.0f,
};

// 0x07023A58 - 0x07023AD0
const struct Painting thi_tiny_painting = {
    /* ID */ PAINTING_ID_CASTLE_THI_TINY,
    /* Image Count */ 2,
    /* Alpha */ 0xFF,
    /* Texture Type */ PAINTING_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Ripple Animation */ RIPPLE_ANIM_PROXIMITY,
    /* Normal DList */ inside_castle_seg7_painting_dl_thi,
    /* Textures */     inside_castle_seg7_painting_textures_thi,
    /* Texture w, h */ 64, 32,
    /* Size */  393.216f, 393.216f,
};

// 0x07023AD0 - 0x07023B48
const struct Painting ttm_painting = {
    /* ID */ PAINTING_ID_CASTLE_TTM,
    /* Image Count */ 2,
    /* Alpha */ 0xFF,
    /* Texture Type */ PAINTING_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Ripple Animation */ RIPPLE_ANIM_PROXIMITY,
    /* Normal DList */ inside_castle_seg7_painting_dl_ttm,
    /* Textures */     inside_castle_seg7_painting_textures_ttm,
    /* Texture w, h */ 64, 32,
    /* Size */  256.0f, 256.0f,
};

// 0x07023B48 - 0x07023BC0
const struct Painting ttc_painting = {
    /* ID */ PAINTING_ID_CASTLE_TTC,
    /* Image Count */ 2,
    /* Alpha */ 0xFF,
    /* Texture Type */ PAINTING_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Ripple Animation */ RIPPLE_ANIM_PROXIMITY,
    /* Normal DList */ inside_castle_seg7_painting_dl_ttc,
    /* Textures */     inside_castle_seg7_painting_textures_ttc,
    /* Texture w, h */ 64, 32,
    /* Size */  409.6f, 409.6f,
};

// 0x07023BC0 - 0x07023C38
const struct Painting sl_painting = {
    /* ID */ PAINTING_ID_CASTLE_SL,
    /* Image Count */ 2,
    /* Alpha */ 0xFF,
    /* Texture Type */ PAINTING_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Ripple Animation */ RIPPLE_ANIM_PROXIMITY,
    /* Normal DList */ inside_castle_seg7_painting_dl_sl,
    /* Textures */     inside_castle_seg7_painting_textures_sl,
    /* Texture w, h */ 64, 32,
    /* Size */  716.8f, 716.8f,
};

// 0x07023C38 - 0x07023CB0
const struct Painting thi_huge_painting = {
    /* ID */ PAINTING_ID_CASTLE_THI_HUGE,
    /* Image Count */ 2,
    /* Alpha */ 0xFF,
    /* Texture Type */ PAINTING_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Ripple Animation */ RIPPLE_ANIM_PROXIMITY_LARGE,
    /* Normal DList */ inside_castle_seg7_painting_dl_thi,
    /* Textures */     inside_castle_seg7_painting_textures_thi,
    /* Texture w, h */ 64, 32,
    /* Size */  1638.4f, 1638.4f,
};

const struct Painting rr_painting = {
    /* ID */ PAINTING_ID_CASTLE_THI_HUGE,
    /* Image Count */ 0,
    /* Alpha */ 0x00,
    /* Texture Type */ PAINTING_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Ripple Animation */ RIPPLE_ANIM_PROXIMITY,
    /* Normal DList */ NULL,
    /* Textures */     NULL,
    /* Texture w, h */  0,  0,
    /* Size */  204.8f, 204.8f,
};
