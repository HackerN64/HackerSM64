// Breakable Box

// 0x08011A78
static const Lights1 breakable_box_seg8_lights_default = gdSPDefLights1(
    0x3f, 0x3f, 0x3f,
    0xff, 0xff, 0xff, 0x28, 0x28, 0x28
);

// 0x08011A90
ALIGNED8 static const Texture breakable_box_seg8_texture_08011A90[] = {
#include "actors/breakable_box/crazy_box_surface.rgba16.inc.c"
};

// 0x08012290
ALIGNED8 static const Texture breakable_box_seg8_texture_08012290[] = {
#include "actors/breakable_box/cork_box_surface.rgba16.inc.c"
};

// 0x08012A90
static const Vtx breakable_box_seg8_vertex[] = {
    {{{  -100,      0,   -100}, 0, {   992,      0}, {0x81, 0x00, 0x00, 0xff}}},
    {{{  -100,      0,    100}, 0, {   992,    992}, {0x81, 0x00, 0x00, 0xff}}},
    {{{  -100,    200,    100}, 0, {     0,    992}, {0x81, 0x00, 0x00, 0xff}}},
    {{{  -100,      0,    100}, 0, {     0,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{   100,      0,    100}, 0, {   992,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{   100,    200,    100}, 0, {   992,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{  -100,    200,    100}, 0, {     0,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{   100,      0,   -100}, 0, {   992,      0}, {0x00, 0x81, 0x00, 0xff}}},
    {{{   100,      0,    100}, 0, {     0,      0}, {0x00, 0x81, 0x00, 0xff}}},
    {{{  -100,      0,    100}, 0, {     0,    992}, {0x00, 0x81, 0x00, 0xff}}},
    {{{  -100,      0,   -100}, 0, {   992,    992}, {0x00, 0x81, 0x00, 0xff}}},
    {{{   100,    200,   -100}, 0, {     0,      0}, {0x7f, 0x00, 0x00, 0xff}}},
    {{{   100,    200,    100}, 0, {     0,    992}, {0x7f, 0x00, 0x00, 0xff}}},
    {{{   100,      0,    100}, 0, {   992,    992}, {0x7f, 0x00, 0x00, 0xff}}},
    {{{   100,      0,   -100}, 0, {   992,      0}, {0x7f, 0x00, 0x00, 0xff}}},
    {{{  -100,    200,    100}, 0, {     0,      0}, {0x00, 0x7f, 0x00, 0xff}}},
    {{{   100,    200,   -100}, 0, {   992,    992}, {0x00, 0x7f, 0x00, 0xff}}},
    {{{  -100,    200,   -100}, 0, {   992,      0}, {0x00, 0x7f, 0x00, 0xff}}},
    {{{   100,    200,    100}, 0, {     0,    992}, {0x00, 0x7f, 0x00, 0xff}}},
    {{{   100,    200,   -100}, 0, {     0,      0}, {0x00, 0x00, 0x81, 0xff}}},
    {{{   100,      0,   -100}, 0, {     0,    992}, {0x00, 0x00, 0x81, 0xff}}},
    {{{  -100,      0,   -100}, 0, {   992,    992}, {0x00, 0x00, 0x81, 0xff}}},
    {{{  -100,    200,   -100}, 0, {   992,      0}, {0x00, 0x00, 0x81, 0xff}}},
    {{{  -100,      0,   -100}, 0, {   992,      0}, {0x81, 0x00, 0x00, 0xff}}},
    {{{  -100,    200,    100}, 0, {     0,    992}, {0x81, 0x00, 0x00, 0xff}}},
    {{{  -100,    200,   -100}, 0, {     0,      0}, {0x81, 0x00, 0x00, 0xff}}},
};

// 0x08012C30 - 0x08012CD8
const Gfx breakable_box_seg8_sub_dl_end[] = {
    gsSPLight(&breakable_box_seg8_lights_default.l, 1),
    gsSPLight(&breakable_box_seg8_lights_default.a, 2),
    gsSPVertex(breakable_box_seg8_vertex, 26, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  3,  4,  5, 0x0),
    gsSP2Triangles( 3,  5,  6, 0x0,  7,  8,  9, 0x0),
    gsSP2Triangles( 7,  9, 10, 0x0, 11, 12, 13, 0x0),
    gsSP2Triangles(11, 13, 14, 0x0, 15, 16, 17, 0x0),
    gsSP2Triangles(15, 18, 16, 0x0, 19, 20, 21, 0x0),
    gsSP2Triangles(19, 21, 22, 0x0, 23, 24, 25, 0x0),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPSetGeometryMode(G_SHADING_SMOOTH),
    gsSPEndDisplayList(),
};

// 0x08012CD8 - 0x08012D20
const Gfx breakable_box_seg8_sub_dl_begin[] = {
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_MODULATERGBFADE, G_CC_MODULATERGBFADE),
    gsSPClearGeometryMode(G_SHADING_SMOOTH),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 8, 0, G_TX_RENDERTILE, 0, G_TX_CLAMP, 5, G_TX_NOLOD, G_TX_CLAMP, 5, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (32 - 1) << G_TEXTURE_IMAGE_FRAC, (32 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPEndDisplayList(),
};

// 0x08012D20 - 0x08012D48
const Gfx breakable_box_seg8_dl_crazy_box[] = {
    gsSPDisplayList(breakable_box_seg8_sub_dl_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, breakable_box_seg8_texture_08011A90),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsSPBranchList(breakable_box_seg8_sub_dl_end),
};

// 0x08012D20 - 0x08012D70
const Gfx breakable_box_seg8_dl_cork_box[] = {
    gsSPDisplayList(breakable_box_seg8_sub_dl_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, breakable_box_seg8_texture_08012290),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsSPBranchList(breakable_box_seg8_sub_dl_end),
};
