// Blue Fish

// 0x0301B5C8
static const Lights1 blue_fish_seg3_lights_0301B5C8 = gdSPDefLights1(
    0x3f, 0x3f, 0x3f,
    0xff, 0xff, 0xff, 0x28, 0x28, 0x28
);

// 0x0301B5E0
ALIGNED8 static const Texture blue_fish_seg3_texture_0301B5E0[] = {
#include "actors/blue_fish/blue_fish.rgba16.inc.c"
};

// 0x0301BDE0
static const Vtx blue_fish_seg3_vertex_body[] = {
    {{{  -108,      1,      0}, 0, {    14,    566}, {0x89, 0x00, 0xd7, 0xff}}}, // 0
    {{{   -62,    -19,     12}, 0, {   210,    502}, {0xe8, 0x84, 0x0a, 0xff}}}, // 1
    {{{   -62,     21,     12}, 0, {   210,    502}, {0xe8, 0x7c, 0x0b, 0xff}}}, // 2
    {{{   -47,      1,    -64}, 0, {   274,    830}, {0xbd, 0xff, 0x95, 0xff}}}, // 3
    {{{   -97,      1,     23}, 0, {    62,    462}, {0xa2, 0x00, 0x54, 0xff}}}, // 4
    {{{    55,      1,    127}, 0, {   712,     -8}, {0x1a, 0x00, 0x7c, 0xff}}}, // 5
    {{{    41,     16,     56}, 0, {   656,    298}, {0x43, 0x67, 0x1b, 0xff}}}, // 6
    {{{    50,     16,    -43}, 0, {   692,    722}, {0x35, 0x6f, 0xe6, 0xff}}}, // 7
    {{{    41,    -14,     56}, 0, {   656,    298}, {0x43, 0x99, 0x1b, 0xff}}}, // 8
    {{{    50,    -13,    -43}, 0, {   692,    722}, {0x35, 0x91, 0xe6, 0xff}}}, // 9
    {{{    62,      1,    -92}, 0, {   742,    930}, {0x42, 0xff, 0x94, 0xff}}}, // 10
    {{{   101,      1,      1}, 0, {   910,    520}, {0x7e, 0x00, 0x01, 0xff}}}, // 11
};

// 0x0301BEC0 - 0x0301BFB8
const Gfx blue_fish_seg3_sub_dl_body[] = {
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, blue_fish_seg3_texture_0301B5E0),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsSPLight(&blue_fish_seg3_lights_0301B5C8.l, 1),
    gsSPLight(&blue_fish_seg3_lights_0301B5C8.a, 2),
    gsSPVertex(blue_fish_seg3_vertex_body, 12, 0),
    gsSP2Triangles( 3,  0,  2, 0x0,  1,  4,  0, 0x0),
    gsSP2Triangles( 0,  4,  2, 0x0,  3,  1,  0, 0x0),
    gsSP2Triangles( 2,  4,  5, 0x0,  4,  1,  5, 0x0),
    gsSP2Triangles( 5,  6,  2, 0x0,  7,  2,  6, 0x0),
    gsSP2Triangles( 1,  8,  5, 0x0,  9,  8,  1, 0x0),
    gsSP2Triangles( 1,  3, 10, 0x0, 10,  3,  2, 0x0),
    gsSP2Triangles( 7,  9, 10, 0x0,  2,  7, 10, 0x0),
    gsSP2Triangles(10,  9,  1, 0x0,  6,  5,  8, 0x0),
    gsSP2Triangles(11,  7,  6, 0x0,  7, 11,  9, 0x0),
    gsSP2Triangles( 9, 11,  8, 0x0,  8, 11,  6, 0x0),
    gsSPEndDisplayList(),
};

// 0x0301BFB8 - 0x0301C018
const Gfx blue_fish_seg3_dl_body[] = {
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_MODULATERGB, G_CC_MODULATERGB),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 8, 0, G_TX_RENDERTILE, 0, G_TX_CLAMP, 5, G_TX_NOLOD, G_TX_CLAMP, 5, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (32 - 1) << G_TEXTURE_IMAGE_FRAC, (32 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPDisplayList(blue_fish_seg3_sub_dl_body),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPEndDisplayList(),
};

// 0x0301C018
static const Vtx blue_fish_seg3_vertex_tail[] = {
    {{{    67,      0,    -58}, 0, {   736,    728}, {0x7a, 0x00, 0xde, 0xff}}}, // 0 bottom tip
    {{{    67,      0,    -28}, 0, {   704,    574}, {0x54, 0x7f, 0x14, 0xff}}}, // 1 lower
    {{{     0,      0,      0}, 0, {  1019,    336}, {0xa0, 0xfb, 0x7e, 0xff}}}, // 2 joint
    {{{    53,      0,      0}, 0, {   744,    414}, {0x7e, 0x00, 0xfe, 0xff}}}, // 3 middle
    {{{    67,      0,     27}, 0, {   644,    290}, {0x40, 0x7f, 0xef, 0xff}}}, // 4 upper
    {{{    67,      0,     57}, 0, {   612,    138}, {0x7a, 0x00, 0x22, 0xff}}}, // 5 top tip
};

// 0x0301C0A8 - 0x0301C150
const Gfx blue_fish_seg3_sub_dl_tail[] = {
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, blue_fish_seg3_texture_0301B5E0),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsSPLight(&blue_fish_seg3_lights_0301B5C8.l, 1),
    gsSPLight(&blue_fish_seg3_lights_0301B5C8.a, 2),
    gsSPVertex(blue_fish_seg3_vertex_tail, 6, 0),
    gsSP2Triangles( 2,  4,  3, 0x0,  3,  1,  2, 0x0),
    gsSP2Triangles( 2,  1,  0, 0x0,  2,  5,  4, 0x0),
    gsSPEndDisplayList(),
};

// 0x0301C150 - 0x0301C1B0
const Gfx blue_fish_seg3_dl_tail[] = {
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_MODULATERGB, G_CC_MODULATERGB),
    gsSPClearGeometryMode(G_CULL_BACK),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 8, 0, G_TX_RENDERTILE, 0, G_TX_CLAMP, 5, G_TX_NOLOD, G_TX_CLAMP, 5, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (32 - 1) << G_TEXTURE_IMAGE_FRAC, (32 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPDisplayList(blue_fish_seg3_sub_dl_tail),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPSetGeometryMode(G_CULL_BACK),
    gsSPEndDisplayList(),
};
