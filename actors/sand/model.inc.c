// Sand

// 0x0302BA90
static const Vtx sand_seg3_vertex_billboard_16x16[] = {
    {{{    -8,     -8,      0}, 0, {     0,    480}, {0xff, 0xff, 0xff, 0xff}}},
    {{{     8,     -8,      0}, 0, {   480,    480}, {0xff, 0xff, 0xff, 0xff}}},
    {{{     8,      8,      0}, 0, {   480,      0}, {0xff, 0xff, 0xff, 0xff}}},
    {{{    -8,      8,      0}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xff}}},
};

// 0x0302BAD0
ALIGNED8 static const Texture sand_seg3_texture_particle[] = {
#include "actors/sand/sand_particle.rgba16.inc.c"
};

// 0x0302BCD0 - 0x0302BD60
const Gfx sand_seg3_dl_particle[] = {
    gsDPPipeSync(),
    gsSPClearGeometryMode(G_LIGHTING),
    gsDPSetCombineMode(G_CC_DECALRGBA, G_CC_DECALRGBA),
    gsDPLoadTextureBlock(sand_seg3_texture_particle, G_IM_FMT_RGBA, G_IM_SIZ_16b, 16, 16, 0, G_TX_CLAMP, G_TX_CLAMP, 4, 4, G_TX_NOLOD, G_TX_NOLOD),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
    gsSPVertex(sand_seg3_vertex_billboard_16x16, 4, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  0,  2,  3, 0x0),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPSetGeometryMode(G_LIGHTING),
    gsSPEndDisplayList(),
};
