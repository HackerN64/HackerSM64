// Burn Smoke

// 0x040217C0
static const Vtx burn_smoke_seg4_vertex[] = {
    {{{   -50,    -50,      0}, 0, {     0,    992}, {0x14, 0x0a, 0x0a, 0xff}}},
    {{{    50,    -50,      0}, 0, {   992,    992}, {0x14, 0x0a, 0x0a, 0xff}}},
    {{{    50,     50,      0}, 0, {   992,      0}, {0x14, 0x0a, 0x0a, 0xff}}},
    {{{   -50,     50,      0}, 0, {     0,      0}, {0x14, 0x0a, 0x0a, 0xff}}},
};

// Wrong texture format. Called as rgba16, which makes the burn smoke appear
// as a transparent black burn smoke. Probably meant to show up as white-ish
// burn smoke, but mistakened for being intended as black smoke.
// 0x04021800
ALIGNED8 static const Texture burn_smoke_seg4_sub_dl_texture[] = {
#include "actors/burn_smoke/burn_smoke.ia16.inc.c"
};

// 0x04022000 - 0x04022028
const Gfx burn_smoke_seg4_sub_dl_begin[] = {
    gsDPPipeSync(),
#ifdef BURN_SMOKE_FIX
    gsDPSetCombineMode(G_CC_MODULATEIFADEA, G_CC_MODULATEIFADEA),
#else
    gsDPSetCombineMode(G_CC_MODULATERGBA, G_CC_MODULATERGBA),
#endif
    gsSPClearGeometryMode(G_LIGHTING),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
    gsSPEndDisplayList(),
};

// 0x04022028 - 0x04022048
const Gfx burn_smoke_seg4_sub_dl_model[] = {
    gsSPVertex(burn_smoke_seg4_vertex, 4, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  0,  2,  3, 0x0),
    gsSPEndDisplayList(),
};

// 0x04022048 - 0x04022070
const Gfx burn_smoke_seg4_sub_dl_end[] = {
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPSetGeometryMode(G_LIGHTING),
    gsSPEndDisplayList(),
};

// 0x04022070 - 0x040220C8
const Gfx burn_smoke_seg4_dl[] = {
    gsSPDisplayList(burn_smoke_seg4_sub_dl_begin),
#ifdef BURN_SMOKE_FIX
    gsDPLoadTextureBlock(burn_smoke_seg4_sub_dl_texture, G_IM_FMT_IA, G_IM_SIZ_16b, 32, 32, 0, G_TX_CLAMP, G_TX_CLAMP, 5, 5, G_TX_NOLOD, G_TX_NOLOD),
#else
    gsDPLoadTextureBlock(burn_smoke_seg4_sub_dl_texture, G_IM_FMT_RGBA, G_IM_SIZ_16b, 32, 32, 0, G_TX_CLAMP, G_TX_CLAMP, 5, 5, G_TX_NOLOD, G_TX_NOLOD),
#endif
    gsSPDisplayList(burn_smoke_seg4_sub_dl_model),
    gsSPDisplayList(burn_smoke_seg4_sub_dl_end),
    gsSPEndDisplayList(),
};
