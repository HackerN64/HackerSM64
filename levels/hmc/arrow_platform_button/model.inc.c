// 0x07022E78 - 0x07022E90

// 0x07022E90 - 0x07022EA8

// 0x07022EA8 - 0x07022EE8
static const Vtx hmc_seg7_vertex_07022EA8[] = {
    {{{    38,     11,     38}, 0, {   990,      0}, {0x00, 0x7f, 0x00, 0xff}}},
    {{{    38,     11,    -37}, 0, {   990,    990}, {0x00, 0x7f, 0x00, 0xff}}},
    {{{   -37,     11,    -37}, 0, {     0,    990}, {0x00, 0x7f, 0x00, 0xff}}},
    {{{   -37,     11,     38}, 0, {     0,      0}, {0x00, 0x7f, 0x00, 0xff}}},
};

// 0x07022EE8 - 0x07022FE8
static const Vtx hmc_seg7_vertex_07022EE8[] = {
    {{{   -50,      0,    -50}, 0, {     0,      0}, {0x00, 0x60, 0xae, 0xff}}},
    {{{   -37,     11,    -37}, 0, {     0,      0}, {0x00, 0x60, 0xae, 0xff}}},
    {{{    51,      0,    -50}, 0, {     0,      0}, {0x00, 0x60, 0xae, 0xff}}},
    {{{    38,     11,    -37}, 0, {     0,      0}, {0x00, 0x60, 0xae, 0xff}}},
    {{{    51,      0,    -50}, 0, {     0,      0}, {0x52, 0x60, 0x00, 0xff}}},
    {{{    38,     11,    -37}, 0, {     0,      0}, {0x52, 0x60, 0x00, 0xff}}},
    {{{    51,      0,     51}, 0, {     0,      0}, {0x52, 0x60, 0x00, 0xff}}},
    {{{    38,     11,     38}, 0, {     0,      0}, {0x52, 0x60, 0x00, 0xff}}},
    {{{   -37,     11,     38}, 0, {     0,      0}, {0xae, 0x60, 0x00, 0xff}}},
    {{{   -37,     11,    -37}, 0, {     0,      0}, {0xae, 0x60, 0x00, 0xff}}},
    {{{   -50,      0,    -50}, 0, {     0,      0}, {0xae, 0x60, 0x00, 0xff}}},
    {{{   -50,      0,     51}, 0, {     0,      0}, {0xae, 0x60, 0x00, 0xff}}},
    {{{    38,     11,     38}, 0, {     0,      0}, {0x00, 0x60, 0x52, 0xff}}},
    {{{   -50,      0,     51}, 0, {     0,      0}, {0x00, 0x60, 0x52, 0xff}}},
    {{{    51,      0,     51}, 0, {     0,      0}, {0x00, 0x60, 0x52, 0xff}}},
    {{{   -37,     11,     38}, 0, {     0,      0}, {0x00, 0x60, 0x52, 0xff}}},
};

// 0x07022FE8 - 0x07023030
static const Gfx hmc_seg7_dl_07022FE8[] = {
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, hmc_seg7_texture_07003800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsSPLightColor(LIGHT_1, 0xffffffff),
    gsSPLightColor(LIGHT_2, 0x797979ff),
    gsSPVertex(hmc_seg7_vertex_07022EA8, 4, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  3,  0,  2, 0x0),
    gsSPEndDisplayList(),
};

// 0x07023030 - 0x07023090
static const Gfx hmc_seg7_dl_07023030[] = {
    gsSPLightColor(LIGHT_1, 0xa8d3c0ff),
    gsSPLightColor(LIGHT_2, 0x4f645bff),
    gsSPVertex(hmc_seg7_vertex_07022EE8, 16, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  1,  3,  2, 0x0),
    gsSP2Triangles( 4,  5,  6, 0x0,  5,  7,  6, 0x0),
    gsSP2Triangles( 8,  9, 10, 0x0,  8, 10, 11, 0x0),
    gsSP2Triangles(12, 13, 14, 0x0, 12, 15, 13, 0x0),
    gsSPEndDisplayList(),
};

// 0x07023090 - 0x07023160
const Gfx hmc_seg7_dl_07023090[] = {
    gsDPPipeSync(),
    gsDPSetCycleType(G_CYC_2CYCLE),
    gsDPSetRenderMode(G_RM_FOG_SHADE_A, G_RM_AA_ZB_OPA_SURF2),
    gsDPSetDepthSource(G_ZS_PIXEL),
    gsDPSetFogColor(0, 0, 0, 255),
    gsSPFogPosition(960, 1000),
    gsSPSetGeometryMode(G_FOG),
    gsDPSetCombineMode(G_CC_MODULATERGB, G_CC_PASS2),
    gsSPClearGeometryMode(G_SHADING_SMOOTH),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 8, 0, G_TX_RENDERTILE, 0, G_TX_CLAMP, 5, G_TX_NOLOD, G_TX_CLAMP, 5, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (32 - 1) << G_TEXTURE_IMAGE_FRAC, (32 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPDisplayList(hmc_seg7_dl_07022FE8),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_PASS2),
    gsSPDisplayList(hmc_seg7_dl_07023030),
    gsDPPipeSync(),
    gsDPSetCycleType(G_CYC_1CYCLE),
    gsDPSetRenderMode(G_RM_AA_ZB_OPA_SURF, G_RM_NOOP2),
    gsSPClearGeometryMode(G_FOG),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPSetGeometryMode(G_SHADING_SMOOTH),
    gsSPEndDisplayList(),
};
