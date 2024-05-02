// 0x07011758 - 0x07011770

// 0x07011770 - 0x07011788

// 0x07011788 - 0x070117A0

// 0x070117A0 - 0x070117B8

// 0x070117B8 - 0x07011838
static const Vtx ttc_seg7_vertex_070117B8[] = {
    {{{    86,      0,    -81}, 0, {   994,    288}, {0x54, 0xd6, 0xac, 0xff}}},
    {{{   -85,     41,    -81}, 0, {   994,    670}, {0xac, 0x2a, 0xac, 0xff}}},
    {{{    86,     41,    -81}, 0, {   994,    288}, {0x33, 0x67, 0xcd, 0xff}}},
    {{{   -85,      0,    -81}, 0, {   994,    670}, {0xcd, 0x99, 0xcd, 0xff}}},
    {{{   -85,      0,   1336}, 0, {   136,    670}, {0xc7, 0x94, 0xdf, 0xff}}},
    {{{   -85,     41,   1336}, 0, {   136,    670}, {0xfb, 0x73, 0xcc, 0xff}}},
    {{{    86,     41,   1336}, 0, {   136,    288}, {0x39, 0x6c, 0xdf, 0xff}}},
    {{{    86,      0,   1336}, 0, {   136,    288}, {0x05, 0x8d, 0xcc, 0xff}}},
};

// 0x07011838 - 0x070118B8
static const Vtx ttc_seg7_vertex_07011838[] = {
    {{{    86,     41,    -81}, 0, {   994,    288}, {0x33, 0x67, 0xcd, 0xff}}},
    {{{   -85,     41,    -81}, 0, {   994,    670}, {0xac, 0x2a, 0xac, 0xff}}},
    {{{   -85,     41,   1336}, 0, {   136,    670}, {0xfb, 0x73, 0xcc, 0xff}}},
    {{{    86,     41,   1336}, 0, {   136,    288}, {0x39, 0x6c, 0xdf, 0xff}}},
    {{{    86,      0,   1336}, 0, {   136,    288}, {0x05, 0x8d, 0xcc, 0xff}}},
    {{{   -85,      0,    -81}, 0, {   994,    670}, {0xcd, 0x99, 0xcd, 0xff}}},
    {{{    86,      0,    -81}, 0, {   994,    288}, {0x54, 0xd6, 0xac, 0xff}}},
    {{{   -85,      0,   1336}, 0, {   136,    670}, {0xc7, 0x94, 0xdf, 0xff}}},
};

// 0x070118B8 - 0x07011958
static const Vtx ttc_seg7_vertex_070118B8[] = {
    {{{   -85,     41,   1336}, 0, {   136,    670}, {0xfb, 0x73, 0xcc, 0xff}}},
    {{{   -85,      0,   1336}, 0, {   136,    670}, {0xc7, 0x94, 0xdf, 0xff}}},
    {{{  -180,      0,   1295}, 0, {   160,    880}, {0xfd, 0xb3, 0x9c, 0xff}}},
    {{{  -180,     41,   1295}, 0, {   160,    880}, {0x9d, 0x4d, 0x0c, 0xff}}},
    {{{     0,      0,   1577}, 0, {   -10,    478}, {0xe2, 0x94, 0x3a, 0xff}}},
    {{{     0,     41,   1577}, 0, {   -10,    478}, {0x1e, 0x6c, 0x3a, 0xff}}},
    {{{   181,      0,   1295}, 0, {   160,     78}, {0x63, 0xb3, 0x0c, 0xff}}},
    {{{   181,     41,   1295}, 0, {   160,     78}, {0x03, 0x4d, 0x9c, 0xff}}},
    {{{    86,      0,   1336}, 0, {   136,    288}, {0x05, 0x8d, 0xcc, 0xff}}},
    {{{    86,     41,   1336}, 0, {   136,    288}, {0x39, 0x6c, 0xdf, 0xff}}},
};

// 0x07011958 - 0x070119F8
static const Vtx ttc_seg7_vertex_07011958[] = {
    {{{     0,     41,   1577}, 0, {   -10,    478}, {0x1e, 0x6c, 0x3a, 0xff}}},
    {{{    86,     41,   1336}, 0, {   136,    288}, {0x39, 0x6c, 0xdf, 0xff}}},
    {{{   -85,     41,   1336}, 0, {   136,    670}, {0xfb, 0x73, 0xcc, 0xff}}},
    {{{  -180,     41,   1295}, 0, {   160,    880}, {0x9d, 0x4d, 0x0c, 0xff}}},
    {{{   181,     41,   1295}, 0, {   160,     78}, {0x03, 0x4d, 0x9c, 0xff}}},
    {{{     0,      0,   1577}, 0, {   -10,    478}, {0xe2, 0x94, 0x3a, 0xff}}},
    {{{    86,      0,   1336}, 0, {   136,    288}, {0x05, 0x8d, 0xcc, 0xff}}},
    {{{   181,      0,   1295}, 0, {   160,     78}, {0x63, 0xb3, 0x0c, 0xff}}},
    {{{  -180,      0,   1295}, 0, {   160,    880}, {0xfd, 0xb3, 0x9c, 0xff}}},
    {{{   -85,      0,   1336}, 0, {   136,    670}, {0xc7, 0x94, 0xdf, 0xff}}},
};

// 0x070119F8 - 0x07011B38
static const Gfx ttc_seg7_dl_070119F8[] = {
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, machine_09000800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsSPLightColor(LIGHT_1, 0xb48246ff),
    gsSPLightColor(LIGHT_2, 0x5a4123ff),
    gsSPVertex(ttc_seg7_vertex_070117B8, 8, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  0,  3,  1, 0x0),
    gsSP2Triangles( 4,  5,  1, 0x0,  4,  1,  3, 0x0),
    gsSP2Triangles( 0,  2,  6, 0x0,  0,  6,  7, 0x0),
    gsSPLightColor(LIGHT_1, 0xffcc65ff),
    gsSPLightColor(LIGHT_2, 0x7f6632ff),
    gsSPVertex(ttc_seg7_vertex_07011838, 8, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  0,  2,  3, 0x0),
    gsSP2Triangles( 4,  5,  6, 0x0,  4,  7,  5, 0x0),
    gsSPLightColor(LIGHT_1, 0x930000ff),
    gsSPLightColor(LIGHT_2, 0x490000ff),
    gsSPVertex(ttc_seg7_vertex_070118B8, 10, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  0,  2,  3, 0x0),
    gsSP2Triangles( 3,  2,  4, 0x0,  3,  4,  5, 0x0),
    gsSP2Triangles( 5,  4,  6, 0x0,  5,  6,  7, 0x0),
    gsSP2Triangles( 7,  6,  8, 0x0,  7,  8,  9, 0x0),
    gsSPLightColor(LIGHT_1, 0xff0000ff),
    gsSPLightColor(LIGHT_2, 0x7f0000ff),
    gsSPVertex(ttc_seg7_vertex_07011958, 10, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  2,  3,  0, 0x0),
    gsSP2Triangles( 0,  4,  1, 0x0,  5,  6,  7, 0x0),
    gsSP2Triangles( 5,  8,  9, 0x0,  5,  9,  6, 0x0),
    gsSPEndDisplayList(),
};

// 0x07011B38 - 0x07011BE0
const Gfx ttc_seg7_dl_07011B38[] = {
    gsDPPipeSync(),
    gsDPSetCycleType(G_CYC_2CYCLE),
    gsDPSetRenderMode(G_RM_FOG_SHADE_A, G_RM_AA_ZB_OPA_SURF2),
    gsDPSetDepthSource(G_ZS_PIXEL),
    gsDPSetFogColor(200, 255, 255, 255),
    gsSPFogPosition(900, 1000),
    gsSPSetGeometryMode(G_FOG),
    gsDPSetCombineMode(G_CC_MODULATERGB, G_CC_PASS2),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 8, 0, G_TX_RENDERTILE, 0, G_TX_CLAMP, 5, G_TX_NOLOD, G_TX_CLAMP, 5, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (32 - 1) << G_TEXTURE_IMAGE_FRAC, (32 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPDisplayList(ttc_seg7_dl_070119F8),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsDPPipeSync(),
    gsDPSetCycleType(G_CYC_1CYCLE),
    gsDPSetRenderMode(G_RM_AA_ZB_OPA_SURF, G_RM_NOOP2),
    gsSPClearGeometryMode(G_FOG),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPEndDisplayList(),
};
