#include "game/paintings.h"

#define CCM_FAKE_1_X -3046
#define CCM_FAKE_1_Z -3724
#define CCM_FAKE_1_DX  304 // PAINTING_SIZE * sin(yaw)
#define CCM_FAKE_1_DZ -534 // PAINTING_SIZE * cos(yaw)

// 0x07021918 - 0x07021998
static const Vtx inside_castle_seg7_vertex_painting_ccm_fake_1[] = {
    {{{(CCM_FAKE_1_X +             0), -(PAINTING_SIZE / 2), (CCM_FAKE_1_Z +             0)}, 0, {   -32,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_1_X + CCM_FAKE_1_DX), -(PAINTING_SIZE / 2), (CCM_FAKE_1_Z + CCM_FAKE_1_DZ)}, 0, {  2012,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_1_X + CCM_FAKE_1_DX),                    0, (CCM_FAKE_1_Z + CCM_FAKE_1_DZ)}, 0, {  2012,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_1_X +             0),                    0, (CCM_FAKE_1_Z +             0)}, 0, {   -32,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_1_X +             0),                    0, (CCM_FAKE_1_Z +             0)}, 0, {   -32,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_1_X + CCM_FAKE_1_DX),                    0, (CCM_FAKE_1_Z + CCM_FAKE_1_DZ)}, 0, {  2012,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_1_X + CCM_FAKE_1_DX),  (PAINTING_SIZE / 2), (CCM_FAKE_1_Z + CCM_FAKE_1_DZ)}, 0, {  2012,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_1_X +             0),  (PAINTING_SIZE / 2), (CCM_FAKE_1_Z +             0)}, 0, {   -32,      0}, {0x00, 0x00, 0x7f, 0xff}}},
};

#define CCM_FAKE_2_X -1866
#define CCM_FAKE_2_Z -4258
#define CCM_FAKE_2_DX  304 // PAINTING_SIZE * sin(yaw)
#define CCM_FAKE_2_DZ  534 // PAINTING_SIZE * cos(yaw)

// 0x07021998 - 0x07021A18
static const Vtx inside_castle_seg7_vertex_painting_ccm_fake_2[] = {
    {{{(CCM_FAKE_2_X +             0), -(PAINTING_SIZE / 2), (CCM_FAKE_2_Z +             0)}, 0, {   -32,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_2_X + CCM_FAKE_2_DX), -(PAINTING_SIZE / 2), (CCM_FAKE_2_Z + CCM_FAKE_2_DZ)}, 0, {  2012,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_2_X + CCM_FAKE_2_DX),                    0, (CCM_FAKE_2_Z + CCM_FAKE_2_DZ)}, 0, {  2012,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_2_X +             0),                    0, (CCM_FAKE_2_Z +             0)}, 0, {   -32,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_2_X +             0),                    0, (CCM_FAKE_2_Z +             0)}, 0, {   -32,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_2_X + CCM_FAKE_2_DX),                    0, (CCM_FAKE_2_Z + CCM_FAKE_2_DZ)}, 0, {  2012,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_2_X + CCM_FAKE_2_DX),  (PAINTING_SIZE / 2), (CCM_FAKE_2_Z + CCM_FAKE_2_DZ)}, 0, {  2012,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_2_X +             0),  (PAINTING_SIZE / 2), (CCM_FAKE_2_Z +             0)}, 0, {   -32,      0}, {0x00, 0x00, 0x7f, 0xff}}},
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
static const Gfx inside_castle_seg7_dl_painting_textured_shaded_begin[] = {
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
static const Gfx inside_castle_seg7_dl_painting_textured_shaded_end[] = {
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPEndDisplayList(),
};

// 0x070234C0 - 0x07023520
const Gfx inside_castle_seg7_dl_ccm_fake_painting_1[] = {
    gsSPDisplayList(inside_castle_seg7_dl_painting_textured_shaded_begin),
    gsSPVertex(inside_castle_seg7_vertex_painting_ccm_fake_1, 8, 0),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0700D800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_bottom),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0700C800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_top),
    gsSPDisplayList(inside_castle_seg7_dl_painting_textured_shaded_end),
    gsSPEndDisplayList(),
};

// 0x07023520 - 0x07023580
const Gfx inside_castle_seg7_dl_ccm_fake_painting_2[] = {
    gsSPDisplayList(inside_castle_seg7_dl_painting_textured_shaded_begin),
    gsSPVertex(inside_castle_seg7_vertex_painting_ccm_fake_2, 8, 0),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0700D800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_bottom),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, inside_castle_seg7_texture_0700C800),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(inside_castle_seg7_sub_dl_painting_top),
    gsSPDisplayList(inside_castle_seg7_dl_painting_textured_shaded_end),
    gsSPEndDisplayList(),
};

/// - PAINTING_ID_CASTLE_BOB -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_bob[] = {
    inside_castle_seg7_texture_0700B800,
    inside_castle_seg7_texture_0700A800,
};

// 0x07023620 - 0x07023698
const struct PaintingImage bob_painting = {
    /* Textures */ inside_castle_seg7_painting_textures_bob,
    /* Texture Count */ ARRAY_COUNT(inside_castle_seg7_painting_textures_bob),
    /* Texture w, h */ 64, 32,
    /* Texture Type */ PAINTING_TYPE_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Shaded */ TRUE,
    /* Alpha */ 0xFF,
    /* Size */ 614.0f, 614.0f,
};

/// - PAINTING_ID_CASTLE_CCM -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_ccm[] = {
    inside_castle_seg7_texture_0700D800,
    inside_castle_seg7_texture_0700C800,
};

// 0x07023698 - 0x07023710
const struct PaintingImage ccm_painting = {
    /* Textures */ inside_castle_seg7_painting_textures_ccm,
    /* Texture Count */ ARRAY_COUNT(inside_castle_seg7_painting_textures_ccm),
    /* Texture w, h */ 64, 32,
    /* Texture Type */ PAINTING_TYPE_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Shaded */ TRUE,
    /* Alpha */ 0xFF,
    /* Size */ 614.0f, 614.0f,
};

/// - PAINTING_ID_CASTLE_WF -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_wf[] = {
    inside_castle_seg7_texture_0700F800,
    inside_castle_seg7_texture_0700E800,
};

// 0x07023710 - 0x07023788
const struct PaintingImage wf_painting = {
    /* Textures */ inside_castle_seg7_painting_textures_wf,
    /* Texture Count */ ARRAY_COUNT(inside_castle_seg7_painting_textures_wf),
    /* Texture w, h */ 64, 32,
    /* Texture Type */ PAINTING_TYPE_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Shaded */ TRUE,
    /* Alpha */ 0xFF,
    /* Size */ 614.0f, 614.0f,
};

/// - PAINTING_ID_CASTLE_JRB -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_jrb[] = {
    inside_castle_seg7_texture_07011800,
    inside_castle_seg7_texture_07010800,
};

// 0x07023788 - 0x07023800
const struct PaintingImage jrb_painting = {
    /* Textures */ inside_castle_seg7_painting_textures_jrb,
    /* Texture Count */ ARRAY_COUNT(inside_castle_seg7_painting_textures_jrb),
    /* Texture w, h */ 64, 32,
    /* Texture Type */ PAINTING_TYPE_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Shaded */ TRUE,
    /* Alpha */ 0xFF,
    /* Size */ 614.0f, 614.0f,
};

/// - PAINTING_ID_CASTLE_LLL -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_lll[] = {
    inside_castle_seg7_texture_07012800,
    inside_castle_seg7_texture_07013800,
};

// 0x07023800 - 0x07023878
const struct PaintingImage lll_painting = {
    /* Textures */ inside_castle_seg7_painting_textures_lll,
    /* Texture Count */ ARRAY_COUNT(inside_castle_seg7_painting_textures_lll),
    /* Texture w, h */ 64, 32,
    /* Texture Type */ PAINTING_TYPE_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Shaded */ TRUE,
    /* Alpha */ 0xFF,
    /* Size */ 614.0f, 614.0f,
};

/// - PAINTING_ID_CASTLE_SSL -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_ssl[] = {
    inside_castle_seg7_texture_07015800,
    inside_castle_seg7_texture_07014800,
};

// 0x07023878 - 0x070238F0
const struct PaintingImage ssl_painting = {
    /* Textures */ inside_castle_seg7_painting_textures_ssl,
    /* Texture Count */ ARRAY_COUNT(inside_castle_seg7_painting_textures_ssl),
    /* Texture w, h */ 64, 32,
    /* Texture Type */ PAINTING_TYPE_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Shaded */ TRUE,
    /* Alpha */ 0xFF,
    /* Size */ 614.0f, 614.0f,
};

/// - PAINTING_ID_CASTLE_HMC -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_hmc_env[] = {
    inside_castle_seg7_texture_07016800,
};

// 0x070238F0 - 0x07023968
const struct PaintingImage hmc_painting = {
    /* Textures */ inside_castle_seg7_painting_textures_hmc_env,
    /* Texture Count */ ARRAY_COUNT(inside_castle_seg7_painting_textures_hmc_env),
    /* Texture w, h */ 32, 32,
    /* Texture Type */ PAINTING_TYPE_ENV_MAP,
    /* Ripple Trigger */ RIPPLE_TRIGGER_CONTINUOUS,
    /* Shaded */ TRUE,
    /* Alpha */ 0xFF,
    /* Size */ 768.0f, 768.0f,
};

/// - PAINTING_ID_CASTLE_DDD -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_ddd_env[] = {
    inside_castle_seg7_texture_07017000,
};

// 0x07023968 - 0x070239E0
const struct PaintingImage ddd_painting = {
    /* Textures */ inside_castle_seg7_painting_textures_ddd_env,
    /* Texture Count */ ARRAY_COUNT(inside_castle_seg7_painting_textures_ddd_env),
    /* Texture w, h */ 32, 32,
    /* Texture Type */ PAINTING_TYPE_ENV_MAP,
    /* Ripple Trigger */ RIPPLE_TRIGGER_CONTINUOUS,
    /* Shaded */ TRUE,
    /* Alpha */ 0xB4,
    /* Size */ 819.2f, 819.2f,
};

/// - PAINTING_ID_CASTLE_WDW -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_wdw[] = {
    inside_castle_seg7_texture_07018800,
    inside_castle_seg7_texture_07017800,
};

// 0x070239E0 - 0x07023A58
const struct PaintingImage wdw_painting = {
    /* Textures */ inside_castle_seg7_painting_textures_wdw,
    /* Texture Count */ ARRAY_COUNT(inside_castle_seg7_painting_textures_wdw),
    /* Texture w, h */ 64, 32,
    /* Texture Type */ PAINTING_TYPE_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Shaded */ TRUE,
    /* Alpha */ 0xFF,
    /* Size */ 614.0f, 614.0f,
};

/// - PAINTING_ID_CASTLE_THI_TINY -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_thi[] = {
    inside_castle_seg7_texture_0701A800,
    inside_castle_seg7_texture_07019800,
};

// 0x07023A58 - 0x07023AD0
const struct PaintingImage thi_tiny_painting = {
    /* Textures */ inside_castle_seg7_painting_textures_thi,
    /* Texture Count */ ARRAY_COUNT(inside_castle_seg7_painting_textures_thi),
    /* Texture w, h */ 64, 32,
    /* Texture Type */ PAINTING_TYPE_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Shaded */ TRUE,
    /* Alpha */ 0xFF,
    /* Size */ 393.216f, 393.216f,
};

/// - PAINTING_ID_CASTLE_THI_HUGE -

// 0x07023C38 - 0x07023CB0
const struct PaintingImage thi_huge_painting = {
    /* Textures */ inside_castle_seg7_painting_textures_thi,
    /* Texture Count */ ARRAY_COUNT(inside_castle_seg7_painting_textures_thi),
    /* Texture w, h */ 64, 32,
    /* Texture Type */ PAINTING_TYPE_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Shaded */ TRUE,
    /* Alpha */ 0xFF,
    /* Size */ 1638.4f, 1638.4f,
};

/// - PAINTING_ID_CASTLE_TTM -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_ttm[] = {
    inside_castle_seg7_texture_0701C800,
    inside_castle_seg7_texture_0701B800,
};

// 0x07023AD0 - 0x07023B48
const struct PaintingImage ttm_painting = {
    /* Textures */ inside_castle_seg7_painting_textures_ttm,
    /* Texture Count */ ARRAY_COUNT(inside_castle_seg7_painting_textures_ttm),
    /* Texture w, h */ 64, 32,
    /* Texture Type */ PAINTING_TYPE_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Shaded */ TRUE,
    /* Alpha */ 0xFF,
    /* Size */ 256.0f, 256.0f,
};

/// - PAINTING_ID_CASTLE_TTC -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_ttc[] = {
    inside_castle_seg7_texture_0701E800,
    inside_castle_seg7_texture_0701D800,
};

// 0x07023B48 - 0x07023BC0
const struct PaintingImage ttc_painting = {
    /* Textures */ inside_castle_seg7_painting_textures_ttc,
    /* Texture Count */ ARRAY_COUNT(inside_castle_seg7_painting_textures_ttc),
    /* Texture w, h */ 64, 32,
    /* Texture Type */ PAINTING_TYPE_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Shaded */ TRUE,
    /* Alpha */ 0xFF,
    /* Size */ 409.6f, 409.6f,
};

/// - PAINTING_ID_CASTLE_SL -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_sl[] = {
    inside_castle_seg7_texture_07020800,
    inside_castle_seg7_texture_0701F800,
};

// 0x07023BC0 - 0x07023C38
const struct PaintingImage sl_painting = {
    /* Textures */ inside_castle_seg7_painting_textures_sl,
    /* Texture Count */ ARRAY_COUNT(inside_castle_seg7_painting_textures_sl),
    /* Texture w, h */ 64, 32,
    /* Texture Type */ PAINTING_TYPE_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Shaded */ FALSE,
    /* Alpha */ 0xFF,
    /* Size */ 716.8f, 716.8f,
};

/// - PAINTING_ID_CASTLE_RR -

const struct PaintingImage rr_painting = {
    /* Textures */ NULL,
    /* Texture Count */ 0,
    /* Texture w, h */  0,  0,
    /* Texture Type */ PAINTING_TYPE_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Shaded */ TRUE,
    /* Alpha */ 0x00,
    /* Size */ 204.8f, 204.8f,
};
