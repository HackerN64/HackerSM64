#include "game/paintings.h"

#define CCM_FAKE_SIZE 614

#define CCM_FAKE_1_X -3046
#define CCM_FAKE_1_Z -3724
// yaw ~ 150.33
#define CCM_FAKE_1_DX  304 // CCM_FAKE_SIZE * sin(yaw)
#define CCM_FAKE_1_DZ -534 // CCM_FAKE_SIZE * cos(yaw)

// 0x07021918 - 0x07021998
static const Vtx inside_castle_seg7_vertex_painting_ccm_fake_1[] = {
    {{{(CCM_FAKE_1_X +             0), -(CCM_FAKE_SIZE / 2), (CCM_FAKE_1_Z +             0)}, 0, {   -32,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_1_X + CCM_FAKE_1_DX), -(CCM_FAKE_SIZE / 2), (CCM_FAKE_1_Z + CCM_FAKE_1_DZ)}, 0, {  2012,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_1_X + CCM_FAKE_1_DX),                    0, (CCM_FAKE_1_Z + CCM_FAKE_1_DZ)}, 0, {  2012,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_1_X +             0),                    0, (CCM_FAKE_1_Z +             0)}, 0, {   -32,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_1_X +             0),                    0, (CCM_FAKE_1_Z +             0)}, 0, {   -32,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_1_X + CCM_FAKE_1_DX),                    0, (CCM_FAKE_1_Z + CCM_FAKE_1_DZ)}, 0, {  2012,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_1_X + CCM_FAKE_1_DX),  (CCM_FAKE_SIZE / 2), (CCM_FAKE_1_Z + CCM_FAKE_1_DZ)}, 0, {  2012,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_1_X +             0),  (CCM_FAKE_SIZE / 2), (CCM_FAKE_1_Z +             0)}, 0, {   -32,      0}, {0x00, 0x00, 0x7f, 0xff}}},
};

#define CCM_FAKE_2_X -1866
#define CCM_FAKE_2_Z -4258
// yaw ~ 29.66
#define CCM_FAKE_2_DX  304 // CCM_FAKE_SIZE * sin(yaw)
#define CCM_FAKE_2_DZ  534 // CCM_FAKE_SIZE * cos(yaw)

// 0x07021998 - 0x07021A18
static const Vtx inside_castle_seg7_vertex_painting_ccm_fake_2[] = {
    {{{(CCM_FAKE_2_X +             0), -(CCM_FAKE_SIZE / 2), (CCM_FAKE_2_Z +             0)}, 0, {   -32,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_2_X + CCM_FAKE_2_DX), -(CCM_FAKE_SIZE / 2), (CCM_FAKE_2_Z + CCM_FAKE_2_DZ)}, 0, {  2012,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_2_X + CCM_FAKE_2_DX),                    0, (CCM_FAKE_2_Z + CCM_FAKE_2_DZ)}, 0, {  2012,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_2_X +             0),                    0, (CCM_FAKE_2_Z +             0)}, 0, {   -32,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_2_X +             0),                    0, (CCM_FAKE_2_Z +             0)}, 0, {   -32,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_2_X + CCM_FAKE_2_DX),                    0, (CCM_FAKE_2_Z + CCM_FAKE_2_DZ)}, 0, {  2012,    992}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_2_X + CCM_FAKE_2_DX),  (CCM_FAKE_SIZE / 2), (CCM_FAKE_2_Z + CCM_FAKE_2_DZ)}, 0, {  2012,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{(CCM_FAKE_2_X +             0),  (CCM_FAKE_SIZE / 2), (CCM_FAKE_2_Z +             0)}, 0, {   -32,      0}, {0x00, 0x00, 0x7f, 0xff}}},
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

const struct PaintingImage bob_painting = {
    .textureArray  = inside_castle_seg7_painting_textures_bob,
    .imageCount    = ARRAY_COUNT(inside_castle_seg7_painting_textures_bob),
    .textureWidth  = 64,
    .textureHeight = 32,
    .imageType     = PAINTING_IMAGE_TYPE_TEXTURE,
    .rippleTrigger = RIPPLE_TRIGGER_PROXIMITY,
    .shaded        = TRUE,
    .alpha         = 0xFF,
    .sizeX         = 614.0f,
    .sizeY         = 614.0f,
};

/// - PAINTING_ID_CASTLE_CCM -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_ccm[] = {
    inside_castle_seg7_texture_0700D800,
    inside_castle_seg7_texture_0700C800,
};

const struct PaintingImage ccm_painting = {
    .textureArray  = inside_castle_seg7_painting_textures_ccm,
    .imageCount    = ARRAY_COUNT(inside_castle_seg7_painting_textures_ccm),
    .textureWidth  = 64,
    .textureHeight = 32,
    .imageType     = PAINTING_IMAGE_TYPE_TEXTURE,
    .rippleTrigger = RIPPLE_TRIGGER_PROXIMITY,
    .shaded        = TRUE,
    .alpha         = 0xFF,
    .sizeX         = 614.0f,
    .sizeY         = 614.0f,
};

/// - PAINTING_ID_CASTLE_WF -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_wf[] = {
    inside_castle_seg7_texture_0700F800,
    inside_castle_seg7_texture_0700E800,
};

const struct PaintingImage wf_painting = {
    .textureArray  = inside_castle_seg7_painting_textures_wf,
    .imageCount    = ARRAY_COUNT(inside_castle_seg7_painting_textures_wf),
    .textureWidth  = 64,
    .textureHeight = 32,
    .imageType     = PAINTING_IMAGE_TYPE_TEXTURE,
    .rippleTrigger = RIPPLE_TRIGGER_PROXIMITY,
    .shaded        = TRUE,
    .alpha         = 0xFF,
    .sizeX         = 614.0f,
    .sizeY         = 614.0f,
};

/// - PAINTING_ID_CASTLE_JRB -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_jrb[] = {
    inside_castle_seg7_texture_07011800,
    inside_castle_seg7_texture_07010800,
};

const struct PaintingImage jrb_painting = {
    .textureArray  = inside_castle_seg7_painting_textures_jrb,
    .imageCount    = ARRAY_COUNT(inside_castle_seg7_painting_textures_jrb),
    .textureWidth  = 64,
    .textureHeight = 32,
    .imageType     = PAINTING_IMAGE_TYPE_TEXTURE,
    .rippleTrigger = RIPPLE_TRIGGER_PROXIMITY,
    .shaded        = TRUE,
    .alpha         = 0xFF,
    .sizeX         = 614.0f,
    .sizeY         = 614.0f,
};

/// - PAINTING_ID_CASTLE_LLL -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_lll[] = {
    inside_castle_seg7_texture_07012800,
    inside_castle_seg7_texture_07013800,
};

const struct PaintingImage lll_painting = {
    .textureArray  = inside_castle_seg7_painting_textures_lll,
    .imageCount    = ARRAY_COUNT(inside_castle_seg7_painting_textures_lll),
    .textureWidth  = 64,
    .textureHeight = 32,
    .imageType     = PAINTING_IMAGE_TYPE_TEXTURE,
    .rippleTrigger = RIPPLE_TRIGGER_PROXIMITY,
    .shaded        = TRUE,
    .alpha         = 0xFF,
    .sizeX         = 614.0f,
    .sizeY         = 614.0f,
};

/// - PAINTING_ID_CASTLE_SSL -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_ssl[] = {
    inside_castle_seg7_texture_07015800,
    inside_castle_seg7_texture_07014800,
};

const struct PaintingImage ssl_painting = {
    .textureArray  = inside_castle_seg7_painting_textures_ssl,
    .imageCount    = ARRAY_COUNT(inside_castle_seg7_painting_textures_ssl),
    .textureWidth  = 64,
    .textureHeight = 32,
    .imageType     = PAINTING_IMAGE_TYPE_TEXTURE,
    .rippleTrigger = RIPPLE_TRIGGER_PROXIMITY,
    .shaded        = TRUE,
    .alpha         = 0xFF,
    .sizeX         = 614.0f,
    .sizeY         = 614.0f,
};

/// - PAINTING_ID_CASTLE_HMC -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_hmc_env[] = {
    inside_castle_seg7_texture_07016800,
};

const struct PaintingImage hmc_painting = {
    .textureArray  = inside_castle_seg7_painting_textures_hmc_env,
    .imageCount    = ARRAY_COUNT(inside_castle_seg7_painting_textures_hmc_env),
    .textureWidth  = 32,
    .textureHeight = 32,
    .imageType     = PAINTING_IMAGE_TYPE_ENV_MAP,
    .rippleTrigger = RIPPLE_TRIGGER_CONTINUOUS,
    .shaded        = TRUE,
    .alpha         = 0xFF,
    .sizeX         = 768.0f,
    .sizeY         = 768.0f,
};

/// - PAINTING_ID_CASTLE_DDD -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_ddd_env[] = {
    inside_castle_seg7_texture_07017000,
};

const struct PaintingImage ddd_painting = {
    .textureArray  = inside_castle_seg7_painting_textures_ddd_env,
    .imageCount    = ARRAY_COUNT(inside_castle_seg7_painting_textures_ddd_env),
    .textureWidth  = 32,
    .textureHeight = 32,
    .imageType     = PAINTING_IMAGE_TYPE_ENV_MAP,
    .rippleTrigger = RIPPLE_TRIGGER_CONTINUOUS,
    .shaded        = TRUE,
    .alpha         = 0xB4,
    .sizeX         = 819.2f,
    .sizeY         = 819.2f,
};

/// - PAINTING_ID_CASTLE_WDW -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_wdw[] = {
    inside_castle_seg7_texture_07018800,
    inside_castle_seg7_texture_07017800,
};

const struct PaintingImage wdw_painting = {
    .textureArray  = inside_castle_seg7_painting_textures_wdw,
    .imageCount    = ARRAY_COUNT(inside_castle_seg7_painting_textures_wdw),
    .textureWidth  = 64,
    .textureHeight = 32,
    .imageType     = PAINTING_IMAGE_TYPE_TEXTURE,
    .rippleTrigger = RIPPLE_TRIGGER_PROXIMITY,
    .shaded        = TRUE,
    .alpha         = 0xFF,
    .sizeX         = 614.0f,
    .sizeY         = 614.0f,
};

/// - PAINTING_ID_CASTLE_THI_TINY -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_thi[] = {
    inside_castle_seg7_texture_0701A800,
    inside_castle_seg7_texture_07019800,
};

const struct PaintingImage thi_tiny_painting = {
    .textureArray  = inside_castle_seg7_painting_textures_thi,
    .imageCount    = ARRAY_COUNT(inside_castle_seg7_painting_textures_thi),
    .textureWidth  = 64,
    .textureHeight = 32,
    .imageType     = PAINTING_IMAGE_TYPE_TEXTURE,
    .rippleTrigger = RIPPLE_TRIGGER_PROXIMITY,
    .shaded        = TRUE,
    .alpha         = 0xFF,
    .sizeX         = 393.216f,
    .sizeY         = 393.216f,
};

/// - PAINTING_ID_CASTLE_THI_HUGE -

const struct PaintingImage thi_huge_painting = {
    .textureArray  = inside_castle_seg7_painting_textures_thi,
    .imageCount    = ARRAY_COUNT(inside_castle_seg7_painting_textures_thi),
    .textureWidth  = 64,
    .textureHeight = 32,
    .imageType     = PAINTING_IMAGE_TYPE_TEXTURE,
    .rippleTrigger = RIPPLE_TRIGGER_PROXIMITY,
    .shaded        = TRUE,
    .alpha         = 0xFF,
    .sizeX         = 1638.4f,
    .sizeY         = 1638.4f,
};

/// - PAINTING_ID_CASTLE_TTM -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_ttm[] = {
    inside_castle_seg7_texture_0701C800,
    inside_castle_seg7_texture_0701B800,
};

const struct PaintingImage ttm_painting = {
    .textureArray  = inside_castle_seg7_painting_textures_ttm,
    .imageCount    = ARRAY_COUNT(inside_castle_seg7_painting_textures_ttm),
    .textureWidth  = 64,
    .textureHeight = 32,
    .imageType     = PAINTING_IMAGE_TYPE_TEXTURE,
    .rippleTrigger = RIPPLE_TRIGGER_PROXIMITY,
    .shaded        = TRUE,
    .alpha         = 0xFF,
    .sizeX         = 256.0f,
    .sizeY         = 256.0f,
};

/// - PAINTING_ID_CASTLE_TTC -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_ttc[] = {
    inside_castle_seg7_texture_0701E800,
    inside_castle_seg7_texture_0701D800,
};

const struct PaintingImage ttc_painting = {
    .textureArray  = inside_castle_seg7_painting_textures_ttc,
    .imageCount    = ARRAY_COUNT(inside_castle_seg7_painting_textures_ttc),
    .textureWidth  = 64,
    .textureHeight = 32,
    .imageType     = PAINTING_IMAGE_TYPE_TEXTURE,
    .rippleTrigger = RIPPLE_TRIGGER_PROXIMITY,
    .shaded        = TRUE,
    .alpha         = 0xFF,
    .sizeX         = 409.6f,
    .sizeY         = 409.6f,
};

/// - PAINTING_ID_CASTLE_SL -

ALIGNED8 static const Texture *const inside_castle_seg7_painting_textures_sl[] = {
    inside_castle_seg7_texture_07020800,
    inside_castle_seg7_texture_0701F800,
};

const struct PaintingImage sl_painting = {
    .textureArray  = inside_castle_seg7_painting_textures_sl,
    .imageCount    = ARRAY_COUNT(inside_castle_seg7_painting_textures_sl),
    .textureWidth  = 64,
    .textureHeight = 32,
    .imageType     = PAINTING_IMAGE_TYPE_TEXTURE,
    .rippleTrigger = RIPPLE_TRIGGER_PROXIMITY,
    .shaded        = FALSE,
    .alpha         = 0xFF,
    .sizeX         = 716.8f,
    .sizeY         = 716.8f,
};

/// - PAINTING_ID_CASTLE_RR -

const struct PaintingImage rr_painting = {
    .textureArray  = NULL,
    .imageCount    = 0,
    .textureWidth  = 0,
    .textureHeight = 0,
    .imageType     = PAINTING_IMAGE_TYPE_INVISIBLE,
    .rippleTrigger = RIPPLE_TRIGGER_NONE,
    .shaded        = TRUE,
    .alpha         = 0x00,
    .sizeX         = 204.8f,
    .sizeY         = 204.8f,
};
