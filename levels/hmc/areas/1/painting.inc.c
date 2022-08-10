#include "game/paintings.h"

// 0x07024228 - 0x07024268
static const Vtx hmc_seg7_vertex_cotmc_pool[] = {
    {{{            0,             0,      0}, 0, {     0,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{PAINTING_SIZE,             0,      0}, 0, {     0,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{PAINTING_SIZE, PAINTING_SIZE,      0}, 0, {     0,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{            0, PAINTING_SIZE,      0}, 0, {     0,      0}, {0x00, 0x00, 0x7f, 0xff}}},
};

// 0x070254E0 - 0x07025518
static const Gfx hmc_seg7_painting_dl_cotmc_normal[] = {
    gsDPPipeSync(),
    gsSPLightColor(LIGHT_1, 0x6464ffff),
    gsSPLightColor(LIGHT_2, 0x404080ff),
    gsSPVertex(hmc_seg7_vertex_cotmc_pool, 4, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  0,  2,  3, 0x0),
    gsSPEndDisplayList(),
};

// 0x07024CE0 - 0x070254E0
ALIGNED8 static const Texture hmc_seg7_texture_cotmc_pool_env[] = {
#include "levels/hmc/7.rgba16.inc.c"
};

// 0x07025518 - 0x07025594
const Texture *const hmc_seg7_painting_textures_cotmc[] = {
    hmc_seg7_texture_cotmc_pool_env,
};

// 0x0702551C (PaintingData)
const struct Painting cotmc_painting = {
    /* id */ PAINTING_ID_HMC_COTMC,
    /* Image Count */ 1,
    /* Texture Type */ PAINTING_ENV_MAP,
    /*                      passive     entry */
    /* Ripple Magnitude */    10.0f,    30.0f,
    /* Ripple Decay */         1.0f,    0.98f,
    /* Ripple Rate */         0.05f,    0.05f,
    /* Ripple Dispersion */   15.0f,    15.0f,
    /* Normal DList */ hmc_seg7_painting_dl_cotmc_normal,
    /* Textures */     hmc_seg7_painting_textures_cotmc,
    /* Texture w, h */ 32, 32,
    /* Ripple Trigger */ RIPPLE_TRIGGER_CONTINUOUS,
    /* Alpha */ 0xFF,
    /* Unused*/ 0,
    /* Size */  723.968018f, 723.968018f,
};
