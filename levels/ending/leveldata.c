#include <PR/ultratypes.h>
#include <PR/gbi.h>

#include "macros.h"
#include "types.h"

#include "make_const_nonconst.h"

#include "levels/ending/cake.inc.c"

#define NUM_CAKE_TEXTURES 40
#define NUM_CAKE_TEXTURES_X 5
#define NUM_CAKE_TEXTURES_Y (NUM_CAKE_TEXTURES / NUM_CAKE_TEXTURES_X)
#define CAKE_TEX_WIDTH (SCREEN_WIDTH  / NUM_CAKE_TEXTURES_X)
#define CAKE_TEX_HEIGHT (SCREEN_HEIGHT / NUM_CAKE_TEXTURES_Y)

#define MAKE_RECT_VERTICES(i, startX, startY, dx, dy, starts, startt) \
    {{{2 + (((i) % NUM_CAKE_TEXTURES_X) * (CAKE_TEX_WIDTH - 1) + startX +  0), (SCREEN_HEIGHT - 6) - (((i) / NUM_CAKE_TEXTURES_X) * (CAKE_TEX_HEIGHT - 1) + startY +  0), -1}, 0, {(starts +  0) << 5, (startt +  0) << 5}, {0xFF, 0xFF, 0xFF, 0xFF}}},\
    {{{2 + (((i) % NUM_CAKE_TEXTURES_X) * (CAKE_TEX_WIDTH - 1) + startX + dx), (SCREEN_HEIGHT - 6) - (((i) / NUM_CAKE_TEXTURES_X) * (CAKE_TEX_HEIGHT - 1) + startY +  0), -1}, 0, {(starts + dx) << 5, (startt +  0) << 5}, {0xFF, 0xFF, 0xFF, 0xFF}}},\
    {{{2 + (((i) % NUM_CAKE_TEXTURES_X) * (CAKE_TEX_WIDTH - 1) + startX +  0), (SCREEN_HEIGHT - 6) - (((i) / NUM_CAKE_TEXTURES_X) * (CAKE_TEX_HEIGHT - 1) + startY + dy), -1}, 0, {(starts +  0) << 5, (startt + dy) << 5}, {0xFF, 0xFF, 0xFF, 0xFF}}},\
    {{{2 + (((i) % NUM_CAKE_TEXTURES_X) * (CAKE_TEX_WIDTH - 1) + startX + dx), (SCREEN_HEIGHT - 6) - (((i) / NUM_CAKE_TEXTURES_X) * (CAKE_TEX_HEIGHT - 1) + startY + dy), -1}, 0, {(starts + dx) << 5, (startt + dy) << 5}, {0xFF, 0xFF, 0xFF, 0xFF}}}

#define MAKE_TEXT_VERTICES(i) \
    MAKE_RECT_VERTICES(i, 0, 0, CAKE_TEX_WIDTH - 1, CAKE_TEX_HEIGHT - 1, 0, 0)

#define MAKE_ROW_VERTICES(row) \
    MAKE_TEXT_VERTICES(row * NUM_CAKE_TEXTURES_X + 0), \
    MAKE_TEXT_VERTICES(row * NUM_CAKE_TEXTURES_X + 1), \
    MAKE_TEXT_VERTICES(row * NUM_CAKE_TEXTURES_X + 2), \
    MAKE_TEXT_VERTICES(row * NUM_CAKE_TEXTURES_X + 3), \
    MAKE_TEXT_VERTICES(row * NUM_CAKE_TEXTURES_X + 4)

const Vtx cake_verts[] = {
    MAKE_ROW_VERTICES(0),
    MAKE_ROW_VERTICES(1),
    MAKE_ROW_VERTICES(2),
    MAKE_ROW_VERTICES(3),
    MAKE_ROW_VERTICES(4),
    MAKE_ROW_VERTICES(5),
    MAKE_ROW_VERTICES(6),
    MAKE_ROW_VERTICES(7),
};

#define LOAD_CAKE_TEXTURE(i) \
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, cake_end_texture_data + (CAKE_TEX_WIDTH * CAKE_TEX_HEIGHT * 2 * (i))), \
    gsDPLoadSync(), \
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, \
        (((CAKE_TEX_WIDTH)*(CAKE_TEX_HEIGHT) + G_IM_SIZ_16b_INCR) >> G_IM_SIZ_16b_SHIFT)-1, \
        CALC_DXT(CAKE_TEX_WIDTH, G_IM_SIZ_16b_BYTES))

#define LOAD_CAKE_VERTICES(i) \
    gsSPVertex(cake_verts + 4 * (i), 4, 0)

#define CAKE_TRIS(i) \
    LOAD_CAKE_TEXTURE(i), \
    gsSP2Triangles(0,  2,  1, 0x0,  1,  2,  3, 0x0)

#define CAKE_RECT(i) \
    LOAD_CAKE_VERTICES(i), \
    CAKE_TRIS(i) \

#define CAKE_ROW(row) \
    CAKE_RECT((row) * NUM_CAKE_TEXTURES_X + 0),\
    CAKE_RECT((row) * NUM_CAKE_TEXTURES_X + 1),\
    CAKE_RECT((row) * NUM_CAKE_TEXTURES_X + 2),\
    CAKE_RECT((row) * NUM_CAKE_TEXTURES_X + 3),\
    CAKE_RECT((row) * NUM_CAKE_TEXTURES_X + 4)

// 0x07026400 - 0x07027350
const Gfx dl_cake_end_screen[] = {
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_DECALRGB, G_CC_DECALRGB),
    gsDPSetRenderMode(G_RM_AA_OPA_SURF, G_RM_AA_OPA_SURF2),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),

    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b_LOAD_BLOCK, 0, 0,
        G_TX_LOADTILE, 0,
        G_TX_CLAMP, G_TX_NOMASK, G_TX_NOLOD,
        G_TX_CLAMP, G_TX_NOMASK, G_TX_NOLOD),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, (((CAKE_TEX_WIDTH * G_IM_SIZ_16b_LINE_BYTES)+7)>>3), 0,
        G_TX_RENDERTILE, 0,
        G_TX_CLAMP, G_TX_NOMASK, G_TX_NOLOD,
        G_TX_CLAMP, G_TX_NOMASK, G_TX_NOLOD),
    gsDPSetTileSize(G_TX_RENDERTILE, 0, 0,
        (CAKE_TEX_WIDTH - 1) << G_TEXTURE_IMAGE_FRAC,
        (CAKE_TEX_HEIGHT - 1) << G_TEXTURE_IMAGE_FRAC),

    CAKE_ROW(0),
    CAKE_ROW(1),
    CAKE_ROW(2),
    CAKE_ROW(3),
    CAKE_ROW(4),
    CAKE_ROW(5),
    CAKE_ROW(6),
    CAKE_ROW(7),

    gsDPPipeSync(),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsSPSetGeometryMode(G_LIGHTING),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsDPSetRenderMode(G_RM_AA_ZB_OPA_SURF, G_RM_AA_ZB_OPA_SURF2),
    gsSPEndDisplayList(),
};
