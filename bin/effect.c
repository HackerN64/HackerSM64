#include <PR/ultratypes.h>
#include <PR/gbi.h>

#include "macros.h"
#include "types.h"

#include "make_const_nonconst.h"

// Flower (Unused)
// 0x0B000008
ALIGNED8 static const Texture effect_flower_0[] = {
#include "textures/effect/flower.00008.rgba16.inc.c"
};

// 0x0B000808
ALIGNED8 static const Texture effect_flower_1[] = {
#include "textures/effect/flower.00808.rgba16.inc.c"
};

// 0x0B001008
ALIGNED8 static const Texture effect_flower_2[] = {
#include "textures/effect/flower.01008.rgba16.inc.c"
};

// 0x0B001808
ALIGNED8 static const Texture effect_flower_3[] = {
#include "textures/effect/flower.01808.rgba16.inc.c"
};

// 0x0B002008
const Texture *const flower_bubbles_textures_ptr_0B002008[] = {
    effect_flower_0,
    effect_flower_1,
    effect_flower_2,
    effect_flower_3,
    effect_flower_2,
    effect_flower_1,
};

// Lava Bubble
// 0x0B002020
ALIGNED8 static const Texture effect_lava_bubble_0[] = {
#include "textures/effect/lava_bubble.02020.rgba16.inc.c"
};

// 0x0B002820
ALIGNED8 static const Texture effect_lava_bubble_1[] = {
#include "textures/effect/lava_bubble.02820.rgba16.inc.c"
};

// 0x0B003020
ALIGNED8 static const Texture effect_lava_bubble_2[] = {
#include "textures/effect/lava_bubble.03020.rgba16.inc.c"
};

// 0x0B003820
ALIGNED8 static const Texture effect_lava_bubble_3[] = {
#include "textures/effect/lava_bubble.03820.rgba16.inc.c"
};

// 0x0B004020
ALIGNED8 static const Texture effect_lava_bubble_4[] = {
#include "textures/effect/lava_bubble.04020.rgba16.inc.c"
};

// 0x0B004820
ALIGNED8 static const Texture effect_lava_bubble_5[] = {
#include "textures/effect/lava_bubble.04820.rgba16.inc.c"
};

// 0x0B005020
ALIGNED8 static const Texture effect_lava_bubble_6[] = {
#include "textures/effect/lava_bubble.05020.rgba16.inc.c"
};

// 0x0B005820
ALIGNED8 static const Texture effect_lava_bubble_7[] = {
#include "textures/effect/lava_bubble.05820.rgba16.inc.c"
};

// 0x0B006020
const Texture *const lava_bubble_ptr_0B006020[] = {
    effect_lava_bubble_0,
    effect_lava_bubble_1,
    effect_lava_bubble_2,
    effect_lava_bubble_2,
    effect_lava_bubble_2,
    effect_lava_bubble_3,
    effect_lava_bubble_4,
    effect_lava_bubble_5,
    effect_lava_bubble_6,
    effect_lava_bubble_7,
};

// Bubble
// 0x0B006048
ALIGNED8 static const Texture effect_bubble[] = {
#include "textures/effect/bubble.06048.rgba16.inc.c"
};

// 0x0B006848
const Texture *const bubble_ptr_0B006848[] = {
    effect_bubble,
};

// Tiny Bubble
// 0x0B00684C
ALIGNED8 const Texture effect_tiny_bubble_gray[] = {
#include "textures/effect/tiny_bubble.0684C.rgba16.inc.c"
};

// 0x0B006A50 - 0x0B006AB0
const Gfx envfx_dl_gray_snowflake[] = {
    gsDPPipeSync(),
    gsSPClearGeometryMode(G_LIGHTING | G_CULL_BACK | G_SHADING_SMOOTH),
    gsSPTexture(0x8000, 0x8000, 0, G_TX_RENDERTILE, G_ON),
    gsDPSetCombineMode(G_CC_DECALRGBA, G_CC_DECALRGBA),
    gsDPSetRenderMode(G_RM_AA_ZB_TEX_EDGE, G_RM_AA_ZB_TEX_EDGE2),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, effect_tiny_bubble_gray),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, 4, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, 4, G_TX_NOLOD),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 16 * 16 - 1, CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 4, 0, G_TX_RENDERTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, 4, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, 4, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (16 - 1) << G_TEXTURE_IMAGE_FRAC, (16 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPEndDisplayList(),
};

// 0x0B006AB0 - 0x0B006AD8
const Gfx envfx_dl_end[] = {
    gsSPTexture(0x0001, 0x0001, 0, G_TX_RENDERTILE, G_OFF),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsDPSetRenderMode(G_RM_AA_ZB_OPA_SURF, G_RM_AA_ZB_OPA_SURF2),
    gsSPSetGeometryMode(G_LIGHTING | G_CULL_BACK | G_SHADING_SMOOTH),
    gsSPEndDisplayList(),
};

// 0x0B006AD8
ALIGNED8 static const Texture effect_tiny_bubble_blue[] = {
#include "textures/effect/tiny_bubble.06AD8.rgba16.inc.c"
};

// 0x0B006CD8 - 0x0B006D38
const Gfx envfx_dl_blue_snowflake[] = {
    gsDPPipeSync(),
    gsSPClearGeometryMode(G_LIGHTING | G_SHADING_SMOOTH),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, effect_tiny_bubble_blue),
    gsDPSetCombineMode(G_CC_DECALRGBA, G_CC_DECALRGBA),
    gsDPSetRenderMode(G_RM_AA_ZB_TEX_EDGE, G_RM_AA_ZB_TEX_EDGE2),
    gsSPTexture(0x8000, 0x8000, 0, G_TX_RENDERTILE, G_ON),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 16 * 16 - 1, CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 4, 0, G_TX_RENDERTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (16 - 1) << G_TEXTURE_IMAGE_FRAC, (16 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPEndDisplayList(),
};

// 0x0B006D38 - 0x0B006D68
const Gfx envfx_dl_bubbles_begin[] = {
    gsDPPipeSync(),
    gsSPClearGeometryMode(G_LIGHTING | G_SHADING_SMOOTH),
    gsDPSetCombineMode(G_CC_DECALRGBA, G_CC_DECALRGBA),
    gsDPSetRenderMode(G_RM_AA_ZB_TEX_EDGE, G_RM_AA_ZB_TEX_EDGE2),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
    gsSPEndDisplayList(),
};

// 0x0B006D68 - 0x0B006D98
const Gfx envfx_dl_bubbles_texture_format[] = {
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_CLAMP, 5, G_TX_NOLOD, G_TX_CLAMP, 5, G_TX_NOLOD),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 8, 0, G_TX_RENDERTILE, 0, G_TX_CLAMP, 5, G_TX_NOLOD, G_TX_CLAMP, 5, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (32 - 1) << G_TEXTURE_IMAGE_FRAC, (32 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPEndDisplayList(),
};
