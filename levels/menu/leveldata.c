#include <PR/ultratypes.h>
#include <PR/gbi.h>

#include "macros.h"
#include "surface_terrains.h"
#include "types.h"

#include "make_const_nonconst.h"
#include "game/ingame_menu.h"

// 0x07000000 - 0x07000018

// 0x07000018 - 0x07000818
ALIGNED8 static const Texture texture_menu_stone[] = {
#include "levels/menu/main_menu_seg7.00018.rgba16.inc.c"
};

// 0x07000818 - 0x07001018
ALIGNED8 static const Texture texture_menu_dark_stone[] = {
#include "levels/menu/main_menu_seg7.00818.rgba16.inc.c"
};

// 0x07001018 - 0x07002018
ALIGNED8 static const Texture texture_menu_mario_save[] = {
#include "levels/menu/main_menu_seg7.01018.rgba16.inc.c"
};

// 0x07002018 - 0x07003018
ALIGNED8 static const Texture texture_menu_mario_new[] = {
#include "levels/menu/main_menu_seg7.02018.rgba16.inc.c"
};

// 0x07003018 - 0x07003118
static const Vtx vertex_menu_save_button_borders[] = {
    {{{  -163,   -122,      0}, 0, {     0,    990}, {0x00, 0xb6, 0x66, 0xff}}},
    {{{   163,   -122,      0}, 0, {   990,    990}, {0x00, 0xb6, 0x66, 0xff}}},
    {{{  -122,    -81,     30}, 0, {    96,    820}, {0x00, 0xb6, 0x66, 0xff}}},
    {{{   122,    -81,     30}, 0, {   862,    820}, {0x00, 0xb6, 0x66, 0xff}}},
    {{{  -163,   -122,      0}, 0, {     0,    990}, {0xb6, 0x00, 0x66, 0xff}}},
    {{{  -122,    -81,     30}, 0, {    96,    820}, {0xb6, 0x00, 0x66, 0xff}}},
    {{{  -163,    122,      0}, 0, {     0,      0}, {0xb6, 0x00, 0x66, 0xff}}},
    {{{  -122,     81,     30}, 0, {    96,    138}, {0xb6, 0x00, 0x66, 0xff}}},
    {{{  -122,     81,     30}, 0, {    96,    138}, {0x00, 0x4a, 0x66, 0xff}}},
    {{{   122,     81,     30}, 0, {   862,    138}, {0x00, 0x4a, 0x66, 0xff}}},
    {{{   163,    122,      0}, 0, {   990,      0}, {0x00, 0x4a, 0x66, 0xff}}},
    {{{  -163,    122,      0}, 0, {     0,      0}, {0x00, 0x4a, 0x66, 0xff}}},
    {{{   122,     81,     30}, 0, {   862,    138}, {0x4a, 0x00, 0x66, 0xff}}},
    {{{   122,    -81,     30}, 0, {   862,    820}, {0x4a, 0x00, 0x66, 0xff}}},
    {{{   163,   -122,      0}, 0, {   990,    990}, {0x4a, 0x00, 0x66, 0xff}}},
    {{{   163,    122,      0}, 0, {   990,      0}, {0x4a, 0x00, 0x66, 0xff}}},
};

// 0x07003118 - 0x07003158
static const Vtx vertex_menu_save_button_front[] = {
    {{{   122,     81,     30}, 0, {  2012,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{  -122,     81,     30}, 0, {     0,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{   122,    -81,     30}, 0, {  2012,    990}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{  -122,    -81,     30}, 0, {     0,    990}, {0x00, 0x00, 0x7f, 0xff}}},
};

// 0x07003158 - 0x070031A0
static const Gfx dl_tex_block_menu_save_button_base[] = {
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_MODULATERGB, G_CC_MODULATERGB),
    gsSPClearGeometryMode(G_SHADING_SMOOTH),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 8, 0, G_TX_RENDERTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, 5, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, 5, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (32 - 1) << G_TEXTURE_IMAGE_FRAC, (32 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPEndDisplayList(),
};

// 0x070031A0 - 0x07003218
static const Gfx dl_vertex_menu_save_button_borders[] = {
    gsSPLightColor(LIGHT_1, 0xffffffff),
    gsSPLightColor(LIGHT_2, 0x3f3f3fff),
    gsSPVertex(vertex_menu_save_button_borders, 16, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  1,  3,  2, 0x0),
    gsSP2Triangles( 4,  5,  6, 0x0,  5,  7,  6, 0x0),
    gsSP2Triangles( 8,  9, 10, 0x0, 11,  8, 10, 0x0),
    gsSP2Triangles(12, 13, 14, 0x0, 15, 12, 14, 0x0),
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 16, 0, G_TX_RENDERTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, 5, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, 6, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (64 - 1) << G_TEXTURE_IMAGE_FRAC, (32 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPEndDisplayList(),
};

// 0x07003218 - 0x07003258
static const Gfx dl_vertex_menu_save_button_front[] = {
    gsSPVertex(vertex_menu_save_button_front, 4, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  1,  3,  2, 0x0),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPSetGeometryMode(G_SHADING_SMOOTH),
    gsSPEndDisplayList(),
};

// 0x07003258 - 0x07003298
static const Vtx vertex_menu_save_button_back[] = {
    {{{   163,   -122,      0}, 0, {     0,    990}, {0x00, 0x00, 0x81, 0xff}}},
    {{{  -163,   -122,      0}, 0, {   990,    990}, {0x00, 0x00, 0x81, 0xff}}},
    {{{   163,    122,      0}, 0, {     0,      0}, {0x00, 0x00, 0x81, 0xff}}},
    {{{  -163,    122,      0}, 0, {   990,      0}, {0x00, 0x00, 0x81, 0xff}}},
};

// 0x07003298 - 0x070032E0
static const Gfx dl_tex_block_menu_save_button_back[] = {
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_MODULATERGB, G_CC_MODULATERGB),
    gsSPClearGeometryMode(G_SHADING_SMOOTH),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 8, 0, G_TX_RENDERTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, 5, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, 5, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (32 - 1) << G_TEXTURE_IMAGE_FRAC, (32 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPEndDisplayList(),
};

// 0x070032E0 - 0x07003330
static const Gfx dl_vertex_menu_save_button_back[] = {
    gsSPLightColor(LIGHT_1, 0xffffffff),
    gsSPLightColor(LIGHT_2, 0x3f3f3fff),
    gsSPVertex(vertex_menu_save_button_back, 4, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  1,  3,  2, 0x0),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPSetGeometryMode(G_SHADING_SMOOTH),
    gsSPEndDisplayList(),
};

// 0x07003330 - 0x07003380
const Gfx dl_menu_mario_save_button_base[] = {
    gsSPDisplayList(dl_tex_block_menu_save_button_base),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_stone),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_vertex_menu_save_button_borders),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_mario_save),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_vertex_menu_save_button_front),
    gsSPEndDisplayList(),
};

// 0x07003380 - 0x070033D0
const Gfx dl_menu_mario_new_button_base[] = {
    gsSPDisplayList(dl_tex_block_menu_save_button_base),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_stone),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_vertex_menu_save_button_borders),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_mario_new),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_vertex_menu_save_button_front),
    gsSPEndDisplayList(),
};

// 0x070033D0 - 0x07003400
const Gfx dl_menu_save_button_back[] = {
    gsSPDisplayList(dl_tex_block_menu_save_button_back),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_dark_stone),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_vertex_menu_save_button_back),
    gsSPEndDisplayList(),
};

// 0x07003400 - 0x07003450
const Gfx dl_menu_save_button_fade_back[] = {
    gsDPPipeSync(),
    gsSPClearGeometryMode(G_SHADING_SMOOTH),
    gsSPLightColor(LIGHT_1, 0xffffffff),
    gsSPLightColor(LIGHT_2, 0x3f3f3fff),
    gsSPVertex(vertex_menu_save_button_back, 4, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  1,  3,  2, 0x0),
    gsDPPipeSync(),
    gsSPSetGeometryMode(G_SHADING_SMOOTH),
    gsSPEndDisplayList(),
};

// 0x07003450 - 0x07003468

// 0x07003468 - 0x07003468
ALIGNED8 static const Texture texture_menu_erase[] = {
#include "levels/menu/main_menu_seg7.03468.rgba16.inc.c"
};

// 0x07003C68 - 0x07003C68
ALIGNED8 static const Texture texture_menu_copy[] = {
#include "levels/menu/main_menu_seg7.03C68.rgba16.inc.c"
};

// 0x07004468 - 0x07004468
ALIGNED8 static const Texture texture_menu_file[] = {
#include "levels/menu/main_menu_seg7.04468.rgba16.inc.c"
};

// 0x07004C68 - 0x07004C68
ALIGNED8 static const Texture texture_menu_score[] = {
#include "levels/menu/main_menu_seg7.04C68.rgba16.inc.c"
};

// 0x07005468 - 0x07005468
ALIGNED8 static const Texture texture_menu_sound[] = {
#include "levels/menu/main_menu_seg7.05468.rgba16.inc.c"
};

// 0x07005C68 - 0x07005D68
static const Vtx vertex_menu_main_button_group1[] = {
    {{{  -163,   -122,      0}, 0, {   990,      0}, {0xb6, 0x00, 0x66, 0xff}}},
    {{{  -122,    -81,     30}, 0, {   862,    138}, {0xb6, 0x00, 0x66, 0xff}}},
    {{{  -163,    122,      0}, 0, {   990,    990}, {0xb6, 0x00, 0x66, 0xff}}},
    {{{  -143,    102,      0}, 0, {   926,    904}, {0x59, 0x00, 0xa7, 0xff}}},
    {{{  -133,     92,     10}, 0, {   894,    862}, {0x59, 0x00, 0xa7, 0xff}}},
    {{{  -133,    -92,     10}, 0, {   894,     96}, {0x59, 0x00, 0xa7, 0xff}}},
    {{{  -133,     92,     10}, 0, {   894,    862}, {0x00, 0x00, 0x81, 0xff}}},
    {{{   133,    -92,     10}, 0, {    64,     96}, {0x00, 0x00, 0x81, 0xff}}},
    {{{  -133,    -92,     10}, 0, {   894,     96}, {0x00, 0x00, 0x81, 0xff}}},
    {{{   133,     92,     10}, 0, {    64,    862}, {0x00, 0x00, 0x81, 0xff}}},
    {{{   133,     92,     10}, 0, {    64,    862}, {0x00, 0xa7, 0xa7, 0xff}}},
    {{{  -133,     92,     10}, 0, {   894,    862}, {0x00, 0xa7, 0xa7, 0xff}}},
    {{{  -143,    102,      0}, 0, {   926,    904}, {0x00, 0xa7, 0xa7, 0xff}}},
    {{{   143,   -102,      0}, 0, {    32,     54}, {0xa7, 0x00, 0xa7, 0xff}}},
    {{{   133,     92,     10}, 0, {    64,    862}, {0xa7, 0x00, 0xa7, 0xff}}},
    {{{   143,    102,      0}, 0, {    32,    904}, {0xa7, 0x00, 0xa7, 0xff}}},
};

// 0x07005D68 - 0x07005E68
static const Vtx vertex_menu_main_button_group2[] = {
    {{{   143,   -102,      0}, 0, {    32,     54}, {0xa7, 0x00, 0xa7, 0xff}}},
    {{{   133,    -92,     10}, 0, {    64,     96}, {0xa7, 0x00, 0xa7, 0xff}}},
    {{{   133,     92,     10}, 0, {    64,    862}, {0xa7, 0x00, 0xa7, 0xff}}},
    {{{   133,     92,     10}, 0, {    64,    862}, {0x00, 0xa7, 0xa7, 0xff}}},
    {{{  -143,    102,      0}, 0, {   926,    904}, {0x00, 0xa7, 0xa7, 0xff}}},
    {{{   143,    102,      0}, 0, {    32,    904}, {0x00, 0xa7, 0xa7, 0xff}}},
    {{{  -143,   -102,      0}, 0, {   926,     54}, {0x00, 0x59, 0xa7, 0xff}}},
    {{{   133,    -92,     10}, 0, {    64,     96}, {0x00, 0x59, 0xa7, 0xff}}},
    {{{   143,   -102,      0}, 0, {    32,     54}, {0x00, 0x59, 0xa7, 0xff}}},
    {{{  -133,    -92,     10}, 0, {   894,     96}, {0x00, 0x59, 0xa7, 0xff}}},
    {{{  -143,    102,      0}, 0, {   926,    904}, {0x59, 0x00, 0xa7, 0xff}}},
    {{{  -133,    -92,     10}, 0, {   894,     96}, {0x59, 0x00, 0xa7, 0xff}}},
    {{{  -143,   -102,      0}, 0, {   926,     54}, {0x59, 0x00, 0xa7, 0xff}}},
    {{{   163,    122,      0}, 0, {     0,    990}, {0x00, 0x00, 0x81, 0xff}}},
    {{{  -143,    102,      0}, 0, {   926,    904}, {0x00, 0x00, 0x81, 0xff}}},
    {{{  -163,    122,      0}, 0, {   990,    990}, {0x00, 0x00, 0x81, 0xff}}},
};

// 0x07005E68 - 0x07005F48
static const Vtx vertex_menu_main_button_group3[] = {
    {{{   163,    122,      0}, 0, {     0,    990}, {0x00, 0x00, 0x81, 0xff}}},
    {{{   143,    102,      0}, 0, {    32,    904}, {0x00, 0x00, 0x81, 0xff}}},
    {{{  -143,    102,      0}, 0, {   926,    904}, {0x00, 0x00, 0x81, 0xff}}},
    {{{   143,   -102,      0}, 0, {    32,     54}, {0x00, 0x00, 0x81, 0xff}}},
    {{{   163,   -122,      0}, 0, {     0,      0}, {0x00, 0x00, 0x81, 0xff}}},
    {{{  -163,    122,      0}, 0, {   990,    990}, {0x00, 0x00, 0x81, 0xff}}},
    {{{  -143,   -102,      0}, 0, {   926,     54}, {0x00, 0x00, 0x81, 0xff}}},
    {{{  -163,   -122,      0}, 0, {   990,      0}, {0x00, 0x00, 0x81, 0xff}}},
    {{{   163,   -122,      0}, 0, {     0,      0}, {0x00, 0xb6, 0x66, 0xff}}},
    {{{   122,    -81,     30}, 0, {    96,    138}, {0x00, 0xb6, 0x66, 0xff}}},
    {{{  -122,    -81,     30}, 0, {   862,    138}, {0x00, 0xb6, 0x66, 0xff}}},
    {{{  -122,    -81,     30}, 0, {   862,    138}, {0xb6, 0x00, 0x66, 0xff}}},
    {{{  -122,     81,     30}, 0, {   862,    820}, {0xb6, 0x00, 0x66, 0xff}}},
    {{{  -163,    122,      0}, 0, {   990,    990}, {0xb6, 0x00, 0x66, 0xff}}},
};

// 0x07005F48 - 0x07006038
static const Vtx vertex_menu_main_button_group4[] = {
    {{{  -122,     81,     30}, 0, {   862,    820}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{  -122,    -81,     30}, 0, {   862,    138}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{   122,    -81,     30}, 0, {    96,    138}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{  -163,   -122,      0}, 0, {   990,      0}, {0x00, 0xb6, 0x66, 0xff}}},
    {{{   163,   -122,      0}, 0, {     0,      0}, {0x00, 0xb6, 0x66, 0xff}}},
    {{{  -122,    -81,     30}, 0, {   862,    138}, {0x00, 0xb6, 0x66, 0xff}}},
    {{{  -122,     81,     30}, 0, {   862,    820}, {0x00, 0x4a, 0x66, 0xff}}},
    {{{   122,     81,     30}, 0, {    96,    820}, {0x00, 0x4a, 0x66, 0xff}}},
    {{{   163,    122,      0}, 0, {     0,    990}, {0x00, 0x4a, 0x66, 0xff}}},
    {{{  -163,    122,      0}, 0, {   990,    990}, {0x00, 0x4a, 0x66, 0xff}}},
    {{{   122,     81,     30}, 0, {    96,    820}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{   163,    122,      0}, 0, {     0,    990}, {0x4a, 0x00, 0x66, 0xff}}},
    {{{   122,     81,     30}, 0, {    96,    820}, {0x4a, 0x00, 0x66, 0xff}}},
    {{{   163,   -122,      0}, 0, {     0,      0}, {0x4a, 0x00, 0x66, 0xff}}},
    {{{   122,    -81,     30}, 0, {    96,    138}, {0x4a, 0x00, 0x66, 0xff}}},
};

// 0x07006038 - 0x07006150
static const Gfx dl_vertex_menu_main_button[] = {
    gsSPLightColor(LIGHT_1, 0xffffffff),
    gsSPLightColor(LIGHT_2, 0x3f3f3fff),
    gsSPVertex(vertex_menu_main_button_group1, 16, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  3,  4,  5, 0x0),
    gsSP2Triangles( 6,  7,  8, 0x0,  6,  9,  7, 0x0),
    gsSP2Triangles(10, 11, 12, 0x0, 13, 14, 15, 0x0),
    gsSPVertex(vertex_menu_main_button_group2, 16, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  3,  4,  5, 0x0),
    gsSP2Triangles( 6,  7,  8, 0x0,  6,  9,  7, 0x0),
    gsSP2Triangles(10, 11, 12, 0x0, 13, 14, 15, 0x0),
    gsSPVertex(vertex_menu_main_button_group3, 14, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  0,  3,  1, 0x0),
    gsSP2Triangles( 0,  4,  3, 0x0,  5,  2,  6, 0x0),
    gsSP2Triangles( 5,  6,  7, 0x0,  6,  3,  4, 0x0),
    gsSP2Triangles( 6,  4,  7, 0x0,  8,  9, 10, 0x0),
    gsSP1Triangle(11, 12, 13, 0x0),
    gsSPVertex(vertex_menu_main_button_group4, 15, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  3,  4,  5, 0x0),
    gsSP2Triangles( 6,  7,  8, 0x0,  9,  6,  8, 0x0),
    gsSP2Triangles(10,  0,  2, 0x0, 11, 12, 13, 0x0),
    gsSP1Triangle(12, 14, 13, 0x0),
    gsSPEndDisplayList(),
};

// 0x07006150 - 0x07006198
static const Gfx dl_tex_block_menu_main_button[] = {
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_MODULATERGB, G_CC_MODULATERGB),
    gsSPClearGeometryMode(G_SHADING_SMOOTH),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 8, 0, G_TX_RENDERTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, 5, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, 5, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (32 - 1) << G_TEXTURE_IMAGE_FRAC, (32 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPEndDisplayList(),
};

// 0x07006198 - 0x070061C8
static const Gfx dl_menu_main_button[] = {
    gsSPDisplayList(dl_vertex_menu_main_button),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPSetGeometryMode(G_SHADING_SMOOTH),
    gsSPEndDisplayList(),
};

// 0x070061C8 - 0x070061F8
const Gfx dl_menu_erase_button[] = {
    gsSPDisplayList(dl_tex_block_menu_main_button),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_erase),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_menu_main_button),
    gsSPEndDisplayList(),
};

// 0x070061F8 - 0x07006228
const Gfx dl_menu_copy_button[] = {
    gsSPDisplayList(dl_tex_block_menu_main_button),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_copy),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_menu_main_button),
    gsSPEndDisplayList(),
};

// 0x07006228 - 0x07006258
const Gfx dl_menu_file_button[] = {
    gsSPDisplayList(dl_tex_block_menu_main_button),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_file),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_menu_main_button),
    gsSPEndDisplayList(),
};

// 0x07006258 - 0x07006288
const Gfx dl_menu_score_button[] = {
    gsSPDisplayList(dl_tex_block_menu_main_button),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_score),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_menu_main_button),
    gsSPEndDisplayList(),
};

// 0x07006288 - 0x070062B8
const Gfx dl_menu_sound_button[] = {
    gsSPDisplayList(dl_tex_block_menu_main_button),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_sound),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_menu_main_button),
    gsSPEndDisplayList(),
};

// 0x070062B8 - 0x070062E8
const Gfx dl_menu_generic_button[] = {
    gsSPDisplayList(dl_tex_block_menu_main_button),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_stone),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_menu_main_button),
    gsSPEndDisplayList(),
};

// 0x070062E8 - 0x07006328
static const Vtx vertex_menu_hand[] = {
    {{{     0,      0,      0}, 0, {     0,   1984}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{    32,      0,      0}, 0, {  1984,   1984}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{    32,     32,      0}, 0, {  1984,      0}, {0x00, 0x00, 0x7f, 0xff}}},
    {{{     0,     32,      0}, 0, {     0,      0}, {0x00, 0x00, 0x7f, 0xff}}},
};

// 0x07006328 - 0x07006B28
ALIGNED8 static const Texture texture_menu_idle_hand[] = {
#include "levels/menu/main_menu_seg7.06328.rgba16.inc.c"
};

// 0x07006B28 - 0x07007328
ALIGNED8 static const Texture texture_menu_grabbing_hand[] = {
#include "levels/menu/main_menu_seg7.06B28.rgba16.inc.c"
};

// 0x07007328 - 0x070073A0
static const Gfx dl_menu_hand[] = {
    gsDPSetCombineMode(G_CC_DECALRGBA, G_CC_DECALRGBA),
    gsDPSetRenderMode(G_RM_AA_TEX_EDGE, G_RM_AA_TEX_EDGE2),
    gsSPTexture(0x8000, 0x8000, 0, G_TX_RENDERTILE, G_ON),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 32 * 32 - 1, CALC_DXT(32, G_IM_SIZ_16b_BYTES)),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 8, 0, G_TX_RENDERTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (32 - 1) << G_TEXTURE_IMAGE_FRAC, (32 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPVertex(vertex_menu_hand, 4, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  0,  2,  3, 0x0),
    gsSPTexture(0x0001, 0x0001, 0, G_TX_RENDERTILE, G_OFF),
    gsDPPipeSync(),
    gsDPSetRenderMode(G_RM_AA_ZB_OPA_SURF, G_RM_AA_ZB_OPA_SURF2),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPEndDisplayList(),
};

// 0x070073A0 - 0x070073B8
const Gfx dl_menu_idle_hand[] = {
    gsDPPipeSync(),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_idle_hand),
    gsSPBranchList(dl_menu_hand),
};

// 0x070073B8 - 0x070073D0
const Gfx dl_menu_grabbing_hand[] = {
    gsDPPipeSync(),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_grabbing_hand),
    gsSPBranchList(dl_menu_hand),
};

#ifdef JAPANESE_CHARACTERS
// 0x0700AC48
ALIGNED8 static const Texture texture_menu_font_char_jp_0[] = {
#include "levels/menu/main_menu_seg7.0AC48.ia8.inc.c"
};

// 0x0700AC88
ALIGNED8 static const Texture texture_menu_font_char_jp_1[] = {
#include "levels/menu/main_menu_seg7.0AC88.ia8.inc.c"
};

// 0x0700ACC8
ALIGNED8 static const Texture texture_menu_font_char_jp_2[] = {
#include "levels/menu/main_menu_seg7.0ACC8.ia8.inc.c"
};

// 0x0700AD08
ALIGNED8 static const Texture texture_menu_font_char_jp_3[] = {
#include "levels/menu/main_menu_seg7.0AD08.ia8.inc.c"
};

// 0x0700AD48
ALIGNED8 static const Texture texture_menu_font_char_jp_4[] = {
#include "levels/menu/main_menu_seg7.0AD48.ia8.inc.c"
};

// 0x0700AD88
ALIGNED8 static const Texture texture_menu_font_char_jp_5[] = {
#include "levels/menu/main_menu_seg7.0AD88.ia8.inc.c"
};

// 0x0700ADC8
ALIGNED8 static const Texture texture_menu_font_char_jp_6[] = {
#include "levels/menu/main_menu_seg7.0ADC8.ia8.inc.c"
};

// 0x0700AE08
ALIGNED8 static const Texture texture_menu_font_char_jp_7[] = {
#include "levels/menu/main_menu_seg7.0AE08.ia8.inc.c"
};

// 0x0700AE48
ALIGNED8 static const Texture texture_menu_font_char_jp_8[] = {
#include "levels/menu/main_menu_seg7.0AE48.ia8.inc.c"
};

// 0x0700AE88
ALIGNED8 static const Texture texture_menu_font_char_jp_9[] = {
#include "levels/menu/main_menu_seg7.0AE88.ia8.inc.c"
};

// 0x0700AEC8
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_a[] = {
#include "levels/menu/main_menu_seg7.0AEC8.ia8.inc.c"
};

// 0x0700AF08
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_i[] = {
#include "levels/menu/main_menu_seg7.0AF08.ia8.inc.c"
};

// 0x0700AF48
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_u[] = {
#include "levels/menu/main_menu_seg7.0AF48.ia8.inc.c"
};

// 0x0700AF88
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_e[] = {
#include "levels/menu/main_menu_seg7.0AF88.ia8.inc.c"
};

// 0x0700AFC8
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_o[] = {
#include "levels/menu/main_menu_seg7.0AFC8.ia8.inc.c"
};

// 0x0700B008
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_ka[] = {
#include "levels/menu/main_menu_seg7.0B008.ia8.inc.c"
};

// 0x0700B048
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_ki[] = {
#include "levels/menu/main_menu_seg7.0B048.ia8.inc.c"
};

// 0x0700B088
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_ku[] = {
#include "levels/menu/main_menu_seg7.0B088.ia8.inc.c"
};

// 0x0700B0C8
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_ke[] = {
#include "levels/menu/main_menu_seg7.0B0C8.ia8.inc.c"
};

// 0x0700B108
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_ko[] = {
#include "levels/menu/main_menu_seg7.0B108.ia8.inc.c"
};

// 0x0700B148
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_sa[] = {
#include "levels/menu/main_menu_seg7.0B148.ia8.inc.c"
};

// 0x0700B188
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_shi[] = {
#include "levels/menu/main_menu_seg7.0B188.ia8.inc.c"
};

// 0x0700B1C8
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_su[] = {
#include "levels/menu/main_menu_seg7.0B1C8.ia8.inc.c"
};

// 0x0700B208
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_se[] = {
#include "levels/menu/main_menu_seg7.0B208.ia8.inc.c"
};

// 0x0700B248
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_so[] = {
#include "levels/menu/main_menu_seg7.0B248.ia8.inc.c"
};

// 0x0700B288
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_ta[] = {
#include "levels/menu/main_menu_seg7.0B288.ia8.inc.c"
};

// 0x0700B2C8
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_chi[] = {
#include "levels/menu/main_menu_seg7.0B2C8.ia8.inc.c"
};

// 0x0700B308
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_tsu[] = {
#include "levels/menu/main_menu_seg7.0B308.ia8.inc.c"
};

// 0x0700B348
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_te[] = {
#include "levels/menu/main_menu_seg7.0B348.ia8.inc.c"
};

// 0x0700B388
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_to[] = {
#include "levels/menu/main_menu_seg7.0B388.ia8.inc.c"
};

// 0x0700B3C8
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_na[] = {
#include "levels/menu/main_menu_seg7.0B3C8.ia8.inc.c"
};

// 0x0700B408
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_ni[] = {
#include "levels/menu/main_menu_seg7.0B408.ia8.inc.c"
};

// 0x0700B448
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_nu[] = {
#include "levels/menu/main_menu_seg7.0B448.ia8.inc.c"
};

// 0x0700B488
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_ne[] = {
#include "levels/menu/main_menu_seg7.0B488.ia8.inc.c"
};

// 0x0700B4C8
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_no[] = {
#include "levels/menu/main_menu_seg7.0B4C8.ia8.inc.c"
};

// 0x0700B508
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_ha[] = {
#include "levels/menu/main_menu_seg7.0B508.ia8.inc.c"
};

// 0x0700B548
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_hi[] = {
#include "levels/menu/main_menu_seg7.0B548.ia8.inc.c"
};

// 0x0700B588
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_fu[] = {
#include "levels/menu/main_menu_seg7.0B588.ia8.inc.c"
};

// 0x0700B5C8
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_he[] = {
#include "levels/menu/main_menu_seg7.0B5C8.ia8.inc.c"
};

// 0x0700B608
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_ho[] = {
#include "levels/menu/main_menu_seg7.0B608.ia8.inc.c"
};

// 0x0700B648
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_ma[] = {
#include "levels/menu/main_menu_seg7.0B648.ia8.inc.c"
};

// 0x0700B688
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_mi[] = {
#include "levels/menu/main_menu_seg7.0B688.ia8.inc.c"
};

// 0x0700B6C8
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_mu[] = {
#include "levels/menu/main_menu_seg7.0B6C8.ia8.inc.c"
};

// 0x0700B708
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_me[] = {
#include "levels/menu/main_menu_seg7.0B708.ia8.inc.c"
};

// 0x0700B748
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_mo[] = {
#include "levels/menu/main_menu_seg7.0B748.ia8.inc.c"
};

// 0x0700B788
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_ya[] = {
#include "levels/menu/main_menu_seg7.0B788.ia8.inc.c"
};

// 0x0700B7C8
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_yu[] = {
#include "levels/menu/main_menu_seg7.0B7C8.ia8.inc.c"
};

// 0x0700B808
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_yo[] = {
#include "levels/menu/main_menu_seg7.0B808.ia8.inc.c"
};

// 0x0700B848
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_ra[] = {
#include "levels/menu/main_menu_seg7.0B848.ia8.inc.c"
};

// 0x0700B888
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_ri[] = {
#include "levels/menu/main_menu_seg7.0B888.ia8.inc.c"
};

// 0x0700B8C8
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_ru[] = {
#include "levels/menu/main_menu_seg7.0B8C8.ia8.inc.c"
};

// 0x0700B908
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_re[] = {
#include "levels/menu/main_menu_seg7.0B908.ia8.inc.c"
};

// 0x0700B948
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_ro[] = {
#include "levels/menu/main_menu_seg7.0B948.ia8.inc.c"
};

// 0x0700B988
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_wa[] = {
#include "levels/menu/main_menu_seg7.0B988.ia8.inc.c"
};

// 0x0700B9C8
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_wo[] = {
#include "levels/menu/main_menu_seg7.0B9C8.ia8.inc.c"
};

// 0x0700BA08
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_n[] = {
#include "levels/menu/main_menu_seg7.0BA08.ia8.inc.c"
};

// 0x0700BA48
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_small_a[] = {
#include "levels/menu/main_menu_seg7.0BA48.ia8.inc.c"
};

// 0x0700BA88
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_small_i[] = {
#include "levels/menu/main_menu_seg7.0BA88.ia8.inc.c"
};

// 0x0700BAC8
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_small_u[] = {
#include "levels/menu/main_menu_seg7.0BAC8.ia8.inc.c"
};

// 0x0700BB08
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_small_e[] = {
#include "levels/menu/main_menu_seg7.0BB08.ia8.inc.c"
};

// 0x0700BB48
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_small_o[] = {
#include "levels/menu/main_menu_seg7.0BB48.ia8.inc.c"
};

// 0x0700BB88
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_small_ya[] = {
#include "levels/menu/main_menu_seg7.0BB88.ia8.inc.c"
};

// 0x0700BBC8
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_small_yu[] = {
#include "levels/menu/main_menu_seg7.0BBC8.ia8.inc.c"
};

// 0x0700BC08
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_small_yo[] = {
#include "levels/menu/main_menu_seg7.0BC08.ia8.inc.c"
};

// 0x0700BC48
ALIGNED8 static const Texture texture_menu_font_char_jp_hiragana_small_tsu[] = {
#include "levels/menu/main_menu_seg7.0BC48.ia8.inc.c"
};

// 0x0700BC88
ALIGNED8 static const Texture texture_menu_font_char_jp_handakuten[] = {
#include "levels/menu/main_menu_seg7.0BC88.ia8.inc.c"
};

// 0x0700BCC8
ALIGNED8 static const Texture texture_menu_font_char_jp_dakuten[] = {
#include "levels/menu/main_menu_seg7.0BCC8.ia8.inc.c"
};

// 0x0700BD08
ALIGNED8 static const Texture texture_menu_font_char_jp_long_vowel[] = {
#include "levels/menu/main_menu_seg7.0BD08.ia8.inc.c"
};

// 0x0700BD48
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_a[] = {
#include "levels/menu/main_menu_seg7.0BD48.ia8.inc.c"
};

// 0x0700BD88
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_i[] = {
#include "levels/menu/main_menu_seg7.0BD88.ia8.inc.c"
};

// 0x0700BDC8
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_u[] = {
#include "levels/menu/main_menu_seg7.0BDC8.ia8.inc.c"
};

// 0x0700BE08
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_e[] = {
#include "levels/menu/main_menu_seg7.0BE08.ia8.inc.c"
};

// 0x0700BE48
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_o[] = {
#include "levels/menu/main_menu_seg7.0BE48.ia8.inc.c"
};

// 0x0700BE88
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_ka[] = {
#include "levels/menu/main_menu_seg7.0BE88.ia8.inc.c"
};

// 0x0700BEC8
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_ki[] = {
#include "levels/menu/main_menu_seg7.0BEC8.ia8.inc.c"
};

// 0x0700BF08
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_ku[] = {
#include "levels/menu/main_menu_seg7.0BF08.ia8.inc.c"
};

// 0x0700BF48
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_ke[] = {
#include "levels/menu/main_menu_seg7.0BF48.ia8.inc.c"
};

// 0x0700BF88
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_ko[] = {
#include "levels/menu/main_menu_seg7.0BF88.ia8.inc.c"
};

// 0x0700BFC8
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_sa[] = {
#include "levels/menu/main_menu_seg7.0BFC8.ia8.inc.c"
};

// 0x0700C008
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_shi[] = {
#include "levels/menu/main_menu_seg7.0C008.ia8.inc.c"
};

// 0x0700C048
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_su[] = {
#include "levels/menu/main_menu_seg7.0C048.ia8.inc.c"
};

// 0x0700C088
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_se[] = {
#include "levels/menu/main_menu_seg7.0C088.ia8.inc.c"
};

// 0x0700C0C8
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_so[] = {
#include "levels/menu/main_menu_seg7.0C0C8.ia8.inc.c"
};

// 0x0700C108
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_ta[] = {
#include "levels/menu/main_menu_seg7.0C108.ia8.inc.c"
};

// 0x0700C148
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_chi[] = {
#include "levels/menu/main_menu_seg7.0C148.ia8.inc.c"
};

// 0x0700C188
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_tsu[] = {
#include "levels/menu/main_menu_seg7.0C188.ia8.inc.c"
};

// 0x0700C1C8
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_te[] = {
#include "levels/menu/main_menu_seg7.0C1C8.ia8.inc.c"
};

// 0x0700C208
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_to[] = {
#include "levels/menu/main_menu_seg7.0C208.ia8.inc.c"
};

// 0x0700C248
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_na[] = {
#include "levels/menu/main_menu_seg7.0C248.ia8.inc.c"
};

// 0x0700C288
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_ni[] = {
#include "levels/menu/main_menu_seg7.0C288.ia8.inc.c"
};

// 0x0700C2C8
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_nu[] = {
#include "levels/menu/main_menu_seg7.0C2C8.ia8.inc.c"
};

// 0x0700C308
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_ne[] = {
#include "levels/menu/main_menu_seg7.0C308.ia8.inc.c"
};

// 0x0700C348
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_no[] = {
#include "levels/menu/main_menu_seg7.0C348.ia8.inc.c"
};

// 0x0700C388
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_ha[] = {
#include "levels/menu/main_menu_seg7.0C388.ia8.inc.c"
};

// 0x0700C3C8
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_hi[] = {
#include "levels/menu/main_menu_seg7.0C3C8.ia8.inc.c"
};

// 0x0700C408
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_fu[] = {
#include "levels/menu/main_menu_seg7.0C408.ia8.inc.c"
};

// 0x0700C448
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_he[] = {
#include "levels/menu/main_menu_seg7.0C448.ia8.inc.c"
};

// 0x0700C488
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_ho[] = {
#include "levels/menu/main_menu_seg7.0C488.ia8.inc.c"
};

// 0x0700C4C8
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_ma[] = {
#include "levels/menu/main_menu_seg7.0C4C8.ia8.inc.c"
};

// 0x0700C508
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_mi[] = {
#include "levels/menu/main_menu_seg7.0C508.ia8.inc.c"
};

// 0x0700C548
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_mu[] = {
#include "levels/menu/main_menu_seg7.0C548.ia8.inc.c"
};

// 0x0700C588
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_me[] = {
#include "levels/menu/main_menu_seg7.0C588.ia8.inc.c"
};

// 0x0700C5C8
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_mo[] = {
#include "levels/menu/main_menu_seg7.0C5C8.ia8.inc.c"
};

// 0x0700C608
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_ya[] = {
#include "levels/menu/main_menu_seg7.0C608.ia8.inc.c"
};

// 0x0700C648
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_yu[] = {
#include "levels/menu/main_menu_seg7.0C648.ia8.inc.c"
};

// 0x0700C688
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_yo[] = {
#include "levels/menu/main_menu_seg7.0C688.ia8.inc.c"
};

// 0x0700C6C8
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_ra[] = {
#include "levels/menu/main_menu_seg7.0C6C8.ia8.inc.c"
};

// 0x0700C708
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_ri[] = {
#include "levels/menu/main_menu_seg7.0C708.ia8.inc.c"
};

// 0x0700C748
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_ru[] = {
#include "levels/menu/main_menu_seg7.0C748.ia8.inc.c"
};

// 0x0700C788
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_re[] = {
#include "levels/menu/main_menu_seg7.0C788.ia8.inc.c"
};

// 0x0700C7C8
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_ro[] = {
#include "levels/menu/main_menu_seg7.0C7C8.ia8.inc.c"
};

// 0x0700C808
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_wa[] = {
#include "levels/menu/main_menu_seg7.0C808.ia8.inc.c"
};

// 0x0700C848
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_wo[] = {
#include "levels/menu/main_menu_seg7.0C848.ia8.inc.c"
};

// 0x0700C888
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_n[] = {
#include "levels/menu/main_menu_seg7.0C888.ia8.inc.c"
};

// 0x0700C8C8
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_small_a[] = {
#include "levels/menu/main_menu_seg7.0C8C8.ia8.inc.c"
};

// 0x0700C908
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_small_i[] = {
#include "levels/menu/main_menu_seg7.0C908.ia8.inc.c"
};

// 0x0700C948
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_small_u[] = {
#include "levels/menu/main_menu_seg7.0C948.ia8.inc.c"
};

// 0x0700C988
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_small_e[] = {
#include "levels/menu/main_menu_seg7.0C988.ia8.inc.c"
};

// 0x0700C9C8
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_small_o[] = {
#include "levels/menu/main_menu_seg7.0C9C8.ia8.inc.c"
};

// 0x0700CA08
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_small_ya[] = {
#include "levels/menu/main_menu_seg7.0CA08.ia8.inc.c"
};

// 0x0700CA48
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_small_yu[] = {
#include "levels/menu/main_menu_seg7.0CA48.ia8.inc.c"
};

// 0x0700CA88
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_small_yo[] = {
#include "levels/menu/main_menu_seg7.0CA88.ia8.inc.c"
};

// 0x0700CAC8
ALIGNED8 static const Texture texture_menu_font_char_jp_katakana_small_tsu[] = {
#include "levels/menu/main_menu_seg7.0CAC8.ia8.inc.c"
};

// 0x0700CB08
ALIGNED8 static const Texture texture_menu_font_char_jp_A[] = {
#include "levels/menu/main_menu_seg7.0CB08.ia8.inc.c"
};

// 0x0700CB48
ALIGNED8 static const Texture texture_menu_font_char_jp_B[] = {
#include "levels/menu/main_menu_seg7.0CB48.ia8.inc.c"
};

// 0x0700CB88
ALIGNED8 static const Texture texture_menu_font_char_jp_C[] = {
#include "levels/menu/main_menu_seg7.0CB88.ia8.inc.c"
};

// 0x0700CBC8
ALIGNED8 static const Texture texture_menu_font_char_jp_D[] = {
#include "levels/menu/main_menu_seg7.0CBC8.ia8.inc.c"
};

// 0x0700CCC8
ALIGNED8 static const Texture texture_menu_font_char_jp_exclamation[] = {
#include "levels/menu/main_menu_seg7.0CCC8.ia8.inc.c"
};
#endif

// 0x0700AC40
ALIGNED8 static const Texture texture_menu_font_char_0[] = {
#include "levels/menu/main_menu_seg7_us.0AC40.ia8.inc.c"
};

// 0x0700AC80
ALIGNED8 static const Texture texture_menu_font_char_1[] = {
#include "levels/menu/main_menu_seg7_us.0AC80.ia8.inc.c"
};

// 0x0700ACC0
ALIGNED8 static const Texture texture_menu_font_char_2[] = {
#include "levels/menu/main_menu_seg7_us.0ACC0.ia8.inc.c"
};

// 0x0700AD00
ALIGNED8 static const Texture texture_menu_font_char_3[] = {
#include "levels/menu/main_menu_seg7_us.0AD00.ia8.inc.c"
};

// 0x0700AD40
ALIGNED8 static const Texture texture_menu_font_char_4[] = {
#include "levels/menu/main_menu_seg7_us.0AD40.ia8.inc.c"
};

// 0x0700AD80
ALIGNED8 static const Texture texture_menu_font_char_5[] = {
#include "levels/menu/main_menu_seg7_us.0AD80.ia8.inc.c"
};

// 0x0700ADC0
ALIGNED8 static const Texture texture_menu_font_char_6[] = {
#include "levels/menu/main_menu_seg7_us.0ADC0.ia8.inc.c"
};

// 0x0700AE00
ALIGNED8 static const Texture texture_menu_font_char_7[] = {
#include "levels/menu/main_menu_seg7_us.0AE00.ia8.inc.c"
};

// 0x0700AE40
ALIGNED8 static const Texture texture_menu_font_char_8[] = {
#include "levels/menu/main_menu_seg7_us.0AE40.ia8.inc.c"
};

// 0x0700AE80
ALIGNED8 static const Texture texture_menu_font_char_9[] = {
#include "levels/menu/main_menu_seg7_us.0AE80.ia8.inc.c"
};

// 0x0700AEC0
ALIGNED8 static const Texture texture_menu_font_char_A[] = {
#include "levels/menu/main_menu_seg7_us.0AEC0.ia8.inc.c"
};

// 0x0700AF00
ALIGNED8 static const Texture texture_menu_font_char_B[] = {
#include "levels/menu/main_menu_seg7_us.0AF00.ia8.inc.c"
};

// 0x0700AF40
ALIGNED8 static const Texture texture_menu_font_char_C[] = {
#include "levels/menu/main_menu_seg7_us.0AF40.ia8.inc.c"
};

// 0x0700AF80
ALIGNED8 static const Texture texture_menu_font_char_D[] = {
#include "levels/menu/main_menu_seg7_us.0AF80.ia8.inc.c"
};

// 0x0700AFC0
ALIGNED8 static const Texture texture_menu_font_char_E[] = {
#include "levels/menu/main_menu_seg7_us.0AFC0.ia8.inc.c"
};

// 0x0700B000
ALIGNED8 static const Texture texture_menu_font_char_F[] = {
#include "levels/menu/main_menu_seg7_us.0B000.ia8.inc.c"
};

// 0x0700B040
ALIGNED8 static const Texture texture_menu_font_char_G[] = {
#include "levels/menu/main_menu_seg7_us.0B040.ia8.inc.c"
};

// 0x0700B080
ALIGNED8 static const Texture texture_menu_font_char_H[] = {
#include "levels/menu/main_menu_seg7_us.0B080.ia8.inc.c"
};

// 0x0700B0C0
ALIGNED8 static const Texture texture_menu_font_char_I[] = {
#include "levels/menu/main_menu_seg7_us.0B0C0.ia8.inc.c"
};

// 0x0700B100
ALIGNED8 static const Texture texture_menu_font_char_J[] = {
#include "levels/menu/main_menu_seg7_us.0B100.ia8.inc.c"
};

// 0x0700B140
ALIGNED8 static const Texture texture_menu_font_char_K[] = {
#include "levels/menu/main_menu_seg7_us.0B140.ia8.inc.c"
};

// 0x0700B180
ALIGNED8 static const Texture texture_menu_font_char_L[] = {
#include "levels/menu/main_menu_seg7_us.0B180.ia8.inc.c"
};

// 0x0700B1C0
ALIGNED8 static const Texture texture_menu_font_char_M[] = {
#include "levels/menu/main_menu_seg7_us.0B1C0.ia8.inc.c"
};

// 0x0700B200
ALIGNED8 static const Texture texture_menu_font_char_N[] = {
#include "levels/menu/main_menu_seg7_us.0B200.ia8.inc.c"
};

// 0x0700B240
ALIGNED8 static const Texture texture_menu_font_char_O[] = {
#include "levels/menu/main_menu_seg7_us.0B240.ia8.inc.c"
};

// 0x0700B280
ALIGNED8 static const Texture texture_menu_font_char_P[] = {
#include "levels/menu/main_menu_seg7_us.0B280.ia8.inc.c"
};

// 0x0700B2C0
ALIGNED8 static const Texture texture_menu_font_char_Q[] = {
#include "levels/menu/main_menu_seg7_us.0B2C0.ia8.inc.c"
};

// 0x0700B300
ALIGNED8 static const Texture texture_menu_font_char_R[] = {
#include "levels/menu/main_menu_seg7_us.0B300.ia8.inc.c"
};

// 0x0700B340
ALIGNED8 static const Texture texture_menu_font_char_S[] = {
#include "levels/menu/main_menu_seg7_us.0B340.ia8.inc.c"
};

// 0x0700B380
ALIGNED8 static const Texture texture_menu_font_char_T[] = {
#include "levels/menu/main_menu_seg7_us.0B380.ia8.inc.c"
};

// 0x0700B3C0
ALIGNED8 static const Texture texture_menu_font_char_U[] = {
#include "levels/menu/main_menu_seg7_us.0B3C0.ia8.inc.c"
};

// 0x0700B400
ALIGNED8 static const Texture texture_menu_font_char_V[] = {
#include "levels/menu/main_menu_seg7_us.0B400.ia8.inc.c"
};

// 0x0700B440
ALIGNED8 static const Texture texture_menu_font_char_W[] = {
#include "levels/menu/main_menu_seg7_us.0B440.ia8.inc.c"
};

// 0x0700B480
ALIGNED8 static const Texture texture_menu_font_char_X[] = {
#include "levels/menu/main_menu_seg7_us.0B480.ia8.inc.c"
};

// 0x0700B4C0
ALIGNED8 static const Texture texture_menu_font_char_Y[] = {
#include "levels/menu/main_menu_seg7_us.0B4C0.ia8.inc.c"
};

// 0x0700B500
ALIGNED8 static const Texture texture_menu_font_char_Z[] = {
#include "levels/menu/main_menu_seg7_us.0B500.ia8.inc.c"
};

// 0x0700B540
ALIGNED8 static const Texture texture_menu_font_char_coin[] = {
#include "levels/menu/main_menu_seg7_us.0B540.ia8.inc.c"
};

// 0x0700B580
ALIGNED8 static const Texture texture_menu_font_char_multiply[] = {
#include "levels/menu/main_menu_seg7_us.0B580.ia8.inc.c"
};

// 0x0700B5C0
ALIGNED8 static const Texture texture_menu_font_char_star_filled[] = {
#include "levels/menu/main_menu_seg7_us.0B5C0.ia8.inc.c"
};

// 0x0700B600
ALIGNED8 static const Texture texture_menu_font_char_dash[] = {
#include "levels/menu/main_menu_seg7_us.0B600.ia8.inc.c"
};

// 0x0700B640
ALIGNED8 static const Texture texture_menu_font_char_comma[] = {
#include "levels/menu/main_menu_seg7_us.0B640.ia8.inc.c"
};

// 0x0700B680
ALIGNED8 static const Texture texture_menu_font_char_apostrophe[] = {
#include "levels/menu/main_menu_seg7_us.0B680.ia8.inc.c"
};

// 0x0700B6C0
ALIGNED8 static const Texture texture_menu_font_char_exclamation[] = {
#include "levels/menu/main_menu_seg7_us.0B6C0.ia8.inc.c"
};

// 0x0700B700
ALIGNED8 static const Texture texture_menu_font_char_question[] = {
#include "levels/menu/main_menu_seg7_us.0B700.ia8.inc.c"
};

// 0x0700B740
ALIGNED8 static const Texture texture_menu_font_char_mface1[] = {
#include "levels/menu/main_menu_seg7_us.0B740.ia8.inc.c"
};

// 0x0700B780
ALIGNED8 static const Texture texture_menu_font_char_mface2[] = {
#include "levels/menu/main_menu_seg7_us.0B780.ia8.inc.c"
};

// 0x0700B7C0
ALIGNED8 static const Texture texture_menu_font_char_period[] = {
#include "levels/menu/main_menu_seg7_us.0B7C0.ia8.inc.c"
};

// 0x0700B800
ALIGNED8 static const Texture texture_menu_font_char_ampersand[] = {
#include "levels/menu/main_menu_seg7_us.0B800.ia8.inc.c"
};

ALIGNED8 static const Texture texture_menu_font_char_colon[] = {
#include "levels/menu/main_menu_seg7.colon.ia8.inc.c"
};

ALIGNED8 static const Texture texture_menu_font_char_inverted_exclamation_mark[] = {
#include "levels/menu/main_menu_seg7.inverted_exclamation_mark.ia8.inc.c"
};

ALIGNED8 static const Texture texture_menu_font_char_inverted_question_mark[] = {
#include "levels/menu/main_menu_seg7.inverted_question_mark.ia8.inc.c"
};

ALIGNED8 static const u8 texture_menu_font_char_diacritic_grave[] = {
#include "levels/menu/main_menu_seg7.grave.ia8.inc.c"
};

ALIGNED8 static const u8 texture_menu_font_char_diacritic_acute[] = {
#include "levels/menu/main_menu_seg7.acute.ia8.inc.c"
};

ALIGNED8 static const u8 texture_menu_font_char_diacritic_circumflex[] = {
#include "levels/menu/main_menu_seg7.circumflex.ia8.inc.c"
};

ALIGNED8 static const u8 texture_menu_font_char_diacritic_tilde[] = {
#include "levels/menu/main_menu_seg7.tilde_diacritic.ia8.inc.c"
};

ALIGNED8 static const u8 texture_menu_font_char_diacritic_umlaut[] = {
#include "levels/menu/main_menu_seg7.umlaut.ia8.inc.c"
};

ALIGNED8 static const Texture texture_menu_font_char_diacritic_cedilla[] = {
#include "levels/menu/main_menu_seg7.cedilla.ia8.inc.c"
};

ALIGNED8 static const Texture texture_menu_font_missing_character[] = {
#include "levels/menu/main_menu_seg7.missing_character.ia8.inc.c"
};

// Diacritics for the small white menu font
const struct DiacriticLUTEntry menu_font_diacritic_lut[] = {
    [TEXT_DIACRITIC_CIRCUMFLEX_UPPERCASE] = {-1,  5, "ˆ"},
    [TEXT_DIACRITIC_ACUTE_UPPERCASE]      = { 0,  5, "ˊ"},
    [TEXT_DIACRITIC_GRAVE_UPPERCASE]      = { 0,  5, "ˋ"},
    [TEXT_DIACRITIC_TILDE_UPPERCASE]      = { 0,  5, "˜"},
    [TEXT_DIACRITIC_UMLAUT_UPPERCASE]     = { 0,  4, "¨"},
    [TEXT_DIACRITIC_CEDILLA]              = { 0, -2, "¸"},
#ifdef JAPANESE_CHARACTERS
    [TEXT_DIACRITIC_DAKUTEN]              = { 6,  7, "゛"},
    [TEXT_DIACRITIC_HANDAKUTEN]           = { 6,  7, "゜"},
#endif
};

// ASCII lookup table for the small white menu font
const struct AsciiCharLUTEntry menu_font_lut[] = {
    {NULL, 4}, // 32 " "
    {texture_menu_font_char_exclamation, 5}, // 33 "!"
    {NULL, 0}, // 34 "\"" (Unimplemented)
    {NULL, 0}, // 35 "#" (Unimplemented)
    {NULL, 0}, // 36 "$" (Unimplemented)
    {NULL, 0}, // 37 "%" (Unimplemented)
    {texture_menu_font_char_ampersand, 8}, // 38 "&"
    {texture_menu_font_char_apostrophe, 4}, // 39 "'"
    {NULL, 0}, // 40 "(" (Unimplemented)
    {NULL, 0}, // 41 ")" (Unimplemented)
    {NULL, 0}, // 42 "*" (Unimplemented)
    {NULL, 0}, // 43 "+" (Unimplemented)
    {texture_menu_font_char_comma, 4}, // 44 ","
    {texture_menu_font_char_dash, 6}, // 45 "-"
    {texture_menu_font_char_period, 4}, // 46 "."
    {NULL, 0}, // 47 "/" (Unimplemented)
    {texture_menu_font_char_0, 7}, // 48 "0"
    {texture_menu_font_char_1, 7}, // 49 "1"
    {texture_menu_font_char_2, 7}, // 50 "2"
    {texture_menu_font_char_3, 7}, // 51 "3"
    {texture_menu_font_char_4, 7}, // 52 "4"
    {texture_menu_font_char_5, 7}, // 53 "5"
    {texture_menu_font_char_6, 7}, // 54 "6"
    {texture_menu_font_char_7, 7}, // 55 "7"
    {texture_menu_font_char_8, 7}, // 56 "8"
    {texture_menu_font_char_9, 7}, // 57 "9"
    {texture_menu_font_char_colon, 4}, // 58 ":"
    {NULL, 0}, // 59 ";" (Unimplemented)
    {NULL, 0}, // 60 "<" (Unimplemented)
    {NULL, 0}, // 61 "=" (Unimplemented)
    {NULL, 0}, // 62 ">" (Unimplemented)
    {texture_menu_font_char_question, 7}, // 63 "?"
    {NULL, 0}, // 64 "@" (Unimplemented)
    {texture_menu_font_char_A, 6}, // 65 "A"
    {texture_menu_font_char_B, 6}, // 66 "B"
    {texture_menu_font_char_C, 6}, // 67 "C"
    {texture_menu_font_char_D, 6}, // 68 "D"
    {texture_menu_font_char_E, 6}, // 69 "E"
    {texture_menu_font_char_F, 6}, // 70 "F"
    {texture_menu_font_char_G, 6}, // 71 "G"
    {texture_menu_font_char_H, 6}, // 72 "H"
    {texture_menu_font_char_I, 5}, // 73 "I"
    {texture_menu_font_char_J, 6}, // 74 "J"
    {texture_menu_font_char_K, 6}, // 75 "K"
    {texture_menu_font_char_L, 5}, // 76 "L"
    {texture_menu_font_char_M, 8}, // 77 "M"
    {texture_menu_font_char_N, 8}, // 78 "N"
    {texture_menu_font_char_O, 6}, // 79 "O"
    {texture_menu_font_char_P, 6}, // 80 "P"
    {texture_menu_font_char_Q, 6}, // 81 "Q"
    {texture_menu_font_char_R, 6}, // 82 "R"
    {texture_menu_font_char_S, 6}, // 83 "S"
    {texture_menu_font_char_T, 5}, // 84 "T"
    {texture_menu_font_char_U, 6}, // 85 "U"
    {texture_menu_font_char_V, 6}, // 86 "V"
    {texture_menu_font_char_W, 8}, // 87 "W"
    {texture_menu_font_char_X, 7}, // 88 "X"
    {texture_menu_font_char_Y, 6}, // 89 "Y"
    {texture_menu_font_char_Z, 6}, // 90 "Z"
    {NULL, 0}, // 91 "[" (Unimplemented)
    {NULL, 0}, // 92 "\\" (Unimplemented)
    {NULL, 0}, // 93 "]" (Unimplemented)
    {NULL, 0}, // 94 "^" (Unimplemented)
    {NULL, 0}, // 95 "_" (Unimplemented)
    {NULL, 0}, // 96 "`" (Unimplemented)
    {texture_menu_font_char_A, 6}, // 97 "a"
    {texture_menu_font_char_B, 6}, // 98 "b"
    {texture_menu_font_char_C, 6}, // 99 "c"
    {texture_menu_font_char_D, 6}, // 100 "d"
    {texture_menu_font_char_E, 6}, // 101 "e"
    {texture_menu_font_char_F, 6}, // 102 "f"
    {texture_menu_font_char_G, 6}, // 103 "g"
    {texture_menu_font_char_H, 6}, // 104 "h"
    {texture_menu_font_char_I, 5}, // 105 "i"
    {texture_menu_font_char_J, 6}, // 106 "j"
    {texture_menu_font_char_K, 6}, // 107 "k"
    {texture_menu_font_char_L, 5}, // 108 "l"
    {texture_menu_font_char_M, 8}, // 109 "m"
    {texture_menu_font_char_N, 8}, // 110 "n"
    {texture_menu_font_char_O, 6}, // 111 "o"
    {texture_menu_font_char_P, 6}, // 112 "p"
    {texture_menu_font_char_Q, 6}, // 113 "q"
    {texture_menu_font_char_R, 6}, // 114 "r"
    {texture_menu_font_char_S, 6}, // 115 "s"
    {texture_menu_font_char_T, 5}, // 116 "t"
    {texture_menu_font_char_U, 6}, // 117 "u"
    {texture_menu_font_char_V, 6}, // 118 "v"
    {texture_menu_font_char_W, 8}, // 119 "w"
    {texture_menu_font_char_X, 7}, // 120 "x"
    {texture_menu_font_char_Y, 6}, // 121 "y"
    {texture_menu_font_char_Z, 6}, // 122 "z"
    {texture_menu_font_char_mface1, 8}, // 123 "{" (First half of Mario face)
    {NULL, 0}, // 124 "|" (Unimplemented)
    {texture_menu_font_char_mface2, 8}, // 125 "}" (Second half of Mario face)
    {NULL, 0}, // 126 "~" (Unimplemented)
};

// UTF-8 lookup tables for the small white menu font
const struct Utf8CharLUTEntry menu_font_utf8_2byte_lut[] = {
    {0x00A1, 5, 0, texture_menu_font_char_inverted_exclamation_mark}, // ¡
    {0x00A8, 0, 0, texture_menu_font_char_diacritic_umlaut}, // ¨
    {0x00B8, 0, 0, texture_menu_font_char_diacritic_cedilla}, // ¸
    {0x00BF, 7, 0, texture_menu_font_char_inverted_question_mark}, // ¿

    {0x00C0, 6, TEXT_DIACRITIC_GRAVE_UPPERCASE, texture_menu_font_char_A}, // À
    {0x00C1, 6, TEXT_DIACRITIC_ACUTE_UPPERCASE, texture_menu_font_char_A}, // Á
    {0x00C2, 6, TEXT_DIACRITIC_CIRCUMFLEX_UPPERCASE, texture_menu_font_char_A}, // Â
    {0x00C3, 6, TEXT_DIACRITIC_TILDE_UPPERCASE, texture_menu_font_char_A}, // Ã
    {0x00C4, 6, TEXT_DIACRITIC_UMLAUT_UPPERCASE, texture_menu_font_char_A}, // Ä

    {0x00C7, 6, TEXT_DIACRITIC_CEDILLA, texture_menu_font_char_C}, // Ç
    {0x00C8, 6, TEXT_DIACRITIC_GRAVE_UPPERCASE, texture_menu_font_char_E}, // È
    {0x00C9, 6, TEXT_DIACRITIC_ACUTE_UPPERCASE, texture_menu_font_char_E}, // É
    {0x00CA, 6, TEXT_DIACRITIC_CIRCUMFLEX_UPPERCASE, texture_menu_font_char_E}, // Ê
    {0x00CB, 6, TEXT_DIACRITIC_UMLAUT_UPPERCASE, texture_menu_font_char_E}, // Ë

    {0x00CC, 5, TEXT_DIACRITIC_GRAVE_UPPERCASE, texture_menu_font_char_I}, // Ì
    {0x00CD, 5, TEXT_DIACRITIC_ACUTE_UPPERCASE, texture_menu_font_char_I}, // Í
    {0x00CE, 5, TEXT_DIACRITIC_CIRCUMFLEX_UPPERCASE, texture_menu_font_char_I}, // Î
    {0x00CF, 5, TEXT_DIACRITIC_UMLAUT_UPPERCASE, texture_menu_font_char_I}, // Ï

    {0x00D1, 6, TEXT_DIACRITIC_TILDE_UPPERCASE, texture_menu_font_char_N}, // Ñ
    {0x00D2, 6, TEXT_DIACRITIC_GRAVE_UPPERCASE, texture_menu_font_char_O}, // Ò
    {0x00D3, 6, TEXT_DIACRITIC_ACUTE_UPPERCASE, texture_menu_font_char_O}, // Ó
    {0x00D4, 6, TEXT_DIACRITIC_CIRCUMFLEX_UPPERCASE, texture_menu_font_char_O}, // Ô
    {0x00D5, 6, TEXT_DIACRITIC_TILDE_UPPERCASE, texture_menu_font_char_O}, // Õ
    {0x00D6, 6, TEXT_DIACRITIC_UMLAUT_UPPERCASE, texture_menu_font_char_O}, // Ö

    {0x00D7, 9, 0, texture_menu_font_char_multiply}, // ×

    {0x00D9, 6, TEXT_DIACRITIC_GRAVE_UPPERCASE, texture_menu_font_char_U}, // Ù
    {0x00DA, 6, TEXT_DIACRITIC_ACUTE_UPPERCASE, texture_menu_font_char_U}, // Ú
    {0x00DB, 6, TEXT_DIACRITIC_CIRCUMFLEX_UPPERCASE, texture_menu_font_char_U}, // Û
    {0x00DC, 6, TEXT_DIACRITIC_UMLAUT_UPPERCASE, texture_menu_font_char_U}, // Ü

    {0x02C6, 0, 0, texture_menu_font_char_diacritic_circumflex}, // ˆ
    {0x02CA, 0, 0, texture_menu_font_char_diacritic_acute}, // ˊ
    {0x02CB, 0, 0, texture_menu_font_char_diacritic_grave}, // ˋ
    {0x02DC, 0, 0, texture_menu_font_char_diacritic_tilde}, // ˜
};

const struct Utf8CharLUTEntry menu_font_utf8_3byte_lut[] = {
    {0x2605, 10, 0, texture_menu_font_char_star_filled}, // ★
    {0x272A, 8, 0, texture_menu_font_char_coin}, // ✪

#ifdef JAPANESE_CHARACTERS
    {0x3000, 9, 0, NULL}, // "　" (ideographic space)
    {0x3041, 9, 0, texture_menu_font_char_jp_hiragana_small_a}, // ぁ
    {0x3042, 9, 0, texture_menu_font_char_jp_hiragana_a}, // あ
    {0x3043, 9, 0, texture_menu_font_char_jp_hiragana_small_i}, // ぃ
    {0x3044, 9, 0, texture_menu_font_char_jp_hiragana_i}, // い
    {0x3045, 9, 0, texture_menu_font_char_jp_hiragana_small_u}, // ぅ
    {0x3046, 9, 0, texture_menu_font_char_jp_hiragana_u}, // う
    {0x3047, 9, 0, texture_menu_font_char_jp_hiragana_small_e}, // ぇ
    {0x3048, 9, 0, texture_menu_font_char_jp_hiragana_e}, // え
    {0x3049, 9, 0, texture_menu_font_char_jp_hiragana_small_o}, // ぉ
    {0x304A, 9, 0, texture_menu_font_char_jp_hiragana_o}, // お
    {0x304B, 9, 0, texture_menu_font_char_jp_hiragana_ka}, // か
    {0x304C, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_ka}, // が
    {0x304D, 9, 0, texture_menu_font_char_jp_hiragana_ki}, // き
    {0x304E, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_ki}, // ぎ
    {0x304F, 9, 0, texture_menu_font_char_jp_hiragana_ku}, // く
    {0x3050, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_ku}, // ぐ
    {0x3051, 9, 0, texture_menu_font_char_jp_hiragana_ke}, // け
    {0x3052, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_ke}, // げ
    {0x3053, 9, 0, texture_menu_font_char_jp_hiragana_ko}, // こ
    {0x3054, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_ko}, // ご
    {0x3055, 9, 0, texture_menu_font_char_jp_hiragana_sa}, // さ
    {0x3056, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_sa}, // ざ
    {0x3057, 9, 0, texture_menu_font_char_jp_hiragana_shi}, // し
    {0x3058, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_shi}, // じ
    {0x3059, 9, 0, texture_menu_font_char_jp_hiragana_su}, // す
    {0x305A, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_su}, // ず
    {0x305B, 9, 0, texture_menu_font_char_jp_hiragana_se}, // せ
    {0x305C, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_se}, // ぜ
    {0x305D, 9, 0, texture_menu_font_char_jp_hiragana_so}, // そ
    {0x305E, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_so}, // ぞ
    {0x305F, 9, 0, texture_menu_font_char_jp_hiragana_ta}, // た
    {0x3060, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_ta}, // だ
    {0x3061, 9, 0, texture_menu_font_char_jp_hiragana_chi}, // ち
    {0x3062, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_chi}, // ぢ
    {0x3063, 9, 0, texture_menu_font_char_jp_hiragana_small_tsu}, // っ
    {0x3064, 9, 0, texture_menu_font_char_jp_hiragana_tsu}, // つ
    {0x3065, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_tsu}, // づ
    {0x3066, 9, 0, texture_menu_font_char_jp_hiragana_te}, // て
    {0x3067, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_te}, // で
    {0x3068, 9, 0, texture_menu_font_char_jp_hiragana_to}, // と
    {0x3069, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_to}, // ど
    {0x306A, 9, 0, texture_menu_font_char_jp_hiragana_na}, // な
    {0x306B, 9, 0, texture_menu_font_char_jp_hiragana_ni}, // に
    {0x306C, 9, 0, texture_menu_font_char_jp_hiragana_nu}, // ぬ
    {0x306D, 9, 0, texture_menu_font_char_jp_hiragana_ne}, // ね
    {0x306E, 9, 0, texture_menu_font_char_jp_hiragana_no}, // の
    {0x306F, 9, 0, texture_menu_font_char_jp_hiragana_ha}, // は
    {0x3070, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_ha}, // ば
    {0x3071, 9, TEXT_DIACRITIC_HANDAKUTEN, texture_menu_font_char_jp_hiragana_ha}, // ぱ
    {0x3072, 9, 0, texture_menu_font_char_jp_hiragana_hi}, // ひ
    {0x3073, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_hi}, // び
    {0x3074, 9, TEXT_DIACRITIC_HANDAKUTEN, texture_menu_font_char_jp_hiragana_hi}, // ぴ
    {0x3075, 9, 0, texture_menu_font_char_jp_hiragana_fu}, // ふ
    {0x3076, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_fu}, // ぶ
    {0x3077, 9, TEXT_DIACRITIC_HANDAKUTEN, texture_menu_font_char_jp_hiragana_fu}, // ぷ
    {0x3078, 9, 0, texture_menu_font_char_jp_hiragana_he}, // へ
    {0x3079, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_he}, // べ
    {0x307A, 9, TEXT_DIACRITIC_HANDAKUTEN, texture_menu_font_char_jp_hiragana_he}, // ぺ
    {0x307B, 9, 0, texture_menu_font_char_jp_hiragana_ho}, // ほ
    {0x307C, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_hiragana_ho}, // ぼ
    {0x307D, 9, TEXT_DIACRITIC_HANDAKUTEN, texture_menu_font_char_jp_hiragana_ho}, // ぽ
    {0x307E, 9, 0, texture_menu_font_char_jp_hiragana_ma}, // ま
    {0x307F, 9, 0, texture_menu_font_char_jp_hiragana_mi}, // み
    {0x3080, 9, 0, texture_menu_font_char_jp_hiragana_mu}, // む
    {0x3081, 9, 0, texture_menu_font_char_jp_hiragana_me}, // め
    {0x3082, 9, 0, texture_menu_font_char_jp_hiragana_mo}, // も
    {0x3083, 9, 0, texture_menu_font_char_jp_hiragana_small_ya}, // ゃ
    {0x3084, 9, 0, texture_menu_font_char_jp_hiragana_ya}, // や
    {0x3085, 9, 0, texture_menu_font_char_jp_hiragana_small_yu}, // ゅ
    {0x3086, 9, 0, texture_menu_font_char_jp_hiragana_yu}, // ゆ
    {0x3087, 9, 0, texture_menu_font_char_jp_hiragana_small_yo}, // ょ
    {0x3088, 9, 0, texture_menu_font_char_jp_hiragana_yo}, // よ
    {0x3089, 9, 0, texture_menu_font_char_jp_hiragana_ra}, // ら
    {0x308A, 9, 0, texture_menu_font_char_jp_hiragana_ri}, // り
    {0x308B, 9, 0, texture_menu_font_char_jp_hiragana_ru}, // る
    {0x308C, 9, 0, texture_menu_font_char_jp_hiragana_re}, // れ
    {0x308D, 9, 0, texture_menu_font_char_jp_hiragana_ro}, // ろ
    {0x308F, 9, 0, texture_menu_font_char_jp_hiragana_wa}, // わ
    {0x3092, 9, 0, texture_menu_font_char_jp_hiragana_wo}, // を
    {0x3093, 9, 0, texture_menu_font_char_jp_hiragana_n}, // ん

    {0x309B, 0, 0, texture_menu_font_char_jp_dakuten}, // ゛
    {0x309C, 0, 0, texture_menu_font_char_jp_handakuten}, // ゜

    {0x30A1, 9, 0, texture_menu_font_char_jp_katakana_small_a}, // ァ
    {0x30A2, 9, 0, texture_menu_font_char_jp_katakana_a}, // ア
    {0x30A3, 9, 0, texture_menu_font_char_jp_katakana_small_i}, // ィ
    {0x30A4, 9, 0, texture_menu_font_char_jp_katakana_i}, // イ
    {0x30A5, 9, 0, texture_menu_font_char_jp_katakana_small_u}, // ゥ
    {0x30A6, 9, 0, texture_menu_font_char_jp_katakana_u}, // ウ
    {0x30A7, 9, 0, texture_menu_font_char_jp_katakana_small_e}, // ェ
    {0x30A8, 9, 0, texture_menu_font_char_jp_katakana_e}, // エ
    {0x30A9, 9, 0, texture_menu_font_char_jp_katakana_small_o}, // ォ
    {0x30AA, 9, 0, texture_menu_font_char_jp_katakana_o}, // オ
    {0x30AB, 9, 0, texture_menu_font_char_jp_katakana_ka}, // カ
    {0x30AC, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_ka}, // ガ
    {0x30AD, 9, 0, texture_menu_font_char_jp_katakana_ki}, // キ
    {0x30AE, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_ki}, // ギ
    {0x30AF, 9, 0, texture_menu_font_char_jp_katakana_ku}, // ク
    {0x30B0, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_ku}, // グ
    {0x30B1, 9, 0, texture_menu_font_char_jp_katakana_ke}, // ケ
    {0x30B2, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_ke}, // ゲ
    {0x30B3, 9, 0, texture_menu_font_char_jp_katakana_ko}, // コ
    {0x30B4, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_ko}, // ゴ
    {0x30B5, 9, 0, texture_menu_font_char_jp_katakana_sa}, // サ
    {0x30B6, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_sa}, // ザ
    {0x30B7, 9, 0, texture_menu_font_char_jp_katakana_shi}, // シ
    {0x30B8, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_shi}, // ジ
    {0x30B9, 9, 0, texture_menu_font_char_jp_katakana_su}, // ス
    {0x30BA, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_su}, // ズ
    {0x30BB, 9, 0, texture_menu_font_char_jp_katakana_se}, // セ
    {0x30BC, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_se}, // ゼ
    {0x30BD, 9, 0, texture_menu_font_char_jp_katakana_so}, // ソ
    {0x30BE, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_so}, // ゾ
    {0x30BF, 9, 0, texture_menu_font_char_jp_katakana_ta}, // タ
    {0x30C0, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_ta}, // ダ
    {0x30C1, 9, 0, texture_menu_font_char_jp_katakana_chi}, // チ
    {0x30C2, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_chi}, // ヂ
    {0x30C3, 9, 0, texture_menu_font_char_jp_katakana_small_tsu}, // ッ
    {0x30C4, 9, 0, texture_menu_font_char_jp_katakana_tsu}, // ツ
    {0x30C5, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_tsu}, // ヅ
    {0x30C6, 9, 0, texture_menu_font_char_jp_katakana_te}, // テ
    {0x30C7, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_te}, // デ
    {0x30C8, 9, 0, texture_menu_font_char_jp_katakana_to}, // ト
    {0x30C9, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_to}, // ド
    {0x30CA, 9, 0, texture_menu_font_char_jp_katakana_na}, // ナ
    {0x30CB, 9, 0, texture_menu_font_char_jp_katakana_ni}, // ニ
    {0x30CC, 9, 0, texture_menu_font_char_jp_katakana_nu}, // ヌ
    {0x30CD, 9, 0, texture_menu_font_char_jp_katakana_ne}, // ネ
    {0x30CE, 9, 0, texture_menu_font_char_jp_katakana_no}, // ノ
    {0x30CF, 9, 0, texture_menu_font_char_jp_katakana_ha}, // ハ
    {0x30D0, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_ha}, // バ
    {0x30D1, 9, TEXT_DIACRITIC_HANDAKUTEN, texture_menu_font_char_jp_katakana_ha}, // パ
    {0x30D2, 9, 0, texture_menu_font_char_jp_katakana_hi}, // ヒ
    {0x30D3, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_hi}, // ビ
    {0x30D4, 9, TEXT_DIACRITIC_HANDAKUTEN, texture_menu_font_char_jp_katakana_hi}, // ピ
    {0x30D5, 9, 0, texture_menu_font_char_jp_katakana_fu}, // フ
    {0x30D6, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_fu}, // ブ
    {0x30D7, 9, TEXT_DIACRITIC_HANDAKUTEN, texture_menu_font_char_jp_katakana_fu}, // プ
    {0x30D8, 9, 0, texture_menu_font_char_jp_katakana_he}, // ヘ
    {0x30D9, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_he}, // ベ
    {0x30DA, 9, TEXT_DIACRITIC_HANDAKUTEN, texture_menu_font_char_jp_katakana_he}, // ペ
    {0x30DB, 9, 0, texture_menu_font_char_jp_katakana_ho}, // ホ
    {0x30DC, 9, TEXT_DIACRITIC_DAKUTEN, texture_menu_font_char_jp_katakana_ho}, // ボ
    {0x30DD, 9, TEXT_DIACRITIC_HANDAKUTEN, texture_menu_font_char_jp_katakana_ho}, // ポ
    {0x30DE, 9, 0, texture_menu_font_char_jp_katakana_ma}, // マ
    {0x30DF, 9, 0, texture_menu_font_char_jp_katakana_mi}, // ミ
    {0x30E0, 9, 0, texture_menu_font_char_jp_katakana_mu}, // ム
    {0x30E1, 9, 0, texture_menu_font_char_jp_katakana_me}, // メ
    {0x30E2, 9, 0, texture_menu_font_char_jp_katakana_mo}, // モ
    {0x30E3, 9, 0, texture_menu_font_char_jp_katakana_small_ya}, // ャ
    {0x30E4, 9, 0, texture_menu_font_char_jp_katakana_ya}, // ヤ
    {0x30E5, 9, 0, texture_menu_font_char_jp_katakana_small_yu}, // ュ
    {0x30E6, 9, 0, texture_menu_font_char_jp_katakana_yu}, // ユ
    {0x30E7, 9, 0, texture_menu_font_char_jp_katakana_small_yo}, // ョ
    {0x30E8, 9, 0, texture_menu_font_char_jp_katakana_yo}, // ヨ
    {0x30E9, 9, 0, texture_menu_font_char_jp_katakana_ra}, // ラ
    {0x30EA, 9, 0, texture_menu_font_char_jp_katakana_ri}, // リ
    {0x30EB, 9, 0, texture_menu_font_char_jp_katakana_ru}, // ル
    {0x30EC, 9, 0, texture_menu_font_char_jp_katakana_re}, // レ
    {0x30ED, 9, 0, texture_menu_font_char_jp_katakana_ro}, // ロ
    {0x30EF, 9, 0, texture_menu_font_char_jp_katakana_wa}, // ワ
    {0x30F2, 9, 0, texture_menu_font_char_jp_katakana_wo}, // ヲ
    {0x30F3, 9, 0, texture_menu_font_char_jp_katakana_n}, // ン
    {0x30FC, 9, 0, texture_menu_font_char_jp_long_vowel}, // ー

    {0xFF01, 9, 0, texture_menu_font_char_jp_exclamation}, // ！
    {0xFF10, 9, 0, texture_menu_font_char_jp_0}, // ０
    {0xFF11, 9, 0, texture_menu_font_char_jp_1}, // １
    {0xFF12, 9, 0, texture_menu_font_char_jp_2}, // ２
    {0xFF13, 9, 0, texture_menu_font_char_jp_3}, // ３
    {0xFF14, 9, 0, texture_menu_font_char_jp_4}, // ４
    {0xFF15, 9, 0, texture_menu_font_char_jp_5}, // ５
    {0xFF16, 9, 0, texture_menu_font_char_jp_6}, // ６
    {0xFF17, 9, 0, texture_menu_font_char_jp_7}, // ７
    {0xFF18, 9, 0, texture_menu_font_char_jp_8}, // ８
    {0xFF19, 9, 0, texture_menu_font_char_jp_9}, // ９
    {0xFF21, 9, 0, texture_menu_font_char_jp_A}, // Ａ
    {0xFF22, 9, 0, texture_menu_font_char_jp_B}, // Ｂ
    {0xFF23, 9, 0, texture_menu_font_char_jp_C}, // Ｃ
    {0xFF24, 9, 0, texture_menu_font_char_jp_D}, // Ｄ
#endif
};

const struct Utf8CharLUTEntry menu_font_utf8_4byte_lut[] = {

};

const struct Utf8CharLUTEntry menu_font_utf8_missing_char = {0, 8, 0, texture_menu_font_missing_character};

const struct Utf8LUT menu_font_utf8_lut = {
    menu_font_utf8_2byte_lut,
    menu_font_utf8_3byte_lut,
    menu_font_utf8_4byte_lut,
    ARRAY_COUNT(menu_font_utf8_2byte_lut),
    ARRAY_COUNT(menu_font_utf8_3byte_lut),
    ARRAY_COUNT(menu_font_utf8_4byte_lut),
    &menu_font_utf8_missing_char,
};

// Menu small font print table
//     texture_menu_font_char_jp_0, texture_menu_font_char_jp_1, texture_menu_font_char_jp_2, texture_menu_font_char_jp_3,
//     texture_menu_font_char_jp_4, texture_menu_font_char_jp_5, texture_menu_font_char_jp_6, texture_menu_font_char_jp_7,
//     texture_menu_font_char_jp_8, texture_menu_font_char_jp_9, texture_menu_font_char_jp_A, texture_menu_font_char_jp_B,
//     texture_menu_font_char_jp_C, texture_menu_font_char_jp_D,                   0x0,                      0x0,
//                               0x0,                      0x0,                      0x0,                      0x0,
//                               0x0,                      0x0,                      0x0,                      0x0,
//                               0x0,                      0x0,                      0x0,                      0x0,
//                               0x0,                      0x0,                      0x0,                      0x0,
//                               0x0,                      0x0,                      0x0,                      0x0,
//                               0x0,                      0x0,                      0x0,                      0x0,
//                               0x0,                      0x0,                      0x0,                      0x0,
//                               0x0,                      0x0,                      0x0,                      0x0,
//                               0x0,                      0x0,                      0x0,                      0x0,
//                               0x0,                      0x0,                      0x0,                      0x0,
//                               0x0,                      0x0,                      0x0,                      0x0,
//                               0x0,                      0x0,                      0x0,                      0x0,
//     texture_menu_font_char_jp_hiragana_a, texture_menu_font_char_jp_hiragana_i, texture_menu_font_char_jp_hiragana_u, texture_menu_font_char_jp_hiragana_c,
//     texture_menu_font_char_jp_hiragana_o, texture_menu_font_char_jp_hiragana_ka, texture_menu_font_char_jp_hiragana_ki, texture_menu_font_char_jp_hiragana_ku,
//     texture_menu_font_char_jp_hiragana_ke, texture_menu_font_char_jp_hiragana_ko, texture_menu_font_char_jp_hiragana_sa, texture_menu_font_char_jp_hiragana_shi,
//     texture_menu_font_char_jp_hiragana_su, texture_menu_font_char_jp_hiragana_se, texture_menu_font_char_jp_hiragana_so, texture_menu_font_char_jp_hiragana_ta,
//     texture_menu_font_char_jp_hiragana_chi, texture_menu_font_char_jp_hiragana_tsu, texture_menu_font_char_jp_hiragana_te, texture_menu_font_char_jp_hiragana_to,
//     texture_menu_font_char_jp_hiragana_na, texture_menu_font_char_jp_hiragana_ni, texture_menu_font_char_jp_hiragana_nu, texture_menu_font_char_jp_hiragana_ne,
//     texture_menu_font_char_jp_hiragana_no, texture_menu_font_char_jp_hiragana_ha, texture_menu_font_char_jp_hiragana_hi, texture_menu_font_char_jp_hiragana_hu,
//     texture_menu_font_char_jp_hiragana_he, texture_menu_font_char_jp_hiragana_ho, texture_menu_font_char_jp_hiragana_ma, texture_menu_font_char_jp_hiragana_mi,
//     texture_menu_font_char_jp_hiragana_mu, texture_menu_font_char_jp_hiragana_me, texture_menu_font_char_jp_hiragana_mo, texture_menu_font_char_jp_hiragana_ya,
//     texture_menu_font_char_jp_hiragana_yu, texture_menu_font_char_jp_hiragana_yo, texture_menu_font_char_jp_hiragana_ra, texture_menu_font_char_jp_hiragana_ri,
//     texture_menu_font_char_jp_hiragana_ru, texture_menu_font_char_jp_hiragana_re, texture_menu_font_char_jp_hiragana_ro, texture_menu_font_char_jp_hiragana_wa,
//     texture_menu_font_char_jp_hiragana_wo, texture_menu_font_char_jp_hiragana_n, 0x0,                                0x0,
//     texture_menu_font_char_jp_katakana_a, texture_menu_font_char_jp_katakana_i, texture_menu_font_char_jp_katakana_u, texture_menu_font_char_jp_katakana_e,
//     texture_menu_font_char_jp_katakana_o, texture_menu_font_char_jp_katakana_ka, texture_menu_font_char_jp_katakana_ki, texture_menu_font_char_jp_katakana_ku,
//     texture_menu_font_char_jp_katakana_ke, texture_menu_font_char_jp_katakana_ko, texture_menu_font_char_jp_katakana_sa, texture_menu_font_char_jp_katakana_shi,
//     texture_menu_font_char_jp_katakana_su, texture_menu_font_char_jp_katakana_se, texture_menu_font_char_jp_katakana_so, texture_menu_font_char_jp_katakana_ta,
//     texture_menu_font_char_jp_katakana_chi, texture_menu_font_char_jp_katakana_tsu, texture_menu_font_char_jp_katakana_te, texture_menu_font_char_jp_katakana_to,
//     texture_menu_font_char_jp_katakana_na, texture_menu_font_char_jp_katakana_ni, texture_menu_font_char_jp_katakana_nu, texture_menu_font_char_jp_katakana_ne,
//     texture_menu_font_char_jp_katakana_no, texture_menu_font_char_jp_katakana_ha, texture_menu_font_char_jp_katakana_hi, texture_menu_font_char_jp_katakana_hu,
//     texture_menu_font_char_jp_katakana_he, texture_menu_font_char_jp_katakana_ho, texture_menu_font_char_jp_katakana_ma, texture_menu_font_char_jp_katakana_mi,
//     texture_menu_font_char_jp_katakana_mu, texture_menu_font_char_jp_katakana_me, texture_menu_font_char_jp_katakana_mo, texture_menu_font_char_jp_katakana_ya,
//     texture_menu_font_char_jp_katakana_yu, texture_menu_font_char_jp_katakana_yo, texture_menu_font_char_jp_katakana_ra, texture_menu_font_char_jp_katakana_ri,
//     texture_menu_font_char_jp_katakana_ru, texture_menu_font_char_jp_katakana_re, texture_menu_font_char_jp_katakana_ro, texture_menu_font_char_jp_katakana_wa,
//     texture_menu_font_char_jp_katakana_wo, texture_menu_font_char_jp_katakana_n,                                   0x0, texture_menu_font_char_jp_long_vowel,
//     texture_menu_font_char_jp_hiragana_small_e, texture_menu_font_char_jp_hiragana_small_tsu, texture_menu_font_char_jp_hiragana_small_ya, texture_menu_font_char_jp_hiragana_small_yu,
//     texture_menu_font_char_jp_hiragana_small_yo, texture_menu_font_char_jp_hiragana_small_a, texture_menu_font_char_jp_hiragana_small_i, texture_menu_font_char_jp_hiragana_small_u,
//     texture_menu_font_char_jp_hiragana_small_o,          0x0,                      0x0,                      0x0,
//     0x0,                      0x0,                      0x0,                      0x0,
//     0x0,                      0x0,                      0x0,                      0x0,
//     0x0,                      0x0,                      0x0,                      0x0,
//     0x0,                      0x0,                      0x0,                      0x0,
//     0x0,                      0x0,                      0x0,                      0x0,
//     0x0,                      0x0,                      0x0,                      0x0,
//     0x0,                      0x0,                      0x0,                      0x0,
//     0x0,                      0x0,                      0x0,                      0x0,
//     0x0,                      0x0,                      0x0,                      0x0,
//     texture_menu_font_char_jp_katakana_small_e, texture_menu_font_char_jp_katakana_small_tsu, texture_menu_font_char_jp_katakana_small_ya, texture_menu_font_char_jp_katakana_small_yu,
//     texture_menu_font_char_jp_katakana_small_yo, texture_menu_font_char_jp_katakana_small_a, texture_menu_font_char_jp_katakana_small_i, texture_menu_font_char_jp_katakana_small_u,
//     texture_menu_font_char_jp_katakana_small_o, 0x0,                      0x0,                      0x0,
//     0x0,                      0x0,                      0x0,                      0x0,
//     0x0,                      0x0,                      0x0,                      0x0,
//     0x0,                      0x0,                      0x0,                      0x0,
//     0x0,                      0x0,                      0x0,                      0x0,
//     0x0,                      0x0,                      0x0,                      0x0,
//     texture_menu_font_char_jp_dakuten, texture_menu_font_char_jp_handakuten, texture_menu_font_char_jp_exclamation, 0x0,
//     0x0,                      0x0,                      0x0,                      0x0,
//     0x0,                      texture_menu_font_char_jp_coin, texture_menu_font_char_jp_star_filled, texture_menu_font_char_jp_multiply,
//     0x0,                      0x0,                      0x0,                      0x0,

// 0x0700D108 - 0x0700D160
const Gfx dl_menu_ia8_text_begin[] = {
    gsDPPipeSync(),
    gsDPSetTexturePersp(G_TP_NONE),
    gsDPSetCombineMode(G_CC_FADEA, G_CC_FADEA),
    gsDPSetEnvColor(255, 255, 255, 255),
    gsDPSetRenderMode(G_RM_AA_XLU_SURF, G_RM_AA_XLU_SURF2),
    gsDPSetTextureFilter(G_TF_POINT),
    gsDPSetTile(G_IM_FMT_IA, G_IM_SIZ_8b, 0, 0, G_TX_LOADTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_IA, G_IM_SIZ_8b, 1, 0, G_TX_RENDERTILE, 0, G_TX_CLAMP, 3, G_TX_NOLOD, G_TX_CLAMP, 3, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (8 - 1) << G_TEXTURE_IMAGE_FRAC, (8 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPEndDisplayList(),
};

// 0x0700D160 - 0x0700D1A0
const Gfx dl_menu_ia8_text_end[] = {
    gsDPPipeSync(),
    gsDPSetTexturePersp(G_TP_PERSP),
    gsDPSetRenderMode(G_RM_AA_ZB_OPA_SURF, G_RM_AA_ZB_OPA_SURF2),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsDPSetEnvColor(255, 255, 255, 255),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsDPSetTextureFilter(G_TF_BILERP),
    gsSPEndDisplayList(),
};

#ifdef MULTILANG

// 0x0700BDA0 - 0x0700CDA0
ALIGNED8 static const Texture texture_menu_course_upper[] = {
#include "levels/menu/main_menu_seg7.course_upper.rgba16.inc.c"
};

// 0x0700EDA0 - 0x0700FDA0
ALIGNED8 static const Texture texture_menu_course_lower[] = {
#include "levels/menu/main_menu_seg7.course_lower.rgba16.inc.c"
};

#else

// 0x0700D1A8 - 0x0700E1A8
ALIGNED8 static const Texture texture_menu_course_upper[] = {
#include "levels/menu/main_menu_seg7.0D1A8.rgba16.inc.c"
};

// 0x0700E1A8 - 0x0700F1A8
ALIGNED8 static const Texture texture_menu_course_lower[] = {
#include "levels/menu/main_menu_seg7.0E1A8.rgba16.inc.c"
};
#endif

// 0x0700F1A8 - 0x0700F1E8
static const Vtx vertex_menu_course_upper[] = {
    {{{   -32,      0,      0}, 0, {     0,   1984}, {0x00, 0x00, 0x7f, 0x00}}},
    {{{    32,      0,      0}, 0, {  4032,   1984}, {0x00, 0x00, 0x7f, 0x00}}},
    {{{    32,     32,      0}, 0, {  4032,      0}, {0x00, 0x00, 0x7f, 0x00}}},
    {{{   -32,     32,      0}, 0, {     0,      0}, {0x00, 0x00, 0x7f, 0x00}}},
};

// 0x0700F1E8 - 0x0700F228
static const Vtx vertex_menu_course_lower[] = {
    {{{   -32,    -32,      0}, 0, {     0,   1984}, {0x00, 0x00, 0x7f, 0x00}}},
    {{{    32,    -32,      0}, 0, {  4032,   1984}, {0x00, 0x00, 0x7f, 0x00}}},
    {{{    32,      0,      0}, 0, {  4032,      0}, {0x00, 0x00, 0x7f, 0x00}}},
    {{{   -32,      0,      0}, 0, {     0,      0}, {0x00, 0x00, 0x7f, 0x00}}},
};

// 0x0700F228 - 0x0700F2F8
const Gfx dl_menu_rgba16_wood_course[] = {
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_DECALRGBA, G_CC_DECALRGBA),
    gsSPTexture(0x8000, 0x8000, 0, G_TX_RENDERTILE, G_ON),
    gsDPSetRenderMode(G_RM_AA_TEX_EDGE, G_RM_AA_TEX_EDGE2),
#ifdef MULTILANG
    gsSPEndDisplayList(),
};
const Gfx dl_menu_rgba16_wood_course_end[] = {
#else
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_course_upper),
#endif
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 16, 0, G_TX_RENDERTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (64 - 1) << G_TEXTURE_IMAGE_FRAC, (32 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPVertex(vertex_menu_course_upper, 4, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  0,  2,  3, 0x0),
    gsDPPipeSync(),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_course_lower),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, 64 * 32 - 1, CALC_DXT(64, G_IM_SIZ_16b_BYTES)),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 16, 0, G_TX_RENDERTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (64 - 1) << G_TEXTURE_IMAGE_FRAC, (32 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPVertex(vertex_menu_course_lower, 4, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  0,  2,  3, 0x0),
    gsDPSetRenderMode(G_RM_AA_ZB_OPA_SURF, G_RM_AA_ZB_OPA_SURF2),
    gsSPTexture(0x0001, 0x0001, 0, G_TX_RENDERTILE, G_OFF),
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPEndDisplayList(),
};

#ifdef MULTILANG
// 0x0700FEF0 - 0x0700FF00
const Gfx dl_menu_texture_course_upper[] = {
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_course_upper),
    gsSPEndDisplayList(),
};

#ifdef ENABLE_FRENCH
// 0x0700CDA0 - 0x0700DDA0
ALIGNED8 static const Texture texture_menu_niveau_upper[] = {
#include "levels/menu/main_menu_seg7.niveau_upper.rgba16.inc.c"
};

// 0x0700FF00 - 0x0700FF10
const Gfx dl_menu_texture_niveau_upper[] = {
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_niveau_upper),
    gsSPEndDisplayList(),
};
#endif

#ifdef ENABLE_GERMAN
// 0x0700DDA0 - 0x0700EDA0
ALIGNED8 static const Texture texture_menu_kurs_upper[] = {
#include "levels/menu/main_menu_seg7.kurs_upper.rgba16.inc.c"
};

// 0x0700FF10 - 0x0700FF20
const Gfx dl_menu_texture_kurs_upper[] = {
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_kurs_upper),
    gsSPEndDisplayList(),
};
#endif

#ifdef ENABLE_SPANISH
ALIGNED8 static const Texture texture_menu_nivel_upper[] = {
#include "levels/menu/main_menu_seg7.nivel_upper.rgba16.inc.c"
};

const Gfx dl_menu_texture_nivel_upper[] = {
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_menu_nivel_upper),
    gsSPEndDisplayList(),
};
#endif
#endif

// 0x0700F2F8 - 0x0700F328
const Collision main_menu_seg7_collision[] = {
    COL_INIT(),
    COL_VERTEX_INIT(0x4),
    COL_VERTEX( 8192, -1000, -8192),
    COL_VERTEX(-8192, -1000, -8192),
    COL_VERTEX(-8192, -1000,  8192),
    COL_VERTEX( 8192, -1000,  8192),
    COL_TRI_INIT(SURFACE_DEFAULT, 2),
    COL_TRI(0, 1, 2),
    COL_TRI(0, 2, 3),
    COL_TRI_STOP(),
    COL_END(),
};

#ifdef MULTILANG

// Duplicate course name tables; the main menu needs all languages loaded at
// once since it switches language, so the copies in segment 19 aren't good
// enough.

#define COURSE_NAME_TABLE course_strings_en_table
#define COURSE_FILE "us/courses.h"
#include "text/define_courses.inc.c"
#undef COURSE_NAME_TABLE
#undef COURSE_FILE

#ifdef ENABLE_FRENCH
#define COURSE_NAME_TABLE course_strings_fr_table
#define COURSE_FILE "fr/courses.h"
#include "text/define_courses.inc.c"
#undef COURSE_NAME_TABLE
#undef COURSE_FILE
#endif

#ifdef ENABLE_GERMAN
#define COURSE_NAME_TABLE course_strings_de_table
#define COURSE_FILE "de/courses.h"
#include "text/define_courses.inc.c"
#undef COURSE_NAME_TABLE
#undef COURSE_FILE
#endif

#ifdef ENABLE_JAPANESE
#define COURSE_NAME_TABLE course_strings_jp_table
#define COURSE_FILE "jp/courses.h"
#include "text/define_courses.inc.c"
#undef COURSE_NAME_TABLE
#undef COURSE_FILE
#endif

#ifdef ENABLE_SPANISH
#define COURSE_NAME_TABLE course_strings_es_table
#define COURSE_FILE "es/courses.h"
#include "text/define_courses.inc.c"
#undef COURSE_NAME_TABLE
#undef COURSE_FILE
#endif

const char *(*course_strings_language_table[])[] = DEFINE_LANGUAGE_ARRAY(
    &course_strings_en_table,
    &course_strings_fr_table,
    &course_strings_de_table,
    &course_strings_jp_table,
    &course_strings_es_table
);

#endif
