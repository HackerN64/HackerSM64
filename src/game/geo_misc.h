#ifndef GEO_MISC_H
#define GEO_MISC_H

#include <PR/ultratypes.h>
#include <PR/gbi.h>

enum FlyingCarpetState {
    FLYING_CARPET_IDLE = 0,
    FLYING_CARPET_MOVING_WITHOUT_MARIO = 1,
    FLYING_CARPET_MOVING_WITH_MARIO = 2
};

extern s8 gFlyingCarpetState;

/**
 * Create a vertex with the given parameters and insert it into `vtx` at
 * position `n`.
 *
 * Texture coordinates are s10.5 fixed-point, which means you should left-shift the actual coordinates by 5.
 */
ALWAYS_INLINE void make_vertex(Vtx *vtx, s32 n, s16 x, s16 y, s16 z, s16 tx, s16 ty, u8 r, u8 g, u8 b, u8 a) {
    vtx[n].v.ob[0] = x;
    vtx[n].v.ob[1] = y;
    vtx[n].v.ob[2] = z;

    vtx[n].v.flag = 0;

    vtx[n].v.tc[0] = tx;
    vtx[n].v.tc[1] = ty;

    vtx[n].v.cn[0] = r;
    vtx[n].v.cn[1] = g;
    vtx[n].v.cn[2] = b;
    vtx[n].v.cn[3] = a;
}

/**
 * Create a colourless vertex with the given parameters and insert it into `vtx` at
 * position `n`.
 *
 * Texture coordinates are s10.5 fixed-point, which means you should left-shift the actual coordinates by 5.
 */
ALWAYS_INLINE void make_simple_vertex(Vtx *vtx, s32 n, s16 x, s16 y, s16 z, s16 tx, s16 ty) {
    vtx[n].v.ob[0] = x;
    vtx[n].v.ob[1] = y;
    vtx[n].v.ob[2] = z;

    vtx[n].v.flag = 0;

    vtx[n].v.tc[0] = tx;
    vtx[n].v.tc[1] = ty;

    vtx[n].v.cn[0] = 255;
    vtx[n].v.cn[1] = 255;
    vtx[n].v.cn[2] = 255;
    vtx[n].v.cn[3] = 255;
}

extern Gfx *geo_exec_inside_castle_light(s32 callContext, struct GraphNode *node, UNUSED Mat4 mtx);
extern Gfx *geo_exec_flying_carpet_timer_update(s32 callContext, struct GraphNode *node, UNUSED Mat4 mtx);
extern Gfx *geo_exec_flying_carpet_create(s32 callContext, struct GraphNode *node, UNUSED Mat4 mtx);
extern Gfx *geo_exec_cake_end_screen(s32 callContext, struct GraphNode *node, UNUSED Mat4 mtx);

#define gLoadBlockTexture(dl, width, height, format, image)                                                  \
{                                                                                                            \
    gDPSetTextureImage((dl), (format), G_IM_SIZ_16b, 1, (image));                                            \
    gDPTileSync((dl));                                                                                       \
    gDPSetTile((dl), (format), G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD, \
               G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD);                                                      \
    gDPLoadSync((dl));                                                                                       \
    gDPLoadBlock((dl), G_TX_LOADTILE, 0, 0, (width) * (height) - 1, CALC_DXT((width), G_IM_SIZ_16b_BYTES))   \
}

#endif // GEO_MISC_H
