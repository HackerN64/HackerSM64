#include <ultra64.h>

#ifdef VISUAL_DEBUG

/**
 * @file debug_box.c
 * Draws 3D boxes for debugging purposes
 * Originally written by Synray, modified by Fazana.
 *
 * How to use:
 *
 * Just call debug_box() whenever you want to draw one!
 *
 * debug_box by default takes two arguments: a center and bounds vec3f.
 * This will draw a box starting from the point (center - bounds) to (center + bounds).
 *
 * Use debug_box_rot to draw a box rotated in the xz-plane.
 *
 * If you want to draw a box by specifying min and max points, use debug_box_pos() instead.
 */

#include "sm64.h"
#include "game/game_init.h"
#include "game/geo_misc.h"
#include "engine/math_util.h"
#include "area.h"
#include "level_update.h"
#include "print.h"
#include "engine/surface_collision.h"
#include "engine/surface_load.h"
#include "object_list_processor.h"

#include "debug_box.h"

Vtx debug_box_mesh[32] = {
	{{{0, 0, -100},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{50, 100, -87},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{50, 0, -87},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{0, 100, -100},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{-50, 0, -87},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{-50, 100, -87},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{-87, 0, -50},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{-87, 100, -50},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{-100, 0, 0},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{-100, 100, 0},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{-87, 0, 50},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{-87, 100, 50},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{-50, 0, 87},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{-50, 100, 87},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{0, 0, 100},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{0, 100, 100},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{50, 0, 87},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{50, 100, 87},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{87, 0, 50},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{87, 100, 50},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{100, 0, 0},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{100, 100, 0},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{87, 0, -50},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{87, 100, -50},0, {-16, 1008},{0x0, 0x0, 0x0, 0xFF}}},
	{{{-100, 0, 100},0, {-16, 1008},{0xFF, 0xFF, 0xFF, 0xFF}}},
	{{{-100, 100, 100},0, {-16, 1008},{0xFF, 0xFF, 0xFF, 0xFF}}},
	{{{-100, 100, -100},0, {-16, 1008},{0xFF, 0xFF, 0xFF, 0xFF}}},
	{{{100, 0, 100},0, {-16, 1008},{0xFF, 0xFF, 0xFF, 0xFF}}},
	{{{-100, 0, -100},0, {-16, 1008},{0xFF, 0xFF, 0xFF, 0xFF}}},
	{{{100, 100, -100},0, {-16, 1008},{0xFF, 0xFF, 0xFF, 0xFF}}},
	{{{100, 100, 100},0, {-16, 1008},{0xFF, 0xFF, 0xFF, 0xFF}}},
	{{{100, 0, -100},0, {-16, 1008},{0xFF, 0xFF, 0xFF, 0xFF}}},
};

Gfx dl_debug_box_verts[] = {
	gsSPVertex(debug_box_mesh, 32, 0),
	gsSP2Triangles(24, 25, 26, 0, 27, 25, 24, 0),
	gsSP2Triangles(28, 27, 24, 0, 24, 26, 28, 0),
	gsSP2Triangles(28, 26, 29, 0, 29, 26, 25, 0),
	gsSP2Triangles(29, 25, 30, 0, 27, 30, 25, 0),
	gsSP2Triangles(31, 30, 27, 0, 28, 31, 27, 0),
	gsSP2Triangles(28, 29, 31, 0, 31, 29, 30, 0),
	gsSPEndDisplayList(),
};

Gfx dl_debug_cylinder_verts[] = {
	gsSPVertex(debug_box_mesh, 24, 0),
	gsSP2Triangles(0, 1, 2, 0, 0, 3, 1, 0),
	gsSP2Triangles(4, 3, 0, 0, 4, 5, 3, 0),
	gsSP2Triangles(6, 5, 4, 0, 6, 7, 5, 0),
	gsSP2Triangles(8, 7, 6, 0, 8, 9, 7, 0),
	gsSP2Triangles(10, 9, 8, 0, 10, 11, 9, 0),
	gsSP2Triangles(12, 11, 10, 0, 12, 13, 11, 0),
	gsSP2Triangles(14, 13, 12, 0, 14, 15, 13, 0),
	gsSP2Triangles(16, 15, 14, 0, 16, 17, 15, 0),
	gsSP2Triangles(18, 17, 16, 0, 18, 19, 17, 0),
	gsSP2Triangles(20, 19, 18, 0, 20, 21, 19, 0),
	gsSP2Triangles(22, 21, 20, 0, 22, 23, 21, 0),
	gsSP2Triangles(2, 23, 22, 0, 2, 1, 23, 0),
	gsSP2Triangles(23, 1, 3, 0, 23, 3, 7, 0),
	gsSP2Triangles(3, 5, 7, 0, 7, 15, 23, 0),
	gsSP2Triangles(7, 11, 15, 0, 7, 9, 11, 0),
	gsSP2Triangles(11, 13, 15, 0, 15, 17, 19, 0),
	gsSP2Triangles(15, 19, 23, 0, 19, 21, 23, 0),
	gsSPEndDisplayList(),
};

u8 hitboxView = 0;
u8 surfaceView = 0;

/**
 * Internal struct containing box info
 */
struct DebugBox {
    u32 color;
    Vec3s center;
    Vec3s bounds;
    s16 yaw;
    u8 type;
};

struct DebugVert
{
    Vec3s pos;
    Vec3f normal;
};

struct DebugBox sBoxes[MAX_DEBUG_BOXES];
s16 sNumBoxes = 0;

extern Mat4 gMatStack[32]; //XXX: Hack

/**
 * The debug boxes' default transparency
 */
#define DBG_BOX_ALPHA     0x7F
/**
 * The debug boxes' default color. sCurBoxColor is reset to this every frame.
 */
#define DBG_BOX_DEF_COLOR 0xFF0000

/**
 * The color that new boxes will be drawn with.
 */
u32 sCurBoxColor = DBG_BOX_ALPHA << 24 | DBG_BOX_DEF_COLOR;

/**
 * The allocated size of a rotated box's dl
 */
#define DBG_BOX_DLSIZE ((s32)(6 * sizeof(Gfx) + 8 * sizeof(Vtx)))

/**
 * Sets up the RCP for drawing the boxes
 */
static const Gfx dl_debug_box_begin[] = {
    gsDPPipeSync(),
    gsDPSetRenderMode(G_RM_ZB_XLU_SURF, G_RM_NOOP2),
    gsSPClearGeometryMode(G_CULL_BACK),
    gsSPSetGeometryMode(G_ZBUFFER),
    gsSPTexture(0, 0, 0, 0, G_OFF),
    gsDPSetCombineLERP(0, 0, 0, ENVIRONMENT, 0, 0, 0, ENVIRONMENT, 0, 0, 0, ENVIRONMENT, 0, 0, 0, ENVIRONMENT),
    gsSPEndDisplayList(),
};

static const Gfx dl_debug_box_begin_water[] = {
    gsDPPipeSync(),
    gsDPSetRenderMode(G_RM_ZB_XLU_SURF, G_RM_NOOP2),
    gsSPClearGeometryMode(G_LIGHTING),
    gsSPSetGeometryMode(G_ZBUFFER | G_SHADE | G_SHADING_SMOOTH),
    gsSPTexture(0, 0, 0, 0, G_OFF),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPEndDisplayList(),
};

static const Gfx dl_debug_box_end[] = {
    gsDPPipeSync(),
    gsDPSetRenderMode(G_RM_OPA_SURF, G_RM_OPA_SURF2),
    gsSPClearGeometryMode(G_LIGHTING | G_CULL_BACK),
    gsSPTexture(0, 0, 0, 0, G_OFF),
    gsDPSetCombineMode(G_CC_FADE, G_CC_FADE),
    gsDPSetEnvColor(0xFF, 0xFF, 0xFF, 0xFF),
    gsSPEndDisplayList(),
};

static const Gfx dl_visual_surface[] = {
    gsDPPipeSync(),
    gsDPSetRenderMode(G_RM_ZB_XLU_DECAL, G_RM_NOOP2),
    gsSPClearGeometryMode(G_LIGHTING),
    gsSPSetGeometryMode(G_ZBUFFER | G_SHADE | G_SHADING_SMOOTH),
    gsSPTexture(0, 0, 0, 0, G_OFF),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPEndDisplayList(),
};

u8 viewCycle = 0;

//Puppyprint will call this from elsewhere.
void debug_box_input(void)
{
    if (gPlayer1Controller->buttonPressed & R_JPAD)
    {
        viewCycle++;
        if (viewCycle > 3)
            viewCycle = 0;

        hitboxView = 0;
        surfaceView = 0;

        if (viewCycle == 1 || viewCycle == 3)
            hitboxView = 1;
        if (viewCycle == 2 || viewCycle == 3)
            surfaceView = 1;
    }
}

s16 gVisualSurfaceCount;
s32 gVisualOffset;
extern s32 gSurfaceNodesAllocated;
extern s32 gSurfacesAllocated;

void iterate_surfaces_visual(s32 x, s32 z, Vtx *verts)
{
    struct SurfaceNode *node;
    struct Surface *surf;
    s32 cellX, cellZ;
    s32 i = 0;
    s32 col[3] = {0xFF, 0x00, 0x00};

    if (x <= -LEVEL_BOUNDARY_MAX || x >= LEVEL_BOUNDARY_MAX) {
        return;
    }
    if (z <= -LEVEL_BOUNDARY_MAX || z >= LEVEL_BOUNDARY_MAX) {
        return;
    }

    cellX = ((x + LEVEL_BOUNDARY_MAX) / CELL_SIZE) & NUM_CELLS_INDEX;
    cellZ = ((z + LEVEL_BOUNDARY_MAX) / CELL_SIZE) & NUM_CELLS_INDEX;

    for (i = 0; i < 8; i++)
    {
        switch (i)
        {
        case 0: node = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS].next; col[0] = 0x00; col[1] = 0xFF; col[2] = 0x00; break;
        case 1: node = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS].next; col[0] = 0x00; col[1] = 0xFF; col[2] = 0x00; break;
        case 2: node = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_FLOORS].next; col[0] = 0x00; col[1] = 0x00; col[2] = 0xFF; break;
        case 3: node = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_FLOORS].next; col[0] = 0x00; col[1] = 0x00; col[2] = 0xFF; break;
        case 4: node = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_CEILS].next; col[0] = 0xFF; col[1] = 0x00; col[2] = 0x00; break;
        case 5: node = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_CEILS].next; col[0] = 0xFF; col[1] = 0x00; col[2] = 0x00; break;
        case 6: node = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WATER].next; col[0] = 0xFF; col[1] = 0xFF; col[2] = 0x00; break;
        case 7: node = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WATER].next; col[0] = 0xFF; col[1] = 0xFF; col[2] = 0x00; break;
        }

        while (node != NULL)
        {
            surf = node->surface;
            node = node->next;

            make_vertex(verts, gVisualSurfaceCount, surf->vertex1[0], surf->vertex1[1], surf->vertex1[2], 0, 0, col[0], col[1], col[2], 0x80);
            make_vertex(verts, gVisualSurfaceCount+1, surf->vertex2[0], surf->vertex2[1], surf->vertex2[2], 0, 0, col[0], col[1], col[2], 0x80);
            make_vertex(verts, gVisualSurfaceCount+2, surf->vertex3[0], surf->vertex3[1], surf->vertex3[2], 0, 0, col[0], col[1], col[2], 0x80);

            gVisualSurfaceCount+=3;
        }
    }
}

void iterate_surfaces_envbox(Vtx *verts)
{
    TerrainData *p = gEnvironmentRegions;
    s32 numRegions;
    s32 col[3] = {0xFF, 0xFF, 0x00};
    s32 i = 0;

    if (p != NULL)
    {
        numRegions = *p++;
        for (i = 0; i < numRegions; i++)
        {
            make_vertex(verts, gVisualSurfaceCount, p[1], p[5], p[2], 0, 0, col[0], col[1], col[2], 0x80);
            make_vertex(verts, gVisualSurfaceCount+1, p[1], p[5], p[4], 0, 0, col[0], col[1], col[2], 0x80);
            make_vertex(verts, gVisualSurfaceCount+2, p[3], p[5], p[2], 0, 0, col[0], col[1], col[2], 0x80);

            make_vertex(verts, gVisualSurfaceCount+3, p[3], p[5], p[2], 0, 0, col[0], col[1], col[2], 0x80);
            make_vertex(verts, gVisualSurfaceCount+4, p[1], p[5], p[4], 0, 0, col[0], col[1], col[2], 0x80);
            make_vertex(verts, gVisualSurfaceCount+5, p[3], p[5], p[4], 0, 0, col[0], col[1], col[2], 0x80);

            gVisualSurfaceCount+=6;
            gVisualOffset+=6;
            p+= 6;
        }
    }
}

#if defined(F3DEX_GBI_2) || defined(F3DEX_GBI)
#define VERTCOUNT 30
#else
#define VERTCOUNT 12
#endif // F3DEX_GBI_2

void visual_surface_display(Vtx *verts, s32 iteration)
{
    s32 vts;
    s32 vtl = 0;
    s32 count = VERTCOUNT;
    s32 ntx = 0;
    if (!iteration)
    {
        vts = gVisualSurfaceCount;
    }
    else
    {
        vts = gVisualOffset;
    }

    while (vts > 0)
    {
        if (count == VERTCOUNT)
        {
            ntx = MIN(VERTCOUNT, vts);
            gSPVertex(gDisplayListHead++, VIRTUAL_TO_PHYSICAL(verts + ((gVisualSurfaceCount)-vts)), ntx, 0);
            count = 0;
            vtl = VERTCOUNT;
        }

        if (vtl >= 6)
        {
            gSP2Triangles(gDisplayListHead++, count + 0, count + 1, count + 2, 0, count + 3, count + 4, count + 5, 0);
            vts -= 6;
            vtl -= 6;
            count+= 6;
        }
        else
        if (vtl >= 3)
        {
            gSP1Triangle(gDisplayListHead++, count + 0, count + 1, count + 2, 0);
            vts -= 3;
            vtl -= 6;
            count+= 3;
        }
    }
}

s32 iterate_surface_count(s32 x, s32 z)
{
    struct SurfaceNode *node;
    s32 cellX, cellZ;
    s32 i = 0;
    s32 j = 0;
    TerrainData *p = gEnvironmentRegions;
    s32 numRegions;

    if (x <= -LEVEL_BOUNDARY_MAX || x >= LEVEL_BOUNDARY_MAX) {
        return 0;
    }
    if (z <= -LEVEL_BOUNDARY_MAX || z >= LEVEL_BOUNDARY_MAX) {
        return 0;
    }

    cellX = ((x + LEVEL_BOUNDARY_MAX) / CELL_SIZE) & NUM_CELLS_INDEX;
    cellZ = ((z + LEVEL_BOUNDARY_MAX) / CELL_SIZE) & NUM_CELLS_INDEX;

    for (i = 0; i < 8; i++)
    {
        switch (i)
        {
        case 0: node = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS].next; break;
        case 1: node = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS].next; break;
        case 2: node = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_FLOORS].next; break;
        case 3: node = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_FLOORS].next; break;
        case 4: node = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_CEILS].next; break;
        case 5: node = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_CEILS].next; break;
        case 6: node = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WATER].next; break;
        case 7: node = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WATER].next; break;
        }

        while (node != NULL)
        {
            node = node->next;
            j++;
        }
    }
    if (p != NULL)
    {
        numRegions = *p++;
        j += numRegions*6;
    }

    return j;
}

void visual_surface_loop(void)
{
    Vtx *verts;
    Mtx *mtx;

    if (!gSurfaceNodesAllocated || !gSurfacesAllocated || !gMarioState->marioObj)
        return;

    mtx =           alloc_display_list(sizeof(Mtx));
    verts = alloc_display_list((iterate_surface_count(gMarioState->pos[0], gMarioState->pos[2])*3) * sizeof(Vtx));

    gVisualSurfaceCount = 0;
    gVisualOffset = 0;

    if (mtx == NULL || verts == NULL)
        return;

    mtxf_to_mtx(mtx, gMatStack[1]);

    gSPDisplayList(gDisplayListHead++, dl_visual_surface);

    gSPMatrix(gDisplayListHead++, mtx, G_MTX_MODELVIEW | G_MTX_LOAD | G_MTX_NOPUSH);

    iterate_surfaces_visual(gMarioState->pos[0], gMarioState->pos[2], verts);

    visual_surface_display(verts, 0);

    iterate_surfaces_envbox(verts);

    gSPDisplayList(gDisplayListHead++, dl_debug_box_begin_water);

    visual_surface_display(verts, 1);

    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
    gSPDisplayList(gDisplayListHead++, dl_debug_box_end);
}

/**
 * Adds a box to the list to be rendered this frame.
 *
 * If there are already MAX_DEBUG_BOXES boxes, does nothing.
 */
static void append_debug_box(Vec3f center, Vec3f bounds, s16 yaw, s32 type)
{
    if (hitboxView)
    {
        if (sNumBoxes >= MAX_DEBUG_BOXES) {
        return;
        }

        vec3f_to_vec3s(sBoxes[sNumBoxes].center, center);
        vec3f_to_vec3s(sBoxes[sNumBoxes].bounds, bounds);

        sBoxes[sNumBoxes].yaw = yaw;
        sBoxes[sNumBoxes].color = sCurBoxColor;
        sBoxes[sNumBoxes].type = type;
        if (!(sBoxes[sNumBoxes].type & DEBUG_UCODE_REJ) && !(sBoxes[sNumBoxes].type & DEBUG_UCODE_DEFAULT))
            sBoxes[sNumBoxes].type |= DEBUG_UCODE_DEFAULT;

        ++sNumBoxes;
    }

}

/**
 * Draw new boxes with the given color.
 * Color format is 32-bit ARGB.
 * If the alpha component is zero, DBG_BOX_ALPHA (0x7f) will be used instead.
 * Ex: 0xFF0000 becomes 0x7FFF0000
 */
void debug_box_color(u32 color)
{
    if ((color >> 24) == 0) color |= (DBG_BOX_ALPHA << 24);
    sCurBoxColor = color;
}

/**
 * Draws a debug box from (center - bounds) to (center + bounds)
 * To draw a rotated box, use debug_box_rot()
 *
 * @see debug_box_rot()
 */
void debug_box(Vec3f center, Vec3f bounds, s32 type)
{
    append_debug_box(center, bounds, 0, type);
}

/**
 * Draws a debug box from (center - bounds) to (center + bounds), rotating it by `yaw`
 */
void debug_box_rot(Vec3f center, Vec3f bounds, s16 yaw, s32 type)
{
    append_debug_box(center, bounds, yaw, type);
}

/**
 * Draws a debug box from pMin to pMax
 * To draw a rotated box this way, use debug_box_pos_rot()
 *
 * @see debug_box_pos_rot()
 */
void debug_box_pos(Vec3f pMin, Vec3f pMax, s32 type)
{
    debug_box_pos_rot(pMin, pMax, 0, type);
}

/**
 * Draws a debug box from pMin to pMax, rotating it in the xz-plane by `yaw`
 */
void debug_box_pos_rot(Vec3f pMin, Vec3f pMax, s16 yaw, s32 type)
{
    Vec3f center, bounds;

    bounds[0] = (pMax[0] - pMin[0]) / 2.0f;
    bounds[1] = (pMax[1] - pMin[1]) / 2.0f;
    bounds[2] = (pMax[2] - pMin[2]) / 2.0f;

    center[0] = pMin[0] + bounds[0];
    center[1] = pMin[1] + bounds[1];
    center[2] = pMin[2] + bounds[2];

    append_debug_box(center, bounds, yaw, type);
}

static void render_box(int index)
{
    Mtx *translate;
    Mtx *rotate;
    Mtx *scale;
    Mtx *mtx;
    struct DebugBox *box = &sBoxes[index];
    s32 color = box->color;

    // Translate to the origin, rotate, then translate back, effectively rotating the box about
    // its center
    mtx =           alloc_display_list(sizeof(Mtx));
    translate =     alloc_display_list(sizeof(Mtx));
    rotate =        alloc_display_list(sizeof(Mtx));
    scale =         alloc_display_list(sizeof(Mtx));

    if (mtx == NULL || translate == NULL || rotate == NULL || scale == NULL)
    return;

    mtxf_to_mtx(mtx, gMatStack[1]);
    guTranslate(translate, box->center[0],  box->center[1],  box->center[2]);
    guRotate(rotate, box->yaw / (float)0x10000 * 360.0f, 0, 1.0f, 0);
    guScale(scale, (f32) box->bounds[0]*0.01f, (f32) box->bounds[1]*0.01f, (f32) box->bounds[2]*0.01f);

    gSPMatrix(gDisplayListHead++, mtx, G_MTX_MODELVIEW | G_MTX_LOAD | G_MTX_NOPUSH);
    gSPMatrix(gDisplayListHead++, translate,     G_MTX_MODELVIEW | G_MTX_MUL | G_MTX_NOPUSH);
    gSPMatrix(gDisplayListHead++, rotate,        G_MTX_MODELVIEW | G_MTX_MUL | G_MTX_NOPUSH);
    gSPMatrix(gDisplayListHead++, scale, G_MTX_MODELVIEW | G_MTX_MUL | G_MTX_NOPUSH);

    gDPSetEnvColor(gDisplayListHead++, (color >> 16) & 0xFF, (color >> 8) & 0xFF, (color) & 0xFF, (color >> 24) & 0xFF);

    if (box->type & DEBUG_SHAPE_BOX)
    {
        gSPDisplayList(gDisplayListHead++, dl_debug_box_verts);
    }
    if (box->type & DEBUG_SHAPE_CYLINDER)
    {
        gSPDisplayList(gDisplayListHead++, dl_debug_cylinder_verts);
    }

    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
}

void render_debug_boxes(s32 type)
{
    s32 i;

    debug_box_color(DBG_BOX_DEF_COLOR);

    if (sNumBoxes == 0)
        return;
    if (gAreaUpdateCounter < 3)
        return;

    gSPDisplayList(gDisplayListHead++, dl_debug_box_begin);

    for (i = 0; i < sNumBoxes; ++i) {
        if (type & DEBUG_UCODE_DEFAULT && sBoxes[i].type & DEBUG_UCODE_DEFAULT)
            render_box(i);
        if (type & DEBUG_UCODE_REJ && sBoxes[i].type & DEBUG_UCODE_REJ)
            render_box(i);
    }

    if (type & DEBUG_BOX_CLEAR)
        sNumBoxes = 0;

    gSPDisplayList(gDisplayListHead++, dl_debug_box_end);
}

#endif
