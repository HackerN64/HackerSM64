#include <PR/ultratypes.h>

#include "sm64.h"
#include "area.h"
#include "engine/graph_node.h"
#include "engine/surface_collision.h"
#include "engine/math_util.h"
#include "game_init.h"
#include "geo_misc.h"
#include "levels/castle_inside/header.h"
#include "levels/hmc/header.h"
#include "levels/ttm/header.h"
#include "mario.h"
#include "memory.h"
#include "moving_texture.h"
#include "level_update.h"
#include "object_list_processor.h"
#include "paintings.h"
#include "save_file.h"
#include "segment2.h"
#include "rendering_graph_node.h"

/**
 * @file paintings.c
 *
 * Implements the rippling painting effect. Paintings are GraphNodes that are attached to a bhvPainting object.
 *
 * Paintings are defined in level data. Look at levels/castle_inside/painting.inc.c for examples.
 *
 * The ripple effect uses data that is split into several parts:
 *      In bin/segment2.c:
 *          seg2_painting_triangle_mesh: The mesh positions are generated from a base mesh.
 *          seg2_painting_mesh_neighbor_tris: The lighting for the ripple is also generated from a base table.
 *          Each painting's texture uses yet another table to map its texture to the mesh.
 *      In levels/[LEVEL]/painting.inc.c:
 *          Painting structs, texture pointers, non-ripple display lists.
 *
 * Painting state machine:
 * Paintings spawn in the PAINTING_STATE_IDLE state
 *      From IDLE, paintings can change to PAINTING_STATE_RIPPLING or PAINTING_STATE_ENTERED
 *        - This state checks for ENTERED because if Mario waits long enough, a PROXIMITY painting could
 *          reset to IDLE
 *
 * Paintings in the PAINTING_STATE_RIPPLING state are passively rippling.
 *      For RIPPLE_TRIGGER_PROXIMITY paintings, this means Mario bumped the front of the painting.
 *
 *      Paintings that use RIPPLE_TRIGGER_CONTINUOUS try to transition to this state as soon as possible,
 *          usually when Mario enters the room.
 *
 *      A PROXIMITY painting will automatically reset to IDLE if its ripple magnitude becomes small enough.
 *
 * Paintings in the PAINTING_STATE_ENTERED state have been entered by Mario.
 *      A CONTINUOUS painting will automatically reset to RIPPLE if its ripple magnitude becomes small enough.
 */

/**
 * Array of pointers to painting data structs.
 */
const struct Painting *sPaintings[] = {
    /* Painting ID                  */
    /* PAINTING_ID_NULL             */ NULL,
    /* PAINTING_ID_CASTLE_BOB       */ &bob_painting,
    /* PAINTING_ID_CASTLE_CCM       */ &ccm_painting,
    /* PAINTING_ID_CASTLE_WF        */ &wf_painting,
    /* PAINTING_ID_CASTLE_JRB       */ &jrb_painting,
    /* PAINTING_ID_CASTLE_LLL       */ &lll_painting,
    /* PAINTING_ID_CASTLE_SSL       */ &ssl_painting,
    /* PAINTING_ID_CASTLE_HMC       */ &hmc_painting,
    /* PAINTING_ID_CASTLE_DDD       */ &ddd_painting,
    /* PAINTING_ID_CASTLE_WDW       */ &wdw_painting,
    /* PAINTING_ID_CASTLE_THI_TINY  */ &thi_tiny_painting,
    /* PAINTING_ID_CASTLE_TTM       */ &ttm_painting,
    /* PAINTING_ID_CASTLE_TTC       */ &ttc_painting,
    /* PAINTING_ID_CASTLE_SL        */ &sl_painting,
    /* PAINTING_ID_CASTLE_THI_HUGE  */ &thi_huge_painting,
    /* PAINTING_ID_CASTLE_RR        */ &rr_painting,
    /* PAINTING_ID_HMC_COTMC        */ &cotmc_painting,
    /* PAINTING_ID_TTM_SLIDE        */ &ttm_slide_painting,
};

/**
 * A list of preset constants for the ripple animations.
 */
static const struct RippleAnimationTypeInfo sRippleAnimations[] = {
    { // RIPPLE_ANIM_CONTINUOUS
        .passive = { .mag =  10.0f, .decay = 1.0f,    .rate = 0.05f, .dispersion = 15.0f },
        .entry   = { .mag =  30.0f, .decay = 0.98f,   .rate = 0.05f, .dispersion = 15.0f }
    },
    { // RIPPLE_ANIM_PROXIMITY
        .passive = { .mag =  20.0f, .decay = 0.9608f, .rate = 0.24f, .dispersion = 40.0f },
        .entry   = { .mag =  80.0f, .decay = 0.9524f, .rate = 0.14f, .dispersion = 30.0f }
    },
    { // RIPPLE_ANIM_PROXIMITY_LARGE
        .passive = { .mag =  40.0f, .decay = 0.9608f, .rate = 0.12f, .dispersion = 80.0f },
        .entry   = { .mag = 160.0f, .decay = 0.9524f, .rate = 0.07f, .dispersion = 60.0f }
    }
};

/// - DRAW -

/**
 * Allocates and generates a mesh for the rippling painting effect by modifying the passed in `mesh`
 * based on the painting's current ripple state.
 *
 * The `mesh` table describes the location of mesh vertices, whether they move when rippling, and what
 * triangles they belong to.
 *
 * The static mesh passed in is organized into two lists. This function only uses the first list,
 * painting_calculate_triangle_normals below uses the second one.
 *
 * The first list describes the vertices in this format:
 *      numVertices
 *      v0 x, v0 y, movable
 *      ...
 *      vN x, vN y, movable
 *      Where x and y are from 0 to PAINTING_SIZE, movable is 0 or 1.
 *
 * The mesh used in game, seg2_painting_triangle_mesh, is in bin/segment2.c.
 */
void painting_generate_mesh(struct Object *obj, const PaintingData *meshData, PaintingData numVtx, struct PaintingMeshVertex *paintingMesh) {
    const struct Painting *painting = obj->oPaintingImageInfo;
    const struct RippleAnimationInfo *objRippleInfo = obj->oPaintingRippleInfo;
    PaintingData i, tri;

    /// Controls the peaks of the ripple.
    f32 rippleMag = obj->oPaintingCurrRippleMag;
    /// Controls the ripple's frequency.
    f32 rippleRate = objRippleInfo->rate;
    /// Controls how fast the ripple spreads.
    f32 dispersionFactor = (1.0f / objRippleInfo->dispersion);
    /// How far the ripple has spread.
    f32 rippleTimer = obj->oPaintingRippleTimer;

    /// X and Y of the ripple origin.
    f32 rippleX = obj->oPaintingRipplePosX;
    f32 rippleY = obj->oPaintingRipplePosY;

    f32 sizeRatioX = (painting->sizeX / PAINTING_SIZE);
    f32 sizeRatioY = (painting->sizeY / PAINTING_SIZE);

    f32 dx, dy;
    f32 rippleDistance;

    // Loop through all the painting vertices and calculate the ripple magnitude at each point.
    // Accesses are off by 1 since the first entry is the number of vertices.
    for (i = 0; i < numVtx; i++) {
        tri = (i * 3);
        paintingMesh->pos[0] = meshData[tri + 1];
        paintingMesh->pos[1] = meshData[tri + 2];
        // The "Z coordinate" of each vertex in the mesh is either 1 or 0. Instead of being an
        // actual coordinate, it just determines whether the vertex moves.
        if (meshData[tri + 3]) {
            // Scale and calculate the distance to the ripple origin.
            dx = ((paintingMesh->pos[0] * sizeRatioX) - rippleX);
            dy = ((paintingMesh->pos[1] * sizeRatioY) - rippleY);
            // A larger dispersionFactor makes the ripple spread slower.
            rippleDistance = sqrtf(sqr(dx) + sqr(dy)) * dispersionFactor;

            if (rippleTimer < rippleDistance) {
                // If the ripple hasn't reached the point yet, make the point magnitude 0.
                paintingMesh->pos[2] = 0;
            } else {
                // Use a cosine wave to make the ripple go up and down,
                // scaled by the painting's ripple magnitude,
                // round it to an int and return it.
                paintingMesh->pos[2] = roundf(rippleMag * coss((u16)((rippleRate * (rippleTimer - rippleDistance)) * (f32)0x10000)));
            }
        } else {
            paintingMesh->pos[2] = 0;
        }

        paintingMesh++;
    }
}

/**
 * Calculate the surface normals of each triangle in the generated ripple mesh.
 *
 * The static mesh passed in is organized into two lists. This function uses the second list,
 * painting_generate_mesh above uses the first one.
 *
 * The second list in `mesh` describes the mesh's triangles in this format:
 *      numTris
 *      tri0 v0, tri0 v1, tri0 v2
 *      ...
 *      triN v0, triN v1, triN v2
 *      Where each v0, v1, v2 is an index into the first list in `mesh`.
 *
 * The mesh used in game, seg2_painting_triangle_mesh, is in bin/segment2.c.
 */
void painting_calculate_triangle_normals(const PaintingData *meshData, PaintingData numVtx, PaintingData numTris, struct PaintingMeshVertex *paintingMesh, Vec3f *paintingTriNorms) {
    PaintingData i;
    Vec3s v;
    Vec3f vp0, vp1, vp2;

    for (i = 0; i < numTris; i++) {
        // Add 2 because of the 2 length entries preceding the list.
        PaintingData tri = (1 + (numVtx * 3) + 1 + (i * 3));
        vec3s_copy(v, &meshData[tri]);
        vec3s_to_vec3f(vp0, paintingMesh[v[0]].pos);
        vec3s_to_vec3f(vp1, paintingMesh[v[1]].pos);
        vec3s_to_vec3f(vp2, paintingMesh[v[2]].pos);

        // Cross product to find each triangle's normal vector.
        find_vector_perpendicular_to_plane(paintingTriNorms[i], vp0, vp1, vp2);
    }
}

/**
 * Approximates the painting mesh's vertex normals by averaging the normals of all triangles sharing a
 * vertex. Used for Gouraud lighting.
 *
 * After each triangle's surface normal is calculated, the `neighborTris` table describes which triangles
 * each vertex should use when calculating the average normal vector.
 *
 * The table is a list of entries in this format:
 *      numNeighbors, tri0, tri1, ..., triN
 *
 *      Where each 'tri' is an index into sPaintingTriNorms.
 *      Entry i in `neighborTris` corresponds to the vertex at sPaintingMesh[i]
 *
 * The table used in game, seg2_painting_mesh_neighbor_tris, is in bin/segment2.c.
 */
void painting_average_vertex_normals(const PaintingData *neighborTris, PaintingData numVtx, struct PaintingMeshVertex *paintingMesh, Vec3f *paintingTriNorms) {
    PaintingData tri;
    PaintingData i, j;
    PaintingData numNeighbors;
    PaintingData entry = 0;

    for (i = 0; i < numVtx; i++) {
        Vec3f n = { 0.0f, 0.0f, 0.0f };

        // The first number of each entry is the number of adjacent tris.
        numNeighbors = neighborTris[entry];
        for (j = 0; j < numNeighbors; j++) {
            tri = neighborTris[entry + 1 + j];
            vec3f_add(n, paintingTriNorms[tri]);
        }

        // Move to the next vertex's entry
        entry += (1 + numNeighbors);

        // Average the surface normals from each neighboring tri.
        vec3_div_val(n, numNeighbors);
        f32 nlen = vec3_sumsq(n);

        if (FLT_IS_NONZERO(nlen)) {
            nlen = (127.0f / sqrtf(nlen));
            vec3_prod_val(paintingMesh[i].norm, n, nlen);
        } else {
            vec3_zero(paintingMesh[i].norm);
        }
    }
}

#ifdef F3DEX_GBI_2
    #define VTX_BUF_MAX 32
#else
    #define VTX_BUF_MAX 16
#endif
#define TRI_PER_GRP (VTX_BUF_MAX / 3) //  5 or 10
#define VTX_PER_GRP (TRI_PER_GRP * 3) // 15 or 30

/**
 * Creates a display list that draws the rippling painting, with 'img' mapped to the painting's mesh,
 * using 'textureMap'.
 *
 * If the textureMap doesn't describe the whole mesh, then multiple calls are needed to draw the whole
 * painting.
 */
Gfx *render_painting(const Texture *img, s16 index, s16 imageCount, s16 tWidth, s16 tHeight, const PaintingData *textureMap, Alpha alpha, struct PaintingMeshVertex *paintingMesh) {
    struct PaintingMeshVertex *currVtx = NULL;
    PaintingData group;
    PaintingData groupIndex;
    PaintingData map;
    PaintingData triGroup;
    PaintingData mapping;
    PaintingData meshVtx;
    s16 tx, ty;

    PaintingData mapVerts = textureMap[0];
    PaintingData mapTris = textureMap[1 + mapVerts];

    // We can fit VTX_PER_GRP vertices in the RSP's vertex buffer.
    // Group triangles by TRI_PER_GRP, with one remainder group.
    PaintingData triGroups    = (mapTris / TRI_PER_GRP);
    PaintingData remGroupTris = (mapTris % TRI_PER_GRP);
    PaintingData numVtx       = (mapTris * 3); // 3 verts per tri

    Vtx *verts = alloc_display_list(numVtx * sizeof(Vtx));
    u32 gfxCmds = (
        /*gLoadBlockTexture */ 5 +
        (triGroups * (
            /*gSPVertex         */ 1 +
            /*gSPDisplayList    */ 1
        )) +
        /*gSPVertex         */ 1 +
        (remGroupTris * (
            /*gSP1Triangle      */ 1
        )) +
        /*gSPEndDisplayList */ 1
    );
    Gfx *dlist = alloc_display_list(gfxCmds * sizeof(Gfx));
    Gfx *gfx = dlist;

    gLoadBlockTexture(gfx++, tWidth, tHeight, G_IM_FMT_RGBA, img);

    // Width and height of each section.
    const f32 dx = (PAINTING_SIZE / 1);
    const f32 dy = (PAINTING_SIZE / imageCount);

    // These are used to scale the vertex positions into texture positions.
    const f32 tWidthScale  = (TC(tWidth ) / dx);
    const f32 tHeightScale = (TC(tHeight) / dy);

    // Bottom Y position of the current section.
    const s16 y2 = ((index + 1) * dy);

    // Draw the groups of TRI_PER_GRP first.
    for (group = 0; group < triGroups; group++) {
        // The index of the first vertex in the group.
        groupIndex = (group * VTX_PER_GRP);

        // The triangle groups are the second part of the texture map.
        // Each group is a list of VTX_PER_GRP mappings.
        triGroup = (1 + mapVerts + 1 + groupIndex);

        // Vertices within the group
        for (map = 0; map < VTX_PER_GRP; map++) {
            // The mapping is just an index into the earlier part of the textureMap.
            // Some mappings are repeated, for example, when multiple triangles share a vertex.
            mapping = (textureMap[triGroup + map]);

            // The first entry is the ID of the vertex in the mesh.
            meshVtx = textureMap[1 + mapping];

            // Get a pointer to the current mesh.
            currVtx = &paintingMesh[meshVtx];

            // Texture coordinates.
            tx = (currVtx->pos[0] * tWidthScale);
            ty = ((y2 - currVtx->pos[1]) * tHeightScale);

            // Map the texture and place it in the verts array.
            make_vertex(verts, (groupIndex + map),
                currVtx->pos[0],
                currVtx->pos[1],
                currVtx->pos[2],
                tx, ty,
                currVtx->norm[0],
                currVtx->norm[1],
                currVtx->norm[2],
                alpha
            );
        }

        // Load the vertices and draw the TRI_PER_GRP triangles.
        gSPVertex(gfx++, VIRTUAL_TO_PHYSICAL(verts + groupIndex), VTX_PER_GRP, 0);
        gSPDisplayList(gfx++, dl_paintings_draw_ripples);
    }

    // One group left with < TRI_PER_GRP triangles.
    triGroup = (1 + mapVerts + 1 + (triGroups * VTX_PER_GRP));

    // Map the texture to the triangles.
    for (map = 0; map < (remGroupTris * 3); map++) {
        // The mapping is just an index into the earlier part of the textureMap.
        // Some mappings are repeated, for example, when multiple triangles share a vertex.
        mapping = (textureMap[triGroup + map]);

        // The first entry is the ID of the vertex in the mesh.
        meshVtx = textureMap[1 + mapping];

        // Get a pointer to the current mesh.
        currVtx = &paintingMesh[meshVtx];

        // Texture coordinates.
        tx = (currVtx->pos[0] * tWidthScale);
        ty = ((y2 - currVtx->pos[1]) * tHeightScale);

        make_vertex(verts, ((triGroups * VTX_PER_GRP) + map),
            currVtx->pos[0],
            currVtx->pos[1],
            currVtx->pos[2],
            tx, ty,
            currVtx->norm[0],
            currVtx->norm[1],
            currVtx->norm[2],
            alpha
        );
    }

    // Draw the remaining triangles individually.
    gSPVertex(gfx++, VIRTUAL_TO_PHYSICAL(verts + (triGroups * VTX_PER_GRP)), (remGroupTris * 3), 0);

    for (group = 0; group < (remGroupTris * 3); group += 3) {
        gSP1Triangle(gfx++,
            (group + 0),
            (group + 1),
            (group + 2),
            0x0
        );
    }

    gSPEndDisplayList(gfx);

    return dlist;
}

#undef VTX_BUF_MAX
#undef TRI_PER_GRP
#undef VTX_PER_GRP

/**
 * Orient the painting mesh for rendering.
 */
Gfx *painting_model_view_transform(const struct Painting *painting) {
    u32 gfxCmds = (
        /*gSPMatrix         */ 1 +
        /*gSPEndDisplayList */ 1
    );
    Gfx *dlist = alloc_display_list(gfxCmds * sizeof(Gfx));
    Gfx *gfx = dlist;

    // Scale
    Mtx *scale = alloc_display_list(sizeof(Mtx));
    guScale(
        scale,
        (painting->sizeX / PAINTING_SIZE),
        (painting->sizeY / PAINTING_SIZE),
        1.0f
    );
    gSPMatrix(gfx++, scale, (G_MTX_MODELVIEW | G_MTX_MUL | G_MTX_PUSH));
    gSPEndDisplayList(gfx);

    return dlist;
}

/**
 * Gets the exponent of an integer by converting it into a float and extracting the exponent bits.
 */
s32 get_exponent(s32 x) {
    union { s32 i; f32 f; } b = { .f = x };
    return (((b.i >> 23) & (u32)BITMASK(8)) - 127);
}

/**
 * Set up the texture format in the display list.
 */
void painting_setup_textures(Gfx **gfx, s16 tWidth, s16 tHeight, s32 isEnvMap) {
    s16 cm = isEnvMap ? (G_TX_WRAP | G_TX_NOMIRROR) : G_TX_CLAMP;
    u32 masks = get_exponent(tWidth);
    u32 maskt = get_exponent(tHeight);

    // Set up the textures.
    gDPSetTile((*gfx)++, G_IM_FMT_RGBA, G_IM_SIZ_16b,
        (tWidth >> 2), 0, G_TX_RENDERTILE, 0,
        cm, maskt, G_TX_NOLOD,
        cm, masks, G_TX_NOLOD
    );
    gDPSetTileSize((*gfx)++, 0,
        0, 0,
        ((tWidth  - 1) << G_TEXTURE_IMAGE_FRAC),
        ((tHeight - 1) << G_TEXTURE_IMAGE_FRAC)
    );
}

/**
 * Ripple a painting that has 1 or more images that need to be mapped.
 */
Gfx *dl_painting_rippling(const struct Painting *painting, struct PaintingMeshVertex *paintingMesh) {
    s16 i;
    const PaintingData *textureMap = NULL;
    s16 imageCount = painting->imageCount;
    s16 tWidth = painting->textureWidth;
    s16 tHeight = painting->textureHeight;
    const Texture **tArray = segmented_to_virtual(painting->textureArray);
    s32 isEnvMap = (painting->textureType == PAINTING_TYPE_ENV_MAP);

    u32 gfxCmds = (
        /*gSPDisplayList    */ 1 +
        /*gSPDisplayList    */ 1 +
        /*gDPSetTile        */ 1 +
        /*gDPSetTileSize    */ 1 +
        (imageCount * (
            /*gSPDisplayList    */ 1
        )) +
        /*gSPPopMatrix      */ 1 +
        /*gSPDisplayList    */ 1 +
        /*gSPEndDisplayList */ 1
    );
    Gfx *dlist = alloc_display_list(gfxCmds * sizeof(Gfx));
    Gfx *gfx = dlist;

    if (dlist == NULL) {
        return dlist;
    }

    gSPDisplayList(gfx++, painting_model_view_transform(painting));
    Gfx *beginDl = (isEnvMap ? dl_paintings_env_mapped_begin : dl_paintings_rippling_begin);
    gSPDisplayList(gfx++, beginDl);

    //! TODO: Automatically create texture maps based on the image count.
    const PaintingData **textureMaps = NULL;
    switch (imageCount) {
        case 1: textureMaps = segmented_to_virtual(seg2_painting_texture_maps_1); break;
        case 2: textureMaps = segmented_to_virtual(seg2_painting_texture_maps_2); break;
    }

    painting_setup_textures(&gfx, tWidth, tHeight, isEnvMap);

    // Map each image to the mesh's vertices.
    for (i = 0; i < imageCount; i++) {
        textureMap = segmented_to_virtual(textureMaps[i]);
        // Render a section of the painting.
        gSPDisplayList(gfx++, render_painting(tArray[i], i, imageCount, tWidth, tHeight, textureMap, painting->alpha, paintingMesh));
    }

    gSPPopMatrix(gfx++, G_MTX_MODELVIEW);
    Gfx *endDl = (isEnvMap ? dl_paintings_env_mapped_end : dl_paintings_rippling_end);
    gSPDisplayList(gfx++, endDl);
    gSPEndDisplayList(gfx);

    return dlist;
}

/**
 * Generates a mesh, calculates vertex normals for lighting, and renders a rippling painting.
 * The mesh and vertex normals are regenerated and freed every frame.
 */
Gfx *display_painting_rippling(struct Object *obj) {
    const struct Painting *painting = obj->oPaintingImageInfo;
    const PaintingData *meshData = segmented_to_virtual(seg2_painting_triangle_mesh);
    const PaintingData *neighborTris = segmented_to_virtual(seg2_painting_mesh_neighbor_tris);
    PaintingData numVtx = meshData[0];
    PaintingData numTris = meshData[(numVtx * 3) + 1];
    Gfx *dlist = NULL;
    // When a painting is rippling, this mesh is generated each frame using the Painting's parameters.
    // This mesh only contains the vertex positions and normals.
    // Paintings use an additional array to map textures to the mesh.
    struct PaintingMeshVertex *paintingMesh = mem_pool_alloc(gEffectsMemoryPool, (numVtx * sizeof(struct PaintingMeshVertex)));
    // The painting's surface normals, used to approximate each of the vertex normals (for gouraud shading).
    Vec3f *paintingTriNorms = mem_pool_alloc(gEffectsMemoryPool, (numTris * sizeof(Vec3f)));

    // Generate the mesh and its lighting data
    painting_generate_mesh(obj, meshData, numVtx, paintingMesh);
    painting_calculate_triangle_normals(meshData, numVtx, numTris, paintingMesh, paintingTriNorms);
    painting_average_vertex_normals(neighborTris, numVtx, paintingMesh, paintingTriNorms);

    // Map the painting's texture depending on the painting's texture type.
    dlist = dl_painting_rippling(painting, paintingMesh);

    // The mesh data is freed every frame.
    mem_pool_free(gEffectsMemoryPool, paintingMesh);
    mem_pool_free(gEffectsMemoryPool, paintingTriNorms);

    return dlist;
}

Gfx *dl_painting_not_rippling(const struct Painting *painting) {
    Alpha alpha = painting->alpha;
    s16 imageCount = painting->imageCount;
    s32 shaded = painting->shaded;
    u32 gfxCmds = (
        /*gSPDisplayList        */ 1 +
        /*gSPVertex             */ 1 +
        /*gDPSetTile            */ 1 +
        /*gDPSetTileSize        */ 1 +
        (imageCount * (
            /*gDPSetTextureImage    */ 1 +
            /*gDPLoadSync           */ 1 +
            /*gDPLoadBlock          */ 1 +
            /*gSP2Triangles         */ 1
        )) +
        /*gSPDisplayList        */ 1 +
        (!shaded * (
            /*gSPSetGeometryMode    */ 1
        )) +
        /*gSPEndDisplayList     */ 1
    );
    Gfx *dlist = alloc_display_list(gfxCmds * sizeof(Gfx));
    Gfx *gfx = dlist;

    if (dlist == NULL) {
        return dlist;
    }

    Vtx *verts = alloc_display_list((imageCount * 4) * sizeof(*verts));
    Vec3c n;

    const Texture **textures = segmented_to_virtual(painting->textureArray);

    s32 isEnvMap = (painting->textureType == PAINTING_TYPE_ENV_MAP);

    if (isEnvMap) {
        vec3_set(n, 0x00, 0x00, 0x7f);
        gSPDisplayList(gfx++, dl_paintings_env_mapped_begin);
    } else if (shaded) {
        vec3_set(n, 0x00, 0x00, 0x7f);
        gSPDisplayList(gfx++, dl_paintings_textured_shaded_begin);
    } else {
        vec3_same(n, 0xdd);
        gSPDisplayList(gfx++, dl_paintings_textured_vertex_colored_begin);
    }

    s16 tWidth = painting->textureWidth;
    s16 tHeight = painting->textureHeight;

    // Width and height of each section.
    const f32 dx = (PAINTING_SIZE / 1);
    const f32 dy = (PAINTING_SIZE / imageCount);

    // These are used to scale the vertex positions into texture positions.
    const f32 tWidthScale  = (TC(tWidth ) / dx);
    const f32 tHeightScale = (TC(tHeight) / dy);

    s16 x1 = 0; // Left
    s16 x2 = dx; // Right
    s16 s1 = (x1 * tWidthScale); // Left
    s16 s2 = (x2 * tWidthScale); // Right
    s16 y1, y2;
    s16 t1, t2;

    s32 vertIndex = 0;
    s32 i;

    // Generate vertices
    for (i = 0; i < imageCount; i++) {
        y1 = (i * dy); // Top
        y2 = (y1 + dy); // Bottom
        t1 = ((y2 - y1) * tHeightScale); // Top
        t2 = ((y2 - y2) * tHeightScale); // Bottom
        make_vertex(verts, vertIndex++,  0, y1, 0, s1, t1, n[0], n[1], n[2], alpha); // Bottom Left
        make_vertex(verts, vertIndex++, dx, y1, 0, s2, t1, n[0], n[1], n[2], alpha); // Bottom Right
        make_vertex(verts, vertIndex++, dx, y2, 0, s2, t2, n[0], n[1], n[2], alpha); // Top Right
        make_vertex(verts, vertIndex++,  0, y2, 0, s1, t2, n[0], n[1], n[2], alpha); // Top left
    }

    gSPVertex(gfx++, verts, vertIndex, 0);

    painting_setup_textures(&gfx, tWidth, tHeight, isEnvMap);

    s32 quadIndex;

    for (i = 0; i < imageCount; i++) {
        gDPSetTextureImage(gfx++, G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, textures[i]);
        gDPLoadSync(gfx++);
        gDPLoadBlock(gfx++, G_TX_LOADTILE, 0, 0, ((tWidth * tHeight) - 1), CALC_DXT(tWidth, G_IM_SIZ_16b_BYTES));
        quadIndex = (i * 4);
        gSP2Triangles(gfx++,
            (quadIndex + 0), (quadIndex + 1), (quadIndex + 2), 0x0,
            (quadIndex + 0), (quadIndex + 2), (quadIndex + 3), 0x0
        );
    }

    if (isEnvMap) {
        gSPDisplayList(gfx++, dl_paintings_env_mapped_end);
    } else {
        gSPDisplayList(gfx++, dl_paintings_textured_end);
    }

    if (!shaded) {
        gSPSetGeometryMode(gfx++, G_LIGHTING);
    }

    gSPEndDisplayList(gfx);

    return dlist;
}

/**
 * Render a normal painting.
 */
Gfx *display_painting_not_rippling(struct Object *obj) {
    const struct Painting *painting = obj->oPaintingImageInfo;
    u32 gfxCmds = (
        /*gSPDisplayList    */ 1 +
        /*gSPDisplayList    */ 1 +
        /*gSPPopMatrix      */ 1 +
        /*gSPEndDisplayList */ 1
    );
    Gfx *dlist = alloc_display_list(gfxCmds * sizeof(Gfx));
    Gfx *gfx = dlist;

    if (dlist == NULL) {
        return dlist;
    }

    gSPDisplayList(gfx++, painting_model_view_transform(painting));
    gSPDisplayList(gfx++, dl_painting_not_rippling(painting));
    gSPPopMatrix(gfx++, G_MTX_MODELVIEW);
    gSPEndDisplayList(gfx);

    return dlist;
}

/**
 * Draw the painting.
 */
Gfx *geo_painting_draw(s32 callContext, struct GraphNode *node, UNUSED void *context) {
    struct Object *obj = gCurGraphNodeObjectNode;

    if (obj == NULL) {
        return NULL;
    }

    // Get the const painting data.
    const struct Painting *painting = obj->oPaintingImageInfo;

    if (painting == NULL) {
        return NULL;
    }

    Gfx *paintingDlist = NULL;

    if (callContext == GEO_CONTEXT_RENDER) {
        // Draw the painting.
        if (painting->imageCount > 0
         && painting->textureArray != NULL
         && painting->textureWidth > 0
         && painting->textureHeight > 0
         && painting->alpha > 0x00) {
            // Determine whether the painting is opaque or transparent.
            if (painting->alpha == 0xFF) {
                SET_GRAPH_NODE_LAYER(node->flags, LAYER_OCCLUDE_SILHOUETTE_OPAQUE);
            } else {
                SET_GRAPH_NODE_LAYER(node->flags, LAYER_TRANSPARENT);
            }

            if (obj->oPaintingState == PAINTING_STATE_IDLE) {
                paintingDlist = display_painting_not_rippling(obj);
            } else {
                paintingDlist = display_painting_rippling(obj);
            }
        }
    }

    return paintingDlist;
}

/// - INIT -

/**
 * Updates a painting object's room from a point in front of the center of the painting.
 */
void painting_update_room(struct Object *obj) {
    const struct Painting *painting = obj->oPaintingImageInfo;

    // The center of the painting, but with an offset since paintings are usually between floor triangle edges laterally.
    Vec3f distPos = {
        (painting->sizeX * 0.5f),
        (painting->sizeY * 0.5f),
        PAINTING_WOBBLE_DEPTH // Distance in front of the painting to check for a room floor.
    };

    // Get the painting object's rotation.
    Vec3s rotation;
    vec3i_to_vec3s(rotation, &obj->oFaceAngleVec);

    // Set 'roomCheckPos' to the world space coords of 'distPos'.
    Vec3f roomCheckPos;
    vec3f_local_pos_to_world_pos(roomCheckPos, distPos, &obj->oPosVec, rotation);

    // Set the object's room so that paintings only render in their room.
    obj->oRoom = get_room_at_pos(
        roomCheckPos[0],
        roomCheckPos[1],
        roomCheckPos[2]
    );
}

void bhv_painting_init(void) {
    struct Object *obj = o;

    // Get the painting id from the first byte of the behavior params. The second byte is used for the warp node ID.
    s32 id = GET_BPARAM1(obj->oBehParams);

    const struct Painting *painting = segmented_to_virtual(sPaintings[id]);

    // Set the object's painting data pointer.
    obj->oPaintingImageInfo = painting;

    // Clear flags.
    obj->oPaintingCurrFlags = RIPPLE_FLAGS_NONE;
    obj->oPaintingChangedFlags = RIPPLE_FLAGS_NONE;

    // Update the painting object's room.
    painting_update_room(obj);
}

/// - LOOP -

/**
 * Check for Mario entering the painting.
 */
void painting_update_mario_pos_and_flags(struct Object *obj) {
    if (!gMarioObject) {
        return;
    }

    s8 rippleFlags = RIPPLE_FLAGS_NONE;

    Vec3f marioWorldPos;
    Vec3f marioLocalPos;
    Vec3s rotation;

    // Add PAINTING_MARIO_Y_OFFSET to make the ripple closer to Mario's center of mass.
    vec3f_copy_y_off(marioWorldPos, &gMarioObject->oPosVec, PAINTING_MARIO_Y_OFFSET);

    // Get the painting's rotation.
    vec3i_to_vec3s(rotation, &obj->oFaceAngleVec);

    // Get Mario's position in the painting's local space.
    vec3f_world_pos_to_local_pos(marioLocalPos, marioWorldPos, &obj->oPosVec, rotation);

    // Get the const painting data.
    const struct Painting *painting = obj->oPaintingImageInfo;

    // Check if Mario is within the painting bounds laterally in local space.
    if (marioLocalPos[0] > -PAINTING_EDGE_MARGIN
     && marioLocalPos[0] < (painting->sizeX + PAINTING_EDGE_MARGIN)
     && marioLocalPos[1] > -PAINTING_EDGE_MARGIN
     && marioLocalPos[1] < (painting->sizeY + PAINTING_EDGE_MARGIN)) {
        // Check whether Mario is inside the wobble zone.
        if (marioLocalPos[2] < PAINTING_WOBBLE_DEPTH
         && marioLocalPos[2] > 0.0f) {
            rippleFlags |= RIPPLE_FLAG_RIPPLE;
        }
        // Check whether Mario is inside the warp zone.
        if (marioLocalPos[2] < 0.0f
         && marioLocalPos[2] > -PAINTING_WARP_DEPTH) {
            rippleFlags |= RIPPLE_FLAG_ENTER;
        }
    }
    
    obj->oPaintingLocalMarioPosX = marioLocalPos[0];
    obj->oPaintingLocalMarioPosY = marioLocalPos[1];

    s16 lastFlags = obj->oPaintingCurrFlags;

    // At most 1 of these will be nonzero.
    obj->oPaintingCurrFlags = rippleFlags;

    // changedFlags is true if currFlags is true and lastFlags is false
    // (Mario just entered the floor on this frame).
    obj->oPaintingChangedFlags = ((lastFlags ^ obj->oPaintingCurrFlags) & obj->oPaintingCurrFlags);
    
    // Detect whether Mario is entering this painting, and set paintingObj accordingly
    if (obj->oPaintingCurrFlags & RIPPLE_FLAG_ENTER) {
        // Mario has entered the painting.
        gMarioState->paintingObj = obj;
    } else if (gMarioState->paintingObj == obj) {
        // Reset gMarioState->paintingObj if it's this painting and this painting is not entered.
        gMarioState->paintingObj = NULL;
    }
}

/**
 * Returns a pointer to the RippleAnimationTypeInfo that best fits the painting type.
 */
const struct RippleAnimationTypeInfo *painting_get_ripple_animation_type_info(const struct Painting *painting) {
    s8 rippleAnimationType = RIPPLE_ANIM_CONTINUOUS;

    if (painting->rippleTrigger == RIPPLE_TRIGGER_PROXIMITY) {
        rippleAnimationType = RIPPLE_ANIM_PROXIMITY;

        if (painting->sizeX >= (PAINTING_SIZE * 2)
         || painting->sizeY >= (PAINTING_SIZE * 2)) {
            rippleAnimationType = RIPPLE_ANIM_PROXIMITY_LARGE;
        }
    }

    return &sRippleAnimations[rippleAnimationType];
}

/**
 * Set a painting's ripple animation and magnitude.
 */
void painting_set_ripple_animation(struct Object *obj, const struct RippleAnimationInfo *baseRippleInfo) {
    obj->oPaintingCurrRippleMag = baseRippleInfo->mag;
    obj->oPaintingRippleInfo = baseRippleInfo;
}

/**
 * Update the ripple's timer and magnitude, making it propagate outwards.
 *
 * Automatically changes the painting back to IDLE state (or RIPPLE for continuous paintings) if the
 * ripple's magnitude becomes small enough.
 */
void painting_update_ripple_state(struct Object *obj) {
    const struct Painting *painting = obj->oPaintingImageInfo;
    const struct RippleAnimationInfo *objRippleInfo = obj->oPaintingRippleInfo;

    if (objRippleInfo != NULL) {
        obj->oPaintingCurrRippleMag *= objRippleInfo->decay;
    }

    // Reset the timer to 0 if it overflows.
    if (obj->oPaintingRippleTimer < 0) {
        obj->oPaintingRippleTimer = 0;
    }

    obj->oPaintingRippleTimer++;

    if (painting->rippleTrigger == RIPPLE_TRIGGER_PROXIMITY) {
        // If the painting is barely rippling, make it stop rippling.
        if (obj->oPaintingCurrRippleMag <= 1.0f) {
            obj->oPaintingState = PAINTING_STATE_IDLE;
        }
    } else if (painting->rippleTrigger == RIPPLE_TRIGGER_CONTINUOUS) {
        const struct RippleAnimationTypeInfo *rippleAnim = painting_get_ripple_animation_type_info(painting);
        // If the painting is doing the entry ripple but the ripples are as small as those from the
        // passive ripple, make it do a passive ripple.
        // If Mario goes below the surface but doesn't warp, the painting will eventually reset.
        if ((obj->oPaintingState == PAINTING_STATE_ENTERED)
         && (obj->oPaintingCurrRippleMag <= rippleAnim->passive.mag)) {
            obj->oPaintingState = PAINTING_STATE_RIPPLING;
            painting_set_ripple_animation(obj, &rippleAnim->passive);
        }
    }
}

#if defined(ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS) || defined(UNLOCK_ALL)
s32 gDDDPaintingNotMoved = FALSE;

/**
 * Controls the x coordinate of the DDD painting.
 *
 * Before Mario gets the "Board Bowser's Sub" star in DDD, the painting spawns at frontPos.
 *
 * If Mario just got the star, the painting's x coordinate moves to backPos at a rate of `speed` units.
 *
 * When the painting reaches backPos, a save flag is set so that the painting will spawn at backPos
 * whenever it loads.
 */
void move_ddd_painting(struct Object *obj, f32 frontPos, f32 backPos, f32 speed) {
#ifdef UNLOCK_ALL
    obj->oPosX = backPos;
    return;
#endif
    // Obtain the DDD star flags and find out whether Board Bowser's Sub was collected.
    if (save_file_get_star_flags((gCurrSaveFileNum - 1), COURSE_NUM_TO_INDEX(COURSE_DDD)) & STAR_FLAG_ACT_1) {
        // Check whether DDD has already moved back.
        if (gDDDPaintingNotMoved) {
            // If we've collected the star but not moved the painting back...
            // Each frame, move the painting by a certain speed towards the back area.
            obj->oPosX += speed;
            if (obj->oPosX >= backPos) {
                obj->oPosX = backPos;

                gDDDPaintingNotMoved = FALSE;
            }
        } else {
            // If the painting has already moved back, place it in the back position.
            obj->oPosX = backPos;
        }
    } else {
        // If we haven't collected the star, put the painting at the front.
        obj->oPosX = frontPos;

        // Set this so the painting gets moved once the star is collected.
        gDDDPaintingNotMoved = TRUE;
    }
}
#endif // (ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS || UNLOCK_ALL)

/**
 * Set the painting's state, causing it to start a passive ripple or a ripple from Mario entering.
 *
 * @param obj identifies the painting that is changing state
 * @param state The state to enter
 * @param centerRipples if TRUE, center the ripples instead of putting them at Mario's position
 * @param doResetTimer if TRUE, set the timer to 0
 */
void painting_state(struct Object *obj, s8 state, s8 centerRipples, s8 doResetTimer) {
    const struct Painting *painting = obj->oPaintingImageInfo;
    const struct RippleAnimationTypeInfo *rippleAnim = painting_get_ripple_animation_type_info(painting);

    // Use a different set of variables depending on the state
    switch (state) {
        case PAINTING_STATE_RIPPLING:
            painting_set_ripple_animation(obj, &rippleAnim->passive);
            break;

        case PAINTING_STATE_ENTERED:
            painting_set_ripple_animation(obj, &rippleAnim->entry);
            break;
    }

    obj->oPaintingState = state;

    // Set the ripple position.
    if (centerRipples) {
        obj->oPaintingRipplePosX = (painting->sizeX * 0.5f);
        obj->oPaintingRipplePosY = (painting->sizeY * 0.5f);
    } else {
        obj->oPaintingRipplePosX = obj->oPaintingLocalMarioPosX;
        obj->oPaintingRipplePosY = obj->oPaintingLocalMarioPosY;
    }

#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
    if (GET_BPARAM1(obj->oBehParams) == PAINTING_ID_CASTLE_WDW) {
        // Set Mario's Y position for the WDW water level.
        // The WDW painting is at 1306 in vanilla.
        gPaintingMarioYEntry = gMarioObject->oPosY - obj->oPosY;
    }
#endif

    if (doResetTimer) {
        obj->oPaintingRippleTimer = 0;
    }
}


void bhv_painting_loop(void) {
    struct Object *obj = o;
    const struct Painting *painting = obj->oPaintingImageInfo;

    // Update the painting info.
    painting_update_mario_pos_and_flags(obj);

    // Update the ripple, may automatically reset the painting's state.
    painting_update_ripple_state(obj);

#if defined(ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS) || defined(UNLOCK_ALL)
    // Update the DDD painting before drawing.
    if (GET_BPARAM1(obj->oBehParams) == PAINTING_ID_CASTLE_DDD) {
        move_ddd_painting(obj, 3456.0f, 5529.6f, 20.0f);
    }
#endif // (ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS || UNLOCK_ALL)

    if (painting->rippleTrigger == RIPPLE_TRIGGER_PROXIMITY) {
        // Proximity type:
        if (obj->oPaintingChangedFlags & RIPPLE_FLAG_ENTER) {
            painting_state(obj, PAINTING_STATE_ENTERED, FALSE, TRUE ); // Entering
        } else if (obj->oPaintingState != PAINTING_STATE_ENTERED && (obj->oPaintingChangedFlags & RIPPLE_FLAG_RIPPLE)) {
            painting_state(obj, PAINTING_STATE_RIPPLING,  FALSE, TRUE ); // Wobbling
        }
    } else if (painting->rippleTrigger == RIPPLE_TRIGGER_CONTINUOUS) {
        // Continuous type:
        if (obj->oPaintingChangedFlags & RIPPLE_FLAG_ENTER) {
            painting_state(obj, PAINTING_STATE_ENTERED, FALSE, FALSE); // Entering
        } else if (obj->oPaintingState == PAINTING_STATE_IDLE) {
            painting_state(obj, PAINTING_STATE_RIPPLING,  TRUE,  TRUE ); // Idle
        }
    }
}
