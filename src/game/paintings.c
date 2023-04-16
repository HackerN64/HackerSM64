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
 *          painting_data_vertices: The mesh positions are generated from a base mesh.
 *          painting_data_mesh_neighbor_tris: The lighting for the ripple is also generated from a base table.
 *          Each painting's texture uses yet another table to determine the order to map its texture to the mesh.
 *      In levels/[LEVEL]/painting.inc.c:
 *          PaintingImage structs, texture pointers.
 *
 * Painting actions:
 * Paintings spawn in the PAINTING_ACT_IDLE action
 *      From PAINTING_ACT_IDLE, paintings can change to PAINTING_ACT_RIPPLING or PAINTING_ACT_ENTERED
 *        - This action checks for PAINTING_ACT_ENTERED because if Mario waits long enough, a RIPPLE_TRIGGER_PROXIMITY painting could reset to PAINTING_ACT_IDLE
 *
 * Paintings in PAINTING_ACT_RIPPLING are passively rippling.
 *      For RIPPLE_TRIGGER_PROXIMITY paintings, this means Mario bumped the front of the painting.
 *
 *      Paintings that use RIPPLE_TRIGGER_CONTINUOUS try to transition to this action as soon as possible, usually when Mario enters the room.
 *
 *      A PROXIMITY painting will automatically reset to PAINTING_ACT_IDLE if its ripple magnitude becomes small enough.
 *
 * Paintings in PAINTING_ACT_ENTERED have been entered by Mario.
 *      A CONTINUOUS painting will automatically reset to PAINTING_ACT_RIPPLING if its ripple magnitude becomes small enough.
 */

/**
 * Array of pointers to painting image data structs.
 */
const struct PaintingImage* sPaintings[] = {
    [PAINTING_ID_NULL           ] = NULL,
    [PAINTING_ID_CASTLE_BOB     ] = &bob_painting,
    [PAINTING_ID_CASTLE_CCM     ] = &ccm_painting,
    [PAINTING_ID_CASTLE_WF      ] = &wf_painting,
    [PAINTING_ID_CASTLE_JRB     ] = &jrb_painting,
    [PAINTING_ID_CASTLE_LLL     ] = &lll_painting,
    [PAINTING_ID_CASTLE_SSL     ] = &ssl_painting,
    [PAINTING_ID_CASTLE_HMC     ] = &hmc_painting,
    [PAINTING_ID_CASTLE_DDD     ] = &ddd_painting,
    [PAINTING_ID_CASTLE_WDW     ] = &wdw_painting,
    [PAINTING_ID_CASTLE_THI_TINY] = &thi_tiny_painting,
    [PAINTING_ID_CASTLE_TTM     ] = &ttm_painting,
    [PAINTING_ID_CASTLE_TTC     ] = &ttc_painting,
    [PAINTING_ID_CASTLE_SL      ] = &sl_painting,
    [PAINTING_ID_CASTLE_THI_HUGE] = &thi_huge_painting,
    [PAINTING_ID_CASTLE_RR      ] = &rr_painting,
    [PAINTING_ID_HMC_COTMC      ] = &cotmc_painting,
    [PAINTING_ID_TTM_SLIDE      ] = &ttm_slide_painting,
};

/**
 * A list of preset constants for the ripple animations.
 */
const struct RippleAnimationPair sRippleAnimations[] = {
    [RIPPLE_ANIM_CONTINUOUS] = {
        .passive = { .mag =  10.0f, .decay = 1.0f,    .rate = 0.05f, .dispersion = 15.0f },
        .entry   = { .mag =  30.0f, .decay = 0.98f,   .rate = 0.05f, .dispersion = 15.0f },
    },
    [RIPPLE_ANIM_PROXIMITY] = {
        .passive = { .mag =  20.0f, .decay = 0.9608f, .rate = 0.24f, .dispersion = 40.0f },
        .entry   = { .mag =  80.0f, .decay = 0.9524f, .rate = 0.14f, .dispersion = 30.0f },
    },
    [RIPPLE_ANIM_PROXIMITY_LARGE] = {
        .passive = { .mag =  40.0f, .decay = 0.9608f, .rate = 0.12f, .dispersion = 80.0f },
        .entry   = { .mag = 160.0f, .decay = 0.9524f, .rate = 0.07f, .dispersion = 60.0f },
    },
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
 * The mesh used in game, painting_data_vertices, is in bin/segment2.c.
 */
void painting_generate_mesh(struct Object* obj, const PaintingData* vtxData, PaintingData numVtx, struct PaintingMeshVertex* paintingMesh) {
    const struct RippleAnimation* objRippleAnim = obj->oPaintingRippleAnimation;
    PaintingData i, tri;

    /// Controls the peaks of the ripple.
    f32 rippleMag = obj->oPaintingCurrRippleMag;
    /// Controls the ripple's frequency.
    f32 rippleRate = objRippleAnim->rate;
    /// Controls how fast the ripple spreads.
    f32 dispersionFactor = (1.0f / objRippleAnim->dispersion);
    /// How far the ripple has spread.
    f32 rippleTimer = obj->oPaintingRippleTimer;

    /// X and Y of the ripple origin.
    f32 rippleX = obj->oPaintingRipplePosX;
    f32 rippleY = obj->oPaintingRipplePosY;

    f32 sizeRatioX = (obj->header.gfx.scale[0] / PAINTING_SIZE);
    f32 sizeRatioY = (obj->header.gfx.scale[1] / PAINTING_SIZE);

    f32 dx, dy;
    f32 rippleDistance;

    PaintingData vtxX, vtxY;

    // Loop through all the painting vertices and calculate the ripple magnitude at each point.
    // Accesses are off by 1 since the first entry is the number of vertices.
    for (i = 0; i < numVtx; i++) {
        tri = (i * 3);

        vtxX = vtxData[tri + 1];
        vtxY = vtxData[tri + 2];

        paintingMesh->pos[0] = vtxX;
        paintingMesh->pos[1] = vtxY;
        // The "Z coordinate" of each vertex in painting_data_vertices is either 1 or 0.
        // Instead of being an actual coordinate, it just determines whether the vertex can move or not.
        if (vtxData[tri + 3]) {
            // Scale and calculate the distance to the ripple origin.
            dx = ((vtxX * sizeRatioX) - rippleX);
            dy = ((vtxY * sizeRatioY) - rippleY);
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
 * The mesh used in game, painting_data_vertices, is in bin/segment2.c.
 */
void painting_calculate_triangle_normals(const PaintingData* triangleData, struct PaintingNeighborTris* neighborTris, struct PaintingMeshVertex* paintingMesh, Vec3f* paintingTriNorms) {
    struct PaintingNeighborTris* vtn = NULL;
    PaintingData i, j;
    PaintingData numTris = *triangleData++;
    Vec3f vp[3]; // Vertex positions

    for (i = 0; i < numTris; i++) {
        // Get the 3 vertices from the triangle.
        for (j = 0; j < 3; j++) {
            vec3s_to_vec3f(vp[j], paintingMesh[triangleData[j]].pos);

            // Add the current triangle to each vertex's neighbors list.
            vtn = &neighborTris[triangleData[j]];
            if (vtn->numNeighbors < ARRAY_COUNT(vtn->neighborTris)) {
                vtn->neighborTris[vtn->numNeighbors++] = i;
            }
        }

        // Cross product to find each triangle's normal vector.
        find_vector_perpendicular_to_plane(paintingTriNorms[i], vp[0], vp[1], vp[2]);

        triangleData += 3;
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
 *      Where each 'tri' is an index into paintingTriNorms.
 *      Entry i in `neighborTris` corresponds to the vertex at paintingMesh[i]
 *
 * The table used in game, painting_data_mesh_neighbor_tris, is in bin/segment2.c.
 */
void painting_average_vertex_normals(struct PaintingNeighborTris* neighborTris, PaintingData numVtx, struct PaintingMeshVertex* paintingMesh, Vec3f* paintingTriNorms) {
    PaintingData tri;
    PaintingData i, j;
    PaintingData numNeighbors;

    for (i = 0; i < numVtx; i++) {
        Vec3f n = { 0.0f, 0.0f, 0.0f };

        // The first number of each entry is the number of adjacent tris.
        struct PaintingNeighborTris* vtn = &neighborTris[i];
        numNeighbors = vtn->numNeighbors;

        // Average the surface normals from each neighboring tri.
        for (j = 0; j < numNeighbors; j++) {
            tri = vtn->neighborTris[j];
            vec3f_add(n, paintingTriNorms[tri]);
        }

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
#define TRI_PER_GRP (VTX_BUF_MAX / 3) // Rounded,  5 or 10
#define VTX_PER_GRP (TRI_PER_GRP * 3) // Rounded, 15 or 30

/**
 * Creates a display list that draws the rippling painting, with 'img' mapped to the painting's mesh, using 'triangleMap'.
 *
 * If the triangleMap doesn't describe the whole mesh, then multiple calls are needed to draw the whole painting.
 *
 * TODO: Automatically create seams between segments based on the image count.
 */
Gfx* render_painting_segment(const Texture* img, s16 index, s16 imageCount, s16 tWidth, s16 tHeight, struct PaintingMeshVertex* paintingMesh, const PaintingData* triangleMap, PaintingData numTris, Alpha alpha) {
    struct PaintingMeshVertex* currVtx = NULL;
    PaintingData group;
    PaintingData groupIndex;
    PaintingData map;
    PaintingData triGroup;

    // We can fit VTX_PER_GRP vertices in the RSP's vertex buffer.
    // Group triangles by TRI_PER_GRP, with one remainder group.
    PaintingData triGroups    = (numTris / TRI_PER_GRP);
    PaintingData remGroupTris = (numTris % TRI_PER_GRP);
    PaintingData numVtx       = (numTris * 3); // 3 verts per tri

    Vtx* verts = alloc_display_list(numVtx * sizeof(Vtx));
    Gfx* dlist = alloc_display_list(
        SIZEOF_GFX_CMD(LoadBlockTexture(0,0,0,0)) +
        (triGroups * (
            SIZEOF_GFX_CMD(SPVertex(0,0,0)) +
            SIZEOF_GFX_CMD(SPDisplayList(0))
        )) +
        SIZEOF_GFX_CMD(SPVertex(0,0,0)) +
        (remGroupTris * (
            SIZEOF_GFX_CMD(SP1Triangle(0,0,0,0))
        )) +
        SIZEOF_GFX_CMD(SPEndDisplayList())
    );
    Gfx* gfx = dlist;

    gLoadBlockTexture(gfx++, tWidth, tHeight, G_IM_FMT_RGBA, img);

    // Width and height of each section.
    const f32 dx = (PAINTING_SIZE / 1);
    const f32 dy = (PAINTING_SIZE / imageCount);

    // These are used to scale the texture positions into vertex positions.
    const f32 tWidthScale  = (TC(tWidth ) / dx);
    const f32 tHeightScale = (TC(tHeight) / dy);

    // Bottom Y position of the current section.
    const s16 y2 = ((index + 1) * dy);

    // Draw the groups of TRI_PER_GRP first.
    for (group = 0; group < triGroups; group++) {
        // The index of the first vertex in the group.
        groupIndex = (group * VTX_PER_GRP);

        // Each group is a list of VTX_PER_GRP mappings.
        triGroup = (1 + groupIndex);

        // Vertices within the group
        for (map = 0; map < VTX_PER_GRP; map++) {
            // Get a pointer to the current vertex.
            currVtx = &paintingMesh[triangleMap[triGroup + map]];
            make_vertex(verts, (groupIndex + map),
                currVtx->pos[0],
                currVtx->pos[1],
                currVtx->pos[2],
                (currVtx->pos[0] * tWidthScale),
                ((y2 - currVtx->pos[1]) * tHeightScale),
                currVtx->norm[0],
                currVtx->norm[1],
                currVtx->norm[2],
                alpha
            );
        }

        // Load the vertices and draw the TRI_PER_GRP triangles.
        gSPVertex(gfx++, VIRTUAL_TO_PHYSICAL(verts + groupIndex), VTX_PER_GRP, 0);
        gSPDisplayList(gfx++, dl_paintings_ripple_triangles);
    }

    // One group left with < TRI_PER_GRP triangles.
    triGroup = (1 + (triGroups * VTX_PER_GRP));

    // Map the texture to the triangles.
    for (map = 0; map < (remGroupTris * 3); map++) {
        // Get a pointer to the current vertex.
        currVtx = &paintingMesh[triangleMap[triGroup + map]];
        make_vertex(verts, ((triGroups * VTX_PER_GRP) + map),
            currVtx->pos[0],
            currVtx->pos[1],
            currVtx->pos[2],
            (currVtx->pos[0] * tWidthScale),
            ((y2 - currVtx->pos[1]) * tHeightScale),
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
 * Gets the exponent of an integer by converting it into a float and extracting the exponent bits.
 */
static s32 get_exponent(s32 x) {
    union {
        struct PACKED {
            u32 sign     :  1;
            u32 exponent :  8;
            u32 mantissa : 23;
        };
        f32 f;
    } b = { .f = x };
    return (b.exponent - 127);
}

/**
 * Set up the texture format in the display list.
 */
void painting_setup_textures(Gfx** gfx, s16 tWidth, s16 tHeight, _Bool isEnvMap) {
    s16 cm = isEnvMap ? (G_TX_WRAP | G_TX_NOMIRROR) : G_TX_CLAMP;
    u32 masks = get_exponent(tWidth);
    u32 maskt = get_exponent(tHeight);

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
Gfx* dl_painting_rippling(const struct PaintingImage* paintingImage, struct PaintingMeshVertex* paintingMesh, const PaintingData* triangleMap) {
    s16 imageCount = paintingImage->imageCount;
    s16 tWidth = paintingImage->textureWidth;
    s16 tHeight = paintingImage->textureHeight;
    const Texture** tArray = segmented_to_virtual(paintingImage->textureArray);
    _Bool isEnvMap = (paintingImage->imageType == PAINTING_IMAGE_TYPE_ENV_MAP);

    Gfx* dlist = alloc_display_list(
        SIZEOF_GFX_CMD(SPMatrix(0,0)) +
        SIZEOF_GFX_CMD(SPDisplayList(0)) +
        SIZEOF_GFX_CMD(DPSetTile(0,0,0,0,0,0,0,0,0,0,0,0)) +
        SIZEOF_GFX_CMD(DPSetTileSize(0,0,0,0,0)) +
        (imageCount * (
            SIZEOF_GFX_CMD(SPDisplayList(0))
        )) +
        SIZEOF_GFX_CMD(SPDisplayList(0)) +
        SIZEOF_GFX_CMD(SPPopMatrix(0)) +
        SIZEOF_GFX_CMD(SPEndDisplayList())
    );
    Gfx* gfx = dlist;

    if (dlist == NULL) {
        return dlist;
    }
    // Scale
    Mtx* scale = alloc_display_list(sizeof(Mtx));
    guScale(
        scale,
        (1.0f / PAINTING_SIZE),
        (1.0f / PAINTING_SIZE),
        1.0f
    );
    gSPMatrix(gfx++, scale, (G_MTX_MODELVIEW | G_MTX_MUL | G_MTX_PUSH));

    Gfx* beginDl = (isEnvMap ? dl_paintings_env_mapped_begin : dl_paintings_rippling_begin);
    gSPDisplayList(gfx++, beginDl);

    painting_setup_textures(&gfx, tWidth, tHeight, isEnvMap);

    PaintingData numTris = (triangleMap[0] / imageCount);
    // Map each image to the mesh's vertices.
    for (s16 i = 0; i < imageCount; i++) {
        // Render a section of the painting.
        gSPDisplayList(gfx++, render_painting_segment(tArray[i], i, imageCount, tWidth, tHeight, paintingMesh, triangleMap, numTris, paintingImage->alpha));

        triangleMap += (numTris * 3);
    }

    Gfx* endDl = (isEnvMap ? dl_paintings_env_mapped_end : dl_paintings_rippling_end);
    gSPDisplayList(gfx++, endDl);
    gSPPopMatrix(gfx++, G_MTX_MODELVIEW);
    gSPEndDisplayList(gfx);

    return dlist;
}

/**
 * Generates a mesh, calculates vertex normals for lighting, and renders a rippling painting.
 * The mesh and vertex normals are regenerated and freed every frame.
 */
Gfx* display_painting_rippling(struct Object* obj) {
    const struct PaintingImage* paintingImage = obj->oPaintingImage;
    const PaintingData* vtxData = segmented_to_virtual(painting_data_vertices);
    const PaintingData* triangleData = segmented_to_virtual(painting_data_triangles);
    PaintingData numVtx = vtxData[0];
    PaintingData numTris = triangleData[0];
    Gfx* dlist = NULL;
    // When a painting is rippling, this mesh is generated each frame using the Painting's parameters.
    // This mesh only contains the vertex positions and normals.
    // Paintings use an additional array to map textures to the mesh.
    //! TODO: Find out why clearing this between frames causes flickering.
    struct PaintingMeshVertex* paintingMesh = mem_pool_alloc(gEffectsMemoryPool, (numVtx * sizeof(struct PaintingMeshVertex)));
    // A list of neighbor triangles for each vertes. This gets cleared each frame while 'paintingMesh' isn't.
    struct PaintingNeighborTris* neighborTris = mem_pool_alloc(gEffectsMemoryPool, (numVtx * sizeof(struct PaintingNeighborTris)));
    bzero(neighborTris, (numVtx * sizeof(struct PaintingNeighborTris)));
    // The painting's surface normals, used to approximate each of the vertex normals (for gouraud shading).
    Vec3f *paintingTriNorms = mem_pool_alloc(gEffectsMemoryPool, (numTris * sizeof(Vec3f)));

    // Generate the mesh and its lighting data
    painting_generate_mesh(obj, vtxData, numVtx, paintingMesh);
    painting_calculate_triangle_normals(triangleData, neighborTris, paintingMesh, paintingTriNorms);
    painting_average_vertex_normals(neighborTris, numVtx, paintingMesh, paintingTriNorms);

    // Map the painting's texture depending on the painting's texture type.
    dlist = dl_painting_rippling(paintingImage, paintingMesh, triangleData);

    // The mesh data is freed every frame.
    //! This data is not actually cleared.
    mem_pool_free(gEffectsMemoryPool, paintingMesh);
    mem_pool_free(gEffectsMemoryPool, neighborTris);
    mem_pool_free(gEffectsMemoryPool, paintingTriNorms);

    return dlist;
}

Gfx* dl_painting_not_rippling(struct Object* obj) {
    const struct PaintingImage* paintingImage = obj->oPaintingImage;
    Alpha alpha = paintingImage->alpha;
    s16 imageCount = paintingImage->imageCount;
    s32 shaded = paintingImage->shaded;
    Gfx* dlist = alloc_display_list(
        SIZEOF_GFX_CMD(SPMatrix(0,0)) +
        SIZEOF_GFX_CMD(SPDisplayList(0)) +
        SIZEOF_GFX_CMD(SPVertex(0,0,0)) +
        SIZEOF_GFX_CMD(DPSetTile(0,0,0,0,0,0,0,0,0,0,0,0)) +
        SIZEOF_GFX_CMD(DPSetTileSize(0,0,0,0,0)) +
        (imageCount * (
            SIZEOF_GFX_CMD(DPSetTextureImage(0,0,0,0)) +
            SIZEOF_GFX_CMD(DPLoadSync()) +
            SIZEOF_GFX_CMD(DPLoadBlock(0,0,0,0,0)) +
            SIZEOF_GFX_CMD(SP2Triangles(0,0,0,0,0,0,0,0))
        )) +
        SIZEOF_GFX_CMD(SPDisplayList(0)) +
        (!shaded * (
            SIZEOF_GFX_CMD(SPSetGeometryMode(0))
        )) +
        SIZEOF_GFX_CMD(SPPopMatrix(0)) +
        SIZEOF_GFX_CMD(SPEndDisplayList())
    );
    Gfx* gfx = dlist;

    if (dlist == NULL) {
        return dlist;
    }

    // Scale
    Mtx* scale = alloc_display_list(sizeof(Mtx));
    guScale(
        scale,
        (1.0f / PAINTING_SIZE),
        (1.0f / PAINTING_SIZE),
        1.0f
    );
    gSPMatrix(gfx++, scale, (G_MTX_MODELVIEW | G_MTX_MUL | G_MTX_PUSH));

    Vtx* verts = alloc_display_list((imageCount * 4) * sizeof(*verts));
    Vec3c n;

    const Texture** textures = segmented_to_virtual(paintingImage->textureArray);

    _Bool isEnvMap = (paintingImage->imageType == PAINTING_IMAGE_TYPE_ENV_MAP);

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

    s16 tWidth  = paintingImage->textureWidth;
    s16 tHeight = paintingImage->textureHeight;

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
    s32 quadIndex;
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

    Gfx* endDl = (isEnvMap ? dl_paintings_env_mapped_end : dl_paintings_textured_end);
    gSPDisplayList(gfx++, endDl);

    if (!shaded) {
        gSPSetGeometryMode(gfx++, G_LIGHTING);
    }

    gSPPopMatrix(gfx++, G_MTX_MODELVIEW);
    gSPEndDisplayList(gfx);

    return dlist;
}

/**
 * Draw the painting.
 */
Gfx* geo_painting_draw(s32 callContext, struct GraphNode* node, UNUSED void* context) {
    struct Object* obj = gCurGraphNodeObjectNode;

    if (obj == NULL) {
        return NULL;
    }

    // Get the const painting image data.
    const struct PaintingImage* paintingImage = obj->oPaintingImage;

    if (paintingImage == NULL) {
        return NULL;
    }

    Gfx* paintingDlist = NULL;

    if (callContext == GEO_CONTEXT_RENDER) {
        // Draw the painting.
        if (
            paintingImage->textureArray != NULL &&
            paintingImage->imageCount    > 0    &&
            paintingImage->textureWidth  > 0    &&
            paintingImage->textureHeight > 0    &&
            paintingImage->imageType != PAINTING_IMAGE_TYPE_INVISIBLE &&
            paintingImage->alpha > 0x00
        ) {
            // Determine whether the painting is opaque or transparent.
            if (paintingImage->alpha == 0xFF) {
                SET_GRAPH_NODE_LAYER(node->flags, LAYER_OCCLUDE_SILHOUETTE_OPAQUE);
            } else {
                SET_GRAPH_NODE_LAYER(node->flags, LAYER_TRANSPARENT);
            }

            if (obj->oAction == PAINTING_ACT_IDLE) {
                paintingDlist = dl_painting_not_rippling(obj);
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
void painting_update_room(struct Object* obj) {
    // The center of the painting, but with an offset since paintings are usually between floor triangle edges laterally.
    Vec3f distPos = {
        (obj->header.gfx.scale[0] * 0.5f),
        (obj->header.gfx.scale[1] * 0.5f),
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
    struct Object* obj = o;

    // Get the painting id from the first byte of the behavior params. The second byte is used for the warp node ID.
    s32 id = GET_BPARAM1(obj->oBehParams);

    const struct PaintingImage* paintingImage = segmented_to_virtual(sPaintings[id]);

    // Set the object's painting image data pointer.
    obj->oPaintingImage = paintingImage;

    // Set the object's initial scale.
    obj->header.gfx.scale[0] = paintingImage->sizeX;
    obj->header.gfx.scale[1] = paintingImage->sizeY;

    // Update the painting object's room.
    painting_update_room(obj);
}

/// - LOOP -

/**
 * Check for Mario entering the painting. Returns changed action.
 */
enum oActionsPainting painting_update_mario_pos(struct Object* obj, Vec3f marioLocalPos) {
    struct MarioState* m = gMarioState;

    // PAINTING_ACT_IDLE will not change the action.
    enum oActionsPainting newAction = PAINTING_ACT_IDLE;

    Vec3f marioWorldPos;
    Vec3s rotation;

    // Add PAINTING_MARIO_Y_OFFSET to make the ripple closer to Mario's center of mass.
    vec3f_copy_y_off(marioWorldPos, m->pos, PAINTING_MARIO_Y_OFFSET);

    // Get the painting's rotation.
    vec3i_to_vec3s(rotation, &obj->oFaceAngleVec);

    // Get Mario's position in the painting's frame of reference.
    vec3f_world_pos_to_local_pos(marioLocalPos, marioWorldPos, &obj->oPosVec, rotation);

    // Check if Mario is within the painting bounds laterally in local space.
    if (
        marioLocalPos[0] > -PAINTING_EDGE_MARGIN &&
        marioLocalPos[0] < (obj->header.gfx.scale[0] + PAINTING_EDGE_MARGIN) &&
        marioLocalPos[1] > -PAINTING_EDGE_MARGIN &&
        marioLocalPos[1] < (obj->header.gfx.scale[1] + PAINTING_EDGE_MARGIN)
    ) {
        if (marioLocalPos[2] > PAINTING_WOBBLE_WARP_THRESHOLD) {
            // In front of the painting, check whether Mario is inside the wobble zone.
            if (marioLocalPos[2] < PAINTING_WOBBLE_DEPTH) {
                newAction = PAINTING_ACT_RIPPLING;
            }
        } else {
            // Behind the painting, check whether Mario is inside the warp zone.
            if (marioLocalPos[2] > -PAINTING_WARP_DEPTH) {
                newAction = PAINTING_ACT_ENTERED;
            }
        }
    }

    // Detect whether Mario is entering this painting, and set paintingObj accordingly
    if (newAction == PAINTING_ACT_ENTERED) {
        // Mario has entered the painting.
        m->paintingObj = obj;
    } else if (m->paintingObj == obj) {
        // Reset m->paintingObj if it's this painting and this painting is not entered.
        m->paintingObj = NULL;
    }

    enum oActionsPainting oldAction = obj->oPaintingStoredAction;

    obj->oPaintingStoredAction = newAction;

    // The action to change to later. PAINTING_ACT_IDLE will not update the action.
    if (newAction != oldAction) {
        return newAction;
    } else {
        return PAINTING_ACT_IDLE;
    }
}

/**
 * Returns a pointer to the RippleAnimationPair that best fits the painting type.
 */
const struct RippleAnimationPair* painting_get_ripple_animation_type_info(struct Object* obj) {
    const struct PaintingImage* paintingImage = obj->oPaintingImage;
    s8 rippleAnimationType = RIPPLE_ANIM_CONTINUOUS;

    if (paintingImage->rippleTrigger == RIPPLE_TRIGGER_PROXIMITY) {
        rippleAnimationType = RIPPLE_ANIM_PROXIMITY;

        if (
            obj->header.gfx.scale[0] >= PAINTING_SCALE_LARGE_RIPPLE_THRESHOLD ||
            obj->header.gfx.scale[1] >= PAINTING_SCALE_LARGE_RIPPLE_THRESHOLD
        ) {
            rippleAnimationType = RIPPLE_ANIM_PROXIMITY_LARGE;
        }
    }

    return &sRippleAnimations[rippleAnimationType];
}

/**
 * Set a painting's ripple animation and magnitude.
 */
void painting_set_ripple_animation_type(struct Object* obj, const struct RippleAnimation* baseRippleAnim) {
    obj->oPaintingRippleAnimation = baseRippleAnim;
    obj->oPaintingCurrRippleMag = baseRippleAnim->mag;
}

#if defined(ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS) || defined(UNLOCK_ALL)
// Whether the painting should be moved or not.
_Bool gDDDPaintingNotMoved = FALSE;

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
void move_ddd_painting(struct Object* obj, f32 frontPos, f32 backPos, f32 speed) {
#ifdef UNLOCK_ALL
    obj->oPosX = backPos;
    return;
#endif
    // Obtain the DDD star flags and find out whether Board Bowser's Sub was collected.
    if (
        save_file_get_star_flags(
            (gCurrSaveFileNum - 1),
            COURSE_NUM_TO_INDEX(COURSE_DDD)
        ) & STAR_FLAG_ACT_1
    ) {
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
 * Set the painting's action, causing it to start a passive ripple or a ripple from Mario entering.
 *
 * @param obj identifies the painting that is changing state.
 * @param ripplePosX,ripplePosY the position of the ripples.
 * @param shouldResetTimer if TRUE, set the timer to 0.
 */
void painting_start_ripples(struct Object* obj, f32 ripplePosX, f32 ripplePosY, _Bool shouldResetTimer) {
    const struct RippleAnimationPair* rippleAnim = painting_get_ripple_animation_type_info(obj);

    // Use a different set of variables depending on the state
    switch (obj->oAction) {
        case PAINTING_ACT_RIPPLING:
            painting_set_ripple_animation_type(obj, &rippleAnim->passive);
            break;

        case PAINTING_ACT_ENTERED:
            painting_set_ripple_animation_type(obj, &rippleAnim->entry);
            break;
    }

    // Set the ripple position.
    obj->oPaintingRipplePosX = ripplePosX;
    obj->oPaintingRipplePosY = ripplePosY;

#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
    if (GET_BPARAM1(obj->oBehParams) == PAINTING_ID_CASTLE_WDW) {
        // Set Mario's Y position for the WDW water level.
        // The WDW painting is at 1306 in vanilla.
        gPaintingMarioYEntry = gMarioObject->oPosY - obj->oPosY;
    }
#endif

    if (shouldResetTimer) {
        obj->oPaintingRippleTimer = 0;
    }
}

void bhv_painting_loop(void) {
    struct Object* obj = o;
    const struct PaintingImage* paintingImage = obj->oPaintingImage;
    Vec3f marioLocalPos;

    // Update the painting's next action based on Mario's relative position.
    enum oActionsPainting newAction = painting_update_mario_pos(obj, marioLocalPos);

    const struct RippleAnimation* objRippleAnim = obj->oPaintingRippleAnimation;

    // Decay the ripple over time.
    if (objRippleAnim != NULL) {
        obj->oPaintingCurrRippleMag *= objRippleAnim->decay;
    }

    // Update the ripple's timer, making it propagate outwards.
    if (obj->oPaintingRippleTimer++ < 0) {
        // Reset the timer to 0 if it overflows.
        obj->oPaintingRippleTimer = 0;
    }

    if (paintingImage->rippleTrigger == RIPPLE_TRIGGER_PROXIMITY) {
        // Proximity trigger type:

        // If the painting is barely rippling, make it stop rippling.
        if (obj->oPaintingCurrRippleMag <= 1.0f) {
            obj->oAction = PAINTING_ACT_IDLE;
        }

        if (newAction == PAINTING_ACT_ENTERED) {
            obj->oAction = newAction;
            painting_start_ripples(obj, marioLocalPos[0], marioLocalPos[1], TRUE); // Start entering
        } else if (obj->oAction != PAINTING_ACT_ENTERED && (newAction == PAINTING_ACT_RIPPLING)) {
            obj->oAction = newAction;
            painting_start_ripples(obj, marioLocalPos[0], marioLocalPos[1], TRUE); // Start wobbling
        }
    } else if (paintingImage->rippleTrigger == RIPPLE_TRIGGER_CONTINUOUS) {
        // Continuous trigger type:

        const struct RippleAnimationPair* rippleAnim = painting_get_ripple_animation_type_info(obj);
        // If the painting is doing the entry ripple but the ripples are as small as those from the
        // passive ripple, make it do a passive ripple.
        // If Mario goes below the surface but doesn't warp, the painting will eventually reset.
        if (
            (obj->oAction == PAINTING_ACT_ENTERED) &&
            (obj->oPaintingCurrRippleMag <= rippleAnim->passive.mag)
        ) {
            painting_set_ripple_animation_type(obj, &rippleAnim->passive);
            obj->oAction = PAINTING_ACT_RIPPLING;
        }

        if (newAction == PAINTING_ACT_ENTERED) {
            obj->oAction = newAction;
            painting_start_ripples(obj, marioLocalPos[0], marioLocalPos[1], FALSE); // Start entering
        } else if (obj->oAction == PAINTING_ACT_IDLE) {
            obj->oAction = PAINTING_ACT_RIPPLING;
            painting_start_ripples(obj, (obj->header.gfx.scale[0] * 0.5f), (obj->header.gfx.scale[1] * 0.5f), TRUE); // Start idle wobbling
        }
    }

#if defined(ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS) || defined(UNLOCK_ALL)
    // Update the DDD painting before drawing.
    if (GET_BPARAM1(obj->oBehParams) == PAINTING_ID_CASTLE_DDD) {
        move_ddd_painting(obj, 3456.0f, 5529.6f, 20.0f);
    }
#endif // (ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS || UNLOCK_ALL)
}
