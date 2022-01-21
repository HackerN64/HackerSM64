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
 * Implements the rippling painting effect. Paintings are GraphNodes that exist without being connected
 * to any particular object.
 *
 * Paintings are defined in level data. Look at levels/castle_inside/painting.inc.c for examples.
 *
 * The ripple effect uses data that is split into several parts:
 *      The mesh positions are generated from a base mesh. See seg2_painting_triangle_mesh near the
 *          bottom of bin/segment2.c
 *
 *      The lighting for the ripple is also generated from a base table, seg2_painting_mesh_neighbor_tris
 *          in bin/segment2.c
 *
 *      Each painting's texture uses yet another table to map its texture to the mesh.
 *          These maps are in level data, see levels/castle_inside/painting.inc.c for example.
 *
 *      Finally, each painting has two display lists, normal and rippling, which are defined in the same
 *      level data file as the Painting itself. See levels/castle_inside/painting.inc.c.
 *
 *
 * Painting state machine:
 * Paintings spawn in the PAINTING_IDLE state
 *      From IDLE, paintings can change to PAINTING_RIPPLE or PAINTING_ENTERED
 *        - This state checks for ENTERED because if Mario waits long enough, a PROXIMITY painting could
 *          reset to IDLE
 *
 * Paintings in the PAINTING_RIPPLE state are passively rippling.
 *      For RIPPLE_TRIGGER_PROXIMITY paintings, this means Mario bumped the wall in front of the
 *          painting.
 *
 *      Paintings that use RIPPLE_TRIGGER_CONTINUOUS try to transition to this state as soon as possible,
 *          usually when Mario enters the room.
 *
 *      A PROXIMITY painting will automatically reset to IDLE if its ripple magnitude becomes small enough.
 *
 * Paintings in the PAINTING_ENTERED state have been entered by Mario.
 *      A CONTINUOUS painting will automatically reset to RIPPLE if its ripple magnitude becomes small enough.
 */

/**
 * When a painting is rippling, this mesh is generated each frame using the Painting's parameters.
 *
 * This mesh only contains the vertex positions and normals.
 * Paintings use an additional array to map textures to the mesh.
 */
struct PaintingMeshVertex *gPaintingMesh;

/**
 * The painting's surface normals, used to approximate each of the vertex normals (for gouraud shading).
 */
Vec3f *gPaintingTriNorms;

/**
 * The painting that is currently rippling. Only one painting can be rippling at once.
 */
struct Object *gRipplingPainting = NULL;

/**
 * The id of the painting Mario has entered.
 */
struct Object *gEnteredPainting = NULL;

/**
 * Whether the DDD painting is moved forward, should being moving backwards, or has already moved backwards.
 */
s8 gDddPaintingStatus;

// HMC painting group
struct Painting *sHmcPaintings[] = {
    &cotmc_painting,
    NULL,
};

// Inside Castle painting group
struct Painting *sInsideCastlePaintings[] = {
    &bob_painting, &ccm_painting, &wf_painting,  &jrb_painting,      &lll_painting,
    &ssl_painting, &hmc_painting, &ddd_painting, &wdw_painting,      &thi_tiny_painting,
    &ttm_painting, &ttc_painting, &sl_painting,  &thi_huge_painting, &rr_painting,
    NULL,
};

// TTM painting group
struct Painting *sTtmPaintings[] = {
    &ttm_slide_painting,
    NULL,
};

// Array of all painting groups
struct Painting **sPaintingGroups[] = {
    sHmcPaintings,
    sInsideCastlePaintings,
    sTtmPaintings,
};

/**
 * Get the painting group from gCurrLevelNum.
 */
UNUSED s32 get_painting_group(void) {
    switch (gCurrLevelNum) {
        case LEVEL_HMC:
            return PAINTING_GROUP_HMC;
        case LEVEL_CASTLE:
            return PAINTING_GROUP_INSIDE_CASTLE;
        case LEVEL_TTM:
            return PAINTING_GROUP_TTM;
        default:
            return PAINTING_GROUP_NULL;
    }
}

/**
 * Set the painting's state, causing it to start a passive ripple or a ripple from Mario entering.
 *
 * @param state The state to enter
 * @param painting,paintingGroup identifies the painting that is changing state
 * @param xSource,ySource what to use for the x and y origin of the ripple
 * @param resetTimer if TRUE, set the timer to 0
 */
void painting_state(struct Object *obj, s8 state, s8 centerRipples, s8 resetTimer) {
    struct Painting *painting = obj->oPaintingPtr;

    // use a different set of variables depending on the state
    switch (state) {
        case PAINTING_RIPPLE:
            obj->oPaintingCurrRippleMag    = painting->passiveRippleMag;
            obj->oPaintingRippleDecay      = painting->passiveRippleDecay;
            obj->oPaintingCurrRippleRate   = painting->passiveRippleRate;
            obj->oPaintingDispersionFactor = painting->passiveDispersionFactor;
            break;

        case PAINTING_ENTERED:
            obj->oPaintingCurrRippleMag    = painting->entryRippleMag;
            obj->oPaintingRippleDecay      = painting->entryRippleDecay;
            obj->oPaintingCurrRippleRate   = painting->entryRippleRate;
            obj->oPaintingDispersionFactor = painting->entryDispersionFactor;
            break;
    }

    obj->oPaintingState = state;

    // Set the ripple position
    if (centerRipples) {
        obj->oPaintingRippleX = (painting->sizeX * 0.5f);
        obj->oPaintingRippleY = (painting->sizeY * 0.5f);
    } else {
        obj->oPaintingRippleX = obj->oPaintingLocalMarioPosX;
        obj->oPaintingRippleY = obj->oPaintingLocalMarioPosY;
    }

    // Set Mario's Y position for the WDW water level
    gPaintingMarioYEntry = gMarioObject->oPosY;

    if (resetTimer) {
        obj->oPaintingRippleTimer = 0;
    }

    gRipplingPainting = obj;
}

/**
 * Check for Mario entering one of the special floors associated with the painting.
 */
void painting_update_floors(struct Object *obj) {
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

    // Check if Mario is within the painting bounds laterally in local space.
    if (marioLocalPos[0] > -PAINTING_EDGE_MARGIN
     && marioLocalPos[0] < (obj->oPaintingPtr->sizeX + PAINTING_EDGE_MARGIN)
     && marioLocalPos[1] > -PAINTING_EDGE_MARGIN
     && marioLocalPos[1] < (obj->oPaintingPtr->sizeY + PAINTING_EDGE_MARGIN)) {
        // Check whether Mario is in the wobble zone.
        if (marioLocalPos[2] < PAINTING_WOBBLE_DEPTH
         && marioLocalPos[2] > 0.0f) {
            rippleFlags |= RIPPLE_FLAG_RIPPLE;
        }
        // Check whether Mario is in the warp zone.
        if (marioLocalPos[2] < 0.0f
         && marioLocalPos[2] > -PAINTING_WARP_DEPTH) {
            rippleFlags |= RIPPLE_FLAG_ENTER;
        }
    }

    s16 lastFlags = obj->oPaintingCurrFlags;
    // at most 1 of these will be nonzero
    obj->oPaintingCurrFlags = rippleFlags;

    // changedFlags is true if currFlags is true and lastFlags is false
    // (Mario just entered the floor on this frame)
    obj->oPaintingChangedFlags = ((lastFlags ^ obj->oPaintingCurrFlags) & obj->oPaintingCurrFlags);

    obj->oPaintingLocalMarioPosX = marioLocalPos[0];
    obj->oPaintingLocalMarioPosY = marioLocalPos[1];
}

/**
 * Update the ripple's timer and magnitude, making it propagate outwards.
 *
 * Automatically changes the painting back to IDLE state (or RIPPLE for continuous paintings) if the
 * ripple's magnitude becomes small enough.
 */
void painting_update_ripple_state(struct Painting *painting) {
    struct Object *obj = gCurGraphNodeObjectNode;

    if (obj->oPaintingUpdateCounter != obj->oLastPaintingUpdateCounter) {
        obj->oPaintingCurrRippleMag *= obj->oPaintingRippleDecay;

        if (obj->oPaintingRippleTimer >= (S16_MAX - 1)) {
            obj->oPaintingRippleTimer = 0;
        }

        obj->oPaintingRippleTimer++;
    }
    if (painting->rippleTrigger == RIPPLE_TRIGGER_PROXIMITY) {
        // if the painting is barely rippling, make it stop rippling
        if (obj->oPaintingCurrRippleMag <= 1.0f) {
            obj->oPaintingState = PAINTING_IDLE;
            gRipplingPainting = NULL;
        }
    } else if (painting->rippleTrigger == RIPPLE_TRIGGER_CONTINUOUS) {
        // if the painting is doing the entry ripple but the ripples are as small as those from the
        // passive ripple, make it do a passive ripple
        // If Mario goes below the surface but doesn't warp, the painting will eventually reset.
        if ((obj->oPaintingState == PAINTING_ENTERED)
         && (obj->oPaintingCurrRippleMag <= painting->passiveRippleMag)) {
            obj->oPaintingState = PAINTING_RIPPLE;
            obj->oPaintingCurrRippleMag    = painting->passiveRippleMag;
            obj->oPaintingRippleDecay      = painting->passiveRippleDecay;
            obj->oPaintingCurrRippleRate   = painting->passiveRippleRate;
            obj->oPaintingDispersionFactor = painting->passiveDispersionFactor;
        }
    }
}

/**
 * @return the ripple function at posX, posY
 * note that posX and posY correspond to a point on the face of the painting, not actual axes
 */
s32 calculate_ripple_at_point(struct Object *obj, f32 dx, f32 dy) {
    /// Controls the peaks of the ripple.
    f32 rippleMag = obj->oPaintingCurrRippleMag;
    /// Controls the ripple's frequency
    f32 rippleRate = obj->oPaintingCurrRippleRate;
    /// Controls how fast the ripple spreads
    f32 dispersionFactor = obj->oPaintingDispersionFactor;
    /// How far the ripple has spread
    f32 rippleTimer = obj->oPaintingRippleTimer;

    f32 distanceToOrigin = sqrtf(sqr(dx) + sqr(dy));
    // A larger dispersionFactor makes the ripple spread slower
    f32 rippleDistance = (distanceToOrigin / dispersionFactor);

    if (rippleTimer < rippleDistance) {
        // if the ripple hasn't reached the point yet, make the point magnitude 0
        return 0;
    } else {
        // use a cosine wave to make the ripple go up and down,
        // scaled by the painting's ripple magnitude
        // round it to an int and return it
        return roundf(rippleMag * coss((s16)((rippleRate * (rippleTimer - rippleDistance)) * 0x10000)));
    }
}

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
void painting_generate_mesh(struct Painting *painting, PaintingData *mesh, PaintingData numTris) {
    struct Object *obj = gCurGraphNodeObjectNode;
    s32 i, tri;

    gPaintingMesh = mem_pool_alloc(gEffectsMemoryPool, (numTris * sizeof(struct PaintingMeshVertex)));

    /// x and y ripple origin
    f32 rippleX = obj->oPaintingRippleX;
    f32 rippleY = obj->oPaintingRippleY;

    f32 sizeRatioX = (painting->sizeX / PAINTING_SIZE);
    f32 sizeRatioY = (painting->sizeY / PAINTING_SIZE);

    // accesses are off by 1 since the first entry is the number of vertices
    for (i = 0; i < numTris; i++) {
        tri = i * 3;
        gPaintingMesh[i].pos[0] = mesh[tri + 1];
        gPaintingMesh[i].pos[1] = mesh[tri + 2];
        // The "z coordinate" of each vertex in the mesh is either 1 or 0. Instead of being an
        // actual coordinate, it just determines whether the vertex moves
        if (mesh[tri + 3]) {
            gPaintingMesh[i].pos[2] = calculate_ripple_at_point(obj, ((gPaintingMesh[i].pos[0] * sizeRatioX) - rippleX),
                                                                     ((gPaintingMesh[i].pos[1] * sizeRatioY) - rippleY));
        } else {
            gPaintingMesh[i].pos[2] = 0;
        }
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
void painting_calculate_triangle_normals(PaintingData *mesh, PaintingData numVtx, PaintingData numTris) {
    s16 i;
    Vec3s v;
    Vec3f vp0, vp1, vp2;

    gPaintingTriNorms = mem_pool_alloc(gEffectsMemoryPool, (numTris * sizeof(Vec3f)));

    for (i = 0; i < numTris; i++) {
        PaintingData tri = (numVtx * 3) + (i * 3) + 2; // Add 2 because of the 2 length entries preceding the list
        vec3s_copy(v, &mesh[tri]);
        vec3s_to_vec3f(vp0, gPaintingMesh[v[0]].pos);
        vec3s_to_vec3f(vp1, gPaintingMesh[v[1]].pos);
        vec3s_to_vec3f(vp2, gPaintingMesh[v[2]].pos);

        // Cross product to find each triangle's normal vector
        find_vector_perpendicular_to_plane(gPaintingTriNorms[i], vp0, vp1, vp2);
    }
}

/**
 * Rounds a floating-point component of a normal vector to an s8 by multiplying it by 127 or 128 and
 * rounding away from 0.
 */
s32 normalize_component(f32 comp) {
    if (comp > 0.0f) {
        return (s8)((comp * 127.0f) + 0.5f); // round up
    } else if (comp < 0.0f) {
        return (s8)((comp * 128.0f) - 0.5f); // round down
    } else {
        return 0; // don't round 0
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
 *      Where each 'tri' is an index into gPaintingTriNorms.
 *      Entry i in `neighborTris` corresponds to the vertex at gPaintingMesh[i]
 *
 * The table used in game, seg2_painting_mesh_neighbor_tris, is in bin/segment2.c.
 */
void painting_average_vertex_normals(PaintingData *neighborTris, PaintingData numVtx) {
    PaintingData tri;
    PaintingData i, j;
    PaintingData neighbors;
    PaintingData entry = 0;

    for (i = 0; i < numVtx; i++) {
        Vec3f n = { 0.0f, 0.0f, 0.0f };

        // The first number of each entry is the number of adjacent tris
        neighbors = neighborTris[entry];
        for (j = 0; j < neighbors; j++) {
            tri = neighborTris[entry + j + 1];
            vec3f_add(n, gPaintingTriNorms[tri]);
        }
        // Move to the next vertex's entry
        entry += (neighbors + 1);

        // average the surface normals from each neighboring tri
        vec3_div_val(n, neighbors);
        f32 nlen = vec3_sumsq(n);

        if (FLT_IS_NONZERO(nlen)) {
            nlen = sqrtf(nlen);
            gPaintingMesh[i].norm[0] = normalize_component(n[0] / nlen);
            gPaintingMesh[i].norm[1] = normalize_component(n[1] / nlen);
            gPaintingMesh[i].norm[2] = normalize_component(n[2] / nlen);
        } else {
            vec3_zero(gPaintingMesh[i].norm);
        }
    }
}

/**
 * Creates a display list that draws the rippling painting, with 'img' mapped to the painting's mesh,
 * using 'textureMap'.
 *
 * If the textureMap doesn't describe the whole mesh, then multiple calls are needed to draw the whole
 * painting.
 */
Gfx *render_painting(Texture *img, PaintingData tWidth, PaintingData tHeight, PaintingData *textureMap, PaintingData mapVerts, PaintingData mapTris, Alpha alpha) {
    PaintingData group;
    PaintingData map;
    PaintingData triGroup;
    PaintingData mapping;
    PaintingData meshVtx;
    PaintingData tx, ty;

    // We can fit 15 (16 / 3) vertices in the RSP's vertex buffer.
    // Group triangles by 5, with one remainder group.
    PaintingData triGroups = mapTris / 5;
    PaintingData remGroupTris = mapTris % 5;
    PaintingData numVtx = mapTris * 3;

    PaintingData commands = (triGroups * 2) + remGroupTris + 7;
    Vtx *verts = alloc_display_list(numVtx * sizeof(Vtx));
    Gfx *dlist = alloc_display_list(commands * sizeof(Gfx));
    Gfx *gfx = dlist;

    gLoadBlockTexture(gfx++, tWidth, tHeight, G_IM_FMT_RGBA, img);

    // Draw the groups of 5 first
    for (group = 0; group < triGroups; group++) {
        // The triangle groups are the second part of the texture map.
        // Each group is a list of 15 mappings
        triGroup = (mapVerts * 3) + (group * 15) + 2;

        for (map = 0; map < 15; map++) {
            // The mapping is just an index into the earlier part of the textureMap
            // Some mappings are repeated, for example, when multiple triangles share a vertex
            mapping = textureMap[triGroup + map];

            // The first entry is the ID of the vertex in the mesh
            meshVtx = textureMap[(mapping * 3) + 1];

            // The next two are the texture coordinates for that vertex
            tx = textureMap[(mapping * 3) + 2];
            ty = textureMap[(mapping * 3) + 3];

            // Map the texture and place it in the verts array
            make_vertex(verts, ((group * 15) + map), gPaintingMesh[meshVtx].pos[0],
                                                     gPaintingMesh[meshVtx].pos[1],
                                                     gPaintingMesh[meshVtx].pos[2],
                                                     tx, ty,
                                                     gPaintingMesh[meshVtx].norm[0],
                                                     gPaintingMesh[meshVtx].norm[1],
                                                     gPaintingMesh[meshVtx].norm[2],
                                                     alpha);
        }

        // Load the vertices and draw the 5 triangles
        gSPVertex(gfx++, VIRTUAL_TO_PHYSICAL(verts + (group * 15)), 15, 0);
        gSPDisplayList(gfx++, dl_paintings_draw_ripples);
    }

    // One group left with < 5 triangles
    triGroup = (mapVerts * 3) + (triGroups * 15) + 2;

    // Map the texture to the triangles
    for (map = 0; map < (remGroupTris * 3); map++) {
        mapping = textureMap[triGroup + map];
        meshVtx = textureMap[(mapping * 3) + 1];
        tx = textureMap[(mapping * 3) + 2];
        ty = textureMap[(mapping * 3) + 3];
        make_vertex(verts, ((triGroups * 15) + map), gPaintingMesh[meshVtx].pos[0],
                                                     gPaintingMesh[meshVtx].pos[1],
                                                     gPaintingMesh[meshVtx].pos[2],
                                                     tx, ty,
                                                     gPaintingMesh[meshVtx].norm[0],
                                                     gPaintingMesh[meshVtx].norm[1],
                                                     gPaintingMesh[meshVtx].norm[2],
                                                     alpha);
    }

    // Draw the triangles individually
    gSPVertex(gfx++, VIRTUAL_TO_PHYSICAL(verts + (triGroups * 15)), (remGroupTris * 3), 0);

    for (group = 0; group < remGroupTris; group++) {
        gSP1Triangle(gfx++, ((group * 3) + 0),
                            ((group * 3) + 1),
                            ((group * 3) + 2), 0);
    }

    gSPEndDisplayList(gfx);

    return dlist;
}

/**
 * Orient the painting mesh for rendering.
 */
Gfx *painting_model_view_transform(struct Painting *painting) {
    Gfx *dlist = alloc_display_list(2 * sizeof(Gfx));
    Gfx *gfx = dlist;

    // Scale
    Mtx *scale = alloc_display_list(sizeof(Mtx));
    guScale(scale, (painting->sizeX / PAINTING_SIZE), (painting->sizeY / PAINTING_SIZE), 1.0f);
    gSPMatrix(gfx++, scale, (G_MTX_MODELVIEW | G_MTX_MUL | G_MTX_PUSH));

    gSPEndDisplayList(gfx);

    return dlist;
}

/**
 * Ripple a painting that has 1 or more images that need to be mapped
 */
Gfx *painting_ripple_image(struct Painting *painting) {
    s16 i;
    PaintingData meshVerts;
    PaintingData meshTris;
    PaintingData *textureMap;
    PaintingData imageCount = painting->imageCount;
    PaintingData tWidth = painting->textureWidth;
    PaintingData tHeight = painting->textureHeight;
    PaintingData **textureMaps = segmented_to_virtual(painting->textureMaps);
    Texture **textures = segmented_to_virtual(painting->textureArray);
    Gfx *dlist = alloc_display_list((imageCount + 6) * sizeof(Gfx));
    Gfx *gfx = dlist;

    if (dlist == NULL) {
        return dlist;
    }

    gSPDisplayList(gfx++, painting_model_view_transform(painting));
    gSPDisplayList(gfx++, dl_paintings_rippling_begin);
    gSPDisplayList(gfx++, painting->rippleDisplayList);

    // Map each image to the mesh's vertices
    for (i = 0; i < imageCount; i++) {
        textureMap = segmented_to_virtual(textureMaps[i]);
        meshVerts = textureMap[0];
        meshTris = textureMap[(meshVerts * 3) + 1];
        gSPDisplayList(gfx++, render_painting(textures[i], tWidth, tHeight, textureMap, meshVerts, meshTris, painting->alpha));
    }

    // Update the ripple, may automatically reset the painting's state.
    painting_update_ripple_state(painting);

    gSPPopMatrix(gfx++, G_MTX_MODELVIEW);
    gSPDisplayList(gfx++, dl_paintings_rippling_end);
    gSPEndDisplayList(gfx);

    return dlist;
}

/**
 * Ripple a painting that has 1 "environment map" texture.
 */
Gfx *painting_ripple_env_mapped(struct Painting *painting) {
    PaintingData tWidth = painting->textureWidth;
    PaintingData tHeight = painting->textureHeight;
    PaintingData **textureMaps = segmented_to_virtual(painting->textureMaps);
    Texture **tArray = segmented_to_virtual(painting->textureArray);
    Gfx *dlist = alloc_display_list(7 * sizeof(Gfx));
    Gfx *gfx = dlist;

    if (dlist == NULL) {
        return dlist;
    }

    gSPDisplayList(gfx++, painting_model_view_transform(painting));
    gSPDisplayList(gfx++, dl_paintings_env_mapped_begin);
    gSPDisplayList(gfx++, painting->rippleDisplayList);

    // Map the image to the mesh's vertices
    PaintingData *textureMap = segmented_to_virtual(textureMaps[0]);
    PaintingData meshVerts = textureMap[0];
    PaintingData meshTris = textureMap[(meshVerts * 3) + 1];
    gSPDisplayList(gfx++, render_painting(tArray[0], tWidth, tHeight, textureMap, meshVerts, meshTris, painting->alpha));

    // Update the ripple, may automatically reset the painting's state.
    painting_update_ripple_state(painting);

    gSPPopMatrix(gfx++, G_MTX_MODELVIEW);
    gSPDisplayList(gfx++, dl_paintings_env_mapped_end);
    gSPEndDisplayList(gfx);

    return dlist;
}

/**
 * Generates a mesh, calculates vertex normals for lighting, and renders a rippling painting.
 * The mesh and vertex normals are regenerated and freed every frame.
 */
Gfx *display_painting_rippling(struct Painting *painting) {
    PaintingData *mesh = segmented_to_virtual(seg2_painting_triangle_mesh);
    PaintingData *neighborTris = segmented_to_virtual(seg2_painting_mesh_neighbor_tris);
    PaintingData numVtx = mesh[0];
    PaintingData numTris = mesh[(numVtx * 3) + 1];
    Gfx *dlist = NULL;

    // Generate the mesh and its lighting data
    painting_generate_mesh(painting, mesh, numVtx);
    painting_calculate_triangle_normals(mesh, numVtx, numTris);
    painting_average_vertex_normals(neighborTris, numVtx);

    // Map the painting's texture depending on the painting's texture type.
    switch (painting->textureType) {
        case PAINTING_IMAGE:
            dlist = painting_ripple_image(painting);
            break;
        case PAINTING_ENV_MAP:
            dlist = painting_ripple_env_mapped(painting);
            break;
    }

    // The mesh data is freed every frame.
    mem_pool_free(gEffectsMemoryPool, gPaintingMesh);
    mem_pool_free(gEffectsMemoryPool, gPaintingTriNorms);

    return dlist;
}

/**
 * Render a normal painting.
 */
Gfx *display_painting_not_rippling(struct Painting *painting) {
    Gfx *dlist = alloc_display_list(4 * sizeof(Gfx));
    Gfx *gfx = dlist;

    if (dlist == NULL) {
        return dlist;
    }

    gSPDisplayList(gfx++, painting_model_view_transform(painting));
    gSPDisplayList(gfx++, painting->normalDisplayList);
    gSPPopMatrix(gfx++, G_MTX_MODELVIEW);
    gSPEndDisplayList(gfx);

    return dlist;
}

/**
 * Clear Mario-related state and clear gRipplingPainting.
 */
void reset_painting(struct Object *obj) {
    obj->oPaintingCurrFlags = RIPPLE_FLAGS_NONE;
    obj->oPaintingChangedFlags = RIPPLE_FLAGS_NONE;

    gRipplingPainting = NULL;

#ifdef NO_SEGMENTED_MEMORY
    // Make sure all variables are reset correctly.
    // With segmented memory the segments that contain the relevant
    // Painting structs are reloaded from ROM upon level load.
    obj->oPaintingState = PAINTING_IDLE;
    obj->oPaintingCurrRippleMag = 0.0f;
    obj->oPaintingRippleDecay = 1.0f;
    obj->oPaintingCurrRippleRate = 0.0f;
    obj->oPaintingDispersionFactor = 0.0f;
    obj->oPaintingRippleTimer = 0;
    obj->oPaintingRippleX = 0;
    obj->oPaintingRippleY = 0;
    if (obj->oPaintingPtr == &ddd_painting) {
        // Move DDD painting to initial position, in case the animation
        // that moves the painting stops during level unload.
        obj->oPosX = 3456.0f;
    }
#endif
}

/**
 * Controls the x coordinate of the DDD painting.
 *
 * Before Mario gets the "Board Bowser's Sub" star in DDD, the painting spawns at frontPos.
 *
 * If Mario just got the star, the painting's x coordinate moves to backPos at a rate of `speed` units.
 *
 * When the painting reaches backPos, a save flag is set so that the painting will spawn at backPos
 * whenever it loads.
 *
 * This function also sets gDddPaintingStatus, which controls the warp:
 *  0 (0b00): set x coordinate to frontPos
 *  2 (0b10): set x coordinate to backPos
 *  3 (0b11): same as 2. Bit 0 is ignored
 */
void move_ddd_painting(struct Object *obj, f32 frontPos, f32 backPos, f32 speed) {
#ifdef UNLOCK_ALL
    obj->oPosX = backPos;
    gDddPaintingStatus = (DDD_FLAG_BOWSERS_SUB_BEATEN | DDD_FLAG_BACK);
    return;
#endif
    // Obtain the DDD star flags and find out whether Board Bowser's Sub was collected
    u32 bowsersSubBeaten = (save_file_get_star_flags((gCurrSaveFileNum - 1), COURSE_NUM_TO_INDEX(COURSE_DDD)) & STAR_FLAG_ACT_1);
    // Get the other save file flags and check whether DDD has already moved back
    u32 dddBack = (save_file_get_flags() & SAVE_FLAG_DDD_MOVED_BACK);

    if (!bowsersSubBeaten && !dddBack) {
        // If we haven't collected the star or moved the painting, put the painting at the front
        obj->oPosX = frontPos;
        gDddPaintingStatus = DDD_FLAGS_NONE;
    } else if (bowsersSubBeaten && !dddBack) {
        // If we've collected the star but not moved the painting back,
        // Each frame, move the painting by a certain speed towards the back area.
        obj->oPosX += speed;
        gDddPaintingStatus = DDD_FLAG_BOWSERS_SUB_BEATEN;
        if (obj->oPosX >= backPos) {
            obj->oPosX = backPos;
            // Tell the save file that we've moved DDD back.
            save_file_set_flags(SAVE_FLAG_DDD_MOVED_BACK);
        }
    } else if (bowsersSubBeaten && dddBack) {
        // If the painting has already moved back, place it in the back position.
        obj->oPosX = backPos;
        gDddPaintingStatus = (DDD_FLAG_BOWSERS_SUB_BEATEN | DDD_FLAG_BACK);
    }
}

/**
 * Render and update the painting whose id and group matches the values in the GraphNode's parameter.
 * Use PAINTING_ID(id, group) to set the right parameter in a level's geo layout.
 */
Gfx *geo_painting_draw(s32 callContext, struct GraphNode *node, UNUSED void *context) {
    struct GraphNodeGenerated *gen = (struct GraphNodeGenerated *) node;
    struct Object *obj = gCurGraphNodeObjectNode;

    if (obj == NULL) {
        return NULL;
    }

    // Failsafe for nonexistent painting groups.
    if (obj->oPaintingGroup >= PAINTING_NUM_GROUPS) {
        return NULL;
    }

    Gfx *paintingDlist = NULL;
    struct Painting *painting = obj->oPaintingPtr;

    if (painting == NULL) {
        return NULL;
    }

    if (callContext != GEO_CONTEXT_RENDER) {
        // Reset the update counter
        obj->oLastPaintingUpdateCounter = (gAreaUpdateCounter - 1);
        obj->oPaintingUpdateCounter = gAreaUpdateCounter;

        reset_painting(obj);
    } else if (callContext == GEO_CONTEXT_RENDER) {
        // Reset the update counter
        obj->oLastPaintingUpdateCounter = obj->oPaintingUpdateCounter;
        obj->oPaintingUpdateCounter = gAreaUpdateCounter;

#if defined(ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS) || defined(UNLOCK_ALL)
        // Update the ddd painting before drawing
        if (obj->oPaintingGroup == PAINTING_GROUP_INSIDE_CASTLE && obj->oPaintingId == PAINTING_ID_CASTLE_DDD) {
            move_ddd_painting(obj, 3456.0f, 5529.6f, 20.0f);
        }
#endif

        // Update the painting
        painting_update_floors(obj);

        // Draw the painting
        if (painting->imageCount > 0
         && painting->normalDisplayList != NULL
         && painting->textureMaps != NULL
         && painting->textureArray != NULL
         && painting->textureWidth > 0
         && painting->textureHeight > 0
         && painting->alpha > 0) {
            // Determine if the painting is transparent
            if (painting->alpha == 0xFF) { // Opaque
                SET_GRAPH_NODE_LAYER(gen->fnNode.node.flags, LAYER_OCCLUDE_SILHOUETTE_OPAQUE);
            } else {
                SET_GRAPH_NODE_LAYER(gen->fnNode.node.flags, LAYER_TRANSPARENT);
            }

            if (obj->oPaintingState == PAINTING_IDLE) {
                paintingDlist = display_painting_not_rippling(painting);
            } else {
                paintingDlist = display_painting_rippling(painting);
            }
        }

        if (painting->rippleTrigger == RIPPLE_TRIGGER_PROXIMITY) {
            if (obj->oPaintingChangedFlags & RIPPLE_FLAG_ENTER) {
                // Proximity painting enter ripple
                painting_state(obj, PAINTING_ENTERED, FALSE, TRUE);
            } else if (obj->oPaintingState != PAINTING_ENTERED && (obj->oPaintingChangedFlags & RIPPLE_FLAG_RIPPLE)) {
                // Proximity painting wobble ripple
                painting_state(obj, PAINTING_RIPPLE, FALSE, TRUE);
            }
        } else if (painting->rippleTrigger == RIPPLE_TRIGGER_CONTINUOUS) {
            if (obj->oPaintingChangedFlags & RIPPLE_FLAG_ENTER) {
                // Continuous painting enter ripple
                painting_state(obj, PAINTING_ENTERED, FALSE, FALSE);
            } else if (obj->oPaintingState == PAINTING_IDLE) {
                // Continuous painting idle ripple
                painting_state(obj, PAINTING_RIPPLE, TRUE, TRUE);
            }
        }

        if (obj->oPaintingCurrFlags & RIPPLE_FLAG_ENTER) {
            // Mario has entered the painting.
            gEnteredPainting = obj;
        } else if (gEnteredPainting == obj) {
            // Reset gEnteredPainting if it's this painting and this painting is not entered.
            gEnteredPainting = NULL;
        }
    }

    return paintingDlist;
}

void bhv_painting_init(void) {
    // Get the painting group and id from the behavior params.
    o->oPaintingGroup = GET_BPARAM1(o->oBehParams);
    o->oPaintingId    = GET_BPARAM2(o->oBehParams);

    // Failsafe for nonexistent painting groups.
    if (o->oPaintingGroup >= PAINTING_NUM_GROUPS) {
        return;
    }

    struct Painting **paintingGroup = sPaintingGroups[o->oPaintingGroup];
    o->oPaintingPtr = segmented_to_virtual(paintingGroup[o->oPaintingId]);

    Vec3f roomFloorCheckPos;

    // The center of the painting with a z offset since paintings are usually between floor triangles.
    Vec3f distPos = {
        (o->oPaintingPtr->sizeX * 0.5f),
        (o->oPaintingPtr->sizeY * 0.5f),
        PAINTING_WOBBLE_DEPTH
    };

    // Get the painting object's rotation.
    Vec3s rotation;
    vec3i_to_vec3s(rotation, &o->oFaceAngleVec);

    // Set 'roomFloorCheckPos' to world space coords.
    vec3f_local_pos_to_world_pos(roomFloorCheckPos, distPos, &o->oPosVec, rotation);

    // Set the object's room so that paintings only render in their room.
    o->oRoom = get_room_at_pos(roomFloorCheckPos[0],
                               roomFloorCheckPos[1],
                               roomFloorCheckPos[2]);
}

void bhv_painting_loop(void) {
}
