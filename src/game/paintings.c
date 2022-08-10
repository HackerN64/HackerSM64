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
 * HMC painting group.
 */
const struct Painting *sHmcPaintings[] = {
    /*Painting ID                   */
    /*PAINTING_ID_HMC_COTMC         */ &cotmc_painting,
    NULL,
};

/**
 * Inside Castle painting group.
 */
const struct Painting *sInsideCastlePaintings[] = {
    /*Painting ID                   */
    /*PAINTING_ID_CASTLE_BOB        */ &bob_painting,
    /*PAINTING_ID_CASTLE_CCM        */ &ccm_painting,
    /*PAINTING_ID_CASTLE_WF         */ &wf_painting,
    /*PAINTING_ID_CASTLE_JRB        */ &jrb_painting,
    /*PAINTING_ID_CASTLE_LLL        */ &lll_painting,
    /*PAINTING_ID_CASTLE_SSL        */ &ssl_painting,
    /*PAINTING_ID_CASTLE_HMC        */ &hmc_painting,
    /*PAINTING_ID_CASTLE_DDD        */ &ddd_painting,
    /*PAINTING_ID_CASTLE_WDW        */ &wdw_painting,
    /*PAINTING_ID_CASTLE_THI_TINY   */ &thi_tiny_painting,
    /*PAINTING_ID_CASTLE_TTM        */ &ttm_painting,
    /*PAINTING_ID_CASTLE_TTC        */ &ttc_painting,
    /*PAINTING_ID_CASTLE_SL         */ &sl_painting,
    /*PAINTING_ID_CASTLE_THI_HUGE   */ &thi_huge_painting,
    /*PAINTING_ID_CASTLE_RR         */ &rr_painting,
    NULL,
};

/**
 * TTM painting group.
 */
const struct Painting *sTtmPaintings[] = {
    /*Painting ID                   */
    /*PAINTING_ID_TTM_SLIDE         */ &ttm_slide_painting,
    NULL,
};

/**
 * Array of all painting groups.
 */
const struct Painting * const* sPaintingGroups[] = {
    /*Group ID                      */
    /*PAINTING_GROUP_HMC            */ sHmcPaintings,
    /*PAINTING_GROUP_INSIDE_CASTLE  */ sInsideCastlePaintings,
    /*PAINTING_GROUP_TTM            */ sTtmPaintings,
};

/**
 * The painting that is currently rippling. Only one painting can be rippling at once.
 */
struct Object *gRipplingPainting = NULL;

/**
 * The id of the painting Mario has entered.
 */
struct Object *gEnteredPainting = NULL;

/**
 * When a painting is rippling, this mesh is generated each frame using the Painting's parameters.
 *
 * This mesh only contains the vertex positions and normals.
 * Paintings use an additional array to map textures to the mesh.
 */
static struct PaintingMeshVertex *sPaintingMesh;

/**
 * The painting's surface normals, used to approximate each of the vertex normals (for gouraud shading).
 */
static Vec3f *sPaintingTriNorms;

/**
 * Info for ripple animations.
 */
static const struct RippleAnimationInfo sRippleAnimationInfo[] = {
    { // RIPPLE_ANIM_CONTINUOUS
        /*                      passive     entry */
        /* Ripple Magnitude */    10.0f,    30.0f,
        /* Ripple Decay */         1.0f,    0.98f,
        /* Ripple Rate */         0.05f,    0.05f,
        /* Ripple Dispersion */   15.0f,    15.0f,
    },
    { // RIPPLE_ANIM_PROXIMITY
        /*                      passive     entry */
        /* Ripple Magnitude */    20.0f,    80.0f,
        /* Ripple Decay */      0.9608f,  0.9524f,
        /* Ripple Rate */         0.24f,    0.14f,
        /* Ripple Dispersion */   40.0f,    30.0f,
    },
    { // RIPPLE_ANIM_PROXIMITY_LARGE
        /*                      passive     entry */
        /* Ripple Magnitude */    40.0f,   160.0f,
        /* Ripple Decay */      0.9608f,  0.9524f,
        /* Ripple Rate */         0.12f,    0.07f,
        /* Ripple Dispersion */   80.0f,    60.0f,
    }
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
    const struct Painting *painting = obj->oPaintingPtr;
    const struct RippleAnimationInfo *anim = &sRippleAnimationInfo[painting->rippleAnimationType];

    // Use a different set of variables depending on the state
    switch (state) {
        case PAINTING_RIPPLE:
            obj->oPaintingCurrRippleMag    = anim->passiveRippleMag;
            obj->oPaintingRippleDecay      = anim->passiveRippleDecay;
            obj->oPaintingCurrRippleRate   = anim->passiveRippleRate;
            obj->oPaintingDispersionFactor = anim->passiveDispersionFactor;
            break;

        case PAINTING_ENTERED:
            obj->oPaintingCurrRippleMag    = anim->entryRippleMag;
            obj->oPaintingRippleDecay      = anim->entryRippleDecay;
            obj->oPaintingCurrRippleRate   = anim->entryRippleRate;
            obj->oPaintingDispersionFactor = anim->entryDispersionFactor;
            break;
    }

    obj->oPaintingState = state;

    // Set the ripple position.
    if (centerRipples) {
        obj->oPaintingRippleX = (painting->sizeX * 0.5f);
        obj->oPaintingRippleY = (painting->sizeY * 0.5f);
    } else {
        obj->oPaintingRippleX = obj->oPaintingLocalMarioPosX;
        obj->oPaintingRippleY = obj->oPaintingLocalMarioPosY;
    }

    // Set Mario's Y position for the WDW water level.
    gPaintingMarioYEntry = gMarioObject->oPosY;

    if (resetTimer) {
        obj->oPaintingRippleTimer = 0;
    }

    gRipplingPainting = obj;
}

/**
 * Check for Mario entering the painting.
 */
void painting_update_mario_pos(struct Object *obj) {
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

    s8 lastFlags = obj->oPaintingCurrFlags;

    // At most 1 of these will be nonzero.
    obj->oPaintingCurrFlags = rippleFlags;

    // changedFlags is true if currFlags is true and lastFlags is false
    // (Mario just entered the floor on this frame).
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
void painting_update_ripple_state(const struct Painting *painting) {
    struct Object *obj = gCurGraphNodeObjectNode;
    const struct RippleAnimationInfo *anim = &sRippleAnimationInfo[painting->rippleAnimationType];

    if (obj->oPaintingUpdateCounter != obj->oLastPaintingUpdateCounter) {
        obj->oPaintingCurrRippleMag *= obj->oPaintingRippleDecay;

        // Reset the timer to 0 if it overflows.
        if (obj->oPaintingRippleTimer < 0) {
            obj->oPaintingRippleTimer = 0;
        }

        obj->oPaintingRippleTimer++;
    }
    if (painting->rippleTrigger == RIPPLE_TRIGGER_PROXIMITY) {
        // If the painting is barely rippling, make it stop rippling.
        if (obj->oPaintingCurrRippleMag <= 1.0f) {
            obj->oPaintingState = PAINTING_IDLE;
            gRipplingPainting = NULL;
        }
    } else if (painting->rippleTrigger == RIPPLE_TRIGGER_CONTINUOUS) {
        // If the painting is doing the entry ripple but the ripples are as small as those from the
        // passive ripple, make it do a passive ripple.
        // If Mario goes below the surface but doesn't warp, the painting will eventually reset.
        if ((obj->oPaintingState == PAINTING_ENTERED)
         && (obj->oPaintingCurrRippleMag <= anim->passiveRippleMag)) {
            obj->oPaintingState = PAINTING_RIPPLE;
            obj->oPaintingCurrRippleMag    = anim->passiveRippleMag;
            obj->oPaintingRippleDecay      = anim->passiveRippleDecay;
            obj->oPaintingCurrRippleRate   = anim->passiveRippleRate;
            obj->oPaintingDispersionFactor = anim->passiveDispersionFactor;
        }
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
void painting_generate_mesh(const struct Painting *painting, const PaintingData *mesh, PaintingData numTris) {
    struct Object *obj = gCurGraphNodeObjectNode;
    PaintingData i, tri;

    sPaintingMesh = mem_pool_alloc(gEffectsMemoryPool, (numTris * sizeof(struct PaintingMeshVertex)));

    struct PaintingMeshVertex *paintingMesh = sPaintingMesh;

    /// Controls the peaks of the ripple.
    f32 rippleMag = obj->oPaintingCurrRippleMag;
    /// Controls the ripple's frequency.
    f32 rippleRate = obj->oPaintingCurrRippleRate;
    /// Controls how fast the ripple spreads.
    f32 dispersionFactor = obj->oPaintingDispersionFactor;
    /// How far the ripple has spread.
    f32 rippleTimer = obj->oPaintingRippleTimer;

    /// X and Y of the ripple origin.
    f32 rippleX = obj->oPaintingRippleX;
    f32 rippleY = obj->oPaintingRippleY;

    f32 sizeRatioX = (painting->sizeX / PAINTING_SIZE);
    f32 sizeRatioY = (painting->sizeY / PAINTING_SIZE);

    f32 dx, dy;
    f32 rippleDistance;

    // Loop through all the painting vertices and calculate the ripple magnitude at each point.
    // Accesses are off by 1 since the first entry is the number of vertices.
    for (i = 0; i < numTris; i++) {
        tri = (i * 3);
        paintingMesh->pos[0] = mesh[tri + 1];
        paintingMesh->pos[1] = mesh[tri + 2];
        // The "Z coordinate" of each vertex in the mesh is either 1 or 0. Instead of being an
        // actual coordinate, it just determines whether the vertex moves.
        if (mesh[tri + 3]) {
            // Scale and calculate the distance to the ripple origin.
            dx = ((paintingMesh->pos[0] * sizeRatioX) - rippleX);
            dy = ((paintingMesh->pos[1] * sizeRatioY) - rippleY);
            // A larger dispersionFactor makes the ripple spread slower.
            rippleDistance = sqrtf(sqr(dx) + sqr(dy)) / dispersionFactor;

            if (rippleTimer < rippleDistance) {
                // If the ripple hasn't reached the point yet, make the point magnitude 0.
                paintingMesh->pos[2] = 0;
            } else {
                // Use a cosine wave to make the ripple go up and down,
                // scaled by the painting's ripple magnitude,
                // round it to an int and return it.
                paintingMesh->pos[2] = roundf(rippleMag * coss((s16)((rippleRate * (rippleTimer - rippleDistance)) * 0x10000)));
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
void painting_calculate_triangle_normals(const PaintingData *mesh, PaintingData numVtx, PaintingData numTris) {
    PaintingData i;
    Vec3s v;
    Vec3f vp0, vp1, vp2;

    sPaintingTriNorms = mem_pool_alloc(gEffectsMemoryPool, (numTris * sizeof(Vec3f)));

    for (i = 0; i < numTris; i++) {
        // Add 2 because of the 2 length entries preceding the list.
        PaintingData tri = ((numVtx * 3) + (i * 3) + 2);
        vec3s_copy(v, &mesh[tri]);
        vec3s_to_vec3f(vp0, sPaintingMesh[v[0]].pos);
        vec3s_to_vec3f(vp1, sPaintingMesh[v[1]].pos);
        vec3s_to_vec3f(vp2, sPaintingMesh[v[2]].pos);

        // Cross product to find each triangle's normal vector.
        find_vector_perpendicular_to_plane(sPaintingTriNorms[i], vp0, vp1, vp2);
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
void painting_average_vertex_normals(const PaintingData *neighborTris, PaintingData numVtx) {
    PaintingData tri;
    PaintingData i, j;
    PaintingData neighbors;
    PaintingData entry = 0;

    for (i = 0; i < numVtx; i++) {
        Vec3f n = { 0.0f, 0.0f, 0.0f };

        // The first number of each entry is the number of adjacent tris.
        neighbors = neighborTris[entry];
        for (j = 0; j < neighbors; j++) {
            tri = neighborTris[entry + j + 1];
            vec3f_add(n, sPaintingTriNorms[tri]);
        }

        // Move to the next vertex's entry
        entry += (neighbors + 1);

        // Average the surface normals from each neighboring tri.
        vec3_div_val(n, neighbors);
        f32 nlen = vec3_sumsq(n);

        if (FLT_IS_NONZERO(nlen)) {
            nlen = (127.0f / sqrtf(nlen));
            vec3_prod_val(sPaintingMesh[i].norm, n, nlen);
        } else {
            vec3_zero(sPaintingMesh[i].norm);
        }
    }
}

#ifdef F3DEX_GBI_2
    #define VTX_BUF_MAX 32
#else
    #define VTX_BUF_MAX 16
#endif
#define TRI_PER_DL (VTX_BUF_MAX / 3) //  5 or 10
#define VTX_PER_DL (TRI_PER_DL  * 3) // 15 or 30

/**
 * Creates a display list that draws the rippling painting, with 'img' mapped to the painting's mesh,
 * using 'textureMap'.
 *
 * If the textureMap doesn't describe the whole mesh, then multiple calls are needed to draw the whole
 * painting.
 */
Gfx *render_painting(const Texture *img, PaintingData tWidth, PaintingData tHeight, const PaintingData *textureMap, PaintingData mapVerts, PaintingData mapTris, Alpha alpha) {
    struct PaintingMeshVertex *mesh = NULL;
    PaintingData group;
    PaintingData groupIndex;
    PaintingData map;
    PaintingData triGroup;
    PaintingData mapping;
    PaintingData meshVtx;
    PaintingData tx, ty;

    // We can fit VTX_PER_DL vertices in the RSP's vertex buffer.
    // Group triangles by TRI_PER_DL, with one remainder group.
    PaintingData triGroups    = (mapTris / TRI_PER_DL);
    PaintingData remGroupTris = (mapTris % TRI_PER_DL);
    PaintingData numVtx       = (mapTris * 3);

    Vtx *verts = alloc_display_list(numVtx * sizeof(Vtx));
    u32 commands = (
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
    Gfx *dlist = alloc_display_list(commands * sizeof(Gfx));
    Gfx *gfx = dlist;

    gLoadBlockTexture(gfx++, tWidth, tHeight, G_IM_FMT_RGBA, img);

    // Draw the groups of TRI_PER_DL first.
    for (group = 0; group < triGroups; group++) {
        // The index of the first vertex in the group.
        groupIndex = (group * VTX_PER_DL);

        // The triangle groups are the second part of the texture map.
        // Each group is a list of VTX_PER_DL mappings.
        triGroup = ((mapVerts * 3) + groupIndex + 2);

        // Vertices within the group
        for (map = 0; map < VTX_PER_DL; map++) {
            // The mapping is just an index into the earlier part of the textureMap.
            // Some mappings are repeated, for example, when multiple triangles share a vertex.
            mapping = (textureMap[triGroup + map] * 3);

            // The first entry is the ID of the vertex in the mesh.
            meshVtx = textureMap[mapping + 1];
            // The next two are the texture coordinates for that vertex.
            tx      = textureMap[mapping + 2];
            ty      = textureMap[mapping + 3];

            // Get a pointer to the current mesh.
            mesh = &sPaintingMesh[meshVtx];

            // Map the texture and place it in the verts array.
            make_vertex(verts, (groupIndex + map),
                mesh->pos[0],
                mesh->pos[1],
                mesh->pos[2],
                tx, ty,
                mesh->norm[0],
                mesh->norm[1],
                mesh->norm[2],
                alpha);
        }

        // Load the vertices and draw the TRI_PER_DL triangles.
        gSPVertex(gfx++, VIRTUAL_TO_PHYSICAL(verts + groupIndex), VTX_PER_DL, 0);
        gSPDisplayList(gfx++, dl_paintings_draw_ripples);
    }

    // One group left with < TRI_PER_DL triangles.
    triGroup = ((mapVerts * 3) + (triGroups * VTX_PER_DL) + 2);

    // Map the texture to the triangles.
    for (map = 0; map < (remGroupTris * 3); map++) {
        mapping = (textureMap[triGroup + map] * 3);
        meshVtx = textureMap[mapping + 1];
        tx      = textureMap[mapping + 2];
        ty      = textureMap[mapping + 3];

        mesh = &sPaintingMesh[meshVtx];
        make_vertex(verts, ((triGroups * VTX_PER_DL) + map),
            mesh->pos[0],
            mesh->pos[1],
            mesh->pos[2],
            tx, ty,
            mesh->norm[0],
            mesh->norm[1],
            mesh->norm[2],
            alpha);
    }

    // Draw the remaining triangles individually.
    gSPVertex(gfx++, VIRTUAL_TO_PHYSICAL(verts + (triGroups * VTX_PER_DL)), (remGroupTris * 3), 0);

    for (group = 0; group < (remGroupTris * 3); group += 3) {
        gSP1Triangle(gfx++,
            (group + 0),
            (group + 1),
            (group + 2),
        0x0);
    }

    gSPEndDisplayList(gfx);

    return dlist;
}

#undef VTX_BUF_MAX
#undef TRI_PER_DL
#undef VTX_PER_DL

/**
 * Orient the painting mesh for rendering.
 */
Gfx *painting_model_view_transform(const struct Painting *painting) {
    u32 commands = (
        /*gSPMatrix         */ 1 +
        /*gSPEndDisplayList */ 1
    );
    Gfx *dlist = alloc_display_list(commands * sizeof(Gfx));
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
 * Ripple a painting that has 1 or more images that need to be mapped
 */
Gfx *painting_ripple_image(const struct Painting *painting, const PaintingData **textureMaps) {
    PaintingData i;
    PaintingData meshVerts;
    PaintingData meshTris;
    const PaintingData *textureMap;
    PaintingData imageCount = painting->imageCount;
    PaintingData tWidth = painting->textureWidth;
    PaintingData tHeight = painting->textureHeight;
    const Texture **textures = segmented_to_virtual(painting->textureArray);
    u32 commands = (
        /*gSPDisplayList    */ 1 +
        /*gSPDisplayList    */ 1 +
        (imageCount * (
            /*gSPDisplayList    */ 1
        )) +
        /*gSPPopMatrix      */ 1 +
        /*gSPDisplayList    */ 1 +
        /*gSPEndDisplayList */ 1
    );
    Gfx *dlist = alloc_display_list(commands * sizeof(Gfx));
    Gfx *gfx = dlist;

    if (dlist == NULL) {
        return dlist;
    }

    gSPDisplayList(gfx++, painting_model_view_transform(painting));
    gSPDisplayList(gfx++, dl_paintings_rippling_begin);

    // Map each image to the mesh's vertices.
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
Gfx *painting_ripple_env_mapped(const struct Painting *painting, const PaintingData **textureMaps) {
    PaintingData meshVerts;
    PaintingData meshTris;
    const PaintingData *textureMap;
    PaintingData tWidth = painting->textureWidth;
    PaintingData tHeight = painting->textureHeight;
    const Texture **tArray = segmented_to_virtual(painting->textureArray);
    u32 commands = (
        /*gSPDisplayList    */ 1 +
        /*gSPDisplayList    */ 1 +
        /*gSPDisplayList    */ 1 +
        /*gSPPopMatrix      */ 1 +
        /*gSPDisplayList    */ 1 +
        /*gSPEndDisplayList */ 1
    );
    Gfx *dlist = alloc_display_list(commands * sizeof(Gfx));
    Gfx *gfx = dlist;

    if (dlist == NULL) {
        return dlist;
    }

    gSPDisplayList(gfx++, painting_model_view_transform(painting));
    gSPDisplayList(gfx++, dl_paintings_env_mapped_begin);

    // Map the image to the mesh's vertices.
    textureMap = segmented_to_virtual(textureMaps[0]);
    meshVerts = textureMap[0];
    meshTris = textureMap[(meshVerts * 3) + 1];

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
Gfx *display_painting_rippling(const struct Painting *painting) {
    const PaintingData *mesh = segmented_to_virtual(seg2_painting_triangle_mesh);
    const PaintingData *neighborTris = segmented_to_virtual(seg2_painting_mesh_neighbor_tris);
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
            dlist = painting_ripple_image(painting, segmented_to_virtual(seg2_painting_image_texture_maps));
            break;
        case PAINTING_ENV_MAP:
            dlist = painting_ripple_env_mapped(painting, segmented_to_virtual(seg2_painting_env_map_texture_maps));
            break;
    }

    // The mesh data is freed every frame.
    mem_pool_free(gEffectsMemoryPool, sPaintingMesh);
    mem_pool_free(gEffectsMemoryPool, sPaintingTriNorms);

    return dlist;
}

/**
 * Render a normal painting.
 */
Gfx *display_painting_not_rippling(const struct Painting *painting) {
    u32 commands = (
        /*gSPDisplayList    */ 1 +
        /*gSPDisplayList    */ 1 +
        /*gSPPopMatrix      */ 1 +
        /*gSPEndDisplayList */ 1
    );
    Gfx *dlist = alloc_display_list(commands * sizeof(Gfx));
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
 */
void move_ddd_painting(struct Object *obj, f32 frontPos, f32 backPos, f32 speed) {
#ifdef UNLOCK_ALL
    obj->oPosX = backPos;
    return;
#endif
    // Obtain the DDD star flags and find out whether Board Bowser's Sub was collected.
    u32 bowsersSubBeaten = (save_file_get_star_flags((gCurrSaveFileNum - 1), COURSE_NUM_TO_INDEX(COURSE_DDD)) & STAR_FLAG_ACT_1);
    // Get the other save file flags and check whether DDD has already moved back.
    u32 dddBack = (save_file_get_flags() & SAVE_FLAG_DDD_MOVED_BACK);

    if (!bowsersSubBeaten && !dddBack) {
        // If we haven't collected the star or moved the painting, put the painting at the front.
        obj->oPosX = frontPos;
    } else if (bowsersSubBeaten && !dddBack) {
        // If we've collected the star but not moved the painting back,
        // Each frame, move the painting by a certain speed towards the back area.
        obj->oPosX += speed;
        if (obj->oPosX >= backPos) {
            obj->oPosX = backPos;
            // Tell the save file that we've moved DDD back.
            save_file_set_flags(SAVE_FLAG_DDD_MOVED_BACK);
        }
    } else if (bowsersSubBeaten && dddBack) {
        // If the painting has already moved back, place it in the back position.
        obj->oPosX = backPos;
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
    const struct Painting *painting = obj->oPaintingPtr;

    if (painting == NULL) {
        return NULL;
    }

    if (callContext != GEO_CONTEXT_RENDER) {
        // Reset the update counter.
        obj->oLastPaintingUpdateCounter = (gAreaUpdateCounter - 1);
        obj->oPaintingUpdateCounter = gAreaUpdateCounter;

        reset_painting(obj);
    } else if (callContext == GEO_CONTEXT_RENDER) {
        // Reset the update counter.
        obj->oLastPaintingUpdateCounter = obj->oPaintingUpdateCounter;
        obj->oPaintingUpdateCounter = gAreaUpdateCounter;

#if defined(ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS) || defined(UNLOCK_ALL)
        // Update the ddd painting before drawing.
        if (obj->oPaintingGroup == PAINTING_GROUP_INSIDE_CASTLE
         && obj->oPaintingId == PAINTING_ID_CASTLE_DDD) {
            move_ddd_painting(obj, 3456.0f, 5529.6f, 20.0f);
        }
#endif

        // Update the painting info.
        painting_update_mario_pos(obj);

        // Draw the painting.
        if (painting->imageCount > 0
         && painting->normalDisplayList != NULL
         && painting->textureArray != NULL
         && painting->textureWidth > 0
         && painting->textureHeight > 0
         && painting->alpha > 0) {
            // Determine whether the painting is opaque or transparent.
            gen->fnNode.node.drawingLayer = ((painting->alpha == 0xFF) ? LAYER_OCCLUDE_SILHOUETTE_OPAQUE : LAYER_TRANSPARENT);

            if (obj->oPaintingState == PAINTING_IDLE) {
                paintingDlist = display_painting_not_rippling(painting);
            } else {
                paintingDlist = display_painting_rippling(painting);
            }
        }

        if (painting->rippleTrigger == RIPPLE_TRIGGER_PROXIMITY) {
            if (obj->oPaintingChangedFlags & RIPPLE_FLAG_ENTER) {
                // Proximity painting enter ripple.
                painting_state(obj, PAINTING_ENTERED, FALSE, TRUE);
            } else if (obj->oPaintingState != PAINTING_ENTERED && (obj->oPaintingChangedFlags & RIPPLE_FLAG_RIPPLE)) {
                // Proximity painting wobble ripple.
                painting_state(obj, PAINTING_RIPPLE, FALSE, TRUE);
            }
        } else if (painting->rippleTrigger == RIPPLE_TRIGGER_CONTINUOUS) {
            if (obj->oPaintingChangedFlags & RIPPLE_FLAG_ENTER) {
                // Continuous painting enter ripple.
                painting_state(obj, PAINTING_ENTERED, FALSE, FALSE);
            } else if (obj->oPaintingState == PAINTING_IDLE) {
                // Continuous painting idle ripple.
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
    struct Object *obj = o;

    // Get the painting group and id from the behavior params.
    obj->oPaintingGroup = GET_BPARAM1(obj->oBehParams);
    obj->oPaintingId    = GET_BPARAM2(obj->oBehParams);

    // Failsafe for nonexistent painting groups.
    if (obj->oPaintingGroup >= PAINTING_NUM_GROUPS) {
        return;
    }

    const struct Painting * const* paintingGroup = sPaintingGroups[obj->oPaintingGroup];
    obj->oPaintingPtr = segmented_to_virtual(paintingGroup[obj->oPaintingId]);

    // The center of the painting, but with a z offset since paintings are usually between floor triangle edges laterally.
    Vec3f distPos = {
        (obj->oPaintingPtr->sizeX * 0.5f),
        (obj->oPaintingPtr->sizeY * 0.5f),
        PAINTING_WOBBLE_DEPTH // Distance in front of the painting to check for a room floor.
    };

    // Get the painting object's rotation.
    Vec3s rotation;
    vec3i_to_vec3s(rotation, &obj->oFaceAngleVec);

    Vec3f roomCheckPos;

    // Set 'roomCheckPos' to the world space coords of 'distPos'.
    vec3f_local_pos_to_world_pos(roomCheckPos, distPos, &obj->oPosVec, rotation);

    // Set the object's room so that paintings only render in their room.
    obj->oRoom = get_room_at_pos(roomCheckPos[0],
                                 roomCheckPos[1],
                                 roomCheckPos[2]);
}

void bhv_painting_loop(void) {
}
