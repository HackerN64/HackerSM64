#include <PR/ultratypes.h>

#include "sm64.h"
#include "game/ingame_menu.h"
#include "graph_node.h"
#include "behavior_script.h"
#include "behavior_data.h"
#include "game/memory.h"
#include "game/object_helpers.h"
#include "surface_collision.h"
#include "math_util.h"
#include "game/mario.h"
#include "game/object_list_processor.h"
#include "surface_load.h"
#include "game/puppyprint.h"
#include "game/debug.h"

#include "config.h"

/**
 * Partitions for course and object surfaces. The arrays represent
 * the 16x16 cells that each level is split into.
 */
SpatialPartitionCell gStaticSurfacePartition[NUM_CELLS][NUM_CELLS];
SpatialPartitionCell gDynamicSurfacePartition[NUM_CELLS][NUM_CELLS];
struct CellCoords {
    u8 z;
    u8 x;
    u8 partition;
};
struct CellCoords sCellsUsed[NUM_CELLS];
u16 sNumCellsUsed;
u8 sClearAllCells;

/**
 * Pools of data that can contain either surface nodes or surfaces.
 * The static surface pool is resized to be exactly the amount of memory needed for the level geometry.
 * The dynamic surface pool is set at a fixed length and cleared every frame.
 */
void *gCurrStaticSurfacePool;
void *gDynamicSurfacePool;

/**
 * The end of the data currently allocated to the surface pools.
 */
void *gCurrStaticSurfacePoolEnd;
void *gDynamicSurfacePoolEnd;

/**
 * The amount of data currently allocated to static surfaces.
 */
u32 gTotalStaticSurfaceData;

/**
 * Allocate the part of the surface node pool to contain a surface node.
 */
static struct SurfaceNode *alloc_surface_node(u32 dynamic) {
    struct SurfaceNode **poolEnd = (struct SurfaceNode **)(dynamic ? &gDynamicSurfacePoolEnd : &gCurrStaticSurfacePoolEnd);

    struct SurfaceNode *node = *poolEnd;
    (*poolEnd)++;
    gSurfaceNodesAllocated++;

    node->next = NULL;

    return node;
}

/**
 * Allocate the part of the surface pool to contain a surface and
 * initialize the surface.
 */
static struct Surface *alloc_surface(u32 dynamic) {
    struct Surface **poolEnd = (struct Surface **)(dynamic ? &gDynamicSurfacePoolEnd : &gCurrStaticSurfacePoolEnd);
    
    struct Surface *surface = *poolEnd;
    (*poolEnd)++;
    gSurfacesAllocated++;

    surface->type = SURFACE_DEFAULT;
    surface->force = 0;
    surface->flags = SURFACE_FLAGS_NONE;
    surface->room = 0;
    surface->object = NULL;

    return surface;
}

/**
 * Add a surface to the correct cell list of surfaces.
 * @param dynamic Determines whether the surface is static or dynamic
 * @param cellX The X position of the cell in which the surface resides
 * @param cellZ The Z position of the cell in which the surface resides
 * @param surface The surface to add
 */
static void add_surface_to_cell(s32 dynamic, s32 cellX, s32 cellZ, struct Surface *surface) {
    struct SurfaceNode **list;
    s32 priority;
    s32 sortDir = 1; // highest to lowest, then insertion order (water and floors)
    s32 listIndex;

    if (SURFACE_IS_NEW_WATER(surface->type)) {
        listIndex = SPATIAL_PARTITION_WATER;
    } else if (surface->normal.y > NORMAL_FLOOR_THRESHOLD) {
        listIndex = SPATIAL_PARTITION_FLOORS;
    } else if (surface->normal.y < NORMAL_CEIL_THRESHOLD) {
        listIndex = SPATIAL_PARTITION_CEILS;
        sortDir = -1; // lowest to highest, then insertion order
    } else {
        listIndex = SPATIAL_PARTITION_WALLS;
        sortDir = 0; // insertion order
    }

    s32 surfacePriority = surface->upperY * sortDir;

    struct SurfaceNode *newNode = alloc_surface_node(dynamic);
    newNode->surface = surface;

    if (dynamic) {
        list = &gDynamicSurfacePartition[cellZ][cellX][listIndex];
        if (sNumCellsUsed >= sizeof(sCellsUsed) / sizeof(struct CellCoords)) {
            sClearAllCells = TRUE;
        } else {
            if (*list == NULL) {
                sCellsUsed[sNumCellsUsed].z = cellZ;
                sCellsUsed[sNumCellsUsed].x = cellX;
                sCellsUsed[sNumCellsUsed].partition = listIndex;
                sNumCellsUsed++;
            }
        }
    } else {
        list = &gStaticSurfacePartition[cellZ][cellX][listIndex];
    }

    if (*list == NULL) {
        *list = newNode;
        return;
    }

    struct SurfaceNode *curNode = *list;

    // Check if surface should be placed at the beginning of the list.
    priority = curNode->surface->upperY * sortDir;
    if (surfacePriority > priority) {
        *list = newNode;
        newNode->next = curNode;
        return;
    }

    // Loop until we find the appropriate place for the surface in the list.
    while (curNode->next != NULL) {
        priority = curNode->next->surface->upperY * sortDir;

        if (surfacePriority > priority) {
            break;
        }

        curNode = curNode->next;
    }

    newNode->next = curNode->next;
    curNode->next = newNode;
}

/**
 * Every level is split into CELL_SIZE * CELL_SIZE cells of surfaces (to limit computing
 * time). This function determines the lower cell for a given x/z position.
 * @param coord The coordinate to test
 */
static s32 lower_cell_index(s32 coord) {
    // Move from range [-LEVEL_BOUNDARY_MAX, LEVEL_BOUNDARY_MAX) to [0, 2 * LEVEL_BOUNDARY_MAX)
    coord += LEVEL_BOUNDARY_MAX;
    if (coord < 0) {
        coord = 0;
    }

    // [0, NUM_CELLS)
    s32 index = coord / CELL_SIZE;

    // Potentially > NUM_CELLS - 1, but since the upper index is <= NUM_CELLS - 1, not exploitable
    return MAX(0, index);
}

/**
 * Every level is split into CELL_SIZE * CELL_SIZE cells of surfaces (to limit computing
 * time). This function determines the upper cell for a given x/z position.
 * @param coord The coordinate to test
 */
static s32 upper_cell_index(s32 coord) {
    // Move from range [-LEVEL_BOUNDARY_MAX, LEVEL_BOUNDARY_MAX) to [0, 2 * LEVEL_BOUNDARY_MAX)
    coord += LEVEL_BOUNDARY_MAX;
    if (coord < 0) {
        coord = 0;
    }

    // [0, NUM_CELLS)
    s32 index = coord / CELL_SIZE;

    // Potentially < 0, but since lower index is >= 0, not exploitable
    return MIN((NUM_CELLS - 1), index);
}

/**
 * Every level is split into 16x16 cells, this takes a surface, finds
 * the appropriate cells (with a buffer), and adds the surface to those
 * cells.
 * @param surface The surface to check
 * @param dynamic Boolean determining whether the surface is static or dynamic
 */
static void add_surface(struct Surface *surface, s32 dynamic) {
    s32 cellZ, cellX;
    s32 minX, maxX, minZ, maxZ;

    min_max_3i(surface->vertex1[0], surface->vertex2[0], surface->vertex3[0], &minX, &maxX);
    min_max_3i(surface->vertex1[2], surface->vertex2[2], surface->vertex3[2], &minZ, &maxZ);

    s32 minCellX = lower_cell_index(minX);
    s32 maxCellX = upper_cell_index(maxX);
    s32 minCellZ = lower_cell_index(minZ);
    s32 maxCellZ = upper_cell_index(maxZ);

    for (cellZ = minCellZ; cellZ <= maxCellZ; cellZ++) {
        for (cellX = minCellX; cellX <= maxCellX; cellX++) {
            add_surface_to_cell(dynamic, cellX, cellZ, surface);
        }
    }
}

/**
 * Initializes a Surface struct using the given vertex data
 * @param vertexData The raw data containing vertex positions
 * @param vertexIndices Helper which tells positions in vertexData to start reading vertices
 * @param dynamic If the surface belongs to an object or not
 */
static struct Surface *read_surface_data(TerrainData *vertexData, TerrainData **vertexIndices, u32 dynamic) {
    Vec3t v[3];
    Vec3f n;
    Vec3t offset;
    s16 min, max;

    vec3_scale_dest(offset, (*vertexIndices), 3);

    vec3s_copy(v[0], (vertexData + offset[0]));
    vec3s_copy(v[1], (vertexData + offset[1]));
    vec3s_copy(v[2], (vertexData + offset[2]));

    find_vector_perpendicular_to_plane(n, v[0], v[1], v[2]);

    f32 mag = (sqr(n[0]) + sqr(n[1]) + sqr(n[2]));
    // This will never need to be run for custom levels because Fast64 does this step before exporting.
    // DEBUG_ASSERT((mag >= NEAR_ZERO), "Denorm tri was found.");
#ifdef ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
    if (mag < NEAR_ZERO) {
        return NULL;
    }
#endif
    mag = 1.0f / sqrtf(mag);
    vec3_scale(n, mag);

    struct Surface *surface = alloc_surface(dynamic);

    vec3s_copy(surface->vertex1, v[0]);
    vec3s_copy(surface->vertex2, v[1]);
    vec3s_copy(surface->vertex3, v[2]);

    surface->normal.x = n[0];
    surface->normal.y = n[1];
    surface->normal.z = n[2];

    surface->originOffset = -vec3_dot(n, v[0]);

    min_max_3s(v[0][1], v[1][1], v[2][1], &min, &max);
    surface->lowerY = (min - SURFACE_VERTICAL_BUFFER);
    surface->upperY = (max + SURFACE_VERTICAL_BUFFER);

    return surface;
}

#ifndef ALL_SURFACES_HAVE_FORCE
/**
 * Returns whether a surface has exertion/moves Mario
 * based on the surface type.
 */
static s32 surface_has_force(s32 surfaceType) {
    s32 hasForce = FALSE;

    switch (surfaceType) {
        case SURFACE_0004: // Unused
        case SURFACE_FLOWING_WATER:
        case SURFACE_DEEP_MOVING_QUICKSAND:
        case SURFACE_SHALLOW_MOVING_QUICKSAND:
        case SURFACE_MOVING_QUICKSAND:
        case SURFACE_HORIZONTAL_WIND:
        case SURFACE_INSTANT_MOVING_QUICKSAND:
            hasForce = TRUE;
            break;

        default:
            break;
    }
    return hasForce;
}
#endif

/**
 * Returns whether a surface should have the
 * SURFACE_FLAG_NO_CAM_COLLISION flag.
 */
static s32 surf_has_no_cam_collision(s32 surfaceType) {
    s32 flags = SURFACE_FLAGS_NONE;

    switch (surfaceType) {
        case SURFACE_NO_CAM_COLLISION:
        case SURFACE_NO_CAM_COLLISION_77: // Unused
        case SURFACE_NO_CAM_COL_VERY_SLIPPERY:
        case SURFACE_SWITCH:
            flags = SURFACE_FLAG_NO_CAM_COLLISION;
            break;

        default:
            break;
    }

    return flags;
}

/**
 * Load in the surfaces for a given surface type. This includes setting the flags,
 * exertion, and room.
 */
static void load_static_surfaces(TerrainData **data, TerrainData *vertexData, s32 surfaceType, RoomData **surfaceRooms) {
    s32 i;
    struct Surface *surface;
    RoomData room = 0;
#ifndef ALL_SURFACES_HAVE_FORCE
    s16 hasForce = surface_has_force(surfaceType);
#endif
    s32 flags = surf_has_no_cam_collision(surfaceType);

    s32 numSurfaces = *(*data)++;

    for (i = 0; i < numSurfaces; i++) {
        if (*surfaceRooms != NULL) {
            room = *(*surfaceRooms)++;
        }

        surface = read_surface_data(vertexData, data, FALSE);
        if (surface != NULL) {
            surface->room = room;
            surface->type = surfaceType;
            surface->flags = flags;

#ifdef ALL_SURFACES_HAVE_FORCE
            surface->force = *(*data + 3);
#else
            if (hasForce) {
                surface->force = *(*data + 3);
            } else {
                surface->force = 0;
            }
#endif

            add_surface(surface, FALSE);
        }

#ifdef ALL_SURFACES_HAVE_FORCE
        *data += 4;
#else
        *data += 3;
        if (hasForce) {
            (*data)++;
        }
#endif
    }
}

/**
 * Read the data for vertices for reference by triangles.
 */
static TerrainData *read_vertex_data(TerrainData **data) {
    s32 numVertices = *(*data)++;

    TerrainData *vertexData = *data;
    *data += 3 * numVertices;

    return vertexData;
}

/**
 * Loads in special environmental regions, such as water, poison gas, and JRB fog.
 */
static void load_environmental_regions(TerrainData **data) {
    s32 i;

    gEnvironmentRegions = *data;
    s32 numRegions = *(*data)++;

    for (i = 0; i < numRegions; i++) {
        *data += 5;
        gEnvironmentLevels[i] = *(*data)++;
    }
}

/**
 * Allocate the dynamic surface pool for object collision.
 */
void alloc_surface_pools(void) {
    gDynamicSurfacePool = main_pool_alloc(DYNAMIC_SURFACE_POOL_SIZE, MEMORY_POOL_LEFT);
    gDynamicSurfacePoolEnd = gDynamicSurfacePool;

    gCCMEnteredSlide = FALSE;
    reset_red_coins_collected();
}

#ifdef NO_SEGMENTED_MEMORY
/**
 * Get the size of the terrain data, to get the correct size when copying later.
 */
u32 get_area_terrain_size(TerrainData *data) {
    TerrainData *startPos = data;
    s32 end = FALSE;
    TerrainData terrainLoadType;
    s32 numVertices;
    s32 numRegions;
    s32 numSurfaces;
#ifndef ALL_SURFACES_HAVE_FORCE
    TerrainData hasForce;
#endif

    while (!end) {
        terrainLoadType = *data++;

        switch (terrainLoadType) {
            case TERRAIN_LOAD_VERTICES:
                numVertices = *data++;
                data += 3 * numVertices;
                break;

            case TERRAIN_LOAD_ENVIRONMENT:
                numRegions = *data++;
                data += 6 * numRegions;
                break;

            case TERRAIN_LOAD_CONTINUE:
                continue;

            case TERRAIN_LOAD_END:
                end = TRUE;
                break;

            default:
                numSurfaces = *data++;
#ifdef ALL_SURFACES_HAVE_FORCE
                data += 4 * numSurfaces;
#else
                hasForce = surface_has_force(terrainLoadType);
                data += (3 + hasForce) * numSurfaces;
#endif
                break;
        }
    }

    return data - startPos;
}
#endif


/**
 * Process the level file, loading in vertices, surfaces, some objects, and environmental
 * boxes (water, gas, JRB fog).
 */
void load_area_terrain(TerrainData *data, RoomData *surfaceRooms) {
    PUPPYPRINT_GET_SNAPSHOT();
    s32 terrainLoadType;
    TerrainData *vertexData = NULL;
    u32 surfacePoolData;

    // Initialize the data for this.
    gEnvironmentRegions = NULL;
    gSurfaceNodesAllocated = 0;
    gSurfacesAllocated = 0;
    bzero(&sCellsUsed, sizeof(sCellsUsed));
    sNumCellsUsed = 0;
    sClearAllCells = TRUE;

    // Clear the static (level) surface partitions for new use.
    bzero(gStaticSurfacePartition, sizeof(gStaticSurfacePartition));
    gTotalStaticSurfaceData = 0;

    // Initialise a new surface pool for this block of static surface data
    gCurrStaticSurfacePool = main_pool_alloc(main_pool_available() - 0x10, MEMORY_POOL_LEFT);
    gCurrStaticSurfacePoolEnd = gCurrStaticSurfacePool;

    // A while loop iterating through each section of the level data. Sections of data
    // are prefixed by a terrain "type." This type is reused for surfaces as the surface
    // type.
    while (TRUE) {
        terrainLoadType = *data++;

        if (TERRAIN_LOAD_IS_SURFACE_TYPE_LOW(terrainLoadType)) {
            load_static_surfaces(&data, vertexData, terrainLoadType, &surfaceRooms);
        } else if (terrainLoadType == TERRAIN_LOAD_VERTICES) {
            vertexData = read_vertex_data(&data);
        } else if (terrainLoadType == TERRAIN_LOAD_ENVIRONMENT) {
            load_environmental_regions(&data);
        } else if (terrainLoadType == TERRAIN_LOAD_CONTINUE) {
            continue;
        } else if (terrainLoadType == TERRAIN_LOAD_END) {
            break;
        } else if (TERRAIN_LOAD_IS_SURFACE_TYPE_HIGH(terrainLoadType)) {
            load_static_surfaces(&data, vertexData, terrainLoadType, &surfaceRooms);
            continue;
        }
    }

    surfacePoolData = (uintptr_t)gCurrStaticSurfacePoolEnd - (uintptr_t)gCurrStaticSurfacePool;
    gTotalStaticSurfaceData += surfacePoolData;
    main_pool_realloc(gCurrStaticSurfacePool, surfacePoolData);

    gNumStaticSurfaceNodes = gSurfaceNodesAllocated;
    gNumStaticSurfaces = gSurfacesAllocated;
    profiler_collision_update(first);
}

/**
 * If not in time stop, clear the surface partitions.
 */
void clear_dynamic_surfaces(void) {
    PUPPYPRINT_GET_SNAPSHOT();
    if (!(gTimeStopState & TIME_STOP_ACTIVE)) {
        clear_dynamic_surface_references();

        gSurfacesAllocated = gNumStaticSurfaces;
        gSurfaceNodesAllocated = gNumStaticSurfaceNodes;
        gDynamicSurfacePoolEnd = gDynamicSurfacePool;
        if (sClearAllCells) {
            bzero(gDynamicSurfacePartition, sizeof(gDynamicSurfacePartition));
        } else {
            for (u32 i = 0; i < sNumCellsUsed; i++) {
                gDynamicSurfacePartition[sCellsUsed[i].z][sCellsUsed[i].x][sCellsUsed[i].partition] = NULL;
            }
        }
        sNumCellsUsed = 0;
        sClearAllCells = FALSE;
    }
    profiler_collision_update(first);
}

/**
 * Applies an object's transformation to the object's vertices.
 */
void transform_object_vertices(TerrainData **data, TerrainData *vertexData) {
    Mat4 *objectTransform = &o->transform;

    register s32 numVertices = *(*data)++;

    register TerrainData *vertices = *data;

    if (o->header.gfx.throwMatrix == NULL) {
        o->header.gfx.throwMatrix = objectTransform;
        obj_build_transform_from_pos_and_angle(o, O_POS_INDEX, O_FACE_ANGLE_INDEX);
    }

    Mat4 transform;
    mtxf_scale_vec3f(transform, *objectTransform, o->header.gfx.scale);

    // Go through all vertices, rotating and translating them to transform the object.
    Vec3f pos;
    while (numVertices--) {
        vec3s_to_vec3f(pos, vertices);
        vertices += 3;

        //! No bounds check on vertex data
        linear_mtxf_mul_vec3_and_translate(transform, vertexData, pos);

        vertexData += 3;
    }

    *data = vertices;
}

/**
 * Load in the surfaces for the o. This includes setting the flags, exertion, and room.
 */
void load_object_surfaces(TerrainData **data, TerrainData *vertexData, u32 dynamic) {
    s32 i;

    s32 surfaceType = *(*data)++;
    s32 numSurfaces = *(*data)++;

#ifndef ALL_SURFACES_HAVE_FORCE
    TerrainData hasForce = surface_has_force(surfaceType);
#endif

    s32 flags = surf_has_no_cam_collision(surfaceType) | (dynamic ? SURFACE_FLAG_DYNAMIC : 0);

    // The DDD warp is initially loaded at the origin and moved to the proper
    // position in paintings.c and doesn't update its room, so set it here.
    RoomData room = (o->behavior == segmented_to_virtual(bhvDddWarp)) ? 5 : 0;

    for (i = 0; i < numSurfaces; i++) {
        struct Surface *surface = read_surface_data(vertexData, data, dynamic);

        if (surface != NULL) {
            surface->object = o;
            surface->type = surfaceType;

#ifdef ALL_SURFACES_HAVE_FORCE
            surface->force = *(*data + 3);
#else
            if (hasForce) {
                surface->force = *(*data + 3);
            } else {
                surface->force = 0;
            }
#endif

            surface->flags |= flags;
            surface->room = room;
            add_surface(surface, dynamic);
        }

#ifdef ALL_SURFACES_HAVE_FORCE
        *data += 4;
#else
        if (hasForce) {
            *data += 4;
        } else {
            *data += 3;
        }
#endif
    }
}

#ifdef AUTO_COLLISION_DISTANCE
static void get_optimal_coll_dist(struct Object *obj) {
    register f32 thisVertDist, maxDist = 0.0f;
    Vec3f v;
    TerrainData *collisionData = o->collisionData;
    obj->oFlags |= OBJ_FLAG_DONT_CALC_COLL_DIST;
    collisionData++;
    register u32 vertsLeft = *(collisionData)++;
    while (vertsLeft) {
        vec3_prod(v, collisionData, obj->header.gfx.scale);
        thisVertDist = vec3_sumsq(v);
        if (thisVertDist > maxDist) maxDist = thisVertDist;
        collisionData += 3;
        vertsLeft--;
    }
    obj->oCollisionDistance = (sqrtf(maxDist) + 100.0f);
}
#endif

static TerrainData sVertexData[600];

/**
 * Transform an object's vertices, reload them, and render the object.
 */
void load_object_collision_model(void) {
    PUPPYPRINT_GET_SNAPSHOT();
    TerrainData *collisionData = o->collisionData;

    DEBUG_ASSERTF((collisionData != NULL), ASSERT_PREFIX_LEVEL"Object's collision data is missing!");

    Vec3f dist;
    vec3_diff(dist, &o->oPosVec, &gMarioObject->oPosVec);
    f32 sqrLateralDist = sqr(dist[0]) + sqr(dist[2]);

    f32 verticalMarioDiff = gMarioObject->oPosY - o->oPosY;

#ifdef AUTO_COLLISION_DISTANCE
    if (!(o->oFlags & OBJ_FLAG_DONT_CALC_COLL_DIST)) {
        get_optimal_coll_dist(o);
    }
#endif

    // If the object collision is supposed to be loaded more than the
    // drawing distance, extend the drawing range.
    if (o->oCollisionDistance > o->oDrawingDistance) {
        o->oDrawingDistance = o->oCollisionDistance;
    }
    
    s32 inColRadius = (
           (sqrLateralDist < sqr(o->oCollisionDistance))
        && (verticalMarioDiff > 0 || verticalMarioDiff > -o->oCollisionDistance)
        && (verticalMarioDiff < 0 || verticalMarioDiff < o->oCollisionDistance + 2000.f)
    );

    // Update if no Time Stop, in range, and in the current room.
    if (
        !(gTimeStopState & TIME_STOP_ACTIVE)
        && inColRadius
        && !(o->activeFlags & ACTIVE_FLAG_IN_DIFFERENT_ROOM)
    ) {
        collisionData++;
        transform_object_vertices(&collisionData, sVertexData);

        // TERRAIN_LOAD_CONTINUE acts as an "end" to the terrain data.
        while (*collisionData != TERRAIN_LOAD_CONTINUE) {
            load_object_surfaces(&collisionData, sVertexData, TRUE);
        }
    }

    f32 marioDist = o->oDistanceToMario;

    // On an object's first frame, the distance is set to 19000.0f.
    // If the distance hasn't been updated, update it now.
    if (marioDist == 19000.0f) {
        marioDist = dist_between_objects(o, gMarioObject);
    }

    COND_BIT((marioDist < o->oDrawingDistance), o->header.gfx.node.flags, GRAPH_RENDER_ACTIVE);
    profiler_collision_update(first);
}

/**
 * Transform an object's vertices and add them to the static surface pool.
 */
void load_object_static_model(void) {
    PUPPYPRINT_GET_SNAPSHOT();
    TerrainData *collisionData = o->collisionData;
    u32 surfacePoolData;

    DEBUG_ASSERTF((collisionData != NULL), ASSERT_PREFIX_LEVEL"Object's collision data is missing!");

    // Initialise a new surface pool for this block of surface data
    gCurrStaticSurfacePool = main_pool_alloc(main_pool_available() - 0x10, MEMORY_POOL_LEFT);
    gCurrStaticSurfacePoolEnd = gCurrStaticSurfacePool;
    gSurfaceNodesAllocated = gNumStaticSurfaceNodes;
    gSurfacesAllocated = gNumStaticSurfaces;

    collisionData++;
    transform_object_vertices(&collisionData, sVertexData);

    // TERRAIN_LOAD_CONTINUE acts as an "end" to the terrain data.
    while (*collisionData != TERRAIN_LOAD_CONTINUE) {
        load_object_surfaces(&collisionData, sVertexData, FALSE);
    }

    surfacePoolData = (uintptr_t)gCurrStaticSurfacePoolEnd - (uintptr_t)gCurrStaticSurfacePool;
    gTotalStaticSurfaceData += surfacePoolData;
    main_pool_realloc(gCurrStaticSurfacePool, surfacePoolData);

    gNumStaticSurfaceNodes = gSurfaceNodesAllocated;
    gNumStaticSurfaces = gSurfacesAllocated;
    profiler_collision_update(first);
}
