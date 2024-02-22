#include <PR/ultratypes.h>

#include "sm64.h"
#include "game/debug.h"
#include "game/level_update.h"
#include "game/mario.h"
#include "game/object_list_processor.h"
#include "math_util.h"
#include "surface_collision.h"
#include "surface_load.h"
#include "game/puppyprint.h"


/**************************************************
 *                      WALLS                     *
 **************************************************/

#ifdef ROUNDED_WALL_CORNERS

static s32 check_wall_triangle_vw(f32 d00, f32 d01, f32 d11, f32 d20, f32 d21, f32 invDenom) {
    f32 v = ((d11 * d20) - (d01 * d21)) * invDenom;
    if (v < 0.0f || v > 1.0f) {
        return TRUE;
    }

    f32 w = ((d00 * d21) - (d01 * d20)) * invDenom;
    if (w < 0.0f || w > 1.0f || (v + w) > 1.0f) {
        return TRUE;
    }

    return FALSE;
}

s32 check_wall_triangle_edge(Vec3f vert, Vec3f v2, f32 *d00, f32 *d01, f32 *invDenom, f32 *offset, f32 margin_radius) {
    f32 y = vert[1];

    if (FLT_IS_NONZERO(y)) {
        f32 v = (v2[1] / y);
        if (v < 0.0f || v > 1.0f) {
            return TRUE;
        }

        *d00 = ((vert[0] * v) - v2[0]);
        *d01 = ((vert[2] * v) - v2[2]);
        *invDenom = sqrtf(sqr(*d00) + sqr(*d01));
        *offset = (*invDenom - margin_radius);

        return (*offset > 0.0f);
    }

    return TRUE;
}

#endif

/**
 * Iterate through the list of walls until all walls are checked and
 * have given their wall push.
 */
static s32 find_wall_collisions_from_list(struct SurfaceNode *surfaceNode, struct WallCollisionData *data) {
    struct Surface *surf = NULL;
    f32 radius = data->radius;
    Vec3f pos = { data->x, data->y + data->offsetY, data->z };
    TerrainData type = SURFACE_DEFAULT;
    s32 numCols = 0;
#ifdef ROUNDED_WALL_CORNERS
    const f32 corner_threshold = -0.9f;
    f32 margin_radius = radius - 1.0f;
#endif

    _Bool checkingForCamera = (gCollisionFlags & COLLISION_FLAG_CAMERA);
    _Bool returnFirst = (gCollisionFlags & COLLISION_FLAG_RETURN_FIRST);

    // Check whether the object will be able to pass through certain walls.
    struct Object* obj = o;
    _Bool canPassVanishWalls = (
        (obj != NULL) && (
            (obj->activeFlags & ACTIVE_FLAG_MOVE_THROUGH_GRATE) ||
            (obj == gMarioObject && (gMarioState->flags & MARIO_VANISH_CAP))
        )
    );


    // Stay in this loop until out of walls.
    while (surfaceNode != NULL) {
        surf        = surfaceNode->surface;
        surfaceNode = surfaceNode->next;
        type        = surf->type;

        // Exclude a large number of walls immediately to optimize.
        if (pos[1] < surf->lowerY || pos[1] > surf->upperY) continue;

        // Determine if checking for the camera or not.
        if (checkingForCamera) {
            if (surf->flags & SURFACE_FLAG_NO_CAM_COLLISION) continue;
        } else {
            // Ignore camera only surfaces.
            if (type == SURFACE_CAMERA_BOUNDARY) continue;

            // If an object can pass through a vanish cap wall, pass through.
            if (canPassVanishWalls && (type == SURFACE_VANISH_CAP_WALLS)) continue;
        }

        // Dot of normal and pos, + origin offset
        f32 offset = (surf->normal.x * pos[0]) +
                     (surf->normal.y * pos[1]) +
                     (surf->normal.z * pos[2]) +
                     surf->originOffset;

        // Exclude surfaces outside of the radius.
        if (offset < -radius || offset > radius) continue;

#ifdef ROUNDED_WALL_CORNERS
        Vec3f v0, v1, v2;
        vec3_diff(v0, surf->vertex2, surf->vertex1);
        vec3_diff(v1, surf->vertex3, surf->vertex1);
        vec3_diff(v2, pos,           surf->vertex1);

        // Face
        f32 d00 = vec3_dot(v0, v0);
        f32 d01 = vec3_dot(v0, v1);
        f32 d11 = vec3_dot(v1, v1);
        f32 d20 = vec3_dot(v2, v0);
        f32 d21 = vec3_dot(v2, v1);

        f32 invDenom = (d00 * d11) - (d01 * d01);
        if (FLT_IS_NONZERO(invDenom)) {
            invDenom = 1.0f / invDenom;
        }

        if (check_wall_triangle_vw(d00, d01, d11, d20, d21, invDenom)) {
            if (offset < 0) continue;

            // Edge 1-2
            if (check_wall_triangle_edge(v0, v2, &d00, &d01, &invDenom, &offset, margin_radius)) {
                // Edge 1-3
                if (check_wall_triangle_edge(v1, v2, &d00, &d01, &invDenom, &offset, margin_radius)) {
                    vec3_diff(v1, surf->vertex3, surf->vertex2);
                    vec3_diff(v2, pos, surf->vertex2);
                    // Edge 2-3
                    if (check_wall_triangle_edge(v1, v2, &d00, &d01, &invDenom, &offset, margin_radius)) {
                        continue;
                    }
                }
            }

            // Check collision
            if (FLT_IS_NONZERO(invDenom)) {
                invDenom = (offset / invDenom);
            }

            // Update pos
            pos[0] += (d00 *= invDenom);
            pos[2] += (d01 *= invDenom);
            margin_radius += 0.01f;

            if ((d00 * surf->normal.x) + (d01 * surf->normal.z) < (corner_threshold * offset)) {
                continue;
            }
        } else {
            // Update pos
            pos[0] += surf->normal.x * (radius - offset);
            pos[2] += surf->normal.z * (radius - offset);
        }
#else
        f32 x = pos[0];
        f32 y = pos[1];
        f32 z = pos[2];
        f32 w1, w2, w3;
        f32 y1, y2, y3;

        //! (Quantum Tunneling) Due to issues with the vertices walls choose and
        //  the fact they are floating point, certain floating point positions
        //  along the seam of two walls may collide with neither wall or both walls.
        if (surf->flags & SURFACE_FLAG_X_PROJECTION) {
            w1 = -surf->vertex1[2]; w2 = -surf->vertex2[2]; w3 = -surf->vertex3[2];
            y1 =  surf->vertex1[1]; y2 =  surf->vertex2[1]; y3 =  surf->vertex3[1];

            if (surf->normal.x > 0.0f) {
                if (
                    ((((y1 - y) * (w2 - w1)) - ((w1 - -z) * (y2 - y1))) > 0.0f) ||
                    ((((y2 - y) * (w3 - w2)) - ((w2 - -z) * (y3 - y2))) > 0.0f) ||
                    ((((y3 - y) * (w1 - w3)) - ((w3 - -z) * (y1 - y3))) > 0.0f)
                ) continue;
            } else {
                if (
                    ((((y1 - y) * (w2 - w1)) - ((w1 - -z) * (y2 - y1))) < 0.0f) ||
                    ((((y2 - y) * (w3 - w2)) - ((w2 - -z) * (y3 - y2))) < 0.0f) ||
                    ((((y3 - y) * (w1 - w3)) - ((w3 - -z) * (y1 - y3))) < 0.0f)
                ) continue;
            }
        } else {
            w1 = surf->vertex1[0]; w2 = surf->vertex2[0]; w3 = surf->vertex3[0];
            y1 = surf->vertex1[1]; y2 = surf->vertex2[1]; y3 = surf->vertex3[1];

            if (surf->normal.z > 0.0f) {
                if (
                    ((((y1 - y) * (w2 - w1)) - ((w1 - x) * (y2 - y1))) > 0.0f) ||
                    ((((y2 - y) * (w3 - w2)) - ((w2 - x) * (y3 - y2))) > 0.0f) ||
                    ((((y3 - y) * (w1 - w3)) - ((w3 - x) * (y1 - y3))) > 0.0f)
                ) continue;
            } else {
                if (
                    ((((y1 - y) * (w2 - w1)) - ((w1 - x) * (y2 - y1))) < 0.0f) ||
                    ((((y2 - y) * (w3 - w2)) - ((w2 - x) * (y3 - y2))) < 0.0f) ||
                    ((((y3 - y) * (w1 - w3)) - ((w3 - x) * (y1 - y3))) < 0.0f)
                ) continue;
            }
        }

        // Update pos
        pos[0] += surf->normal.x * (radius - offset);
        pos[2] += surf->normal.z * (radius - offset);
#endif

        // Has collision
        if (data->numWalls < MAX_REFERENCED_WALLS) {
            data->walls[data->numWalls++] = surf;
        }
        numCols++;

        if (returnFirst) {
            break;
        }
    }

    data->x = pos[0];
    data->z = pos[2];

    return numCols;
}

/**
 * Formats the position and wall search for find_wall_collisions.
 */
s32 f32_find_wall_collision(f32 *xPtr, f32 *yPtr, f32 *zPtr, f32 offsetY, f32 radius) {
    struct WallCollisionData collision;

    collision.offsetY = offsetY;
    collision.radius = radius;

    collision.x = *xPtr;
    collision.y = *yPtr;
    collision.z = *zPtr;

    collision.numWalls = 0;

    s32 numCollisions = find_wall_collisions(&collision);

    *xPtr = collision.x;
    *yPtr = collision.y;
    *zPtr = collision.z;

    return numCollisions;
}

/**
 * Find wall collisions and receive their push.
 */
s32 find_wall_collisions(struct WallCollisionData *colData) {
    struct SurfaceNode *node;
    s32 numCollisions = 0;
    s32 x = colData->x;
    s32 z = colData->z;
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.collision_wall);
    PUPPYPRINT_GET_SNAPSHOT();

    colData->numWalls = 0;

    if (is_outside_level_bounds(x, z)) {
        profiler_collision_update(first);
        return numCollisions;
    }

    // World (level) consists of a 16x16 grid. Find where the collision is on the grid (round toward -inf)
    s32 radius = colData->radius;
    s32 minCellX = GET_CELL_COORD(x - radius);
    s32 minCellZ = GET_CELL_COORD(z - radius);
    s32 maxCellX = GET_CELL_COORD(x + radius);
    s32 maxCellZ = GET_CELL_COORD(z + radius);

    for (s32 cellX = minCellX; cellX <= maxCellX; cellX++) {
        for (s32 cellZ = minCellZ; cellZ <= maxCellZ; cellZ++) {
            if (!(gCollisionFlags & COLLISION_FLAG_EXCLUDE_DYNAMIC)) {
                // Check for surfaces belonging to objects.
                node = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS].next;
                numCollisions += find_wall_collisions_from_list(node, colData);
            }

            // Check for surfaces that are a part of level geometry.
            node = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS].next;
            numCollisions += find_wall_collisions_from_list(node, colData);
        }
    }

    gCollisionFlags &= ~(COLLISION_FLAG_RETURN_FIRST | COLLISION_FLAG_EXCLUDE_DYNAMIC | COLLISION_FLAG_INCLUDE_INTANGIBLE);
#ifdef VANILLA_DEBUG
    // Increment the debug tracker.
    gNumCalls.wall++;
#endif

    profiler_collision_update(first);
    return numCollisions;
}

/**
 * Collides with walls and returns the most recent wall.
 */
void resolve_and_return_wall_collisions(Vec3f pos, f32 offset, f32 radius, struct WallCollisionData *collisionData) {
    collisionData->x = pos[0];
    collisionData->y = pos[1];
    collisionData->z = pos[2];
    collisionData->radius = radius;
    collisionData->offsetY = offset;

    find_wall_collisions(collisionData);

    pos[0] = collisionData->x;
    pos[1] = collisionData->y;
    pos[2] = collisionData->z;
}

/**************************************************
 *                     CEILINGS                   *
 **************************************************/

static s32 check_within_ceil_triangle_bounds(s32 x, s32 z, struct Surface *surf) {
    Vec3i vx, vz;
    vx[0] = surf->vertex1[0];
    vz[0] = surf->vertex1[2];
    vx[1] = surf->vertex2[0];
    vz[1] = surf->vertex2[2];

    // Checking if point is in bounds of the triangle laterally.
    if (((vz[0] - z) * (vx[1] - vx[0]) - (vx[0] - x) * (vz[1] - vz[0])) > 0) return FALSE;

    // Slight optimization by checking these later.
    vx[2] = surf->vertex3[0];
    vz[2] = surf->vertex3[2];

    if (((vz[1] - z) * (vx[2] - vx[1]) - (vx[1] - x) * (vz[2] - vz[1])) > 0) return FALSE;
    if (((vz[2] - z) * (vx[0] - vx[2]) - (vx[2] - x) * (vz[0] - vz[2])) > 0) return FALSE;

    return TRUE;
}

/**
 * Iterate through the list of ceilings and find the first ceiling over a given point.
 */
static struct Surface *find_ceil_from_list(struct SurfaceNode *surfaceNode, s32 x, s32 y, s32 z, f32 *pheight) {
    struct Surface *surf, *ceil = NULL;
    f32 height;
    SurfaceType type = SURFACE_DEFAULT;
    *pheight = CELL_HEIGHT_LIMIT;

    _Bool returnFirst = (gCollisionFlags & COLLISION_FLAG_RETURN_FIRST);
    _Bool checkingForCamera = (gCollisionFlags & COLLISION_FLAG_CAMERA);

    // Stay in this loop until out of ceilings.
    while (surfaceNode != NULL) {
        surf        = surfaceNode->surface;
        surfaceNode = surfaceNode->next;
        type        = surf->type;

        // Exclude all ceilings below the point
        if (y > surf->upperY) continue;

        // Determine if checking for the camera or not
        if (checkingForCamera) {
            if (surf->flags & SURFACE_FLAG_NO_CAM_COLLISION) {
                continue;
            }
        } else if (type == SURFACE_CAMERA_BOUNDARY) {
            // Ignore camera only surfaces
            continue;
        }

        // Check that the point is within the triangle bounds
        if (!check_within_ceil_triangle_bounds(x, z, surf)) continue;

        // Find the height of the ceil at the given location
        height = get_surface_height_at_location(x, z, surf);

        // Exclude ceilings above the previous lowest ceiling
        if (height > *pheight) continue;

        // Checks for ceiling interaction
        if (y > height) continue;


        // Use the current ceiling
        *pheight = height;
        ceil = surf;

        // Exit the loop if it's not possible for another ceiling to be closer
        // to the original point, or if COLLISION_FLAG_RETURN_FIRST.
        if (height == y) break;
#ifdef SLOPE_FIX
        if (returnFirst) break;
#endif
    }

    return ceil;
}

/**
 * Find the lowest ceiling above a given position and return the height.
 */
f32 find_ceil(f32 posX, f32 posY, f32 posZ, struct Surface **pceil) {
    f32 height        = CELL_HEIGHT_LIMIT;
    f32 dynamicHeight = CELL_HEIGHT_LIMIT;
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.collision_ceil);
    PUPPYPRINT_GET_SNAPSHOT();
    s32 x = posX;
    s32 y = posY;
    s32 z = posZ;
    *pceil = NULL;

    if (is_outside_level_bounds(x, z)) {
        profiler_collision_update(first);
        return height;
    }

    // Each level is split into cells to limit load, find the appropriate cell.
    s32 cellX = GET_CELL_COORD(x);
    s32 cellZ = GET_CELL_COORD(z);

    struct SurfaceNode *surfaceList;
    struct Surface *ceil = NULL;
    struct Surface *dynamicCeil = NULL;

    s32 includeDynamic = !(gCollisionFlags & COLLISION_FLAG_EXCLUDE_DYNAMIC);

    if (includeDynamic) {
        // Check for surfaces belonging to objects.
        surfaceList = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_CEILS].next;
        dynamicCeil = find_ceil_from_list(surfaceList, x, y, z, &dynamicHeight);

        // In the next check, only check for ceilings lower than the previous check.
        height = dynamicHeight;
    }

    // Check for surfaces that are a part of level geometry.
    surfaceList = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_CEILS].next;
    ceil = find_ceil_from_list(surfaceList, x, y, z, &height);

    // Use the lower ceiling.
    if (includeDynamic && height >= dynamicHeight) {
        ceil   = dynamicCeil;
        height = dynamicHeight;
    }

    // To prevent accidentally leaving the floor tangible, stop checking for it.
    gCollisionFlags &= ~(COLLISION_FLAG_RETURN_FIRST | COLLISION_FLAG_EXCLUDE_DYNAMIC | COLLISION_FLAG_INCLUDE_INTANGIBLE);

    // Return the ceiling.
    *pceil = ceil;
#ifdef VANILLA_DEBUG
    // Increment the debug tracker.
    gNumCalls.ceil++;
#endif

    profiler_collision_update(first);
    return height;
}

/**************************************************
 *                     FLOORS                     *
 **************************************************/

static s32 check_within_floor_triangle_bounds(s32 x, s32 z, struct Surface *surf) {
    Vec3i vx, vz;
    vx[0] = surf->vertex1[0];
    vz[0] = surf->vertex1[2];
    vx[1] = surf->vertex2[0];
    vz[1] = surf->vertex2[2];

    // Checking if point is in bounds of the triangle laterally.
    if (((vz[0] - z) * (vx[1] - vx[0]) - (vx[0] - x) * (vz[1] - vz[0])) < 0) return FALSE;

    // Slight optimization by checking these later.
    vx[2] = surf->vertex3[0];
    vz[2] = surf->vertex3[2];

    if (((vz[1] - z) * (vx[2] - vx[1]) - (vx[1] - x) * (vz[2] - vz[1])) < 0) return FALSE;
    if (((vz[2] - z) * (vx[0] - vx[2]) - (vx[2] - x) * (vz[0] - vz[2])) < 0) return FALSE;

    return TRUE;
}

/**
 * Iterate through the list of floors and find the first floor under a given point.
 */
static struct Surface *find_floor_from_list(struct SurfaceNode *surfaceNode, s32 x, s32 y, s32 z, f32 *pheight) {
    struct Surface *surf, *floor = NULL;
    SurfaceType type = SURFACE_DEFAULT;
    f32 height;

    _Bool returnFirst = (gCollisionFlags & COLLISION_FLAG_RETURN_FIRST);
    _Bool checkingForCamera = (gCollisionFlags & COLLISION_FLAG_CAMERA);
    _Bool skipIntangible = !(gCollisionFlags & COLLISION_FLAG_INCLUDE_INTANGIBLE);

    // Iterate through the list of floors until there are no more floors.
    while (surfaceNode != NULL) {
        surf        = surfaceNode->surface;
        surfaceNode = surfaceNode->next;
        type        = surf->type;

        // To prevent the Merry-Go-Round room from loading when Mario passes above the hole that leads
        // there, SURFACE_INTANGIBLE is used. This prevent the wrong room from loading, but can also allow
        // Mario to pass through.
        if (skipIntangible && (type == SURFACE_INTANGIBLE)) {
            continue;
        }

        // Determine if we are checking for the camera or not.
        if (checkingForCamera) {
            if (surf->flags & SURFACE_FLAG_NO_CAM_COLLISION) {
                continue;
            }
        } else if (type == SURFACE_CAMERA_BOUNDARY) {
            continue; // If we are not checking for the camera, ignore camera only floors.
        }

        // Exclude all floors above the point.
        if (y < surf->lowerY) continue;

        // Check that the point is within the triangle bounds.
        if (!check_within_floor_triangle_bounds(x, z, surf)) continue;

        // Get the height of the floor under the current location.
        height = get_surface_height_at_location(x, z, surf);

        // Exclude floors lower than the previous highest floor.
        if (height <= *pheight) continue;

        // Checks for floor interaction with a FIND_FLOOR_BUFFER unit buffer.
        if (y < height) continue;

        // Use the current floor
        *pheight = height;
        floor = surf;

        // Exit the loop if it's not possible for another floor to be closer
        // to the original point, or if COLLISION_FLAG_RETURN_FIRST.
        if (height == y) break;
#ifdef SLOPE_FIX
        if (returnFirst) break;
#else
        break;
#endif
    }

    return floor;
}

// Generic triangle bounds func
ALWAYS_INLINE static s32 check_within_bounds_y_norm(s32 x, s32 z, struct Surface *surf) {
    if (surf->normal.y >= NORMAL_FLOOR_THRESHOLD) return check_within_floor_triangle_bounds(x, z, surf);
    return check_within_ceil_triangle_bounds(x, z, surf);
}

/**
 * Iterate through the list of water floors and find the first water floor under a given point.
 */
struct Surface *find_water_floor_from_list(struct SurfaceNode *surfaceNode, s32 x, s32 y, s32 z, f32 *pheight) {
    struct Surface *surf;
    struct Surface *floor = NULL;
    struct SurfaceNode *topSurfaceNode = surfaceNode;
    struct SurfaceNode *bottomSurfaceNode = surfaceNode;
    f32 height = FLOOR_LOWER_LIMIT;
    f32 curHeight = FLOOR_LOWER_LIMIT;
    f32 bottomHeight = FLOOR_LOWER_LIMIT;
    f32 curBottomHeight = FLOOR_LOWER_LIMIT;
    f32 buffer = FIND_FLOOR_BUFFER;

    // Iterate through the list of water floors until there are no more water floors.
    // SURFACE_NEW_WATER_BOTTOM
    while (bottomSurfaceNode != NULL) {
        surf = bottomSurfaceNode->surface;
        bottomSurfaceNode = bottomSurfaceNode->next;

        // skip wall angled water
        if (surf->type != SURFACE_NEW_WATER_BOTTOM || absf(surf->normal.y) < NORMAL_FLOOR_THRESHOLD) continue;

        if (!check_within_bounds_y_norm(x, z, surf)) continue;

        curBottomHeight = get_surface_height_at_location(x, z, surf);

        if (curBottomHeight < y + buffer) {
            continue;
        } else {
            bottomHeight = curBottomHeight;
        }
    }

    // Iterate through the list of water tops until there are no more water tops.
    // SURFACE_NEW_WATER
    while (topSurfaceNode != NULL) {
        surf = topSurfaceNode->surface;
        topSurfaceNode = topSurfaceNode->next;

        // skip water tops or wall angled water bottoms
        if (surf->type == SURFACE_NEW_WATER_BOTTOM || absf(surf->normal.y) < NORMAL_FLOOR_THRESHOLD) continue;

        if (!check_within_bounds_y_norm(x, z, surf)) continue;

        curHeight = get_surface_height_at_location(x, z, surf);

        if (bottomHeight != FLOOR_LOWER_LIMIT && curHeight > bottomHeight) continue;

        if (curHeight > height) {
            height = curHeight;
            *pheight = curHeight;
            floor = surf;
        }
    }

    return floor;
}

/**
 * Find the height of the highest floor below a point.
 */
f32 find_floor_height(f32 x, f32 y, f32 z) {
    struct Surface *floor;
    return find_floor(x, y, z, &floor);
}

/**
 * Find the highest dynamic floor under a given position. Perhaps originally static
 * and dynamic floors were checked separately.
 */
f32 unused_find_dynamic_floor(f32 xPos, f32 yPos, f32 zPos, struct Surface **pfloor) {
    f32 floorHeight = FLOOR_LOWER_LIMIT;

    // Would normally cause PUs, but dynamic floors unload at that range.
    s32 x = xPos;
    s32 y = yPos + FIND_FLOOR_BUFFER;
    s32 z = zPos;

    // Each level is split into cells to limit load, find the appropriate cell.
    s32 cellX = GET_CELL_COORD(x);
    s32 cellZ = GET_CELL_COORD(z);

    struct SurfaceNode *surfaceList = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_FLOORS].next;

    *pfloor = find_floor_from_list(surfaceList, x, y, z, &floorHeight);

    return floorHeight;
}

/**
 * Find the highest floor under a given position and return the height.
 */
f32 find_floor(f32 xPos, f32 yPos, f32 zPos, struct Surface **pfloor) {
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.collision_floor);
    PUPPYPRINT_GET_SNAPSHOT();

    f32 height        = FLOOR_LOWER_LIMIT;
    f32 dynamicHeight = FLOOR_LOWER_LIMIT;

    //! (Parallel Universes) Because position is casted to an s32, reaching higher
    //  float locations can return floors despite them not existing there.
    //  (Dynamic floors will unload due to the range.)
    s32 x = xPos;
    s32 y = yPos + FIND_FLOOR_BUFFER;
    s32 z = zPos;

    *pfloor = NULL;

    if (is_outside_level_bounds(x, z)) {
        profiler_collision_update(first);
        return height;
    }
    // Each level is split into cells to limit load, find the appropriate cell.
    s32 cellX = GET_CELL_COORD(x);
    s32 cellZ = GET_CELL_COORD(z);

    struct SurfaceNode *surfaceList;
    struct Surface *floor = NULL;
    struct Surface *dynamicFloor = NULL;

    s32 includeDynamic = !(gCollisionFlags & COLLISION_FLAG_EXCLUDE_DYNAMIC);

    if (includeDynamic) {
        // Check for surfaces belonging to objects.
        surfaceList = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_FLOORS].next;
        dynamicFloor = find_floor_from_list(surfaceList, x, y, z, &dynamicHeight);

        // In the next check, only check for floors higher than the previous check.
        height = dynamicHeight;
    }

    // Check for surfaces that are a part of level geometry.
    surfaceList = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_FLOORS].next;
    floor = find_floor_from_list(surfaceList, x, y, z, &height);

    // Use the higher floor.
    if (includeDynamic && height <= dynamicHeight) {
        floor  = dynamicFloor;
        height = dynamicHeight;
    }

    // To prevent accidentally leaving the floor tangible, stop checking for it.
    gCollisionFlags &= ~(COLLISION_FLAG_RETURN_FIRST | COLLISION_FLAG_EXCLUDE_DYNAMIC | COLLISION_FLAG_INCLUDE_INTANGIBLE);
    // If a floor was missed, increment the debug counter.
    if (floor == NULL) {
        gNumFindFloorMisses++;
    }

    // Return the floor.
    *pfloor = floor;
#ifdef VANILLA_DEBUG
    // Increment the debug tracker.
    gNumCalls.floor++;
#endif

    profiler_collision_update(first);
    return height;
}

f32 find_room_floor(f32 x, f32 y, f32 z, struct Surface **pfloor) {
    gCollisionFlags |= (COLLISION_FLAG_EXCLUDE_DYNAMIC | COLLISION_FLAG_INCLUDE_INTANGIBLE);

    return find_floor(x, y, z, pfloor);
}

/**
 * Get the room index at a given position.
 */
s32 get_room_at_pos(f32 x, f32 y, f32 z) {
    if (gCurrentArea->surfaceRooms != NULL) {
        struct Surface *floor;

        find_room_floor(x, y, z, &floor);

        if (floor != NULL) {
            return floor->room;
        }
    }

    return -1;
}

/**
 * Find the highest water floor under a given position and return the height.
 */
f32 find_water_floor(s32 xPos, s32 yPos, s32 zPos, struct Surface **pfloor) {
    f32 height = FLOOR_LOWER_LIMIT;

    s32 x = xPos;
    s32 y = yPos;
    s32 z = zPos;

    if (is_outside_level_bounds(x, z)) return height;

    // Each level is split into cells to limit load, find the appropriate cell.
    s32 cellX = GET_CELL_COORD(x);
    s32 cellZ = GET_CELL_COORD(z);

    // Check for surfaces that are a part of level geometry.
    struct SurfaceNode *surfaceList = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WATER].next;
    struct Surface     *floor       = find_water_floor_from_list(surfaceList, x, y, z, &height);

    if (floor == NULL) {
        height = FLOOR_LOWER_LIMIT;
    } else {
        *pfloor = floor;
    }
#ifdef VANILLA_DEBUG
    // Increment the debug tracker.
    gNumCalls.floor++;
#endif
    return height;
}

/**************************************************
 *               ENVIRONMENTAL BOXES              *
 **************************************************/

/**
 * Finds the height of water at a given location.
 */
s32 find_water_level_and_floor(s32 x, s32 y, s32 z, struct Surface **pfloor) {
    s32 val;
    s32 loX, hiX, loZ, hiZ;
    TerrainData *p = gEnvironmentRegions;
    struct Surface *floor = NULL;
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.collision_water);
    PUPPYPRINT_GET_SNAPSHOT();
    s32 waterLevel = find_water_floor(x, y, z, &floor);

    if (p != NULL && waterLevel == FLOOR_LOWER_LIMIT) {
        s32 numRegions = *p++;

        for (s32 i = 0; i < numRegions; i++) {
            val = *p++;
            loX = *p++;
            loZ = *p++;
            hiX = *p++;
            hiZ = *p++;

            // If the location is within a water box and it is a water box.
            // Water is less than 50 val only, while above is gas and such.
            if (loX < x && x < hiX && loZ < z && z < hiZ && val < 50) {
                // Set the water height. Since this breaks, only return the first height.
                waterLevel = *p;
                break;
            }
            p++;
        }
    } else {
        *pfloor = floor;
    }

    profiler_collision_update(first);
    return waterLevel;
}

/**
 * Finds the height of water at a given location.
 */
s32 find_water_level(s32 x, s32 z) { // TODO: Allow y pos
    s32 val;
    s32 loX, hiX, loZ, hiZ;
    TerrainData *p = gEnvironmentRegions;
    struct Surface *floor = NULL;
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.collision_water);
    PUPPYPRINT_GET_SNAPSHOT();
    s32 waterLevel = find_water_floor(x, ((gCollisionFlags & COLLISION_FLAG_CAMERA) ? gLakituState.pos[1] : gMarioState->pos[1]), z, &floor);

    if ((p != NULL) && (waterLevel == FLOOR_LOWER_LIMIT)) {
        s32 numRegions = *p++;

        for (s32 i = 0; i < numRegions; i++) {
            val = *p++;
            loX = *p++;
            loZ = *p++;
            hiX = *p++;
            hiZ = *p++;

            // If the location is within a water box and it is a water box.
            // Water is less than 50 val only, while above is gas and such.
            if (loX <= x && x <= hiX && loZ <= z && z <= hiZ && val < 50) {
                // Set the water height. Since this breaks, only return the first height.
                waterLevel = *p;
                break;
            }
            p++;
        }
    }

    profiler_collision_update(first);

    return waterLevel;
}

/**
 * Finds the height of the poison gas (used only in HMC) at a given location.
 */
s32 find_poison_gas_level(s32 x, s32 z) {
    s32 val;
    s32 loX, hiX, loZ, hiZ;
    s32 gasLevel = FLOOR_LOWER_LIMIT;
    TerrainData *p = gEnvironmentRegions;
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.collision_water);
    PUPPYPRINT_GET_SNAPSHOT();

    if (p != NULL) {
        s32 numRegions = *p++;

        for (s32 i = 0; i < numRegions; i++) {
            val = *p;

            if (val >= 50) {
                loX = p[1];
                loZ = p[2];
                hiX = p[3];
                hiZ = p[4];

                // If the location is within a gas's box and it is a gas box.
                // Gas has a value of 50, 60, etc.
                if (loX < x && x < hiX && loZ < z && z < hiZ && val % 10 == 0) {
                    // Set the gas height. Since this breaks, only return the first height.
                    gasLevel = p[5];
                    break;
                }
            }

            p += 6;
        }
    }

    profiler_collision_update(first);
    return gasLevel;
}

/**************************************************
 *                      DEBUG                     *
 **************************************************/

#ifdef VANILLA_DEBUG
/**
 * Finds the length of a surface list for debug purposes.
 */
static s32 surface_list_length(struct SurfaceNode *list) {
    s32 count = 0;
    while (list != NULL) {
        list = list->next;
        count++;
    }
    return count;
}

/**
 * Print the area,number of walls, how many times they were called,
 * and some allocation information.
 */
void debug_surface_list_info(f32 xPos, f32 zPos) {
    struct SurfaceNode *list;
    s32 numFloors = 0;
    s32 numWalls  = 0;
    s32 numCeils  = 0;

    s32 cellX = GET_CELL_COORD(xPos);
    s32 cellZ = GET_CELL_COORD(zPos);

    list = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_FLOORS].next;
    numFloors += surface_list_length(list);

    list = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_FLOORS].next;
    numFloors += surface_list_length(list);

    list = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS].next;
    numWalls += surface_list_length(list);

    list = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS].next;
    numWalls += surface_list_length(list);

    list = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_CEILS].next;
    numCeils += surface_list_length(list);

    list = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_CEILS].next;
    numCeils += surface_list_length(list);

    print_debug_top_down_mapinfo("area   %x", cellZ * NUM_CELLS + cellX);

    // Names represent ground, walls, and roofs as found in SMS.
    print_debug_top_down_mapinfo("dg %d", numFloors);
    print_debug_top_down_mapinfo("dw %d", numWalls);
    print_debug_top_down_mapinfo("dr %d", numCeils);

    set_text_array_x_y(80, -3);

    print_debug_top_down_mapinfo("%d", gNumCalls.floor);
    print_debug_top_down_mapinfo("%d", gNumCalls.wall);
    print_debug_top_down_mapinfo("%d", gNumCalls.ceil);

    set_text_array_x_y(-80, 0);

    // listal- List Allocated?, statbg- Static Background?, movebg- Moving Background?
    print_debug_top_down_mapinfo("listal %d", gSurfaceNodesAllocated);
    print_debug_top_down_mapinfo("statbg %d", gNumStaticSurfaces);
    print_debug_top_down_mapinfo("movebg %d", (gSurfacesAllocated - gNumStaticSurfaces));

    gNumCalls.floor = 0;
    gNumCalls.ceil = 0;
    gNumCalls.wall = 0;
}
#endif

/**
 * An unused function that finds and interacts with any type of surface.
 * Perhaps an original implementation of surfaces before they were more specialized.
 */
s32 unused_resolve_floor_or_ceil_collisions(s32 checkCeil, f32 *px, f32 *py, f32 *pz, f32 radius,
                                            struct Surface **psurface, f32 *surfaceHeight) {
    f32 x = *px;
    f32 y = *py;
    f32 z = *pz;

    *psurface = NULL;

    if (checkCeil) {
        *surfaceHeight = find_ceil(x, y, z, psurface);
    } else {
        *surfaceHeight = find_floor(x, y, z, psurface);
    }

    if (*psurface == NULL) return -1;

    f32 nx = (*psurface)->normal.x;
    f32 ny = (*psurface)->normal.y;
    f32 nz = (*psurface)->normal.z;
    f32 oo = (*psurface)->originOffset;

    f32 offset = absf((nx * x) + (ny * y) + (nz * z) + oo);

    // Interesting surface interaction that should be surf type independent.
    if (offset < radius) {
        offset = (radius - offset);
        *px += (nx * offset);
        *py += (ny * offset);
        *pz += (nz * offset);

        return 1;
    }

    return 0;
}
