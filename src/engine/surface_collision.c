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

#define CALC_OFFSET(vert, next_step) {          \
    if (FLT_IS_NONZERO((vert)[1])) {            \
        v = (v2[1] / (vert)[1]);                \
        if ((v < 0.0f) || (v > 1.0f)) next_step;\
        d00 = (((vert)[0] * v) - v2[0]);        \
        d01 = (((vert)[2] * v) - v2[2]);        \
        invDenom = sqrtf(sqr(d00) + sqr(d01));  \
        offset   = (invDenom - margin_radius);  \
        if (offset > 0.0f) next_step;           \
        goto check_collision;                   \
    }                                           \
    next_step;                                  \
}

/**
 * Iterate through the list of walls until all walls are checked and
 * have given their wall push.
 */
static s32 find_wall_collisions_from_list(struct SurfaceNode *surfaceNode, struct WallCollisionData *data) {
    const f32 corner_threshold = -0.9f;
    register struct Surface *surf;
    register f32 offset;
    register f32 radius = data->radius;

    Vec3f pos = { data->x, data->y + data->offsetY, data->z };
    Vec3f v0, v1, v2;
    register f32 d00, d01, d11, d20, d21;
    register f32 invDenom;
    register f32 v, w;
    register TerrainData type = SURFACE_DEFAULT;
    s32 numCols = 0;

    // Max collision radius = 200
    if (radius > 200) {
        radius = 200;
    }

    f32 margin_radius = radius - 1.0f;

    // Stay in this loop until out of walls.
    while (surfaceNode != NULL) {
        surf        = surfaceNode->surface;
        surfaceNode = surfaceNode->next;
        type        = surf->type;

        // Exclude a large number of walls immediately to optimize.
        if (pos[1] < surf->lowerY || pos[1] > surf->upperY) continue;

        // Determine if checking for the camera or not.
        if (gCollisionFlags & COLLISION_FLAG_CAMERA) {
            if (surf->flags & SURFACE_FLAG_NO_CAM_COLLISION) continue;
        } else {
            // Ignore camera only surfaces.
            if (type == SURFACE_CAMERA_BOUNDARY) continue;

            // If an object can pass through a vanish cap wall, pass through.
            if (type == SURFACE_VANISH_CAP_WALLS && o != NULL) {
                // If an object can pass through a vanish cap wall, pass through.
                if (o->activeFlags & ACTIVE_FLAG_MOVE_THROUGH_GRATE) continue;
                // If Mario has a vanish cap, pass through the vanish cap wall.
                if (o == gMarioObject && gMarioState->flags & MARIO_VANISH_CAP) continue;
            }
        }

        // Dot of normal and pos, + origin offset
        offset = (surf->normal.x * pos[0]) + (surf->normal.y * pos[1]) + (surf->normal.z * pos[2]) + surf->originOffset;

        // Exclude surfaces outside of the radius.
        if (offset < -radius || offset > radius) continue;
    
        vec3_diff(v0, surf->vertex2, surf->vertex1);
        vec3_diff(v1, surf->vertex3, surf->vertex1);
        vec3_diff(v2, pos,           surf->vertex1);

        // Face
        d00 = vec3_dot(v0, v0);
        d01 = vec3_dot(v0, v1);
        d11 = vec3_dot(v1, v1);
        d20 = vec3_dot(v2, v0);
        d21 = vec3_dot(v2, v1);

        invDenom = (d00 * d11) - (d01 * d01);
        if (FLT_IS_NONZERO(invDenom)) invDenom = 1.0f / invDenom;

        v = ((d11 * d20) - (d01 * d21)) * invDenom;
        if (v < 0.0f || v > 1.0f) goto edge_1_2;

        w = ((d00 * d21) - (d01 * d20)) * invDenom;
        if (w < 0.0f || w > 1.0f || v + w > 1.0f) goto edge_1_2;

        pos[0] += surf->normal.x * (radius - offset);
        pos[2] += surf->normal.z * (radius - offset);
        goto hasCollision;

    edge_1_2:
        if (offset < 0) continue;
        CALC_OFFSET(v0, goto edge_1_3);

    edge_1_3:
        CALC_OFFSET(v1, goto edge_2_3);

    edge_2_3:
        vec3_diff(v1, surf->vertex3, surf->vertex2);
        vec3_diff(v2, pos, surf->vertex2);
        CALC_OFFSET(v1, continue);

    check_collision:
        if (FLT_IS_NONZERO(invDenom)) invDenom = (offset / invDenom);
        pos[0] += (d00 *= invDenom);
        pos[2] += (d01 *= invDenom);
        margin_radius += 0.01f;
        if ((d00 * surf->normal.x) + (d01 * surf->normal.z) < (corner_threshold * offset)) continue;

    hasCollision:
        if (data->numWalls < MAX_REFERENCED_WALLS) {
            data->walls[data->numWalls++] = surf;
        }
        numCols++;

        if (gCollisionFlags & COLLISION_FLAG_RETURN_FIRST) {
            break;
        }
    }
    data->x = pos[0];
    data->z = pos[2];
    return numCols;
}
#undef CALC_OFFSET

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
#if PUPPYPRINT_DEBUG
    OSTime first = osGetTime();
#endif

    colData->numWalls = 0;

    if (is_outside_level_bounds(x, z)) {
#if PUPPYPRINT_DEBUG
        collisionTime[perfIteration] += osGetTime() - first;
#endif
        return numCollisions;
    }

    // World (level) consists of a 16x16 grid. Find where the collision is on the grid (round toward -inf)
    s32 cellX = GET_CELL_COORD(x);
    s32 cellZ = GET_CELL_COORD(z);

    if (!(gCollisionFlags & COLLISION_FLAG_EXCLUDE_DYNAMIC)) {
        // Check for surfaces belonging to objects.
        node = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS].next;
        numCollisions += find_wall_collisions_from_list(node, colData);
    }

    // Check for surfaces that are a part of level geometry.
    node = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS].next;
    numCollisions += find_wall_collisions_from_list(node, colData);

    gCollisionFlags &= ~(COLLISION_FLAG_RETURN_FIRST | COLLISION_FLAG_EXCLUDE_DYNAMIC | COLLISION_FLAG_INCLUDE_INTANGIBLE);
#ifdef VANILLA_DEBUG
    // Increment the debug tracker.
    gNumCalls.wall++;
#endif
#if PUPPYPRINT_DEBUG
    collisionTime[perfIteration] += osGetTime() - first;
#endif

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

void add_ceil_margin(s32 *x, s32 *z, Vec3s target1, Vec3s target2, f32 margin) {
    register f32 diff_x = target1[0] - *x + target2[0] - *x;
    register f32 diff_z = target1[2] - *z + target2[2] - *z;
    register f32 invDenom = margin / sqrtf(sqr(diff_x) + sqr(diff_z));

    *x += diff_x * invDenom;
    *z += diff_z * invDenom;
}

static s32 check_within_ceil_triangle_bounds(s32 x, s32 z, struct Surface *surf, f32 margin) {
    s32 addMargin = surf->type != SURFACE_HANGABLE && !FLT_IS_NONZERO(margin);
    Vec3i vx, vz;
    vx[0] = surf->vertex1[0];
    vz[0] = surf->vertex1[2];
    if (addMargin) add_ceil_margin(&vx[0], &vz[0], surf->vertex2, surf->vertex3, margin);

    vx[1] = surf->vertex2[0];
    vz[1] = surf->vertex2[2];
    if (addMargin) add_ceil_margin(&vx[1], &vz[1], surf->vertex3, surf->vertex1, margin);

    // Checking if point is in bounds of the triangle laterally.
    if (((vz[0] - z) * (vx[1] - vx[0]) - (vx[0] - x) * (vz[1] - vz[0])) > 0) return FALSE;

    // Slight optimization by checking these later.
    vx[2] = surf->vertex3[0];
    vz[2] = surf->vertex3[2];
    if (addMargin) add_ceil_margin(&vx[2], &vz[2], surf->vertex1, surf->vertex2, margin);

    if (((vz[1] - z) * (vx[2] - vx[1]) - (vx[1] - x) * (vz[2] - vz[1])) > 0) return FALSE;
    if (((vz[2] - z) * (vx[0] - vx[2]) - (vx[2] - x) * (vz[0] - vz[2])) > 0) return FALSE;

    return TRUE;
}

/**
 * Iterate through the list of ceilings and find the first ceiling over a given point.
 */
static struct Surface *find_ceil_from_list(struct SurfaceNode *surfaceNode, s32 x, s32 y, s32 z, f32 *pheight) {
    register struct Surface *surf, *ceil = NULL;
    register f32 height;
    SurfaceType type = SURFACE_DEFAULT;
    *pheight = CELL_HEIGHT_LIMIT;
    // Stay in this loop until out of ceilings.
    while (surfaceNode != NULL) {
        surf = surfaceNode->surface;
        surfaceNode = surfaceNode->next;
        type = surf->type;

        // Exclude all ceilings below the point
        if (y > surf->upperY) continue;

        // Determine if checking for the camera or not
        if (gCollisionFlags & COLLISION_FLAG_CAMERA) {
            if (surf->flags & SURFACE_FLAG_NO_CAM_COLLISION) {
                continue;
            }
        } else if (type == SURFACE_CAMERA_BOUNDARY) {
            // Ignore camera only surfaces
            continue;
        }

        // Check that the point is within the triangle bounds
        if (!check_within_ceil_triangle_bounds(x, z, surf, 1.5f)) continue;

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
        if (height == y || (gCollisionFlags & COLLISION_FLAG_RETURN_FIRST)) break;
    }
    return ceil;
}

/**
 * Find the lowest ceiling above a given position and return the height.
 */
f32 find_ceil(f32 posX, f32 posY, f32 posZ, struct Surface **pceil) {
    f32 height        = CELL_HEIGHT_LIMIT;
    f32 dynamicHeight = CELL_HEIGHT_LIMIT;
#if PUPPYPRINT_DEBUG
    OSTime first = osGetTime();
#endif
    s32 x = posX;
    s32 y = posY;
    s32 z = posZ;
    *pceil = NULL;

    if (is_outside_level_bounds(x, z)) {
#if PUPPYPRINT_DEBUG
        collisionTime[perfIteration] += (osGetTime() - first);
#endif
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
#if PUPPYPRINT_DEBUG
    collisionTime[perfIteration] += osGetTime() - first;
#endif

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

    if (((vz[0] - z) * (vx[1] - vx[0]) - (vx[0] - x) * (vz[1] - vz[0])) < 0) return FALSE;

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
    register struct Surface *surf, *floor = NULL;
    register SurfaceType type = SURFACE_DEFAULT;
    register f32 height;
    register s32 bufferY = y + FIND_FLOOR_BUFFER;

    // Iterate through the list of floors until there are no more floors.
    while (surfaceNode != NULL) {
        surf = surfaceNode->surface;
        surfaceNode = surfaceNode->next;
        type        = surf->type;

        // To prevent the Merry-Go-Round room from loading when Mario passes above the hole that leads
        // there, SURFACE_INTANGIBLE is used. This prevent the wrong room from loading, but can also allow
        // Mario to pass through.
        if (!(gCollisionFlags & COLLISION_FLAG_INCLUDE_INTANGIBLE) && (type == SURFACE_INTANGIBLE)) {
            continue;
        }

        // Determine if we are checking for the camera or not.
        if (gCollisionFlags & COLLISION_FLAG_CAMERA) {
            if (surf->flags & SURFACE_FLAG_NO_CAM_COLLISION) {
                continue;
            }
        } else if (type == SURFACE_CAMERA_BOUNDARY) {
            continue; // If we are not checking for the camera, ignore camera only floors.
        }

        // Exclude all floors above the point.
        if (bufferY < surf->lowerY) continue;
        // Check that the point is within the triangle bounds.
        if (!check_within_floor_triangle_bounds(x, z, surf)) continue;

        // Get the height of the floor under the current location.
        height = get_surface_height_at_location(x, z, surf);

        // Exclude floors lower than the previous highest floor.
        if (height < *pheight) continue;

        // Checks for floor interaction with a FIND_FLOOR_BUFFER unit buffer.
        if (bufferY < height) continue;

        // Use the current floor
        *pheight = height;
        floor = surf;

        // Exit the loop if it's not possible for another floor to be closer
        // to the original point, or if COLLISION_FLAG_RETURN_FIRST.
        if ((height == bufferY) || (gCollisionFlags & COLLISION_FLAG_RETURN_FIRST)) break;
    }
    return floor;
}

// Generic triangle bounds func
ALWAYS_INLINE static s32 check_within_bounds_y_norm(s32 x, s32 z, struct Surface *surf) {
    if (surf->normal.y >= NORMAL_FLOOR_THRESHOLD) return check_within_floor_triangle_bounds(x, z, surf);
    return check_within_ceil_triangle_bounds(x, z, surf, 0);
}

/**
 * Iterate through the list of water floors and find the first water floor under a given point.
 */
struct Surface *find_water_floor_from_list(struct SurfaceNode *surfaceNode, s32 x, s32 y, s32 z, f32 *pheight) {
    register struct Surface *surf;
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
    s32 y = yPos;
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
#if PUPPYPRINT_DEBUG
    OSTime first = osGetTime();
#endif

    f32 height        = FLOOR_LOWER_LIMIT;
    f32 dynamicHeight = FLOOR_LOWER_LIMIT;

    //! (Parallel Universes) Because position is casted to an s16, reaching higher
    //  float locations can return floors despite them not existing there.
    //  (Dynamic floors will unload due to the range.)
    s32 x = xPos;
    s32 y = yPos;
    s32 z = zPos;

    *pfloor = NULL;

    if (is_outside_level_bounds(x, z)) {
#if PUPPYPRINT_DEBUG
        collisionTime[perfIteration] += (osGetTime() - first);
#endif
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
#if PUPPYPRINT_DEBUG
    collisionTime[perfIteration] += (osGetTime() - first);
#endif
    return height;
}

f32 find_room_floor(f32 x, f32 y, f32 z, struct Surface **pfloor) {
    gCollisionFlags |= (COLLISION_FLAG_RETURN_FIRST | COLLISION_FLAG_EXCLUDE_DYNAMIC | COLLISION_FLAG_INCLUDE_INTANGIBLE);
    return find_floor(x, y, z, pfloor);
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
#if PUPPYPRINT_DEBUG
    OSTime first = osGetTime();
#endif
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

#if PUPPYPRINT_DEBUG
    collisionTime[perfIteration] += (osGetTime() - first);
#endif
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
#if PUPPYPRINT_DEBUG
    OSTime first = osGetTime();
#endif
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
            if (loX < x && x < hiX && loZ < z && z < hiZ && val < 50) {
                // Set the water height. Since this breaks, only return the first height.
                waterLevel = *p;
                break;
            }
            p++;
        }
    }

#if PUPPYPRINT_DEBUG
    collisionTime[perfIteration] += osGetTime() - first;
#endif

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
#if PUPPYPRINT_DEBUG
    OSTime first = osGetTime();
#endif

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

#if PUPPYPRINT_DEBUG
    collisionTime[perfIteration] += osGetTime() - first;
#endif

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

/**************************************************
 *                    RAYCASTING                  *
 **************************************************/

#define RAY_OFFSET 30.0f /* How many units to extrapolate surfaces when testing for a raycast */
#define RAY_STEPS      4 /* How many steps to do when casting rays, default to quartersteps.  */

/**
 * @brief Checks if a ray intersects a surface using Möller–Trumbore intersection algorithm.
 * 
 * @param orig is the starting point of the ray.
 * @param dir is the normalized ray direction.
 * @param dir_length is the length of the ray.
 * @param surface is the surface to check.
 * @param hit_pos returns the position on the surface where the ray intersects it.
 * @param length returns the distance from the starting point to the hit position.
 * @return s32 TRUE if the ray intersects a surface.
 */
s32 ray_surface_intersect(Vec3f orig, Vec3f dir, f32 dir_length, struct Surface *surface, Vec3f hit_pos, f32 *length) {
    // Ignore certain surface types.
    if ((surface->type == SURFACE_INTANGIBLE) || (surface->flags & SURFACE_FLAG_NO_CAM_COLLISION)) return FALSE;
    // Convert the vertices to Vec3f.
    Vec3f v0, v1, v2;
    vec3s_to_vec3f(v0, surface->vertex1);
    vec3s_to_vec3f(v1, surface->vertex2);
    vec3s_to_vec3f(v2, surface->vertex3);
    // Get surface normal and extend it by RAY_OFFSET.
    Vec3f norm;
    surface_normal_to_vec3f(norm, surface);
    vec3_mul_val(norm, RAY_OFFSET);
    // Move the face forward by RAY_OFFSET.
    vec3f_add(v0, norm);
    vec3f_add(v1, norm);
    vec3f_add(v2, norm);
    // Make 'e1' (edge 1) the vector from vertex 0 to vertex 1.
    Vec3f e1;
    vec3f_diff(e1, v1, v0);
    // Make 'e2' (edge 2) the vector from vertex 0 to vertex 2.
    Vec3f e2;
    vec3f_diff(e2, v2, v0);
    // Make 'h' the cross product of 'dir' and edge 2.
    Vec3f h;
    vec3f_cross(h, dir, e2);
    // Determine the cos(angle) difference between ray and surface normals.
    f32 det = vec3f_dot(e1, h);
    // Check if we're perpendicular from the surface.
    if ((det > -NEAR_ZERO) && (det < NEAR_ZERO)) return FALSE;
    // Check if we're making contact with the surface.
    // Make f the inverse of the cos(angle) between ray and surface normals.
    f32 f = 1.0f / det; // invDet
    // Make 's' the vector from vertex 0 to 'orig'.
    Vec3f s;
    vec3f_diff(s, orig, v0);
    // Make 'u' the cos(angle) between vectors 's' and normals, divided by 'det'.
    f32 u = f * vec3f_dot(s, h);
    // Check if 'u' is within bounds.
    if ((u < 0.0f) || (u > 1.0f)) return FALSE;
    // Make 'q' the cross product of 's' and edge 1. 
    Vec3f q;
    vec3f_cross(q, s, e1);
    // Make 'v' the cos(angle) between the ray and 'q', divided by 'det'.
    f32 v = f * vec3f_dot(dir, q);
    // Check if 'v' is within bounds.
    if ((v < 0.0f) || ((u + v) > 1.0f)) return FALSE;
    // Get the length between our origin and the surface contact point.
    // Make '*length' the cos(angle) betqwwn edge 2 and 'q', divided by 'det'.
    *length = f * vec3f_dot(e2, q);
    // Check if the length to the hit point is shorter than the ray length.
    if ((*length <= NEAR_ZERO) || (*length > dir_length)) return FALSE;
    // Successful contact.
    // Make 'add_dir' into 'dir' scaled by 'length'.
    Vec3f add_dir;
    vec3_prod_val(add_dir, dir, *length);
    // Make 'hit_pos' into the sum of 'orig' and 'add_dir'.
    vec3f_sum(hit_pos, orig, add_dir);
    return TRUE;
}

void find_surface_on_ray_list(struct SurfaceNode *list, Vec3f orig, Vec3f dir, f32 dir_length, struct Surface **hit_surface, Vec3f hit_pos, f32 *max_length) {
    s32 hit;
    f32 length;
    Vec3f chk_hit_pos;
    f32 top, bottom;
#if PUPPYPRINT_DEBUG
    OSTime first = osGetTime();
#endif
    // Get upper and lower bounds of ray
    if (dir[1] >= 0.0f) {
        // Ray is upwards.
        top    = orig[1] + (dir[1] * dir_length);
        bottom = orig[1];
    } else {
        // Ray is downwards.
        top    = orig[1];
        bottom = orig[1] + (dir[1] * dir_length);
    }

    // Iterate through every surface of the list
    for (; list != NULL; list = list->next) {
        // Reject surface if out of vertical bounds
        if ((list->surface->lowerY > top) || (list->surface->upperY < bottom)) continue;
        // Check intersection between the ray and this surface
        hit = ray_surface_intersect(orig, dir, dir_length, list->surface, chk_hit_pos, &length);
        if (hit && (length <= *max_length)) {
            *hit_surface = list->surface;
            vec3f_copy(hit_pos, chk_hit_pos);
            *max_length = length;
        }
    }
#if PUPPYPRINT_DEBUG
    collisionTime[perfIteration] += osGetTime() - first;
#endif
}

void find_surface_on_ray_cell(s32 cellX, s32 cellZ, Vec3f orig, Vec3f normalized_dir, f32 dir_length, struct Surface **hit_surface, Vec3f hit_pos, f32 *max_length, s32 flags) {
    // Skip if OOB
    if ((cellX >= 0) && (cellX <= (NUM_CELLS - 1)) && (cellZ >= 0) && (cellZ <= (NUM_CELLS - 1))) {
        // Iterate through each surface in this partition
        if ((normalized_dir[1] > -NEAR_ONE) && (flags & RAYCAST_FIND_CEIL)) {
            find_surface_on_ray_list( gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_CEILS ].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
            find_surface_on_ray_list(gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_CEILS ].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
        }
        if ((normalized_dir[1] <  NEAR_ONE) && (flags & RAYCAST_FIND_FLOOR)) {
            find_surface_on_ray_list( gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_FLOORS].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
            find_surface_on_ray_list(gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_FLOORS].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
        }
        if (flags & RAYCAST_FIND_WALL) {
            find_surface_on_ray_list( gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS ].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
            find_surface_on_ray_list(gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS ].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
        }
        if (flags & RAYCAST_FIND_WATER) {
            find_surface_on_ray_list( gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WATER ].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
            find_surface_on_ray_list(gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WATER ].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
        }
    }
}

void find_surface_on_ray(Vec3f orig, Vec3f dir, struct Surface **hit_surface, Vec3f hit_pos, s32 flags) {
    Vec3f normalized_dir;
    f32 step;
    s32 i;
    const f32 invcell = 1.0f / CELL_SIZE;

    // Set that no surface has been hit
    *hit_surface = NULL;
    vec3f_sum(hit_pos, orig, dir);

    // Get normalized direction
    f32 dir_length = vec3_mag(dir);
    f32 max_length = dir_length;
    vec3f_copy(normalized_dir, dir);
    vec3f_normalize(normalized_dir);

    // Get our cell coordinate
    f32 fCellX    = (orig[0] + LEVEL_BOUNDARY_MAX) * invcell;
    f32 fCellZ    = (orig[2] + LEVEL_BOUNDARY_MAX) * invcell;
    s32 cellX     = fCellX;
    s32 cellZ     = fCellZ;
    s32 cellPrevX = cellX;
    s32 cellPrevZ = cellZ;

    // Don't do DDA if straight down
    if ((normalized_dir[1] >= NEAR_ONE) || (normalized_dir[1] <= -NEAR_ONE)) {
        find_surface_on_ray_cell(cellX, cellZ, orig, normalized_dir, dir_length, hit_surface, hit_pos, &max_length, flags);
        return;
    }

    // Get cells we cross using DDA
    f32 absDir0 = absf(dir[0]);
    f32 absDir2 = absf(dir[2]);
    if (absDir0 >= absDir2) {
        step = (RAY_STEPS * absDir0) * invcell;
    } else {
        step = (RAY_STEPS * absDir2) * invcell;
    }

    f32 dx = (dir[0] / step) * invcell;
    f32 dz = (dir[2] / step) * invcell;

    for (i = 0; i < step && *hit_surface == NULL; i++) {
        find_surface_on_ray_cell(cellX, cellZ, orig, normalized_dir, dir_length, hit_surface, hit_pos, &max_length, flags);

        // Move cell coordinate
        fCellX   += dx;
        fCellZ   += dz;
        cellPrevX = cellX;
        cellPrevZ = cellZ;
        cellX     = fCellX;
        cellZ     = fCellZ;

        if ((cellPrevX != cellX) && (cellPrevZ != cellZ)) {
            find_surface_on_ray_cell(cellX, cellPrevZ, orig, normalized_dir, dir_length, hit_surface, hit_pos, &max_length, flags);
            find_surface_on_ray_cell(cellPrevX, cellZ, orig, normalized_dir, dir_length, hit_surface, hit_pos, &max_length, flags);
        }
    }
}
