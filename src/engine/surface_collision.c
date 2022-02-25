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
 *                   GENERAL USE                  *
 **************************************************/

/**
 * Checks whether the coords are within level boundaries laterally.
 */
s32 is_outside_level_bounds(s32 xPos, s32 zPos) {
    return ((xPos <= -LEVEL_BOUNDARY_MAX)
         || (xPos >=  LEVEL_BOUNDARY_MAX)
         || (zPos <= -LEVEL_BOUNDARY_MAX)
         || (zPos >=  LEVEL_BOUNDARY_MAX));
}

/**
 * Converts a single coordinate value into the corresponding cell coordinate value on the same axis.
 */
s32 get_cell_coord(s32 coord) {
    return (((coord + LEVEL_BOUNDARY_MAX) / CELL_SIZE) % NUM_CELLS);
}

/**
 * Gets the height of a point at 'xPos' and 'zPos' coplanar to the triangle of 'surf'.
 */
f32 get_surface_height_at_pos(f32 xPos, f32 zPos, struct Surface *surf) {
    f32 nx = surf->normal.x;
    f32 ny = surf->normal.y;
    f32 nz = surf->normal.z;
    f32 oo = surf->originOffset;

    return -((xPos * nx) + (zPos * nz) + oo) / ny;
}

/**
 * Gets the yaw facing angle of a surface.
 */
s32 get_surface_yaw(struct Surface *surf) {
    return (s16)atan2s(surf->normal.z, surf->normal.x);
}

/**************************************************
 *                      WALLS                     *
 **************************************************/

static s32 check_wall_triangle_vw(f32 d00, f32 d01, f32 d11, f32 d20, f32 d21, f32 invDenom) {
    f32 v = ((d11 * d20) - (d01 * d21)) * invDenom;
    if (v < 0.0f || v > construct_float(1.0f)) {
        return TRUE;
    }

    f32 w = ((d00 * d21) - (d01 * d20)) * invDenom;
    if (w < 0.0f || w > construct_float(1.0f) || v + w > construct_float(1.0f)) {
        return TRUE;
    }

    return FALSE;
}

s32 check_wall_triangle_edge(Vec3f vert, Vec3f v2, f32 *d00, f32 *d01, f32 *invDenom, f32 *offset, f32 margin_radius) {
    f32 y = vert[1];

    if (FLT_IS_NONZERO(y)) {
        f32 v = (v2[1] / y);
        if (v < 0.0f || v > construct_float(1.0f)) {
            return TRUE;
        }

        *d00 = (vert[0] * v) - v2[0];
        *d01 = (vert[2] * v) - v2[2];
        *invDenom = sqrtf(sqr(*d00) + sqr(*d01));
        *offset = *invDenom - margin_radius;

        return (*offset > 0.0f);
    }

    return TRUE;
}

/**
 * Iterate through the list of walls until all walls are checked and
 * have given their wall push.
 */
static s32 find_wall_collisions_from_list(struct SurfaceNode *surfaceNode, struct WallCollisionData *data) {
    const f32 corner_threshold = construct_float(-0.9f);
    struct Surface *surf;

    Vec3f pos = { data->x, (data->y + data->offsetY), data->z };
    f32 offset;
    f32 radius = data->radius;
    s32 numCols = 0;

    f32 nx, ny, nz, oo;
    Vec3f v0, v1, v2;
    f32 d00, d01, d11, d20, d21;
    f32 invDenom;

    s32 returnFirst = (gCollisionFlags & COLLISION_FLAG_RETURN_FIRST);
    s32 checkingForCamera = (gCollisionFlags & COLLISION_FLAG_CAMERA);

    struct Object *obj = o;
    s32 passVanishWalls = ((obj != NULL)
     && ((obj->activeFlags & ACTIVE_FLAG_MOVE_THROUGH_GRATE) ||
         ((obj == gMarioObject) && (gMarioState->flags & MARIO_VANISH_CAP)))
    );

    TerrainData type = SURFACE_DEFAULT;

    if (radius > MAX_COLLISION_RADIUS) {
        radius = MAX_COLLISION_RADIUS;
    }

    f32 margin_radius = radius - construct_float(1.0f);

    // Stay in this loop until out of walls.
    while (surfaceNode != NULL) {
        surf = surfaceNode->surface;
        surfaceNode = surfaceNode->next;
        type = surf->type;

        // Exclude a large number of walls immediately to optimize.
        if (pos[1] < surf->lowerY || pos[1] > surf->upperY) continue;

        // Determine if checking for the camera or not.
        if (checkingForCamera) {
            // Certain surfaces are ignored by the camera.
            if (surf->flags & SURFACE_FLAG_NO_CAM_COLLISION) continue;
        } else {
            // Ignore camera only surfaces.
            if (type == SURFACE_CAMERA_BOUNDARY) continue;

            // If an object can pass through a vanish cap wall, pass through.
            if (passVanishWalls && (type == SURFACE_VANISH_CAP_WALLS)) continue;
        }

        // Read surface data
        nx = surf->normal.x;
        ny = surf->normal.y;
        nz = surf->normal.z;
        oo = surf->originOffset;

        // Dot of normal and pos, + origin offset.
        //! TODO: Is 'offset' just the distance from 'pos' to the triangle?
        offset = (nx * pos[0]) + (ny * pos[1]) + (nz * pos[2]) + oo;

        // Exclude surfaces outside of the radius.
        if (offset < -radius || offset > radius) continue;

        // Edge 1 vector.
        vec3_diff(v0, surf->vertex2, surf->vertex1);
        // Edge 2 vector.
        vec3_diff(v1, surf->vertex3, surf->vertex1);
        // Vector from vertex 1 to pos.
        vec3_diff(v2, pos, surf->vertex1);

        // Face dot products.
        d00 = vec3_dot(v0, v0);
        d01 = vec3_dot(v0, v1);
        d11 = vec3_dot(v1, v1);
        d20 = vec3_dot(v2, v0);
        d21 = vec3_dot(v2, v1);

        // Inverse denom.
        invDenom = (d00 * d11) - (d01 * d01);
        if (FLT_IS_NONZERO(invDenom)) {
            invDenom = (construct_float(1.0f) / invDenom);
        }

        if (check_wall_triangle_vw(d00, d01, d11, d20, d21, invDenom)) {
            // Skip if behind surface.
            if (offset < 0) {
                continue;
            }

            // Edge 1-2.
            if (check_wall_triangle_edge(v0, v2, &d00, &d01, &invDenom, &offset, margin_radius)) {
                // Edge 1-3.
                if (check_wall_triangle_edge(v1, v2, &d00, &d01, &invDenom, &offset, margin_radius)) {
                    // Edge 3 vector.
                    vec3_diff(v1, surf->vertex3, surf->vertex2);
                    // Vector from vertex 2 to pos.
                    vec3_diff(v2, pos, surf->vertex2);
                    // Edge 2-3.
                    if (check_wall_triangle_edge(v1, v2, &d00, &d01, &invDenom, &offset, margin_radius)) {
                        continue;
                    }
                }
            }

            // Check collision.
            if (FLT_IS_NONZERO(invDenom)) {
                invDenom = (offset / invDenom);
            }

            // Update pos.
            pos[0] += (d00 *= invDenom);
            pos[2] += (d01 *= invDenom);

            if ((d00 * nx) + (d01 * nz) < (corner_threshold * offset)) {
                continue;
            }
        } else {
            // Update pos.
            pos[0] += nx * (radius - offset);
            pos[2] += nz * (radius - offset);
        }

        // The surface has collision.
        if (data->numWalls < MAX_REFERENCED_WALLS) {
            data->walls[data->numWalls++] = surf;
        }
        numCols++;

        // If COLLISION_FLAG_RETURN_FIRST, return the first wall.
        if (returnFirst) {
            break;
        }
    }

    // Update the collisionData's position.
    data->x = pos[0];
    data->z = pos[2];

    // Return the number of wall collisions.
    return numCols;
}

/**
 * Find wall collisions and receive their push.
 */
s32 find_wall_collisions(struct WallCollisionData *colData) {
#if PUPPYPRINT_DEBUG
    OSTime first = osGetTime();
#endif
    struct SurfaceNode *node;
    s32 numCollisions = 0;
    s32 x = colData->x;
    s32 z = colData->z;

    colData->numWalls = 0;

    if (is_outside_level_bounds(x, z)) {
#if PUPPYPRINT_DEBUG
        collisionTime[perfIteration] += osGetTime() - first;
#endif
        return numCollisions;
    }

    s32 radius = colData->radius;

    // World (level) consists of a 16x16 grid. Find where the collision is on the grid (round toward -inf)
    s32 cellX, cellZ;
    s32 minCellX = get_cell_coord(x - radius);
    s32 maxCellX = get_cell_coord(x + radius);
    s32 minCellZ = get_cell_coord(z - radius);
    s32 maxCellZ = get_cell_coord(z + radius);

    for (cellZ = minCellZ; cellZ <= maxCellZ; cellZ++) {
        for (cellX = minCellX; cellX <= maxCellX; cellX++) {
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

#if PUPPYPRINT_DEBUG
    collisionTime[perfIteration] += osGetTime() - first;
#endif
    return numCollisions;
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

/**
 * Send a raycast from pos to intendedPos shifted upward by yOffset,
 * and set intendedPos to the collided point if the ray reaches a wall.
 */
void raycast_collision_walls(Vec3f pos, Vec3f intendedPos, f32 yOffset) {
    UNUSED struct Surface *surf;
    Vec3f dir;

    // Get the vector from pos to the original intendedPos.
    vec3f_diff(dir, intendedPos, pos);

    // Shift the source pos upward by yOffset.
    pos[1] += yOffset;

    // Send the raycast and find the new pos.
    find_surface_on_ray(pos, dir, &surf, intendedPos, RAYCAST_FIND_WALL);

    // Shift pos and intendedPos back down.
    pos[1]         -= yOffset;
    intendedPos[1] -= yOffset;
}

/**************************************************
 *                     CEILINGS                   *
 **************************************************/

void add_ceil_margin(s32 *x, s32 *z, Vec3s target1, Vec3s target2, f32 margin) {
    f32 dx = ((target1[0] - *x) + (target2[0] - *x));
    f32 dz = ((target1[2] - *z) + (target2[2] - *z));
    f32 invDenom = (sqr(dx) + sqr(dz));

    if (FLT_IS_NONZERO(invDenom)) {
        invDenom = (margin / sqrtf(invDenom));

        *x += (dx * invDenom);
        *z += (dz * invDenom);
    }
}

static s32 check_within_ceil_triangle_bounds(s32 x, s32 z, struct Surface *surf, f32 margin) {
    s32 addMargin = (FLT_IS_NONZERO(margin) && (surf->type != SURFACE_HANGABLE));
    Vec3i vx, vz;

    vx[0] = surf->vertex1[0];
    vz[0] = surf->vertex1[2];

    // Expand vertex 1 in the opposite direction of vertices 2 and 3
    if (addMargin) add_ceil_margin(&vx[0], &vz[0], surf->vertex2, surf->vertex3, margin);

    vx[1] = surf->vertex2[0];
    vz[1] = surf->vertex2[2];

    // Expand vertex 2 in the opposite direction of vertices 3 and 1
    if (addMargin) add_ceil_margin(&vx[1], &vz[1], surf->vertex3, surf->vertex1, margin);

    // Checking if point is in bounds of the triangle laterally.
    if (((vz[0] - z) * (vx[1] - vx[0]) - (vx[0] - x) * (vz[1] - vz[0])) > 0) return FALSE;

    // Slight optimization by checking these later.
    vx[2] = surf->vertex3[0];
    vz[2] = surf->vertex3[2];

    // Expand vertex 3 in the opposite direction of vertices 1 and 2
    if (addMargin) add_ceil_margin(&vx[2], &vz[2], surf->vertex1, surf->vertex2, margin);

    if (((vz[1] - z) * (vx[2] - vx[1]) - (vx[1] - x) * (vz[2] - vz[1])) > 0) return FALSE;
    if (((vz[2] - z) * (vx[0] - vx[2]) - (vx[2] - x) * (vz[0] - vz[2])) > 0) return FALSE;

    return TRUE;
}

/**
 * Iterate through the list of ceilings and find the first ceiling over a given point.
 */
static struct Surface *find_ceil_from_list(struct SurfaceNode *surfaceNode, s32 x, s32 y, s32 z, f32 *pheight) {
    const f32 margin = construct_float(1.5f);
    struct Surface *surf, *ceil = NULL;
    f32 height, lowest = *pheight;

    s32 returnFirst = (gCollisionFlags & COLLISION_FLAG_RETURN_FIRST);
    s32 checkingForCamera = (gCollisionFlags & COLLISION_FLAG_CAMERA);

    SurfaceType type = SURFACE_DEFAULT;

    // Iterate through the list until there are no more ceilings.
    while (surfaceNode != NULL) {
        surf = surfaceNode->surface;
        surfaceNode = surfaceNode->next;
        type = surf->type;

        // Determine if checking for the camera or not
        if (checkingForCamera) {
            if (surf->flags & SURFACE_FLAG_NO_CAM_COLLISION) {
                continue;
            }
        } else if (type == SURFACE_CAMERA_BOUNDARY) {
            // Ignore camera only surfaces
            continue;
        }

        // Exclude all ceilings below the point
        if (y > surf->upperY) continue;

        // Check that the point is within the triangle bounds
        if (!check_within_ceil_triangle_bounds(x, z, surf, margin)) continue;

        // Find the height of the ceil at the given location
        height = get_surface_height_at_pos(x, z, surf);

        // Exclude ceilings lower than the check height.
        if (height < y) continue;

        // Exclude ceilings higher than the previous lowest ceiling
        if (height > lowest) continue;

        // Use the current ceiling
        lowest = height;
        ceil = surf;

#ifdef SLOPE_FIX
        // Exit the loop if it's not possible for another ceiling to be closer
        // to the original point, or if COLLISION_FLAG_RETURN_FIRST.
        if (returnFirst) {
            break;
        }
#else
        break;
#endif
    }

    *pheight = lowest;

    return ceil;
}

/**
 * Find the lowest ceiling above a given position and return the height.
 */
f32 find_ceil(f32 posX, f32 posY, f32 posZ, struct Surface **pceil) {
#if PUPPYPRINT_DEBUG
    OSTime first = osGetTime();
#endif
    f32 height        = construct_float(CELL_HEIGHT_LIMIT);
    f32 dynamicHeight = construct_float(CELL_HEIGHT_LIMIT);

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
    s32 cellX = get_cell_coord(x);
    s32 cellZ = get_cell_coord(z);

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
    struct Surface *surf, *floor = NULL;
    f32 height, highest = *pheight;

    s32 returnFirst = (gCollisionFlags & COLLISION_FLAG_RETURN_FIRST);
    s32 checkingForCamera = (gCollisionFlags & COLLISION_FLAG_CAMERA);
    s32 skipIntangible = !(gCollisionFlags & COLLISION_FLAG_INCLUDE_INTANGIBLE);

    SurfaceType type = SURFACE_DEFAULT;

    // Iterate through the list until there are no more floors.
    while (surfaceNode != NULL) {
        surf = surfaceNode->surface;
        surfaceNode = surfaceNode->next;
        type = surf->type;

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

        // Exclude all floors whose lowest vertex is above the point.
        if (y < surf->lowerY) continue;

        // Check that the point is within the triangle bounds.
        if (!check_within_floor_triangle_bounds(x, z, surf)) continue;

        // Get the exact height of the floor under the current location.
        height = get_surface_height_at_pos(x, z, surf);

        // Exclude floors higher than the check height.
        if (height > y) continue;

        // Exclude floors lower than the previous highest floor.
        if (height < highest) continue;

        // Use the current floor
        highest = height;
        floor = surf;

#ifdef SLOPE_FIX
        // Exit the loop if it's not possible for another floor to be closer
        // to the original point, or if COLLISION_FLAG_RETURN_FIRST.
        if (returnFirst) {
            break;
        }
#else
        break;
#endif
    }

    *pheight = highest;

    return floor;
}

/**
 * Generic triangle bounds function.
 */
ALWAYS_INLINE static s32 check_within_bounds_y_norm(s32 x, s32 z, struct Surface *surf) {
    if (surf->normal.y >= construct_float(NORMAL_FLOOR_THRESHOLD)) {
        return check_within_floor_triangle_bounds(x, z, surf);
    } else {
        return check_within_ceil_triangle_bounds(x, z, surf, 0);
    }
}

/**
 * Iterate through the list of water surfaces and find the first water bottom above a given point.
 */
struct Surface *find_water_bottom_from_list(struct SurfaceNode *surfaceNode, s32 x, s32 y, s32 z, f32 *pheight) {
    struct SurfaceNode *currSurfaceNode = surfaceNode;
    struct Surface *surf, *waterBottom = NULL;
    f32 height, lowest = *pheight;
    f32 bufferY = (y + MARIO_HITBOX_HEIGHT);

    while (currSurfaceNode != NULL) {
        surf = currSurfaceNode->surface;
        currSurfaceNode = currSurfaceNode->next;

        // Only check water bottoms.
        if (surf->type != SURFACE_NEW_WATER_BOTTOM) continue;

        // Skip wall angled surfaces.
        if (absf(surf->normal.y) < construct_float(NORMAL_FLOOR_THRESHOLD)) continue;

        // Check that the point is within the triangle bounds.
        if (!check_within_bounds_y_norm(x, z, surf)) continue;

        // Get the exact height of the water under the current location.
        height = get_surface_height_at_pos(x, z, surf);

        // Skip surfaces below the point.
        if (height < bufferY) continue;

        // Skip surfaces above the previous lowest.
        if (height > lowest) continue;

        lowest = height;
        waterBottom = surf;
    }

    *pheight = lowest;

    return waterBottom;
}

/**
 * Iterate through the list of water surfaces and find the highest water top at a given point, unless there is a water bottom under it.
 */
struct Surface *find_water_top_from_list(struct SurfaceNode *surfaceNode, s32 x, s32 bottomHeight, s32 z, f32 *pheight) {
    struct SurfaceNode *currSurfaceNode = surfaceNode;
    struct Surface *surf, *waterTop = NULL;
    f32 height, highest = *pheight;

    s32 hasBottom = (bottomHeight != construct_float(CELL_HEIGHT_LIMIT));

    while (currSurfaceNode != NULL) {
        surf = currSurfaceNode->surface;
        currSurfaceNode = currSurfaceNode->next;

        // Only check water tops.
        if (surf->type != SURFACE_NEW_WATER_TOP) continue;

        // Skip wall angled surfaces.
        if (absf(surf->normal.y) < construct_float(NORMAL_FLOOR_THRESHOLD)) continue;

        // Check that the point is within the triangle bounds.
        if (!check_within_bounds_y_norm(x, z, surf)) continue;

        // Get the exact height of the surface under the current location.
        height = get_surface_height_at_pos(x, z, surf);

        // If the water has a bottom, skip water tops higher than it.
        if (hasBottom && height > bottomHeight) continue;

        // Skip surfaces lower than the previous highest.
        if (height < highest) continue;

        highest = height;
        waterTop = surf;
    }

    *pheight = highest;

    return waterTop;
}

/**
 * Find the height of the highest floor below a point.
 */
f32 find_floor_height(f32 x, f32 y, f32 z) {
    struct Surface *floor;
    return find_floor(x, y, z, &floor);
}

/**
 * Find the highest floor under a given position and return the height.
 */
f32 find_floor(f32 xPos, f32 yPos, f32 zPos, struct Surface **pfloor) {
#if PUPPYPRINT_DEBUG
    OSTime first = osGetTime();
#endif
    f32 height        = construct_float(FLOOR_LOWER_LIMIT);
    f32 dynamicHeight = construct_float(FLOOR_LOWER_LIMIT);

    s32 x = xPos;
    s32 y = yPos + FIND_FLOOR_BUFFER;
    s32 z = zPos;

    *pfloor = NULL;

    if (is_outside_level_bounds(x, z)) {
#if PUPPYPRINT_DEBUG
        collisionTime[perfIteration] += (osGetTime() - first);
#endif
        return height;
    }

    // Each level is split into cells to limit load, find the appropriate cell.
    s32 cellX = get_cell_coord(x);
    s32 cellZ = get_cell_coord(z);

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

s32 get_room_at_pos(f32 x, f32 y, f32 z) {
    if (gCurrentArea->surfaceRooms != NULL) {
        struct Surface *floor;
        gCollisionFlags |= (COLLISION_FLAG_RETURN_FIRST | COLLISION_FLAG_EXCLUDE_DYNAMIC | COLLISION_FLAG_INCLUDE_INTANGIBLE);
        find_floor(x, y, z, &floor);
        if (floor) {
            return floor->room;
        }
    }
    return -1;
}

/**
 * Find the highest water floor under a given position and return the height.
 */
f32 find_water_floor(s32 xPos, s32 yPos, s32 zPos, struct Surface **pfloor) {
    const f32 floorLowerLimit = construct_float(FLOOR_LOWER_LIMIT);
    const f32 cellHeightLimit = construct_float(CELL_HEIGHT_LIMIT);
    f32 height = floorLowerLimit;

    s32 x = xPos;
    s32 y = yPos;
    s32 z = zPos;

    if (is_outside_level_bounds(x, z)) {
        return height;
    }

    // Each level is split into cells to limit load, find the appropriate cell.
    s32 cellX = get_cell_coord(x);
    s32 cellZ = get_cell_coord(z);

    struct SurfaceNode *dynamicWater = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WATER].next;
    struct SurfaceNode *staticWater = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WATER].next;

    struct SurfaceNode *surfaceList;
    struct Surface *floor = NULL;

    f32 dynamicBottomHeight = cellHeightLimit;
    f32 staticBottomHeight = cellHeightLimit;

    surfaceList = dynamicWater;
    find_water_bottom_from_list(surfaceList, x, y, z, &dynamicBottomHeight);

    surfaceList = staticWater;
    find_water_bottom_from_list(surfaceList, x, y, z, &staticBottomHeight);

    // Use the lower bottom.
    f32 bottomHeight = MIN(staticBottomHeight, dynamicBottomHeight);

    f32 dynamicTopHeight = floorLowerLimit;
    f32 staticTopHeight = floorLowerLimit;

    surfaceList = dynamicWater;
    struct Surface *dynamicTop = find_water_top_from_list(surfaceList, x, bottomHeight, z, &dynamicTopHeight);

    surfaceList = staticWater;
    struct Surface *staticTop = find_water_top_from_list(surfaceList, x, bottomHeight, z, &staticTopHeight);

    // Use the higher top.
    if (staticTopHeight > dynamicTopHeight) {
        height = staticTopHeight;
        floor = staticTop;
    } else {
        height = dynamicTopHeight;
        floor = dynamicTop;
    }

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
s32 find_water_level(s32 x, s32 y, s32 z) {
    s32 val;
    s32 loX, hiX, loZ, hiZ;
    TerrainData *p = gEnvironmentRegions;
    struct Surface *floor = NULL;
#if PUPPYPRINT_DEBUG
    OSTime first = osGetTime();
#endif
    s32 waterLevel = find_water_floor(x, y, z, &floor);

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

    s32 cellX = get_cell_coord(xPos);
    s32 cellZ = get_cell_coord(zPos);

    // Floors
    list = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_FLOORS].next;
    numFloors += surface_list_length(list);

    list = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_FLOORS].next;
    numFloors += surface_list_length(list);

    // Walls
    list = gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS].next;
    numWalls += surface_list_length(list);

    list = gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS].next;
    numWalls += surface_list_length(list);

    // Ceilings
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
