#include <PR/ultratypes.h>
#include <PR/gbi.h>
#include <math.h>

#include "engine/math_util.h"
#include "engine/surface_collision.h"
#include "geo_misc.h"
#include "level_table.h"
#include "memory.h"
#include "object_list_processor.h"
#include "rendering_graph_node.h"
#include "segment2.h"
#include "shadow.h"
#include "sm64.h"

/**
 * @file shadow.c
 * This file implements a self-contained subsystem used to draw shadows.
 */

/**
 * Encapsulation of information about a shadow.
 */
struct Shadow {
    /* The (x, y, z) position of the object whose shadow this is. */
    Vec3f parentPos;
    /* The y-position of the floor (or water or lava) underneath the object. */
    f32 floorHeight;
    /* Initial (unmodified) size of the shadow. */
    f32 shadowScale;
    /* (nx, ny, nz) normal vector of the floor underneath the object. */
    Vec3f floorNormal;
    /* "originOffset" of the floor underneath the object. */
    f32 floorOriginOffset;
    /* Angle describing "which way a marble would roll," in degrees. */
    f32 floorDownwardAngle;
    /* Angle describing "how tilted the ground is" in degrees (-90 to 90). */
    f32 floorTilt;
    /* Initial solidity of the shadow, from 0 to 255 (just an alpha value). */
    u8 solidity;
};

/**
 * Constant to indicate that a shadow should not be drawn.
 * This is used to disable shadows during specific frames of Mario's
 * animations.
 */
#define SHADOW_SOLIDITY_NO_SHADOW 0
/**
 * Constant to indicate that a shadow's solidity has been pre-set by a previous
 * function and should not be overwritten.
 */
#define SHADOW_SOILDITY_ALREADY_SET 1
/**
 * Constant to indicate that a shadow's solidity has not yet been set.
 */
#define SHADOW_SOLIDITY_NOT_YET_SET 2

/**
 * Constant to indicate any sort of circular shadow.
 */
#define SHADOW_SHAPE_CIRCLE 10
/**
 * Constant to indicate any sort of rectangular shadow.
 */
#define SHADOW_SHAPE_SQUARE 20

/**
 * Constant to indicate a shadow consists of 9 vertices.
 */
#define SHADOW_WITH_9_VERTS 0
/**
 * Constant to indicate a shadow consists of 4 vertices.
 */
#define SHADOW_WITH_4_VERTS 1

/**
 * A struct containing info about hardcoded rectangle shadows.
 */
typedef struct {
    /* Half the width of the rectangle. */
    f32 halfWidth;
    /* Half the length of the rectangle. */
    f32 halfLength;
    /* Flag for if this shadow be smaller when its object is further away. */
    s8 scaleWithDistance;
} shadowRectangle;

/**
 * An array consisting of all the hardcoded rectangle shadows in the game.
 */
shadowRectangle rectangles[2] = {
    /* Shadow for Spindels. */
    { 360.0f, 230.0f, TRUE },
    /* Shadow for Whomps. */
    { 200.0f, 180.0f, TRUE }
};

// See shadow.h for documentation.
s8  gShadowFlags;
s16 sSurfaceTypeBelowShadow;

/**
 * Let (oldZ, oldX) be the relative coordinates of a point on a rectangle,
 * assumed to be centered at the origin on the standard SM64 X-Z plane. This
 * function will update (newZ, newX) to equal the new coordinates of that point
 * after a rotation equal to the yaw of the current graph node object.
 */
void rotate_rectangle(f32 *newZ, f32 *newX, f32 oldZ, f32 oldX) {
    struct Object *obj = (struct Object *) gCurGraphNodeObject;
    f32 s = sins(obj->oFaceAngleYaw);
    f32 c = coss(obj->oFaceAngleYaw);
    *newZ = ((oldZ * c) - (oldX * s));
    *newX = ((oldZ * s) + (oldX * c));
}

/**
 * Return atan2(a, b) in degrees. Note that the argument order is swapped from
 * the standard atan2.
 */
f32 atan2_deg(f32 a, f32 b) {
    return angle_to_degrees(atan2s(a, b));
}

/**
 * Shrink a shadow when its parent object is further from the floor, given the
 * initial size of the shadow and the current distance.
 */
f32 scale_shadow_with_distance(f32 initial, f32 distFromFloor) {
    if (distFromFloor <= 0.0f) {
        return initial;
    } else if (distFromFloor >= 600.0f) {
        return (initial * 0.5f);
    } else {
        return (initial * (1.0f - (0.5f * distFromFloor / 600.0f)));
    }
}

/**
 * Disable a shadow when its parent object is more than 600 units from the ground.
 */
f32 disable_shadow_with_distance(f32 shadowScale, f32 distFromFloor) {
    return ((distFromFloor >= 600.0f) ? 0.0f : shadowScale);
}

/**
 * Dim a shadow when its parent object is further from the ground.
 */
s32 dim_shadow_with_distance(u8 solidity, f32 distFromFloor) {
    if (solidity < 121) {
        return solidity;
    } else if (distFromFloor <=   0.0f) {
        return solidity;
    } else if (distFromFloor >= 600.0f) {
        return 120;
    } else {
        return ((((120 - solidity) * distFromFloor) / 600.0f) + (f32) solidity);
    }
}

/**
 * Return the water level below a shadow, or 0 if the water level is below
 * -10,000.
 */
f32 get_water_level_below_shadow(struct Shadow *s, struct Surface **waterFloor) {
    f32 waterLevel = find_water_level_and_floor(s->parentPos[0], s->parentPos[2], waterFloor);
    if (waterLevel < FLOOR_LOWER_LIMIT_MISC) {
        return 0;
    } else if ((s->parentPos[1] >= waterLevel) && (s->floorHeight <= waterLevel)) {
        gShadowFlags |= SHADOW_FLAG_WATER_BOX;
    }
    return waterLevel;
}

/**
 * Initialize a shadow. Return 0 on success, 1 on failure.
 *
 * @param xPos,yPos,zPos Position of the parent object (not the shadow)
 * @param shadowScale Diameter of the shadow
 * @param overwriteSolidity Flag for whether the existing shadow solidity should
 *                          be dimmed based on its distance to the floor
 */
s32 init_shadow(struct Shadow *s, f32 xPos, f32 yPos, f32 zPos, s16 shadowScale, u8 overwriteSolidity) {
    struct Surface *floor;
    struct Surface *waterFloor = NULL;

    vec3f_set(s->parentPos, xPos, yPos, zPos);

    if ((gCurGraphNodeObjectNode != gMarioObject)
     && (gCurGraphNodeObject != &gMirrorMario)
     && (gCurGraphNodeObjectNode->oFloor != NULL)) {
        floor          = gCurGraphNodeObjectNode->oFloor;
        s->floorHeight = gCurGraphNodeObjectNode->oFloorHeight;
    } else {
        s->floorHeight = find_floor(xPos, yPos, zPos, &floor);
    }

    f32 waterLevel = get_water_level_below_shadow(s, &waterFloor);

    // if (gEnvironmentRegions != 0) {
    //     waterLevel = get_water_level_below_shadow(s);
    // }

    if (gShadowFlags & SHADOW_FLAG_WATER_BOX) {
        s->floorHeight = waterLevel;

        if (waterFloor != NULL) {
            vec3f_set(s->floorNormal, waterFloor->normal.x,
                                      waterFloor->normal.y,
                                      waterFloor->normal.z);
            s->floorOriginOffset = waterFloor->originOffset;
            gShadowFlags &= ~SHADOW_FLAG_WATER_BOX;
            gShadowFlags |= SHADOW_FLAG_WATER_SURFACE;
            s->solidity = 200;
        } else {
            gShadowFlags &= ~SHADOW_FLAG_WATER_SURFACE;
            // Assume that the water is flat.
            s->floorNormal[0] = 0.0f;
            ((u32 *) s->floorNormal)[1] = FLOAT_ONE;
            s->floorNormal[2] = 0.0f;
            s->floorOriginOffset = -waterLevel;
        }

    } else {
        // Don't draw a shadow if the floor is lower than expected possible,
        // or if the y-normal is negative (an unexpected result).
        if ((s->floorHeight < FLOOR_LOWER_LIMIT_MISC) || (floor->normal.y <= 0.0f)) {
            return TRUE;
        }

        vec3f_set(s->floorNormal, floor->normal.x,
                                  floor->normal.y,
                                  floor->normal.z);
        s->floorOriginOffset = floor->originOffset;
    }

    if (overwriteSolidity) {
        s->solidity = dim_shadow_with_distance(overwriteSolidity, (yPos - s->floorHeight));
    }

    s->shadowScale = scale_shadow_with_distance(shadowScale, (yPos - s->floorHeight));

    s->floorDownwardAngle = atan2_deg(s->floorNormal[2], s->floorNormal[0]);

    f32 floorSteepness = (sqr(s->floorNormal[0]) + sqr(s->floorNormal[2]));

    // This if-statement avoids dividing by 0.
    if (floorSteepness == 0.0f) {
        s->floorTilt = 0;
    } else {
        floorSteepness = sqrtf(floorSteepness);
        s->floorTilt = (90.0f - atan2_deg(floorSteepness, s->floorNormal[1]));
    }
    return FALSE;
}

/**
 * Given a `vertexNum` from a shadow with nine vertices, update the
 * texture coordinates corresponding to that vertex. That is:
 *      0 = (-15, -15)         1 = (0, -15)         2 = (15, -15)
 *      3 = (-15,   0)         4 = (0,   0)         5 = (15,   0)
 *      6 = (-15,  15)         7 = (0,  15)         8 = (15,  15)
 */
void get_texture_coords_9_vertices(s8 vertexNum, s16 *textureX, s16 *textureY) {
#ifdef HD_SHADOWS
    *textureX = (((vertexNum % 3) * 63) - 63);
    *textureY = (((vertexNum / 3) * 63) - 63);
#else
    *textureX = (((vertexNum % 3) * 15) - 15);
    *textureY = (((vertexNum / 3) * 15) - 15);
#endif
}

/**
 * Given a `vertexNum` from a shadow with four vertices, update the
 * texture coordinates corresponding to that vertex. That is:
 *      0 = (-15, -15)         1 = (15, -15)
 *      2 = (-15,  15)         3 = (15,  15)
 */
void get_texture_coords_4_vertices(s8 vertexNum, s16 *textureX, s16 *textureY) {
#ifdef HD_SHADOWS
    *textureX = (((vertexNum & 0x1) * 2 * 63) - 63);
    *textureY = (((vertexNum >>  1) * 2 * 63) - 63);
#else
    *textureX = (((vertexNum & 0x1) * 2 * 15) - 15);
    *textureY = (((vertexNum >>  1) * 2 * 15) - 15);
#endif
}

/**
 * Make a shadow's vertex at a position relative to its parent.
 *
 * @param vertices A preallocated display list for vertices
 * @param index Index into `vertices` to insert the vertex
 * @param relX,relY,relZ Vertex position relative to its parent object
 * @param alpha Opacity of the vertex
 * @param shadowVertexType One of SHADOW_WITH_9_VERTS or SHADOW_WITH_4_VERTS
 */
void make_shadow_vertex_at_xyz(Vtx *vertices, s8 index, f32 relX, f32 relY, f32 relZ, u8 alpha, s8 shadowVertexType) {
    s16 vtxX = roundf(relX);
    s16 vtxY = roundf(relY);
    s16 vtxZ = roundf(relZ);
    s16 textureX = 0;
    s16 textureY = 0;

    switch (shadowVertexType) {
        case SHADOW_WITH_9_VERTS:
            get_texture_coords_9_vertices(index, &textureX, &textureY);
            break;
        case SHADOW_WITH_4_VERTS:
            get_texture_coords_4_vertices(index, &textureX, &textureY);
            break;
    }

    // Move the shadow up and over slightly while standing on a flying carpet.
    if (gShadowFlags & SHADOW_FLAG_RAISED) {
        vtxX += 5;
        vtxY += 5;
        vtxZ += 5;
    }
    make_vertex( // shadows are black
        vertices, index, vtxX, vtxY, vtxZ, (textureX << 5), (textureY << 5), 255, 255, 255, alpha
    );
}

/**
 * Given an (x, z)-position close to a shadow, extrapolate the y-position
 * according to the floor's normal vector.
 */
f32 extrapolate_vertex_y_position(struct Shadow s, f32 vtxX, f32 vtxZ) {
    return (-((s.floorNormal[0] * vtxX) + (s.floorNormal[2] * vtxZ) + s.floorOriginOffset) / s.floorNormal[1]);
}

/**
 * Given a shadow vertex with the given `index`, return the corresponding texture
 * coordinates ranging in the square with corners at (-1, -1), (1, -1), (-1, 1),
 * and (1, 1) in the x-z plane. See `get_texture_coords_9_vertices()` and
 * `get_texture_coords_4_vertices()`, which have similar functionality, but
 * return 15 times these values.
 */
void get_vertex_coords(s8 index, s8 shadowVertexType, s8 *xCoord, s8 *zCoord) {
    *xCoord = ((index % (3 - shadowVertexType)) - 1);
    *zCoord = ((index / (3 - shadowVertexType)) - 1);

    // This just corrects the 4-vertex case to have consistent results with the 9-vertex case.
    if (shadowVertexType == SHADOW_WITH_4_VERTS) {
        if (*xCoord == 0) *xCoord = 1;
        if (*zCoord == 0) *zCoord = 1;
    }
}

/**
 * Populate `xPosVtx`, `yPosVtx`, and `zPosVtx` with the (x, y, z) position of the
 * shadow vertex with the given index. If the shadow is to have 9 vertices,
 * then each of those vertices is clamped down to the floor below it. Otherwise,
 * in the 4 vertex case, the vertex positions are extrapolated from the center
 * of the shadow.
 *
 * In practice, due to the if-statement in `make_shadow_vertex()`, the 9
 * vertex and 4 vertex cases are identical, and the above-described clamping
 * behavior is overwritten.
 */
void calculate_vertex_xyz(s8 index, struct Shadow s, f32 *xPosVtx, f32 *yPosVtx, f32 *zPosVtx, s8 shadowVertexType) {
    f32 tiltedScale = (cosf(degrees_to_radians(s.floorTilt)) * s.shadowScale);
    f32 downwardAngle = degrees_to_radians(s.floorDownwardAngle);
    s8 xCoordUnit, zCoordUnit;

    // This makes xCoordUnit and yCoordUnit each one of -1, 0, or 1.
    get_vertex_coords(index, shadowVertexType, &xCoordUnit, &zCoordUnit);

    f32 halfScale       = ((xCoordUnit * s.shadowScale) / 2.0f);
    f32 halfTiltedScale = ((zCoordUnit *   tiltedScale) / 2.0f);

    *xPosVtx = ((halfTiltedScale * sinf(downwardAngle)) + (halfScale * cosf(downwardAngle)) + s.parentPos[0]);
    *zPosVtx = ((halfTiltedScale * cosf(downwardAngle)) - (halfScale * sinf(downwardAngle)) + s.parentPos[2]);

    if (gShadowFlags & SHADOW_FLAG_WATER_BOX) {
        *yPosVtx = s.floorHeight;
    } else {
        switch (shadowVertexType) {
            /**
             * Note that this dichotomy is later overwritten in
             * make_shadow_vertex().
             */
            case SHADOW_WITH_9_VERTS:
                // Clamp this vertex's y-position to that of the floor directly
                // below it, which may differ from the floor below the center
                // vertex.
                *yPosVtx = find_floor_height(*xPosVtx, s.parentPos[1], *zPosVtx);
                break;
            case SHADOW_WITH_4_VERTS:
                // Do not clamp. Instead, extrapolate the y-position of this
                // vertex based on the directly floor below the parent object.
                *yPosVtx = extrapolate_vertex_y_position(s, *xPosVtx, *zPosVtx);
                break;
        }
    }
}

/**
 * Given a vertex's location, return the dot product of the
 * position of that vertex (relative to the shadow's center) with the floor
 * normal (at the shadow's center).
 *
 * Since it is a dot product, this returns 0 if these two vectors are
 * perpendicular, meaning the ground is locally flat. It returns nonzero
 * in most cases where `vtxY` is on a different floor triangle from the
 * center vertex, as in the case with SHADOW_WITH_9_VERTS, which sets
 * the y-value from `find_floor`. (See the bottom of `calculate_vertex_xyz`.)
 */
s32 floor_local_tilt(struct Shadow s, f32 vtxX, f32 vtxY, f32 vtxZ) {
    f32 relX = (vtxX - s.parentPos[0]);
    f32 relY = (vtxY - s.floorHeight);
    f32 relZ = (vtxZ - s.parentPos[2]);

    return (s16)((relX * s.floorNormal[0])
               + (relY * s.floorNormal[1])
               + (relZ * s.floorNormal[2]));
}

/**
 * Make a particular vertex from a shadow, calculating its position and solidity.
 */
void make_shadow_vertex(Vtx *vertices, s8 index, struct Shadow s, s8 shadowVertexType) {
    Vec3f posVtx, rel;

    u8 solidity = s.solidity;
    if (gShadowFlags & SHADOW_FLAG_WATER_BOX) {
        solidity = 200;
    }

    calculate_vertex_xyz(index, s, &posVtx[0], &posVtx[1], &posVtx[2], shadowVertexType);

    /**
     * This is the hack that makes "SHADOW_WITH_9_VERTS" act identically to
     * "SHADOW_WITH_4_VERTS" in the game; this same hack is disabled by the
     * GameShark code in this video: https://youtu.be/MSIh4rtNGF0. The code in
     * the video makes `extrapolate_vertex_y_position` return the same value as
     * the last-called function that returns a float; in this case, that's
     * `find_floor`, which this if-statement was designed to overwrite in the
     * first place. Thus, this if-statement is disabled by that code.
     *
     * The last condition here means the y-position calculated previously
     * was probably on a different floor triangle from the center vertex.
     * The gShadowAboveWaterOrLava check is redundant, since `floor_local_tilt`
     * will always be 0 over water or lava (since they are always flat).
     */
    if ((shadowVertexType == SHADOW_WITH_9_VERTS)
     && !(gShadowFlags & SHADOW_FLAG_WATER_BOX)
     && (floor_local_tilt(s, posVtx[0], posVtx[1], posVtx[2]) != 0)) {
        posVtx[1] = extrapolate_vertex_y_position(s, posVtx[0], posVtx[2]);
        solidity = 0;
    }
    vec3f_diff(rel, posVtx, s.parentPos);

    make_shadow_vertex_at_xyz(vertices, index, rel[0], rel[1], rel[2], solidity, shadowVertexType);
}

/**
 * Add a shadow to the given display list.
 */
void add_shadow_to_display_list(Gfx *displayListHead, Vtx *verts, s8 shadowVertexType, s8 shadowShape) {
    switch (shadowShape) {
        case SHADOW_SHAPE_CIRCLE:
            gSPDisplayList(displayListHead++, dl_shadow_circle);
            break;
        case SHADOW_SHAPE_SQUARE:
            gSPDisplayList(displayListHead++, dl_shadow_square) break;
    }
    switch (shadowVertexType) {
        case SHADOW_WITH_9_VERTS:
            gSPVertex(displayListHead++, verts, 9, 0);
            gSPDisplayList(displayListHead++, dl_shadow_9_verts);
            break;
        case SHADOW_WITH_4_VERTS:
            gSPVertex(displayListHead++, verts, 4, 0);
            gSPDisplayList(displayListHead++, dl_shadow_4_verts);
            break;
    }
    gSPDisplayList(displayListHead++, dl_shadow_end);
    gSPEndDisplayList(displayListHead);
}

/**
 * Linearly interpolate a shadow's solidity between zero and finalSolidity
 * depending on curr's relation to start and end.
 */
void linearly_interpolate_solidity_positive(struct Shadow *s, u8 finalSolidity, s16 curr, s16 start, s16 end) {
    if ((curr >= 0) && (curr < start)) {
        s->solidity = 0;
    } else if (end < curr) {
        s->solidity = finalSolidity;
    } else {
        s->solidity = ((f32) finalSolidity * (curr - start) / (end - start));
    }
}

/**
 * Linearly interpolate a shadow's solidity between initialSolidity and zero
 * depending on curr's relation to start and end. Note that if curr < start,
 * the solidity will be zero.
 */
void linearly_interpolate_solidity_negative(struct Shadow *s, u8 initialSolidity, s16 curr, s16 start, s16 end) {
    // The curr < start case is not handled. Thus, if start != 0, this function
    // will have the surprising behavior of hiding the shadow until start.
    // This is not necessarily a bug, since this function is only used once,
    // with start == 0.
    if ((curr >= start) && (end >= curr)) {
        s->solidity = ((f32) initialSolidity * (1.0f - (f32)(curr - start) / (end - start)));
    } else {
        s->solidity = 0;
    }
}

/**
 * Change a shadow's solidity based on the player's current animation frame.
 */
s32 correct_shadow_solidity_for_animations(UNUSED s32 isLuigi, u8 initialSolidity, struct Shadow *shadow) {
    struct Object *player = gMarioObject;
    s16 animFrame = player->header.gfx.animInfo.animFrame;
    switch (player->header.gfx.animInfo.animID) {
        case MARIO_ANIM_IDLE_ON_LEDGE:
            return SHADOW_SOLIDITY_NO_SHADOW;
            break;
        case MARIO_ANIM_FAST_LEDGE_GRAB:
            linearly_interpolate_solidity_positive(shadow, initialSolidity, animFrame,  5, 14);
            return SHADOW_SOILDITY_ALREADY_SET;
            break;
        case MARIO_ANIM_SLOW_LEDGE_GRAB:
            linearly_interpolate_solidity_positive(shadow, initialSolidity, animFrame, 21, 33);
            return SHADOW_SOILDITY_ALREADY_SET;
            break;
        case MARIO_ANIM_CLIMB_DOWN_LEDGE:
            linearly_interpolate_solidity_negative(shadow, initialSolidity, animFrame,  0,  5);
            return SHADOW_SOILDITY_ALREADY_SET;
            break;
        default:
            return SHADOW_SOLIDITY_NOT_YET_SET;
            break;
    }
}

/**
 * Slightly change the height of a shadow in levels with lava.
 */
void correct_lava_shadow_height(struct Shadow *s) {
    if ((gCurrLevelNum == LEVEL_BITFS) && (sSurfaceTypeBelowShadow == SURFACE_BURNING)) {
        if (s->floorHeight < -3000.0f) {
            s->floorHeight = -3062.0f;
            gShadowFlags |= SHADOW_FLAG_WATER_BOX;
        } else if (s->floorHeight > 3400.0f) {
            s->floorHeight = 3492.0f;
            gShadowFlags |= SHADOW_FLAG_WATER_BOX;
        }
    } else if ((gCurrLevelNum == LEVEL_LLL)
            && (gCurrAreaIndex == 1)
            && (sSurfaceTypeBelowShadow == SURFACE_BURNING)) {
        s->floorHeight = 5.0f;
        gShadowFlags |= SHADOW_FLAG_WATER_BOX;
    }
}

/**
 * Create a shadow under a player, correcting that shadow's opacity during
 * appropriate animations and other states.
 */
Gfx *create_shadow_player(f32 xPos, f32 yPos, f32 zPos, s16 shadowScale, u8 solidity, s32 isLuigi) {
    struct Shadow shadow;
    s8 ret = 0;
    s32 i;

    // Update global variables about whether Mario is on a flying carpet.
    if ((gCurrLevelNum == LEVEL_RR) && (sSurfaceTypeBelowShadow != SURFACE_DEATH_PLANE)) {
        switch (gFlyingCarpetState) {
            case FLYING_CARPET_MOVING_WITHOUT_MARIO:
                gShadowFlags |= (SHADOW_FLAG_ICE_CARPET | SHADOW_FLAG_RAISED);
                break;
            case FLYING_CARPET_MOVING_WITH_MARIO:
                gShadowFlags = SHADOW_FLAG_ICE_CARPET;
                break;
        }
    }

    switch (correct_shadow_solidity_for_animations(isLuigi, solidity, &shadow)) {
        case SHADOW_SOLIDITY_NO_SHADOW: return NULL; break;
        case SHADOW_SOILDITY_ALREADY_SET:
            ret = init_shadow(&shadow, xPos, yPos, zPos, shadowScale, /* overwriteSolidity */ 0);
            break;
        case SHADOW_SOLIDITY_NOT_YET_SET:
            ret = init_shadow(&shadow, xPos, yPos, zPos, shadowScale, solidity);
            break;
    }
    if (ret != 0) {
        return NULL;
    }

    Vtx *verts       = alloc_display_list(9 * sizeof(Vtx));
    Gfx *displayList = alloc_display_list(5 * sizeof(Gfx));

    if ((verts == NULL) || (displayList == NULL)) {
        return NULL;
    }

    correct_lava_shadow_height(&shadow);

    for (i = 0; i < 9; i++) {
        make_shadow_vertex(verts, i, shadow, SHADOW_WITH_9_VERTS);
    }
    add_shadow_to_display_list(displayList, verts, SHADOW_WITH_9_VERTS, SHADOW_SHAPE_CIRCLE);
    return displayList;
}

/**
 * Create a circular shadow composed of 9 vertices.
 */
Gfx *create_shadow_circle_9_verts(f32 xPos, f32 yPos, f32 zPos, s16 shadowScale, u8 solidity) {
    struct Shadow shadow;
    s32 i;

    if (init_shadow(&shadow, xPos, yPos, zPos, shadowScale, solidity) != 0) {
        return NULL;
    }

    Vtx *verts       = alloc_display_list(9 * sizeof(Vtx));
    Gfx *displayList = alloc_display_list(5 * sizeof(Gfx));

    if ((verts == NULL) || (displayList == NULL)) {
        return NULL;
    }

    for (i = 0; i < 9; i++) {
        make_shadow_vertex(verts, i, shadow, SHADOW_WITH_9_VERTS);
    }
    add_shadow_to_display_list(displayList, verts, SHADOW_WITH_9_VERTS, SHADOW_SHAPE_CIRCLE);
    return displayList;
}

/**
 * Create a circular shadow composed of 4 vertices.
 */
Gfx *create_shadow_circle_4_verts(f32 xPos, f32 yPos, f32 zPos, s16 shadowScale, u8 solidity) {
    struct Shadow shadow;
    s32 i;

    if (init_shadow(&shadow, xPos, yPos, zPos, shadowScale, solidity) != 0) {
        return NULL;
    }

    Vtx *verts       = alloc_display_list(4 * sizeof(Vtx));
    Gfx *displayList = alloc_display_list(5 * sizeof(Gfx));

    if ((verts == NULL) || (displayList == NULL)) {
        return NULL;
    }

    for (i = 0; i < 4; i++) {
        make_shadow_vertex(verts, i, shadow, SHADOW_WITH_4_VERTS);
    }
    add_shadow_to_display_list(displayList, verts, SHADOW_WITH_4_VERTS, SHADOW_SHAPE_CIRCLE);
    return displayList;
}

/**
 * Create a circular shadow composed of 4 vertices and assume that the ground
 * underneath it is totally flat.
 */
Gfx *create_shadow_circle_assuming_flat_ground(f32 xPos, f32 yPos, f32 zPos, s16 shadowScale, u8 solidity) {
    f32 distBelowFloor;
    f32 floorHeight;
    f32 radius = (shadowScale / 2);

    if (gCurGraphNodeObjectNode->oFloor != NULL) {
        floorHeight = gCurGraphNodeObjectNode->oFloorHeight;
    } else {
        floorHeight = find_floor_height(xPos, yPos, zPos);
    }

    if (floorHeight < FLOOR_LOWER_LIMIT_MISC) {
        return NULL;
    } else {
        distBelowFloor = (floorHeight - yPos);
    }

    Vtx *verts       = alloc_display_list(4 * sizeof(Vtx));
    Gfx *displayList = alloc_display_list(5 * sizeof(Gfx));

    if ((verts == NULL) || (displayList == NULL)) {
        return 0;
    }

    make_shadow_vertex_at_xyz(verts, 0, -radius, distBelowFloor, -radius, solidity, 1);
    make_shadow_vertex_at_xyz(verts, 1,  radius, distBelowFloor, -radius, solidity, 1);
    make_shadow_vertex_at_xyz(verts, 2, -radius, distBelowFloor,  radius, solidity, 1);
    make_shadow_vertex_at_xyz(verts, 3,  radius, distBelowFloor,  radius, solidity, 1);

    add_shadow_to_display_list(displayList, verts, SHADOW_WITH_4_VERTS, SHADOW_SHAPE_CIRCLE);
    return displayList;
}

/**
 * Create a rectangular shadow composed of 4 vertices. This assumes the ground
 * underneath the shadow is totally flat.
 */
Gfx *create_shadow_rectangle(f32 halfWidth, f32 halfLength, f32 relY, u8 solidity) {
    Vtx *verts       = alloc_display_list(4 * sizeof(Vtx));
    Gfx *displayList = alloc_display_list(5 * sizeof(Gfx));
    f32 frontLeftX, frontLeftZ, frontRightX, frontRightZ, backLeftX, backLeftZ, backRightX, backRightZ;

    if ((verts == NULL) || (displayList == NULL)) {
        return NULL;
    }

    // Rotate the shadow based on the parent object's face angle.
    rotate_rectangle(&frontLeftZ,  &frontLeftX,  -halfLength, -halfWidth);
    rotate_rectangle(&frontRightZ, &frontRightX, -halfLength,  halfWidth);
    rotate_rectangle(&backLeftZ,   &backLeftX,    halfLength, -halfWidth);
    rotate_rectangle(&backRightZ,  &backRightX,   halfLength,  halfWidth);

    make_shadow_vertex_at_xyz(verts, 0, frontLeftX,  relY, frontLeftZ,  solidity, 1);
    make_shadow_vertex_at_xyz(verts, 1, frontRightX, relY, frontRightZ, solidity, 1);
    make_shadow_vertex_at_xyz(verts, 2, backLeftX,   relY, backLeftZ,   solidity, 1);
    make_shadow_vertex_at_xyz(verts, 3, backRightX,  relY, backRightZ,  solidity, 1);

    add_shadow_to_display_list(displayList, verts, SHADOW_WITH_4_VERTS, SHADOW_SHAPE_SQUARE);
    return displayList;
}

/**
 * Populate `shadowHeight` and `solidity` appropriately; the default solidity
 * value is 200. Return 0 if a shadow should be drawn, 1 if not.
 */
s32 get_shadow_height_solidity(f32 xPos, f32 yPos, f32 zPos, f32 *shadowHeight, u8 *solidity) {
    if (gCurGraphNodeObjectNode->oFloor != NULL) {
        *shadowHeight = gCurGraphNodeObjectNode->oFloorHeight;
    } else {
        *shadowHeight = find_floor_height(xPos, yPos, zPos);
    }

    if (*shadowHeight < FLOOR_LOWER_LIMIT_MISC) {
        return TRUE;
    } else {
        f32 waterLevel = find_water_level(xPos, zPos);
        if (waterLevel < FLOOR_LOWER_LIMIT_MISC) {
            // Dead if-statement. There may have been an assert here.
        } else if ((yPos >= waterLevel) && (waterLevel >= *shadowHeight)) {
            gShadowFlags |= SHADOW_FLAG_WATER_BOX;
            *shadowHeight = waterLevel;
            *solidity = 200;
        }
    }
    return FALSE;
}

/**
 * Create a square shadow composed of 4 vertices.
 */
Gfx *create_shadow_square(f32 xPos, f32 yPos, f32 zPos, s16 shadowScale, u8 solidity, s8 shadowType) {
    f32 shadowHeight;
    f32 shadowRadius;

    if (get_shadow_height_solidity(xPos, yPos, zPos, &shadowHeight, &solidity)) {
        return NULL;
    }

    f32 distFromShadow = (yPos - shadowHeight);
    switch (shadowType) {
        case SHADOW_SQUARE_PERMANENT: shadowRadius = (shadowScale >> 1); break;
        case SHADOW_SQUARE_SCALABLE:  shadowRadius = (  scale_shadow_with_distance(shadowScale, distFromShadow) * 0.5f); break;
        case SHADOW_SQUARE_TOGGLABLE: shadowRadius = (disable_shadow_with_distance(shadowScale, distFromShadow) * 0.5f); break;
        default: return NULL;
    }

    return create_shadow_rectangle(shadowRadius, shadowRadius, -distFromShadow, solidity);
}

/**
 * Create a rectangular shadow whose parameters have been hardcoded in the
 * `rectangles` array.
 */
Gfx *create_shadow_hardcoded_rectangle(f32 xPos, f32 yPos, f32 zPos, UNUSED s16 shadowScale, u8 solidity, s8 shadowType) {
    f32 shadowHeight;
    f32 halfWidth, halfLength;
    s8 idx = (shadowType - SHADOW_RECTANGLE_HARDCODED_OFFSET);

    if (get_shadow_height_solidity(xPos, yPos, zPos, &shadowHeight, &solidity)) {
        return NULL;
    }

    f32 distFromShadow = (yPos - shadowHeight);
    /**
     * Note that idx could be negative or otherwise out of the bounds of
     * the `rectangles` array. In practice, it never is, because this was
     * only used twice.
     */
    if (rectangles[idx].scaleWithDistance == TRUE) {
        halfWidth  = scale_shadow_with_distance(rectangles[idx].halfWidth,  distFromShadow);
        halfLength = scale_shadow_with_distance(rectangles[idx].halfLength, distFromShadow);
    } else {
        // This code is never used because the third element of the rectangle struct is always TRUE.
        halfWidth  = rectangles[idx].halfWidth;
        halfLength = rectangles[idx].halfLength;
    }
    return create_shadow_rectangle(halfWidth, halfLength, -distFromShadow, solidity);
}

/**
 * Create a shadow at the absolute position given, with the given parameters.
 * Return a pointer to the display list representing the shadow.
 */
Gfx *create_shadow_below_xyz(f32 xPos, f32 yPos, f32 zPos, s16 shadowScale, u8 shadowSolidity, s8 shadowType) {
    struct Surface *pfloor;
    if ((gCurGraphNodeObject != &gMirrorMario)
     && (gCurGraphNodeObjectNode->oFloor != NULL)) {
        pfloor = gCurGraphNodeObjectNode->oFloor;
    } else {
        find_floor(xPos, yPos, zPos, &pfloor);
    }
    gShadowFlags = SHADOW_FLAGS_NONE;
    if (pfloor != NULL) {
        if (pfloor->type == SURFACE_ICE) {
            gShadowFlags |= SHADOW_FLAG_ICE_CARPET;
        }
        sSurfaceTypeBelowShadow = pfloor->type;
    }
    switch (shadowType) {
        case SHADOW_CIRCLE_9_VERTS:             return create_shadow_circle_9_verts             (xPos, yPos, zPos, shadowScale, shadowSolidity                     ); break;
        case SHADOW_CIRCLE_4_VERTS:             return create_shadow_circle_4_verts             (xPos, yPos, zPos, shadowScale, shadowSolidity                     ); break;
        case SHADOW_CIRCLE_4_VERTS_FLAT_UNUSED: return create_shadow_circle_assuming_flat_ground(xPos, yPos, zPos, shadowScale, shadowSolidity                     ); break; // unused shadow type
        case SHADOW_SQUARE_PERMANENT:           return create_shadow_square                     (xPos, yPos, zPos, shadowScale, shadowSolidity, shadowType         ); break;
        case SHADOW_SQUARE_SCALABLE:            return create_shadow_square                     (xPos, yPos, zPos, shadowScale, shadowSolidity, shadowType         ); break;
        case SHADOW_SQUARE_TOGGLABLE:           return create_shadow_square                     (xPos, yPos, zPos, shadowScale, shadowSolidity, shadowType         ); break;
        case SHADOW_CIRCLE_PLAYER:              return create_shadow_player                     (xPos, yPos, zPos, shadowScale, shadowSolidity, /* isLuigi */ FALSE); break;
        default:                                return create_shadow_hardcoded_rectangle        (xPos, yPos, zPos, shadowScale, shadowSolidity, shadowType         ); break;
    }
}
