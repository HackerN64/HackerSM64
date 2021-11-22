#include <PR/ultratypes.h>
#include <PR/gbi.h>

#include "engine/math_util.h"
#include "engine/surface_collision.h"
#include "behavior_data.h"
#include "geo_misc.h"
#include "level_table.h"
#include "memory.h"
#include "level_update.h"
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
 * An array consisting of all the hardcoded rectangle shadows in the game.
 */
static ShadowRectangle sShadowRectangles[2] = {
    { 3.6f, 2.3f, TRUE }, // Spindel
    { 2.0f, 1.8f, TRUE }, // Whomp
};

struct Shadow gCurrShadow;
struct Shadow *s = &gCurrShadow;

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
        return (initial * (1.0f - ((distFromFloor * 0.5f) / 600.0f)));
    }
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
f32 get_water_level_below_shadow(Vec3f pos, struct Surface **waterFloor) {
    f32 waterLevel = find_water_level_and_floor(pos[0], pos[1], pos[2], waterFloor);
    if (waterLevel < FLOOR_LOWER_LIMIT_MISC) {
        return FLOOR_LOWER_LIMIT_MISC;
    } else if ((pos[1] >= waterLevel)
            && (s->floorHeight  <= waterLevel)) {
        s->flags |= SHADOW_FLAG_WATER_BOX;
    }
    return waterLevel;
}

/**
 * Initialize a shadow. Return 0 on success, 1 on failure.
 *
 * @param pos Position of the parent object (not the shadow)
 * @param shadowScale Diameter of the shadow
 * @param overwriteSolidity Flag for whether the existing shadow solidity should
 *                          be dimmed based on its distance to the floor
 */
s32 init_shadow(Vec3f pos, s16 shadowScale, s8 shadowType, u8 overwriteSolidity) {
    struct Surface *waterFloor = NULL;
    // Check for water under the shadow.
    f32 waterLevel = get_water_level_below_shadow(pos, &waterFloor);

    // if (gEnvironmentRegions != 0) {
    //     waterLevel = get_water_level_below_shadow(s);
    // }

    if (s->flags & SHADOW_FLAG_WATER_BOX) {
        // If there is water under the shadow, put the shadow on the water.
        s->floorHeight = waterLevel;

        if (waterFloor != NULL) {
            // If the water is a surface:
            s->floor    = waterFloor;
            s->flags   &= ~SHADOW_FLAG_WATER_BOX;
            s->flags   |=  SHADOW_FLAG_WATER_SURFACE;
            s->solidity = 200;
        } else {
            // If the water is an environment box:
            s->flags &= ~SHADOW_FLAG_WATER_SURFACE;
            // Assume that the water is flat, so the normal vector points up.
            s->floorNormal[0] = 0.0f;
            ((u32 *) s->floorNormal)[1] = FLOAT_ONE;
            s->floorNormal[2] = 0.0f;
        }

    } else {
        // Don't draw a shadow if the floor is lower than expected possible,
        // or if the y-normal is negative (an unexpected result).
        if ((s->floorNormal[1] <= 0.0f) || (s->floorHeight < FLOOR_LOWER_LIMIT_MISC)) {
            return TRUE;
        }
    }
    if (shadowType != SHADOW_SQUARE_PERMANENT) {
        // Set solidity and scale based on distance.
        f32 dy = (pos[1] - s->floorHeight);

        if (overwriteSolidity) {
            s->solidity = dim_shadow_with_distance(overwriteSolidity, dy);
        }

        s->scale[1] = scale_shadow_with_distance(shadowScale, dy);
    } else {
        s->solidity = overwriteSolidity;
        s->scale[1] = shadowScale;
    }

    return !(s->solidity);
}

/**
 * Linearly interpolate a shadow's solidity between zero and finalSolidity
 * depending on curr's relation to start and end.
 */
void linearly_interpolate_solidity_positive(u8 finalSolidity, s16 curr, s16 start, s16 end) {
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
void linearly_interpolate_solidity_negative(u8 initialSolidity, s16 curr, s16 start, s16 end) {
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
s32 correct_shadow_solidity_for_animations(u8 initialSolidity) {
    s16 animFrame = gMarioObject->header.gfx.animInfo.animFrame;
    switch (gMarioObject->header.gfx.animInfo.animID) {
        case MARIO_ANIM_IDLE_ON_LEDGE:
            return SHADOW_SOLIDITY_NO_SHADOW;
        case MARIO_ANIM_FAST_LEDGE_GRAB:
            linearly_interpolate_solidity_positive(initialSolidity, animFrame,  5, 14);
            return SHADOW_SOILDITY_ALREADY_SET;
        case MARIO_ANIM_SLOW_LEDGE_GRAB:
            linearly_interpolate_solidity_positive(initialSolidity, animFrame, 21, 33);
            return SHADOW_SOILDITY_ALREADY_SET;
        case MARIO_ANIM_CLIMB_DOWN_LEDGE:
            linearly_interpolate_solidity_negative(initialSolidity, animFrame,  0,  5);
            return SHADOW_SOILDITY_ALREADY_SET;
        default:
            return SHADOW_SOLIDITY_NOT_YET_SET;
    }
}

/**
 * Slightly change the height of a shadow in levels with lava.
 */
void correct_lava_shadow_height(void) {
    SurfaceType type = s->floor->type;
    if ((gCurrLevelNum == LEVEL_BITFS) && (type == SURFACE_BURNING)) {
        if (s->floorHeight < -3000.0f) {
            s->floorHeight = -3062.0f;
            s->flags |= SHADOW_FLAG_WATER_BOX;
        } else if (s->floorHeight > 3400.0f) {
            s->floorHeight =  3492.0f;
            s->flags |= SHADOW_FLAG_WATER_BOX;
        }
    } else if ((gCurrLevelNum  == LEVEL_LLL)
            && (gCurrAreaIndex == 1)
            && (type  == SURFACE_BURNING)) {
        s->floorHeight = 5.0f;
        s->flags |= SHADOW_FLAG_WATER_BOX;
    }
}

/**
 * Add a shadow to the given display list.
 * shadowType 0 uses a circle texture, the rest use a square texture.
 * Uses environment alpha for shadow solidity.
 */
void add_shadow_to_display_list(Gfx *displayListHead, s8 shadowType) {
    gSPDisplayList(displayListHead++, (shadowType ? dl_shadow_square : dl_shadow_circle));
    gDPSetEnvColor(displayListHead++, 255, 255, 255, s->solidity);
    gSPDisplayList(displayListHead++, dl_shadow_end);
    gSPEndDisplayList(displayListHead);
}

/**
 * Create a shadow at the absolute position given, with the given parameters.
 * Return a pointer to the display list representing the shadow.
 */
Gfx *create_shadow_below_xyz(Vec3f pos, s16 shadowScale, u8 shadowSolidity, s8 shadowType) {
    struct Object *obj = gCurGraphNodeObjectNode;
    s8 isPlayer = FALSE;
    s8 notHeldObj = (gCurGraphNodeHeldObject == NULL);
    // Check if the object exists.
    if (obj == NULL) {
        return NULL;
    }
    // Attempt to use existing floors before finding a new one.
    if (notHeldObj && (obj == gMarioObject) && gMarioState->floor) {
        // Object is player and has a referenced floor.
        s->floor       = gMarioState->floor;
        s->floorHeight = gMarioState->floorHeight;
        isPlayer = TRUE;
    } else if (notHeldObj && (gCurGraphNodeObject != &gMirrorMario) && obj->oFloor) {
        // Object is not player but has a referenced floor.
        s->floor       = obj->oFloor;
        s->floorHeight = obj->oFloorHeight;
    } else {
        // Object has no referenced floor, so find a new one.
        gCollisionFlags |= COLLISION_FLAG_RETURN_FIRST;
        s->floorHeight = find_floor(pos[0], pos[1], pos[2], &s->floor);
        // No shadow if OOB.
        if (s->floor == NULL) {
            return NULL;
        }
    }
    // Hide shadow if the object is too high.
    if (pos[1] - s->floorHeight > 1024.0f) {
        return NULL;
    }

    // Check for ice floor, which is usually a transparent surface.
    s->flags = SHADOW_FLAGS_NONE;
    if (s->floor->type == SURFACE_ICE) {
        s->flags |= SHADOW_FLAG_ICE;
    }
    // Read the floor's normals
    surface_normal_to_vec3f(s->floorNormal, s->floor);

    f32 scaleXMod = 1.0f;
    f32 scaleZMod = 1.0f;

    if (isPlayer) {
        // Update s->flags if Mario is on a flying carpet.
        if ((gCurrLevelNum == LEVEL_RR)
         && (s->floor->object != NULL)
         && (s->floor->object->behavior == segmented_to_virtual(bhvPlatformOnTrack))) {
            s->flags |= SHADOW_FLAG_CARPET;
        }
        // Set the shadow solidity manually for certain Mario animations.
        switch (correct_shadow_solidity_for_animations(shadowSolidity)) {
            case SHADOW_SOLIDITY_NO_SHADOW:
                return NULL;
            case SHADOW_SOILDITY_ALREADY_SET:
                if (init_shadow(pos, shadowScale, shadowType, /* overwriteSolidity */ 0)) {
                    return NULL;
                }
                break;
            case SHADOW_SOLIDITY_NOT_YET_SET:
                if (init_shadow(pos, shadowScale, shadowType, shadowSolidity)) {
                    return NULL;
                }
                break;
            default:
                return NULL;
        }
        correct_lava_shadow_height();
    } else {
        if (init_shadow(pos, shadowScale, shadowType, shadowSolidity)) {
            return NULL;
        }
        // Get the scaling modifiers for rectangular shadows (Whomp and Spindel).
        if (shadowType >= SHADOW_RECTANGLE_HARDCODED_OFFSET) {
            s8 idx = shadowType - SHADOW_RECTANGLE_HARDCODED_OFFSET;
            scaleXMod = sShadowRectangles[idx].halfWidth;
            scaleZMod = sShadowRectangles[idx].halfLength;
        }
    }

    Gfx *displayList = alloc_display_list(4 * sizeof(Gfx));

    if (displayList == NULL) {
        return NULL;
    }

    // Generate the shadow display list with type and solidity.
    add_shadow_to_display_list(displayList, shadowType);

    // Raise the shadow 5 units on the flying carpet so it doesn't clip into it.
    if (s->flags & SHADOW_FLAG_CARPET) {
        s->floorHeight += 5;
    }

    // scale[1] is set at the end of init_shadow().
    f32 baseScale = (s->scale[1] * 0.5f);
    s->scale[0] = (baseScale * scaleXMod);
    s->scale[1] = baseScale;
    s->scale[2] = (baseScale * scaleZMod);

    // Move the shadow position to the floor height.
    pos[1] = s->floorHeight;

    return displayList;
}
