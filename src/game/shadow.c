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
static ShadowRectangle shadowRectangles[2] = {
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
    f32 waterLevel = find_water_level_and_floor(pos[0], pos[2], waterFloor);
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
s32 init_shadow(Vec3f pos, s16 shadowScale, u8 overwriteSolidity) {
    struct Surface *waterFloor = NULL;

    f32 waterLevel = get_water_level_below_shadow(pos, &waterFloor);

    // if (gEnvironmentRegions != 0) {
    //     waterLevel = get_water_level_below_shadow(s);
    // }

    if (s->flags & SHADOW_FLAG_WATER_BOX) {
        s->floorHeight = waterLevel;

        if (waterFloor != NULL) {
            s->floor    = waterFloor;
            s->flags   &= ~SHADOW_FLAG_WATER_BOX;
            s->flags   |=  SHADOW_FLAG_WATER_SURFACE;
            s->solidity = 200;
        } else {
            s->flags &= ~SHADOW_FLAG_WATER_SURFACE;
            // Assume that the water is flat.
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

    f32 dy = (pos[1] - s->floorHeight);

    if (overwriteSolidity) {
        s->solidity = dim_shadow_with_distance(overwriteSolidity, dy);
    }

    s->scale[1] = scale_shadow_with_distance(shadowScale, dy);

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
    s8 isPlayer = FALSE;
    // Attempt to use existing floors before finding a new one.
    if ((gCurGraphNodeObjectNode == gMarioObject)
     && (gMarioState->floor != NULL)
     && (gCurGraphNodeHeldObject == NULL)) {
        s->floor       = gMarioState->floor;
        s->floorHeight = gMarioState->floorHeight;
        isPlayer = TRUE;
    } else if ((gCurGraphNodeObject != &gMirrorMario)
            && (gCurGraphNodeObjectNode->oFloor != NULL)
            && (gCurGraphNodeHeldObject == NULL)) {
        s->floor       = gCurGraphNodeObjectNode->oFloor;
        s->floorHeight = gCurGraphNodeObjectNode->oFloorHeight;
    } else {
        gCollisionFlags |= COLLISION_FLAG_RETURN_FIRST;
        s->floorHeight = find_floor(pos[0], pos[1], pos[2], &s->floor);
        if (s->floor == NULL) return NULL;
    }

    if ((pos[1] - s->floorHeight) > 1024.0f) {
        return NULL;
    }

    s->flags = SHADOW_FLAGS_NONE;
    if (s->floor->type == SURFACE_ICE) {
        s->flags |= SHADOW_FLAG_ICE;
    }

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
        switch (correct_shadow_solidity_for_animations(shadowSolidity)) {
            case SHADOW_SOLIDITY_NO_SHADOW:
                return NULL;
            case SHADOW_SOILDITY_ALREADY_SET:
                if (init_shadow(pos, shadowScale, /* overwriteSolidity */ 0)) {
                    return NULL;
                }
                break;
            case SHADOW_SOLIDITY_NOT_YET_SET:
                if (init_shadow(pos, shadowScale, shadowSolidity)) {
                    return NULL;
                }
                break;
            default:
                return NULL;
        }
        correct_lava_shadow_height();
    } else {
        if (init_shadow(pos, shadowScale, shadowSolidity)) {
            return NULL;
        }
        if (shadowType >= SHADOW_RECTANGLE_HARDCODED_OFFSET) {
            s8 idx = (shadowType - SHADOW_RECTANGLE_HARDCODED_OFFSET);
            scaleXMod = (shadowRectangles[idx].halfWidth );
            scaleZMod = (shadowRectangles[idx].halfLength);
        }
    }

    Gfx *displayList = alloc_display_list(6 * sizeof(Gfx));

    if (displayList == NULL) return NULL;

    add_shadow_to_display_list(displayList, shadowType);

    if (s->flags & SHADOW_FLAG_CARPET) {
        s->floorHeight += 5;
    }

    f32 baseScale = (s->scale[1] * 0.5f);
    s->scale[0] = (baseScale * scaleXMod);
    s->scale[1] = baseScale;
    s->scale[2] = (baseScale * scaleZMod);

    pos[1] = s->floorHeight;

    return displayList;
}
