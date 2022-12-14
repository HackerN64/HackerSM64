#ifndef PAINTINGS_H
#define PAINTINGS_H

#include <PR/ultratypes.h>
#include <PR/gbi.h>

#include "macros.h"
#include "types.h"


/// The default painting edge length. Default is 614.4f.
#define PAINTING_SIZE 614.4f

/// The depth of the area in front of the painting which triggers ripples without warping. Default is 102.4f.
#define PAINTING_WOBBLE_DEPTH 102.4f

/// The depth of the area behind the painting which triggers the warp. Default is 409.6f.
#define PAINTING_WARP_DEPTH 409.6f

/// The threshold relative to the painting's plane between wobbling and warping. Default is -30.72f.
#define PAINTING_WOBBLE_WARP_THRESHOLD -30.72f

/// The size of the buffer around the edges of the painting in which Mario is still considered within bounds.
#define PAINTING_EDGE_MARGIN 160.0f

/// This is added to Mario's Y position to make the ripple closer to Mario's center of mass. Default is 50.0f
#define PAINTING_MARIO_Y_OFFSET 50.0f

/// Convert image coordinates to texel coordinates.
#define TC(t) (((t) - 1) << 5)


enum PaintingIDs {
    /* Painting ID */
    /*        0x00 */ PAINTING_ID_NULL,
    /*        0x01 */ PAINTING_ID_CASTLE_BOB,
    /*        0x02 */ PAINTING_ID_CASTLE_CCM,
    /*        0x03 */ PAINTING_ID_CASTLE_WF,
    /*        0x04 */ PAINTING_ID_CASTLE_JRB,
    /*        0x05 */ PAINTING_ID_CASTLE_LLL,
    /*        0x06 */ PAINTING_ID_CASTLE_SSL,
    /*        0x07 */ PAINTING_ID_CASTLE_HMC,
    /*        0x08 */ PAINTING_ID_CASTLE_DDD,
    /*        0x09 */ PAINTING_ID_CASTLE_WDW,
    /*        0x0A */ PAINTING_ID_CASTLE_THI_TINY,
    /*        0x0B */ PAINTING_ID_CASTLE_TTM,
    /*        0x0C */ PAINTING_ID_CASTLE_TTC,
    /*        0x0D */ PAINTING_ID_CASTLE_SL,
    /*        0x0E */ PAINTING_ID_CASTLE_THI_HUGE,
    /*        0x0F */ PAINTING_ID_CASTLE_RR,
    /*        0x10 */ PAINTING_ID_HMC_COTMC,
    /*        0x11 */ PAINTING_ID_TTM_SLIDE,
};

// Types of ripple animations.
enum PaintingRippleAnimations {
    RIPPLE_ANIM_CONTINUOUS,
    RIPPLE_ANIM_PROXIMITY,
    RIPPLE_ANIM_PROXIMITY_LARGE,
};

// Painting->textureType
enum PaintingType {
    /// Painting that uses 1 or more images as a texture
    PAINTING_TYPE_IMAGE,
    /// Painting that has one texture used for an environment map effect
    PAINTING_TYPE_ENV_MAP
};

// Painting->rippleTrigger
enum RippleTriggers {
    RIPPLE_TRIGGER_NONE,
    RIPPLE_TRIGGER_PROXIMITY,
    RIPPLE_TRIGGER_CONTINUOUS,
};

// oAction
enum oActionsPainting {
    PAINTING_ACT_IDLE,
    PAINTING_ACT_RIPPLING,
    PAINTING_ACT_ENTERED,
};


/**
 * A list of preset constants for the ripple animations.
 */
struct RippleAnimation {
    /*0x00*/ const f32 mag;         /// Controls how high the peaks of the ripple are when the animation starts.
    /*0x04*/ const f32 decay;       /// Multiplier that controls how fast the ripple regresses to the IDLE state.
    /*0x08*/ const f32 rate;        /// Controls the ripple's frequency.
    /*0x0C*/ const f32 dispersion;  /// The rate at which the magnitude of the ripple decreases as you move farther from the central point of the ripple.
}; /*0x10*/

/**
 * A ripple animation pair.
 */
struct RippleAnimationPair {
    /*0x00*/ const struct RippleAnimation passive;  /// The ripple when the painting is continuously rippling or is lightly touched.
    /*0x10*/ const struct RippleAnimation entry;    /// The ripple when the painting is entered.
}; /*0x20*/

/**
 * Painting info struct.
 */
struct PaintingImage {
    /// Texture data.
    /*0x00*/ const Texture *const *textureArray;

    /// How many textures the painting uses.
    /*0x04*/ s32 imageCount;

    /// Texture size.
    /*0x08*/ s16 textureWidth;
    /*0x0A*/ s16 textureHeight;

    /// Either PAINTING_TYPE_IMAGE or PAINTING_TYPE_ENV_MAP.
    /*0x0C*/ s8 textureType;

    /// Controls when a passive ripple starts. RIPPLE_TRIGGER_NONE, RIPPLE_TRIGGER_CONTINUOUS or RIPPLE_TRIGGER_PROXIMITY.
    /*0x0D*/ s8 rippleTrigger;

    /// Whether the painting uses shading when not rippling. Only used for Snowman's Land in vanilla and makes the transition to/from rippling not seamless.
    /*0x0E*/ s8 shaded;

    /// The painting's transparency (0..255). Determines the drawing layer of the painting.
    /*0x0F*/ Alpha alpha;

    /// By default a painting is 614.0f x 614.0f (PAINTING_SIZE).
    /*0x10*/ f32 sizeX;
    /*0x14*/ f32 sizeY;
}; /*0x18*/

/**
 * Contains the position and normal of a vertex in the painting's generated mesh.
 */
struct PaintingMeshVertex {
    /*0x00*/ Vec3s pos;
    /*0x06*/ Vec3c norm;
}; /*0x0C*/


Gfx *geo_painting_draw(s32 callContext, struct GraphNode *node, UNUSED void *context);

void bhv_painting_init(void);
void bhv_painting_loop(void);


#endif // PAINTINGS_H
