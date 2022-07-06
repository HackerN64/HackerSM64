#ifndef PAINTINGS_H
#define PAINTINGS_H

#include <PR/ultratypes.h>
#include <PR/gbi.h>

#include "macros.h"
#include "types.h"

/// Use to properly set a GraphNodeGenerated's parameter to point to the right painting.
/// Use this for both bparam1 and bparam2 for painting objects.
#define PAINTING_ID(id, grp) (id | (grp << 8))

/// The default painting side length.
#define PAINTING_SIZE 614.0f

/// The depth of the area in front of the painting which triggers ripples without warping.
#define PAINTING_WOBBLE_DEPTH 100.0f

/// The depth of the area behind the painting which triggers the warp.
#define PAINTING_WARP_DEPTH PAINTING_SIZE

/// The space around the edges in which Mario is still considered within painting bounds.
#define PAINTING_EDGE_MARGIN (PAINTING_SIZE / 2)

/// This is added to Mario's Y position to make the ripple closer to Mario's center of mass.
#define PAINTING_MARIO_Y_OFFSET 50.0f

// HMC painting group
enum HMCPaintingIDs {
    /*0x0*/ PAINTING_ID_HMC_COTMC,
};

// Inside Castle painting group
enum CastlePaintingIDs {
    /*0x0*/ PAINTING_ID_CASTLE_BOB,
    /*0x1*/ PAINTING_ID_CASTLE_CCM,
    /*0x2*/ PAINTING_ID_CASTLE_WF,
    /*0x3*/ PAINTING_ID_CASTLE_JRB,
    /*0x4*/ PAINTING_ID_CASTLE_LLL,
    /*0x5*/ PAINTING_ID_CASTLE_SSL,
    /*0x6*/ PAINTING_ID_CASTLE_HMC,
    /*0x7*/ PAINTING_ID_CASTLE_DDD,
    /*0x8*/ PAINTING_ID_CASTLE_WDW,
    /*0x9*/ PAINTING_ID_CASTLE_THI_TINY,
    /*0xA*/ PAINTING_ID_CASTLE_TTM,
    /*0xB*/ PAINTING_ID_CASTLE_TTC,
    /*0xC*/ PAINTING_ID_CASTLE_SL,
    /*0xD*/ PAINTING_ID_CASTLE_THI_HUGE,
    /*0xE*/ PAINTING_ID_CASTLE_RR,
};

// TTM painting group
enum TTMPaintingIDs {
    /*0x0*/ PAINTING_ID_TTM_SLIDE,
};

// Painting group ids
enum PaintingGroups {
    PAINTING_GROUP_HMC,
    PAINTING_GROUP_INSIDE_CASTLE,
    PAINTING_GROUP_TTM,
    PAINTING_NUM_GROUPS,
    PAINTING_GROUP_NULL = -1,
};

// Painting->state
enum PaintingState {
    PAINTING_IDLE,
    PAINTING_RIPPLE,
    PAINTING_ENTERED,
};

// Painting->rippleTrigger
enum RippleTriggers {
    RIPPLE_TRIGGER_PROXIMITY,
    RIPPLE_TRIGGER_CONTINUOUS,
};

// Painting->lastFlags, Painting->currFlags, Painting->changedFlags
enum PaintingRippleFlags {
    // Not rippling.
    RIPPLE_FLAGS_NONE  = (0 << 0), // 0x00
    // Triggers an entry ripple.
    RIPPLE_FLAG_ENTER  = (1 << 0), // 0x01
    // Triggers a passive ripple.
    RIPPLE_FLAG_RIPPLE = (1 << 1), // 0x02
};

// Painting->textureType
enum PaintingType {
    /// Painting that uses 1 or more images as a texture
    PAINTING_IMAGE,
    /// Painting that has one texture used for an environment map effect
    PAINTING_ENV_MAP
};

struct Painting {
    /// Id of the painting warp node.
    PaintingData id;
    /// How many images should be drawn when the painting is rippling.
    s8 imageCount;
    /// Either PAINTING_IMAGE or PAINTING_ENV_MAP
    s8 textureType;

    /// Controls how high the peaks of the ripple are.
    f32 passiveRippleMag;
    f32 entryRippleMag;

    /// Multiplier that controls how fast the ripple regresses to the IDLE state.
    f32 passiveRippleDecay;
    f32 entryRippleDecay;

    /// Controls the ripple's frequency
    f32 passiveRippleRate;
    f32 entryRippleRate;

    /// The rate at which the magnitude of the ripple decreases as you move farther from the central point of the ripple
    f32 passiveDispersionFactor;
    f32 entryDispersionFactor;

    /// Display list used when the painting is normal.
    const Gfx *normalDisplayList;
    /// Data used to map the texture to the mesh
    const PaintingData *const *textureMaps;

    // Texture data
    const Texture *const *textureArray;
    s16 textureWidth;
    s16 textureHeight;

    /// Display list used when the painting is rippling.
    const Gfx *rippleDisplayList;

    /// Controls when a passive ripple starts. RIPPLE_TRIGGER_CONTINUOUS or RIPPLE_TRIGGER_PROXIMITY.
    s8 rippleTrigger;

    /// The painting's transparency. Determines what layer the painting is in.
    Alpha alpha;

    /// Uniformly scales the painting to a multiple of PAINTING_SIZE.
    /// By default a painting is 614.0f x 614.0f
    f32 sizeX;
    f32 sizeY;
};

/**
 * Contains the position and normal of a vertex in the painting's generated mesh.
 */
struct PaintingMeshVertex {
    /*0x00*/ Vec3s pos;
    /*0x06*/ Vec3c norm;
};

extern struct PaintingMeshVertex *gPaintingMesh;
extern Vec3f *gPaintingTriNorms;
extern struct Object *gRipplingPainting;
extern struct Object *gEnteredPainting;

Gfx *geo_painting_draw(s32 callContext, struct GraphNode *node, UNUSED void *context);

void bhv_painting_init(void);
void bhv_painting_loop(void);

#endif // PAINTINGS_H
