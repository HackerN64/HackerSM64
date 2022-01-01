#ifndef LEVEL_GEO_H
#define LEVEL_GEO_H

#include <PR/ultratypes.h>
#include <PR/gbi.h>

enum EnvFxMode {
    /* 0*/ ENVFX_MODE_NONE,         // no effects
    /* 1*/ ENVFX_SNOW_NORMAL,       // CCM, SL
    /* 2*/ ENVFX_SNOW_WATER,        // Secret Aquarium, Sunken Ship
    /* 3*/ ENVFX_SNOW_BLIZZARD,     // unused
    /*10*/ ENVFX_BUBBLE_START = 10, // Separates snow effects and flower/bubble effects
    /*11*/ ENVFX_FLOWERS,           // unused
    /*12*/ ENVFX_LAVA_BUBBLES,      // LLL, BitFS, Bowser 2
    /*13*/ ENVFX_WHIRLPOOL_BUBBLES, // DDD
    /*14*/ ENVFX_JETSTREAM_BUBBLES  // JRB, DDD (submarine area)
};

Gfx *geo_envfx_main(s32 callContext, struct GraphNode *node, Mat4 mtxf);
Gfx *geo_skybox_main(s32 callContext, struct GraphNode *node, UNUSED Mat4 *mtx);

#endif // LEVEL_GEO_H
