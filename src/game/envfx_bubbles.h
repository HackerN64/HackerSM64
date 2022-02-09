#ifndef ENVFX_BUBBLES_H
#define ENVFX_BUBBLES_H

#include <PR/ultratypes.h>
#include <PR/gbi.h>

struct EnvFxBubblesState {
    /*0x00*/ Vec3s src;   // whirlpool / jetsream bubble source position
    /*0x06*/ Vec3s dest;  // only for whirlpool, where bubbles get sucked in
    /*0x0C*/ s16 particleCount;
    /*0x0E*/ Vec3s angle; // whirlpool can rotate around DEST point
};

// Used to communicate from whirlpool behavior to envfx
extern struct EnvFxBubblesState *gEnvFxBubbleConfig;

Gfx *envfx_update_bubbles(s32 mode, Vec3s marioPos, Vec3s camTo, Vec3s camFrom);

#endif // ENVFX_BUBBLES_H
