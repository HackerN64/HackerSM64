#ifndef ENVFX_BUBBLES_H
#define ENVFX_BUBBLES_H

#include <PR/ultratypes.h>
#include <PR/gbi.h>

enum EnvfxBubblesState {
    ENVFX_STATE_UNUSED,
    // whirlpool / jetsream bubble source position
    ENVFX_STATE_SRC_X,
    ENVFX_STATE_SRC_Y,
    ENVFX_STATE_SRC_Z,
    // only for whirlpool, where bubbles get sucked in
    ENVFX_STATE_DEST_X,
    ENVFX_STATE_DEST_Y,
    ENVFX_STATE_DEST_Z,
    ENVFX_STATE_PARTICLECOUNT,
    // whirlpool can rotate around DEST point
    ENVFX_STATE_PITCH,
    ENVFX_STATE_YAW
};

// Used to communicate from whirlpool behavior to envfx
extern s16 gEnvFxBubbleConfig[10];
Gfx *envfx_update_bubbles(s32 mode, Vec3s marioPos, Vec3s camTo, Vec3s camFrom);

#endif // ENVFX_BUBBLES_H
