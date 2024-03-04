#ifndef INTRO_GEO_H
#define INTRO_GEO_H

#include <PR/ultratypes.h>
#include <PR/gbi.h>

#include "types.h"
#include "engine/graph_node.h"

enum IntroContexts {
    INTRO_CONTEXT_NORMAL,
    INTRO_CONTEXT_GAME_OVER,
};

Gfx *geo_intro_super_mario_64_logo(s32 callContext, struct GraphNode *node, UNUSED void *context);
Gfx *geo_intro_tm_copyright(s32 callContext, struct GraphNode *node, UNUSED void *context);
Gfx *geo_intro_regular_backdrop(s32 callContext, struct GraphNode *node, UNUSED void *context);
Gfx *geo_intro_gameover_backdrop(s32 callContext, struct GraphNode *node, UNUSED void *context);

#ifdef GODDARD_EASTER_EGG
Gfx *geo_intro_face_easter_egg(s32 callContext, struct GraphNode *node, UNUSED void *context);
#endif
#ifdef ENABLE_RUMBLE
#define RUMBLE_TEXT_W 51
#define RUMBLE_TEXT_H 24
#define RUMBLE_CONT_W 29
#define RUMBLE_CONT_H 24

#define RUMBLE_TEXT_X 220
#define RUMBLE_TEXT_Y 200
#define RUMBLE_CONT_X (RUMBLE_TEXT_X + RUMBLE_TEXT_W)
#define RUMBLE_CONT_Y RUMBLE_TEXT_Y

Gfx *geo_intro_rumble_pak_graphic(s32 callContext, struct GraphNode *node, UNUSED void *context);
#endif

#endif // INTRO_GEO_H
