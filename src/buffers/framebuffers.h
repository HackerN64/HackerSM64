#ifndef FRAMEBUFFERS_H
#define FRAMEBUFFERS_H

#include <PR/ultratypes.h>

#include "config.h"
#include "types.h"

// level_script.c assumes that the frame buffers are adjacent, while game.c's
// -g codegen implies that they are separate variables. This is impossible to
// reconcile without undefined behavior. Avoid that when possible.
extern RGBA16 gFrameBuffers[3][SCREEN_WIDTH * SCREEN_HEIGHT];
#define gFrameBuffer0 gFrameBuffers[0]
#define gFrameBuffer1 gFrameBuffers[1]
#define gFrameBuffer2 gFrameBuffers[2]

#endif // FRAMEBUFFERS_H
