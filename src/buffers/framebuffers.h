#ifndef FRAMEBUFFERS_H
#define FRAMEBUFFERS_H

#include <PR/ultratypes.h>

#include "config.h"
#include "types.h"

extern RGBA16 gFramebuffer0[SCREEN_WIDTH * SCREEN_HEIGHT];
extern RGBA16 gFramebuffer1[SCREEN_WIDTH * SCREEN_HEIGHT];
extern RGBA16 gFramebuffer2[SCREEN_WIDTH * SCREEN_HEIGHT];

static inline RGBA16* getFramebuffer(int i)
{
    switch (i)
    {
        case 0:
            return gFramebuffer0;
        case 1:
            return gFramebuffer1;
        case 2:
            return gFramebuffer2;
        default:
            return NULL;
    }
}

#endif // FRAMEBUFFERS_H
