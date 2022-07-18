#include "config.h"
#include "types.h"

#ifdef CRASH_SCREEN_CRASH_SCREEN

// 320x240 I4 image
ALIGNED8 const Texture texture_crash_screen_crashed[] = {
    #include "textures/crash_custom/crash_screen_crashed.i4.inc.c"
};

#endif
