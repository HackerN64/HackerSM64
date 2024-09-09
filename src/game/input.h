#pragma once

#include "game_init.h"
#include "main.h"
#include "crash_screen/cs_main.h"


extern void read_controller_inputs(OSId threadID);


ALWAYS_INLINE void handle_input(UNUSED OSMesg* mesg) {
    if (gControllerBits) {
#ifdef ENABLE_RUMBLE
        block_until_rumble_pak_free();
#endif
        osContStartReadDataEx(&gSIEventMesgQueue);
    }

    read_controller_inputs(gActiveCSThreadInfo->thread.id);
}
