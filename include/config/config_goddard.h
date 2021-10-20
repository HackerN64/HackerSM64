#pragma goddard_conf


/********************
 * GODDARD SETTINGS *
 ********************/

// Uncomment this if you want to keep the mario head and not skip it
//#define KEEP_MARIO_HEAD

// Goddard easter egg from Shindou (has no effect if KEEP_MARIO_HEAD is disabled)
#define GODDARD_EASTER_EGG

// Disables the demo that plays when idle on the start screen (has no effect if KEEP_MARIO_HEAD is disabled)
#define DISABLE_DEMO

// -- Compatibility safeguards. Don't mess with these unless you know what you're doing.--
#ifndef KEEP_MARIO_HEAD
#undef GODDARD_EASTER_EGG
#define DISABLE_DEMO
#endif // !KEEP_MARIO_HEAD
