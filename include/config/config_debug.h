#pragma once

/******************
 * DEBUG SETTINGS *
 ******************/

// Enables most debug options
//#define DEBUG_ALL

// TEST LEVEL
// Uncomment this define and set a test level in order to boot straight into said level.
// This allows you to quickly test the level you're working on.
// If you want the game to boot normally, just comment out the define again.
//#define TEST_LEVEL LEVEL_BOB

// Enable debug level select
//#define DEBUG_LEVEL_SELECT

// Enable debug free move (DPad up to enter, A to exit)
//#define ENABLE_DEBUG_FREE_MOVE

// Custom debug mode. Press DPAD left to show the debug UI. Press DPAD right to enter the noclip mode.
//#define CUSTOM_DEBUG

// Include Puppyprint, a display library for text and large images.
#define PUPPYPRINT
// Include a custom, enhanced performance profiler (Requires PUPPYPRINT).
#define PUPPYPRINT_DEBUG 1

// Use cycles instead of microseconds in Puppyprint debug output
//#define PUPPYPRINT_DEBUG_CYCLES

// Visual debug enables some collision visuals. Tapping Right on the dpad will cycle between visual hitboxes, visual surfaces, both, and neither.
// If puppyprint is enabled, then this can be cycled only while the screen is active.
//#define VISUAL_DEBUG

// Open all courses and doors. Used for debugging purposes to unlock all content.
//#define UNLOCK_ALL

// Forces a crash when the game starts. Useful for debugging the crash screen.
//#define DEBUG_FORCE_CRASH_ON_BOOT

// -- Compatibility safeguards. Don't mess with these unless you know what you're doing. --
#ifdef DEBUG_ALL
#define DEBUG_LEVEL_SELECT
#define ENABLE_DEBUG_FREE_MOVE
#define PUPPYPRINT
#define PUPPYPRINT_DEBUG 1
#define VISUAL_DEBUG
#define UNLOCK_ALL
#endif // DEBUG_ALL
