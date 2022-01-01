#pragma once

/******************
 * DEBUG SETTINGS *
 ******************/

// Enables most debug options, including PUPPYPRINT_DEBUG.
// #define DEBUG_ALL

// Disables all debug options (except PUPPYPRINT)
#define DISABLE_ALL

// TEST LEVEL
// Uncomment this define and set a test level in order to boot straight into said level.
// This allows you to quickly test the level you're working on.
// If you want the game to boot normally, just comment out the define again.
// #define TEST_LEVEL LEVEL_BOB

// Enable debug level select. Hold L while the game boots to turn it on.
#define DEBUG_LEVEL_SELECT

// Enable debug free move (DPad up to enter, A to exit).
#define ENABLE_DEBUG_FREE_MOVE

// Include a custom, enhanced performance profiler. (Enables PUPPYPRINT by default in config_safeguards).
// #define PUPPYPRINT_DEBUG 1

// Use cycles instead of microseconds in Puppyprint debug output.
// #define PUPPYPRINT_DEBUG_CYCLES

// A vanilla style debug mode. It doesn't rely on a text engine, but it's much less powerful that PUPPYPRINT_DEBUG. 
// Press DPAD left to show the debug UI.
// #define VANILLA_STYLE_CUSTOM_DEBUG

// Visual debug enables some collision visuals. Tapping Right on the dpad will cycle between visual hitboxes, visual surfaces, both, and neither.
// If puppyprint is enabled, then this can be cycled only while the screen is active.
// #define VISUAL_DEBUG

// Open all courses and doors. Used for debugging purposes to unlock all content.
#define UNLOCK_ALL

// Same as above, but also reads all save file flags as complete.
// This will not overwrite existing save file data unless you save over it.
// #define COMPLETE_SAVE_FILE

// Removes the limit on FPS
// #define UNLOCK_FPS

// Include vanilla debug functionality.
// #define VANILLA_DEBUG

// Forces a crash when the game starts. Useful for debugging the crash screen.
// #define DEBUG_FORCE_CRASH_ON_BOOT
