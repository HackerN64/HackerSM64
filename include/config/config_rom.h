#pragma once

/****************
 * ROM SETTINGS *
 ****************/

// Internal ROM name. NEEDS TO BE **EXACTLY** 20 CHARACTERS. Can't be 19 characters, can't be 21 characters. You can fill it with spaces.
// The end quote should be here:               "
#define INTERNAL_ROM_NAME "HackerSM64          "

// Enables Rumble Pak Support.
// Currently not recommended, as it may cause random crashes.
// #define ENABLE_RUMBLE (1 || VERSION_SH)

// Screen Size Defines.
#define SCREEN_WIDTH  320
#define SCREEN_HEIGHT 240

// Height of the black borders at the top and bottom of the screen for NTSC Versions. You can set it to different values for console and emulator.
// There is generally no reason to have a value other than 0 for emulator. As for console, it provides a (small) performance boost.
#define BORDER_HEIGHT_CONSOLE  0
#define BORDER_HEIGHT_EMULATOR 0
