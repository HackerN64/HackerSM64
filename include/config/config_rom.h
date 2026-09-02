#pragma once

/****************
 * ROM SETTINGS *
 ****************/

/**
 * Internal ROM name. NEEDS TO BE **EXACTLY** 20 CHARACTERS. Can't be 19 characters, can't be 21 characters. You can fill it with spaces.
 * The end quote should be here:               "
 */
#define INTERNAL_ROM_NAME "HackerSM64          "

/**
 * Enable the realtime clock (RTC)
 * See lib/librtc/librtc.h for RTC related functions
 */
// #define USE_RTC

/**
 * Force the game to delete any existing save data originating from a different hack. This requires INTERNAL_ROM_NAME to be unique to work properly.
 * It is recommended to enable this if any significant changes to the save file are made that could cause issues with this or other hacks.
 * NOTE: Using save editors with this define will likely just end up wiping your save, since SM64 specific save editors most likely use hardcoded save magic.
 */
// #define UNIQUE_SAVE_DATA

/**
 * Enables Rumble Pak Support.
 * Currently not recommended, as it may cause random crashes.
 */
// #define ENABLE_RUMBLE (1 || VERSION_SH)

/**
 * The maximum number of supported players/controllers. 1-4.
 * This will save performance if the player has extra unused controllers plugged in.
 * NOTE: Default is 2, maximum is 4.
 * NOTE: This needs to be at least 2 for now for gamecube controller swap to work.
 */
#define MAX_NUM_PLAYERS 2

/**
 * Informs supported emulators to default to GameCube controller inputs.
 */
// #define EMU_DEFAULT_TO_GCN

/**
 * Screen Size Defines.
 */
#define SCREEN_WIDTH  320
#define SCREEN_HEIGHT 240

/**
 * Height of the black borders at the top and bottom of the screen for NTSC Versions. You can set it to different values for console and emulator.
 * There is generally no reason to have a value other than 0 for emulator. As for console, it provides a (small) performance boost.
 */
#define BORDER_HEIGHT_CONSOLE  0
#define BORDER_HEIGHT_EMULATOR 0

/**
 * RCVI hack. Increases performance on emulator, and does nothing on console.
 * Might break on some emulators. Use at your own risk, and don't use it unless you actually need the extra performance.
 */
// #define RCVI_HACK
