#ifndef CONFIG_H
#define CONFIG_H
// ULTRASM64-EXTBOUNDS CONFIG FLAGS NEAR BOTTOM
/**
 * @file config.h
 * A catch-all file for configuring various bugfixes and other settings (maybe eventually) in SM64
 */

// Bug Fixes
// --| Post-JP Version Nintendo Bug Fixes
/// Fixes bug where obtaining over 999 coins sets the number of lives to 999 (or -25)
#define BUGFIX_MAX_LIVES (0 || VERSION_US || VERSION_EU || VERSION_SH)
/// Fixes bug where the Boss music won't fade out after defeating King Bob-omb
#define BUGFIX_KING_BOB_OMB_FADE_MUSIC (0 || VERSION_US || VERSION_EU || VERSION_SH)
/// Fixes bug in Bob-Omb Battlefield where entering a warp stops the Koopa race music
#define BUGFIX_KOOPA_RACE_MUSIC (0 || VERSION_US || VERSION_EU || VERSION_SH)
/// Fixes bug where Piranha Plants do not reset their action state when the
/// player exits their activation radius.
#define BUGFIX_PIRANHA_PLANT_STATE_RESET (0 || VERSION_US || VERSION_EU || VERSION_SH)
/// Fixes bug where sleeping Piranha Plants damage players that bump into them
#define BUGFIX_PIRANHA_PLANT_SLEEP_DAMAGE (0 || VERSION_US || VERSION_SH)
/// Fixes bug where it shows a star when you grab a key in bowser battle stages
#define BUGFIX_STAR_BOWSER_KEY (0 || VERSION_US || VERSION_EU || VERSION_SH)
/// Fixes bug that enables Mario in time stop even if is not ready to speak
#define BUGFIX_DIALOG_TIME_STOP (0 || VERSION_US || VERSION_EU || VERSION_SH)
/// Fixes bug that causes Mario to still collide with Bowser in BITS after his defeat
#define BUGFIX_BOWSER_COLLIDE_BITS_DEAD (0 || VERSION_US || VERSION_EU || VERSION_SH)
/// Fixes bug where Bowser wouldn't reset his speed when fallen off (and adds missing checks)
#define BUGFIX_BOWSER_FALLEN_OFF_STAGE (0 || VERSION_US || VERSION_EU || VERSION_SH)
/// Fixes bug where Bowser would look weird while fading out
#define BUGFIX_BOWSER_FADING_OUT (0 || VERSION_US || VERSION_EU || VERSION_SH)
/// Removes multi-language cake screen
#define EU_CUSTOM_CAKE_FIX 1

// Support Rumble Pak
// Currently not recommended, as it may cause random crashes.
//#define ENABLE_RUMBLE (1 || VERSION_SH)

// Clear RAM on boot
#define CLEARRAM 1

// Screen Size Defines
#define SCREEN_WIDTH 320
#define SCREEN_HEIGHT 240

// Border Height Define for NTSC Versions
#ifdef TARGET_N64
// Size of the black border at the top and bottom of the screen. You can set it to different values for console and emulator.
// There is generally no reason to have a value other than 0 for emulator. As for console, it provides a (small) performance boost.
#define BORDER_HEIGHT_CONSOLE 0
#define BORDER_HEIGHT_EMULATOR 0

#endif

// -- ultrasm64-extbounds specific settings --

// TEST LEVEL
// Uncomment this define and set a test level in order to boot straight into said level.
// This allows you to quickly test the level you're working on.
// If you want the game to boot normally, just comment out the define again.
//#define TEST_LEVEL LEVEL_BOB

// COMMON HACK CHANGES
// Internal ROM name. NEEDS TO BE **EXACTLY** 20 CHARACTERS. Can't be 19 characters, can't be 21 characters. You can fill it with spaces.
// The end quote should be here:               "
#define INTERNAL_ROM_NAME "SUPERMARIO64        "
// Disable lives and hide the lives counter
#define DISABLE_LIVES
// Skip peach letter cutscene
#define PEACH_SKIP
// Fixes the castle music sometimes triggering after getting a dialog
#define CASTLE_MUSIC_FIX
// Remove course specific camera processing
#define CAMERA_FIX
// Change the movement speed when hanging from a ceiling (the vanilla value is 4.f)
#define HANGING_SPEED 12.f
// Makes Mario face the direction of the analog stick directly while hanging from a ceiling, without doing "semicircles"
#define TIGHTER_HANGING_CONTROLS
// Makes Mario turn around instantly when moving on the ground
//#define SUPER_RESPONSIVE_CONTROLS
// Disables fall damage
#define NO_FALL_DAMAGE
// Disables the scream that mario makes when falling off a great height (this is separate from actual fall damage)
//#define NO_FALL_DAMAGE_SOUND
// Number of coins to spawn the "100 coin" star. If you remove the define altogether, then there won't be a 100 coin star at all.
#define X_COIN_STAR 100
// Platform displacement 2 also known as momentum patch. Makes Mario keep the momemtum from moving platforms. Doesn't break treadmills anymore!
#define PLATFORM_DISPLACEMENT_2
// Stars don't kick you out of the level
// #define NON_STOP_STARS
// Uncomment this if you want global star IDs (useful for creating an open world hack ala MVC)
//#define GLOBAL_STAR_IDS
// Uncomment this if you want to skip the title screen (Super Mario 64 logo)
//#define SKIP_TITLE_SCREEN
// Uncomment this if you want to keep the mario head and not skip it
//#define KEEP_MARIO_HEAD
// Enables "parallel lakitu camera" or "aglab cam" which lets you move the camera smoothly with the dpad
#define PARALLEL_LAKITU_CAM
// Allows Mario to ledgegrab sloped floors
#define NO_FALSE_LEDGEGRABS
//Adds multiple languages to the game. Just a placeholder for the most part, because it only works with EU, and must be enabled with EU.
#define MULTILANG (0 || VERSION_EU)
//Enables Puppy Camera 2, a rewritten camera that can be freely configured and modified.
#define PUPPYCAM


// HACKER QOL
// Increase the maximum pole length (it will treat bparam1 and bparam2 as a single value)
#define LONGER_POLES
// Number of possible unique model ID's (keep it higher than 256)
#define MODEL_ID_COUNT 256
// Increase audio heap size to allow for more concurrent notes to be played and for more custom sequences/banks to be imported (does nothing with EU and SH versions)
#define EXPAND_AUDIO_HEAP
// Allow all surfaces types to have force, (doesn't require setting force, just allows it to be optional).
#define ALL_SURFACES_HAVE_FORCE
// Custom debug mode. Press DPAD left to show the debug UI. Press DPAD right to enter the noclip mode.
//#define CUSTOM_DEBUG
// Include Puppyprint, a display library for text and large images. Also includes a custom, enhanced performance profiler.
#define PUPPYPRINT
#define PUPPYPRINT_DEBUG 1

// BUG/GAME QOL FIXES
// Fix instant warp offset not working when warping across different areas
#define INSTANT_WARP_OFFSET_FIX
// Whether a tree uses snow particles or not is decided via the model IDs instead of the course number
#define TREE_PARTICLE_FIX
// Allows Mario to jump kick on steep surfaces that are set to be non slippery, instead of being forced to dive
#define JUMP_KICK_FIX
// Allow Mario to grab hangable ceilings from any state
#define HANGING_FIX
// The last frame that will be considered a firsty when wallkicking
#define FIRSTY_LAST_FRAME 1
// 46 degree walkicks
//#define WALLKICKS_46_DEGREES
// Disable BLJs and crush SimpleFlips's dreams
//#define DISABLE_BLJ

// RELATING TO EXIT COURSE
// Disable exit course
//#define DISABLE_EXIT_COURSE
// Decides whether you can exit course while moving (has no effect if you disable exit course)
//#define EXIT_COURSE_WHILE_MOVING
// Decides which level "exit course" takes you to (has no effect if you disable exit course)
#define EXIT_COURSE_LEVEL LEVEL_CASTLE
// Decides the area node "exit course" takes you to (has no effect if you disable exit course)
#define EXIT_COURSE_AREA 0x01
// Decides the warp node "exit course" takes you to (has no effect if you disable exit course)
#define EXIT_COURSE_NODE 0x1F

// OTHER ENHANCEMENTS
// Enable widescreen (16:9) support
#define WIDE
// Skybox size modifier, changing this will add support for larger skybox images. NOTE: Vanilla skyboxes may break if you change this option. Be sure to rescale them accordingly.
// Whenever you change this, make sure to run "make -C tools clean" to rebuild the skybox tool (alternatively go into skyconv.c and change the file in any way (like adding/deleting a space) to specifically rebuild that tool).
// When increasing this, you should probably also increase the GFX pool size. (the GFX_POOL_SIZE define in src/game/game_init.h)
#define SKYBOX_SIZE 1
// When this option is enabled, LODs will ONLY work on console.
// When this option is disabled, LODs will work regardless of whether console or emulator is used.
// Regardless of whether this setting is enabled or not, you can use gIsConsole to wrap your own code in a console check.
#define AUTO_LOD
// Disable AA (Recommended: it changes nothing on emulator, and it makes console run better)
#define DISABLE_AA
// Makes the coins ia8 64x64 instead of ia16 32x32. Uses new ia8 textures so that vanilla coins look better.
#define IA8_COINS
// Use a much better implementation of reverb over vanilla's fake echo reverb. Great for caves or eerie levels, as well as just a better audio experience in general.
// Reverb parameters can be configured in audio/synthesis.c to meet desired aesthetic/performance needs.
//#define BETTER_REVERB

// If you want to change the extended boundaries mode, go to engine/extended_bounds.h and change EXTENDED_BOUNDS_MODE

#endif // CONFIG_H
