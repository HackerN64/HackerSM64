#pragma once

/**
 * Thank you to Kaze Emanuar for these major optimizations!
 * https://www.youtube.com/watch?v=uYPH-NH3B6k
 *
 * If you are making a hack with this repo, it is recommended to credit Kaze Emanuar.
 */

/**
 * @file config.h
 * A catch-all file for configuring various bugfixes and other settings in SM64
 */

// -- ROM SETTINGS --
// Internal ROM name. NEEDS TO BE **EXACTLY** 20 CHARACTERS. Can't be 19 characters, can't be 21 characters. You can fill it with spaces.
// The end quote should be here:               "
#define INTERNAL_ROM_NAME "SUPER MARIO 64      "
// Support Rumble Pak
// Currently not recommended, as it may cause random crashes.
//#define ENABLE_RUMBLE (1 || VERSION_SH)
// Clear RAM on boot
#define CLEARRAM 1
// Screen Size Defines
#define SCREEN_WIDTH  320
#define SCREEN_HEIGHT 240
// Height of the black borders at the top and bottom of the screen for NTSC Versions. You can set it to different values for console and emulator.
// There is generally no reason to have a value other than 0 for emulator. As for console, it provides a (small) performance boost.
#define BORDER_HEIGHT_CONSOLE  0
#define BORDER_HEIGHT_EMULATOR 0

// -- GAME SETTINGS --
// Disables some mechanics that change behavior depending on hardcoded level numbers.
// You may also need to change sLevelsWithRooms in object_helpers.c
// TODO: separate this into separate defines, behavior params, or make these mechanics otherwise dynamic
//#define DISABLE_LEVEL_SPECIFIC_CHECKS
// Disable lives and hide the lives counter
#define DISABLE_LIVES
// Save number of lives to the save file (Does nothing if DISABLE_LIVES is enabled)
#define SAVE_NUM_LIVES
// The number of lives Mario starts with after a game over or starting the game for the first time.
#define DEFAULT_NUM_LIVES 4
// This can be 0..127
#define MAX_NUM_LIVES   100
// This can be 0..32767
#define MAX_NUM_COINS   999
// Air/Breath meter is separate from health meter when underwater
//#define BREATH_METER
// Number of coins to spawn the "100 coin" star. If you remove the define altogether, then there won't be a 100 coin star at all.
#define X_COIN_STAR 100
// Stars don't kick you out of the level
//#define NON_STOP_STARS
// Uncomment this if you want global star IDs (useful for creating an open world hack ala MVC)
//#define GLOBAL_STAR_IDS
// Number of possible unique model ID's (keep it higher than 256)
#define MODEL_ID_COUNT 256
// Number of supported areas per level.
#define AREA_COUNT 8
// Makes signs and NPCs easier to talk to.
#define EASIER_DIALOG_TRIGGER
// Show an "A" when Mario is able to talk [requires EASIER_DIALOG_TRIGGER]
#define DIALOG_INDICATOR
// Include the English characters that were missing from US segment2
// J, Q, V, X, Z, ¨, !, !!, ?, &, %, ., and the beta key.
// [MAKE SURE TO ALSO BUILD FROM JP/SH AND EU TO OBTAIN THE ASSETS]
// If this is disabled, backup assets will be used.
//#define COMPLETE_EN_US_SEGMENT2
/// Removes multi-language cake screen
#define EU_CUSTOM_CAKE_FIX
// Adds multiple languages to the game. Just a placeholder for the most part, because it only works with EU, and must be enabled with EU.
#define MULTILANG (0 || VERSION_EU)
// Prevents infinite death loops by always restoring Mario's health when he's warped to any kind of warp while dead.
#define PREVENT_DEATH_LOOP
// The level that the game starts in after file select
#define START_LEVEL LEVEL_CASTLE_GROUNDS

// -- EXIT COURSE SETTINGS --
// Disable exit course
//#define DISABLE_EXIT_COURSE
// Decides whether you can exit course while moving (has no effect if you disable exit course)
#define EXIT_COURSE_WHILE_MOVING
// Decides which level "exit course" takes you to (has no effect if you disable exit course)
#define EXIT_COURSE_LEVEL LEVEL_CASTLE
// Decides the area node "exit course" takes you to (has no effect if you disable exit course)
#define EXIT_COURSE_AREA 0x01
// Decides the warp node "exit course" takes you to (has no effect if you disable exit course)
#define EXIT_COURSE_NODE 0x1F

// -- MOVEMENT SETTINGS --
// Fixes Mario's turn ground radius by making it dependent on the analog stick magnitude.
#define GROUND_TURN_FIX
// Flips Mario around when running backwards really fast.
// This can happen when sliding backwards off a slope onto a floor.
//#define GROUND_SPEED_FLIP
// Improved hanging:
// - Doesn't require holding down the A button
// - Percise turning control
// - Preventis falling from the edges
#define BETTER_HANGING
// Change the movement speed when hanging from a ceiling (the vanilla value is 4.0f, has no effect if BETTER_HANGING is enabled)
#define HANGING_SPEED 12.0f
// Disables fall damage
#define NO_FALL_DAMAGE
// Disables the scream that mario makes when falling off a great height (this is separate from actual fall damage)
//#define NO_FALL_DAMAGE_SOUND
// Fall height for normal fall damage. Vanilla is 1150.0f
#define FALL_DAMAGE_HEIGHT_SMALL 1150.0f
// Fall height for double fall damage. Vanilla is 3000.0f
#define FALL_DAMAGE_HEIGHT_LARGE 3000.0f
// Disables Mario getting stuck in snow and sand when falling
//#define NO_GETTING_BURIED
// Detect Mario's collision with lava regardless of action
//#define LAVA_INTERACTION_FIX
// Platform displacement 2 also known as momentum patch. Makes Mario keep the momemtum from moving platforms. Doesn't break treadmills anymore!
#define PLATFORM_DISPLACEMENT_2
// Use Shindou's pole behavior
//#define SHINDOU_POLES
// Mario can swing around poles and jump off them while swinging.
#define POLE_SWING
// If A and Z are pressed on the same frame, Mario will long jump instead of ground pound.
#define EASIER_LONG_JUMPS
// Holding A while bouncing on an enemy will bounce Mario higher
#define BETTER_BOUNCE
// Hold Z while twirling to descend faster
#define Z_TWIRL
// Prevents bonks when ground pounding next to a wall
#define GROUND_POUND_WALL_FIX
// Allows Mario to jump kick on steep surfaces that are set to be non slippery, instead of being forced to dive
#define JUMP_KICK_FIX
// Allow Mario to grab hangable ceilings from any state
#define HANGING_FIX
// The last frame that will be considered a firsty when wallkicking
#define FIRSTY_LAST_FRAME 1
// The maximum angle the player can wall kick, in degrees. 0..90. To allow 45 degree wall kicks, you must supply `46` to allow 45 and under.
#define WALL_KICK_DEGREES 45
// Disable BLJs and crush SimpleFlips's dreams
//#define DISABLE_BLJ

// -- COLLISION SETTINGS --
// Reduces some find_floor calls, at the cost of some barely noticeable smoothness in Mario's visual movement in a few actions at higher speeds.
// The defined number is the forward speed threshold before the change is active, since it's only noticeable at lower speeds.
#define FAST_FLOOR_ALIGN 10
// Automatically calculate the optimal collision distance for an object based on its vertices.
#define AUTO_COLLISION_DISTANCE
// Allow all surfaces types to have force, (doesn't require setting force, just allows it to be optional).
#define ALL_SURFACES_HAVE_FORCE
// Number of walls that can push Mario at once. Vanilla is 4.
#define MAX_REFEREMCED_WALLS 4
// Collision data is the type that the collision system uses. All data by default is stored as an s16, but you may change it to s32.
// Naturally, that would double the size of all collision data, but would allow you to use 32 bit values instead of 16.
// Rooms are s8 in vanilla, but if you somehow have more than 255 rooms, you may raise this number.
// Currently, they *must* say as s8, because the room tables generated by literally anything are explicitly u8 and don't use a macro, making this currently infeasable.
#define COLLISION_DATA_TYPE s16
#define ROOM_DATA_TYPE s8

// -- SPECIFIC OBJECT SETTINGS --
// Moving Coins flicker and disappear when they hit lava instead of being instantly deleted.
#define COIN_LAVA_FLICKER
// Allow for retries on collecting the remaining blue coins from a blue coin switch.
#define BLUE_COIN_SWITCH_RETRY
// Fixes shell cancel
//#define SHELL_CANCEL_FIX
// Fix DDD water rings by checking for interaction rather than normals.
#define FIX_WATER_RINGS
// Use intendedYaw to control Hoot instead of raw left and right inputs.
#define HOOT_YAW_FIX
// Leaf particles occasionally fall from trees which contain Hoot.
#define HOOT_TREE_PARTICLES
// Tiny Goombas (from THI) always drop their coin.
#define TINY_GOOMBA_ALWAYS_DROPS_COIN
// Collecting a 1-Up Mushroom will fully heal Mario.
#define MUSHROOMS_HEAL
// Collecting a Power Star will fully heal Mario.
#define POWER_STARS_HEAL
// The speed of a platform on a track can be controlled by standing near the front or back of it
//#define CONTROLLABLE_PLATFORM_SPEED
// The number of chain balls the Chain Chomp has.  Vanilla is 5.
#define CHAIN_CHOMP_NUM_SEGMENTS 5
// The number of parts Pokey has, including the head. Vanilla is 5, max is 30.
#define POKEY_NUM_SEGMENTS       5
// The number of segments Wiggler has, not including the head. Vanilla is 4.
#define WIGGLER_NUM_SEGMENTS     4
// Floombas! These fellas sport custom behaviors (bhvFloomba, bhvFloombaTripletSpawner) and of course the iconic Floomba texture.
// Also support macros (macro_floomba, macro_huge_floomba, macro_tiny_floomba, macro_floomba_triplet_spawner, macro_floomba_quintuplet_spawner).
#define FLOOMBAS

// -- CUTSCENE SKIPS --
// Skip peach letter cutscene
#define PEACH_SKIP
// Uncomment this if you want to skip the title screen (Super Mario 64 logo)
//#define SKIP_TITLE_SCREEN
// Uncomment this if you want to keep the mario head and not skip it
//#define KEEP_MARIO_HEAD
// Goddard easter egg from Shindou (has no effect if KEEP_MARIO_HEAD is disabled)
#define GODDARD_EASTER_EGG
// Disables the demo that plays when idle on the start screen (has no effect if KEEP_MARIO_HEAD is disabled)
#define DISABLE_DEMO

// -- CAMERA SETTINGS --
// Remove course specific camera processing
#define CAMERA_FIX
// Makes the camera approach Mario's height much more quickly
#define FAST_VERTICAL_CAMERA_MOVEMENT
// Enables "parallel lakitu camera" or "aglab cam" which lets you move the camera smoothly with the dpad
#define PARALLEL_LAKITU_CAM
// Enables Puppy Camera 2, a rewritten camera that can be freely configured and modified.
//#define PUPPYCAM

// -- GRAPHICS SETTINGS --
// Use HD versions of the intro splash screen textures. This includes "Made with HackerSM64".
#define HD_INTRO_TEXTURES
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
// Similar to the above, but 30 FPS (Textures by InTheBeef, cleaned up by Arceveti)
#define IA8_30FPS_COINS
// Mario's silhouette when behind solid objects/surfaces
// Also enables new render layers, such as LAYER_ALPHA_DECAL.
// The number is the intensity of the silhouette, from 0-255.
// NOTE: The overlap between Mario's model parts is visible on certain HLE plugins.
// Also, this also disables anti-aliasing on Mario.
#define SILHOUETTE 127
// Use 64x64 quarter shadow textures (Vanilla are 16x16)
//#define HD_SHADOWS
// Makes certain objects (mainly trees) transparent when the camera gets close
#define OBJ_OPACITY_BY_CAM_DIST
// Fixes the game reading the ia8 burn smoke texture as an rgba16
#define BURN_SMOKE_FIX
// Disable the fix to Koopa's unshelled model
#define KOOPA_KEEP_PINK_SHORTS
// Lightweight directional lighting engine by Fazana. Intended for giving proximity and positional pointlights to small objects.
// NOTE: Stil breaks occasionally, and PUPPYLIGHT_NODE doesn't work in areas that aren't area 1.
//#define PUPPYLIGHTS

// -- AUDIO SETTINGS --
// Fixes the castle music sometimes triggering after getting a dialog
#define CASTLE_MUSIC_FIX
// Increase audio heap size to allow for more concurrent notes to be played and for more custom sequences/banks to be imported (not supported for SH)
#define EXPAND_AUDIO_HEAP
// Use a much better implementation of reverb over vanilla's fake echo reverb. Great for caves or eerie levels, as well as just a better audio experience in general.
// Reverb parameters can be configured in audio/synthesis.c to meet desired aesthetic/performance needs. Currently US/JP only.
//#define BETTER_REVERB

// -- DEBUG SETTINGS --
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
// Include Puppyprint, a display library for text and large images. Also includes a custom, enhanced performance profiler.
#define PUPPYPRINT
#define PUPPYPRINT_DEBUG 1
// Use cycles instead of microseconds
//#define PUPPYPRINT_DEBUG_CYCLES
// Visual debug enables some collision visuals. Tapping Right on the dpad will cycle between visual hitboxes, visual surfaces, both, and neither.
// If puppyprint is enabled, then this can be cycled only while the screen is active.
//#define VISUAL_DEBUG
// Open all courses and doors. Used for debugging purposes to unlock all content.
//#define UNLOCK_ALL
// Forces a crash when the game starts. Useful for debugging the crash screen.
//#define DEBUG_FORCE_CRASH_ON_BOOT

// If you want to change the extended boundaries mode, go to engine/extended_bounds.h and change EXTENDED_BOUNDS_MODE

// -- Compatibility safeguards. Don't mess with these unless you know what you're doing.--
#ifndef TARGET_N64
#define BORDER_HEIGHT_CONSOLE  0
#define BORDER_HEIGHT_EMULATOR 0
#endif // !TARGET_N64
#ifdef DISABLE_LIVES
#undef SAVE_NUM_LIVES
#endif // DISABLE_LIVES
#ifdef DISABLE_EXIT_COURSE
#undef EXIT_COURSE_WHILE_MOVING
#undef EXIT_COURSE_LEVEL
#undef EXIT_COURSE_AREA
#undef EXIT_COURSE_NODE
#endif // DISABLE_EXIT_COURSE
#ifndef KEEP_MARIO_HEAD
#undef GODDARD_EASTER_EGG
#define DISABLE_DEMO
#endif // !KEEP_MARIO_HEAD
