#pragma once

/*********************
 * GRAPHICS SETTINGS *
 *********************/

// The size of the master display list (gDisplayListHead). 6400 is vanilla.
#define GFX_POOL_SIZE 10000

// Show a watermark on the title screen that reads "Made with HackerSM64", instead of the copyright message.
#define INTRO_CREDIT

// Spawns floombas on the title screen.
#define INTRO_FLOOMBAS

// Enables widescreen (16:9) support.
#define WIDE

// Skybox size modifier, changing this will add support for larger skybox images. NOTE: Vanilla skyboxes may break if you change this option. Be sure to rescale them accordingly.
// Whenever you change this, make sure to run "make -C tools clean" to rebuild the skybox tool (alternatively go into skyconv.c and change the file in any way (like adding/deleting a space) to specifically rebuild that tool).
// When increasing this, you should probably also increase the GFX pool size (the GFX_POOL_SIZE define above).
#define SKYBOX_SIZE 1

// When this option is enabled, LODs will ONLY work on console.
// When this option is disabled, LODs will work regardless of whether console or emulator is used.
// Regardless of whether this setting is enabled or not, you can use gIsConsole to wrap your own code in a console check.
#define AUTO_LOD

// Enables Puppyprint, a display library for text and large images.
// Automatically enabled when PUPPYPRINT_DEBUG is enabled.
// #define PUPPYPRINT

// Disables AA (Improves console performance but causes visible seams between unconnected geometry).
// #define DISABLE_AA

// Makes the coins ia8 64x64 instead of ia16 32x32. Uses new ia8 textures so that vanilla coins look better.
#define IA8_COINS

// Similar to the above, but 30 FPS (Textures by InTheBeef, cleaned up by Arceveti).
#define IA8_30FPS_COINS

// Use .rej microcode for certain objects (experimental - only should be used when F3DEX_GBI_2 is defined).
// For advanced users only. Does not work perfectly out the box, best used when exported actor models are
// using 64 vertex sizes, offered by Fast64 in the microcode menu.
// #define OBJECTS_REJ

/**
 * Mario's silhouette when behind solid objects/surfaces.
 * Also enables new render layers, such as LAYER_ALPHA_DECAL.
 * The number is the intensity of the silhouette, from 0-255.
 * NOTE: The overlap between Mario's model parts is visible on certain HLE plugins.
 * Also, this also disables anti-aliasing on Mario.
 * NOTE: Regarding performance, silhouette seems to lose 1-2 frames on console.
 */
// #define SILHOUETTE 127

// Use 64x64 quarter shadow textures (Vanilla are 16x16).
#define HD_SHADOWS

// Makes certain objects (mainly trees) transparent when the camera gets close.
// #define OBJ_OPACITY_BY_CAM_DIST

// Fixes the game reading the ia8 burn smoke texture as an rgba16.
#define BURN_SMOKE_FIX

// Disables the fix to Koopa's unshelled model.
#define KOOPA_KEEP_PINK_SHORTS

// Uses the star object's model in the star dance cutscene.
// This has a side effect of making the star dance star also transparent when Mario collects a transparent star.
// #define STAR_DANCE_USES_STARS_MODEL

// Lightweight directional lighting engine by Fazana. Intended for giving proximity and positional pointlights to small objects.
// NOTE: Still breaks occasionally, and PUPPYLIGHT_NODE might not work in areas that aren't area 1.
// #define PUPPYLIGHTS

// Uses the correct "up" vector for the guLookAtReflect call in geo_process_master_list_sub.
// It is sideways in vanilla, and since vanilla's environment map textures are sideways too, those will appear as sideways in-game if this is enabled.
// Make sure your custom environment map textures are the correct orientation.
// #define FIX_REFLECT_MTX

// This improves performance a bit, and does not seem to break anything.
#define DISABLE_GRAPH_NODE_TYPE_FUNCTIONAL

// Disables all object shadows. You'll probably only want this either as a last resort for performance or if you're making a super stylized hack.
// #define DISABLE_SHADOWS

// Uses old shadow IDs for Fast64 compatibility. This is a temporary fix until Fast64 is updated to use the enum defines.
// NOTE: When this is enabled, The 49th hardcoded rectangle shadow will act as a regular circular shadow, due to Mario's shadow ID being 99 in vanilla.
#define LEGACY_SHADOW_IDS
