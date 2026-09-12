# ![](https://i.imgur.com/CeOukzk.gif) HackerSM64 ![](https://i.imgur.com/s0LUbTo.gif)

HackerSM64 is a commonly used base repository for Super Mario 64 ROM hacking projects. It includes many useful features for ROM hackers, improvements that are commonly applied in ROM hacks, software optimizations, and bug fixes for some gameplay annoyances.

# Setup Guide

A guide to install tools and dependencies, and to build HackerSM64 can be found [on the HackerSM64 Wiki](https://github.com/HackerN64/HackerSM64/wiki/Installing-HackerSM64).

This repo requires a US ROM in order to build. JP/EU ROMs are optional for some assets.

# Features

HackerSM64 adds a lot of new features and makes several improvements to the game engine, so there are far too many to list all of them here. Some of the more significant and often-used features/changes are listed below.

Many of these tweaks and features may be toggled by editing the files in the `include/config` folder. _It's strongly recommended that you look through these when starting a brand new project, and before doing anything else in the repo._

## Lighting Engine by Wiseguy
- Lighting Engine is NOT finished. It's a WIP. Expect issues and expect it to be somewhat hard to use. Point lights in particular are extremely finnicky. 
- In order for lighting engine to work, your materials must have Shade Color in the combiner, **but you have to DISABLE Shade Color in sources**. If you leave Shade Color ticked for even a single material, the lighting will break **for the entire mesh**. Until Fast64 gets some sort of Lighting Engine support, it's recommended you create some custom presets to make this less annoying. 
  - You can intentionally make a material fullbright by leaving Shade Color out of the combiner for that material. Useful for, say, a lamp.
- Use the function `set_directional_light` to set the directional light for the current area. It takes a `Vec3f` for the direction and three `s32` for the RGB values.
- Use the function `set_ambient_light` to set the ambient light for the current area. It takes three `s32` for the RGB values.
- You can use the function `emit_light` to emit a point light via code. It takes a `Vec3f` for the position, three `s32` for the RGB values and three `u32` for each one of the falloff types. The falloff is basically how strong the light is: the HIGHER the falloff is, the DIMMER the light will be. The three types of falloffs can be stacked, and work like this:
  - 2 for constant falloff = the point light is half as bright always.
  - 2 for linear falloff = the point light is half as bright when you double the distance
  - 2 for quadratic falloff = the point is half as bright when you double the square of the distance
- Important note regarding point lights: They are affected by WORLD_SCALE, which is set based on your extended bounds mode. Esentially, at 2x bounds lights will be twice as bright as in 1x bounds, and with 4x bounds they will be twice as bright as that. This can be dealt with simply by using high falloffs, but it can be annoying if you decide to change the extended bounds mode mid-development.
- You can also make an object emit light by using the `OBJ_FLAG_EMIT_LIGHT` object flag, as well as `SET_LIGHT_COLOR` and `SET_LIGHT_FALLOFF` in the script.

## HackerSM64 Features
### Common Hack Changes
- Extended boundaries (1x/2x/4x)
- Instant input patch (does not affect console)
- Mario head skip
- Peach letter cutscene skip
- Toggles for fall damage, lives, 100 coin stars, and other vanilla mechanics

### Features for Hackers
- Rich debug features: debug fly mode, a much improved crash screen, visual surface and object hitbox debug, live display of several game state variables, and detailed performance profiling information.
- Automatic console/emulator detection. You can use `gEmulator` and `gSystemCapabilities` to determine which emulator is being used (or if running on console), and the available features (like framebuffer emulation).
- An option for global, non-level based star IDs
- 16 bit model IDs. This means you can have up to 65536 models.
- Expanded audio heap allows for a larger concurrent note count and the importing of more m64 sequences and sound banks.
- Many general use defines for object struct members, for use in custom object behaviors. Check `include/object_fields.h` for more information.
- Puppyprint text engine (see `src/game/puppyprint.c`)
- New water surface type, which is easier to use and more flexible than vanilla water boxes
- An option to show Mario's silhouette when behind most surfaces
- Farcall TLB mapping allows to store executable code inside uncompressed segments, that can be loaded and ran as needed, instead of it having to be loaded at all times. See `include/farcall.h` for instructions and details.

### Bug Fixes/Physics Improvements
- Fixes for common collision issues, such as slope fix, exposed ceilings fix, and false ledgegrabs fix
- Improved wall collision with rounded corners, which also helps to reduce potential softlocks in custom levels

### Colored Text Support
- Add colors to text boxes by adding `@RRGGBBAA` to the text, where each letter is a hex digit representing red, green, blue, and alpha (transparency).
- Reset the color by adding `@--------`. It is not mandatory to do this, but text will need to be recolored each time it scrolls in a dialog box, or the custom color will reset.
- For example: `"@FF0000FFRED @00FF00FFGREEN @0000FFFFBLUE @FFFFFF00INVISIBLE @--------NORMAL"`

### Lighting Engine
- Allows for the use of dynamic lights in your levels.
- Ambient, directional, and point lights are supported.
- Available on a separate branch ([base/lighting-engine](https://github.com/HackerN64/HackerSM64/tree/base/lighting-engine)). Instructions on how to use it can be found in the readme of that branch.

# FAQ

### Should I use HackerSM64 or UltraSM64 for my ROM hack?
HackerSM64 is best suited for ROM hacks with custom levels and content. If you want to make a ROM hack that is TAS-accurate to the vanilla game and its levels, [UltraSM64](https://github.com/CrashOveride95/ultrasm64) may be a better choice for your project.

### How do I make my own levels?
You can use Blender with the [Fast64 plugin](https://github.com/Fast-64/fast64) to create and export levels as 3D models. 


# Still Have Questions?

If you have any questions about HackerSM64, want to discuss its development, or are having trouble with your own project that uses it, feel free to join the [HackerN64 Discord Server](https://discord.gg/brETAakcXr).

# Credits

HackerSM64 is a community effort, and many people have contributed to its features. A list of major contributors can be found in [CREDITS.md](CREDITS.md).
