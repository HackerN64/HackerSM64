# ![](https://i.imgur.com/CeOukzk.gif) HackerSM64 ![](https://i.imgur.com/s0LUbTo.gif)

HackerSM64 is a base for Super Mario 64 rom hack projects. It includes many useful features for rom hackers, improvements that are commonly applied in rom hacks, and bug fixes for some gameplay annoyances.

# Setup Guide

A guide to install tools and dependencies, and to build HackerSM64 can be found [on the project wiki](https://github.com/HackerN64/HackerSM64/wiki/Installing-HackerSM64).

This repo requires a US ROM in order to build. JP/EU ROMs are optional for some assets.

# Features

HackerSM64 adds a lot of new features and makes several improvements to the game engine, so there are too many to list all of them here, but the more significant and often-used features/changes are listed below.

Many of these tweaks and features can be turned on or off by editing the files in the `include/config` folder, and **it's strongly recommended that you look through those files before making any of your own changes.**

### Common Hack Changes
- Extended boundaries (1x/2x/4x)
- Instant input patch (does not affect console)
- Mario head skip
- Peach letter cutscene skip
- Toggles for fall damage, lives, 100 coin stars, and other vanilla mechanics

### Bug Fixes/Physics Improvements
- Fixes for common collision issues, like slope fix, exposed ceilings fix, false ledgegrabs fix
- Hanging fix (Mario can grab hangable ceilings from any state, instead of only jump or double jump)
- Better wall collision with rounded corners

### Lighting Engine
- Allows for the use of dynamic lights in your levels.
- Ambient, directional, and point lights are supported.
- Available on a separate branch ([base/lighting-engine](https://github.com/HackerN64/HackerSM64/tree/base/lighting-engine)). Instructions on how to use it can be found in the readme of that branch.

### Colored Text Support
- Add colors to text boxes by adding `@RRGGBBAA` to the text, where each letter is a hex digit representing red, green, blue, and alpha (transparency).
- Reset the color by adding `@--------`. It is not mandatory to do this, but text will need to be recolored each time it scrolls in a dialog box, or the custom color will reset.
- For example: `"@FF0000FFRED @00FF00FFGREEN @0000FFFFBLUE @FFFFFF00INVISIBLE @--------NORMAL"`

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

# FAQ

### Should I use HackerSM64 or UltraSM64 for my rom hack?
HackerSM64 is best used for rom hacks with custom levels and content. If you want to make a rom hack based on the levels in the original game, [UltraSM64](https://github.com/CrashOveride95/ultrasm64) may be a better choice for your project.

### How do I make my own levels?
You can use Blender with the [Fast64 plugin](https://github.com/Fast-64/fast64) to create and export levels as 3D models. 


# Still Have Questions?

If you have any questions about HackerSM64, want to discuss its development, or are having trouble with your own project that uses it, feel free to join the HackerN64 discord server: https://discord.gg/brETAakcXr

# Credits

HackerSM64 is a community effort, and many people have contributed to its features. A list of major contributors can be found in [CONTRIBUTORS.md](CONTRIBUTORS.md).