# UltraSM64-extbounds
**AFTER CLONING THE REPO, CHECK OUT THE `include/config.h` FILE BEFORE ANYTHING ELSE! IT THERE'S A LOT OF STUFF IN THIS REPO THAT CAN BE TOGGLED THERE.**

This repo needs gcc in order to be able to build it. To install it, run `sudo apt install gcc-mips-linux-gnu`

This is a fork of the ultrasm64 repo by CrashOveride which includes the following commonly used patches (patches marked with `*` are toggleable in `config.h`): 
- slope fix
- exposed ceilings fix
- No false ledgegrabs fix * 
- Jump kick fix * 
- Instant Input patch by Wiseguy (Removes all input lag caused by good emulators and plugins)
- pole fix
- Mario head skip *
- Peach letter cutscene skip *
- better extended boundaries by anonymous_moose
- water surface type patch by thecozies
- platform displacement 2 by arthur. 
- FPS counter (use the function `print_fps(x,y)` anywhere that runs code every frame)
- Automatic console/emulator detection. If emulator is detected, LODs are disabled. *
- Rounded corners by Frame, merged by Cheezepin
- Widescreen (16:9) support toggleable by pressing `L` in the pause menu. *
- Removed course-specific camera processing *
- Increased maximum pole lenght (The game will read bparam1 and bparam2 together as a single value, so you can have a very long pole) *
- bparam4 fix (the game no longer uses bparam4 to check if an object is mario and therefore you can safely use it)
- Instant warp offset fix (makes the instant warp offset work even when warping to a different area) *
- Global star IDs (disabled by default, toggleable in config.h). This allows you to create an open world (MVC-style) hack.
- Included `actors/group0.c` in `behavior_data.c`
- 16 bit model IDs by someone2639. This means you can have up to 65536 models (lol)
- s2dex engine by someone2639! To use it, compile with `make TEXT_ENGINE=s2dex_text_engine` or just set `TEXT_ENGINE` to `s2dex_text_engine` in the makefile.
- coordinate overflow fix by falcobuster 
  - If you're planning on making maps bigger than 2x bounds, change the value of `WORLD_SCALE` in `config.h` to a bigger value. `3.5f` should be enough for 4x boundaries but you can go up to `4.0f` if you somehow still get rendering glitches on your map.

It also uncringes the way that apply_patch.sh works, and removes the black border.
# UltraSM64

- This repo contains a full decompilation of Super Mario 64 (J), (U), (E), and (SH).
- Naming and documentation of the source code and data structures are in progress.
- It has been edited to allow for the usage of the final "N64 OS" library, version ``2.0L``
- Shindou Rumble Pak code is on for all regions.
- Targeting the iQue Player is supported.
- Saving to 32kbyte/256kbit SRAM is supported.
- Newer compression options are supported.
- UNFLoader (flashcart USB library) is supported, allowing for debugging on EverDrive/64Drive.
- It has been patched with someone2639's shiftable segments patch
- Getting HVQM FMV support to work with the game is WIP.

## UNFLoader support

The repository supports UNFLoader for debugging.

To build with UNF, run make with ``UNF=1``.

Further instructions can be found at the [official repository](https://github.com/buu342/N64-UNFLoader)

## Multi-Save support

The repository supports SRAM in addition to EEPROM. The standard save data functions are #ifdef'd to accommedate this.

To build with SRAM support, run make with ``SAVETYPE=sram``.

I may attempt FlashRAM in the future.

## Multi-Console support

The repository supports targeting the iQue Player in addition to the N64. The iQue libultra is ***NOT*** compatible with N64 in many ways, so it is currently NOT possible to have one build for both consoles.

To target iQue, run make with the ``CONSOLE=bb`` argument.

## Compression

The repository supports using DEFLATE compression instead of Nintendo's Yay0. This boasts a better compression ratio, but at a slight cost to load times.

On average I'd estimate that the bottleneck on decompression is about 1-2 seconds.

To switch to gzip, run make with the ``COMPRESS=gzip`` argument.

The repo also supports gziping with ``libdeflate-gzip``. This compresses at a slightly better ratio than standard ``gzip``, with no real downside from a decompression standpoint.

To use ``libdeflate-gzip``, first clone the [repo](https://github.com/ebiggers/libdeflate), then make and make install it.

Then run make for sm64 with ``GZIPVER=libdef`` in addition to ``COMPRESS=gzip``

The repo also supports RNC (Rob Northen Compression). RNC has two methods. 

Method 1 is designed to compress as small as possible, while method 2 is designed so that decompression is as fast as possible.

Both methods are fast. Method 1 has better compression than 2, so I suggest using method 1 if using RNC.

To switch to RNC, run make with either ``COMPRESS=rnc1`` or ``COMPRESS=rnc2``, depending on preferred method.

The repo also supports building a ROM with no compression.

This is not recommended as it increases ROM size significantly, with little point other than load times decreased to almost nothing.

To switch to no compression, run make with the ``COMPRESS=uncomp`` argument.


## FAQ

Q: Why in the hell are you bundling your own build of ``ld``?

A: Newer binutils (Like the one bundled with Ubuntu, 2.34) break linking with libultra builds due to local asm symbols.

This puts me at a crossroads of either touching leaked code and requiring GCC, or just using an older linker that works just fine.

I went with the latter.

Thanks to "someone2639" for this hacky-ass idea

Q: Will this allow me to use FlashRAM/Transfer Pak/microcode swapping/Other Cool N64 Features?

A: Theoretically, all yes.

## Installation help

Go read the original repo README.md
