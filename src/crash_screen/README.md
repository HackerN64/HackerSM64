# ![](https://i.imgur.com/CeOukzk.gif) HackerSM64 ![](https://i.imgur.com/s0LUbTo.gif)

**HackerSM64 crash screen and debugger**

by Arceveti


TODO: more/update documentation


# Pages:

## Summary
- Description of the crash and some relevant information.
- If a code crash, prints the asm line at the crash and the values of the registers involved.
- If an assert crash, shows the assert info.

## Stack trace
- Prints information for all function addresses in the thread stack.
- More accurate if `INCLUDE_DEBUG_MAP` is defined.
- TODO: Use Libdragon's stack trace functionality.

## Thread Registers
- Similar to the original "Context" page.
- Select a register to see more info.
- TODO: move thread select hear

## Disassembly
- Interprets memory as MIPS assembly code.
- Includes every MIPS III instruction.
- Branch arrows

## Memory/RAM
- Shows memory as hex or ascii (press `B` to toggle).

## Puppyprint Log
- If `PUPPYPRINT_DEBUG` is defined

## Debug Map Symbols
- If `INCLUDE_DEBUG_MAP` is defined

## Settings
- Global settings + page-specific settings
- Press `A+B` on a setting to reset it to default, or on a collapsible header to reset that section to default.
- Maybe temporary, might be changed to a popup box later

## About
- Any information that doesn't go on any other page.
- Compiler info, ROM info, Collision pool info, Misc. info, and Emulator info (uses libpl if supported).

# Popups:

## Page Select
- Press L+R together to open a page selection popup.

## Page Controls
- Press `START` to open a popup wiht a list of the controls for the current page.

## Address Select
- On certain pages, you can select an address and jump to it, with a preview of the symbol if `INCLUDE_DEBUG_MAP` is defined.

## Register Inspect
- On the Thread Registers page, you can select a register and open a popup with more info.
- `$Status`, `$Cause`, and `$FCR` have special handling to decode/document their bitfields.

## Thread Select
- List of existing threads.
- Press `A` to jump to the thread's location in memory.
- Press `START` to change the crash screen's current inspected thread.

# Other:

## Asserts:
- See `src/game/assert.h`
- Added assert condition string
- Added assert address

## Map Parser:
- Now includes static symbols, symbol types, and symbol sizes

## Memory reads
- To prevent the crash screen from crashing or freezing due to unaligned/unmapped/etc.

## UNF commpatibility
- Press `Z+START` to print the current page as UNF (still WIP on some pages)

## Recursive crashes
- In the extremely rare case of the crash screen itself crashing (currently no known crashes), it has its own system to handle crashes so that you can continue using it even after that happens.
- When the original crash screen thread (csT0) detects a crash on a game thread and opens the crash screen, a second crash screen thread (csT1) is created, which acts as a crash screen for csT0 but with the page that crashed disabled (revive page with `A+B+START`). csT1 creates csT2 which acts as a crash screen for the crash screen's crash screen, and csT0 is repurposed for csT2's crash screen.
- If the crash screen crashes and you want to return to debugging the original crashed thread from the game itself, you can select the original thread again from the Threads dropdown on the Thread Registers page.


<details><summary><h1>TODO:</h1></summary>
<p>

### General
- **Fix thread queue list getting messed up (looped) after the second crash screen crash**
- **Fix the flickering on Ares (and some other emulators) if possible.**
- **Fix .rodata symbols not appearing in debug map.**
- Detect which segments are loaded to prevent trying to disasm garbage data (eg. reading from menu segment during normal gameplay).
- Don't have all crash screen code always loaded
  - Keep in its own segment then DMA it on crash?.
    - DMA to end of RAM right before Goddard.
    - Same place as map data.
    - Determine crash screen code/data size (like goddard.txt and debug_map.txt).
  - Simplified crash screen (for HLE? or if DMA fails?).
  - Ifdef the entire crash screen?
- Finish and clean up exception macros in `asm.h`.
- Move all inline asm stuff (eg. math_util.h) to `asm.h`/`asm.c`?
- Clean up `INCLUDE_DEBUG_MAP` ifdefs as much as possible.
- Verify whether `osWritebackDCacheAll()` usage is correct.
- Make the controls list in the popup scrollable if too long.
- A page to interpret memory as an image? For texture viewing? How would wrap width work?
  - Or a mode on the memory page to interpret byte pairs as large RGBA16 squares.
- Ability to undo address select and disasm jumps?
- Controls rebinding page (necessary?)
  - Or just preset controls modes in settings?
- Should assert macros be uppercase or lowercase?
- Horizontal text scrolling should actually scroll by pixels rather than scrolling the char buffer.
  - Use scissor box?
- Implement global grid system for selection cursor stuff.
- Better UNF print combo?
- Better page revive combo?
  - Should it just be a selection on the crashed page?
  - Currently `A+B+START`
- Better page select popup combo?
  - Currently `L+R`
- Write draw commands to a buffer then read them all at once instead of drawing directly?
  - 64-bit command entries
  - 1024 commands
  - 0x2000 bytes total
- Improve or remove WRAP macro.
- Is the stuff with `$(CRASH_TEXTURE_C_FILES)` in the makefile necessary?
- On a crash screen crash, should the new crash screen automatically return to the previous position debugging the crashed game thread instead of inspecting the first crash screen thread?
- Should cs_print/cs_draw be in util folder?
- Move print specific stuff out of util files.
- Update UNF to match pages.
- Should the coprocessor enum start at 0?
### Summary page
- Show cond bit from fpcsr if pc is c.cond? Or would that be the old cond bit?
- Special crash/assert handling:
  - RCP hang/Null SPTask (what RCP info can be printed?)
    - Mention the need to restart console when this happens.
    - rcp thread register and other interface registers
  - Object bank overflow (show bhv of the object that attempted to spawn)
  - Stack overflow
- Show hex and binary data on "illegal instruction" crash.
- Add thread display from thread registers page.
- Select section to go to the relevant page.
### Stack trace page
- Use Libdragon's better stack trace functionality.
- Mention that the stack is thread-specific (show thread on page?)
### Registers page
- Extended version with a scrollable list of all registers and their full 64 bit contents (Everything from [here](https://n64.readthedocs.io/index.html) plus any other CPU/RCP registers). Thread registers on top (old context page) then all registers if scroll down.
- Scrollable reginspect.
- Status register diagnostic/interrupt bits in reginspect
- Explain thread select
  - Controls
  - Explain that it affects the stack page (and summary page?)
  - Find out what that unknown thread 0 (libultra) thread is with pri 149 is that only appears with make UNF (but not necessarily if UNF is on)
  - Single-line thread display?
- Show offsets in parse register address names mode.
- Different colors for register names from parsed global variable names (disasm page too)?
- Multiple FPCSR descriptions at once (already kinda done in reginspect).
- Better 64-bit register handling
  - Automatic bit mode check based on registers
  - Use upper bits of float registers from odd registers
- Add "hi", "lo", and "rcp" from thread context.
- Show upper/odd bits of float registers on reginspect.
- Show direct register access values.
- Switch between registers in reginspect like pages or by selecting in the background.
- Determine whether that one register is a saved value or a frame pointer.
- Add missing controls descriptions.
- Fix floats in hex mode printing the incorrect data.
- Highlight registers used in instruction at pc.
### Disasm page
- Show addresses for each row (setting).
- Multi-line pseudoinstructions if possible (ABS, BLT, BGT, BLE, NEG, NEGU, NOT, BGE, LI, LA, SGE, SGE, ADD?).
- Is it possible to include function names inline at the beginning of each function without compromising scrolling?
- Can the `insn_as_string` and `insn_name` buffers be combined?
- Implement "OVERSCAN" mode for branch arrows.
- Translucent dividers at the end of symbols (already at beginning).
- Can the bootleg "multithreading" for branch arrows be removed now that there is no longer lag with binary symbol searching?
- Reset branch arrow distance when it won't overlap instead of wrapping only after the distance reaches the end of the screen.
- Save register data types in register buffer?
### Memory view page
- Read 4 bytes as address for address select popup (requires multi-select?).
- Binary view mode (disasm already has a version of this).
- Is search functionality possible/reasonable?
- Show dividers/borders around symbols?
### Map view page
- Should moving the cursor location here also change the location in ram view and disasm?
- Jumping to an address that's not in a symbol should find the nearest symbol index and jump to there.
- Is search functionality possible/reasonable?
- Describe "type" char.
- Determine segment/linker data type from map data?
### Logs page
- Timestamps?
### Settings page
- Save all changed settings somehow?
- Confirmation dialog box to reset all to defaults.
- Jump to the page from a page group.
- Fix/remove redundant/similar settings.
- Automatically add the page-specific settings instead of being a separate array.
- Individual page settings in each page's controls/help popup box.
- Move entirely to page-specific popup?
- Can this work without a buffer for shown entries like the threads page?
- Low vs. High resolution setting.
- Physical vs. Virtual address setting?
### About page
- Can this work without a buffer for shown entries like the threads page?
- Button to cycle memory size formats (bytes/kb/mb/hex/num entries)
- Arbitrarily determine microcode name (from map symbol?)
- Clean up code
- More entries:
  - Current RTC time if RTC is enabled? or `osGetTime()`/`osGetCount()`?
  - `gGlobalTimer`?
  - Mario action?
  - Mario floor?
  - VI/etc. info
</p>
</details>
