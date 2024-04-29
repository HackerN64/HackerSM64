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
- **Fix .rodata symbols not appearing in debug map.**
- **Find out why `break` and `coprocessor unusable` exceptions don't trigger the crash screen**
  - Find out if any other exceptions have the same issue
- **Don't have all crash screen code always loaded.**
  - Keep in its own segment then DMA it on crash?.
    - DMA to end of RAM right before Goddard.
    - Same place as map data.
    - Determine crash screen code/data size (like goddard.txt and debug_map.txt).
    - Can it be DMAd to a framebuffer? The crash screen works double buffered.
  - Simplified crash screen (for HLE? or if DMA fails?).
  - Ifdef the entire crash screen?
- RSP crash screen (see libdragon).
- Finish and clean up exception macros in `asm.h`.
- Move all inline asm stuff (eg. math_util.h) to `asm.h`/`asm.c`?
- Clean up `INCLUDE_DEBUG_MAP` ifdefs as much as possible.
- Verify whether `osWritebackDCacheAll()` usage is correct.
- Make the controls list in the popup scrollable if too long.
- A page to interpret memory as an image with proper wrap width (for texture viewing).
- Ability to undo address select and disasm jumps?
- Controls rebinding page (necessary?)
  - Or just preset controls modes in settings?
- Implement global grid system for selection cursor stuff (currently only exists on Thread Registers page).
- Better page revive combo?
  - Should it just be a selection on the crashed page?
  - Currently `A+B+START`
- Better page select popup combo?
  - Currently `L+R`
- Improve or remove WRAP macro.
- Is the stuff with `$(CRASH_TEXTURE_C_FILES)` in the makefile necessary?
- On a crash screen crash, should the new crash screen automatically return to the previous position debugging the crashed game thread instead of inspecting the first crash screen thread?
- Draw multiple pixels at a time (eg. RGBA16FILL).
- Makefile rule or config define (possible?) for whether to include non-virtual symbols
  - eg. behavior and displaylist names
- "...ID" vs. "...Id" naming discrepancy
- Show data preview in address select (same as reginspect).
- Should FORCE_CRASH_AT_PTR only set the initial selected address and not the actual crash address?
#### Refactoring
- Should cs_print/cs_draw be in util folder?
- Move print specific stuff out of util files.
- Include `os_convert.h` in crash_main.h and move the framerate defines/macros to it.
#### UNF
- Update UNF to match pages.
- Better UNF print combo?
- Combine cs_print and unf print layout funcs.
- Find out why UNF print is constantly desyncing (not crash screen related).
#### Crash screen crashes
- On a crash screen crash, disable specific components instead of the whole page
  Especially on the Summary page
- The next crash screen checks for timeout of prev (handle infinite loop crashes).
#### Rendering
- **Fix the flickering on Ares (and some other emulators) if possible.**
- Does double framebuffer mode have any input lag?
#### Debug map

#### Asserts
- Should assert macros be uppercase or lowercase?
- More special crash/assert handling:
  - RCP hang/Null SPTask.
    - Mention the need to restart console when this happens.
    - `rcp` thread register and other interface registers.
  - Object (eg. bank overflow).
    - (show bhv of the object that attempted to spawn or gCurrentObject).
  - Stack overflow.
  - Audio
    - Show AI registers?
  - Crash screen
    - Crashed page number and name
    - Selection cursor location
- More asserts for common crashes
  - `geo_process_animated_part`
  - `geo_process_node_and_siblings`
  - NULL Mario floor
- In `ASSERTF`/`ERRORF`, automatically create static buffer using `sizeof(the const string)`
  - Could help bypass cs_print buffer limit?
- Fix asserts expanding macros in condition string.
  - eg. NULL being printed as (void *)0
- If assert is too long, split buffer and print on the next line.
- Should `__func__` be reverted to getting the actual address + symbol?
  - Can this be done via the stack instead of inline asm?
  - Use for summary page print/jump.
- `check_stack_validity` for other thread stacks (loop through entire thread queue).
### Summary page
- Show cond bit from `fpcsr` if pc is c.cond? Or does a crash happen before that is set?
- Select section to go to the relevant page.
- `0x` prefix for Unimplemented instructions.
- Separate registers from insn again.
- Finalize layout.
- If `pc` is invalid, use the next function in the stack.
- Check for f64 denorms/NaN.
- "Likely NULL pointer dereference" if badvaddr is not actually 0 but still < 128
	Check disasm registers for NULL sureAddress to make sure it's a NULL pointer for more accuracy
- Skip printing duplicate saved registers.
- Show register data as ASCII.
### Stack trace page
- Use Libdragon's better stack trace functionality.
- Can stack trace be printed without a buffer?
- Make it clearer that the stack is thread-specific (show thread name on page?)
### Thread Registers page
- Extended version with a scrollable list of all registers and their full 64 bit contents (Everything from [here](https://n64.readthedocs.io/index.html) plus any other CPU/RCP registers). Thread registers on top (old context page) then all registers if scroll down.
#### Threads
- Can thread entries fit on one line while still being readable?
- Find out what the extra libultra thread is that only appears when UNF is on.
- Get thread name from map symbol of thread pointer?
- Color for "#th.found" text.
#### Reginspect
  - Scrollable for more data.
  - Status register individual interrupt bits.
  - Show upper/odd bits of float registers separately.
  - `A: GO TO` text if valid pointer.
  - If pointer, print the entire symbol?
    - Get size from map data.
    - Scrollable
  - If valid pointer, highlight address portion green like address select (lower 32 bits of 64 bit register).
- Explain thread select
  - Controls (press start to select thread)
  - Explain that it affects the stack page (and summary page?)
  - Find out what that unknown thread 0 (libultra) thread is with pri 149 is that only appears with make UNF (but not necessarily if UNF is on)
  - Clearn up thread print format.
    - Single-line thread display?
  - `A: GO TO MEMORY`, `START: SWITCH THREAD`, `B:EXIT`
- Show offsets in parse register address names mode.
- Different colors for register names from parsed global variable names (disasm page too)?
- Multiple FPCSR descriptions at once (already kinda done in reginspect).
- Better 64-bit register handling
  - Automatic bit mode check based on registers and `FR` bit in `fpcsr`
- Show direct register access values for each one.
- Determine whether that one register is a saved value or a frame pointer.
- Add missing controls descriptions.
- Highlight registers used in the instruction at `pc`.
  - Including `pc` itself?
- Move bit info arrays to the register souce's respective .inc.c files.
- Show "N/A" for registers without a corresponding thread register instead of falling back on direct access.
- Show register data as ASCII.
### Disasm page
- **Fix cursor passing the bottom of the screen when inline symbol headers are on.**
- Show addresses for each row (setting).
  - How can branch arrows fit?
- Multi-line pseudoinstructions if possible (ABS, BLT, BGT, BLE, NEG, NEGU, NOT, BGE, LI, LA, SGE, SGE, ADD?).
- Can the `insn_as_string` and `insn_name` buffers be combined?
- Implement "OVERSCAN" mode for branch arrows.
- Translucent dividers at the end of symbols (already at beginning).
- Can the bootleg "multithreading" for branch arrows be removed now that there is no longer lag with binary symbol searching?
- Reset branch arrow distance when it won't overlap instead of wrapping only after the distance reaches the end of the screen.
- Save register data types in the register buffer?
- Detect which segments are currently loaded to prevent trying to disasm garbage data (eg. reading from menu segment during normal gameplay).
  - Entry in text segment address range array?
- Print unknowns as binary should work on unimpl too.
- Automatically detect symbol change to reset/refill branch arrows?
- Segment name to the right of range at the top?
- Show offset/total of symbol.
- Non-symbol in .text segment and `NOP` prints "function alignment".
### Memory view page
- Press and hold to select multiple bytes?
- Is search functionality possible/reasonable?
- Highlight stuff like `$sp` location
- Should the cursor wrap horizontally?
- Segment name to the right of range at the top?
- Move segments page to be a submenu of this page.
- Show offset/total of symbol.
- Non-symbol in .text segment and `NOP` prints "function alignment".
### Map view page
- Should moving the cursor location here also change the location in ram view and disasm?
- Jumping to an address that's not in a symbol should find the nearest symbol index and jump to there.
- Is search functionality possible/reasonable?
- Describe "type" char.
- Determine segment/linker data type from map data?
- Is search functionality possible/reasonable?
### Segments page
- Show both rom and ram size? (for compressed data)
- Show hardcoded segments:
  - Main/engine/boot
  - Goddard
  - Crash screen/mapData
  - framebuffers/zbuffer/buffers
  - Various pools
- Move to be a submenu on memory page?
### Interface registers page
- Combine with thread registers page?
  - Interfaces as part of the threads dropdown.
    - 1 line vs. 2 lines.
    - Ordering?
    - Also register lists for direct access of CPU/CP0(+SPC)/CP1(+FCR) registers.
- Remove GIO/RDB/GIO_RDB?
- Print PIF ROM/RAM when PI/SI is selected.
- Combine DPC/DPS into "RCP"?
- Combine RI into RDRAM?
### Logs page
- Timestamps?
- Separate logs from puppyprint debug.
- Different warning levels per message.
  - info/debug/warning/error
  - Filtering
### Settings page
- Move entirely to page-specific popup?
  - Individual page settings in each page's controls/help popup box.
  - Press `B` to open
- Jump to the page from a page group.
- Save all changed settings somehow?
- Confirmation dialog box to reset all to defaults.
- Fix/remove redundant/similar settings.
- Automatically add the page-specific settings instead of being a separate array.
- Can this work without a buffer for shown entries like the threads page?
- Low vs. High resolution setting.
- Physical vs. Virtual address setting?
- Dropdowns for settings with enum values?
- Draw boolean settings as checkboxes?
- Fix/remove redundant/shared settings (mostly map symbol related).
- Print without buffer?
- Setting for staying on the same page on crash.
- Setting for staying on the same inspected thread on crash.
### About page
- Can this work without a buffer for shown entries like the threads page?
  - Info uses a text buffer
- Button/setting to cycle memory size formats (bytes/kb/mb/hex/num entries)
- Can goddardSegmentEnd -goddardSegmentStart replace gGoddardSize?
- Clean up code
- More entries:
  - Current RTC time if RTC is enabled? or `osGetTime()`/`osGetCount()`?
  - `gGlobalTimer`?
  - Mario action?
  - Mario floor?
</p>
</details>
