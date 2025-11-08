#ifndef EMUTEST_H
#define EMUTEST_H

#include "types.h"

enum Emulator {
    EMU_CONSOLE              = (1 << 0), // Also detects emulators accurate enough to emulate DPC registers
    EMU_WIIVC                = (1 << 1),
    EMU_PROJECT64_1_OR_2     = (1 << 2), // PJ64 1.6 to 2.3
    EMU_PROJECT64_3          = (1 << 3), // PJ64 2.4 to 3.0
        EMU_PROJECT64        = (EMU_PROJECT64_1_OR_2 | EMU_PROJECT64_3),
    EMU_MUPEN                = (1 << 4), // Also includes pre-2.12 ParallelN64
    EMU_PARALLEL_LAUNCHER    = (1 << 5), // Parallel Launcher (ParallelN64 core only)
    EMU_OTHER                = (1 << 6), // Any other emulator
};

enum SystemCapabilities {
    SUPPORTS_CACHING = (1 << 0),
    SUPPORTS_FLOAT_ROUNDING_MODE = (1 << 1),
    SUPPORTS_LIBPL = (1 << 2),
    SUPPORTS_SOFTWARE_FRAMEBUFFER = (1 << 3),
    SUPPORTS_EMULATOR_EXTENSIONS = (1 << 4), // i.e. `emux`

    // idk how to implement the following lol
    // SUPPORTS_DMA_TIMING = 0,
    // SUPPORTS_RSP_PIPELINE_STALL_TIMING = 0,
};

extern u32 detect_emulator();

extern u32 emulator_supports_trap_instructions(u32 emulator);

extern u32 check_emux_support();

/* gEmulator is an enum that identifies the current emulator.
 * The enum values work as a bitfield, so you can use the & and | operators
 * to test for multiple emulators or versions at once.
 * 
 * Examples:
 * 
 * Test for any version of PJ64:
 * if (gEmulator & EMU_PROJECT64)
 * 
 * Test for only PJ64 < 2.4:
 * if (gEmulator & EMU_PROJECT64_1_OR_2)
 * 
 * Test for Console or Parallel Launcher:
 * if (gEmulator & (EMU_CONSOLE | EMU_PARALLEL_LAUNCHER))
 */
extern u8 gEmulator;

/**
 * Bitflag that lists all system capabilities, for more granular
 * feature detection than gEmulator.
 */
extern u32 gSystemCapabilities;

// Whether the emulator supports libpl.
// Left for backwards compatibility
#define gSupportsLibpl (gSystemCapabilities & SUPPORTS_LIBPL)

// Included for backwards compatibility when upgrading from HackerSM64 2.0
#define gIsConsole ((gEmulator & EMU_CONSOLE) != 0)

#endif
