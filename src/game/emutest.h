#ifndef EMUTEST_H
#define EMUTEST_H

#include "types.h"

enum Emulator {
    EMU_WIIVC = 0x0001,
    EMU_PROJECT64_ANY = 0x006,
        EMU_PROJECT64_1_OR_2 = 0x0002, // PJ64 1.6 to 2.3
        EMU_PROJECT64_3 = 0x0004, // PJ64 2.4 to 3.0
    EMU_MUPEN_BASED = 0x0018,
        EMU_MUPEN_OLD = 0x0008, // Older versions of Mupen and pre-2.12 ParallelN64
        EMU_MUPEN_NEW = 0x0010, // Mupen64Plus-Next and RMG
    EMU_PL = 0x0020, // Paralle Launcher (ParallelN64 core only)
    EMU_OTHER = 0x0040, // Any other emulator
    EMU_CONSOLE = 0x0080 // Also detects emulators accurate enough to emulate DPC registers
};

// initializes gEmulator
extern void detect_emulator();

/* gEmulator is an enum that identifies the current emulator.
 * The enum values work as a bitfield, so you can use the & and | operators
 * to test for multiple emulators or versions at once.
 * 
 * Examples:
 * 
 * Test for any version of PJ64:
 * if (gEmulator & EMU_PROJECT64_ANY)
 * 
 * Test for only PJ64 < 3.0:
 * if (gEmulator & EMU_PROJECT64_1_OR_2)
 * 
 * Test for Console, Ares, or ParallelN64:
 * if (gEmulator & (EMU_CONSOLE | EMU_ARES | EMU_PARALLELN64))
 */
extern enum Emulator gEmulator;

// determines whether libpl is safe to use
extern u8 gSupportsLibpl;

// Included for backwards compatibility when upgrading from HackerSM64 2.0
#define gIsConsole ((gEmulator & EMU_CONSOLE) != 0)

#endif
