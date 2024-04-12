#ifndef EMUTEST_H
#define EMUTEST_H

enum Emulator {
    EMU_WIIVC = 0x0001,
    EMU_PROJECT64_ANY = 0x001E,
        EMU_PROJECT64_1_OR_2 = 0x0006, // PJ64 1.6 or similar (up to 2.3)
        EMU_PROJECT64_3 = 0x0008, // PJ64 3.X
        EMU_PROJECT64_4 = 0x0010, // PJ64 4.0 or later
    EMU_MUPEN_BASED = 0x0060, // mupen64plus or pre-2.12 paralleln64, but NOT simple64 or new paralleln64
        EMU_MUPEN_OLD = 0x0020, // Nemu64 and pre-2.12 paralleln64 will also get detected as this
        EMU_MUPEN64PLUS_NEXT = 0x040,
    EMU_CEN64 = 0x0080,
    EMU_SIMPLE64 = 0x0100,
    EMU_PARALLELN64 = 0x0200, // ParallelN64 2.12 or later
    EMU_ARES = 0x0400,
    EMU_CONSOLE = 0x0800
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

// Included for backwards compatibility when upgrading from HackerSM64 2.0
#define gIsConsole ((gEmulator & EMU_CONSOLE) != 0)

#endif
