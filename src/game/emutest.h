#ifndef EMUTEST_H
#define EMUTEST_H

enum Emulator {
    EMU_WIIVC = 0x0100,
    EMU_PROJECT64_ANY = 0x0200,
        EMU_PROJECT64_1_OR_2 = 0x0201, // PJ64 1.6 or similar (up to 2.3)
        EMU_PROJECT64_3 = 0x0203, // PJ64 3.X
        EMU_PROJECT64_4 = 0x0204, // PJ64 4.0 or later
    EMU_MUPEN_BASED = 0x0400, // mupen64plus or pre-2.12 paralleln64, but NOT simple64 or new paralleln64
        EMU_MUPEN_OLD = 0x0401, // 1964 and pre-2.12 paralleln64 will also get detected as this
        EMU_MUPEN64PLUS_NEXT = 0x0402,
    EMU_CEN64 = 0x0800,
    EMU_SIMPLE64 = 0x1000,
    EMU_PARALLELN64 = 0x2000, // ParallelN64 2.12 or later
    EMU_ARES = 0x4000,
    EMU_CONSOLE = 0x8000,

    EMU_MASK = 0xFF00
};

// initializes gEmulator, gIsConsole, gIsVC, and gCacheEmulated
extern void detect_emulator();

extern enum Emulator gEmulator;

#endif
