#ifndef USE_INSTANT_INPUT

#include "emutest.h"

// TODO: Need to test if this still needs to be disabled on Simple64 and cen64. It seems to work fine, but might only break under more intensive graphics usage
#define USE_INSTANT_INPUT (!(gEmulator & (EMU_CONSOLE | EMU_WIIVC | EMU_ARES | EMU_SIMPLE64 | EMU_CEN64 )))

extern void __osViSwapContext(void);

#endif
